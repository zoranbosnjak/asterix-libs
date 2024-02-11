-- | Base asterix processing

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Asterix.Base where

import           GHC.TypeLits
import           Control.Applicative
import           Control.Monad
import           Data.Function       (fix)
import           Data.Maybe
import           Data.Text           (Text)
import           Prelude

import           Asterix.Parsing
import           Asterix.Schema
import           Bits

data UVariation bs
    = Element bs VRule
    | Group bs [UItem bs]
    | Extended bs [Maybe (UItem bs)]
    | Repetitive bs (Maybe Int) [UVariation bs]
    | Explicit bs (Maybe ExplicitType) bs
    | Compound bs [Maybe (Maybe (UItem bs))]
    | RandomFieldSequencing -- TODO
    deriving (Show, Eq, Functor)

data UItem bs
    = USpare bs
    | UItem Text Text (UVariation bs)
    deriving (Show, Eq, Functor)

newtype URecord bs = URecord (UVariation bs)
    deriving (Show, Eq, Functor)

newtype UExpansion bs = UExpansion (UVariation bs)
    deriving (Show, Eq, Functor)

data UDatablock bs
    = UDatablockSingle bs [URecord bs]
    | UDatablockMultiple bs [(Text, URecord bs)]
    deriving (Show, Eq, Functor)

newtype Variation (t :: TVariation) bs = Variation (UVariation bs)
    deriving (Show, Eq, Functor)

newtype Item (t :: TItem) bs = Item (UItem bs)
    deriving (Show, Eq, Functor)

newtype Record (cat :: Nat) (ed :: TEdition) (t :: [Maybe TItem]) bs
    = Record (URecord bs)
    deriving (Show, Eq, Functor)

newtype Expansion (cat :: Nat) (ed :: TEdition) (t :: [Maybe TItem]) bs
    = Expansion (UExpansion bs)
    deriving (Show, Eq, Functor)

newtype Datablock (uap :: TUap) bs = Datablock (UDatablock bs)
    deriving (Show, Eq, Functor)

unDatablock :: UDatablock bs -> bs
unDatablock = \case
    UDatablockSingle bs _ -> bs
    UDatablockMultiple bs _ -> bs

unUVariation :: UVariation bs -> bs
unUVariation = \case
    Element bs _ -> bs
    Group bs _ -> bs
    Extended bs _ -> bs
    Repetitive bs _ _ -> bs
    Explicit bs _ _ -> bs
    Compound bs _ -> bs

unUItem :: UItem bs -> bs
unUItem = \case
    USpare bs -> bs
    UItem _ _ var -> unUVariation var

mkElement :: Int -> Int -> Int -> Builder
mkElement o n x = fromBits $ fromUInteger o n x

mkGroup :: [UItem Builder] -> Builder
mkGroup = mconcat . fmap unUItem

mkExtended :: [Maybe (UItem Builder)] -> Builder
mkExtended = undefined

mkRepetitive :: Maybe Int -> [UVariation Builder] -> Builder
mkRepetitive = undefined

mkExplicit :: Builder -> Builder
mkExplicit = undefined

mkUDatablockSingle :: Int -> [URecord Builder] -> UDatablock Builder
mkUDatablockSingle = undefined

mkUDatablockMultiple :: Int -> [(Text, URecord Builder)] -> UDatablock Builder
mkUDatablockMultiple = undefined

-- | Parse something, return actual bits, together with the result.
parseBitsWith :: Parsing a -> Parsing (Bits, a)
parseBitsWith act = do
    s1 <- get
    result <- act
    s2 <- get
    let n = bitLength s1 - bitLength s2
    pure (Bits.take n s1, result)

parseUVariation :: VVariation -> Parsing (UVariation Bits)
parseUVariation = \case
    VElement _o n vrule ->
        Element <$> fetch n <*> pure vrule
    VGroup lst -> do
        (s, items) <- parseBitsWith (mapM parseUItem lst)
        pure (Group s items)
    VExtended ts -> do
        let go = \case
                [] -> pure []
                (Just i : xs) -> (:) <$> fmap Just (parseUItem i) <*> go xs
                (Nothing : xs) -> do
                    fx <- fetchBool
                    case fx of
                        False -> pure [Nothing]
                        True  -> (:) <$> pure Nothing <*> go xs
        (s, lst) <- parseBitsWith (go ts)
        pure $ Extended s lst
    VRepetitive mn var -> case mn of
        Nothing -> do
            let go = do
                    x <- parseUVariation var
                    fx <- fetchBool
                    case fx of
                        False -> pure [x]
                        True  -> (:) <$> pure x <*> go
            (s, vars) <- parseBitsWith go
            pure $ Repetitive s mn vars
        Just n1 -> do
            (s, vars) <- parseBitsWith $ do
                n2 <- Bits.getNumberAligned <$> fetch (n1*8)
                sequence (replicate n2 $ parseUVariation var)
            pure $ Repetitive s mn vars
    VExplicit met -> do
        (s, _) <- parseBitsWith $ do
            n <- fmap fromIntegral fetchWord8
            when (n <= 0) $ throw ExplicitError
            fetch (8 * pred n)
        pure $ Explicit s met undefined
    VRandomFieldSequencing -> do
        undefined
    VCompound _mn _lst -> do
        undefined {-
        (s, items) <- parseBitsWith $ do
            fspec' <- case mn of
                Nothing -> fix $ \loop -> do
                    w <- fmap unpackWord8 fetchWord8
                    let (flags, fx) = (init w, last w)
                    case fx of
                        False -> pure flags
                        True  -> (<>) <$> pure flags <*> loop
                Just n -> do
                    ws <- sequence (replicate n fetchWord8)
                    pure $ join $ fmap unpackWord8 ws
            let fspec = reverse $ dropWhile (== False) $ reverse fspec'
            when (Prelude.length fspec > Prelude.length lst) $ throw FxError
            forM (zip fspec lst) $ \case
                (False, _) -> pure Nothing
                (True, Nothing) -> throw ItemPresenceError
                (True, Just i) -> Just <$> parseUItem i
        pure $ Compound s $ catMaybes items
        -}

parseUItem :: VItem -> Parsing (UItem Bits)
parseUItem = \case
    VSpare _o n ->
        USpare <$> fetch n
    VItem name title var ->
        UItem <$> pure name <*> pure title <*> parseUVariation var

parseURecord :: VVariation -> Parsing (URecord Bits)
parseURecord vvar = URecord <$> parseUVariation vvar

-- | Single UAP datablock parser.
parseUDatablock :: VVariation -> Parsing (UDatablock Bits)
parseUDatablock vvar = uncurry UDatablockSingle <$> parseBitsWith f
  where
    f = eof >>= \case
        True -> pure []
        False -> (:) <$> (parseURecord vvar) <*> f

-- | Try to parse with different UAPs, where each record in a datablock
-- can potentially be of a different UAP.
parseUDatablockTryWith :: Bits -> [(Text, VVariation)] -> [UDatablock Bits]
parseUDatablockTryWith s probes
    | Bits.null s = pure $ UDatablockMultiple s []
    | otherwise = do
        (name, var) <- probes
        case runParser (parseUVariation var) s of
            Left _ -> []
            Right (v, s') -> parseUDatablockTryWith s' probes >>= \case
                UDatablockSingle _ _ -> error "internal error"
                UDatablockMultiple _ lst -> [UDatablockMultiple s ((name, URecord v) : lst)]
