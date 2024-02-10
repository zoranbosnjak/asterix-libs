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
    | Repetitive bs [UVariation bs]
    | Explicit bs
    | Compound bs [UItem bs]
    deriving (Show, Eq, Functor)

data UItem bs
    = USpare bs
    | UItem Text Text (UVariation bs)
    deriving (Show, Eq, Functor)

newtype URecord bs = URecord (UVariation bs)
    deriving (Show, Eq, Functor)

newtype UExpansion bs = UExpansion (UVariation bs)
    deriving (Show, Eq, Functor)

-- | Use type parameter, to avoid runtime check for the common case (single).
data UDatablock (single :: Bool) bs where
    UDatablockSingle :: bs -> [URecord bs] -> UDatablock 'True bs
    UDatablockMultiple :: bs -> [(Text, URecord bs)] -> UDatablock 'False bs

deriving instance Eq bs => Eq (UDatablock single bs)
deriving instance Show bs => Show (UDatablock single bs)
deriving instance Functor (UDatablock single)

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

newtype Datablock (uap :: TUap) bs = Datablock (UDatablock (IsSingleUap uap) bs)
    deriving (Show, Eq, Functor)

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
            pure $ Repetitive s vars
        Just n1 -> do
            (s, vars) <- parseBitsWith $ do
                n2 <- Bits.getNumberAligned <$> fetch (n1*8)
                sequence (replicate n2 $ parseUVariation var)
            pure $ Repetitive s vars
    VExplicit -> do
        (s, _) <- parseBitsWith $ do
            n <- fmap fromIntegral fetchWord8
            when (n <= 0) $ throw ExplicitError
            fetch (8 * pred n)
        pure $ Explicit s
    VRandomFieldSequencing -> do
        undefined
    VCompound mn lst -> do
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

parseUItem :: VItem -> Parsing (UItem Bits)
parseUItem = \case
    VSpare _o n ->
        USpare <$> fetch n
    VItem name title var ->
        UItem <$> pure name <*> pure title <*> parseUVariation var

-- | Single UAP datablock parser.
parseUDatablock :: VVariation -> Parsing (UDatablock 'True Bits)
parseUDatablock vvar = uncurry UDatablockSingle <$> parseBitsWith f
  where
    f = eof >>= \case
        True -> pure []
        False -> (:) <$> (URecord <$> parseUVariation vvar) <*> f

-- | Try to parse with different UAPs, where each record in a datablock
-- can potentially be of a different UAP.
parseUDatablockTryWith :: Bits -> [(Text, VVariation)] -> [UDatablock 'False Bits]
parseUDatablockTryWith s probes
    | Bits.null s = pure $ UDatablockMultiple s []
    | otherwise = do
        (name, var) <- probes
        case runParser (parseUVariation var) s of
            Left _ -> []
            Right (v, s') -> parseUDatablockTryWith s' probes >>= \case
                UDatablockMultiple _ lst -> [UDatablockMultiple s ((name, URecord v) : lst)]
