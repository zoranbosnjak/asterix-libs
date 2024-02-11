-- | Base asterix processing

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- {-# OPTIONS_GHC -Wno-all #-}

module Asterix.Base where

import           GHC.TypeLits
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.RWS
import           Data.Function       (fix)
import           Data.Maybe
import           Data.Word
import           Data.ByteString as BS
import           Data.Text           (Text)
import           Prelude

import           Asterix.Schema
--import           Asterix.Parsing
--import           Bits

data ParserError
    = Overflow -- Not enough data available
    | FxError -- Fx bit is set when it is not suppose to be
    | ItemPresenceError -- Item presence indication for non-defined item
    | ExplicitError -- Explicit item size error
    | ValidationError -- Data validation error
    | MultipleDatablockError -- Unable to parse multi UAP datablock
    deriving (Show, Eq)

type BitOffset = Int
type BitSize = Int

type Parser = RWST (ByteString, BitSize) () BitOffset (Either ParserError)

runParser :: Parser a -> ByteString -> BitOffset -> Either ParserError (a, BitOffset)
runParser act bytes offset = f <$> runRWST act (bytes, BS.length bytes * 8) offset
  where
    f (result, offset', ()) = (result, offset')

throw :: ParserError -> Parser a
throw = lift . Left

eof :: Parser Bool
eof = (>=) <$> get <*> asks snd

fetch :: BitSize -> Parser BitOffset
fetch n = do
    totalSize <- asks snd
    offset1 <- get
    let offset2 = offset1 + n
    when (offset2 > totalSize) $ throw Overflow
    put offset2
    pure offset1

-- | Parse something, return result with actual bits.
parseWithBits :: Parser a -> Parser (a, (BitOffset, BitSize))
parseWithBits act = do
    offset1 <- get
    result <- act
    offset2 <- get
    pure (result, (offset1, offset2-offset1))

data UVariation
    = Element BitOffset BitSize VRule
    | Group BitOffset BitSize [UItem]
    | Extended BitOffset BitSize [Maybe UItem] [Maybe VItem] -- present, non-present
    | Repetitive BitOffset BitSize (Maybe Int) [UVariation]
    | Explicit BitOffset BitSize (Maybe ExplicitType) Word8
    | Compound BitOffset BitSize
        (Either Int Int) -- fixed/actual bytes of fspec
        [Maybe (Either VItem UItem)] -- non-spare items (defined/present)
    | RandomFieldSequencing BitOffset BitSize
        [VItem] -- defined items (uap without RFS)
        [(Int, UItem)] -- present items
    deriving (Show, Eq)

data UItem
    = USpare BitOffset BitSize
    | UItem Text Text UVariation
    deriving (Show, Eq)

newtype URecord = URecord UVariation
    deriving (Show, Eq)

newtype UExpansion = UExpansion UVariation
    deriving (Show, Eq)

data UDatablock
    = UDatablockSingle BitOffset BitSize [URecord]
    | UDatablockMultiple BitOffset BitSize [(Text, URecord)]
    deriving (Show, Eq)

parseUVariation :: VVariation -> Parser UVariation
parseUVariation = \case
    VElement _o n vrule ->
        Element <$> fetch n <*> pure n <*> pure vrule
    VGroup lst -> do
        (items, (o, n)) <- parseWithBits (mapM parseUItem lst)
        pure (Group o n items)
    _ -> undefined
    {-
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
-}

parseUItem :: VItem -> Parser UItem
parseUItem = \case
    VSpare _o n ->
        USpare <$> get <*> fetch n
    VItem name title var ->
        UItem <$> pure name <*> pure title <*> parseUVariation var

parseURecord :: VVariation -> Parser URecord
parseURecord vvar = URecord <$> parseUVariation vvar

parseUExpansion :: VVariation -> Parser UExpansion
parseUExpansion vvar = UExpansion <$> parseUVariation vvar

-- | Single UAP datablock parser.
parseUDatablock :: VVariation -> Parser UDatablock
parseUDatablock vvar = do
    (lst, (o, n)) <- parseWithBits f
    pure $ UDatablockSingle o n lst
  where
    f = eof >>= \case
        True -> pure []
        False -> (:) <$> (parseURecord vvar) <*> f

-- | Try to parse with different UAPs, where each record in a datablock
-- can potentially be of a different UAP. Instead of a single datablock,
-- this function returns a list of one or more possible answers.
parseUDatablockTryWith :: [(Text, VVariation)] -> Parser [UDatablock]
parseUDatablockTryWith probes = do
    (bytes, totalSize) <- ask
    offset <- get
    let results = go (bytes, totalSize) offset
    when (Prelude.null results) $ do
        throw MultipleDatablockError
    put totalSize
    pure $ fmap (UDatablockMultiple offset (totalSize - offset)) results
  where
    go :: (ByteString, BitSize) -> BitOffset -> [[(Text, URecord)]]
    go (bytes, totalSize) offset
        | offset == totalSize = pure []
        | offset > totalSize = Control.Applicative.empty
        | otherwise = do
            (name, vvar) <- probes
            case runParser (parseURecord vvar) bytes offset of
                Left _err -> Control.Applicative.empty
                Right (r, offset') -> (:)
                    <$> pure (name, r)
                    <*> go (bytes, totalSize) offset'

newtype Variation (t :: TVariation) = Variation UVariation
    deriving (Show, Eq)

newtype Item (t :: TItem) = Item UItem
    deriving (Show, Eq)

newtype Record (cat :: Nat) (ed :: TEdition) (t :: [Maybe TItem])
    = Record URecord
    deriving (Show, Eq)

newtype Expansion (cat :: Nat) (ed :: TEdition) (t :: [Maybe TItem])
    = Expansion UExpansion
    deriving (Show, Eq)

newtype Datablock (uap :: TUap) = Datablock UDatablock
    deriving (Show, Eq)

{-
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
-}
