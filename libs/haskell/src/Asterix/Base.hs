-- | Base asterix processing

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- {-# OPTIONS_GHC -Wno-all #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Asterix.Base where

import           GHC.TypeLits
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.RWS
import           Data.Word
import           Data.Maybe
import           Data.Map as Map
import           Data.Function (fix)
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text           (Text)

import           Asterix.Schema

data ParserError
    = Overflow -- Not enough data available
    | FxError -- Fx bit is set when it is not suppose to be
    | ItemPresenceError -- Item presence indication for non-defined item
    | ExplicitError -- Explicit item size error
    | ValidationError -- Data validation error
    | RfsError -- Problem with parsing RFS
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

-- | Make sure that n-bits are available, return start of block
fetch :: BitSize -> Parser BitOffset
fetch n = do
    totalSize <- asks snd
    offset1 <- get
    let offset2 = offset1 + n
    when (offset2 > totalSize) $ throw Overflow
    put offset2
    pure offset1

-- | Get word8, assume byte alignment
fetchWord8 :: Parser Word8
fetchWord8 = do
    offset <- fetch 8
    bytes <- asks fst
    pure $ BS.index bytes (div offset 8)

-- | Get bytestring, assume byte alignment
fetchBytes :: Int -> Parser ByteString
fetchBytes = undefined

-- | Get single bit
fetchBit :: Parser Bool
fetchBit = do
    offset <- fetch 1
    bytes <- asks fst
    pure $ Data.Bits.testBit (BS.index bytes (div offset 8)) (7 - mod offset 8)

byteStringToNumber :: Num a => ByteString -> a
byteStringToNumber = undefined

unpackWord8 :: Word8 -> [Bool]
unpackWord8 w = [Data.Bits.testBit w i | i <- [7,6..0]]

data Sized a = Sized BitOffset BitSize a
    deriving (Show, Eq)

-- | Parse something, augment with actual offset and bit size.
parseSized :: Parser a -> Parser (Sized a)
parseSized act = do
    offset1 <- get
    result <- act
    offset2 <- get
    pure $ Sized offset1 (offset2-offset1) result

data UVariation
    = Element VRule
    | Group [Sized UItem]
    | Extended [Sized UItem]
    | Repetitive [Sized UVariation]
    | Explicit (Maybe ExplicitType) Word8
    | Compound [Sized UItem]
    deriving (Show, Eq)

data UItem
    = USpare
    | UItem Text Text (Sized UVariation)
    deriving (Show, Eq)

newtype URecord = URecord UVariation
    deriving (Show, Eq)

newtype UExpansion = UExpansion UVariation
    deriving (Show, Eq)

data UDatablock
    = UDatablockSingle BitOffset BitSize [URecord]
    | UDatablockMultiple BitOffset BitSize [(Text, URecord)]
    deriving (Show, Eq)

parseUVariation :: VVariation -> Parser (Sized UVariation)
parseUVariation vvar = parseSized $ case vvar of
    VElement _ n vrule -> fmap Element $ do
        void $ fetch n
        pure vrule
    VGroup lst -> fmap Group $ do
        mapM parseUItem lst
    VExtended ts -> fmap Extended (f ts) where
        f :: [Maybe VItem] -> Parser [Sized UItem]
        f = \case
            [] -> pure []
            Just i : xs -> (:) <$> parseUItem i <*> f xs
            Nothing : xs -> do
                fx <- fetchBit
                case fx of
                    False -> pure []
                    True -> f xs
    VRepetitive mn var -> case mn of
        Nothing -> fmap Repetitive vars where
            vars :: Parser [Sized UVariation]
            vars = do
                x <- parseUVariation var
                fx <- fetchBit
                case fx of
                    False -> pure [x]
                    True  -> (:) <$> pure x <*> vars
        Just n1 -> fmap Repetitive $ do
            n2 <- byteStringToNumber <$> fetchBytes n1
            sequence (replicate n2 $ parseUVariation var)
    VExplicit met -> do
        n <- fetchWord8
        when (n <= 0) $ throw ExplicitError
        void $ fetchBytes (pred $ fromIntegral n)
        pure $ Explicit met n
    VCompound mn lst -> fmap Compound $ do
        fspecBits <- case mn of
            Nothing -> fix $ \loop -> do
                w <- fmap unpackWord8 fetchWord8
                let (flags, fx) = (init w, last w)
                case fx of
                    False -> pure flags
                    True  -> (<>) <$> pure flags <*> loop
            Just n -> do
                ws <- sequence (replicate n fetchWord8)
                pure (join $ fmap unpackWord8 ws)
        let fspec = reverse $ dropWhile (== False) $ reverse fspecBits
        when (length fspec > length lst) $ throw FxError
        items <- forM (zip fspec lst) $ \case
            (False, _) -> pure []
            (True, x) -> case x of
                CompoundSpare -> throw ItemPresenceError
                CompoundSubitem i -> pure <$> parseUItem i
                CompoundRFS -> do
                    itemCount <- fmap fromIntegral fetchWord8
                    sequence (replicate itemCount f)
                      where
                        f :: Parser (Sized UItem)
                        f = do
                            frn :: Int <- fmap fromIntegral fetchWord8
                            let frames = catMaybes (lst >>= \case
                                    CompoundSubitem i -> pure (Just i)
                                    CompoundSpare -> pure Nothing
                                    CompoundRFS -> pure Nothing)
                                framesMap = Map.fromList (zip [1..] frames)
                            i <- case Map.lookup frn framesMap of
                                Nothing -> throw RfsError
                                Just i -> pure i
                            parseUItem i
        pure $ join items

parseUItem :: VItem -> Parser (Sized UItem)
parseUItem i = parseSized $ case i of
    VSpare _o n -> do
        void $ fetch n
        pure USpare
    VItem name title var ->
        UItem <$> pure name <*> pure title <*> parseUVariation var

{-
parseURecord :: VVariation -> Parser URecord
parseURecord vvar = URecord <$> parseUVariation vvar

parseUExpansion :: VVariation -> Parser UExpansion
parseUExpansion vvar = UExpansion <$> parseUVariation vvar

-- | Single UAP datablock parser.
parseUDatablock :: VVariation -> Parser UDatablock
parseUDatablock vvar = do
    (lst, (o, n)) <- parseSized f
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
    when (null results) $ do
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

parseVariation :: forall t. IsSchema t VVariation => Parser (Variation t)
parseVariation = Variation <$> parseUVariation (schema @t)

newtype Item (t :: TItem) = Item UItem
    deriving (Show, Eq)

parseItem :: forall t. IsSchema t VItem => Parser (Item t)
parseItem = Item <$> parseUItem (schema @t)

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
-}
