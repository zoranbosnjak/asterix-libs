-- | Base asterix processing

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: remove this
-- {-# OPTIONS_GHC -Wno-all #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Asterix.Base where

import           GHC.TypeLits
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.RWS
import           Data.Word
import           Data.Maybe
import qualified Data.Map as Map
import           Data.Function (fix)
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text           (Text)

import           BitString as B
import           BitString.Builder as BB
import           Asterix.Parsing
import           Asterix.Schema

{-
byteStringToNumber :: Num a => ByteString -> a
byteStringToNumber = BS.foldl' f 0 where
    f :: Num a => a -> Word8 -> a
    f acc x = acc * 256 + fromIntegral x

unpackWord8 :: Word8 -> [Bool]
unpackWord8 w = [Data.Bits.testBit w i | i <- [7,6..0]]

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
    = UDatablockSingle [Sized URecord]
    | UDatablockMultiple [(Text, Sized URecord)]
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

parseURecord :: VVariation -> Parser (Sized URecord)
parseURecord vvar = fmap URecord <$> parseUVariation vvar

parseUExpansion :: VVariation -> Parser (Sized UExpansion)
parseUExpansion vvar = fmap UExpansion <$> parseUVariation vvar

-- | Single UAP datablock parser.
parseUDatablock :: VVariation -> Parser (Sized UDatablock)
parseUDatablock vvar = parseSized $ fmap UDatablockSingle f
  where
    f :: Parser [Sized URecord]
    f = eof >>= \case
        True -> pure []
        False -> (:) <$> (parseURecord vvar) <*> f

-- | Try to parse with different UAPs, where each record in a datablock
-- can potentially be of a different UAP. Instead of a single datablock,
-- this function returns a list of one or more possible answers.
parseUDatablockTryWith :: [(Text, VVariation)] -> Parser (Sized [UDatablock])
parseUDatablockTryWith probes = parseSized $ do
    (bytes, totalSize) <- ask
    offset <- get
    let results = go (bytes, totalSize) offset
    when (null results) $ do
        throw MultipleDatablockError
    put totalSize
    pure $ fmap UDatablockMultiple results
  where
    go :: (ByteString, BitSize) -> BitOffset -> [[(Text, Sized URecord)]]
    go (bytes, totalSize) offset
        | offset == totalSize = [[]] -- done
        | offset > totalSize = [] -- no solution
        | otherwise = do
            (name, vvar) <- probes
            case runParser (parseURecord vvar) bytes offset of
                Left _err -> [] -- no solution
                Right (r, offset') -> (:)
                    <$> pure (name, r)
                    <*> go (bytes, totalSize) offset'

newtype Variation (t :: TVariation) = Variation UVariation
    deriving (Show, Eq)

parseVariation :: forall t. IsSchema t VVariation => Parser (Sized (Variation t))
parseVariation = fmap Variation <$> parseUVariation (schema @t)

newtype Item (t :: TItem) = Item UItem
    deriving (Show, Eq)

parseItem :: forall t. IsSchema t VItem => Parser (Sized (Item t))
parseItem = fmap Item <$> parseUItem (schema @t)

newtype Record (cat :: Nat) (ed :: TEdition) (t :: [Maybe TItem])
    = Record URecord
    deriving (Show, Eq)

parseRecord :: forall cat ed t. IsSchema t VVariation
    => Parser (Sized (Record cat ed t))
parseRecord = fmap Record <$> parseURecord (schema @t)

newtype Expansion (cat :: Nat) (ed :: TEdition) (t :: [Maybe TItem])
    = Expansion UExpansion
    deriving (Show, Eq)

newtype Datablock (uap :: TUap) = Datablock UDatablock
    deriving (Show, Eq)

-- Constructing

{-
mkUElement :: BitOffset -> BitSize -> Int -> Builder
mkUElement offset n value = undefined -- = fromBits $ fromUInteger o n x

mkUGroup :: [UItem Builder] -> Builder
mkUGroup = mconcat . fmap unUItem

mkUExtended :: [Maybe (UItem Builder)] -> Builder
mkUExtended = undefined

mkURepetitive :: Maybe Int -> [UVariation Builder] -> Builder
mkURepetitive = undefined

mkUExplicit :: Builder -> Builder
mkUExplicit = undefined

mkUDatablockSingle :: Int -> [URecord Builder] -> UDatablock Builder
mkUDatablockSingle = undefined

mkUDatablockMultiple :: Int -> [(Text, URecord Builder)] -> UDatablock Builder
mkUDatablockMultiple = undefined
-}
-}
