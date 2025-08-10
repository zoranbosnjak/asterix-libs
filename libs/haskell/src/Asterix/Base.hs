-- |
-- Module: Asterix.Base
--
-- Asterix base data structures and functions.

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterix.Base where

import           Data.Coerce
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.RWS
import           Data.Bits                 (testBit)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.ByteString.Builder   as BSB
import qualified Data.List                 as L
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Proxy
import           Data.Maybe
import           Data.String               as S
import           Data.Text                 (Text)
import           Data.Word
import           GHC.TypeLits

import           Asterix.Schema
import           Asterix.BitString

class HasCategory t where
    categoryOf :: t -> Word8

class Unparsing r t where
    unparse :: t -> r

instance Unparsing Bits Bits where
    unparse = id

instance Unparsing Bits ByteString where
    unparse = byteStringToBits

instance Unparsing SBuilder ByteString where
    unparse = fromByteString

intError :: a
intError = error "Internal error"

type ItemName = Text

newtype RawDatablock = RawDatablock { unRawDatablock :: ByteString }
    deriving (Eq, Show)

-- | Fspec flag bits, without fx bits and without trailing bits
newtype Fspec = Fspec { fspecBits :: [Bool] }
    deriving Show

type FRN = Word8

data ParsingMode
    = StrictParsing
    | PartialParsing
    deriving (Eq, Enum, Bounded, Show)

data RecordItem nsp
    = RecordItem nsp
    | RecordItemSpare
    | RecordItemRFS [(FRN, nsp)]
    deriving (Eq, Show)

-- | Helper structure for parsing.
-- This module implements actual low-level asterix parsing.
-- Each concrete implementation shall provide a structure of this type
-- as a way to specify how the parsing result shall be stored.
-- This mostly depends on a usecase.
data ParsingStore var rv nsp item rec exp = ParsingStore
    -- variation

    -- Element consumes the actual bits.
    { psElement    :: Bits -> var
    -- Group consumes actual bits + list of parsed items
    , psGroup      :: Bits -> [item] -> var
    -- Extended consumes actual bits + list of parsed items or fx bits
    , psExtended   :: Bits -> [Maybe item] -> var
    -- Repetitive consumes actual bits + list of variations
    , psRepetitive :: Bits -> [var] -> var
    -- Explicit consumes all bits + data bits (all bits except first octet)
    , psExplicit   :: Bits -> Bits -> var
    -- Compound consumes (raw fspec, raw items, items)
    , psCompound   :: Bits -> Bits -> [Maybe nsp] -> var

    -- rule variation
    , psRuleVar :: var -> rv

    -- non-spare
    , psNsp :: rv -> nsp

    -- items
    , psSpare :: Bits -> item
    , psItem  :: nsp -> item

    -- record
    , psRecord ::
        Bits            -- raw fspec
        -> Bits         -- raw items
        -> [Maybe (RecordItem nsp)] -- items
        -> rec

    -- expansion
    , psExpansion ::
        Bits            -- raw fspec
        -> Bits         -- raw items
        -> [Maybe nsp]  -- items
        -> exp
    }

-- | Parsing environment.
data Env var rv nsp item rec exp = Env
    { envStore :: ParsingStore var rv nsp item rec exp
    , envMode  :: ParsingMode
    , envInput :: ByteString
    }

instance HasCategory RawDatablock where
    categoryOf (RawDatablock bs) = BS.index bs 0

getRawRecords :: RawDatablock -> ByteString
getRawRecords (RawDatablock bs) = BS.drop 3 bs

natVal8 :: (KnownNat o, IsNat8 o, Num b) => Proxy o -> b
natVal8 = fromIntegral . natVal

newtype ParsingError = ParsingError Text
    deriving (Show, IsString)

type ParsingM a b c d e f = RWST (Env a b c d e f) () Offset (Either ParsingError)

runParsing :: Monad m => RWST r w s m a -> r -> s -> m (a, s)
runParsing act r s = do
    (a, s', _w) <- runRWST act r s
    pure (a, s')

parsingError :: ParsingError -> ParsingM a b c d e f r
parsingError = lift . Left

-- | Check bit alignment
aligned :: Offset -> Int -> Bool
aligned o o8 = numBits o == o8

-- | End of a bytestring
endOffset :: ByteString -> Offset
endOffset = intToNumBits . (*8) . BS.length

-- | End of input
eof :: ParsingM a b c d e f Bool
eof = do
    o <- get
    o2 <- asks (endOffset . envInput)
    case compare o o2 of
        LT -> pure False
        EQ -> pure True
        GT -> intError

-- | Move offset pointer by 'n' bits.
moveOffset :: Size -> ParsingM a b c d e f ()
moveOffset n = do
    bs <- asks envInput
    o <- get
    let o' = o + coerce n
    when (o' > endOffset bs) $ parsingError "overflow"
    put o'

-- | Fetch some number of bits.
parseBits :: Int -> Size -> ParsingM a b c d e f Bits
parseBits o8 n = do
    bs <- asks envInput
    o <- get
    withAssumption (aligned o o8) $ do
        moveOffset n
        pure $ Bits bs o n

-- | Parse Word8
parseWord8 :: ParsingM a b c d e f Word8
parseWord8 = do
    bs <- asks envInput
    o <- get
    withAssumption (aligned o 0) $ do
        moveOffset 8
        pure $ BS.index bs (numBytes o)

-- | Parse 'n' bytes
parseBytes :: Int -> ParsingM a b c d e f ByteString
parseBytes n = do
    bs <- asks envInput
    o <- get
    withAssumption (aligned o 0) $ do
        moveOffset $ intToNumBits $ n * 8
        pure $ BS.take n $ BS.drop (numBytes o) bs

-- | Parse 'fx' bit
parseFx :: ParsingM a b c d e f Bool
parseFx = do
    Bits bs o _ <- parseBits 7 1
    pure $ testBit (BS.index bs (numBytes o)) 0

-- | Parse complete fspec, take fx bits into account
parseFspec :: [x] -> ParsingM a b c d e f Fspec
parseFspec lst = Fspec . take (length lst) <$> go
  where
    go = do
        w <- word8ToBools <$> parseWord8
        let (flags, fx) = (init w, last w)
        case fx of
            False -> pure flags
            True  -> (<>) <$> pure flags <*> go

parseVariation :: VVariation -> ParsingM var b c d e f var
parseVariation sch = ask >>= \env -> case sch of
    GElement o n _ruleCont -> do
        psElement (envStore env) <$> parseBits o (intToNumBits n)
    GGroup _o lst -> do
        o1 <- get
        items <- go lst
        o2 <- get
        let bits = Bits (envInput env) o1 (coerce $ o2 - o1)
        pure $ psGroup (envStore env) bits items
      where
        go []     = pure []
        go (x:xs) = (:) <$> parseItem x <*> go xs
    GExtended lst -> do
        o1 <- get
        mItems <- go lst
        o2 <- get
        let bits = Bits (envInput env) o1 (coerce $ o2 - o1)
        pure $ psExtended (envStore env) bits mItems
      where
        go [] = pure []
        go (mx:xs) = case mx of
            Nothing -> parseFx >>= \case
                False -> pure [Nothing]
                True -> (:) <$> pure Nothing <*> go xs
            Just x -> (:) <$> (Just <$> parseItem x) <*> go xs
    GRepetitive rt var -> do
        o1 <- get
        lst <- case rt of
            GRepetitiveRegular rep -> do
                parseBytes rep >>= goRegular . byteStringToNum @Int
            GRepetitiveFx -> goFx
        o2 <- get
        let bits = Bits (envInput env) o1 (coerce $ o2 - o1)
        pure $ psRepetitive (envStore env) bits lst
      where
        goRegular = \case
            0 -> pure []
            n -> (:) <$> parseVariation var <*> goRegular (pred n)
        goFx = do
            x <- parseVariation var
            parseFx >>= \case
                False -> pure [x]
                True -> (:) <$> pure x <*> goFx
    GExplicit _met -> do
        o1 <- get
        n <- parseWord8
        o2 <- get
        _ <- case n of
            0 -> parsingError "Unexpected size of explicit item."
            _ -> parseBytes (fromIntegral $ pred n)
        o3 <- get
        let b1 = Bits (envInput env) o1 (coerce $ o3 - o1)
            b2 = Bits (envInput env) o2 (coerce $ o3 - o2)
        pure $ psExplicit (envStore env) b1 b2
    GCompound lst -> do
        o1 <- get
        fspec <- parseFspec lst
        o2 <- get
        items <- go $ zip (fspecBits fspec <> repeat False) lst
        o3 <- get
        let rawFspec = Bits (envInput env) o1 (coerce $ o2 - o1)
            rawItems = Bits (envInput env) o2 (coerce $ o3 - o2)
        pure $ psCompound (envStore env) rawFspec rawItems items
      where
        go :: [(Bool, Maybe VNonSpare)]
            -> ParsingM a b nsp d e f [Maybe nsp]
        go [] = pure mempty
        go ((flag,spec) : xs)
            | not flag = (:) <$> pure Nothing <*> go xs
            | otherwise = case spec of
                Nothing -> parsingError "FX bit set for non-defined item."
                Just nsp -> (:) <$> (Just <$> parseNonSpare nsp) <*> go xs

parseRuleVariation :: VRule VVariation
    -> ParsingM a rv c d e f rv
parseRuleVariation sch1 = ask >>= \env -> do
    let sch2 = case sch1 of
            GContextFree sch   -> sch
            GDependent _ sch _ -> sch
    psRuleVar (envStore env) <$> parseVariation sch2

parseNonSpare :: VNonSpare -> ParsingM var rv nsp d e f nsp
parseNonSpare (GNonSpare _name _title rvSch) = ask >>= \env -> do
    psNsp (envStore env) <$> parseRuleVariation rvSch

parseItem :: VItem -> ParsingM a b c item e f item
parseItem sch = ask >>= \env -> case sch of
    GSpare o n -> psSpare (envStore env) <$> parseBits o (intToNumBits n)
    GItem nsp  -> psItem (envStore env) <$> parseNonSpare nsp

parseRecord :: VRecord -> ParsingM a b c d rec f rec
parseRecord (GRecord lst) = ask >>= \env -> do
    o1 <- get
    fspec <- parseFspec lst
    o2 <- get
    (items, clean) <- goItems (fspecBits fspec) lst
    o3 <- get
    let rawItems = Bits (envInput env) o2 (coerce $ o3 - o2)
        -- We can reuse original bits only if the parsing was 'clean',
        -- otherwise we need to recreate the fspec.
        rawFspec = case clean of
            True  -> Bits (envInput env) o1 (coerce $ o2 - o1)
            False -> recreateFspec (fmap isJust items)
    pure $ psRecord (envStore env) rawFspec rawItems items
  where
    findSchema :: FRN -> [VUapItem] -> Maybe VNonSpare
    findSchema _ [] = Nothing
    findSchema n (x:xs)
        | n == 0 = case x of
            GUapItem nsp -> Just nsp
            _            -> Nothing
        | otherwise = findSchema (pred n) xs

    goRfs :: ParsingM a b nsp d e f ([(FRN, nsp)], Bool)
    goRfs = parseWord8 >>= go
      where
        go :: Word8 -> ParsingM a b nsp d e f ([(FRN, nsp)], Bool)
        go = \case
            0 -> pure (mempty, True)
            cnt -> do
                frn <- parseWord8 >>= \case
                    0 -> parsingError "Invalid FRN."
                    n -> pure $ fromIntegral n
                nsp <- maybe
                    (parsingError "RFS subitem not defined.")
                    pure (findSchema (pred frn) lst)
                env <- ask
                offset <- get
                case runParsing (parseNonSpare nsp) env offset of
                    Left err -> case envMode env of
                        StrictParsing  -> parsingError err
                        PartialParsing -> pure (mempty, False)
                    Right (x, offset') -> do
                        put offset'
                        (xs, clean) <- go (pred cnt)
                        pure ((frn, x) : xs, clean)

    goItems :: [Bool] -> [VUapItem]
        -> ParsingM a b nsp d e f ([Maybe (RecordItem nsp)], Bool)
    goItems [] [] = pure (mempty, True)
    goItems [] (_:ts) = do
        (items, clean) <- goItems [] ts
        pure (Nothing : items, clean)
    goItems _flags [] = asks envMode >>= \case
        StrictParsing -> parsingError "Record subitem not defined."
        PartialParsing -> pure (mempty, False)
    goItems (flag:flags) (spec:specs)
        | not flag = do
            (items, clean) <- goItems flags specs
            pure (Nothing : items, clean)
        | otherwise = case spec of
            GUapItemSpare -> asks envMode >>= \case
                StrictParsing -> parsingError "FX bit set for spare item"
                PartialParsing -> pure (mempty, False)
            GUapItemRFS -> do
                (rfs, clean1) <- goRfs
                case clean1 of
                    False -> pure ([Just (RecordItemRFS rfs)], False)
                    True -> do
                        (items, clean2) <- goItems flags specs
                        pure (Just (RecordItemRFS rfs) : items, clean2)
            GUapItem nsp -> do
                env <- ask
                offset <- get
                case runParsing (parseNonSpare nsp) env offset of
                    Left err -> case envMode env of
                        StrictParsing  -> parsingError err
                        PartialParsing -> pure (mempty, False)
                    Right (x, offset') -> do
                        put offset'
                        (items, clean) <- goItems flags specs
                        pure (Just (RecordItem x) : items, clean)

recreateFspec :: [Bool] -> Bits
recreateFspec
    = byteStringToBits
    . BS.pack
    . terminateFx -- set last FX bit to '0'
    . fmap ((+ 1) . (* 2) . boolsToWord8)
    . L.dropWhileEnd (replicate 7 False ==)
    . groupsOf False 7
  where
    groupsOf :: a -> Int -> [a] -> [[a]]
    groupsOf a n lst
        | m == 0 = []
        | m < n = [lst <> replicate (n-m) a]
        | m == n = [lst]
        | otherwise = L.take n lst : groupsOf a n (L.drop n lst)
      where
        m = Prelude.length lst
    terminateFx :: Num a => [a] -> [a]
    terminateFx []   = []
    terminateFx lst = init lst <> [last lst - 1]

-- | Parse multiple records of the same UAP.
parseRecords :: VRecord -> ParsingM a b c d rec f [rec]
parseRecords sch = eof >>= \case
    True -> pure []
    False -> (:) <$> parseRecord sch <*> parseRecords sch

-- | Try to parse multiple UAP combinations.
-- This function consumes complete input in any case.
parseRecordsTry :: [(name, VRecord)] -> ParsingM a b c d rec f [[(name, rec)]]
parseRecordsTry schs = do
    env <- ask
    offset <- get
    let eo = endOffset $ envInput env
    put eo
    pure $ go eo env offset
  where
    go eo env offset
        | offset > eo = intError
        | offset == eo = [[]]
        | otherwise = do
            (name, sch) <- schs
            (x, offset') <- either (const empty) pure
                (runParsing (parseRecord sch) env offset)
            (:) <$> pure (name, x) <*> go eo env offset'

parseExpansion :: VExpansion -> ParsingM a b c d e exp exp
parseExpansion (GExpansion mn lst) = ask >>= \env -> do
    o1 <- get
    fspec <- case mn of
        Nothing -> parseFspec lst
        Just n -> Fspec . mconcat . fmap word8ToBools . BS.unpack <$> parseBytes n
    o2 <- get
    items <- go $ zip (fspecBits fspec <> repeat False) lst
    o3 <- get
    let rawFspec = Bits (envInput env) o1 (coerce $ o2 - o1)
        rawItems = Bits (envInput env) o2 (coerce $ o3 - o2)
    pure $ psExpansion (envStore env) rawFspec rawItems items
  where
    go :: [(Bool, Maybe VNonSpare)]
        -> ParsingM a b nsp d e f [Maybe nsp]
    go [] = pure mempty
    go ((flag, spec) : xs)
        | not flag = (:) <$> pure Nothing <*> go xs
        | otherwise = case spec of
            Nothing -> parsingError "FX bit set for non-defined item."
            Just nsp -> (:) <$> (Just <$> parseNonSpare nsp) <*> go xs

parseRawDatablock :: ByteString -> Either ParsingError (RawDatablock, ByteString)
parseRawDatablock bs = do
    let n = BS.length bs
    when (n < 3) $ Left "overflow"
    let m = fromIntegral (BS.index bs 1) * 256 + fromIntegral (BS.index bs 2)
    when (m > n) $ Left "overflow"
    pure (RawDatablock $ BS.take m bs, BS.drop m bs)

parseRawDatablocks :: ByteString -> Either ParsingError [RawDatablock]
parseRawDatablocks bs
    | BS.null bs = pure []
    | otherwise = do
        (x, bs') <- parseRawDatablock bs
        (:) <$> pure x <*> parseRawDatablocks bs'

processDatablocks :: Map Word8 (RawDatablock -> r) -> ByteString
    -> Either ParsingError [Maybe r]
processDatablocks mapping bs = fmap go <$> parseRawDatablocks bs
  where
    go db = do
        f <- Map.lookup (categoryOf db) mapping
        pure $ f db

unparseRawDatablock :: RawDatablock -> Builder
unparseRawDatablock = BSB.byteString . unRawDatablock

