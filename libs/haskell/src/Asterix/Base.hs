-- |
-- Module: Asterix.Base
--
-- Asterix base data structures and functions.

-- Some constraints seems redundant to ghc, but are actually required.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE MultiWayIf        #-}

module Asterix.Base where

import           GHC.Stack
import           Data.Coerce
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.RWS
import           Data.Char                 (chr, ord)
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
import           Data.Bool
import           Data.Kind                 (Type)

import           Asterix.Schema
import           Asterix.BitString

type family Fst t where Fst '(a, b) = a
type family Snd t where Snd '(a, b) = b

intError :: HasCallStack => a
intError = error $ "Internal error, " <> prettyCallStack callStack

-- Avoid partial warning of the original 'head' function
head :: [a] -> a
head = \case
    [] -> error "empty list"
    x : _ -> x

-- Avoid partial warning of the original 'tail' function
tail :: [a] -> [a]
tail = drop 1

-- | Number of bits per character of different string types.
bitsPerChar :: VStringType -> Int
bitsPerChar = \case
    GStringAscii -> 8
    GStringICAO -> 6
    GStringOctal -> 3

-- | Evaluate GZ number to Integer.
evalGz :: VZ -> Integer
evalGz (GZ pm i) = fromIntegral $ i * case pm of
    GPlus -> 1
    GMinus -> -1

-- | Evaluate VNumber
evalNum :: Fractional a => VNumber -> a
evalNum = \case
    GNumInt gz -> fromIntegral $ evalGz gz
    GNumDiv a b -> evalNum a / evalNum b
    GNumPow a b -> fromIntegral (evalGz a ^ evalGz b)

asUint :: forall b a. (Unparsing Bits a, Integral b) => a -> b
asUint = bitsToNum . unparse

bitsToString :: VStringType -> Int -> Bits -> String
bitsToString st bitSize =
    let bpc :: Int
        bpc = bitsPerChar st

        toChar :: Int -> Char
        toChar x = case st of
            GStringAscii -> chr x
            GStringICAO -> if
                | x >= 0x01 && x <= 0x1A -> chr (ord 'A' + x - 0x01)
                | x == 0x20 -> ' '
                | x >= 0x30 && x <= 0x39 -> chr (ord '0' + x - 0x30)
                | otherwise -> '?'
            GStringOctal -> chr (ord '0' + x)

        p = 2 ^ bpc
        n = div bitSize bpc

        go :: Int -> Integer -> String
        go cnt x
            | cnt > 0 =
                let (y, i) = divMod x p
                    c = toChar $ fromIntegral i
                in c : go (pred cnt) y
            | cnt == 0 = ""
            | otherwise = intError
    in reverse . go n . bitsToNum @Integer

applySignedness :: (Integral b, Num a, Ord a) => GSignedness -> b -> a -> a
applySignedness sig n val = case sig of
    GUnsigned -> val
    GSigned -> withAssumption (n > 0) $
        let half = 2 ^ (n-1)
        in case val < half of
            True -> val
            False -> val - (2 * half)

bitsToInteger :: VSignedness -> Int -> Bits -> Integer
bitsToInteger sig n
    = applySignedness sig n
    . bitsToNum @Integer

bitsToDouble :: VSignedness -> Int -> Double -> Bits -> Double
bitsToDouble sig n lsb
    = applyLsb
    . applySignedness sig n
    . bitsToNum @Integer
  where
    applyLsb = (* lsb) . fromIntegral

-- | Conversion from typelevel to value level bool.
class KnownBool t where boolVal :: Proxy t -> Bool
instance KnownBool 'False where boolVal _ = False
instance KnownBool 'True where boolVal _ = True

-- Hlist
data HList t where
    HNil :: HList '[]
    HCons :: t -> HList ts -> HList (t ': ts)

nil :: HList '[]
nil = HNil

infixr 5 *:
(*:) :: x -> HList xs -> HList (x ': xs)
(*:) = HCons

hlUncons :: HList (t ': ts) -> (t, HList ts)
hlUncons (HCons x xs) = (x, xs)

hlHead :: HList (c : ts) -> c
hlHead = fst . hlUncons

hlTail :: HList (a : ts) -> HList ts
hlTail = snd . hlUncons

-- | Split HList based on type level index
class HListSplit (ix :: [Type]) (ts :: [Type]) where
    type HListSplitL ix ts :: [Type]
    type HListSplitR ix ts :: [Type]
    hListSplit :: Proxy ix -> HList ts
        -> (HList (HListSplitL ix ts), HList (HListSplitR ix ts))

instance HListSplit '[] ts where
    type HListSplitL '[] ts = '[]
    type HListSplitR '[] ts = ts
    hListSplit _ lst = (nil, lst)

instance
    ( t1 ~ t2
    , HListSplit ts1 ts2
    ) => HListSplit (t1 ': ts1) (t2 ': ts2) where
    type HListSplitL (t1 ': ts1) (t2 ': ts2) = t1 ': HListSplitL ts1 ts2
    type HListSplitR (t1 ': ts1) (t2 ': ts2) = HListSplitR ts1 ts2
    hListSplit _ (HCons x xs) =
        let (a, b) = hListSplit (Proxy @ts1) xs
        in (x *: a, b)

-- Append 2 HLists
class HListAppend l1 l2 where
    type HListAppendR l1 l2 :: [Type]
    hListAppend :: HList l1 -> HList l2 -> HList (HListAppendR l1 l2)

instance HListAppend '[] l2 where
    type HListAppendR '[] l2 = l2
    hListAppend HNil l = l

instance HListAppend ts l2 => HListAppend (t ': ts) l2 where
    type HListAppendR (t ': ts) l2 = t ': HListAppendR ts l2
    hListAppend (HCons x xs) l2 = HCons x (hListAppend xs l2)

-- Fold HList by converting each element to a Monoid
-- and combining the results.
-- A 'c' represents a constraint on each element of HList.
-- A caller of 'foldHList' should provide 'c' as type application,
-- for example: foldHList @Show (\x -> [show x]) someHList,
-- would convert HList to [String], given that each element has
-- Show instance.
class FoldHList c t where
    foldHList :: Monoid r => (forall a. c a => a -> r) -> HList t -> r

instance FoldHList c '[] where
    foldHList _f HNil = mempty

instance
    ( c t
    , FoldHList c ts
    ) => FoldHList c (t ': ts) where
    foldHList f (HCons x xs) = f x <> foldHList @c f xs

class Unparsing r t where
    unparse :: t -> r

instance Unparsing Bits Bits where
    unparse = id

instance Unparsing Bits ByteString where
    unparse = byteStringToBits

instance Unparsing SBuilder ByteString where
    unparse = fromByteString

-- | If we can unparse to bits, we can also get a number
unparseToNum :: (Unparsing Bits t, Integral a) => t -> a
unparseToNum = bitsToNum . unparse

type ItemName = Text

newtype RawDatablock = RawDatablock { unRawDatablock :: ByteString }
    deriving (Eq, Show)

instance Unparsing SBuilder RawDatablock where
    unparse = fromByteString . unRawDatablock

-- | Fspec flag bits, without fx bits and without trailing bits
newtype Fspec = Fspec { fspecBits :: [Bool] }
    deriving Show

type FRN = Word8

data ParsingMode
    = StrictParsing
    | PartialParsing
    deriving (Eq, Enum, Bounded, Show)

class KnownParsingMode t where parsingMode :: Proxy t -> ParsingMode
instance KnownParsingMode 'StrictParsing where parsingMode _ = StrictParsing
instance KnownParsingMode 'PartialParsing where parsingMode _ = PartialParsing

data RecordItem nsp
    = RecordItem nsp
    | RecordItemSpare
    | RecordItemRFS [(FRN, nsp)]
    deriving (Eq, Show)

-- All 'Nothing' items represent FX which must be '1',
-- except for the last which must be '0' (if FX).
recreateExtended :: Unparsing Bits i => [Maybe i] -> Bits
recreateExtended = \case
    [] -> intError
    [Nothing] -> setFx False
    [Just i] -> unparse @Bits i
    (x:xs) -> case x of
        Nothing -> setFx True `appendBits` recreateExtended xs
        Just i -> unparse @Bits i `appendBits` recreateExtended xs
  where
    setFx = integerToBits 7 1 . bool 0 1

-- | Create raw fspec from given binary flags, put fx in between.
mkFspecFx :: [Bool] -> SBuilder
mkFspecFx
    = bitsToSBuilder
    . boolsToBits 0
    . mconcat
    . reverse
    . zipWith addFx (False : repeat True)
    . keepAtLeastOne
    . dropWhile (== replicate 7 False)
    . reverse
    . splitToGroupsOf7
  where
    splitToGroupsOf7 :: [Bool] -> [[Bool]]
    splitToGroupsOf7 lst
        | Prelude.length lst <= 7 = [take 7 (lst <> repeat False)]
        | otherwise = take 7 lst : splitToGroupsOf7 (drop 7 lst)

    addFx :: Bool -> [Bool] -> [Bool]
    addFx fxBit lst = lst <> [fxBit]

    keepAtLeastOne :: [[Bool]] -> [[Bool]]
    keepAtLeastOne = \case
        [] -> [replicate 7 False]
        lst -> lst

-- | Create fixed size raw fspec from given binary flags (no fx).
mkFspecFixed :: Int -> [Bool] -> SBuilder
mkFspecFixed nBytes
    = bitsToSBuilder
    . boolsToBits 0
    . take (nBytes * 8)
    . (<> repeat False)

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
data Env (pm :: ParsingMode) var rv nsp item rec exp = Env
    { envStore :: ParsingStore var rv nsp item rec exp
    , envInput :: ByteString
    }

rawDatablockCategory :: RawDatablock -> Word8
rawDatablockCategory (RawDatablock bs) = BS.index bs 0

getRawRecords :: RawDatablock -> ByteString
getRawRecords (RawDatablock bs) = BS.drop 3 bs

newtype ParsingError = ParsingError Text
    deriving (Show, IsString)

-- | Parsing monad
type ParsingM pm a b c d e f = RWST (Env pm a b c d e f) () Offset (Either ParsingError)

runParsing :: Monad m => RWST r w s m a -> r -> s -> m (a, s)
runParsing act r s = do
    (a, s', _w) <- runRWST act r s
    pure (a, s')

parsingError :: ParsingError -> ParsingM pm a b c d e f r
parsingError = lift . Left

-- | Check bit alignment
aligned :: Offset -> Int -> Bool
aligned o o8 = numBits o == o8

-- | End of a bytestring
endOffset :: ByteString -> Offset
endOffset = intToNumBits . (*8) . BS.length

-- | End of input
eof :: ParsingM pm a b c d e f Bool
eof = do
    o <- get
    o2 <- asks (endOffset . envInput)
    case compare o o2 of
        LT -> pure False
        EQ -> pure True
        GT -> intError

-- | Move offset pointer by 'n' bits.
moveOffset :: Size -> ParsingM pm a b c d e f ()
moveOffset n = do
    bs <- asks envInput
    o <- get
    let o' = o + coerce n
    when (o' > endOffset bs) $ parsingError "overflow"
    put o'

-- | Fetch some number of bits.
parseBits :: Int -> Size -> ParsingM pm a b c d e f Bits
parseBits o8 n = do
    bs <- asks envInput
    o <- get
    withAssumption (aligned o o8) $ do
        moveOffset n
        pure $ Bits bs o n

-- | Parse Word8
parseWord8 :: ParsingM pm a b c d e f Word8
parseWord8 = do
    bs <- asks envInput
    o <- get
    withAssumption (aligned o 0) $ do
        moveOffset 8
        pure $ BS.index bs (numBytes o)

-- | Parse 'n' bytes
parseBytes :: Int -> ParsingM pm a b c d e f ByteString
parseBytes n = do
    bs <- asks envInput
    o <- get
    withAssumption (aligned o 0) $ do
        moveOffset $ intToNumBits $ n * 8
        pure $ BS.take n $ BS.drop (numBytes o) bs

-- | Parse 'fx' bit
parseFx :: ParsingM pm a b c d e f Bool
parseFx = do
    Bits bs o _ <- parseBits 7 1
    pure $ testBit (BS.index bs (numBytes o)) 0

-- | Parse complete fspec, take fx bits into account
-- The ParsingMode is present in the environment, which is ignored in this
-- function. Explicit parameter is used instead. The reason is that a
-- caller can override mode, for example... fspec in compound is always
-- strict and for the record it depends on user preferences.
parseFspec :: ParsingMode -> Int -> ParsingM pm a b c d e f Fspec
parseFspec pm definedItems = do
    result <- go
    let n = length result
        (a, b) = divMod definedItems 7
        maxSize
            | b == 0 = definedItems
            | otherwise = (a+1) * 7
    when (n == 0) $ parsingError "empty fspec"
    case pm of
        StrictParsing -> when (n > maxSize) $ parsingError "fspec too big"
        PartialParsing -> pure ()
    pure $ Fspec $ take definedItems result
  where
    go :: ParsingM pm a b c d e f [Bool]
    go = do
        w <- word8ToBools <$> parseWord8
        let (flags, fx) = (init w, last w)
        case fx of
            False -> pure flags
            True  -> (<>) <$> pure flags <*> go

parseVariation :: VVariation -> ParsingM pm var b c d e f var
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
                True -> case xs of
                    [] -> parsingError "last FX bit is expected to be zero"
                    _ -> (:) <$> pure Nothing <*> go xs
            Just x -> ((:) . Just <$> parseItem x) <*> go xs
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
            0 -> parsingError "unexpected size of explicit item"
            _ -> parseBytes (fromIntegral $ pred n)
        o3 <- get
        let b1 = Bits (envInput env) o1 (coerce $ o3 - o1)
            b2 = Bits (envInput env) o2 (coerce $ o3 - o2)
        pure $ psExplicit (envStore env) b1 b2
    GCompound lst -> do
        o1 <- get
        fspec <- parseFspec StrictParsing (length lst)
        o2 <- get
        items <- go $ zip (fspecBits fspec <> repeat False) lst
        o3 <- get
        let rawFspec = Bits (envInput env) o1 (coerce $ o2 - o1)
            rawItems = Bits (envInput env) o2 (coerce $ o3 - o2)
        pure $ psCompound (envStore env) rawFspec rawItems items
      where
        go :: [(Bool, Maybe VNonSpare)]
            -> ParsingM pm a b nsp d e f [Maybe nsp]
        go [] = pure mempty
        go ((flag,spec) : xs)
            | not flag = (:) <$> pure Nothing <*> go xs
            | otherwise = case spec of
                Nothing -> parsingError "FX bit set for non-defined item"
                Just nsp -> ((:) . Just <$> parseNonSpare nsp) <*> go xs

parseRuleVariation :: VRule VVariation
    -> ParsingM pm a rv c d e f rv
parseRuleVariation sch1 = ask >>= \env -> do
    let sch2 = case sch1 of
            GContextFree sch   -> sch
            GDependent _ sch _ -> sch
    psRuleVar (envStore env) <$> parseVariation sch2

parseNonSpare :: VNonSpare -> ParsingM pm var rv nsp d e f nsp
parseNonSpare (GNonSpare _name _title rvSch) = ask >>= \env -> do
    psNsp (envStore env) <$> parseRuleVariation rvSch

parseItem :: VItem -> ParsingM pm a b c item e f item
parseItem sch = ask >>= \env -> case sch of
    GSpare o n -> psSpare (envStore env) <$> parseBits o (intToNumBits n)
    GItem nsp  -> psItem (envStore env) <$> parseNonSpare nsp

parseRecord :: forall pm a b c d rec f.
    ( KnownParsingMode pm
    ) => VRecord -> ParsingM pm a b c d rec f rec
parseRecord (GRecord lst) = ask >>= \env -> do
    o1 <- get
    fspec <- parseFspec pm (length lst)
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
    pm :: ParsingMode
    pm = parsingMode @pm Proxy

    findSchema :: FRN -> [VUapItem] -> Maybe VNonSpare
    findSchema _ [] = Nothing
    findSchema n (x:xs)
        | n == 0 = case x of
            GUapItem nsp -> Just nsp
            _            -> Nothing
        | otherwise = findSchema (pred n) xs

    goRfs :: ParsingM pm a b nsp d e f ([(FRN, nsp)], Bool)
    goRfs = parseWord8 >>= go
      where
        go :: Word8 -> ParsingM pm a b nsp d e f ([(FRN, nsp)], Bool)
        go = \case
            0 -> pure (mempty, True)
            cnt -> do
                frn <- parseWord8 >>= \case
                    0 -> parsingError "invalid FRN"
                    n -> pure n
                nsp <- maybe
                    (parsingError "RFS subitem not defined")
                    pure (findSchema (pred frn) lst)
                env <- ask
                offset <- get
                case runParsing (parseNonSpare nsp) env offset of
                    Left err -> case pm of
                        StrictParsing  -> parsingError err
                        PartialParsing -> pure (mempty, False)
                    Right (x, offset') -> do
                        put offset'
                        (xs, clean) <- go (pred cnt)
                        pure ((frn, x) : xs, clean)

    goItems :: [Bool] -> [VUapItem]
        -> ParsingM pm a b nsp d e f ([Maybe (RecordItem nsp)], Bool)
    goItems [] [] = pure (mempty, True)
    goItems [] (_:ts) = do
        (items, clean) <- goItems [] ts
        pure (Nothing : items, clean)
    goItems _flags [] = case pm of
        StrictParsing -> parsingError "record subitem not defined"
        PartialParsing -> pure (mempty, False)
    goItems (flag:flags) (spec:specs)
        | not flag = do
            (items, clean) <- goItems flags specs
            pure (Nothing : items, clean)
        | otherwise = case spec of
            GUapItemSpare -> case pm of
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
                    Left err -> case pm of
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
parseRecords ::
    ( pm ~ StrictParsing -- only makes sense with StrictParsing
    ) => VRecord -> ParsingM pm a b c d rec f [rec]
parseRecords sch = eof >>= \case
    True -> pure []
    False -> (:) <$> parseRecord sch <*> parseRecords sch

-- | Try to parse multiple UAP combinations.
-- This function consumes complete input in any case.
parseRecordsTry ::
    ( pm ~ StrictParsing -- only makes sense with StrictParsing
    ) => [(VText, VRecord)] -> ParsingM pm a b c d rec f [[(VText, rec)]]
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

parseExpansion :: VExpansion -> ParsingM pm a b c d e exp exp
parseExpansion (GExpansion mn lst) = ask >>= \env -> do
    o1 <- get
    fspec <- case mn of
        Nothing -> parseFspec StrictParsing (length lst)
        Just n -> Fspec . mconcat . fmap word8ToBools . BS.unpack <$> parseBytes n
    o2 <- get
    items <- go $ zip (fspecBits fspec <> repeat False) lst
    o3 <- get
    let rawFspec = Bits (envInput env) o1 (coerce $ o2 - o1)
        rawItems = Bits (envInput env) o2 (coerce $ o3 - o2)
    pure $ psExpansion (envStore env) rawFspec rawItems items
  where
    go :: [(Bool, Maybe VNonSpare)]
        -> ParsingM pm a b nsp d e f [Maybe nsp]
    go [] = pure mempty
    go ((flag, spec) : xs)
        | not flag = (:) <$> pure Nothing <*> go xs
        | otherwise = case spec of
            Nothing -> parsingError "FX bit set for non-defined item"
            Just nsp -> ((:) . Just <$> parseNonSpare nsp) <*> go xs

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
        f <- Map.lookup (rawDatablockCategory db) mapping
        pure $ f db

unparseRawDatablock :: RawDatablock -> Builder
unparseRawDatablock = BSB.byteString . unRawDatablock

