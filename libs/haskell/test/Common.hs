{-# LANGUAGE LambdaCase #-}

module Common where

import           Data.Base16.Types      (assertBase16, extractBase16)
import           Data.Bool
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Char
import qualified Data.List.NonEmpty     as NE
import           Data.Text              (Text)
import           Test.Tasty.HUnit

import           Asterix.Base
import           Asterix.Coding

-- | Convert bytestring to hex representation.
hexlify :: BS.ByteString -> Text
hexlify = extractBase16 . B16.encodeBase16

-- | Convert hex representation to bytestring.
unhexlify :: Text -> BS.ByteString
unhexlify = B16.decodeBase16' . assertBase16

approximately :: (Ord a, Fractional a) => a -> a -> a -> Bool
approximately err a b = abs (b - a) / a < err

assertApproximately :: (Ord a, Fractional a) =>
    String -> a -> a -> a -> Assertion
assertApproximately name err a b = assertEqual name True
    (approximately err a b)

assertUint :: Unparsing Bits t => Integer -> t -> Assertion
assertUint n obj = assertEqual "uint" n (asUint obj)

assertUnparse :: Unparsing Bits t => Text -> t -> Assertion
assertUnparse s obj = assertEqual "unparse"
        (debugBits @Bits $ byteStringToBits (unhexlify s))
        (debugBits @Bits $ unparse obj)

assertOne :: [a] -> IO a
assertOne [x] = pure x
assertOne _   = assertFailure "expecting list of length 1"

data StResult
    = Bin Text
    | Hex Text

checkBits :: Unparsing Bits a => String -> a -> StResult -> Assertion
checkBits name x = \case
    Bin y -> assertEqual name y (debugBits s)
    Hex y -> assertEqual name y
        (hexlify $ builderToByteStringSlow $ bitsToBuilder s)
  where
    s = unparse x

rStrip :: String -> String
rStrip
    = reverse
    . dropWhile isSpace
    . reverse

-- | Create record with all items set to zero/one.
populateRecord :: Bool -> VRecord -> URecord
populateRecord val (GRecord schItems) = URecord bld items
  where
    bld :: SBuilder
    bld = rebuildRecord items

    items :: [Maybe (RecordItem UNonSpare)]
    items = fmap goUapItem schItems

    goUapItem :: VUapItem -> Maybe (RecordItem UNonSpare)
    goUapItem = \case
        GUapItem nsp -> Just . RecordItem $ goNsp nsp
        _ -> Nothing

    goNsp :: VNonSpare -> UNonSpare
    goNsp (GNonSpare _name _title rv) = UNonSpare $ goRuleVar rv

    goRuleVar :: VRule VVariation -> URuleVar
    goRuleVar sch = URuleVar $ goVar $ case sch of
        GContextFree var   -> var
        GDependent _ var _ -> var

    goVar :: VVariation -> UVariation
    goVar = \case
        GElement o n _rc -> UElement $ integerToBits o n (bool 0 (-1) val)
        GGroup _o lst -> UGroup $ fmap goItem lst
        GExtended lst ->
            let extItems = [fmap goItem i | i <- lst]
                extBld = bitsToSBuilder $ recreateExtended extItems
            in UExtended extBld extItems
        GRepetitive rt var ->
            let repVar = goVar var
                repLst1 = replicate 9 (goVar var)
            in case rt of
                GRepetitiveRegular n ->
                    let repLst2 = repVar : repLst1
                        repBld = rebuildRepetitiveRegular n repLst2
                    in URepetitive repBld repLst2
                GRepetitiveFx ->
                    let repLst2 = repVar NE.:| repLst1
                        repBld = rebuildRepetitiveFx repLst2
                    in URepetitive repBld (NE.toList repLst2)
        GExplicit _met ->
            let expBits = byteStringToBits mempty
                expN = bitsToSBuilder $ integerToBits 0 8 1
                expBld = expN <> bitsToSBuilder expBits
            in UExplicit expBld expBits
        GCompound lst ->
            let compItems = [fmap goNsp i | i <- lst]
                compBld = rebuildCompound compItems
            in UCompound compBld compItems

    goItem :: VItem -> UItem
    goItem = \case
        GSpare o n -> USpare $ integerToBits o n 0
        GItem nsp -> UItem $ goNsp nsp

-- | Generate sample records from the given spec.
-- A result is a list (in case of multiple UAPs) of element, where each element
-- is a tuple (Optional[uap name], record with zeros, record with ones in each item).
sampleRecords :: VAsterix -> [(VInt, ((Maybe VText, VRecord), (URecord, URecord)))]
sampleRecords = \case
    GAsterixBasic cat _ed uap -> case uap of
        GUap sch        -> go cat (Nothing, sch)
        GUaps lst _mSel -> lst >>= \(name, sch) -> go cat (Just name, sch)
    GAsterixExpansion {} -> []
  where
    r1 = populateRecord False
    r2 = populateRecord True
    go :: Int -> (Maybe VText, VRecord)
        -> [(Int, ((Maybe VText, VRecord), (URecord, URecord)))]
    go cat (mName, sch) = [(cat, ((mName, sch), (r1 sch, r2 sch)))]

