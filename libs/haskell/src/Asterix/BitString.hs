-- |
-- Module: Asterix.BitString
--
-- Bits and bytes manipulation module

{-# LANGUAGE LambdaCase #-}

module Asterix.BitString where

import           GHC.Stack
import           Data.Bits               (complement, shift, testBit, (.&.),
                                          (.|.))
import           Data.Coerce
import           Data.Bool
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as B16
import           Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8   as BS8
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.List               as L
import           Data.Maybe
import           Data.Word

-- | For function names overloading
class IsNumBits t where
    numBytes :: t -> Int
    numBits :: t -> Int

-- | Number of bits, but stored as 'divMod n 8'.
data NumBits = NumBits
    { _numBytes :: !Int
    , _numBits  :: !Int
    } deriving (Eq, Show)

numBitsToInt :: IsNumBits t => t -> Int
numBitsToInt x = numBytes x * 8 + numBits x

intToNumBits :: Coercible NumBits t => Int -> t
intToNumBits i = coerce (uncurry NumBits $ divMod i 8)

instance IsNumBits NumBits where
    numBytes = _numBytes
    numBits = _numBits

instance Num NumBits where
    a + b = intToNumBits (numBitsToInt a + numBitsToInt b)
    a * b = intToNumBits (numBitsToInt a * numBitsToInt b)
    abs = intToNumBits . abs . numBitsToInt
    signum = intToNumBits . signum . numBitsToInt
    fromInteger = intToNumBits . fromIntegral
    negate = intToNumBits . negate . numBitsToInt

instance Ord NumBits where
    compare (NumBits a1 a2) (NumBits b1 b2)
        = compare a1 b1
        <> compare a2 b2

newtype Offset = Offset NumBits deriving (Eq, Ord, Num, Show, IsNumBits)

newtype Size = Size NumBits deriving (Eq, Ord, Num, Show, IsNumBits)

-- | ByteString's bits fragment.
data Bits = Bits
    { bitsData   :: ByteString
    , bitsOffset :: !Offset
    , bitsSize   :: !Size
    } deriving Show

-- | ByteString Builder with known byte size.
data SBuilder = SBuilder
    { sbByteSize :: Int
    , sbData     :: Builder
    } deriving Show

-- | For tyes that contain bits.
class HasBits t where
    toBits :: t -> Bits

-- | For tyes that can be converted to/from ByteString.
-- Be aware that some conversions might be slow.
class IsByteString t where
    toByteString :: t -> ByteString
    fromByteString :: ByteString -> t

-- | Convert bytestring to hex representation.
hexlify :: BS.ByteString -> String
hexlify = BS8.unpack . B16.encode

-- | Convert hex representation back to a bytestring.
unhexlify :: String -> Maybe BS.ByteString
unhexlify = either (const Nothing) Just . B16.decode . BS8.pack

-- | Helper function for expression evaluation.
withAssumption :: HasCallStack => Bool -> a -> a
withAssumption False _  =
    error $ "Internal error (wrong assumption), " <> prettyCallStack callStack
withAssumption True val = val

-- | Conversion from 'Builder' to 'ByteString'.
builderToByteStringSlow :: Builder -> ByteString
builderToByteStringSlow = BSL.toStrict . BSB.toLazyByteString

-- | Conversion from 'ByteString' to unsigned number.
byteStringToNum :: Num a => ByteString -> a
byteStringToNum bs
    | BS.null bs = 0
    | otherwise =
        let (xs, x) = (BS.init bs, BS.last bs)
        in byteStringToNum xs * 256 + fromIntegral x

-- | Calculate 'left' and 'right' bit alignment.
alignment :: Bits -> (Int, Int)
alignment (Bits _ o n) = (a, b)
  where
    a = numBits o
    b = numBits (o + coerce n)

-- | Calculate 'left' bit alignment.
leftAlignment :: Bits -> Int
leftAlignment = fst . alignment

-- | Calculate 'right' bit alignment.
rightAlignment :: Bits -> Int
rightAlignment = snd . alignment

-- | Test whether 'Bits' are empty.
nullBits :: Bits -> Bool
nullBits = (<= 0) . bitsSize

-- | Convert from 'ByteString' to 'Bits'.
byteStringToBits :: ByteString -> Bits
byteStringToBits bs = Bits bs 0 (intToNumBits $ BS.length bs * 8)

-- | Calculate required bytes + additional bits.
requiredBytes :: Int -> Int -> (Int, Int)
requiredBytes o8 n = divMod (o8 + n) 8

-- |onvert 'Integer' to 'Bits'.
integerToBits :: Int -> Int -> Integer -> Bits
integerToBits o8 n val = Bits bs o (Size $ intToNumBits n)
  where
    o = Offset (NumBits 0 o8)
    (m, b) = withAssumption (n >= 0) requiredBytes o8 n
    m' = m + bool 1 0 (b == 0)
    shiftedVal
        | b == 0 = val
        | otherwise = shift val (8 - b)
    byteList x = \case
        0 -> []
        k ->
            let (x1, x2) = divMod x 256
            in fromInteger x2 : byteList x1 (pred k)
    bs = BS.pack (reverse $ byteList shiftedVal m')

-- | Split 'Word8' to 8 boolean flags.
word8ToBools :: Word8 -> [Bool]
word8ToBools w = [testBit w i | i <- [7,6..0]]

-- | Combine boolean flags to 'Word8'.
boolsToWord8 :: [Bool] -> Word8
boolsToWord8 = go . Prelude.reverse
  where
    go = \case
        [] -> 0
        (x:xs) -> go xs * 2 + bool 0 1 x

-- | Convert list of bool flags to bits.
boolsToBits :: Int -> [Bool] -> Bits
boolsToBits o8 lst = Bits bs (Offset $ intToNumBits o8) (Size $ intToNumBits n)
  where
    n = Prelude.length lst
    prefix = replicate o8 False
    bs = BS.pack $ byteList (prefix <> lst)
    byteList i =
        let (a, b) = splitAt 8 i
        in case Prelude.length a < 8 of
            True -> [boolsToWord8 $ take 8 $ a <> repeat False]
            False -> boolsToWord8 a : byteList b

-- | Calculate 'compact' version of Bits - helper function.
compactBits :: Bits -> (ByteString, Maybe Word8)
compactBits (Bits bs o n) = (bs', padding)
  where
    k = numBytes o
    (m, b) = requiredBytes (numBits o) (numBitsToInt n)
    bs' = BS.take m $ BS.drop k bs
    padding
        | b == 0 = Nothing
        | otherwise = Just $ BS.index bs (k+m)

-- | Append properly aligned bits.
appendBits :: Bits -> Bits -> Bits
appendBits s1 s2 = withAssumption (rightAlignment s1 == leftAlignment s2) go
  where
    (a1, a2) = compactBits s1
    (b1, b2) = compactBits s2
    padding = maybe BS.empty BS.singleton b2
    bs = case a2 of
        Nothing -> a1 <> b1 <> padding
        Just w1 ->
            let w2 = case BS.null b1 of
                    False -> BS.head b1
                    True  -> fromJust b2
                m = shift 0xff (- rightAlignment s1)
                w = (w1 .&. complement m) .|. (w2 .&. m)
            in a1 <> BS.singleton w <> bool (BS.tail b1) mempty (BS.null b1) <> padding
    o = Offset $ NumBits 0 $ leftAlignment s1
    n = bitsSize s1 + bitsSize s2
    go
        | nullBits s1 = s2
        | nullBits s2 = s1
        | otherwise = Bits bs o n

-- | Concatinate non-empty list of 'Bits'.
concatBits :: [Bits] -> Bits
concatBits = \case
    [] -> error "Empty list"
    [x] -> x
    x:xs -> appendBits x (Asterix.BitString.concatBits xs)

bitsToNum :: Integral a => Bits -> a
bitsToNum s = case n of
    0 -> 0
    _ ->
        let (bs, mw) = compactBits s
            val1 = byteStringToNum bs
            (a, b) = alignment s
            val2 = val1 * (2 ^ b) + fromIntegral (shift (fromJust mw) (- (8-b)))
        in case (a, b) of
            (0, 0) -> val1
            (_, 0) -> mod val1 (2 ^ n)
            (0, _) -> val2
            (_, _) -> mod val2 (2 ^ n)
  where
    n = numBitsToInt $ bitsSize s

-- | Extract 'Bits' to list of bool flags.
bitsToBools :: Bits -> [Bool]
bitsToBools (Bits bs o n') =
    let n = numBitsToInt n'
        o8 = numBits o
        (m, _b) = requiredBytes o8 n
        s = BS.take (succ m) $ BS.drop (numBytes o) bs
        lst = mconcat (word8ToBools <$> BS.unpack s)
    in Prelude.take n $ drop o8 lst

-- | Convert properly aligned 'Bits' to 'Builder'.
bitsToBuilder :: Bits -> Builder
bitsToBuilder s@(Bits bs o n') = withAssumption (alignment s == (0, 0)) bld
  where
    n = numBitsToInt n'
    o8 = numBits o
    (m, _b) = requiredBytes o8 n
    bld = BSB.byteString $ BS.take m $ BS.drop (numBytes o) bs

-- | Convert properly aligned 'Bits' to 'SBuilder'.
bitsToSBuilder :: Bits -> SBuilder
bitsToSBuilder arg = SBuilder
    (numBytes $ bitsSize arg)
    (bitsToBuilder arg) -- this call contains 'withAssumption'

-- | Convert 'Word8' to 'SBuilder'.
word8ToSBuilder :: Word8 -> SBuilder
word8ToSBuilder = SBuilder 1 . BSB.word8

-- | Show value as binary string.
debugBits :: HasBits t => t -> String
debugBits val = mconcat $ L.intersperse " " (goOctet <$> octets)
  where
    Bits bs o n' = toBits val
    n = numBitsToInt n'
    o8 = numBits o
    (m, b) = requiredBytes o8 n
    k = bool m (pred m) (b == 0)
    a = numBytes o
    octets = [a .. (a+k)]
    goOctet ix =
        let w = BS.index bs ix
        in do
            i <- [7,6..0]
            let j = 7 - i
                x = bool '0' '1' $ testBit w i
                o2 = Offset $ NumBits ix j
            pure $ bool '.' x (o2 >= o && o2 < (o + coerce n'))

instance Eq Bits where
    b1 == b2
        = numBits (bitsOffset b1) == numBits (bitsOffset b2)
        && bitsToBools b1 == bitsToBools b2

instance HasBits Bits where
    toBits = id

instance HasBits ByteString where
    toBits = byteStringToBits

instance HasBits Builder where
    toBits = toBits . BSL.toStrict . BSB.toLazyByteString

instance HasBits SBuilder where
    toBits = toBits . sbData

instance Semigroup SBuilder where
    SBuilder a1 a2 <> SBuilder b1 b2 = SBuilder (a1+b1) (a2 <> b2)

instance Monoid SBuilder where
    mempty = SBuilder 0 mempty

instance IsByteString ByteString where
    toByteString = id
    fromByteString = id

instance IsByteString Builder where
    toByteString = BSL.toStrict . BSB.toLazyByteString
    fromByteString = BSB.byteString

instance IsByteString SBuilder where
    toByteString = toByteString . sbData
    fromByteString s = SBuilder (BS.length s) (BSB.byteString s)

