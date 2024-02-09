-- |
-- Module : Bits
-- Bits and Bytes manipulation

module Bits where

import           Data.Monoid
import qualified Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.String
import           Data.Word
import           Prelude         hiding (length, splitAt)

class HasLength t where
    bitLength :: t -> Int

-- | Bitstring, optimized for parsing.
-- Semigroup instance would not be efficient and is not provided.
-- Use Builder when general concatination is required.
data Bits = Bits !ByteString !Int !Int -- (data, bit offset, bit length)
    deriving (Show)

instance IsString Bits where
    fromString = mkBits . fromString

instance HasLength Bits where
    bitLength (Bits _s _o n) = n

-- | Bitstring builder, optimized for constructing.
data Builder = Builder (Endo [Bits]) !Int -- (endo, bit length)

instance Semigroup Builder where
    Builder f1 n1 <> Builder f2 n2 = Builder (f1 <> f2) (n1+n2)

instance Monoid Builder where
    mempty = Builder mempty 0

instance HasLength Builder where
    bitLength (Builder _endo n) = n

-- | Primary way to create Bits is from ByteString.
mkBits :: ByteString -> Bits
mkBits s = Bits s 0 (BS.length s * 8)

null :: Bits -> Bool
null = (<= 0) . bitLength

splitAt :: Int -> Bits -> (Bits, Bits)
splitAt x (Bits s o n) = (Bits s o x, Bits s (o+x) (n-x))

take :: Int -> Bits -> Bits
take x = fst . splitAt x

drop :: Int -> Bits -> Bits
drop x = snd . splitAt x

head :: Bits -> Bool
head (Bits s o _n) = Data.Bits.testBit (BS.index s a) (7-b)
  where
    (a, b) = divMod o 8

-- | Get word8 from bits, assume byte alignment.
indexByte :: Int -> Bits -> Word8
indexByte ix (Bits s o _n) = BS.index s (div o 8 + ix)

-- | Convert from Bits to a number, assume byte alignment.
getNumberAligned :: Num a => Bits -> a
getNumberAligned = go 0 where
    go !acc s
        | Bits.null s = acc
        | otherwise =
            let x = fromIntegral (indexByte 0 s)
            in go (acc*256 + x) (Bits.drop 8 s)

unpackWord8 :: Word8 -> [Bool]
unpackWord8 w = [Data.Bits.testBit w i | i <- [7,6..0]]

-- | Create Builder from Bits.
fromBits :: Bits -> Builder
fromBits b1@(Bits bs1 o1 n1) = Builder (Endo f) n1
  where
    f :: [Bits] -> [Bits]
    f [] = [b1]
    f lst@(Bits bs2 o2 n2 : xs)
        -- b1 is empty
        | n1 == 0 = lst
        -- optimization: adjacent bits, reuse bytestring
        | bs1 == bs2 && o1+n1 == o2 = Bits bs1 o1 (n1+n2) : xs
        | otherwise =
            let (i, j) = divMod (o1+n1) 8
            in case j of
                -- optimization: byte aligned situation
                0 -> b1 : lst
                -- combine non-aligned bits, use bit mask
                _ ->
                    let mLeft, mRight :: Word8 -- left and right bit mask
                        mRight = Data.Bits.shiftR 0xff j
                        mLeft = Data.Bits.complement mRight
                        bLeft  = (Data.Bits..&.) mLeft (BS.index bs1 i)
                        bRight = (Data.Bits..&.) mRight (BS.head bs2)
                        w = (Data.Bits..|.) bLeft bRight
                    in Bits bs1 o1 (i*8) : Bits (BS.singleton w) 0 8 : xs

-- | Create Builder from Word8.
word8 :: Word8 -> Builder
word8 x = Builder (Endo f) 8 where
    f :: [Bits] -> [Bits]
    f lst = Bits (BS.singleton x) 0 8 : lst

-- | Final converter to resulting ByteStrings (do not concatinate)
-- Proper bit alignment is asumed, that is: every list element is aligned.
toByteStrings :: Builder -> [ByteString]
toByteStrings (Builder (Endo f) _ln) = g <$> f [] where
    g (Bits bs o n) = BS.take (div n 8) $ BS.drop (div o 8) bs
