-- |
-- Module : Bits
-- Bits and Bytes manipulation

module Bits where

import qualified Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.String
import           Data.Word
import           Prelude         hiding (length, splitAt)

-- | Bitstring, optimized for parsing.
-- Semigroup instance would not be efficient and is not provided.
-- Use BitsBuilder when general concatination is required.
data Bits = Bits !ByteString !Int !Int -- (data, bit offset, bit length)
    deriving (Show)

instance IsString Bits where
    fromString = mkBits . fromString

-- | Primary way to create Bits is from ByteString.
mkBits :: ByteString -> Bits
mkBits s = Bits s 0 (BS.length s * 8)

length :: Bits -> Int
length (Bits _s _o n) = n

null :: Bits -> Bool
null = (<= 0) . length

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
