-- |
-- Module : BitsBuilder
-- Bits builder

module BitsBuilder where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Monoid
import           Data.Word

import           Bits

-- | Bitstring builder, optimized for constructing.
data BitsBuilder = BitsBuilder (Endo [Bits]) !Int -- (endo, bit length)

instance Semigroup BitsBuilder where
    BitsBuilder f1 n1 <> BitsBuilder f2 n2 = BitsBuilder (f1 <> f2) (n1+n2)

instance Monoid BitsBuilder where
    mempty = BitsBuilder mempty 0

length :: BitsBuilder -> Int
length (BitsBuilder _endo n) = n

-- | Create BitsBuilder from Bits.
fromBits :: Bits -> BitsBuilder
fromBits b@(Bits bs1 o1 n1) = BitsBuilder (Endo f) n1 where
    f :: [Bits] -> [Bits]
    f [] = [b]
    f lst@(Bits bs2 o2 n2 : xs)
        -- optimization: adjacent bits, reuse bytestring
        | bs1 == bs2 && o1+n1 == o2 = Bits bs1 o1 (n1+n2) : xs
        -- optimization: byte aligned
        | mod (o1+n1) 8 == 0 = b : lst
        -- combine non-aligned bits, use bit mask
        | otherwise = undefined

-- | Create BitsBuilder from Word8.
word8 :: Word8 -> BitsBuilder
word8 = undefined

-- | Final converter to resulting ByteStrings and total byte length.
-- Proper bit alignment is asumed, that is: every list element is aligned.
toByteStrings :: BitsBuilder -> ([ByteString], Int)
toByteStrings (BitsBuilder (Endo f) ln) = (g <$> f [], div ln 8) where
    g (Bits bs o n) = BS.take (div n 8) $ BS.drop (div o 8) bs
