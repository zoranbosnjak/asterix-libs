-- |
-- Module : Bitstring

-- TODO: check if all is required
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- TODO: remove this
{-# OPTIONS_GHC -Wno-unused-imports #-}

module BitString where

import           GHC.TypeLits
import qualified Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.String
import           Data.Word
import           Prelude         hiding (length, splitAt)

import           Alignment

-- | Bitstring, optimized for parsing.
-- Semigroup instance would not be efficient and is not provided.
-- Use Builder when general concatination is required.
-- 'a' and 'b' type parameters are left and right bit alignment
-- The 'Eq' would also not be efficient.
data BitString (a :: Nat) (b :: Nat)
    = BitString !ByteString !BitOffset !BitSize
    deriving (Show)

{-
-- | Create BitString from ByteString.
fromByteString :: ByteString -> BitString 0 0
fromByteString s = BitString s 0 (BS.length s * 8)

instance IsString (BitString 0 0) where
    fromString = fromByteString . fromString

instance HasBitLength (BitString a b) where
    bitLength (BitString _bytes _offset n) = n

instance HasBitLength (SomeA BitString) where
    bitLength (SomeA b) = bitLength b

null :: HasBitLength t => t -> Bool
null = (<= 0) . bitLength

{- -- Better with typeclass with 'a' and 'b' statically known
-- | Create BitString from unsigned integer.
uintegerToBits :: Int -> Int -> Int -> BitString
uintegerToBits _offset _n _value = undefined -- TODO: add tests
-}

{-
-- | Convert BitString to unsigned integer.
someBitsToUInteger :: BitString -> Int
someBitsToUInteger = undefined -- TODO: add tests

splitAt :: Int -> BitString -> (BitString, BitString)
splitAt x (BitString s o n) = (BitString s o x, BitString s (o+x) (n-x))

take :: Int -> BitString -> BitString
take x = fst . splitAt x

drop :: Int -> BitString -> BitString
drop x = snd . splitAt x

head :: BitString -> Bool
head (BitString s o _n) = Data.BitString.testBit (BS.index s a) (7-b)
  where
    (a, b) = divMod o 8

-- | Get word8 from bits, assume byte alignment.
indexByte :: Int -> BitString -> Word8
indexByte ix (BitString s o _n) = BS.index s (div o 8 + ix)

-- | Convert from BitString to a number, assume byte alignment.
getNumberAligned :: Num a => BitString -> a
getNumberAligned = go 0 where
    go !acc s
        | BitString.null s = acc
        | otherwise =
            let x = fromIntegral (indexByte 0 s)
            in go (acc*256 + x) (BitString.drop 8 s)

unpackWord8 :: Word8 -> [Bool]
unpackWord8 w = [Data.BitString.testBit w i | i <- [7,6..0]]
-}
-}
