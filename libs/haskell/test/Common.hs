{-# LANGUAGE LambdaCase #-}

module Common where

import           Data.Char
import           Data.Maybe
import           Test.Tasty.HUnit

import           Asterix.Base
import           Asterix.Coding

approximately :: (Ord a, Fractional a) => a -> a -> a -> Bool
approximately err a b = abs (b - a) / a < err

assertApproximately :: (Ord a, Fractional a) =>
    String -> a -> a -> a -> Assertion
assertApproximately name err a b = assertEqual name True
    (approximately err a b)

assertUint :: Unparsing Bits t => Integer -> t -> Assertion
assertUint n obj = assertEqual "uint" n (asUint obj)

assertUnparse :: Unparsing Bits t => String -> t -> Assertion
assertUnparse s obj = assertEqual "unparse"
        (debugBits @Bits $ byteStringToBits (fromJust $ unhexlify s))
        (debugBits @Bits $ unparse obj)

data StResult
    = Bin String
    | Hex String

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

