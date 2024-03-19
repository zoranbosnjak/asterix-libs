
{-# LANGUAGE DataKinds  #-}
-- {-# LANGUAGE LambdaCase #-}

module Asterix.Base
( module Asterix.Base
, module Asterix.Schema
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Bits
import           Data.Some
import           Data.GADT.Show

import           Asterix.Schema

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

-- | Binary value of arbitrary length.
newtype Value = Value ByteString
    deriving (Show)

integerToValue :: BitOffsetMod8 -> BitSize -> Integer -> Value
integerToValue (BitOffsetMod8 o) (BitSize n) =
    Value . BS.reverse . fst . BS.unfoldrN requiredBytes f . flip shift o
  where
    (a, b) = divMod (o+n) 8
    requiredBytes = case b of
        0 -> a
        _ -> succ a
    f x = Just (fromIntegral w, x') where
        (x', w) = divMod x 256

valueToInteger :: BitOffsetMod8 -> BitSize -> Value -> Integer
valueToInteger = undefined

data Variation t
    = Element Value
    | Group [Some Item]
    -- ... TODO
    | Compound [Maybe (Some NonSpare)]

deriving instance Show (Variation t)
instance GShow Variation where gshowsPrec = defaultGshowsPrec

data NonSpare t = NonSpare (Some Variation)

deriving instance Show (NonSpare t)
instance GShow NonSpare where gshowsPrec = defaultGshowsPrec

data Item t
    = Spare Value
    | Item (Some NonSpare)

deriving instance Show (Item t)
instance GShow Item where gshowsPrec = defaultGshowsPrec

-- | A fancy named pair of 2 values.
-- To be used with data filtering for speedup, for example:
--   - parse into 'Augmented ByteString Record'
--   - check Record by some predicate
--   - use ByteString directly, avoid unparsing 'Record'
data Augmented s a = Augmented s a
