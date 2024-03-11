-- |
-- Module : Alignment
--
-- Common definitions for bit sizes and alignments.

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Alignment where

import           GHC.TypeLits

type EndOffsetMod8 o n = Mod (o + n) 8

-- | Forget about alignment of some structure with alignment.
data SomeA t = forall a b. SomeA (t a b)

deriving instance (forall a b. Show (t a b)) => Show (SomeA t)

-- | Absolute bit offset from the start of input
newtype BitOffset = BitOffset Int deriving (Eq, Show, Num)

-- | Bit offset modulo 8, that is [0..7], where 0 means byte aligned.
newtype BitOffsetMod8 = BitOffsetMod8 Int deriving (Eq, Show, Num)

bitOffsetMod8 :: Int -> BitOffsetMod8
bitOffsetMod8 = BitOffsetMod8 . flip mod 8

instance Semigroup BitOffsetMod8 where
    BitOffsetMod8 a <> BitOffsetMod8 b = bitOffsetMod8 (a+b)

instance Monoid BitOffsetMod8 where
    mempty = BitOffsetMod8 0

-- | Size in bits.
newtype BitSize = BitSize Int deriving (Eq, Show, Num)

instance Semigroup BitSize where
    BitSize a <> BitSize b = BitSize (a+b)

instance Monoid BitSize where
    mempty = BitSize 0

-- | Size in bytes.
newtype ByteSize = ByteSize Int deriving (Eq, Show)

instance Semigroup ByteSize where
    ByteSize a <> ByteSize b = ByteSize (a+b)

instance Monoid ByteSize where
    mempty = ByteSize 0

class HasBitLength t where
    bitLength :: t -> BitSize

class HasByteLength t where
    byteLength :: t -> ByteSize
