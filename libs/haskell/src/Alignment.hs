-- |
-- Module : Alignment
--
-- Common definitions for bit sizes and alignments.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- TODO: remove this
--{-# OPTIONS_GHC -Wno-unused-imports #-}

module Alignment where

import           GHC.TypeLits

type EndOffsetMod8 o n = Mod (o + n) 8

-- | Forget about alignment of some structure with alignment.
data SomeA t = forall a b. SomeA (t a b)

deriving instance (forall a b. Show (t a b)) => Show (SomeA t)

-- | Helper class to unwrap Int from newtype wrappers
class UnWrapInt t where
    unWrapInt :: t -> Int

-- | Absolute bit offset from the start of input
newtype BitOffset = BitOffset Int deriving (Eq, Show)

instance UnWrapInt BitOffset where unWrapInt (BitOffset i) = i

-- | Bit offset modulo 8, that is [0..7], where 0 means byte aligned.
newtype BitOffsetMod8 = BitOffsetMod8 Int deriving (Eq, Show)

instance UnWrapInt BitOffsetMod8 where unWrapInt (BitOffsetMod8 i) = i

bitOffsetMod8 :: Int -> BitOffsetMod8
bitOffsetMod8 = BitOffsetMod8 . flip mod 8

instance Semigroup BitOffsetMod8 where
    BitOffsetMod8 a <> BitOffsetMod8 b = bitOffsetMod8 (a+b)

instance Monoid BitOffsetMod8 where
    mempty = BitOffsetMod8 0

-- | Size in bits.
newtype BitSize = BitSize Int deriving (Eq, Show)

instance UnWrapInt BitSize where unWrapInt (BitSize i) = i

instance Semigroup BitSize where
    BitSize a <> BitSize b = BitSize (a+b)

instance Monoid BitSize where
    mempty = BitSize 0

-- | Size in bytes.
newtype ByteSize = ByteSize Int deriving (Eq, Show)

instance UnWrapInt ByteSize where unWrapInt (ByteSize i) = i

instance Semigroup ByteSize where
    ByteSize a <> ByteSize b = ByteSize (a+b)

instance Monoid ByteSize where
    mempty = ByteSize 0

{- TODO: remove this
class HasBitLength t where
    bitLength :: t -> BitSize
-}
