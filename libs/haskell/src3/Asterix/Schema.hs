{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- This module is using 'Data.Reflection' from 'reflection' package.
-- At the time of writing, it requires a 'patched' version of the package,
-- available here: https://github.com/zoranbosnjak/reflection/tree/T34
-- See also: https://github.com/ekmett/reflection/issues/34

module Asterix.Schema
( module Asterix.Schema
, module GHC.TypeLits
, module Data.Proxy
, module Data.Coerce
, module Data.Reflection
, module Data.Some
, module Data.Kind
) where

import           Data.Coerce
import           Data.Proxy
import           Data.Reflection
import           Data.Some
import           Data.String
import           Data.Text       (Text)
import           Data.Kind
import           GHC.TypeLits

-- | We don't statically know exact bit size for every type in Asterix,
-- however we know the byte alignment for every type. For example: The following
-- item when encoded is 12-bits long: .....ddd dddddddd d.......
-- The alignment in this case is A=3, B=7 and 3 bytes are used for encoding.
-- Another example item with the same alignment (4 bits long, 2 bytes required)
-- is: .....ddd d.......
class Alignment t where
    type A t :: Nat
    type B t :: Nat

type Aligned t = (A t ~ 0, B t ~ 0)

-- | Bit offset within one octet [0..7], where the number
-- represents actual number of bits of the left shift.
newtype BitOffsetMod8 = BitOffsetMod8 Int
    deriving (Show, Num)

bitOffsetMod8 :: Int -> BitOffsetMod8
bitOffsetMod8 = BitOffsetMod8 . flip mod 8

newtype BitSize = BitSize Int
    deriving (Show, Num)

-- | Used at type level
data TVariation
    = TElement
        Nat -- bit offsetMod
        Nat -- bit length
    | TGroup
        [TItem]
    | TCompound
        [Maybe TNonSpare] -- Nothing = Spare bit

-- | Used at type level
data TNonSpare = TNonSpare
    Symbol -- name
    Symbol -- title
    TVariation

-- | Used at type level
data TItem
    = TSpare
        Nat -- bit offsetMod8
        Nat -- bit length
    | TItem
        TNonSpare

-- | Helper structure, used at type level to avoid instance conflicts
data HVariation
    = HElement
    | HGroup
    | HCompound

-- | Term level structure
data VVariation
    = VElement BitOffsetMod8 BitSize
    | VGroup [VItem]
    | VCompound [Maybe VNonSpare]
    deriving (Show)

-- | Term level structure
data VNonSpare = VNonSpare Text Text VVariation
    deriving (Show)

-- | Term level structure
data VItem
    = VSpare BitOffsetMod8 BitSize
    | VItem VNonSpare
    deriving (Show)

-- Alignment instances

instance Alignment ('TElement o n) where
    type A ('TElement o n) = o
    type B ('TElement o n) = Mod (o+n) 8

instance Alignment ('TGroup '[]) where
    type A ('TGroup '[]) = 0
    type B ('TGroup '[]) = 0

instance
    ( B t ~ A ('TGroup ts) -- Alignment within group must match
    ) => Alignment ('TGroup (t ': ts)) where
    type A ('TGroup (t ': ts)) = A t
    type B ('TGroup (t ': ts)) = B ('TGroup ts)

instance Alignment ('TNonSpare name title var) where
    type A ('TNonSpare name title var) = A var
    type B ('TNonSpare name title var) = B var

instance Alignment ('TSpare o n) where
    type A ('TSpare o n) = o
    type B ('TSpare o n) = Mod (o+n) 8

instance Alignment ('TItem nsp) where
    type A ('TItem nsp) = A nsp
    type B ('TItem nsp) = B nsp

-- Type to term conversion

instance
    ( KnownNat o
    , KnownNat n
    ) => Reifies ('TElement o n) VVariation where
    reflect _ = VElement
        (BitOffsetMod8 $ fromIntegral $ reflect @o Proxy)
        (BitSize $ fromIntegral $ reflect @n Proxy)

instance Reifies '( 'HGroup, '[]) [VItem] where
    reflect _ = []

instance
    ( Reifies t VItem
    , Reifies '( 'HGroup, ts) [VItem]
    ) => Reifies '( 'HGroup, t ': ts) [VItem] where
    reflect _ = reflect @t Proxy : reflect @'( 'HGroup, ts) Proxy

instance
    ( Reifies '( 'HGroup, ts) [VItem]
    ) => Reifies ('TGroup ts) VVariation where
    reflect _ = VGroup (reflect @'( 'HGroup, ts) Proxy)

instance Reifies '( 'HCompound, '[]) [Maybe VNonSpare] where
    reflect _ = []

instance
    ( Reifies '( 'HCompound, ts) [Maybe VNonSpare]
    ) => Reifies '( 'HCompound, 'Nothing ': ts) [Maybe VNonSpare] where
    reflect _ = Nothing : reflect @'( 'HCompound, ts) Proxy

instance
    ( Reifies t VNonSpare
    , Aligned t
    , Reifies '( 'HCompound, ts) [Maybe VNonSpare]
    ) => Reifies '( 'HCompound, 'Just t ': ts) [Maybe VNonSpare] where
    reflect _ = Just (reflect @t Proxy) : reflect @'( 'HCompound, ts) Proxy

instance
    ( Reifies '( 'HCompound, ts) [Maybe VNonSpare]
    ) => Reifies ('TCompound ts) VVariation where
    reflect _ = VCompound (reflect @'(HCompound, ts) Proxy)

instance
    ( KnownSymbol name
    , KnownSymbol title
    , Reifies var VVariation
    ) => Reifies ('TNonSpare name title var) VNonSpare where
    reflect _ = VNonSpare
        (fromString $ reflect @name Proxy)
        (fromString $ reflect @title Proxy)
        (reflect @var Proxy)

instance
    ( KnownNat o
    , KnownNat n
    ) => Reifies ('TSpare o n) VItem where
    reflect _ = VSpare
        (BitOffsetMod8 $ fromIntegral $ reflect @o Proxy)
        (BitSize $ fromIntegral $ reflect @n Proxy)

instance
    ( Reifies nsp VNonSpare
    ) => Reifies ('TItem nsp) VItem where
    reflect _ = VItem (reflect @nsp Proxy)
