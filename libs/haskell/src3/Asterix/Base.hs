{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Base asterix processing module

module Asterix.Base
( module Asterix.Base
, module Asterix.Schema
, module Data.Tagged
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Bits
import           Data.Some
import           Data.Tagged
import           Data.Text (Text)

import           Asterix.Schema

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

-- | Data structure for fully parsed asterix item/variation
-- For type safe version, use 'Tagged t Item'.
data Item
    = Atom Value -- spare, element, explicit
    | ItemList [Item] -- group, extended, repetitive
    | ItemMap [(Text, Item)] -- compound
    deriving (Show)

-- | For types that we know offset and bit size statically
class Fixed t where
    type FixedOffset8 t :: Nat
    type FixedSize t :: Nat

instance Fixed ('TElement o n) where
    type FixedOffset8 ('TElement o n) = o
    type FixedSize ('TElement o n) = n

instance Fixed var => Fixed ('TNonSpare name title var) where
    type FixedOffset8 ('TNonSpare name title var) = FixedOffset8 var
    type FixedSize ('TNonSpare name title var) = FixedSize var

instance Fixed nsp => Fixed ('TItem nsp) where
    type FixedOffset8 ('TItem nsp) = FixedOffset8 nsp
    type FixedSize ('TItem nsp) = FixedSize nsp

-- | For 'Fixed' types only. The Num instance for 'Tagged' already exists.
instance {-# Overlapping #-}
    ( Fixed t
    , KnownNat (FixedOffset8 t)
    , KnownNat (FixedSize t)
    ) => Num (Tagged t Item) where
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = Tagged . Atom . integerToValue
        (bitOffsetMod8 (fromIntegral $ natVal (Proxy @(FixedOffset8 t))))
        (BitSize (fromIntegral $ natVal (Proxy @(FixedSize t))))

-- | Heterogenous list of Items, used to create either 'group' or 'compound'
data IList (ts :: [k]) where
    INil :: IList '[]
    ICons :: Tagged t Item -> IList ts -> IList (t ': ts)

nil :: IList '[]
nil = INil

infixr 5 &:
(&:) :: Tagged t Item -> IList ts -> IList (t : ts)
(&:) = ICons

-- | Create Group out of 'IList'
group :: IList ts -> Tagged ('TGroup ts) Item
group = Tagged . ItemList . unIList
  where
    unIList :: IList ts -> [Item]
    unIList = \case
        INil -> []
        ICons x xs -> unTagged x : unIList xs

-- | Create Compound out of 'IList'
compound :: a
compound = undefined -- TODO

item :: forall name title t. Tagged t Item -> Tagged ('TItem ('TNonSpare name title t)) Item
item = retag

-- TODO: overload 'item' name, to be able to return either
--     - Tagged ('TNonSpare... or
--     - Tagged ('TItem ('TNonSpare...
-- ...such that 'nonSpare' function is not required
nonSpare :: forall name title t. Tagged t Item -> Tagged ('TNonSpare name title t) Item
nonSpare = retag
