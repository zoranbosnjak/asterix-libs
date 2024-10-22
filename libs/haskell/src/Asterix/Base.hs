module Asterix.Base where

import           Data.Kind
import           Data.Proxy
import           Data.Coerce
import           Unsafe.Coerce

data Some (t :: k -> Type) = forall s. Some (t s)

unSome :: Proxy s -> Some t -> t s
unSome _proxy (Some val) = unsafeCoerce val

newtype ByteOffset = ByteOffset Int
    deriving (Show, Eq, Ord, Enum, Num)
newtype BitOffset8 = BitOffset8 Int
    deriving (Show, Eq, Ord, Enum, Num)
newtype BitSize = BitSize Int
    deriving (Show, Eq, Ord, Enum, Num)
newtype ByteSize = ByteSize Int
    deriving (Show, Eq, Ord, Enum, Num)
data Offset = Offset !ByteOffset !BitOffset8 deriving (Eq)

instance Semigroup Offset where
    Offset (ByteOffset a1) (BitOffset8 b1) <>
        Offset (ByteOffset a2) (BitOffset8 b2) =
            Offset (coerce c1) (coerce c2)
      where
        (n, c2) = divMod (b1 + b2) 8
        c1 = a1 + a2 + n

instance Monoid Offset where
    mempty = Offset 0 0

instance Ord Offset where
    compare (Offset a1 b1) (Offset a2 b2) =
        compare a1 a2 <> compare b1 b2

bitOffset :: Int -> Offset
bitOffset n = Offset (coerce a) (coerce b)
    where (a, b) = divMod n 8

bitDelta :: Offset -> Offset -> BitSize
bitDelta
    (Offset (ByteOffset a2) (BitOffset8 b2))
    (Offset (ByteOffset a1) (BitOffset8 b1)) = coerce (o2 - o1)
  where
    o2 = a2*8+b2
    o1 = a1*8+b1

byteDelta :: ByteOffset -> ByteOffset -> ByteSize
byteDelta b a = coerce (b - a)
