{-# LANGUAGE DataKinds #-}

module Asterix.Rx where

import GHC.TypeLits
import Data.Kind
import Data.Proxy
import Unsafe.Coerce

import Asterix.Schema

data Some (t :: k -> Type) = forall s. Some (t s)

unSome :: Proxy s -> Some t -> t s
unSome _proxy (Some val) = unsafeCoerce val

type ByteOffset = Int
type BitOffset8 = Int
type BitSize = Int
type ByteSize = Int

data Rule b (t :: TRule a) where
    ContextFree :: b -> Rule b ('GContextFree c)
    Dependent :: [b] -> Rule b ('GDependent c d e)

data Variation (t :: TVariation) where
    Element :: !ByteOffset -> !BitOffset8 -> !BitSize -> Variation ('GElement o n rule)
    Group :: !ByteOffset -> !BitOffset8 -> !BitSize -> [Some Item] -> Variation ('GGroup o lst)
    Extended :: !ByteOffset -> !ByteSize -> [[Some Item]] -> Variation ('GExtended lst)
    Repetitive :: !ByteOffset -> !ByteSize -> [Some Variation] -> Variation ('GRepetitive rt var)
    Explicit :: !ByteOffset -> !ByteSize -> Variation ('GExplicit met)
    Compound :: !ByteOffset -> !ByteSize -> [Some NonSpare] -> Variation ('GCompound lst)

data Item (t :: TItem) where
    Spare :: !ByteOffset -> !BitOffset8 -> !BitSize -> Item ('GSpare o n)
    Item :: Some NonSpare -> Item ('GItem nsp)

data NonSpare (t :: TNonSpare) where
    NonSpare :: Some (Rule (Some Variation)) -> NonSpare ('GNonSpare name title vt)

data Record (cat :: TCat) (ed :: TEdition) (t :: [TUapItem]) = Record
    { recOffset :: !ByteOffset
    , recSize   :: !ByteSize
    , recItems  :: [Some NonSpare]
    , recRFS    :: [Some NonSpare]
    }

data Expansion (cat :: TCat) (ed :: TEdition) (fs :: Nat) (t :: [Maybe TNonSpare]) = Expansion
    { expOffset :: !ByteOffset
    , expSize   :: !ByteSize
    , expItems  :: [Some NonSpare]
    }
