-- | Asterix specifications schema in G-generic, T-type and V-value level version.

module Asterix.Schema where

import GHC.TypeLits

data GRule s n t
    = GContextFree t
    | GDependent [[s]] t [([n], t)]

type TRule t = GRule Symbol Nat t
type VRule t = GRule String Int t

data GStringType
    = GStringAscii
    | GStringICAO
    | GStringOctal

data GSignedness
    = GSigned
    | GUnsigned

data GNumber n
    = GNumInt n
    | GNumDiv (GNumber n) (GNumber n)
    | GNumPow n n

type TNumber = GNumber Nat

data GBdsType n
    = GBdsWithAddress
    | GBdsAt (Maybe n)

type TBdsType = GBdsType Nat

data GContent s n
    = GContentRaw
    | GContentTable [(n, s)]
    | GContentString GStringType
    | GContentInteger GSignedness
    | GContentQuantity GSignedness (GNumber n) s
    | GContentBds (GBdsType n)

data TContent = GContent Symbol Nat

data GRepetitiveType n
    = GRepetitiveRegular n
    | GRepetitiveFx

type TRepetitiveType = GRepetitiveType Nat

data GExplicitType
    = GReservedExpansion
    | GSpecialPurpose

data GVariation s n
    = GElement n n (GRule s n (GContent s n))
    | GGroup n [GItem s n]
    | GExtended [Maybe (GItem s n)]
    | GRepetitive (GRepetitiveType n) (GVariation s n)
    | GExplicit (Maybe GExplicitType)
    | GCompound [Maybe (GNonSpare s n)]

type TVariation = GVariation Symbol Nat
type VVariation = GVariation String Int

data GItem s n
    = GSpare n n
    | GItem (GNonSpare s n)

type TItem = GItem Symbol Nat
type VItem = GItem String Int

data GNonSpare s n = GNonSpare s s (GRule s n (GVariation s n))

type TNonSpare = GNonSpare Symbol Nat
type VNonSpare = GNonSpare String Int

data GUapItem s n
    = GUapItem (GNonSpare s n)
    | GUapItemSpare
    | GUapItemRFS

type TUapItem = GUapItem Symbol Nat
type VUapItem = GUapItem String Int

newtype GCat n = GCat n

type TCat = GCat Nat

data GEdition n = GEdition n n

type TEdition = GEdition Nat
