-- | Asterix specifications schema in G-generic, T-type and V-value level version.

module Asterix.Schema where

import           GHC.TypeLits

data GRule s n t
    = GContextFree t
    | GDependent [[s]] t [([n], t)]

type TRule = GRule Symbol Nat
type VRule = GRule String Int

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
type VCat = GCat Int

data GEdition n = GEdition n n

type TEdition = GEdition Nat
type VEdition = GEdition Int

data GRecord s n = GRecord (GCat n) (GEdition n) [GUapItem s n]

type TRecord = GRecord Symbol Nat
type VRecord = GRecord String Int

data GDatablock n = GDatablock (GCat n) (GEdition n)

type TDatablock = GDatablock Nat
type VDatablock = GDatablock Int

data GExpansion s n = GExpansion (GCat n) (GEdition n) n [Maybe (GNonSpare s n)]

type TExpansion = GExpansion Symbol Nat
type VExpansion = GExpansion String Int
