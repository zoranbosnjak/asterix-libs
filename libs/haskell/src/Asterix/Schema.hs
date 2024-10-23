{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Asterix specifications schema in G-generic, T-type and V-value level version.

module Asterix.Schema where

import           GHC.TypeLits
import           Data.Proxy
import           Data.Text

intErr :: a
intErr = error "internal error"

-- | Convert schema from type to value
class IsSchema t a | t -> a where
    schema :: a

instance KnownNat n => IsSchema n Int where
    schema = fromIntegral $ natVal (Proxy @n)

instance KnownSymbol s => IsSchema s Text where
    schema = Data.Text.pack $ symbolVal (Proxy @s)

data GRule s n t
    = GContextFree t
    | GDependent [[s]] t [([n], t)]

type TRule = GRule Symbol Nat
type VRule = GRule Text Int

instance IsSchema t vt => IsSchema ('GContextFree t) (VRule vt) where
    schema = GContextFree (schema @t)

instance
    ( IsSchema t vt
    , IsSchema lst [[Text]]
    ) => IsSchema ('GDependent lst t '[]) (VRule vt)
  where
    schema = GDependent (schema @lst) (schema @t) []

instance
    ( IsSchema t vt
    , IsSchema lst [[Text]]
    , IsSchema xs vt
    , IsSchema x [Int]
    , IsSchema ('GDependent lst t xs) (VRule vt)
    ) => IsSchema ('GDependent lst t ( '(x, t) ': xs)) (VRule vt)
  where
    schema = case schema @('GDependent lst t xs) of
        GDependent a b c -> GDependent a b ((schema @x, schema @t) : c)
        _ -> intErr

data GStringType
    = GStringAscii
    | GStringICAO
    | GStringOctal
    deriving (Eq, Show)

type TStringType = GStringType
type VStringType = GStringType

instance IsSchema 'GStringAscii VStringType where schema = GStringAscii
instance IsSchema 'GStringICAO VStringType where schema = GStringICAO
instance IsSchema 'GStringOctal VStringType where schema = GStringOctal

data GSignedness
    = GSigned
    | GUnsigned
    deriving (Eq, Show)

type TSignedness = GSignedness
type VSignedness = GSignedness

instance IsSchema 'GSigned VSignedness where schema = GSigned
instance IsSchema 'GUnsigned VSignedness where schema = GUnsigned

data GNumber n
    = GNumInt n
    | GNumDiv (GNumber n) (GNumber n)
    | GNumPow n n

type TNumber = GNumber Nat
type VNumber = GNumber Int

instance KnownNat n => IsSchema ('GNumInt n) VNumber where
    schema = GNumInt (schema @n)

instance
    ( IsSchema a VNumber
    , IsSchema b VNumber
    ) => IsSchema ('GNumDiv a b) VNumber where
    schema = GNumDiv (schema @a) (schema @b)

instance (KnownNat a, KnownNat b) => IsSchema ('GNumPow a b) VNumber where
    schema = GNumPow (schema @a) (schema @b)

data GBdsType n
    = GBdsWithAddress
    | GBdsAt (Maybe n)

type TBdsType = GBdsType Nat
type VBdsType = GBdsType Int

instance IsSchema 'GBdsWithAddress VBdsType where schema = GBdsWithAddress
instance IsSchema ('GBdsAt 'Nothing) VBdsType where schema = GBdsAt Nothing
instance KnownNat n => IsSchema ('GBdsAt ('Just n)) VBdsType where
    schema = GBdsAt (Just (schema @n))

data GContent s n
    = GContentRaw
    | GContentTable [(n, s)]
    | GContentString GStringType
    | GContentInteger GSignedness
    | GContentQuantity GSignedness (GNumber n) s
    | GContentBds (GBdsType n)

type TContent = GContent Symbol Nat
type VContent = GContent Text Int

instance IsSchema 'GContentRaw VContent where schema = GContentRaw

instance IsSchema ('GContentTable '[]) VContent where
    schema = GContentTable []
instance
    ( IsSchema ('GContentTable ts) VContent
    , KnownNat n
    , KnownSymbol s
    ) => IsSchema ('GContentTable ('(n, s) ': ts)) VContent
  where
    schema = case schema @('GContentTable ts) of
        GContentTable xs -> GContentTable ((schema @n, schema @s) : xs)
        _ -> intErr

instance IsSchema st VStringType => IsSchema ('GContentString st) VContent
  where
    schema = GContentString (schema @st)

instance IsSchema sig VSignedness => IsSchema ('GContentInteger sig) VContent
  where
    schema = GContentInteger (schema @sig)

instance
    ( IsSchema sig VSignedness
    , IsSchema num VNumber
    , KnownSymbol unit
    ) => IsSchema ('GContentQuantity sig num unit) VContent
  where
    schema = GContentQuantity (schema @sig) (schema @num) (schema @unit)

instance IsSchema bt VBdsType => IsSchema ('GContentBds bt) VContent where
    schema = GContentBds (schema @bt)

data GRepetitiveType n
    = GRepetitiveRegular n
    | GRepetitiveFx

type TRepetitiveType = GRepetitiveType Nat
type VRepetitiveType = GRepetitiveType Int

instance KnownNat n => IsSchema ('GRepetitiveRegular n) VRepetitiveType where
    schema = GRepetitiveRegular (schema @n)

instance IsSchema 'GRepetitiveFx VRepetitiveType where
    schema = GRepetitiveFx

data GExplicitType
    = GReservedExpansion
    | GSpecialPurpose

type TExplicitType = GExplicitType
type VExplicitType = GExplicitType

instance IsSchema 'GReservedExpansion VExplicitType where schema = GReservedExpansion
instance IsSchema 'GSpecialPurpose VExplicitType where schema = GSpecialPurpose

data GVariation s n
    = GElement n n (GRule s n (GContent s n))
    | GGroup n [GItem s n]
    | GExtended [Maybe (GItem s n)]
    | GRepetitive (GRepetitiveType n) (GVariation s n)
    | GExplicit (Maybe GExplicitType)
    | GCompound [Maybe (GNonSpare s n)]

type TVariation = GVariation Symbol Nat
type VVariation = GVariation Text Int

instance
    ( KnownNat o
    , KnownNat n
    , IsSchema rule (VRule VContent)
    ) => IsSchema ('GElement o n rule) VVariation
  where
    schema = GElement (schema @o) (schema @n) (schema @rule)

instance KnownNat o => IsSchema ('GGroup o '[]) VVariation where
    schema = GGroup (schema @o) []

instance
    ( IsSchema ('GGroup o xs) VVariation
    , IsSchema x VItem
    ) => IsSchema ('GGroup o (x ': xs)) VVariation where
    schema = case schema @('GGroup o xs) of
        GGroup o lst -> GGroup o ((schema @x) : lst)
        _ -> intErr

instance IsSchema ('GExtended '[]) VVariation where schema = GExtended []

instance
    ( IsSchema ('GExtended xs) VVariation
    ) => IsSchema ('GExtended ('Nothing ': xs)) VVariation
  where
    schema = case schema @('GExtended xs) of
        GExtended lst -> GExtended (Nothing : lst)
        _ -> intErr

instance
    ( IsSchema ('GExtended xs) VVariation
    , IsSchema x VItem
    ) => IsSchema ('GExtended ('Just x ': xs)) VVariation
  where
    schema = case schema @('GExtended xs) of
        GExtended lst -> GExtended (Just (schema @x) : lst)
        _ -> intErr

instance
    ( IsSchema rt VRepetitiveType
    , IsSchema var VVariation
    ) => IsSchema ('GRepetitive rt var) VVariation where
    schema = GRepetitive (schema @rt) (schema @var)

instance IsSchema ('GExplicit 'Nothing) VVariation where
    schema = GExplicit Nothing

instance
    ( IsSchema et VExplicitType
    ) => IsSchema ('GExplicit ('Just et)) VVariation where
    schema = GExplicit (Just (schema @et))

instance IsSchema ('GCompound '[]) VVariation where
    schema = GCompound []

instance
    ( IsSchema ('GCompound xs) VVariation
    ) => IsSchema ('GCompound ('Nothing ': xs)) VVariation where
    schema = case schema @('GCompound xs) of
        GCompound lst -> GCompound (Nothing : lst)
        _ -> intErr

instance
    ( IsSchema ('GCompound xs) VVariation
    , IsSchema x VNonSpare
    ) => IsSchema ('GCompound ('Just x ': xs)) VVariation where
    schema = case schema @('GCompound xs) of
        GCompound lst -> GCompound (Just (schema @x) : lst)
        _ -> intErr

data GItem s n
    = GSpare n n
    | GItem (GNonSpare s n)

type TItem = GItem Symbol Nat
type VItem = GItem Text Int

instance (KnownNat o, KnownNat n) => IsSchema ('GSpare o n) VItem where
    schema = GSpare (schema @o) (schema @n)

instance IsSchema nsp VNonSpare => IsSchema ('GItem nsp) VItem where
    schema = GItem (schema @nsp)

data GNonSpare s n = GNonSpare s s (GRule s n (GVariation s n))

type TNonSpare = GNonSpare Symbol Nat
type VNonSpare = GNonSpare Text Int

instance
    ( KnownSymbol name
    , KnownSymbol title
    , IsSchema rule (VRule VVariation)
    ) => IsSchema ('GNonSpare name title rule) VNonSpare
  where
    schema = GNonSpare (schema @name) (schema @title) (schema @rule)

data GUapItem s n
    = GUapItem (GNonSpare s n)
    | GUapItemSpare
    | GUapItemRFS

type TUapItem = GUapItem Symbol Nat
type VUapItem = GUapItem Text Int

instance
    ( IsSchema nsp VNonSpare
    ) => IsSchema ('GUapItem nsp) VUapItem where
    schema = GUapItem (schema @nsp)

instance IsSchema 'GUapItemSpare VUapItem where schema = GUapItemSpare

instance IsSchema 'GUapItemRFS VUapItem where schema = GUapItemRFS

newtype GCat n = GCat n

type TCat = GCat Nat
type VCat = GCat Int

instance KnownNat n => IsSchema ('GCat n) VCat where
    schema = GCat (schema @n)

data GEdition n = GEdition n n

type TEdition = GEdition Nat
type VEdition = GEdition Int

instance (KnownNat a, KnownNat b) => IsSchema ('GEdition a b) VEdition where
    schema = GEdition (schema @a) (schema @b)

data GRecord s n = GRecord (GCat n) (GEdition n) [GUapItem s n]

type TRecord = GRecord Symbol Nat
type VRecord = GRecord Text Int

instance
    ( IsSchema cat VCat
    , IsSchema ed VEdition
    ) => IsSchema ('GRecord cat ed '[]) VRecord where
    schema = GRecord (schema @cat) (schema @ed) []

instance
    ( IsSchema cat VCat
    , IsSchema ed VEdition
    , IsSchema ('GRecord cat ed xs) VRecord
    , IsSchema x VUapItem
    ) => IsSchema ('GRecord cat ed (x ': xs)) VRecord where
    schema = case schema @('GRecord cat ed xs) of
        GRecord a b lst -> GRecord a b (schema @x : lst)

data GDatablock n = GDatablock (GCat n) (GEdition n)

type TDatablock = GDatablock Nat
type VDatablock = GDatablock Int

instance
    ( IsSchema cat VCat
    , IsSchema ed VEdition
    ) => IsSchema ('GDatablock cat ed) VDatablock where
    schema = GDatablock (schema @cat) (schema @ed)

data GExpansion s n = GExpansion (GCat n) (GEdition n) n [Maybe (GNonSpare s n)]

type TExpansion = GExpansion Symbol Nat
type VExpansion = GExpansion Text Int

instance
    ( IsSchema cat VCat
    , IsSchema ed VEdition
    , KnownNat fspecByteSize
    ) => IsSchema ('GExpansion cat ed fspecByteSize '[]) VExpansion where
    schema = GExpansion (schema @cat) (schema @ed) (schema @fspecByteSize) []

instance
    ( IsSchema cat VCat
    , IsSchema ed VEdition
    , KnownNat fspecByteSize
    , IsSchema ('GExpansion cat ed fspecByteSize xs) VExpansion
    ) => IsSchema ('GExpansion cat ed fspecByteSize ('Nothing ': xs)) VExpansion where
    schema = case schema @('GExpansion cat ed fspecByteSize xs) of
        GExpansion a b c lst -> GExpansion a b c (Nothing : lst)

instance
    ( IsSchema cat VCat
    , IsSchema ed VEdition
    , KnownNat fspecByteSize
    , IsSchema ('GExpansion cat ed fspecByteSize xs) VExpansion
    , IsSchema x VNonSpare
    ) => IsSchema ('GExpansion cat ed fspecByteSize ('Just x ': xs)) VExpansion where
    schema = case schema @('GExpansion cat ed fspecByteSize xs) of
        GExpansion a b c lst -> GExpansion a b c (Just (schema @x) : lst)
