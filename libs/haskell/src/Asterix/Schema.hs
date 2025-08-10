-- |
-- Module: Asterix.Schema
--
-- Most data structures are used both at the Type and Value level.
-- Naming convention:
--    GSomething ... generic definition of 'Something'
--    TSomething ... type level definition of 'Something'
--    VSomething ... value level definition of 'Something'

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE PolyKinds   #-}

module Asterix.Schema
( module Asterix.Schema
, module Data.Proxy
) where

import           Data.Proxy
import           Data.Text
import           Data.Type.Bool
import           Data.Type.Equality
import           Data.Type.Ord
import           GHC.TypeLits

data Usecase
    = TypeLevel
    | ValueLevel

-- Generic Int is 'Nat' or 'Integer'.
type family GInt (u :: Usecase) = r | r -> u where
    GInt 'TypeLevel = Nat
    GInt 'ValueLevel = Int

type TInt = GInt 'TypeLevel
type VInt = GInt 'ValueLevel

-- Given Nat must be <= 7
type IsNat8 o = o <= 7

-- Generic Text is 'Symbol' or 'Text'.
type family GText (u :: Usecase) = r | r -> u where
    GText 'TypeLevel = Symbol
    GText 'ValueLevel = Text

type TText = GText 'TypeLevel
type VText = GText 'ValueLevel

-- Generic asterix schema

data GRule (u :: Usecase) t
    = GContextFree t
    | GDependent [[GText u]] t [([GInt u], t)]
deriving instance Show t => Show (GRule 'ValueLevel t)

type TRule = GRule 'TypeLevel
type VRule = GRule 'ValueLevel

data GStringType
    = GStringAscii
    | GStringICAO
    | GStringOctal
    deriving Show

type TStringType = GStringType
type VStringType = GStringType

data GSignedness
    = GSigned
    | GUnsigned
    deriving Show

type TSignedness = GSignedness
type VSignedness = GSignedness

data GPlusMinus
    = GPlus
    | GMinus
    deriving Show

type TPlusMinus = GPlusMinus
type VPlusMinus = GPlusMinus

-- Positive or negative integer numbers
data GZ (u :: Usecase) = GZ GPlusMinus (GInt u)
deriving instance Show (GZ 'ValueLevel)

type TZ = GZ 'TypeLevel
type VZ = GZ 'ValueLevel

data GNumber (u :: Usecase)
    = GNumInt (GZ u)
    | GNumDiv (GNumber u) (GNumber u)
    | GNumPow (GZ u) (GZ u)
deriving instance Show (GNumber 'ValueLevel)

type TNumber = GNumber 'TypeLevel
type VNumber = GNumber 'ValueLevel

data GBdsType (u :: Usecase)
    = GBdsWithAddress
    | GBdsAt (Maybe (GInt u))
deriving instance Show (GBdsType 'ValueLevel)

type TBdsType = GBdsType 'TypeLevel
type VBdsType = GBdsType 'ValueLevel

data GContent (u :: Usecase)
    = GContentRaw
    | GContentTable [(GInt u, GText u)]
    | GContentString GStringType
    | GContentInteger GSignedness
    | GContentQuantity GSignedness (GNumber u) (GText u)
    | GContentBds (GBdsType u)
deriving instance Show (GContent 'ValueLevel)

type TContent = GContent 'TypeLevel
type VContent = GContent 'ValueLevel

data GRepetitiveType (u :: Usecase)
    = GRepetitiveRegular (GInt u)
    | GRepetitiveFx
deriving instance Show (GRepetitiveType 'ValueLevel)

type TRepetitiveType = GRepetitiveType 'TypeLevel
type VRepetitiveType = GRepetitiveType 'ValueLevel

data GExplicitType
    = GReservedExpansion
    | GSpecialPurpose
    deriving Show

type TExplicitType = GExplicitType
type VExplicitType = GExplicitType

data GVariation (u :: Usecase)
    = GElement (GInt u) (GInt u) (GRule u (GContent u)) -- offset size
    | GGroup (GInt u) [GItem u] -- offset items
    | GExtended [Maybe (GItem u)]
    | GRepetitive (GRepetitiveType u) (GVariation u)
    | GExplicit (Maybe GExplicitType)
    | GCompound [Maybe (GNonSpare u)]
deriving instance Show (GVariation 'ValueLevel)

type TVariation = GVariation 'TypeLevel
type VVariation = GVariation 'ValueLevel

data GNonSpare (u :: Usecase)
    = GNonSpare (GText u) (GText u) (GRule u (GVariation u)) -- name, title
deriving instance Show (GNonSpare 'ValueLevel)

type TNonSpare = GNonSpare 'TypeLevel
type VNonSpare = GNonSpare 'ValueLevel

data GItem (u :: Usecase)
    = GSpare (GInt u) (GInt u) -- offset size
    | GItem (GNonSpare u)
deriving instance Show (GItem 'ValueLevel)

type TItem = GItem 'TypeLevel
type VItem = GItem 'ValueLevel

data GUapItem (u :: Usecase)
    = GUapItem (GNonSpare u)
    | GUapItemSpare
    | GUapItemRFS
deriving instance Show (GUapItem 'ValueLevel)

type TUapItem = GUapItem 'TypeLevel
type VUapItem = GUapItem 'ValueLevel

newtype GRecord (u :: Usecase) = GRecord [GUapItem u]
deriving instance Show (GRecord 'ValueLevel)

type TRecord = GRecord 'TypeLevel
type VRecord = GRecord 'ValueLevel

data GUapSelector (u :: Usecase) = GUapSelector [GText u] [(GInt u, GText u)]
deriving instance Show (GUapSelector 'ValueLevel)

type TUapSelector = GUapSelector 'TypeLevel
type VUapSelector = GUapSelector 'ValueLevel

data GUap (u :: Usecase)
    = GUap (GRecord u)
    | GUaps [(GText u, GRecord u)] (Maybe (GUapSelector u))
deriving instance Show (GUap 'ValueLevel)

type TUap = GUap 'TypeLevel
type VUap = GUap 'ValueLevel

data GDatablock (u ::  Usecase) = GDatablock (GInt u) (GUap u) -- cat
deriving instance Show (GDatablock 'ValueLevel)

type TDatablock = GDatablock 'TypeLevel
type VDatablock = GDatablock 'ValueLevel

data GExpansion (u :: Usecase) = GExpansion (Maybe (GInt u)) [Maybe (GNonSpare u)]
deriving instance Show (GExpansion 'ValueLevel)

type TExpansion = GExpansion 'TypeLevel
type VExpansion = GExpansion 'ValueLevel

data GEdition (u :: Usecase) = GEdition (GInt u) (GInt u) -- major, minor
deriving instance Show (GEdition 'ValueLevel)

type TEdition = GEdition 'TypeLevel
type VEdition = GEdition 'ValueLevel

data GAsterix (u :: Usecase)
    = GAsterixBasic (GInt u) (GEdition u) (GUap u) -- cat
    | GAsterixExpansion (GInt u) (GEdition u) (GExpansion u) -- cat
deriving instance Show (GAsterix 'ValueLevel)

type TAsterix = GAsterix 'TypeLevel
type VAsterix = GAsterix 'ValueLevel

-- | Helper type class to convert structures from type level to value level
class IsSchema t a where
    schema :: Proxy t -> a

instance KnownNat n => IsSchema n Int where
    schema _ = fromIntegral $ natVal @n Proxy

instance KnownSymbol s => IsSchema s Text where
    schema _ = Data.Text.pack $ symbolVal @s Proxy

instance IsSchema 'Nothing (Maybe a) where
    schema _ = Nothing

instance
    ( IsSchema t a
    ) => IsSchema ('Just t) (Maybe a) where
    schema _ = Just (schema @t Proxy)

instance IsSchema '[] [a] where
    schema _ = []

instance
    ( IsSchema t a
    , IsSchema ts [a]
    ) => IsSchema (t ': ts) [a] where
    schema _ = schema @t Proxy : schema @ts Proxy

instance
    ( IsSchema t1 a1
    , IsSchema t2 a2
    ) => IsSchema '(t1, t2) (a1, a2) where
    schema _ = (schema @t1 Proxy, schema @t2 Proxy)

instance
    ( IsSchema t (f 'ValueLevel)
    ) => IsSchema
        ('GContextFree (t :: f 'TypeLevel))
        (GRule 'ValueLevel (f 'ValueLevel)) where
    schema _ = GContextFree (schema @t Proxy)

instance
    ( IsSchema lst1 [[Text]]
    , IsSchema t (f 'ValueLevel)
    , IsSchema lst2 [([Int], f ValueLevel)]
    ) => IsSchema
        ('GDependent lst1 (t :: f 'TypeLevel) lst2)
        (GRule 'ValueLevel (f 'ValueLevel)) where
    schema _= GDependent
        (schema @lst1 Proxy)
        (schema @t Proxy)
        (schema @lst2 Proxy)

instance IsSchema 'GStringAscii GStringType where
    schema _ = GStringAscii

instance IsSchema 'GStringICAO GStringType where
    schema _ = GStringICAO

instance IsSchema 'GStringOctal GStringType where
    schema _ = GStringOctal

instance IsSchema 'GSigned GSignedness where
    schema _ = GSigned

instance IsSchema 'GUnsigned GSignedness where
    schema _ = GUnsigned

instance IsSchema 'GPlus GPlusMinus where
    schema _ = GPlus

instance IsSchema 'GMinus GPlusMinus where
    schema _ = GMinus

instance
    ( IsSchema pm GPlusMinus
    , IsSchema n Int
    ) => IsSchema ('GZ pm n) VZ where
    schema _ = GZ (schema @pm Proxy) (schema @n Proxy)

instance
    ( IsSchema z VZ
    ) => IsSchema ('GNumInt z) VNumber where
    schema _ = GNumInt (schema @z Proxy)

instance
    ( IsSchema n1 (GNumber ValueLevel)
    , IsSchema n2 (GNumber ValueLevel)
    ) => IsSchema ('GNumDiv n1 n2) VNumber where
    schema _ = GNumDiv (schema @n1 Proxy) (schema @n2 Proxy)

instance
    ( IsSchema a (GZ ValueLevel)
    , IsSchema b (GZ ValueLevel)
    ) => IsSchema ('GNumPow a b) VNumber where
    schema _ = GNumPow (schema @a Proxy) (schema @b Proxy)

instance IsSchema 'GBdsWithAddress VBdsType where
    schema _ = GBdsWithAddress

instance
    ( IsSchema ma (Maybe Int)
    ) => IsSchema ('GBdsAt ma) VBdsType where
    schema _ = GBdsAt (schema @ma Proxy)

instance IsSchema 'GContentRaw VContent where
    schema _ = GContentRaw

instance
    ( IsSchema lst [(Int, Text)]
    ) => IsSchema ('GContentTable lst) VContent where
    schema _ = GContentTable (schema @lst @[(Int, Text)] Proxy)

instance
    ( IsSchema st GStringType
    ) => IsSchema ('GContentString st) VContent where
    schema _ = GContentString (schema @st Proxy)

instance
    ( IsSchema sig GSignedness
    ) => IsSchema ('GContentInteger sig) VContent where
    schema _ = GContentInteger (schema @sig Proxy)

instance
    ( IsSchema sig GSignedness
    , IsSchema lsb (GNumber ValueLevel)
    , IsSchema unit Text
    ) => IsSchema ('GContentQuantity sig lsb unit) VContent where
    schema _ = GContentQuantity
        (schema @sig Proxy)
        (schema @lsb Proxy)
        (schema @unit Proxy)

instance
    ( IsSchema bt (GBdsType ValueLevel)
    ) => IsSchema ('GContentBds bt) VContent where
    schema _ = GContentBds (schema @bt Proxy)

instance
    ( IsSchema n Int
    ) => IsSchema ('GRepetitiveRegular n) VRepetitiveType where
    schema _ = GRepetitiveRegular (schema @n Proxy)

instance IsSchema 'GRepetitiveFx VRepetitiveType where
    schema _ = GRepetitiveFx

instance IsSchema 'GReservedExpansion GExplicitType where
    schema _ = GReservedExpansion

instance IsSchema 'GSpecialPurpose GExplicitType where
    schema _ = GSpecialPurpose

instance
    ( IsSchema o Int
    , IsSchema n Int
    , IsSchema rule (VRule VContent)
    , IsNat8 o
    ) => IsSchema ('GElement o n rule) VVariation where
    schema _ = GElement
        (schema @o Proxy)
        (schema @n Proxy)
        (schema @rule Proxy)

instance
    ( IsSchema o Int
    , IsNat8 o
    , IsSchema ts [VItem]
    ) => IsSchema ('GGroup o ts) VVariation where
    schema _ = GGroup (schema @o Proxy) (schema @ts Proxy)

instance
    ( IsSchema lst [Maybe VItem]
    ) => IsSchema ('GExtended lst) VVariation where
    schema _ = GExtended (schema @lst Proxy)

instance
    ( IsSchema rt VRepetitiveType
    , IsSchema var VVariation
    ) => IsSchema ('GRepetitive rt var) VVariation where
    schema _ = GRepetitive (schema @rt Proxy) (schema @var Proxy)

instance
    ( IsSchema met (Maybe GExplicitType)
    ) => IsSchema ('GExplicit met) VVariation where
    schema _ = GExplicit (schema @met Proxy)

instance
    ( IsSchema lst [Maybe VNonSpare]
    ) => IsSchema ('GCompound lst) VVariation where
    schema _ = GCompound (schema @lst Proxy)

instance
    ( IsSchema name Text
    , IsSchema title Text
    , IsSchema rule (VRule VVariation)
    ) => IsSchema ('GNonSpare name title rule) VNonSpare where
    schema _ = GNonSpare
        (schema @name Proxy)
        (schema @title Proxy)
        (schema @rule Proxy)

instance
    ( IsSchema o Int
    , IsNat8 o
    , IsSchema n Int
    ) => IsSchema ('GSpare o n) VItem where
    schema _ = GSpare (schema @o Proxy) (schema @n Proxy)

instance
    ( IsSchema nsp VNonSpare
    ) => IsSchema ('GItem nsp) VItem where
    schema _ = GItem (schema @nsp Proxy)

instance
    ( IsSchema nsp VNonSpare
    ) => IsSchema ('GUapItem nsp) VUapItem where
    schema _ = GUapItem (schema @nsp Proxy)

instance IsSchema 'GUapItemSpare VUapItem where
    schema _ = GUapItemSpare

instance IsSchema 'GUapItemRFS VUapItem where
    schema _ = GUapItemRFS

instance
    ( IsSchema lst [VUapItem]
    ) => IsSchema ('GRecord lst) VRecord where
    schema _ = GRecord (schema @lst Proxy)

instance
    ( IsSchema iname [Text]
    , IsSchema lst [(Int, Text)]
    ) => IsSchema ('GUapSelector iname lst) VUapSelector where
    schema _ = GUapSelector (schema @iname Proxy) (schema @lst Proxy)

instance
    ( IsSchema rec VRecord
    ) => IsSchema ('GUap rec) VUap where
    schema _ = GUap (schema @rec Proxy)

instance
    ( IsSchema lst [(Text, VRecord)]
    , IsSchema msel (Maybe VUapSelector)
    ) => IsSchema ('GUaps lst msel) VUap where
    schema _ = GUaps (schema @lst Proxy) (schema @msel Proxy)

instance
    ( IsSchema cat Int
    , IsSchema uap VUap
    ) => IsSchema ('GDatablock cat uap) VDatablock where
    schema _ = GDatablock (schema @cat Proxy) (schema @uap Proxy)

instance
    ( IsSchema mn (Maybe Int)
    , IsSchema lst [Maybe VNonSpare]
    ) => IsSchema ('GExpansion mn lst) VExpansion where
    schema _ = GExpansion (schema @mn Proxy) (schema @lst Proxy)

instance
    ( IsSchema a Int
    , IsSchema b Int
    ) => IsSchema ('GEdition a b) VEdition where
    schema _ = GEdition (schema @a Proxy) (schema @b Proxy)

instance
    ( IsSchema cat Int
    , IsSchema ed VEdition
    , IsSchema uap VUap
    ) => IsSchema ('GAsterixBasic cat ed uap) VAsterix where
    schema _ = GAsterixBasic
        (schema @cat Proxy)
        (schema @ed Proxy)
        (schema @uap Proxy)

instance
    ( name ~ VText
    , uap ~ 'GUaps lst msel
    , IsSchema lst [(VText, VRecord)]
    ) => IsSchema ('GAsterixBasic cat ed uap) [(name, VRecord)] where
    schema _ = schema @lst Proxy

instance
    ( IsSchema cat Int
    , IsSchema ed VEdition
    , IsSchema exp VExpansion
    ) => IsSchema ('GAsterixExpansion cat ed exp) VAsterix where
    schema _ = GAsterixExpansion
        (schema @cat Proxy)
        (schema @ed Proxy)
        (schema @exp Proxy)

-- Extract category
type CategoryOf :: k -> Nat
type family CategoryOf t where
    CategoryOf ('GAsterixBasic cat ed uap) = cat
    CategoryOf ('GDatablock cat uap) = cat

-- Bit size calculation
type BitSizeOf :: k -> Nat
type family BitSizeOf t where
    BitSizeOf '[] = 0
    BitSizeOf (t ': ts) = BitSizeOf t + BitSizeOf ts
    BitSizeOf ('GElement o n rc) = n
    BitSizeOf ('GGroup o lst) = BitSizeOf lst
    BitSizeOf ('GContextFree t) = BitSizeOf t
    BitSizeOf ('GDependent lst1 t lst2) = BitSizeOf t
    BitSizeOf ('GSpare o n) = n
    BitSizeOf ('GItem nsp) = BitSizeOf nsp
    BitSizeOf ('GNonSpare name title rv) = BitSizeOf rv

-- | Extract record type from single uap basic asterix
type family RecordOf (t :: GAsterix u) :: GRecord u where
    RecordOf ('GAsterixBasic cat ed ('GUap rec)) = rec
    RecordOf ast = TypeError ('Text "Unspecified")

-- Ignore edition
type family DatablockOf (t :: GAsterix u) :: GDatablock u where
    DatablockOf ('GAsterixBasic cat ed uap) = 'GDatablock cat uap
    DatablockOf ast = TypeError ('Text "Unspecified")

type family ExpansionOf (t :: GAsterix u) :: GExpansion u where
    ExpansionOf ('GAsterixExpansion cat ed exp) = exp
    ExpansionOf ast = TypeError ('Text "Unspecified")

-- Type level subtype extraction

type NotDefined (name :: Symbol) = TypeError ('Text "Not defined: " :<>: 'Text name)

type family GetUap (t :: TAsterix) :: TUap where
    GetUap ('GAsterixBasic cat ed uap) = uap
    GetUap ('GAsterixExpansion cat ed exp) = TypeError ('Text "Expansion")

type family GetRecord (t :: TUap) :: TRecord where
    GetRecord ('GUap rec) = rec
    GetRecord ('GUaps uaps sel) = TypeError ('Text "Multiple UAPs")

type family Lookup (name :: Symbol) (lst :: [(Symbol, k)]) :: k where
    Lookup name '[] = NotDefined name
    Lookup name1 ( '(name2, uap) ': ts) = If
        (name1 == name2)
        uap
        (Lookup name1 ts)

type family GetRecordOf (name :: Symbol) (uap :: TUap) :: TRecord
  where
    GetRecordOf name ('GUap rec) = TypeError ('Text "Single UAP")
    GetRecordOf name ('GUaps uaps sel) = Lookup name uaps

-- | Find index of substructure out of the bigger structure, by it's name.
type FindIndex :: k -> Symbol -> Nat
type family FindIndex t name where
    FindIndex ( '(name1, x) ': ts) name = If
        (name1 == name) 0 (1 + FindIndex ts name)

    FindIndex ('Nothing ': ts) name = 1 + FindIndex ts name
    FindIndex ('Just ('GSpare o n) ': ts) name = 1 + FindIndex ts name
    FindIndex ('Just ('GItem ('GNonSpare name1 title rv)) ': ts) name = If
        (name1 == name) 0 (1 + FindIndex ts name)
    FindIndex ('Just ('GNonSpare name1 title rv) ': ts) name = If
        (name1 == name) 0 (1 + FindIndex ts name)

    FindIndex ('GSpare o n ': ts) name = 1 + FindIndex ts name
    FindIndex ('GItem ('GNonSpare name1 title rv) ': ts) name = If
        (name1 == name) 0 (1 + FindIndex ts name)

    FindIndex ('GUapItem ('GNonSpare name1 title rv) ': ts) name = If
        (name1 == name) 0 (1 + FindIndex ts name)
    FindIndex ('GUapItemSpare ': ts) name = 1 + FindIndex ts name
    FindIndex ('GUapItemRFS ': ts) name = 1 + FindIndex ts name

    FindIndex ('GContextFree t) name = FindIndex t name

    FindIndex ('GNonSpare name1 title rv) name = FindIndex rv name

    FindIndex ('GGroup o lst) name = FindIndex lst name
    FindIndex ('GExtended lst) name = FindIndex lst name
    FindIndex ('GRepetitive rt var) name = FindIndex var name
    FindIndex ('GCompound lst) name = FindIndex lst name
    FindIndex ('GAsterixBasic cat ed ('GUaps lst sel)) name = FindIndex lst name
    FindIndex ('GRecord lst) name = FindIndex lst name
    FindIndex ('GExpansion mn lst) name = FindIndex lst name
    FindIndex x name = NotDefined name

type family RecordNonSpares (t :: [TUapItem]) :: [TNonSpare]
  where
    RecordNonSpares '[] = '[]
    RecordNonSpares ('GUapItem nsp ': ts) = nsp ': RecordNonSpares ts
    RecordNonSpares ('GUapItemSpare ': ts) = RecordNonSpares ts
    RecordNonSpares ('GUapItemRFS ': ts) = RecordNonSpares ts

type family ItemNonSpares (t :: [TItem]) :: [TNonSpare]
  where
    ItemNonSpares '[] = '[]
    ItemNonSpares ('GSpare o2 n2 ': ts) = ItemNonSpares ts
    ItemNonSpares ('GItem nsp ': ts) = nsp ': ItemNonSpares ts

type family FilterMaybe (lst :: [Maybe a]) :: [a] where
    FilterMaybe '[] = '[]
    FilterMaybe ('Nothing ': ts) = FilterMaybe ts
    FilterMaybe ('Just t ': ts) = t ': FilterMaybe ts

type family FirstExtendedGroup (lst :: [Maybe a]) :: [a] where
    FirstExtendedGroup '[] = '[]
    FirstExtendedGroup ('Nothing ': ts) = '[]
    FirstExtendedGroup ('Just t ': ts) = t ': FirstExtendedGroup ts

type family TrailingFx (lst :: [Maybe a]) :: Bool where
    TrailingFx '[] = 'False
    TrailingFx ('Nothing ': ts) = 'True
    TrailingFx ('Just t ': ts) = TrailingFx ts

class KnownBool t where boolVal :: Proxy t -> Bool
instance KnownBool 'False where boolVal _ = False
instance KnownBool 'True where boolVal _ = True

type family RemainingExtendedItems (lst :: [Maybe a]) :: [Maybe a] where
    RemainingExtendedItems '[] = '[]
    RemainingExtendedItems ('Nothing ': ts) = ts
    RemainingExtendedItems ('Just t ': ts) = RemainingExtendedItems ts

type family PrependName t where
    PrependName '[] = '[]
    PrependName ('GNonSpare name title rv ': ts)
        = '(name, 'GNonSpare name title rv) ': PrependName ts

-- | Extract GRecord from multiUAP asterix
type (~~>) :: TAsterix -> Symbol -> TRecord
type family (~~>) t name where
    (~~>) ('GAsterixBasic cat ed ('GUaps lst sel)) name = Lookup name lst
    (~~>) _ name = TypeError ('Text "Undefined")

-- | Overloaded type level function to extract subtype from parent, based on name
type (~>) :: k -> Symbol -> TNonSpare
type family (~>) t name where
    (~>) ('GGroup o lst) name
        = Lookup name (PrependName (ItemNonSpares lst))
    (~>) ('GExtended lst) name
        = Lookup name (PrependName (ItemNonSpares (FilterMaybe lst)))
    (~>) ('GRepetitive rt var) name
        = var ~> name
    (~>) ('GCompound lst) name
        = Lookup name (PrependName (FilterMaybe lst))
    (~>) ('GNonSpare pname title ('GContextFree ('GGroup o lst))) name
        = 'GGroup o lst ~> name
    (~>) ('GNonSpare pname title ('GContextFree ('GExtended lst))) name
        = 'GExtended lst ~> name
    (~>) ('GNonSpare pname title ('GContextFree ('GRepetitive rt var))) name
        = 'GRepetitive rt var ~> name
    (~>) ('GNonSpare pname title ('GContextFree ('GCompound lst))) name
        = 'GCompound lst ~> name
    (~>) ('GRecord lst) name
        = Lookup name (PrependName (RecordNonSpares lst))
    (~>) ('GExpansion mn lst) name
        = Lookup name (PrependName (FilterMaybe lst))
    (~>) ('GAsterixBasic cat ed ('GUap ('GRecord lst))) name
        = Lookup name (PrependName (RecordNonSpares lst))
    (~>) ('GAsterixExpansion cat ed ('GExpansion mn lst)) name
        = 'GExpansion mn lst ~> name
    (~>) k name = TypeError ('Text "Undefined argument: " :<>: ShowType k)

-- | Extract dependent rule
type Rule :: k1 -> k2 -> k3
type family Rule t ix where
    Rule ('GNonSpare name title ('GDependent lst1 d lst2)) ix = Rule lst2 ix
    Rule ('(ix1, t) ': ts) ix = If (ix1 == ix)
        t
        (Rule ts ix)
    Rule k1 ix = TypeError ('Text "Undefined rule: " :<>: ShowType ix)

-- | All of the given types in a non-empty list must be the same,
-- return that type. Usage example:
-- type TSacSic = SameType '[ Cat034 ~> "010", Cat048 ~> "010"]
type family SameType (t :: [k]) :: k where
    SameType '[] = TypeError ('Text "Empty list")
    SameType (t ': '[]) = t
    SameType (t1 ': t2 ': ts) = If
        (t1 == t2)
        (SameType (t2 ': ts))
        (TypeError ('Text "Type mismatch"))

