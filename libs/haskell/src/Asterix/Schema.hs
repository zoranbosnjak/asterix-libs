-- |
-- Module: Asterix.Schema
--
-- This module defines generic asterix structures which can be used as types
-- and as values. This imposes some complexity, mainly because of the fact that
-- in haskell, all type information is erased during program compilation. In
-- other words: normally types are only important when writting and compiling
-- the code, not when running it. Whatever is required at runtime must be
-- explicitely "created" during compile phase.
--
-- A standard approach to convert type level information to the value level
-- equivalent is to use typeclasses and instances. Regular function don't work
-- in this case.
--
-- An approach taken in this library is to have:
--  - generic structures from this module, for type or value level usecases;
--  - generated file where complete asterix specs are defined as types;
--  - conversion rules (as typeclasses and instances) from this module to have
--    runtime equivalent of complete asterix specs at value level too;
--
-- Naming convention:
--
-- * GSomething ... generic definition of 'Something'
--    It is indexed by 'Usecase' type parameter.
--
-- * TSomething ... type level definition of 'Something', defined as:
--    type TSomething = GSomething 'TypeLevel
--
-- * VSomething ... value level definition of 'Something', defined as:
--    type VSomething = GSomething 'ValueLevel
--
-- The type level definitions (TSomething) are generated in a separate module.
-- The 'schema' function from Schema typeclass converts from
-- TSomething -> VSomething, such that any specification part can also be
-- used at runtime.

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

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

-- | Type or value level usecase.
data Usecase
    = TypeLevel
    | ValueLevel

-- | Generic Int is 'Nat' or 'Integer'.
type family GInt (u :: Usecase) = r | r -> u where
    GInt 'TypeLevel = Nat
    GInt 'ValueLevel = Int

type TInt = GInt 'TypeLevel
type VInt = GInt 'ValueLevel

-- | Integer in the interval [0..7].
newtype Int8 = Int8 { unInt8 :: Int }
    deriving (Show, Eq)

class KnownNat8 (n :: Nat) where
    natVal8 :: Proxy n -> Int8

instance (KnownNat n, n <= 7) => KnownNat8 n where
    natVal8 = Int8 . fromIntegral . natVal

-- | Generic Text is 'Symbol' or 'Text'.
type family GText (u :: Usecase) = r | r -> u where
    GText 'TypeLevel = Symbol
    GText 'ValueLevel = Text

type TText = GText 'TypeLevel
type VText = GText 'ValueLevel

-- Generic asterix schema

-- | Rule.
data GRule (u :: Usecase) t
    = GContextFree t
    | GDependent [[GText u]] t [([GInt u], t)]
deriving instance Show t => Show (GRule 'ValueLevel t)

type TRule = GRule 'TypeLevel
type VRule = GRule 'ValueLevel

-- | String type.
data GStringType
    = GStringAscii
    | GStringICAO
    | GStringOctal
    deriving Show

type TStringType = GStringType
type VStringType = GStringType

-- | Signed or unsigned.
data GSignedness
    = GSigned
    | GUnsigned
    deriving Show

type TSignedness = GSignedness
type VSignedness = GSignedness

-- | Plus or minus.
data GPlusMinus
    = GPlus
    | GMinus
    deriving Show

type TPlusMinus = GPlusMinus
type VPlusMinus = GPlusMinus

-- | Positive or negative integer numbers.
data GZ (u :: Usecase) = GZ GPlusMinus (GInt u)
deriving instance Show (GZ 'ValueLevel)

type TZ = GZ 'TypeLevel
type VZ = GZ 'ValueLevel

-- | A number without rounding errors.
data GNumber (u :: Usecase)
    = GNumInt (GZ u)
    | GNumDiv (GNumber u) (GNumber u)
    | GNumPow (GZ u) (GZ u)
deriving instance Show (GNumber 'ValueLevel)

type TNumber = GNumber 'TypeLevel
type VNumber = GNumber 'ValueLevel

-- | BDS type.
data GBdsType (u :: Usecase)
    = GBdsWithAddress
    | GBdsAt (Maybe (GInt u))
deriving instance Show (GBdsType 'ValueLevel)

type TBdsType = GBdsType 'TypeLevel
type VBdsType = GBdsType 'ValueLevel

-- | Type of content.
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

-- | Repetitive type.
data GRepetitiveType (u :: Usecase)
    = GRepetitiveRegular (GInt u)
    | GRepetitiveFx
deriving instance Show (GRepetitiveType 'ValueLevel)

type TRepetitiveType = GRepetitiveType 'TypeLevel
type VRepetitiveType = GRepetitiveType 'ValueLevel

-- | Explicit type.
data GExplicitType
    = GReservedExpansion
    | GSpecialPurpose
    deriving Show

type TExplicitType = GExplicitType
type VExplicitType = GExplicitType

-- | Variation.
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

-- | NonSpare.
data GNonSpare (u :: Usecase)
    = GNonSpare (GText u) (GText u) (GRule u (GVariation u)) -- name, title
deriving instance Show (GNonSpare 'ValueLevel)

type TNonSpare = GNonSpare 'TypeLevel
type VNonSpare = GNonSpare 'ValueLevel

-- | Item.
data GItem (u :: Usecase)
    = GSpare (GInt u) (GInt u) -- offset size
    | GItem (GNonSpare u)
deriving instance Show (GItem 'ValueLevel)

type TItem = GItem 'TypeLevel
type VItem = GItem 'ValueLevel

-- | UAP Item.
data GUapItem (u :: Usecase)
    = GUapItem (GNonSpare u)
    | GUapItemSpare
    | GUapItemRFS
deriving instance Show (GUapItem 'ValueLevel)

type TUapItem = GUapItem 'TypeLevel
type VUapItem = GUapItem 'ValueLevel

-- | Record.
newtype GRecord (u :: Usecase) = GRecord [GUapItem u]
deriving instance Show (GRecord 'ValueLevel)

type TRecord = GRecord 'TypeLevel
type VRecord = GRecord 'ValueLevel

-- | UAP selector.
data GUapSelector (u :: Usecase) = GUapSelector [GText u] [(GInt u, GText u)]
deriving instance Show (GUapSelector 'ValueLevel)

type TUapSelector = GUapSelector 'TypeLevel
type VUapSelector = GUapSelector 'ValueLevel

-- | User application profile.
data GUap (u :: Usecase)
    = GUap (GRecord u)
    | GUaps [(GText u, GRecord u)] (Maybe (GUapSelector u))
deriving instance Show (GUap 'ValueLevel)

type TUap = GUap 'TypeLevel
type VUap = GUap 'ValueLevel

-- | Datablock.
data GDatablock (u ::  Usecase) = GDatablock (GInt u) (GUap u) -- cat uap
deriving instance Show (GDatablock 'ValueLevel)

type TDatablock = GDatablock 'TypeLevel
type VDatablock = GDatablock 'ValueLevel

-- | Expansion.
data GExpansion (u :: Usecase) = GExpansion (Maybe (GInt u)) [Maybe (GNonSpare u)]
deriving instance Show (GExpansion 'ValueLevel)

type TExpansion = GExpansion 'TypeLevel
type VExpansion = GExpansion 'ValueLevel

-- | Edition.
data GEdition (u :: Usecase) = GEdition (GInt u) (GInt u) -- major, minor
deriving instance Show (GEdition 'ValueLevel)

type TEdition = GEdition 'TypeLevel
type VEdition = GEdition 'ValueLevel

deriving instance Eq VEdition
instance Ord VEdition where
    compare (GEdition a1 b1) (GEdition a2 b2)
        = compare a1 a2
       <> compare b1 b2

-- | Toplevel asterix structure.
data GAsterix (u :: Usecase)
    = GAsterixBasic (GInt u) (GEdition u) (GUap u) -- cat
    | GAsterixExpansion (GInt u) (GEdition u) (GExpansion u) -- cat
deriving instance Show (GAsterix 'ValueLevel)

type TAsterix = GAsterix 'TypeLevel
type VAsterix = GAsterix 'ValueLevel

-- | Convert structures from type level 't' to value level 'a',
-- with 'schema' function.
class Schema t a where
    schema :: Proxy t -> a

instance KnownNat n => Schema n Int where
    schema _ = fromIntegral $ natVal @n Proxy

instance KnownNat8 n => Schema n Int8 where
    schema _ = natVal8 @n Proxy

instance KnownSymbol s => Schema s Text where
    schema _ = Data.Text.pack $ symbolVal @s Proxy

instance Schema 'Nothing (Maybe a) where
    schema _ = Nothing

instance
    ( Schema t a
    ) => Schema ('Just t) (Maybe a) where
    schema _ = Just (schema @t Proxy)

instance Schema '[] [a] where
    schema _ = []

instance
    ( Schema t a
    , Schema ts [a]
    ) => Schema (t ': ts) [a] where
    schema _ = schema @t Proxy : schema @ts Proxy

instance
    ( Schema t1 a1
    , Schema t2 a2
    ) => Schema '(t1, t2) (a1, a2) where
    schema _ = (schema @t1 Proxy, schema @t2 Proxy)

instance
    ( Schema t (f 'ValueLevel)
    ) => Schema
        ('GContextFree (t :: f 'TypeLevel))
        (GRule 'ValueLevel (f 'ValueLevel)) where
    schema _ = GContextFree (schema @t Proxy)

instance
    ( Schema lst1 [[Text]]
    , Schema t (f 'ValueLevel)
    , Schema lst2 [([Int], f ValueLevel)]
    ) => Schema
        ('GDependent lst1 (t :: f 'TypeLevel) lst2)
        (GRule 'ValueLevel (f 'ValueLevel)) where
    schema _= GDependent
        (schema @lst1 Proxy)
        (schema @t Proxy)
        (schema @lst2 Proxy)

instance Schema 'GStringAscii GStringType where
    schema _ = GStringAscii

instance Schema 'GStringICAO GStringType where
    schema _ = GStringICAO

instance Schema 'GStringOctal GStringType where
    schema _ = GStringOctal

instance Schema 'GSigned GSignedness where
    schema _ = GSigned

instance Schema 'GUnsigned GSignedness where
    schema _ = GUnsigned

instance Schema 'GPlus GPlusMinus where
    schema _ = GPlus

instance Schema 'GMinus GPlusMinus where
    schema _ = GMinus

instance
    ( Schema pm GPlusMinus
    , Schema n Int
    ) => Schema ('GZ pm n) VZ where
    schema _ = GZ (schema @pm Proxy) (schema @n Proxy)

instance
    ( Schema z VZ
    ) => Schema ('GNumInt z) VNumber where
    schema _ = GNumInt (schema @z Proxy)

instance
    ( Schema n1 (GNumber ValueLevel)
    , Schema n2 (GNumber ValueLevel)
    ) => Schema ('GNumDiv n1 n2) VNumber where
    schema _ = GNumDiv (schema @n1 Proxy) (schema @n2 Proxy)

instance
    ( Schema a (GZ ValueLevel)
    , Schema b (GZ ValueLevel)
    ) => Schema ('GNumPow a b) VNumber where
    schema _ = GNumPow (schema @a Proxy) (schema @b Proxy)

instance Schema 'GBdsWithAddress VBdsType where
    schema _ = GBdsWithAddress

instance
    ( Schema ma (Maybe Int)
    ) => Schema ('GBdsAt ma) VBdsType where
    schema _ = GBdsAt (schema @ma Proxy)

instance Schema 'GContentRaw VContent where
    schema _ = GContentRaw

instance
    ( Schema lst [(Int, Text)]
    ) => Schema ('GContentTable lst) VContent where
    schema _ = GContentTable (schema @lst @[(Int, Text)] Proxy)

instance
    ( Schema st GStringType
    ) => Schema ('GContentString st) VContent where
    schema _ = GContentString (schema @st Proxy)

instance
    ( Schema sig GSignedness
    ) => Schema ('GContentInteger sig) VContent where
    schema _ = GContentInteger (schema @sig Proxy)

instance
    ( Schema sig GSignedness
    , Schema lsb (GNumber ValueLevel)
    , Schema unit Text
    ) => Schema ('GContentQuantity sig lsb unit) VContent where
    schema _ = GContentQuantity
        (schema @sig Proxy)
        (schema @lsb Proxy)
        (schema @unit Proxy)

instance
    ( Schema bt (GBdsType ValueLevel)
    ) => Schema ('GContentBds bt) VContent where
    schema _ = GContentBds (schema @bt Proxy)

instance
    ( Schema n Int
    ) => Schema ('GRepetitiveRegular n) VRepetitiveType where
    schema _ = GRepetitiveRegular (schema @n Proxy)

instance Schema 'GRepetitiveFx VRepetitiveType where
    schema _ = GRepetitiveFx

instance Schema 'GReservedExpansion GExplicitType where
    schema _ = GReservedExpansion

instance Schema 'GSpecialPurpose GExplicitType where
    schema _ = GSpecialPurpose

instance
    ( Schema o Int8
    , Schema n Int
    , Schema rule (VRule VContent)
    ) => Schema ('GElement o n rule) VVariation where
    schema _ = GElement
        (unInt8 $ schema @o Proxy)
        (schema @n Proxy)
        (schema @rule Proxy)

instance
    ( Schema o Int8
    , Schema ts [VItem]
    ) => Schema ('GGroup o ts) VVariation where
    schema _ = GGroup (unInt8 $ schema @o Proxy) (schema @ts Proxy)

instance
    ( Schema lst [Maybe VItem]
    ) => Schema ('GExtended lst) VVariation where
    schema _ = GExtended (schema @lst Proxy)

instance
    ( Schema rt VRepetitiveType
    , Schema var VVariation
    ) => Schema ('GRepetitive rt var) VVariation where
    schema _ = GRepetitive (schema @rt Proxy) (schema @var Proxy)

instance
    ( Schema met (Maybe GExplicitType)
    ) => Schema ('GExplicit met) VVariation where
    schema _ = GExplicit (schema @met Proxy)

instance
    ( Schema lst [Maybe VNonSpare]
    ) => Schema ('GCompound lst) VVariation where
    schema _ = GCompound (schema @lst Proxy)

instance
    ( Schema name Text
    , Schema title Text
    , Schema rule (VRule VVariation)
    ) => Schema ('GNonSpare name title rule) VNonSpare where
    schema _ = GNonSpare
        (schema @name Proxy)
        (schema @title Proxy)
        (schema @rule Proxy)

instance
    ( Schema o Int8
    , Schema n Int
    ) => Schema ('GSpare o n) VItem where
    schema _ = GSpare (unInt8 $ schema @o Proxy) (schema @n Proxy)

instance
    ( Schema nsp VNonSpare
    ) => Schema ('GItem nsp) VItem where
    schema _ = GItem (schema @nsp Proxy)

instance
    ( Schema nsp VNonSpare
    ) => Schema ('GUapItem nsp) VUapItem where
    schema _ = GUapItem (schema @nsp Proxy)

instance Schema 'GUapItemSpare VUapItem where
    schema _ = GUapItemSpare

instance Schema 'GUapItemRFS VUapItem where
    schema _ = GUapItemRFS

instance
    ( Schema lst [VUapItem]
    ) => Schema ('GRecord lst) VRecord where
    schema _ = GRecord (schema @lst Proxy)

instance
    ( Schema iname [Text]
    , Schema lst [(Int, Text)]
    ) => Schema ('GUapSelector iname lst) VUapSelector where
    schema _ = GUapSelector (schema @iname Proxy) (schema @lst Proxy)

instance
    ( Schema rec VRecord
    ) => Schema ('GUap rec) VUap where
    schema _ = GUap (schema @rec Proxy)

instance
    ( Schema lst [(Text, VRecord)]
    , Schema msel (Maybe VUapSelector)
    ) => Schema ('GUaps lst msel) VUap where
    schema _ = GUaps (schema @lst Proxy) (schema @msel Proxy)

instance
    ( Schema cat Int
    , Schema uap VUap
    ) => Schema ('GDatablock cat uap) VDatablock where
    schema _ = GDatablock (schema @cat Proxy) (schema @uap Proxy)

instance
    ( Schema mn (Maybe Int)
    , Schema lst [Maybe VNonSpare]
    ) => Schema ('GExpansion mn lst) VExpansion where
    schema _ = GExpansion (schema @mn Proxy) (schema @lst Proxy)

instance
    ( Schema a Int
    , Schema b Int
    ) => Schema ('GEdition a b) VEdition where
    schema _ = GEdition (schema @a Proxy) (schema @b Proxy)

instance
    ( Schema cat Int
    , Schema ed VEdition
    , Schema uap VUap
    ) => Schema ('GAsterixBasic cat ed uap) VAsterix where
    schema _ = GAsterixBasic
        (schema @cat Proxy)
        (schema @ed Proxy)
        (schema @uap Proxy)

instance
    ( name ~ VText
    , uap ~ 'GUaps lst msel
    , Schema lst [(VText, VRecord)]
    ) => Schema ('GAsterixBasic cat ed uap) [(name, VRecord)] where
    schema _ = schema @lst Proxy

instance
    ( Schema cat Int
    , Schema ed VEdition
    , Schema exp VExpansion
    ) => Schema ('GAsterixExpansion cat ed exp) VAsterix where
    schema _ = GAsterixExpansion
        (schema @cat Proxy)
        (schema @ed Proxy)
        (schema @exp Proxy)

-- | TypeError wrapper.
type Unspecified (t :: k)
    = TypeError ('Text "Unspecified " :<>: ShowType t)

-- | Name is not defined.
type NotDefined (name :: Symbol)
    = TypeError ('Text "Not defined: " :<>: 'Text name)

-- | Type level lookup.
type family Lookup (name :: Symbol) (lst :: [(Symbol, k)]) :: k where
    Lookup name '[] = NotDefined name
    Lookup name1 ( '(name2, uap) ': ts) = If
        (name1 == name2)
        uap
        (Lookup name1 ts)

-- | Statically known bit sizes.
type BitSizeOf :: k -> Nat
type family BitSizeOf t where
    BitSizeOf '[] = 0
    BitSizeOf (t ': ts) = BitSizeOf t + BitSizeOf ts
    BitSizeOf ('GElement o n rc) = n
    BitSizeOf ('GGroup o lst) = BitSizeOf lst
    BitSizeOf ('GContextFree t) = BitSizeOf t
    BitSizeOf ('GDependent lst1 t lst2) = BitSizeOf t
    BitSizeOf ('GSpare o n) = n
    BitSizeOf ('GNonSpare name title rv) = BitSizeOf rv
    BitSizeOf ('GItem nsp) = BitSizeOf nsp
    BitSizeOf other = Unspecified other

-- | Extract category.
type CategoryOf :: k -> Nat
type family CategoryOf t where
    CategoryOf ('GAsterixBasic cat ed uap) = cat
    CategoryOf ('GDatablock cat uap) = cat
    CategoryOf other = Unspecified other

-- | Extract record type from single uap basic asterix.
type RecordOf :: TAsterix -> TRecord
type family RecordOf t where
    RecordOf ('GAsterixBasic cat ed ('GUap rec)) = rec
    RecordOf other = Unspecified other

-- | Extract record type from multiple uap basic asterix.
type RecordOfUap :: TAsterix -> Symbol -> TRecord
type family RecordOfUap t name where
    RecordOfUap ('GAsterixBasic cat ed ('GUaps lst sel)) name
        = Lookup name lst
    RecordOfUap other name = Unspecified other

-- | Get Datablock type.
type family DatablockOf (t :: TAsterix) :: TDatablock where
    DatablockOf ('GAsterixBasic cat ed uap) = 'GDatablock cat uap
    DatablockOf other = Unspecified other

-- | Get Expansion type.
type family ExpansionOf (t :: GAsterix u) :: GExpansion u where
    ExpansionOf ('GAsterixExpansion cat ed exp) = exp
    ExpansionOf other = Unspecified other

-- | Extract first group of extension.
type family ExtendedFirstGroup (lst :: [Maybe a]) :: [a] where
    ExtendedFirstGroup '[] = '[]
    ExtendedFirstGroup ('Nothing ': ts) = '[]
    ExtendedFirstGroup ('Just t ': ts) = t ': ExtendedFirstGroup ts

-- | Does extended end with Fx?
type family ExtendedTrailingFx (lst :: [Maybe a]) :: Bool where
    ExtendedTrailingFx '[] = 'False
    ExtendedTrailingFx ('Nothing ': ts) = 'True
    ExtendedTrailingFx ('Just t ': ts) = ExtendedTrailingFx ts

-- | Remaining extended items.
type family ExtendedRemainingItems (lst :: [Maybe a]) :: [Maybe a] where
    ExtendedRemainingItems '[] = '[]
    ExtendedRemainingItems ('Nothing ': ts) = ts
    ExtendedRemainingItems ('Just t ': ts) = ExtendedRemainingItems ts

-- | Extract record type from datablock type.
type family TypeOfRecord (t :: TDatablock) :: TRecord where
    TypeOfRecord ('GDatablock cat ('GUap rec)) = rec
    TypeOfRecord db = TypeError ('Text "Not defined")

-- | Does this datablock contain multiple UAPs?
type family IsMultiUap (t :: GDatablock u) :: Bool where
    IsMultiUap ('GDatablock cat ('GUap rec)) = 'False
    IsMultiUap ('GDatablock cat ('GUaps lst sel)) = 'True

-- | Find UAP name, based on given record.
type family UapName r lst :: Symbol where
    UapName r '[] = TypeError ('Text "Wrong record" :<>: ShowType r)
    UapName r1 ( '(name, r2) ': ts) = If
        (r1 == r2)
        name
        (UapName r1 ts)

-- | Find group subitem by name.
type family LookupGroup (name :: Symbol) (lst :: [TItem]) :: Nat where
    LookupGroup name '[] = NotDefined name
    LookupGroup name ('GSpare o n ': ts) = 1 + LookupGroup name ts
    LookupGroup name ('GItem ('GNonSpare name1 title rv) ': ts) = If
        (name1 == name) 0 (1 + LookupGroup name ts)

-- | Find extended subitem by name.
type family LookupExtended (name :: Symbol) (lst :: [Maybe TItem]) :: Nat where
    LookupExtended name '[] = NotDefined name
    LookupExtended name ('Nothing ': ts) = 1 + LookupExtended name ts
    LookupExtended name ('Just ('GSpare o n) ': ts) = 1 + LookupExtended name ts
    LookupExtended name ('Just ('GItem ('GNonSpare name1 title rv)) ': ts) = If
        (name1 == name) 0 (1 + LookupExtended name ts)

-- | Find compound subitem by name.
type family LookupCompound (name :: Symbol) (lst :: [Maybe TNonSpare]) :: Nat where
    LookupCompound name '[] = NotDefined name
    LookupCompound name ('Nothing ': ts) = 1 + LookupCompound name ts
    LookupCompound name ('Just ('GNonSpare name1 title rv) ': ts) = If
        (name1 == name) 0 (1 + LookupCompound name ts)

-- | Find record subitem by name.
type family LookupRecord (name :: Symbol) (lst :: [TUapItem]) :: Nat where
    LookupRecord name '[] = NotDefined name
    LookupRecord name ('GUapItem ('GNonSpare name1 title rv) ': ts) = If
        (name1 == name) 0 (1 + LookupRecord name ts)
    LookupRecord name ('GUapItemSpare ': ts) = 1 + LookupRecord name ts
    LookupRecord name ('GUapItemRFS ': ts) = 1 + LookupRecord name ts

-- | Extract NonSpares from uap item listing.
type family RecordNonSpares (t :: [TUapItem]) :: [TNonSpare]
  where
    RecordNonSpares '[] = '[]
    RecordNonSpares ('GUapItem nsp ': ts) = nsp ': RecordNonSpares ts
    RecordNonSpares ('GUapItemSpare ': ts) = RecordNonSpares ts
    RecordNonSpares ('GUapItemRFS ': ts) = RecordNonSpares ts

-- | Extract NonSpares from item listing.
type family ItemNonSpares (t :: [TItem]) :: [TNonSpare]
  where
    ItemNonSpares '[] = '[]
    ItemNonSpares ('GSpare o2 n2 ': ts) = ItemNonSpares ts
    ItemNonSpares ('GItem nsp ': ts) = nsp ': ItemNonSpares ts

-- | Type level list filtering.
type family FilterMaybe (lst :: [Maybe a]) :: [a] where
    FilterMaybe '[] = '[]
    FilterMaybe ('Nothing ': ts) = FilterMaybe ts
    FilterMaybe ('Just t ': ts) = t ': FilterMaybe ts

-- | Prepend name.
type family PrependName t where
    PrependName '[] = '[]
    PrependName ('GNonSpare name title rv ': ts)
        = '(name, 'GNonSpare name title rv) ': PrependName ts

-- | Overloaded type level function to extract subtype from parent, based on name.
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

-- | Extract dependent rule.
type family DepRule2 lst ix where
    DepRule2 ('(ix1, t) ': ts) ix = If (ix1 == ix) t (DepRule2 ts ix)
    DepRule2 t ix = TypeError ('Text "Undefined rule: " :<>: ShowType ix)
type family DepRule t ix where
    DepRule ('GNonSpare name title ('GDependent lst1 d lst2)) ix = DepRule2 lst2 ix

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

