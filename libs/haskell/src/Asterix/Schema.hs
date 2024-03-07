-- | Asterix specifications schema in T-type and V-value level version
-- with the conversions from types to values.

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}

module Asterix.Schema
( module Asterix.Schema
, Text
, Some
) where

import           Data.GADT.Show
import           Data.Proxy
import           Data.Some
import           Data.String
import           Data.Text
import           Data.Word
import           GHC.TypeLits

import           Alignment

-- 'common' and 'type level' definitions

type family Length xs where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs

type family Times a b where
    Times 1 a = a
    Times n a = a + Times (n-1) a

data PlusMinus
    = Plus
    | Minus
    deriving (Show, Eq, Ord)

data TNumber
    = TNumInt PlusMinus Nat
    | TNumDiv TNumber TNumber -- division
    | TNumPow Nat Nat -- power function
    deriving (Show, Eq, Ord)

data BdsType t
    = BdsWithAddress
    | BdsAt (Maybe t)
    deriving (Show, Eq, Ord)

data StringType
    = StringAscii
    | StringICAO
    | StringOctal
    deriving (Show, Eq, Ord)

data Signedness
    = Signed
    | Unsigned
    deriving (Show, Eq, Ord)

data Constrain a
    = EqualTo a
    | NotEqualTo a
    | GreaterThan a
    | GreaterThanOrEqualTo a
    | LessThan a
    | LessThanOrEqualTo a
    deriving (Show, Eq, Ord, Functor)

data Content n s intNum floatNum
    = ContentRaw
    | ContentTable
        [(n, s)] -- table rows
    | ContentString
        StringType
    | ContentInteger
        Signedness
        [Constrain intNum]
    | ContentQuantity
        Signedness
        floatNum -- lsb
        s  -- unit
        [Constrain floatNum]
    | ContentBds
        (BdsType n)
    deriving (Show, Eq, Ord)

type TContent = Content Nat Symbol TNumber TNumber

data TRule a
    = TContextFree a
    | TDependent [[Symbol]] a [([Nat], a)]

data ExplicitType
    = ReservedExpansion
    | SpecialPurpose
    deriving (Show, Eq, Ord)

data TVariation
    = TElement
        Nat -- bit offsetMod
        Nat -- bit length
        (TRule TContent)
    | TGroup
        [TItem]
    | TExtended
        [Maybe TItem] -- Nothing = FX bit
    | TRepetitive
        (Maybe Nat) -- header length (Nothing for FX type)
        TVariation
    | TExplicit (Maybe ExplicitType)
    | TCompound
        [Maybe TItem] -- Nothing = Spare bit

data TItem
    = TSpare
        Nat -- bit offsetMod
        Nat -- bit length
    | TItem
        Symbol -- name
        Symbol -- title
        (TRule TVariation)

data UapItem t
    = UapItem t
    | UapItemSpare
    | UapItemRFS
    deriving (Show, Eq, Ord)

newtype Record t = Record [UapItem t]
    deriving (Show, Eq, Ord)

data Expansion n t = Expansion n [Maybe t]
    deriving (Show, Eq, Ord)

data TUap
    = TUapSingle (Record TItem)
    | TUapMultiple [(Symbol, Record TItem)]

data Edition a b = Edition a b
    deriving (Show, Eq, Ord)

-- | Asterix specification at type level
data TAsterix
    = TBasic Nat (Edition Nat Nat) TUap
    | TExpansion Nat (Edition Nat Nat) (Expansion Nat TItem)

-- Value level definitions

type VContent = Content Int Text Int Double

data RuleType
    = RTContextFree
    | RTDependent

data VRule a (t :: RuleType) where
    VContextFree :: a -> VRule a RTContextFree
    VDependent :: [[Text]] -> a -> [([Int], a)] -> VRule a RTDependent

deriving instance Show a => Show (VRule a t)

instance Show a => GShow (VRule a) where
    gshowsPrec = defaultGshowsPrec

data VariationType
    = VTElement
    | VTGroup
    | VTExtended
    | VTRepetitive
    | VTExplicit
    | VTCompound
    deriving (Show, Eq, Ord)

data VVariation (t :: VariationType) where
    VElement    :: BitOffsetMod8 -> BitSize -> Some (VRule VContent) -> VVariation 'VTElement
    VGroup      :: [Some VItem] -> VVariation 'VTGroup
    VExtended   :: [Maybe (Some VItem)] -> VVariation 'VTExtended
    VRepetitive :: Maybe Int -> (Some VVariation) -> VVariation 'VTRepetitive
    VExplicit   :: Maybe ExplicitType -> VVariation 'VTExplicit
    VCompound   :: [Maybe (Some VItem)] -> VVariation 'VTCompound

deriving instance Show (VVariation t)

instance GShow VVariation where
    gshowsPrec = defaultGshowsPrec

data ItemType
    = ITSpare
    | ITItem
    deriving (Show, Eq, Ord)

data VItem (t :: ItemType) where
    VSpare :: BitOffsetMod8 -> BitSize -> VItem 'ITSpare
    VItem  :: Text -> Text -> Some (VRule (Some VVariation)) -> VItem 'ITItem

deriving instance Show (VItem t)

instance GShow VItem where
    gshowsPrec = defaultGshowsPrec

data UapType
    = UTSingle
    | UTMultiple
    deriving (Show, Eq, Ord)

data VUap (t :: UapType) where
    VUapSingle :: Record (Some VItem)
        -> VUap 'UTSingle
    VUapMultiple :: [(Text, Record (Some VItem))]
        -> VUap 'UTMultiple

deriving instance Show (VUap t)

instance GShow VUap where
    gshowsPrec = defaultGshowsPrec

newtype CatNum = CatNum Word8
    deriving (Show, Eq, Ord, Num)

data AsterixType
    = AsterixBasic
    | AsterixExpansion

data VAsterix (t :: AsterixType) where
    VBasic :: CatNum -> Edition Int Int -> Some VUap -> VAsterix 'AsterixBasic
    VExpansion :: CatNum -> Edition Int Int -> Expansion Int (Some VItem) -> VAsterix 'AsterixExpansion

deriving instance Show (VAsterix t)

instance GShow VAsterix where
    gshowsPrec = defaultGshowsPrec

-- Conversion from types to terms

err :: a
err = error "internal error"

class IsSchema k t where
    schema :: t

-- | Nat -> Num
nv :: (KnownNat k, Num t) => Proxy k -> t
nv = fromIntegral . natVal

-- | Symbol -> IsString
sv :: (KnownSymbol k, IsString t) => Proxy k -> t
sv = fromString . symbolVal

-- Nat -> Num

instance
    ( KnownNat k
    , Num t
    ) => IsSchema k t where
    schema = fromIntegral $ natVal (Proxy @k)

-- Symbol -> Text

instance
    ( KnownSymbol k
    , IsString t
    ) => IsSchema k t where
    schema = fromString $ symbolVal (Proxy @k)

-- Maybe a

instance
    ( t ~ Maybe a
    ) => IsSchema 'Nothing t where
    schema = Nothing

instance
    ( t ~ Maybe a
    , IsSchema k a
    ) => IsSchema ('Just k) t  where
    schema = Just (schema @k)

-- '[] -> []

instance
    ( t ~ [a]
    ) => IsSchema '[] t where
    schema = []

instance
    ( t ~ [a]
    , IsSchema x a
    , IsSchema xs [a]
    ) => IsSchema (x ': xs) t where
    schema = schema @x : schema @xs

-- '(ta, tb) -> (a, b)

instance
    ( t ~ (x, y)
    , IsSchema kx x
    , IsSchema ky y
    ) => IsSchema '(kx, ky) t where
    schema = (schema @kx, schema @ky)

-- k -> Some t

instance
    ( IsSchema k (t a)
    ) => IsSchema k (Some t) where
    schema = mkSome (schema @k @(t a))

--  Number

instance
    ( Num n
    ) => IsSchema 'Plus n where
    schema = 1

instance
    ( Num n
    ) => IsSchema 'Minus n where
    schema = -1

instance
    ( Num b
    , IsSchema plusMinus Int
    , IsSchema n b
    ) => IsSchema ('TNumInt plusMinus n) b where
    schema = fromIntegral (schema @plusMinus @Int) * schema @n

instance
    ( IsSchema a c
    , IsSchema b c
    , Fractional c
    ) => IsSchema ('TNumDiv a b) c where
    schema = schema @a / schema @b

instance
    ( Num c
    , IsSchema a Integer
    , IsSchema b Integer
    ) => IsSchema ('TNumPow a b) c where
    schema = fromIntegral (schema @a :: Integer) ^ (schema @b :: Integer)

-- String type

instance t ~ StringType => IsSchema 'StringAscii t where schema = StringAscii
instance t ~ StringType => IsSchema 'StringICAO t where schema = StringICAO
instance t ~ StringType => IsSchema 'StringOctal t where schema = StringOctal

-- Signedness

instance t ~ Signedness => IsSchema 'Signed t where schema = Signed
instance t ~ Signedness => IsSchema 'Unsigned t where schema = Unsigned

{-
TODO...
-- Constrain

instance t ~ Constrain => IsSchema 'EqualTo t where schema = EqualTo
instance t ~ Constrain => IsSchema 'NotEqualTo t where schema = NotEqualTo
instance t ~ Constrain => IsSchema 'GreaterThan t where schema = GreaterThan
instance t ~ Constrain => IsSchema 'GreaterThanOrEqualTo t where schema = GreaterThanOrEqualTo
instance t ~ Constrain => IsSchema 'LessThan t where schema = LessThan
instance t ~ Constrain => IsSchema 'LessThanOrEqualTo t where schema = LessThanOrEqualTo
-}

-- Explicit type
instance t ~ ExplicitType => IsSchema 'ReservedExpansion t where schema = ReservedExpansion
instance t ~ ExplicitType => IsSchema 'SpecialPurpose t where schema = SpecialPurpose

-- Bds type

instance
    ( t ~ BdsType Int
    ) => IsSchema 'BdsWithAddress t where
    schema = BdsWithAddress

instance
    ( t ~ BdsType Int
    , IsSchema mn (Maybe Int)
    ) => IsSchema ('BdsAt mn) t where
    schema = BdsAt (schema @mn)

instance
    ( t ~ UapItem a
    , IsSchema k a
    ) => IsSchema ('UapItem k) t where
    schema = UapItem (schema @k)

instance
    ( t ~ UapItem a
    ) => IsSchema 'UapItemSpare t where schema = UapItemSpare

instance
    ( t ~ UapItem a
    ) => IsSchema 'UapItemRFS t where schema = UapItemRFS

-- ContentRaw

instance
    ( t ~ VContent
    ) => IsSchema 'ContentRaw t where
    schema = ContentRaw

-- ContentTable

instance
    ( t ~ VContent
    , IsSchema lst [(Int, Text)]
    ) => IsSchema ('ContentTable lst) t where
    schema = ContentTable (schema @lst)

-- ContentString

instance
    ( t ~ VContent
    , IsSchema st StringType
    ) => IsSchema ('ContentString st) t where
    schema = ContentString (schema @st)

-- ContentInteger

instance
    ( t ~ VContent
    , IsSchema sig Signedness
    , IsSchema cons [Constrain Int]
    ) => IsSchema ('ContentInteger sig cons) t where
    schema = ContentInteger (schema @sig) (schema @cons)

-- Content quantity

instance
    ( t ~ VContent
    , IsSchema sig Signedness
    , IsSchema lsb Double
    , IsSchema unit Text
    , IsSchema cons [Constrain Double]
    ) => IsSchema ('ContentQuantity sig lsb unit cons) t where
    schema = ContentQuantity (schema @sig) (schema @lsb) (schema @unit) (schema @cons)

-- Content BDS

instance
    ( t ~ VContent
    , IsSchema bt (BdsType Int)
    ) => IsSchema ('ContentBds bt) t where
    schema = ContentBds (schema @bt)

-- TRule -> VRule

instance
    ( t ~ 'RTContextFree
    , IsSchema a b
    ) => IsSchema ('TContextFree a) (VRule b t) where
    schema = VContextFree (schema @a @b)

instance
    ( t ~ 'RTDependent
    , IsSchema names [[Text]]
    , IsSchema dv b
    , IsSchema lst [([Int], b)]
    ) => IsSchema ('TDependent names dv lst) (VRule b t) where
    schema = VDependent (schema @names) (schema @dv) (schema @lst)

-- TElement -> VVariation

instance
    ( vt ~ 'VTElement
    , IsSchema o BitOffsetMod8
    , IsSchema n BitSize
    , IsSchema rule (VRule VContent rt)
    ) => IsSchema ('TElement o n rule) (VVariation vt) where
    schema =VElement (schema @o) (schema @n) (mkSome $ schema @rule @(VRule VContent rt))

-- TGroup -> VVariation

instance
    ( t ~ 'VTGroup
    , IsSchema xs [Some VItem]
    ) => IsSchema ('TGroup xs) (VVariation t) where
    schema = VGroup (schema @xs)

--TExtended -> VVariation

instance
    ( t ~ 'VTExtended
    , IsSchema xs [Maybe (Some VItem)]
    ) => IsSchema ('TExtended xs) (VVariation t) where
    schema = VExtended (schema @xs)

--TRepetitive -> VVariation

instance
    ( t ~ 'VTRepetitive
    , IsSchema mn (Maybe Int)
    , IsSchema tvar (VVariation v)
    ) => IsSchema ('TRepetitive mn tvar) (VVariation t) where
    schema = VRepetitive (schema @mn) (mkSome $ schema @tvar @(VVariation v))

--TExplicit -> VVariation

instance
    ( t ~ 'VTExplicit
    , IsSchema met (Maybe ExplicitType)
    ) => IsSchema ('TExplicit met) (VVariation t) where
    schema = VExplicit (schema @met)

--TCompound -> VVariation

instance
    ( t ~ 'VTCompound
    , IsSchema mn (Maybe Int)
    , IsSchema lst [Maybe (Some VItem)]
    ) => IsSchema ('TCompound lst) (VVariation t) where
    schema = VCompound (schema @lst)

-- TSpare -> VSpare

instance
    ( t ~ 'ITSpare
    , IsSchema o BitOffsetMod8
    , IsSchema n BitSize
    ) => IsSchema ('TSpare o n) (VItem t) where
    schema = VSpare (schema @o) (schema @n)

-- TItem -> VItem

instance
    ( t ~ 'ITItem
    , IsSchema name Text
    , IsSchema title Text
    , IsSchema rule (VRule (Some VVariation) rt)
    ) => IsSchema ('TItem name title rule) (VItem t) where
    schema = VItem (schema @name) (schema @title)
        (mkSome $ schema @rule @(VRule (Some VVariation) rt))

-- Record TItem -> Record (Some VItem)

instance
    ( t ~ Some VItem
    , IsSchema k [UapItem (Some VItem)]
    ) => IsSchema ('Record k) (Record t) where
    schema = Record (schema @k)

-- Expansion TItem -> Expansion (Some VItem)

instance
    ( t ~ Some VItem
    , IsSchema n Int
    , IsSchema k [Maybe (Some VItem)]
    , Length k <= Times 8 n
    ) => IsSchema ('Expansion n k) (Expansion Int t) where
    schema = Expansion (schema @n) (schema @k)

-- TUap -> VUap (Single)

instance
    ( t ~ 'UTSingle
    , IsSchema record (Record (Some VItem))
    ) => IsSchema ('TUapSingle record) (VUap t) where
    schema = VUapSingle (schema @record)

-- TUap -> VUap (Multiple)

instance
    ( t ~ 'UTMultiple
    , IsSchema lst [(Text, Record (Some VItem))]
    ) => IsSchema ('TUapMultiple lst) (VUap t) where
    schema = VUapMultiple (schema @lst)

-- Edition Nat Nat -> Edition Int Int

instance
    ( t ~ Edition Int Int
    , IsSchema a Int
    , IsSchema b Int
    ) => IsSchema ('Edition a b) t where
    schema = Edition (schema @a) (schema @b)

-- TAsterix -> VAsterix (Basic)

instance
    ( t ~ 'AsterixBasic
    , IsSchema ed (Edition Int Int)
    , IsSchema cat CatNum
    , IsSchema uap (VUap u)
    ) => IsSchema ('TBasic cat ed uap) (VAsterix t) where
    schema = VBasic (schema @cat) (schema @ed) (mkSome $ schema @uap @(VUap u))

-- TAsterix -> VAsterix (Expansion)

instance
    ( t ~ 'AsterixExpansion
    , IsSchema ed (Edition Int Int)
    , IsSchema cat CatNum
    , IsSchema expansion (Expansion Int (Some VItem))
    ) => IsSchema ('TExpansion cat ed expansion) (VAsterix t) where
    schema = VExpansion (schema @cat) (schema @ed) (schema @expansion)
