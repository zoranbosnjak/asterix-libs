-- | Asterix specifications schema in T-type and V-value level version
-- with the conversions from types to values.

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE UndecidableInstances #-}

module Asterix.Schema where

import           Data.GADT.Show
import           Data.Proxy
import           Data.Some
import           Data.String
import           Data.Text
import           Data.Word
import           GHC.TypeLits

import           Alignment

-- 'common' and 'type level' definitions

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

data Constrain
    = EqualTo
    | NotEqualTo
    | GreaterThan
    | GreaterThanOrEqualTo
    | LessThan
    | LessThanOrEqualTo
    deriving (Show, Eq, Ord)

data Content n s intNum floatNum
    = ContentRaw
    | ContentTable
        [(n, s)] -- table rows
    | ContentString
        StringType
    | ContentInteger
        Signedness
        [(Constrain, intNum)]
    | ContentQuantity
        Signedness
        floatNum -- lsb
        s  -- unit
        [(Constrain, floatNum)]
    | ContentBds
        (BdsType n)
    deriving (Show, Eq, Ord)

type TContent = Content Nat Symbol TNumber TNumber

data TRule
    = TContextFree TContent
    | TDependent [Symbol] [(Nat, TContent)]

data ExplicitType
    = ReservedExpansion
    | SpecialPurpose
    deriving (Show, Eq, Ord)

data CompoundSubitem a
    = CompoundSubitem a
    | CompoundSpare
    | CompoundRFS
    deriving (Show, Eq, Ord)

data TVariation
    = TElement
        Nat -- bit offsetMod
        Nat -- bit length
        TRule
    | TGroup
        [TItem]
    | TExtended
        [Maybe TItem] -- Nothing = FX bit
    | TRepetitive
        (Maybe Nat) -- header length (Nothing for FX type)
        TVariation
    | TExplicit (Maybe ExplicitType)
    | TCompound
        (Maybe Nat) -- fixed fspec length or fx based
        [CompoundSubitem TItem]

data TItem
    = TSpare
        Nat -- bit offsetMod
        Nat -- bit length
    | TItem
        Symbol -- name
        Symbol -- title
        TVariation

data Record t = Record [CompoundSubitem t]
    deriving (Show, Eq, Ord)

data Expansion n t = Expansion n [CompoundSubitem t]
    deriving (Show, Eq, Ord)

data TUap
    = TUapSingle (Record TItem)
    | TUapMultiple [(Symbol, (Record TItem))]

data Edition a b = Edition a b
    deriving (Show, Eq, Ord)

-- | Asterix specification at type level
data TSpec
    = TCat Nat (Edition Nat Nat) TUap
    | TRef Nat (Edition Nat Nat) (Expansion Nat TItem)

-- Value level definitions

type VContent = Content Int Text Int Double

data RuleType
    = RTContextFree
    | RTDependent

data VRule (t :: RuleType) where
    VContextFree :: VContent
        -> VRule RTContextFree
    VDependent :: [Text] -> [(Int, VContent)]
        -> VRule RTDependent

deriving instance Show (VRule t)

instance GShow VRule where
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
    VElement    :: BitOffsetMod8 -> BitSize -> Some VRule -> VVariation 'VTElement
    VGroup      :: [Some VItem] -> VVariation 'VTGroup
    VExtended   :: [Maybe (Some VItem)] -> VVariation 'VTExtended
    VRepetitive :: Maybe Int -> (Some VVariation) -> VVariation 'VTRepetitive
    VExplicit   :: Maybe ExplicitType -> VVariation 'VTExplicit
    VCompound   :: Maybe Int -> [CompoundSubitem (Some VItem)] -> VVariation 'VTCompound

deriving instance Show (VVariation t)

instance GShow VVariation where
    gshowsPrec = defaultGshowsPrec

data ItemType
    = ITSpare
    | ITItem
    deriving (Show, Eq, Ord)

data VItem (t :: ItemType) where
    VSpare :: BitOffsetMod8 -> BitSize -> VItem 'ITSpare
    VItem  :: Text -> Text -> Some VVariation -> VItem 'ITItem

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

newtype Cat = Cat { unCat :: Word8 }
    deriving (Show, Eq, Ord, Num)

data SpecType
    = STCat
    | STRef

data VAstSpec (t :: SpecType) where
    VCat :: Cat -> Edition Int Int -> Some VUap -> VAstSpec 'STCat
    VRef :: Cat -> Edition Int Int -> Expansion Int (Some VItem) -> VAstSpec 'STRef

deriving instance Show (VAstSpec t)

instance GShow VAstSpec where
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
    schema = (fromIntegral $ schema @plusMinus @Int) * schema @n

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

-- Constrain

instance t ~ Constrain => IsSchema 'EqualTo t where schema = EqualTo
instance t ~ Constrain => IsSchema 'NotEqualTo t where schema = NotEqualTo
instance t ~ Constrain => IsSchema 'GreaterThan t where schema = GreaterThan
instance t ~ Constrain => IsSchema 'GreaterThanOrEqualTo t where schema = GreaterThanOrEqualTo
instance t ~ Constrain => IsSchema 'LessThan t where schema = LessThan
instance t ~ Constrain => IsSchema 'LessThanOrEqualTo t where schema = LessThanOrEqualTo

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
    ( t ~ CompoundSubitem a
    , IsSchema k a
    ) => IsSchema ('CompoundSubitem k) t where
    schema = CompoundSubitem (schema @k)

instance
    ( t ~ CompoundSubitem a
    ) => IsSchema 'CompoundSpare t where schema = CompoundSpare

instance
    ( t ~ CompoundSubitem a
    ) => IsSchema 'CompoundRFS t where schema = CompoundRFS

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
    , IsSchema cons [(Constrain, Int)]
    ) => IsSchema ('ContentInteger sig cons) t where
    schema = ContentInteger (schema @sig) (schema @cons)

-- Content quantity

instance
    ( t ~ VContent
    , IsSchema sig Signedness
    , IsSchema lsb Double
    , IsSchema unit Text
    , IsSchema cons [(Constrain, Double)]
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
    , IsSchema cont VContent
    ) => IsSchema ('TContextFree cont) (VRule t) where
    schema = VContextFree (schema @cont)

instance
    ( t ~ 'RTDependent
    , IsSchema name [Text]
    , IsSchema lst [(Int, VContent)]
    ) => IsSchema ('TDependent name lst) (VRule t) where
    schema = VDependent (schema @name) (schema @lst)

-- TElement -> VVariation

instance
    ( t ~ 'VTElement
    , IsSchema o BitOffsetMod8
    , IsSchema n BitSize
    , IsSchema rule (VRule r)
    ) => IsSchema ('TElement o n rule) (VVariation t) where
    schema = VElement (schema @o) (schema @n) (mkSome $ schema @rule @(VRule r))

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
    , IsSchema lst [CompoundSubitem (Some VItem)]
    ) => IsSchema ('TCompound mn lst) (VVariation t) where
    schema = VCompound (schema @mn) (schema @lst)

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
    , IsSchema var (VVariation v)
    ) => IsSchema ('TItem name title var) (VItem t) where
    schema = VItem (schema @name) (schema @title) (mkSome $ schema @var @(VVariation v))

-- Record TItem -> Record (Some VItem)

instance
    ( t ~ Some VItem
    , IsSchema k [CompoundSubitem (Some VItem)]
    ) => IsSchema ('Record k) (Record t) where
    schema = Record (schema @k)

-- Expansion TItem -> Expansion (Some VItem)

instance
    ( t ~ Some VItem
    , IsSchema n Int
    , IsSchema k [CompoundSubitem (Some VItem)]
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

-- TAstSpec -> VAstSpec (Cat)

instance
    ( t ~ 'STCat
    , IsSchema ed (Edition Int Int)
    , IsSchema cat Cat
    , IsSchema uap (VUap u)
    ) => IsSchema ('TCat cat ed uap) (VAstSpec t) where
    schema = VCat (schema @cat) (schema @ed) (mkSome $ schema @uap @(VUap u))

-- TAstSpec -> VAstSpec (Ref)

instance
    ( t ~ 'STRef
    , IsSchema ed (Edition Int Int)
    , IsSchema cat Cat
    , IsSchema expansion (Expansion Int (Some VItem))
    ) => IsSchema ('TRef cat ed expansion) (VAstSpec t) where
    schema = VRef (schema @cat) (schema @ed) (schema @expansion)
