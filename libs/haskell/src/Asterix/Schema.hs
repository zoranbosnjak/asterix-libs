-- | Asterix specifications schema in T-type and V-value level version.

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: remove this
-- {-# OPTIONS_GHC -Wno-all #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Asterix.Schema where

import           GHC.TypeLits
import           Data.Proxy
import           Data.Kind
import           Data.Text
import           Data.Word
import           Data.Some

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

data TRule
    = TContextFree (Content Nat Symbol TNumber TNumber)
    | TDependent [Symbol] [(Nat, Content Nat Symbol TNumber TNumber)]

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

newtype Record t = TRecord [CompoundSubitem t]

newtype Expansion t = TExpansion [CompoundSubitem t]

data TUap
    = TUapSingle (Record TItem)
    | TUapMultiple [(Symbol, (Record TItem))]

data Edition a b = Edition a b
    deriving (Show, Eq, Ord)

-- | Asterix specification at type level
data TSpec
    = TCat Nat (Edition Nat Nat) TUap
    | TRef Nat (Edition Nat Nat) (Expansion TItem)

-- Value level definitions

data RuleType
    = RTContextFree
    | RTDependent

data VRule (t :: RuleType) where
    VContextFree :: Content Int Text Int Double
        -> VRule RTContextFree
    VDependent :: [Text] -> [(Int, Content Int Text Int Double)]
        -> VRule RTDependent

data VariationType
    = VTElement
    | VTGroup
    | VTExtended
    | VTRepetitive
    | VTExplicit
    | VTRFS
    | VTCompound

data VVariation (t :: VariationType) where
    VElement    :: BitOffsetMod8 -> BitSize -> Some VRule -> VVariation 'VTElement
    VGroup      :: [Some VItem] -> VVariation 'VTGroup
    VExtended   :: [Maybe (Some VItem)] -> VVariation 'VTExtended
    VRepetitive :: Maybe Int -> Some VVariation -> VVariation 'VTRepetitive
    VExplicit   :: Maybe ExplicitType -> VVariation 'VTExplicit
    VCompound   :: Maybe Int -> [CompoundSubitem (Some VItem)] -> VVariation 'VTCompound

data ItemType
    = ITSpare
    | ITItem

data VItem (t :: ItemType) where
    VSpare :: BitOffsetMod8 -> BitSize -> VItem 'ITSpare
    VItem  :: Text -> Text -> Some VVariation -> VItem 'ITItem

data UapType
    = UTSingle
    | UTMultiple

data VUap (t :: UapType) where
    UapSingle :: Record (Some VItem)
        -> VUap 'UTSingle
    UapMultiple :: [(Text, Record (Some VItem))]
        -> VUap 'UTMultiple

newtype Cat = Cat { unCat :: Word8 }
    deriving (Show, Eq, Ord, Num)

data SpecType
    = STCat
    | STRef

data VSpec (t :: SpecType) where
    VCat :: Cat -> Edition Int Int -> Some VUap -> VSpec 'STCat
    VRef :: Cat -> Edition Int Int -> Expansion (Some VItem) -> VSpec 'STRef

-- Conversion from types to terms

err :: a
err = error "internal error"

-- | Nat value
nv :: (KnownNat n, Num a) => Proxy n -> a
nv = fromIntegral . natVal

-- | Symbol value
sv :: KnownSymbol s => Proxy s -> Text
sv = Data.Text.pack . symbolVal

class IsSchema t a where
    schema :: a

instance
    ( KnownNat a
    , KnownNat b
    ) => IsSchema ('Edition a b) (Edition Int Int) where
    schema = Edition (nv (Proxy @a)) (nv (Proxy @b))

instance
    ( KnownNat cat
    , IsSchema ed (Edition Int Int)
    , IsSchema uap (Some VUap)
    ) => IsSchema ('TCat cat ed uap) (VSpec 'STCat) where
    schema = VCat (nv (Proxy @cat)) (schema @ed) (schema @uap)

instance
    ( KnownNat cat
    , IsSchema ed (Edition Int Int)
    , IsSchema exp (Expansion (Some VItem))
    ) => IsSchema ('TRef cat ed exp) (VSpec 'STRef) where
    schema = VRef (nv (Proxy @cat)) (schema @ed) (schema @exp)

{-
instance KnownNat n => IsSchema n Int where
    schema = nv (Proxy @n)

instance KnownSymbol s => IsSchema s Text where
    schema = sv (Proxy @s)

-- | Helper class for 'repetitive' and 'compound'
class MNat t where mInt :: Maybe Int
instance MNat 'Nothing where mInt = Nothing
instance KnownNat n => MNat ('Just n) where mInt = Just $ nv (Proxy @n)

instance IsSchema '[] [a] where
    schema = []

instance
    ( IsSchema t a
    , IsSchema ts [a]
    ) => IsSchema (t ': ts) [a] where
    schema = schema @t : schema @ts

instance
    ( IsSchema t1 v1
    , IsSchema t2 v2
    ) => IsSchema '(t1, t2) (v1, v2) where
    schema = (schema @t1, schema @t2)

instance IsSchema 'Nothing (Maybe a) where
    schema = Nothing

instance IsSchema t a => IsSchema ('Just t) (Maybe a) where
    schema = Just (schema @t)

instance (KnownNat n) => IsSchema ('TNumInt 'TPlus n) VNumber where
    schema = VNumInt $ nv (Proxy @n)

instance (KnownNat n) => IsSchema ('TNumInt 'TMinus n) VNumber where
    schema = VNumInt $ nv (Proxy @n) * (-1)

instance
    ( IsSchema a VNumber
    , IsSchema b VNumber
    ) => IsSchema ('TNumDiv a b) VNumber where
    schema = VNumDiv (schema @a) (schema @b)

instance
    ( KnownNat a
    , KnownNat b
    ) => IsSchema ('TNumPow a b) VNumber where
    schema = VNumPow (nv (Proxy @a)) (nv (Proxy @b))

evalNumInteger :: VNumber -> Integer
evalNumInteger = \case
    VNumInt i -> i
    VNumDiv a b -> div (evalNumInteger a) (evalNumInteger b)
    VNumPow a b -> a ^ b

evalNumDouble :: VNumber -> Double
evalNumDouble = \case
    VNumInt i -> fromInteger i
    VNumDiv a b -> (/) (evalNumDouble a) (evalNumDouble b)
    VNumPow a b -> (fromInteger a) ^ b

class IsNum t where evalNum :: VNumber -> t
instance IsNum Integer where evalNum = evalNumInteger
instance IsNum Double where evalNum = evalNumDouble

instance IsSchema 'EqualTo Constrain where schema = EqualTo
instance IsSchema 'NotEqualTo Constrain where schema = NotEqualTo
instance IsSchema 'GreaterThan Constrain where schema = GreaterThan
instance IsSchema 'GreaterThanOrEqualTo Constrain where schema = GreaterThanOrEqualTo
instance IsSchema 'LessThan Constrain where schema = LessThan
instance IsSchema 'LessThanOrEqualTo Constrain where schema = LessThanOrEqualTo

instance IsSchema 'Signed Signedness where schema = Signed
instance IsSchema 'Unsigned Signedness where schema = Unsigned

instance IsSchema 'StringAscii StringType where schema = StringAscii
instance IsSchema 'StringICAO StringType where schema = StringICAO
instance IsSchema 'StringOctal StringType where schema = StringOctal

data VBdsType
    = VBdsWithAddress
    | VBdsAt (Maybe Int)
    deriving (Eq, Show)

instance IsSchema 'TBdsWithAddress VBdsType where schema = VBdsWithAddress
instance IsSchema ('TBdsAt 'Nothing) VBdsType where schema = VBdsAt Nothing
instance KnownNat n => IsSchema ('TBdsAt ('Just n)) VBdsType where
    schema = VBdsAt $ Just $ nv (Proxy @n)

data VContent
    = VContentRaw
    | VContentTable [(Int, Text)]
    | VContentString StringType
    | VContentInteger Signedness [(Constrain, Integer)]
    | VContentQuantity Signedness Double Text [(Constrain, Double)]
    | VContentBds VBdsType
    deriving (Eq, Show)

instance IsSchema 'TContentRaw VContent where
    schema = VContentRaw

instance IsSchema ('TContentTable '[]) VContent where
    schema = VContentTable []

instance ( IsSchema ('TContentTable ts) VContent , KnownNat n, KnownSymbol s
    ) => IsSchema ('TContentTable ('(n,s) ': ts)) VContent where
    schema = case schema @('TContentTable ts) of
        VContentTable xs ->
            let x = (nv (Proxy @n), sv (Proxy @s))
            in VContentTable (x : xs)
        _ -> err

instance
    IsSchema st StringType => IsSchema ('TContentString st) VContent where
    schema = VContentString (schema @st)

instance
    ( IsSchema sig Signedness
    , IsSchema cons [(Constrain, VNumber)]
    ) => IsSchema ('TContentInteger sig cons) VContent where
    schema = VContentInteger
        (schema @sig)
        [(a, evalNumInteger b) | (a,b) <- schema @cons]

instance
    ( IsSchema sig Signedness
    , IsSchema cons [(Constrain, VNumber)]
    , IsSchema lsb VNumber
    , KnownSymbol unit
    ) => IsSchema ('TContentQuantity sig lsb unit cons) VContent where
    schema = VContentQuantity
        (schema @sig)
        (evalNum (schema @lsb))
        (sv (Proxy @unit))
        [(a, evalNumDouble b) | (a,b) <- schema @cons]

instance
    ( IsSchema bt VBdsType
    ) => IsSchema ('TContentBds bt) VContent where
    schema = VContentBds (schema @bt)

data VRule
    = VContextFree VContent
    | VDependent [Text] [(Int, VContent)]
    deriving (Eq, Show)

instance IsSchema t VContent => IsSchema ('TContextFree t) VRule where
    schema = VContextFree (schema @t)

instance
    ( IsSchema lst [(Int, VContent)]
    , IsSchema name [Text]
    ) => IsSchema ('TDependent name lst) VRule where
    schema = VDependent (schema @name) (schema @lst)

instance
    ( KnownNat o
    , KnownNat n
    , IsSchema rule VRule
    , a ~ o
    , b ~ EndOffset o n
    ) => IsSchema ('TElement o n rule) (VVariation a b) where
    schema = VElement
        (bitOffsetMod $ nv (Proxy @o))
        (nv (Proxy @n))
        (schema @rule)

instance IsSchema ('TGroup '[]) (VVariation a b) where
    schema = VGroup []

instance
    ( IsSchema t VItem
    , IsSchema ('TGroup ts) VVariation
    ) => IsSchema ('TGroup (t ': ts)) VVariation where
    schema = case schema @('TGroup ts) of
        VGroup xs ->
            let x = schema @t
             in VGroup (x : xs)
        _ -> err

instance
    ( IsSchema ts [Maybe VItem]
    ) => IsSchema ('TExtended ts) VVariation
  where
    schema = VExtended (schema @ts)

instance
    ( MNat mn
    , IsSchema var VVariation
    ) => IsSchema ('TRepetitive mn var) VVariation where
    schema = VRepetitive (mInt @mn) (schema @var)

instance IsSchema ('TExplicit 'Nothing) VVariation where
    schema = VExplicit Nothing

instance IsSchema ('TExplicit ('Just 'ReservedExpansion)) VVariation where
    schema = VExplicit (Just ReservedExpansion)

instance IsSchema ('TExplicit ('Just 'SpecialPurpose)) VVariation where
    schema = VExplicit (Just SpecialPurpose)

instance MNat mn => IsSchema ('TCompound mn '[]) VVariation where
    schema = VCompound (mInt @mn) []

instance
    ( IsSchema ('TCompound mn ts) VVariation
    ) => IsSchema ('TCompound mn ('CompoundSpare ': ts)) VVariation where
    schema = case schema @('TCompound mn ts) of
        VCompound mn xs -> VCompound mn (CompoundSpare : xs)
        _               -> err

instance
    ( IsSchema ('TCompound mn ts) VVariation
    ) => IsSchema ('TCompound mn ('CompoundRFS ': ts)) VVariation where
    schema = case schema @('TCompound mn ts) of
        VCompound mn xs -> VCompound mn (CompoundRFS : xs)
        _               -> err

instance
    ( IsSchema t VItem
    , IsSchema ('TCompound mn ts) VVariation
    ) => IsSchema ('TCompound mn ('CompoundSubitem t ': ts)) VVariation where
    schema = case schema @('TCompound mn ts) of
        VCompound mn xs ->
            let x = schema @t
             in VCompound mn (CompoundSubitem x : xs)
        _ -> err

instance
    ( KnownNat o
    , KnownNat n
    ) => IsSchema ('TSpare o n) VItem where
    schema = VSpare (nv (Proxy @o)) (nv (Proxy @n))

instance
    ( KnownSymbol name
    , KnownSymbol title
    , IsSchema var VVariation
    ) => IsSchema ('TItem name title var) VItem where
    schema = VItem (sv (Proxy @name)) (sv (Proxy @title)) (schema @var)

type family IsSingleUap uap where
    IsSingleUap (TUapSingle _) = 'True
    IsSingleUap (TUapMultiple _) = 'False

data VUap
    = VUapSingle VVariation
    | VUapMultiple [(Text, VVariation)]
    deriving (Eq, Show)

instance IsSchema var VVariation => IsSchema ('TUapSingle var) VUap where
    schema = VUapSingle (schema @var)

instance IsSchema ('TUapMultiple '[]) VUap where
    schema = VUapMultiple []

instance
    ( KnownSymbol s
    , IsSchema var VVariation
    , IsSchema ('TUapMultiple ts) VUap
    ) => IsSchema ('TUapMultiple ( '(s, var) ': ts)) VUap where
    schema = case schema @('TUapMultiple ts) of
        VUapMultiple xs ->
            let x = schema @var
            in VUapMultiple ((sv (Proxy @s), x) : xs)
        _ -> err


instance (KnownNat n, IsSchema ed VEdition, IsSchema spec VSpec)
    => IsSchema ('TAsterix n ed spec) VAsterix where
    schema = VAsterix (nv (Proxy @n)) (schema @ed) (schema @spec)
-}
