-- | Asterix specifications schema in T-type and V-value level version.

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Asterix.Schema where

import           GHC.Generics (Generic)
import           Data.Proxy
import           Data.Text
import           GHC.TypeLits

err :: a
err = error "internal error"

-- | Nat value
nv :: (KnownNat n, Num a) => Proxy n -> a
nv = fromIntegral . natVal

-- | Symbol value
sv :: KnownSymbol s => Proxy s -> Text
sv = Data.Text.pack . symbolVal

-- | Helper class for 'repetitive' and 'compound'
class MNat t where mInt :: Maybe Int
instance MNat 'Nothing where mInt = Nothing
instance KnownNat n => MNat ('Just n) where mInt = Just $ nv (Proxy @n)

class IsSchema t a where
    schema :: a

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

instance KnownNat n => IsSchema n Int where
    schema = nv (Proxy @n)

instance KnownSymbol s => IsSchema s Text where
    schema = sv (Proxy @s)

data TPlusMinus
    = TPlus
    | TMinus

data TNumber
    = TNumInt TPlusMinus Nat
    | TNumDiv TNumber TNumber -- division
    | TNumPow Nat Nat -- power function

data VNumber
    = VNumInt Integer
    | VNumDiv VNumber VNumber
    | VNumPow Integer Integer
    deriving (Generic, Eq, Show)

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

data Constrain
    = EqualTo
    | NotEqualTo
    | GreaterThan
    | GreaterThanOrEqualTo
    | LessThan
    | LessThanOrEqualTo
    deriving (Generic, Eq, Show)

instance IsSchema 'EqualTo Constrain where schema = EqualTo
instance IsSchema 'NotEqualTo Constrain where schema = NotEqualTo
instance IsSchema 'GreaterThan Constrain where schema = GreaterThan
instance IsSchema 'GreaterThanOrEqualTo Constrain where schema = GreaterThanOrEqualTo
instance IsSchema 'LessThan Constrain where schema = LessThan
instance IsSchema 'LessThanOrEqualTo Constrain where schema = LessThanOrEqualTo

data Signedness
    = Signed
    | Unsigned
    deriving (Generic, Eq, Show)

instance IsSchema 'Signed Signedness where schema = Signed
instance IsSchema 'Unsigned Signedness where schema = Unsigned

data StringType
    = StringAscii
    | StringICAO
    | StringOctal
    deriving (Generic, Eq, Show)

instance IsSchema 'StringAscii StringType where schema = StringAscii
instance IsSchema 'StringICAO StringType where schema = StringICAO
instance IsSchema 'StringOctal StringType where schema = StringOctal

data TBdsType
    = TBdsWithAddress
    | TBdsAt (Maybe Nat)

data VBdsType
    = VBdsWithAddress
    | VBdsAt (Maybe Int)
    deriving (Generic, Eq, Show)

instance IsSchema 'TBdsWithAddress VBdsType where schema = VBdsWithAddress
instance IsSchema ('TBdsAt 'Nothing) VBdsType where schema = VBdsAt Nothing
instance KnownNat n => IsSchema ('TBdsAt ('Just n)) VBdsType where
    schema = VBdsAt $ Just $ nv (Proxy @n)

data TContent
    = TContentRaw
    | TContentTable
        [(Nat, Symbol)] -- table rows
    | TContentString
        StringType
    | TContentInteger
        Signedness
        [(Constrain, TNumber)]
    | TContentQuantity
        Signedness
        TNumber -- lsb
        Symbol -- unit
        [(Constrain, TNumber)]
    | TContentBds
        TBdsType

data VContent
    = VContentRaw
    | VContentTable [(Int, Text)]
    | VContentString StringType
    | VContentInteger Signedness [(Constrain, Integer)]
    | VContentQuantity Signedness Double Text [(Constrain, Double)]
    | VContentBds VBdsType
    deriving (Generic, Eq, Show)

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

data TRule
    = TContextFree TContent
    | TDependent [Symbol] [(Nat, TContent)]

data VRule
    = VContextFree VContent
    | VDependent [Text] [(Int, VContent)]
    deriving (Generic, Eq, Show)

instance IsSchema t VContent => IsSchema ('TContextFree t) VRule where
    schema = VContextFree (schema @t)

instance
    ( IsSchema lst [(Int, VContent)]
    , IsSchema name [Text]
    ) => IsSchema ('TDependent name lst) VRule where
    schema = VDependent (schema @name) (schema @lst)

data ExplicitType
    = ReservedExpansion
    | SpecialPurpose
    deriving (Generic, Eq, Show)

data CompoundSubitem a
    = CompoundSubitem a
    | CompoundSpare
    | CompoundRFS
    deriving (Generic, Eq, Show)

data TVariation
    = TElement
        Nat -- bit offset
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
        Nat -- bit offset
        Nat -- bit length
    | TItem
        Symbol -- name
        Symbol -- title
        TVariation

data VVariation
    = VElement
        Int -- bit offset modulo
        Int -- bit length
        VRule
    | VGroup [VItem]
    | VExtended [Maybe VItem]
    | VRepetitive (Maybe Int) VVariation
    | VExplicit (Maybe ExplicitType)
    | VCompound (Maybe Int) [CompoundSubitem VItem]
    deriving (Generic, Eq, Show)

data VItem
    = VSpare
        Int -- bit offset modulo
        Int -- bit length
    | VItem Text Text VVariation
    deriving (Generic, Eq, Show)

instance
    ( KnownNat o
    , KnownNat n
    , IsSchema rule VRule
    ) => IsSchema ('TElement o n rule) VVariation where
    schema = VElement (nv (Proxy @o)) (nv (Proxy @n)) (schema @rule)

instance IsSchema ('TGroup '[]) VVariation where
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

data TUap
    = TUapSingle
        TVariation
    | TUapMultiple
        [(Symbol, TVariation)] -- [(uap name, ...)]

type family IsSingleUap uap where
    IsSingleUap (TUapSingle _) = 'True
    IsSingleUap (TUapMultiple _) = 'False

data VUap
    = VUapSingle VVariation
    | VUapMultiple [(Text, VVariation)]
    deriving (Generic, Eq, Show)

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

data TSpec
    = TCat TUap
    | TRef TVariation

data VSpec
    = VCat VUap
    | VRef VVariation
    deriving (Generic, Eq, Show)

instance IsSchema uap VUap => IsSchema ('TCat uap) VSpec where
    schema = VCat (schema @uap)

instance IsSchema var VVariation => IsSchema ('TRef var) VSpec where
    schema = VRef (schema @var)

data TEdition = TEdition Nat Nat

data VEdition = VEdition Int Int
    deriving (Generic, Eq, Show)

instance (KnownNat a, KnownNat b) => IsSchema ('TEdition a b) VEdition where
    schema = VEdition (nv (Proxy @a)) (nv (Proxy @b))

data TAsterix = TAsterix Nat TEdition TSpec

data VAsterix = VAsterix Int VEdition VSpec
    deriving (Generic, Eq, Show)

instance (KnownNat n, IsSchema ed VEdition, IsSchema spec VSpec)
    => IsSchema ('TAsterix n ed spec) VAsterix where
    schema = VAsterix (nv (Proxy @n)) (schema @ed) (schema @spec)
