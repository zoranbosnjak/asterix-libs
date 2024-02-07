-- | Asterix specifications schema in T-type and V-value level version.

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module Asterix.Schema where

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

class IsSchema t a | t -> a where
    schema :: a

data StringType
    = StringAscii
    | StringICAO
    | StringOctal
    deriving (Eq, Show)

instance IsSchema 'StringAscii StringType where schema = StringAscii
instance IsSchema 'StringICAO StringType where schema = StringICAO
instance IsSchema 'StringOctal StringType where schema = StringOctal

data Signedness
    = Signed
    | Unsigned
    deriving (Eq, Show)

instance IsSchema 'Signed Signedness where schema = Signed
instance IsSchema 'Unsigned Signedness where schema = Unsigned

data TPlusMinus
    = TPlus
    | TMinus

data TNumber
    = TNumInt TPlusMinus Nat
    | TNumDiv TNumber TNumber -- division
    | TNumPow Nat Nat -- power function

class IsNum t where evalNum :: Double
instance KnownNat n => IsNum ('TNumInt 'TPlus n) where evalNum = nv (Proxy @n)
instance KnownNat n => IsNum ('TNumInt 'TMinus n) where evalNum = nv (Proxy @n) * (-1)
instance (IsNum a, IsNum b) => IsNum ('TNumDiv a b) where
    evalNum = evalNum @a / evalNum @b
instance (KnownNat a, KnownNat b) => IsNum ('TNumPow a b) where
    evalNum = (nv (Proxy @a) :: Double) ^ (nv (Proxy @b) :: Integer)

data TContent
    = TContentRaw
    | TContentTable
        [(Nat, Symbol)] -- table rows
    | TContentString
        StringType
    | TContentInteger
        Signedness
    | TContentQuantity
        Signedness
        TNumber -- lsb
        Symbol -- unit

data VContent
    = VContentRaw
    | VContentTable [(Int, Text)]
    | VContentString StringType
    | VContentInteger Signedness
    | VContentQuantity Signedness Double Text
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
    IsSchema sig Signedness => IsSchema ('TContentInteger sig) VContent where
    schema = VContentInteger (schema @sig)

instance
    ( IsSchema sig Signedness
    , IsNum lsb
    , KnownSymbol unit
    ) => IsSchema ('TContentQuantity sig lsb unit) VContent where
    schema = VContentQuantity (schema @sig) (evalNum @lsb) (sv (Proxy @unit))

data TVariation
    = TElement
        Nat -- bit length
        TContent
    | TGroup
        [TItem]
    | TExtended
        [Maybe TItem] -- Nothing = FX bit
    | TRepetitive
        (Maybe Nat) -- header length (Nothing for FX type)
        TVariation
    | TExplicit
    | TCompound
        (Maybe Nat) -- fixed fspec length or fx based
        [Maybe TItem] -- Nothing = empty slot

data TItem
    = TSpare
        Nat -- bit length
    | TItem
        Symbol -- name
        Symbol -- title
        TVariation

data VVariation
    = VElement
        Int -- bit offset modulo
        Int -- bit length
        VContent
    | VGroup [VItem]
    | VExtended [Maybe VItem]
    | VRepetitive (Maybe Int) VVariation
    | VExplicit
    | VCompound (Maybe Int) [Maybe VItem]
    deriving (Eq, Show)

data VItem
    = VSpare
        Int -- bit offset modulo
        Int -- bit length
    | VItem Text Text VVariation
    deriving (Eq, Show)

-- Item length in terms of added bit offset.
type family Ln t  where
    Ln ('TSpare n) = n
    Ln ('TItem name title ('TElement n cont)) = n
    Ln _ = (0::Nat) -- other types are byte aligned

instance (KnownNat o, KnownNat n, IsSchema cont VContent)
    => IsSchema '(o, 'TElement n cont) VVariation where
    schema = VElement (mod (nv (Proxy @o)) 8) (nv (Proxy @n)) (schema @cont)

instance IsSchema '(o, 'TGroup '[]) VVariation where
    schema = VGroup []

instance
    ( KnownNat o
    , KnownNat n, n ~ Ln t
    , IsSchema '(o,t) VItem
    , IsSchema '(o+n, 'TGroup ts) VVariation
    ) => IsSchema '(o, 'TGroup (t ': ts)) VVariation where
    schema = case schema @('(o+n, 'TGroup ts)) of
        VGroup xs ->
            let x = schema @('(o, t))
             in VGroup (x : xs)
        _ -> err

-- Helper instance for extended type ('(offset, '[Maybe TItem]) -> [Maybe VItem])
instance IsSchema '(o, '[]) [Maybe VItem] where schema = []
instance ( KnownNat o, IsSchema '(o+1, ts) [Maybe VItem])
    => IsSchema '(o, 'Nothing ': ts) [Maybe VItem] where
    schema = Nothing : schema @('(o+1,ts))
instance
    ( KnownNat o
    , KnownNat n, n ~ Ln t
    , IsSchema '(o,t) VItem
    , IsSchema '(o+n, ts) [Maybe VItem]
    ) => IsSchema '(o, 'Just t ': ts) [Maybe VItem] where
    schema = Just (schema @('(o, t))) : schema @('(o+n,ts))

instance ( IsSchema '(0, ts) [Maybe VItem])
    => IsSchema ('(o, 'TExtended ts)) VVariation
  where
    schema = VExtended (schema @('(0, ts)))

instance
    ( MNat mn
    , IsSchema '(0, var) VVariation)
    => IsSchema ('(o, 'TRepetitive mn var)) VVariation where
    schema = VRepetitive (mInt @mn) (schema @('(0, var)))

instance IsSchema ('(o, 'TExplicit)) VVariation where
    schema = VExplicit

instance MNat mn => IsSchema ('(o, 'TCompound mn '[])) VVariation where
    schema = VCompound (mInt @mn) []

instance (MNat mn, IsSchema ('(0, 'TCompound mn ts)) VVariation)
    => IsSchema ('(o, 'TCompound mn ('Nothing ': ts))) VVariation where
    schema = case schema @('(0, 'TCompound mn ts)) of
        VCompound mn xs -> VCompound mn (Nothing : xs)
        _               -> err

instance
    ( MNat mn
    , IsSchema '(0, t) VItem
    , IsSchema ('(0, 'TCompound mn ts)) VVariation
    ) => IsSchema ('(o, 'TCompound mn ('Just t ': ts))) VVariation where
    schema = case schema @('(0, 'TCompound mn ts)) of
        VCompound mn xs ->
            let x = schema @('(0, t))
             in VCompound mn (Just x : xs)
        _ -> err

instance (KnownNat o, KnownNat n) => IsSchema '(o, 'TSpare n) VItem where
    schema = VSpare (mod (nv (Proxy @o)) 8) (nv (Proxy @n))

instance
    ( KnownNat o, KnownSymbol name, KnownSymbol title
    , IsSchema '(o, var) VVariation
    ) => IsSchema '(o, 'TItem name title var) VItem where
    schema = VItem (sv (Proxy @name)) (sv (Proxy @title)) (schema @('(o, var)))

data TUap
    = TUapSingle
        TVariation
    | TUapMultiple
        [(Symbol, TVariation)] -- [(uap name, ...)]

data VUap
    = VUapSingle VVariation
    | VUapMultiple [(Text, VVariation)]
    deriving (Eq, Show)

instance IsSchema '(0, var) VVariation => IsSchema ('TUapSingle var) VUap where
    schema = VUapSingle (schema @('(0, var)))

instance IsSchema ('TUapMultiple '[]) VUap where
    schema = VUapMultiple []

instance
    ( KnownSymbol s, IsSchema '(0, var) VVariation
    , IsSchema ('TUapMultiple ts) VUap
    ) => IsSchema ('TUapMultiple ( '(s, var) ': ts)) VUap where
    schema = case schema @('TUapMultiple ts) of
        VUapMultiple xs ->
            let x = schema @('(0, var))
            in VUapMultiple ((sv (Proxy @s), x) : xs)
        _ -> err

data TSpec
    = TCat TUap
    | TRef TVariation

data VSpec
    = VCat VUap
    | VRef VVariation
    deriving (Eq, Show)

instance IsSchema uap VUap => IsSchema ('TCat uap) VSpec where
    schema = VCat (schema @uap)

instance IsSchema ('(0, var)) VVariation => IsSchema ('TRef var) VSpec where
    schema = VRef (schema @('(0, var)))

data TEdition = TEdition Nat Nat

data VEdition = VEdition Int Int
    deriving (Eq, Show)

instance (KnownNat a, KnownNat b) => IsSchema ('TEdition a b) VEdition where
    schema = VEdition (nv (Proxy @a)) (nv (Proxy @b))

data TAsterix = TAsterix Nat TEdition TSpec

data VAsterix = VAsterix Int VEdition VSpec
    deriving (Eq, Show)

instance (KnownNat n, IsSchema ed VEdition, IsSchema spec VSpec)
    => IsSchema ('TAsterix n ed spec) VAsterix where
    schema = VAsterix (nv (Proxy @n)) (schema @ed) (schema @spec)
