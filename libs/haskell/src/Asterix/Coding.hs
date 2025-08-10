-- |
-- Module: Asterix.Coding
--
-- Asterix data structures and functions to decode, encode and
-- manipulate asterix data.
--
-- This module supports complete set of decoding, encoding and filtering
-- operations, but it might result (depending on a scenario) in worse
-- performance, comparing to optimized versions (one direction only)
--  - Asterix.Decoding
--  - Asterix.Encoding

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Asterix.Coding
( module Asterix.Coding
, module Asterix.Schema
, module Asterix.BitString
, module Asterix.Base
) where

import           Control.Applicative
import           Control.Monad
import           Data.Proxy
import qualified Data.Text as T
import           Data.Bool
import           Data.ByteString (ByteString)
import           Data.Char (isAsciiUpper, isDigit, chr, ord)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.Type.Bool
import           Data.Type.Equality
import           GHC.TypeLits
import           Data.Kind (Constraint, Type)

import           Asterix.Base
import           Asterix.Schema
import           Asterix.BitString

type Index = Int

data UVariation
    = UElement Bits
    | UGroup [UItem]
    | UExtended SBuilder [Maybe UItem]
    | URepetitive SBuilder [UVariation]
    | UExplicit SBuilder Bits
    | UCompound SBuilder [Maybe UNonSpare]
    deriving Show

-- In case of ContextFree, the index is always Nothing,
-- in case of dependend, we remember the variation itself
-- and also which one it is (or Nothing if default)
data URuleVar = URuleVar
    { uRuleVarIx :: Maybe Index
    , uRuleVar :: UVariation
    } deriving Show

newtype UNonSpare = UNonSpare
    { unUNonSpare :: URuleVar
    } deriving Show

data UItem
    = USpare Bits
    | UItem UNonSpare
    deriving Show

data URecord = URecord
    { uRecUnparse   :: SBuilder
    , uRecItems     :: [Maybe (RecordItem UNonSpare)]
    } deriving Show

data UExpansion = UExpansion
    { uExpUnparse :: SBuilder
    , uExpItems   :: [Maybe UNonSpare]
    } deriving Show

-- | In case of multiple uaps, we keep UAP name in addition to the record.
data UDatablock = UDatablock
    { uDatablockUnparse :: SBuilder
    , uDatablockRecords :: [(Maybe VText, URecord)]
    } deriving Show

-- Typed wrappers around U-version of data structures

newtype Variation (t :: TVariation) = Variation
    { unVariation :: UVariation
    } deriving Show

newtype RuleVar (t1 :: TRule t2) = RuleVar { unRuleVar :: URuleVar }
    deriving Show

newtype NonSpare (t :: TNonSpare) = NonSpare { unNonSpare :: UNonSpare }
    deriving Show

newtype Item (t :: TItem) = Item { unItem :: UItem }
    deriving Show

newtype Record (t :: TRecord) = Record { unRecord :: URecord }
    deriving Show

newtype Expansion (t :: TExpansion) = Expansion
    { unExpansion :: UExpansion
    } deriving Show

newtype Datablock (db :: TDatablock)
    = Datablock
        { unDatablock :: UDatablock
        } deriving Show

-- Num instances

instance
    ( KnownNat o
    , IsNat8 o
    , KnownNat n
    ) => Num (Variation ('GElement o n rc)) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = Variation . UElement
        . integerToBits (natVal8 (Proxy @o)) (fromInteger $ natVal (Proxy @n))
    negate = fromInteger . negate . bitsToNum . unparse

instance Num (Variation ('GGroup o '[])) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger _ = Variation $ UGroup []
    negate = undefined

instance
    ( Num (Item t)
    , Num (Variation ('GGroup o ts))
    , KnownNat (BitSizeOf ts)
    ) => Num (Variation ('GGroup o (t ': ts))) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger i =
        let n = natVal (Proxy @(BitSizeOf ts))
            (a, b) = divMod i (2 ^ n)
            x = unItem $ fromInteger @(Item t) a
            xs = case fromInteger @(Variation ('GGroup o ts)) b of
                Variation (UGroup lst) -> lst
                _                      -> intError
        in Variation $ UGroup (x : xs)
    negate = undefined

instance
    ( Num (Variation t)
    ) => Num (RuleVar ('GContextFree t)) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = RuleVar . URuleVar Nothing . unVariation . fromInteger @(Variation t)
    negate = RuleVar . URuleVar Nothing . unVariation . negate @(Variation t) . Variation
        . uRuleVar . unRuleVar

instance
    ( Num (Variation t)
    ) => Num (RuleVar ('GDependent ts1 t ts2)) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = RuleVar . URuleVar Nothing . unVariation . fromInteger @(Variation t)
    negate = undefined

-- For dependent variation, can be explicit about
-- variation we are targeting.
newtype Indexed (a :: Maybe Nat) b = Indexed { unWithIndex :: b }
    deriving Show

type family FindDep i lst where
    FindDep n '[] = TypeError ('Text "Dep item index out of range")
    FindDep 0 ( '(val, t) ': ts) = t
    FindDep n (t ': ts) = FindDep (n - 1) ts

-- Default variation for Dependent RuleVar
instance
    ( Num (Variation t)
    ) => Num (Indexed 'Nothing (RuleVar ('GDependent ts1 t ts2))) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = Indexed
        . RuleVar . URuleVar Nothing . unVariation . fromInteger @(Variation t)
    negate = undefined

-- Specific variation for Dependent RuleVar
instance
    ( KnownNat i
    , t ~ FindDep i ts2
    , Num (Variation t)
    ) => Num (Indexed ('Just i) (RuleVar ('GDependent ts1 t0 ts2))) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger
        = Indexed
        . RuleVar . URuleVar (Just $ fromInteger $ natVal (Proxy @i))
        . unVariation . fromInteger @(Variation t)
    negate = undefined

instance
    ( Num (RuleVar rv)
    ) => Num (NonSpare ('GNonSpare name title rv)) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = NonSpare . UNonSpare . unRuleVar . fromInteger @(RuleVar rv)
    negate = NonSpare . UNonSpare . unRuleVar . negate @(RuleVar rv) . RuleVar
        . unUNonSpare . unNonSpare

instance
    ( KnownNat o
    , IsNat8 o
    , KnownNat n
    ) => Num (Item ('GSpare o n)) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = Item . USpare
        . integerToBits (natVal8 (Proxy @o)) (fromInteger $ natVal (Proxy @n))
    negate = undefined

instance
    ( Num (NonSpare nsp)
    ) => Num (Item ('GItem nsp)) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = Item . UItem . unNonSpare . fromInteger @(NonSpare nsp)
    negate = undefined

parsingStore :: ParsingStore UVariation URuleVar UNonSpare UItem URecord UExpansion
parsingStore = ParsingStore
    -- variations
    { psElement    = UElement
    , psGroup      = \_bits lst -> UGroup lst
    , psExtended   = UExtended . bitsToSBuilder
    , psRepetitive = URepetitive . bitsToSBuilder
    , psExplicit   = UExplicit . bitsToSBuilder
    , psCompound   = \rawFspec rawItems items -> UCompound
        (bitsToSBuilder rawFspec <> bitsToSBuilder rawItems) items

    -- rule variation
    , psRuleVar = URuleVar Nothing

    -- non-spare
    , psNsp = UNonSpare

    -- items
    , psSpare = USpare
    , psItem  = UItem

    -- record
    , psRecord = \rawFspec rawItems items -> URecord
        (bitsToSBuilder rawFspec <> bitsToSBuilder rawItems) items

    -- expansion
    , psExpansion = \rawFspec rawItems items -> UExpansion
        (bitsToSBuilder rawFspec <> bitsToSBuilder rawItems) items
    }

parse :: ParsingMode
    -> ParsingM UVariation URuleVar UNonSpare UItem URecord UExpansion r
    -> ByteString
    -> Either ParsingError (r, Offset)
parse pm act bs = runParsing act (Env parsingStore pm bs) 0

-- Unparsing to 'Bits' instances

instance Unparsing Bits UVariation where
    unparse = \case
        UElement bits -> bits
        UGroup lst -> concatBits (unparse <$> lst)
        UExtended b _ -> byteStringToBits $ builderToByteStringSlow $ sbData b
        URepetitive b _ -> byteStringToBits $ builderToByteStringSlow $ sbData b
        UExplicit b _ -> byteStringToBits $ builderToByteStringSlow $ sbData b
        UCompound b _ -> byteStringToBits $ builderToByteStringSlow $ sbData b

instance Unparsing Bits URuleVar where
    unparse = unparse . uRuleVar

instance Unparsing Bits UNonSpare where
    unparse = unparse . unUNonSpare

instance Unparsing Bits UItem where
    unparse = \case
        USpare b -> b
        UItem nsp -> unparse nsp

instance Unparsing Bits URecord where
    unparse
        = byteStringToBits
        . builderToByteStringSlow
        . sbData
        . uRecUnparse

instance Unparsing Bits UExpansion where
    unparse
        = byteStringToBits
        . builderToByteStringSlow
        . sbData
        . uExpUnparse

instance Unparsing Bits UDatablock where
    unparse
        = byteStringToBits
        . builderToByteStringSlow
        . sbData
        . uDatablockUnparse

instance Unparsing Bits (Variation t) where
    unparse = unparse . unVariation

instance Unparsing Bits (RuleVar t) where
    unparse = unparse . unRuleVar

instance Unparsing Bits (NonSpare t) where
    unparse = unparse . unNonSpare

instance Unparsing Bits (Item t) where
    unparse = unparse . unItem

instance Unparsing Bits (Record t) where
    unparse = unparse . unRecord

instance Unparsing Bits (Expansion t) where
    unparse = unparse . unExpansion

instance Unparsing Bits (Datablock db) where
    unparse = unparse . unDatablock

-- Unparsing to 'SBuilder' instances

instance Unparsing SBuilder (RecordItem UNonSpare) where
    unparse = \case
        RecordItem nsp -> bitsToSBuilder $ unparse nsp
        RecordItemSpare -> mempty
        RecordItemRFS lst -> mconcat $ do
            (_i, nsp) <- lst
            pure (bitsToSBuilder $ unparse nsp)

instance Unparsing SBuilder URecord where
    unparse = uRecUnparse

instance Unparsing SBuilder UExpansion where
    unparse = uExpUnparse

instance Unparsing SBuilder UDatablock where
    unparse = uDatablockUnparse

instance Unparsing SBuilder (Record t) where
    unparse = unparse . unRecord

instance Unparsing SBuilder (Expansion t) where
    unparse = unparse . unExpansion

instance Unparsing SBuilder (Datablock db) where
    unparse = unparse . unDatablock

-- Constructing

bitsPerChar :: VStringType -> Int
bitsPerChar = \case
    GStringAscii -> 8
    GStringICAO -> 6
    GStringOctal -> 3

class MkString t where
    string :: String -> t

instance
    ( KnownNat o
    , IsNat8 o
    , KnownNat n
    , IsSchema st GStringType
    ) => MkString (Variation ('GElement o n ('GContextFree ('GContentString st))))
  where
    string val = Variation $ UElement $ integerToBits
        (fromIntegral $ natVal @o Proxy)
        (fromIntegral $ natVal @n Proxy)
        (foldInt 0 $ unChar <$> padAndStrip val)
      where

        padChar :: Char
        padChar = case schema @st Proxy of
            GStringAscii -> ' '
            GStringICAO -> ' '
            GStringOctal -> '0'

        bpc :: Int
        bpc = bitsPerChar (schema @st Proxy)

        foldInt :: Integer -> [Integer] -> Integer
        foldInt acc = \case
            [] -> acc
            (x : xs) -> foldInt (acc*(2^bpc) + x) xs

        unChar :: Char -> Integer
        unChar ch = fromIntegral $ case schema @st Proxy of
            GStringAscii -> ord ch
            GStringICAO -> if
                | isAsciiUpper ch -> 0x01 + ord ch - ord 'A'
                | ch == ' ' -> 0x20
                | isDigit ch -> 0x30 + ord ch - ord '0'
                | otherwise -> 0
            GStringOctal -> ord ch - ord '0'

        padAndStrip :: String -> String
        padAndStrip s = take n (s <> repeat padChar) where
            n = fromIntegral (natVal @n Proxy) `div` bpc

instance
    ( MkString (Variation var)
    ) => MkString (RuleVar ('GContextFree var)) where
    string = RuleVar . URuleVar Nothing . unVariation . string @(Variation var)

instance
    ( MkString (RuleVar rv)
    ) => MkString (NonSpare ('GNonSpare name title rv)) where
    string = NonSpare . UNonSpare . unRuleVar . string @(RuleVar rv)

instance
    ( MkString (NonSpare nsp)
    ) => MkString (Item ('GItem nsp)) where
    string = Item . UItem . unNonSpare . string @(NonSpare nsp)

newtype Quantity (unit :: Symbol) (ruleIx :: (Maybe Nat)) = Quantity
    { unQuantity :: Double
    } deriving (Eq, Ord, Show, Num, Fractional)

class MkQuantity s r t where
    quantity :: Quantity s r -> t

evalGz :: VZ -> Integer
evalGz (GZ pm i) = fromIntegral $ i * case pm of
    GPlus -> 1
    GMinus -> -1

evalNum :: Fractional a => VNumber -> a
evalNum = \case
    GNumInt gz -> fromIntegral $ evalGz gz
    GNumDiv a b -> evalNum a / evalNum b
    GNumPow a b -> fromIntegral (evalGz a ^ evalGz b)

processQuantity :: Int -> Int -> Double -> Quantity unit ruleIx -> Variation t
processQuantity o n lsb = Variation . UElement . integerToBits o n . round
    . (/ lsb) . unQuantity

instance
    ( KnownNat o
    , IsNat8 o
    , KnownNat n
    , IsSchema lsb VNumber
    , unit1 ~ unit2
    , ruleIx ~ 'Nothing
    ) => MkQuantity unit1 ruleIx (Variation
        ('GElement o n ('GContextFree ('GContentQuantity sig lsb unit2))))
  where
    quantity = processQuantity
            (fromIntegral $ natVal @o Proxy)
            (fromIntegral $ natVal @n Proxy)
            (evalNum $ schema @lsb Proxy)

instance {-# OVERLAPPING #-}
    ( KnownNat o
    , IsNat8 o
    , KnownNat n
    , IsSchema lsb VNumber
    , unit1 ~ unit2
    ) => MkQuantity unit1 ('Just 0) (Variation
        ('GElement o n ('GDependent ts1 dt
            ( '(x, 'GContentQuantity sig lsb unit2) ': ts2))))
  where
    quantity = processQuantity
            (fromIntegral $ natVal @o Proxy)
            (fromIntegral $ natVal @n Proxy)
            (evalNum $ schema @lsb Proxy)

instance
    ( MkQuantity unit (Just (ix - 1))
        (Variation (GElement o n (GDependent ts1 dt ts2)))
    ) => MkQuantity unit ('Just ix) (Variation
        ('GElement o n ('GDependent ts1 dt
            ( '(x, 'GContentQuantity sig lsb unit2) ': ts2))))
  where
    quantity (Quantity val) = case
        quantity @unit @('Just (ix-1))
            @(Variation ('GElement o n ('GDependent ts1 dt ts2))) (Quantity val) of
            Variation x -> Variation x

instance
    ( MkQuantity unit ix (Variation var)
    ) => MkQuantity unit ix (RuleVar ('GContextFree var))
  where
    quantity = RuleVar . URuleVar Nothing . unVariation
        . quantity @unit @ix @(Variation var)

instance
    ( MkQuantity unit ix (RuleVar rv)
    ) => MkQuantity unit ix (NonSpare ('GNonSpare name title rv))
  where
    quantity = NonSpare . UNonSpare . unRuleVar
        . quantity @unit @ix @(RuleVar rv)

instance
    ( MkQuantity unit ix (NonSpare nsp)
    ) => MkQuantity unit ix (Item ('GItem nsp)) where
    quantity = Item . UItem . unNonSpare
        . quantity @unit @ix @(NonSpare nsp)

-- Hlist

data HList t where
    HNil :: HList '[]
    HCons :: t -> HList ts -> HList (t ': ts)

nil :: HList '[]
nil = HNil

infixr 5 *:
(*:) :: x -> HList xs -> HList (x ': xs)
(*:) = HCons

-- Fold HList by converting each element to a Monoid
-- and combining the results.
-- A 'c' represents a constraint on each element of HList.
-- A caller of 'foldHList' should provide 'c' as type application,
-- for example: foldHList @Show (\x -> [show x]) someHList,
-- would convert HList to [String], given that each element has
-- Show instance.
class FoldHList c t where
    foldHList :: Monoid r => (forall a. c a => a -> r) -> HList t -> r

instance FoldHList c '[] where
    foldHList _f HNil = mempty

instance
    ( c t
    , FoldHList c ts
    ) => FoldHList c (t ': ts) where
    foldHList f (HCons x xs) = f x <> foldHList @c f xs

newtype Named (name :: Symbol) a = Named
    { unNamed :: a
    } deriving (Eq, Show)

deriving instance Num a => Num (Named name a)

item :: forall name a. a -> Named name a
item = Named

newtype AbuseSpare a = AbuseSpare a

deriving instance Num a => Num (AbuseSpare a)

abuseSpare :: a -> AbuseSpare a
abuseSpare = AbuseSpare

spare :: Num t => AbuseSpare t
spare = abuseSpare 0

data Fx = Fx deriving (Eq, Show)

fx :: Fx
fx = Fx

-- Group construction

class CGroup ts2 ts1 where
    groupConstruct :: Proxy ts2 -> HList ts1 -> [UItem]

instance
    TypeError ('Text "Missing spare item")
    => CGroup ('GSpare o n ': ts) '[] where
    groupConstruct = intError

instance
    TypeError ('Text "Missing item: " :<>: 'ShowType name)
    => CGroup ('GItem ('GNonSpare name title rv) ': ts) '[] where
    groupConstruct = intError

instance
    TypeError ('Text "Unexpected item: " :<>: 'ShowType t)
    => CGroup '[] (t ': ts) where
    groupConstruct = intError

instance CGroup '[] '[] where
    groupConstruct _ _ = []

instance
    ( t1 ~ AbuseSpare a
    , a ~ Item ('GSpare o n)
    , CGroup ts2 ts1
    ) => CGroup ('GSpare o n ': ts2) (t1 ': ts1) where
    groupConstruct _proxy (HCons (AbuseSpare x) xs) =
        unItem x : groupConstruct (Proxy @ts2) xs

instance
    ( t1 ~ Named name1 a
    , a ~ Item ('GItem ('GNonSpare name2 title rv))
    , name1 ~ name2
    , CGroup ts2 ts1
    ) => CGroup ('GItem ('GNonSpare name2 title rv) ': ts2) (t1 ': ts1) where
    groupConstruct _proxy (HCons (Named x) xs)
        = unItem x : groupConstruct (Proxy @ts2) xs

-- | 'group' function is overloaded for different output types
class MkGroup t ts1 where
    group :: HList ts1 -> t

instance
    ( CGroup ts2 ts1
    ) => MkGroup (Variation ('GGroup o ts2)) ts1 where
    group lst1 = Variation $ UGroup lst2 where
        lst2 :: [UItem]
        lst2 = groupConstruct (Proxy @ts2) lst1

instance
    ( MkGroup (Variation var) ts1
    ) => MkGroup (NonSpare ('GNonSpare name title ('GContextFree var))) ts1
  where
    group = NonSpare . UNonSpare . URuleVar Nothing . unVariation
        . group @(Variation var)

instance
    ( MkGroup (NonSpare ('GNonSpare name title rv)) ts1
    ) => MkGroup (Item ('GItem ('GNonSpare name title rv))) ts1
  where
    group = Item . UItem . unNonSpare
        . group @(NonSpare ('GNonSpare name title rv))

instance
    ( name1 ~ name2
    , MkGroup (Item ('GItem ('GNonSpare name2 title (GContextFree var)))) ts1
    ) => MkGroup (Named name1 (Item ('GItem ('GNonSpare name2 title ('GContextFree var))))) ts1
  where
    group = Named
        . group @(Item ('GItem ('GNonSpare name2 title ('GContextFree var))))

-- Extended construction
--
-- Remark: It is possible to construct extended either by
--  1) extended (hList of individual items, fx...)
--  2) extendedGroups (hList of groups-of-items)

-- All 'Nothing' items represent FX which must be '1',
-- except for the last which must be '0' (if FX).
recreateExtended :: [Maybe UItem] -> Bits
recreateExtended = \case
    [] -> intError
    [Nothing] -> setFx False
    [Just i] -> unparse i
    (x:xs) -> case x of
        Nothing -> setFx True `appendBits` recreateExtended xs
        Just i -> unparse i `appendBits` recreateExtended xs
  where
    setFx = integerToBits 7 1 . bool 0 1

-- Extended construction by items

class CExtended ts2 done ts1 where
    extendedConstruct :: Proxy ts2 -> Proxy done -> HList ts1 -> [Maybe UItem]

instance
    TypeError ('Text "Missing FX item")
    => CExtended ('Nothing ': ts) 'False '[] where
    extendedConstruct = intError

instance
    TypeError ('Text "Missing spare item")
    => CExtended ('Just ('GSpare o n) ': ts) 'False '[] where
    extendedConstruct = intError

instance
    TypeError ('Text "Missing item: " :<>: 'ShowType name)
    => CExtended ('Just ('GItem ('GNonSpare name title rv)) ': ts) 'False '[] where
    extendedConstruct = intError

instance
    TypeError ('Text "Unexpected item: " :<>: 'ShowType t)
    => CExtended '[] done (t ': ts) where
    extendedConstruct = intError

instance CExtended ts2 'True '[] where
    extendedConstruct _p1 _p2 _hlst = []

instance CExtended '[] 'False '[] where
    extendedConstruct _p1 _p2 _hlst = []

instance
    ( t1 ~ Fx
    , CExtended ts2 'True ts1
    ) => CExtended ('Nothing ': ts2) done (t1 ': ts1) where
    extendedConstruct _p1 _p2 (HCons _fx xs)
        = Nothing : extendedConstruct (Proxy @ts2) (Proxy @'True) xs

instance
    ( t1 ~ AbuseSpare a
    , a ~ Item ('GSpare o n)
    , CExtended ts2 'False ts1
    ) => CExtended ('Just ('GSpare o n) ': ts2) done (t1 ': ts1) where
    extendedConstruct _p1 _p2 (HCons (AbuseSpare x) xs)
        = Just (unItem x) : extendedConstruct (Proxy @ts2) (Proxy @'False) xs

instance
    ( t1 ~ Named name1 a
    , a ~ Item ('GItem ('GNonSpare name2 title rv))
    , name1 ~ name2
    , CExtended ts2 'False ts1
    ) => CExtended ('Just ('GItem ('GNonSpare name2 title rv)) ': ts2) done (t1 ': ts1)
  where
    extendedConstruct _p1 _p2 (HCons (Named x) xs)
        = Just (unItem x) : extendedConstruct (Proxy @ts2) (Proxy @'False) xs

-- | 'extended' function is overloaded for different output types
class MkExtended t ts1 where
    extended :: HList ts1 -> t

instance
    ( CExtended ts2 'False ts1
    ) => MkExtended (Variation ('GExtended ts2)) ts1
  where
    extended lst1 = Variation $ UExtended bld lst2
      where
        lst2 :: [Maybe UItem]
        lst2 = extendedConstruct (Proxy @ts2) (Proxy @'False) lst1

        bld :: SBuilder
        bld = bitsToSBuilder $ recreateExtended lst2

instance
    ( MkExtended (Variation var) ts1
    ) => MkExtended (NonSpare ('GNonSpare name title ('GContextFree var))) ts1
  where
    extended = NonSpare . UNonSpare . URuleVar Nothing . unVariation
        . extended @(Variation var)

-- Extended construction by groups

class CExtendedGroups ts2 ts1 where
    extendedConstructGroups :: Proxy ts2 -> HList ts1 -> [Maybe UItem]

-- At least one group shall be provided
instance {-# OVERLAPPING #-}
    ( t1 ~ Variation ('GGroup 0 lst)
    , lst ~ FirstExtendedGroup ts2
    , fx ~ TrailingFx ts2
    , KnownBool fx
    ) => CExtendedGroups ts2 (t1 ': '[]) where
    extendedConstructGroups _p (HCons (Variation val) HNil) = case val of
        UGroup lst -> fmap Just lst <> trailingFx
        _ -> intError
      where
        appendFx = boolVal @fx Proxy
        trailingFx = bool [] [Nothing] appendFx

instance
    ( t1 ~ Variation ('GGroup 0 lst)
    , lst ~ FirstExtendedGroup ts2
    , ts3 ~ RemainingExtendedItems ts2
    , CExtendedGroups ts3 ts1
    ) => CExtendedGroups ts2 (t1 ': ts1) where
    extendedConstructGroups _p (HCons (Variation val) xs) = case val of
        UGroup lst ->
            fmap Just lst
          <> [Nothing]
          <> extendedConstructGroups @ts3 @ts1 Proxy xs
        _ -> intError

-- | 'extendedGroups' function is overloaded for different output types
class MkExtendedGroups t ts1 where
    extendedGroups :: HList ts1 -> t

instance
    ( CExtendedGroups ts2 ts1
    ) => MkExtendedGroups (Variation ('GExtended ts2)) ts1
  where
    extendedGroups lst1 = Variation $ UExtended bld lst2
      where
        lst2 :: [Maybe UItem]
        lst2 = extendedConstructGroups (Proxy @ts2) lst1

        bld :: SBuilder
        bld = bitsToSBuilder $ recreateExtended lst2

instance
    ( MkExtendedGroups (Variation var) ts1
    ) => MkExtendedGroups (NonSpare ('GNonSpare name title ('GContextFree var))) ts1
  where
    extendedGroups = NonSpare . UNonSpare . URuleVar Nothing . unVariation
        . extendedGroups @(Variation var)

-- Repetitive construction

-- | 'repetitive' function is overloaded for different output types
class MkRepetitive t2 t1 | t2 -> t1 where
    type RepetitiveInputList t2 :: Type -> Type
    repetitive :: RepetitiveInputList t2 t1 -> t2

instance
    ( KnownNat n
    ) => MkRepetitive
        (Variation ('GRepetitive ('GRepetitiveRegular n) var))
        (Variation var)
      where
    type RepetitiveInputList
        (Variation ('GRepetitive ('GRepetitiveRegular n) var)) = []
    repetitive lst1 = Variation $ URepetitive bld lst2
      where
        lst2 :: [UVariation]
        lst2 = fmap unVariation lst1

        bld :: SBuilder
        bld =
            let nBytes = fromIntegral $ natVal (Proxy @n)
                n = integerToBits 0 (nBytes*8) (fromIntegral $ Prelude.length lst2)
                items = fmap unparse lst1
            in bitsToSBuilder $ concatBits (n : items)

instance MkRepetitive
        (Variation ('GRepetitive 'GRepetitiveFx var))
        (Variation var)
      where
    type RepetitiveInputList
        (Variation ('GRepetitive 'GRepetitiveFx var)) = NE.NonEmpty
    repetitive lst1 = Variation $ URepetitive bld lst2
      where
        lst1' = NE.toList lst1

        lst2 :: [UVariation]
        lst2 = fmap unVariation lst1'

        addFx :: Bool -> Bits -> Bits
        addFx flag arg = appendBits arg (boolsToBits 7 [flag])

        bld :: SBuilder
        bld = bitsToSBuilder
            $ concatBits
            $ reverse
            $ zipWith addFx (False : repeat True)
            $ reverse
            $ fmap unparse lst1'

instance
    ( MkRepetitive (Variation var) ts1
    ) => MkRepetitive (NonSpare ('GNonSpare name title ('GContextFree var))) ts1
  where
    type RepetitiveInputList (NonSpare ('GNonSpare name title ('GContextFree var)))
            = RepetitiveInputList (Variation var)
    repetitive = NonSpare . UNonSpare . URuleVar Nothing . unVariation
        . repetitive @(Variation var)

-- | 'explicit' function is overloaded for different output types

class MkExplicit t2 t1 where
    explicit :: t1 -> t2

instance
    ( Unparsing SBuilder t1
    , Unparsing Bits t1
    ) => MkExplicit (Variation var) t1
  where
    explicit val = Variation $ UExplicit bld s where
        s :: Bits
        s = unparse val

        nBytes :: Int
        nBytes = numBytes $ bitsSize s

        n :: SBuilder
        n = bitsToSBuilder $ integerToBits 0 8 (succ $ fromIntegral nBytes)

        bld :: SBuilder
        bld = n <> unparse val

instance
    ( MkExplicit (Variation var) t1
    ) => MkExplicit (NonSpare ('GNonSpare name title ('GContextFree var))) t1
  where
    explicit = NonSpare . UNonSpare . URuleVar Nothing . unVariation
        . explicit @(Variation var)

mkFspecFx :: [Bool] -> SBuilder
mkFspecFx
    = bitsToSBuilder
    . boolsToBits 0
    . mconcat
    . reverse
    . zipWith addFx (False : repeat True)
    . keepAtLeastOne
    . dropWhile (== replicate 7 False)
    . reverse
    . splitToGroupsOf7
  where
    splitToGroupsOf7 :: [Bool] -> [[Bool]]
    splitToGroupsOf7 lst
        | Prelude.length lst <= 7 = [take 7 (lst <> repeat False)]
        | otherwise = take 7 lst : splitToGroupsOf7 (drop 7 lst)

    addFx :: Bool -> [Bool] -> [Bool]
    addFx fxBit lst = lst <> [fxBit]

    keepAtLeastOne :: [[Bool]] -> [[Bool]]
    keepAtLeastOne = \case
        [] -> [replicate 7 False]
        lst -> lst

mkFspecFixed :: Int -> [Bool] -> SBuilder
mkFspecFixed nBytes
    = bitsToSBuilder
    . boolsToBits 0
    . take (nBytes * 8)
    . (<> repeat False)

-- | Helper class for compound and record construction.
class CSetItem nsp ts1 where
    cSetItem :: Proxy nsp -> HList ts1 -> Maybe UNonSpare

instance CSetItem nsp '[] where
    cSetItem _p _hlist = Nothing

instance {-# OVERLAPPING #-}
    ( nsp ~ NonSpare ('GNonSpare name title rv)
    ) => CSetItem ('GNonSpare name title rv) (Named name nsp ': ts) where
    cSetItem _p (HCons (Named nsp) _ts) = Just $ unNonSpare nsp

instance
    ( CSetItem ('GNonSpare name1 title rv) ts
    ) => CSetItem ('GNonSpare name1 title rv) (Named name2 nsp ': ts) where
    cSetItem p (HCons _nsp ts) = cSetItem p ts

type family Elem name ts where
    Elem n1 '[] = 'False
    Elem n1 (Named n2 a ': ts) = If
        (n1 == n2)
        'True
        (Elem n1 ts)

type family NoDuplicates t where
    NoDuplicates '[] = () :: Constraint
    NoDuplicates (Named name a ': ts) = If
        (Elem name ts)
        (TypeError ('Text "Duplicated item: " :<>: 'ShowType name))
        (NoDuplicates ts)

-- Compound construction

type family IsDefinedCompound ts2 name where
    IsDefinedCompound '[] name = TypeError ('Text "Unknown item: " :<>: 'ShowType name)
    IsDefinedCompound ('Nothing ': ts) name2 = IsDefinedCompound ts name2
    IsDefinedCompound ('Just ('GNonSpare name1 title rv) ': ts) name2 = If
        (name1 == name2)
        (() :: Constraint)
        (IsDefinedCompound ts name2)

type family AllDefinedCompound ts2 ts1 where
    AllDefinedCompound ts2 '[] = () :: Constraint
    AllDefinedCompound ts2 (Named name rv ': ts)
        = (IsDefinedCompound ts2 name, AllDefinedCompound ts2 ts)

class CCompound ts2 ts1 where
    compoundConstruct :: Proxy ts2 -> HList ts1 -> [Maybe UNonSpare]

instance CCompound '[] ts1 where
    compoundConstruct _p1 _hlist = []

instance
    ( CCompound ts ts1
    ) => CCompound ('Nothing ': ts) ts1 where
    compoundConstruct _p1 hlist
        = Nothing
        : compoundConstruct (Proxy @ts) hlist

instance
    ( CCompound ts ts1
    , CSetItem nsp ts1
    ) => CCompound ('Just nsp ': ts) ts1 where
    compoundConstruct _p1 hlist
        = cSetItem (Proxy @nsp) hlist
        : compoundConstruct (Proxy @ts) hlist

-- | 'compound' function is overloaded for different output types
class MkCompound t ts1 where
    compound :: HList ts1 -> t

rebuildCompound :: Unparsing Bits a => [Maybe a] -> SBuilder
rebuildCompound items = mkFspecFx (fmap isJust items)
    <> mconcat (bitsToSBuilder . unparse <$> catMaybes items)

instance
    ( CCompound ts2 ts1
    , NoDuplicates ts1
    , AllDefinedCompound ts2 ts1
    ) => MkCompound (Variation ('GCompound ts2)) ts1 where
    compound lst1 = Variation $ UCompound bld items where
        items :: [Maybe UNonSpare]
        items = compoundConstruct (Proxy @ts2) lst1

        bld :: SBuilder
        bld = rebuildCompound items

instance
    ( MkCompound (Variation var) ts1
    ) => MkCompound (NonSpare ('GNonSpare name title ('GContextFree var))) ts1
  where
    compound = NonSpare . UNonSpare . URuleVar Nothing . unVariation
        . compound @(Variation var)

-- Record construction

type family IsDefinedRecord ts2 name where
    IsDefinedRecord '[] name = TypeError ('Text "Unknown item: " :<>: 'ShowType name)
    IsDefinedRecord ('GUapItem ('GNonSpare name1 title rv) ': ts) name2 = If
        (name1 == name2)
        (() :: Constraint)
        (IsDefinedRecord ts name2)
    IsDefinedRecord ('GUapItemSpare ': ts) name2 = IsDefinedRecord ts name2
    IsDefinedRecord ('GUapItemRFS ': ts) name2 = IsDefinedRecord ts name2

type family AllDefinedRecord ts2 ts1 where
    AllDefinedRecord ts2 '[] = () :: Constraint
    AllDefinedRecord ts2 (Named name rv ': ts)
        = (IsDefinedRecord ts2 name, AllDefinedRecord ts2 ts)

class CRecord ts2 ts1 where
    recordConstruct :: Proxy ts2 -> HList ts1 -> [Maybe (RecordItem UNonSpare)]

instance CRecord '[] ts1 where
    recordConstruct _p1 _hlist = []

instance
    ( CRecord ts ts1
    , CSetItem nsp ts1
    ) => CRecord ('GUapItem nsp ': ts) ts1 where
    recordConstruct _p1 hlist
        = fmap RecordItem (cSetItem (Proxy @nsp) hlist)
        : recordConstruct (Proxy @ts) hlist

instance
    ( CRecord ts ts1
    ) => CRecord ('GUapItemSpare ': ts) ts1 where
    recordConstruct _p1 hlist = Nothing : recordConstruct (Proxy @ts) hlist

instance
    ( CRecord ts ts1
    ) => CRecord ('GUapItemRFS ': ts) ts1 where
    recordConstruct _p1 hlist
        = Nothing -- TODO
        : recordConstruct (Proxy @ts) hlist

rebuildRecord :: Unparsing SBuilder a => [Maybe a] -> SBuilder
rebuildRecord items
    = mkFspecFx (fmap isJust items)
   <> mconcat (unparse <$> catMaybes items)

record :: forall ts2 ts1.
    ( CRecord ts2 ts1
    , NoDuplicates ts1
    , AllDefinedRecord ts2 ts1
    ) => HList ts1 -> Record ('GRecord ts2)
record lst1 = Record $ URecord bld items
  where
    items :: [Maybe (RecordItem UNonSpare)]
    items = recordConstruct (Proxy @ts2) lst1

    bld :: SBuilder
    bld = rebuildRecord items

-- Expansion construction

expansion :: forall mn ts2 ts1.
    ( CCompound ts2 ts1
    , NoDuplicates ts1
    , AllDefinedCompound ts2 ts1
    , IsSchema mn (Maybe Int)
    ) => HList ts1 -> Expansion ('GExpansion mn ts2)
expansion lst1 = Expansion $ UExpansion bld items
  where
    items :: [Maybe UNonSpare]
    items = compoundConstruct (Proxy @ts2) lst1

    fxbits :: SBuilder
    fxbits = case schema @mn Proxy of
        Nothing -> mkFspecFx (fmap isJust items)
        Just nBytes -> mkFspecFixed nBytes (fmap isJust items)

    bld :: SBuilder
    bld = fxbits
        <> mconcat (bitsToSBuilder . unparse <$> catMaybes items)

-- Datablock construction

-- TODO: check if possible to use 'IsSchema' instead of a new class
class MName t where
    uapName :: Proxy t -> Maybe T.Text

instance MName 'Nothing where
    uapName _ = Nothing

instance KnownSymbol name => MName ('Just name) where
    uapName _ = Just $ T.pack $ symbolVal (Proxy @name)

class CDatablock multi dbt ts where
    datablockConstruct :: Proxy multi -> Proxy dbt
        -> HList ts
        -> [(Maybe VText, URecord)]

instance CDatablock multi dbt '[] where
    datablockConstruct _p1 _p2 _hlist = []

type family UapName r lst where
    UapName r '[] = TypeError ('Text "Wrong record" :<>: ShowType r)
    UapName r1 ( '(name, r2) ': ts) = If
        (r1 == r2)
        name
        (UapName r1 ts)

type family MapFirstJust t where
    MapFirstJust '[] = '[]
    MapFirstJust ('(a, b) ': ts) = '( 'Just a, b) ': MapFirstJust ts

type family UapList t where
    UapList ('GUap val) = '[ '( 'Nothing, val)]
    UapList ('GUaps lst sel) = MapFirstJust lst

type family IsMultiUap (t :: GDatablock u) :: Bool where
    IsMultiUap ('GDatablock cat ('GUap rec)) = 'False
    IsMultiUap ('GDatablock cat ('GUaps lst sel)) = 'True

type family TypeOfRecord (t :: GDatablock u) :: GRecord u where
    TypeOfRecord ('GDatablock cat ('GUap rec)) = rec
    TypeOfRecord db = TypeError ('Text "Not defined")

-- For single UAP situation, the type of record is determined
instance
    ( CDatablock 'False dbt ts
    , dbt ~ 'GDatablock cat uap
    , MName (UapName t (UapList uap))
    , t ~ TypeOfRecord dbt
    ) => CDatablock 'False dbt (Record t ': ts) where
    datablockConstruct p1 p2 (HCons x xs)
        = (uapName (Proxy @(UapName t (UapList uap))), unRecord x)
        : datablockConstruct p1 p2 xs

-- For multi UAP situation, the type of record must be explicit
instance
    ( CDatablock 'True dbt ts
    , dbt ~ 'GDatablock cat uap
    , MName (UapName t (UapList uap))
    ) => CDatablock 'True dbt (Record t ': ts) where
    datablockConstruct p1 p2 (HCons x xs)
        = (uapName (Proxy @(UapName t (UapList uap))), unRecord x)
        : datablockConstruct p1 p2 xs

datablock :: forall dbt ts.
    ( CDatablock (IsMultiUap dbt) dbt ts
    , KnownNat (CategoryOf dbt)
    ) => HList ts -> Datablock dbt
datablock lst = Datablock $ UDatablock bld records where
    records :: [(Maybe VText, URecord)]
    records = datablockConstruct (Proxy @(IsMultiUap dbt)) (Proxy @dbt) lst

    bld :: SBuilder
    bld =
        let bldData = mconcat (unparse . snd <$> records)
            n = sbByteSize bldData + 3
            (n1, n2) = divMod n 256
            cat = natVal (Proxy @(CategoryOf dbt))
        in word8ToSBuilder (fromIntegral cat)
         <> word8ToSBuilder (fromIntegral n1)
         <> word8ToSBuilder (fromIntegral n2)
         <> bldData

asUint :: forall b a. (Unparsing Bits a, Integral b, Num b) => a -> b
asUint = bitsToNum . unparse

class ToString ruleIx t where
    asString :: t -> String

bitsToString :: VStringType -> Int -> Bits -> String
bitsToString st bitSize =
    let bpc :: Int
        bpc = bitsPerChar st

        toChar :: Int -> Char
        toChar x = case st of
            GStringAscii -> chr x
            GStringICAO -> if
                | x >= 0x01 && x <= 0x1A -> chr (ord 'A' + x - 0x01)
                | x == 0x20 -> ' '
                | x >= 0x30 && x <= 0x39 -> chr (ord '0' + x - 0x30)
                | otherwise -> '?'
            GStringOctal -> chr (ord '0' + x)

        p = 2 ^ bpc
        n = div bitSize bpc

        go :: Int -> Integer -> String
        go cnt x
            | cnt > 0 =
                let (y, i) = divMod x p
                    c = toChar $ fromIntegral i
                in c : go (pred cnt) y
            | cnt == 0 = ""
            | otherwise = intError
    in reverse . go n . bitsToNum @Integer

instance
    ( IsSchema st VStringType
    , KnownNat n
    ) => ToString ruleIx (Variation
        ('GElement o n ('GContextFree ('GContentString st))))
  where
    asString (Variation val) = case val of
        UElement s -> bitsToString
            (schema @st Proxy)
            (fromIntegral $ natVal (Proxy @n))
            s
        _ -> intError

instance {-# OVERLAPPING #-}
    ( IsSchema st VStringType
    , KnownNat n
    ) => ToString ('Just 0) (Variation
        ('GElement o n ('GDependent ts1 dt
            ( '(x, 'GContentString st) ': ts2))))
  where
    asString (Variation val) = case val of
        UElement s -> bitsToString
            (schema @st Proxy)
            (fromIntegral $ natVal (Proxy @n))
            s
        _ -> intError

instance
    ( ToString (Just (ix - 1))
        (Variation (GElement o n (GDependent ts1 dt ts2)))
    ) => ToString ('Just ix) (Variation
        ('GElement o n ('GDependent ts1 dt
            ( '(x, 'GContentString st) ': ts2))))
  where
    asString (Variation val) = asString
        @('Just (ix -1))
        @(Variation ('GElement o n ('GDependent ts1 dt ts2)))
        (Variation val)

instance
    ( ToString ruleIx (Variation var)
    ) => ToString ruleIx (RuleVar ('GContextFree var))
  where
    asString = asString @ruleIx @(Variation var)
        . Variation . uRuleVar . unRuleVar

instance
    ( ToString ruleIx (RuleVar rv)
    ) => ToString ruleIx (NonSpare ('GNonSpare name title rv))
  where
    asString = asString @ruleIx @(RuleVar rv)
        . RuleVar . unUNonSpare . unNonSpare

instance
    ( ToString ruleIx (NonSpare nsp)
    ) => ToString ruleIx (Item ('GItem nsp))
  where
    asString (Item i) = case i of
        UItem val -> asString @ruleIx @(NonSpare nsp) (NonSpare val)
        _ -> intError

class ToInteger ruleIx t where
    asInteger :: t -> Integer

applySignedness :: (Integral b, Num a, Ord a) => GSignedness -> b -> a -> a
applySignedness sig n val = case sig of
    GUnsigned -> val
    GSigned -> withAssumption (n > 0) $
        let half = 2 ^ (n-1)
        in case val < half of
            True -> val
            False -> val - (2 * half)

bitsToInteger :: VSignedness -> Int -> Bits -> Integer
bitsToInteger sig n
    = applySignedness sig n
    . bitsToNum @Integer

instance
    ( IsSchema sig VSignedness
    , KnownNat n
    ) => ToInteger ruleIx (Variation
        ('GElement o n ('GContextFree ('GContentInteger sig))))
  where
    asInteger (Variation val) = case val of
        UElement s -> bitsToInteger
            (schema @sig Proxy)
            (fromIntegral $ natVal (Proxy @n))
            s
        _ -> intError

instance {-# OVERLAPPING #-}
    ( IsSchema sig VSignedness
    , KnownNat n
    ) => ToInteger ('Just 0) (Variation
        ('GElement o n ('GDependent ts1 dt
            ( '(x, 'GContentInteger sig) ': ts2))))
  where
    asInteger (Variation val) = case val of
        UElement s -> bitsToInteger
            (schema @sig Proxy)
            (fromIntegral $ natVal (Proxy @n))
            s
        _ -> intError

instance
    ( ToInteger (Just (ix - 1))
        (Variation (GElement o n (GDependent ts1 dt ts2)))
    ) => ToInteger ('Just ix) (Variation
        ('GElement o n ('GDependent ts1 dt
            ( '(x, 'GContentString st) ': ts2))))
  where
    asInteger (Variation val) = asInteger
        @('Just (ix -1))
        @(Variation ('GElement o n ('GDependent ts1 dt ts2)))
        (Variation val)

instance
    ( ToInteger ruleIx (Variation var)
    ) => ToInteger ruleIx (RuleVar ('GContextFree var))
  where
    asInteger = asInteger @ruleIx @(Variation var)
        . Variation . uRuleVar . unRuleVar

instance
    ( ToInteger ruleIx (RuleVar rv)
    ) => ToInteger ruleIx (NonSpare ('GNonSpare name title rv))
  where
    asInteger = asInteger @ruleIx @(RuleVar rv)
        . RuleVar . unUNonSpare . unNonSpare

instance
    ( ToInteger ruleIx (NonSpare nsp)
    ) => ToInteger ruleIx (Item ('GItem nsp))
  where
    asInteger (Item i) = case i of
        UItem val -> asInteger @ruleIx @(NonSpare nsp) (NonSpare val)
        _ -> intError

class ToQuantity unit ruleIx t where
    asQuantity :: t -> Quantity unit ruleIx

bitsToDouble :: VSignedness -> Int -> Double -> Bits -> Double
bitsToDouble sig n lsb
    = applyLsb
    . applySignedness sig n
    . bitsToNum @Integer
  where
    applyLsb = (* lsb) . fromIntegral

instance
    ( IsSchema sig VSignedness
    , KnownNat n
    , IsSchema lsb VNumber
    , unit1 ~ unit2
    ) => ToQuantity unit1 ruleIx (Variation
        ('GElement o n ('GContextFree ('GContentQuantity sig lsb unit2))))
  where
    asQuantity (Variation val) = case val of
        UElement s -> Quantity $ bitsToDouble
            (schema @sig Proxy)
            (fromIntegral $ natVal (Proxy @n))
            (evalNum $ schema @lsb Proxy)
            s
        _ -> intError

instance {-# OVERLAPPING #-}
    ( IsSchema sig VSignedness
    , KnownNat n
    , IsSchema lsb VNumber
    , unit1 ~ unit2
    ) => ToQuantity unit1 ('Just 0) (Variation
        ('GElement o n ('GDependent ts1 dt
            ( '(x, 'GContentQuantity sig lsb unit2) ': ts2))))
  where
    asQuantity (Variation val) = case val of
        UElement s -> Quantity $ bitsToDouble
            (schema @sig Proxy)
            (fromIntegral $ natVal (Proxy @n))
            (evalNum $ schema @lsb Proxy)
            s
        _ -> intError

instance
    ( ToQuantity unit1 (Just (ix - 1))
        (Variation (GElement o n (GDependent ts1 dt ts2)))
    ) => ToQuantity unit1 ('Just ix) (Variation
        ('GElement o n ('GDependent ts1 dt
            ( '(x, 'GContentQuantity sig lsb unit2) ': ts2))))
  where
    asQuantity (Variation val) = Quantity $ unQuantity $
        asQuantity @unit1 @('Just (ix -1))
            @(Variation ('GElement o n ('GDependent ts1 dt ts2))) (Variation val)

instance
    ( ToQuantity unit ruleIx (Variation var)
    ) => ToQuantity unit ruleIx (RuleVar ('GContextFree var))
  where
    asQuantity = asQuantity @unit @ruleIx @(Variation var)
        . Variation . uRuleVar . unRuleVar

instance
    ( ToQuantity unit ruleIx (RuleVar rv)
    ) => ToQuantity unit ruleIx (NonSpare ('GNonSpare name title rv))
  where
    asQuantity = asQuantity @unit @ruleIx @(RuleVar rv)
        . RuleVar . unUNonSpare . unNonSpare

instance
    ( ToQuantity unit ruleIx (NonSpare nsp)
    ) => ToQuantity unit ruleIx (Item ('GItem nsp))
  where
    asQuantity (Item i) = case i of
        UItem val -> asQuantity @unit @ruleIx @(NonSpare nsp) (NonSpare val)
        _ -> intError

getVariation :: NonSpare ('GNonSpare name title ('GContextFree var)) -> Variation var
getVariation = Variation . uRuleVar . unUNonSpare . unNonSpare

-- | Extract all spare items from group.
getGroupSpares :: Variation ('GGroup o lst) -> [Bits]
getGroupSpares (Variation var) = case var of
    UGroup lst -> lst >>= \case
        USpare val -> pure val
        _ -> empty
    _ -> intError

getGroupItem :: forall name t o lst ix.
    ( t ~ 'GGroup o lst
    , ix ~ FindIndex ('GGroup o lst) name
    , KnownNat ix
    ) => Variation t -> NonSpare (t ~> name)
getGroupItem (Variation var) = case var of
    UGroup lst -> f $ lst !! fromIntegral (natVal (Proxy @ix))
    _ -> intError
  where
    f = \case
        UItem nsp -> NonSpare nsp
        _ -> intError

-- | Extract all present spare items from extended.
getExtendedSpares :: Variation ('GExtended lst) -> [Bits]
getExtendedSpares (Variation var) = case var of
    UExtended _bld lst -> lst >>= \case
        Just (USpare val) -> pure val
        _ -> empty
    _ -> intError

getExtendedItem :: forall name t lst ix.
    ( t ~ 'GExtended lst
    , ix ~ FindIndex ('GExtended lst) name
    , KnownNat ix
    ) => Variation t -> Maybe (NonSpare (t ~> name))
getExtendedItem (Variation var) = case var of
    UExtended _bld lst -> do
        guard (n < length lst)
        case lst !! n of
            Just (UItem nsp) -> Just (NonSpare nsp)
            _ -> intError
    _ -> intError
  where
    n :: Int = fromIntegral $ natVal (Proxy @ix)

getRepetitiveItems :: Variation ('GRepetitive rt var) -> [Variation var]
getRepetitiveItems (Variation var) = case var of
    URepetitive _bld lst -> fmap Variation lst
    _ -> intError

getExplicitData :: Variation ('GExplicit mt) -> Bits
getExplicitData (Variation var) = case var of
    UExplicit _bld val -> val
    _ -> intError

-- TODO: getCompoundItem, setCompoundItem as class method,
-- to be abl to use it with NonSpare or Variation
-- or even getItem, setItem, delItem... name overloading

getCompoundItem :: forall name t lst ix.
    ( t ~ 'GCompound lst
    , ix ~ FindIndex ('GCompound lst) name
    , KnownNat ix
    ) => Variation t -> Maybe (NonSpare (t ~> name))
getCompoundItem (Variation var) = case var of
    UCompound _bld lst -> NonSpare <$> (lst !! n)
    _ -> intError
  where
    n :: Int = fromIntegral $ natVal (Proxy @ix)

setCompoundItem :: forall name t lst ix.
    ( t ~ 'GCompound lst
    , ix ~ FindIndex ('GCompound lst) name
    , KnownNat ix
    ) => NonSpare (t ~> name) -> Variation t -> Variation t
setCompoundItem (NonSpare nsp) (Variation var) = case var of
    UCompound _bld lst1 ->
        -- replace element in a list and rebuild
        let (a, b) = splitAt n lst1
            lst2 = a <> [Just nsp] <> tail b
            bld = rebuildCompound lst2
        in Variation (UCompound bld lst2)
    _ -> intError
  where
    n :: Int = fromIntegral $ natVal (Proxy @ix)

delCompoundItem :: forall name t lst ix.
    ( t ~ 'GCompound lst
    , ix ~ FindIndex ('GCompound lst) name
    , KnownNat ix
    ) => Variation t -> Variation t
delCompoundItem (Variation var) = case var of
    UCompound _bld lst1 ->
        let (a, b) = splitAt n lst1
            lst2 = a <> [Nothing] <> tail b
            bld = rebuildCompound lst2
        in Variation (UCompound bld lst2)
    _ -> intError
  where
    n :: Int = fromIntegral $ natVal (Proxy @ix)

getRecordItem :: forall name t lst ix.
    ( t ~ 'GRecord lst
    , ix ~ FindIndex ('GRecord lst) name
    , KnownNat ix
    ) => Record t -> Maybe (NonSpare (t ~> name))
getRecordItem (Record (URecord _bld lst))
        = f <$> lst !! fromIntegral (natVal (Proxy @ix))
      where
        f :: RecordItem UNonSpare
            -> NonSpare (Lookup name (PrependName (RecordNonSpares lst)))
        f = \case
            RecordItem nsp -> NonSpare nsp
            _ -> intError

setRecordItem :: forall name t lst ix.
    ( t ~ 'GRecord lst
    , ix ~ FindIndex ('GRecord lst) name
    , KnownNat ix
    ) => NonSpare (t ~> name) -> Record t -> Record t
setRecordItem (NonSpare nsp) (Record (URecord _bld lst1))
    = Record (URecord bld lst2)
  where
    n :: Int = fromIntegral $ natVal (Proxy @ix)
    (a, b) = splitAt n lst1
    lst2 = a <> [Just $ RecordItem nsp] <> tail b
    bld = rebuildRecord lst2

delRecordItem :: forall name t lst ix.
    ( t ~ 'GRecord lst
    , ix ~ FindIndex ('GRecord lst) name
    , KnownNat ix
    ) => Record t -> Record t
delRecordItem (Record (URecord _bld lst1)) = Record (URecord bld lst2)
  where
    n :: Int = fromIntegral $ natVal (Proxy @ix)
    (a, b) = splitAt n lst1
    lst2 = a <> [Nothing] <> tail b
    bld = rebuildRecord lst2

getExpansionItem :: forall name t mn lst ix.
    ( t ~ 'GExpansion mn lst
    , ix ~ FindIndex ('GExpansion mn lst) name
    , KnownNat ix
    ) => Expansion t -> Maybe (NonSpare (t ~> name))
getExpansionItem (Expansion (UExpansion _bld lst))
    = NonSpare <$> (lst !! n)
  where
    n :: Int = fromIntegral $ natVal (Proxy @ix)

