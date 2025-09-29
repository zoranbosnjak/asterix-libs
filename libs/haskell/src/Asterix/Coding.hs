-- |
-- Module: Asterix.Coding
--
-- Asterix data structures and functions to decode, encode and
-- manipulate asterix data.

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE OverloadedStrings    #-}

module Asterix.Coding
( module Asterix.Coding
, module Asterix.Schema
, module Asterix.BitString
, module Asterix.Base
) where

import           Prelude hiding (head, tail)
import           Data.Coerce
import           Control.Applicative
import           Control.Monad
import           Data.Proxy
import qualified Data.Text as T
import           Data.Bool
import           Data.ByteString (ByteString)
import           Data.Char (isAsciiUpper, isDigit, ord)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.Type.Bool
import           Data.Type.Equality
import           GHC.TypeLits
import           Data.Kind (Type)

import           Asterix.Base
import           Asterix.Schema
import           Asterix.BitString

-- The 'U' in the following types stands for 'Untyped' version
-- of data, in contrast to typed versions as defined below.
--
-- In some data structures, the fields are stored redundantly,
-- to optimize some parsing/unparsing scenarios.

data UVariation
    = UElement Bits
    | UGroup [UItem]
    | UExtended SBuilder [Maybe UItem]
    | URepetitive SBuilder [UVariation]
    | UExplicit SBuilder Bits
    | UCompound SBuilder [Maybe UNonSpare]
    deriving Show

newtype URuleVar = URuleVar { unURuleVar :: UVariation }
    deriving Show

newtype UNonSpare = UNonSpare { unUNonSpare :: URuleVar }
    deriving Show

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

newtype Variation (t :: TVariation) = Variation { unVariation :: UVariation }
    deriving Show

newtype RuleVar (t1 :: TRule t2) = RuleVar { unRuleVar :: URuleVar }
    deriving Show

newtype NonSpare (t :: TNonSpare) = NonSpare { unNonSpare :: UNonSpare }
    deriving Show

newtype Item (t :: TItem) = Item { unItem :: UItem }
    deriving Show

newtype Record (t :: TRecord) = Record { unRecord :: URecord }
    deriving Show

newtype Expansion (t :: TExpansion) = Expansion { unExpansion :: UExpansion }
    deriving Show

newtype Datablock (db :: TDatablock) = Datablock { unDatablock :: UDatablock }
    deriving Show

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
    unparse = unparse . unURuleVar

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
        RecordItemRFS lst -> numItems <> items
          where
            numItems = word8ToSBuilder (fromIntegral (length lst))
            items = mconcat $ do
                (frn, nsp) <- lst
                pure
                    ( word8ToSBuilder frn
                   <> bitsToSBuilder (unparse nsp))

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

-- Num instances
-- Normally, only 'fromInteger' method is required,
-- but the existing Num typeclass makes it possible
-- for better syntactic sugar, without function name.

instance
    ( KnownNat8 o
    , KnownNat n
    ) => Num (Variation ('GElement o n rc)) where
    a + b = fromInteger (unparseToNum a + unparseToNum b)
    a * b = fromInteger (unparseToNum a * unparseToNum b)
    abs = fromInteger . abs . unparseToNum
    signum = fromInteger . signum . unparseToNum
    negate = fromInteger . negate . unparseToNum
    fromInteger = Variation . UElement . integerToBits
        (unInt8 $ natVal8 (Proxy @o))
        (fromInteger $ natVal (Proxy @n))

instance Num (Variation ('GGroup o '[])) where
    a + b = fromInteger (unparseToNum a + unparseToNum b)
    a * b = fromInteger (unparseToNum a * unparseToNum b)
    abs = fromInteger . abs . unparseToNum
    signum = fromInteger . signum . unparseToNum
    negate = fromInteger . negate . unparseToNum
    fromInteger _ = Variation $ UGroup []

instance
    ( Num (Item t)
    , Num (Variation ('GGroup o ts))
    , KnownNat (BitSizeOf ts)
    ) => Num (Variation ('GGroup o (t ': ts))) where
    a + b = fromInteger (unparseToNum a + unparseToNum b)
    a * b = fromInteger (unparseToNum a * unparseToNum b)
    abs = fromInteger . abs . unparseToNum
    signum = fromInteger . signum . unparseToNum
    negate = fromInteger . negate . unparseToNum
    fromInteger i =
        let n = natVal (Proxy @(BitSizeOf ts))
            (a, b) = divMod i (2 ^ n)
            x = unItem $ fromInteger @(Item t) a
            xs = case fromInteger @(Variation ('GGroup o ts)) b of
                Variation (UGroup lst) -> lst
                _                      -> intError
        in Variation $ UGroup (x : xs)

instance
    ( Num (Variation t)
    ) => Num (RuleVar ('GContextFree t)) where
    a + b = fromInteger (unparseToNum a + unparseToNum b)
    a * b = fromInteger (unparseToNum a * unparseToNum b)
    abs = fromInteger . abs . unparseToNum
    signum = fromInteger . signum . unparseToNum
    negate = fromInteger . negate . unparseToNum
    fromInteger = RuleVar . URuleVar . unVariation
        . fromInteger @(Variation t)

instance
    ( Num (Variation t)
    ) => Num (RuleVar ('GDependent ts1 t ts2)) where
    a + b = fromInteger (unparseToNum a + unparseToNum b)
    a * b = fromInteger (unparseToNum a * unparseToNum b)
    abs = fromInteger . abs . unparseToNum
    signum = fromInteger . signum . unparseToNum
    negate = fromInteger . negate . unparseToNum
    fromInteger = RuleVar . URuleVar . unVariation
        . fromInteger @(Variation t)

instance
    ( Num (RuleVar rv)
    ) => Num (NonSpare ('GNonSpare name title rv)) where
    a + b = fromInteger (unparseToNum a + unparseToNum b)
    a * b = fromInteger (unparseToNum a * unparseToNum b)
    abs = fromInteger . abs . unparseToNum
    signum = fromInteger . signum . unparseToNum
    negate = fromInteger . negate . unparseToNum
    fromInteger = NonSpare . UNonSpare . unRuleVar . fromInteger @(RuleVar rv)

instance
    ( KnownNat8 o
    , KnownNat n
    ) => Num (Item ('GSpare o n)) where
    a + b = fromInteger (unparseToNum a + unparseToNum b)
    a * b = fromInteger (unparseToNum a * unparseToNum b)
    abs = fromInteger . abs . unparseToNum
    signum = fromInteger . signum . unparseToNum
    negate = fromInteger . negate . unparseToNum
    fromInteger = Item . USpare . integerToBits
        (unInt8 $ natVal8 (Proxy @o))
        (fromInteger $ natVal (Proxy @n))

instance
    ( Num (NonSpare nsp)
    ) => Num (Item ('GItem nsp)) where
    a + b = fromInteger (unparseToNum a + unparseToNum b)
    a * b = fromInteger (unparseToNum a * unparseToNum b)
    abs = fromInteger . abs . unparseToNum
    signum = fromInteger . signum . unparseToNum
    negate = fromInteger . negate . unparseToNum
    fromInteger = Item . UItem . unNonSpare . fromInteger @(NonSpare nsp)

-- The Asterix.Base module performs the actual parsing.
-- This structure tells how to store parsing results.
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
    , psRuleVar = URuleVar

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

type Parsing pm = ParsingM pm UVariation URuleVar UNonSpare UItem URecord UExpansion

parse :: forall pm r.
    ( KnownParsingMode pm
    ) => Parsing pm r -> ByteString -> Either ParsingError r
parse act bs = do
    (result, o) <- runParsing act (Env parsingStore bs) 0
    case parsingMode @pm Proxy of
        StrictParsing -> case o == endOffset bs of
            True -> pure result
            False -> Left "remaining bits"
        PartialParsing -> pure result

-- Constructing

data Ix
    = IxItem Symbol
    | IxRfs Nat

newtype Indexed (ix :: Ix) a = Indexed a
    deriving (Eq, Show)

type Named name = Indexed ('IxItem name)
type Rfs cnt ts = Indexed ('IxRfs cnt) (HList ts)

deriving instance (Num a) => Num (Indexed ('IxItem name) a)

item :: forall name a. a -> Named name a
item = Indexed

rfs :: forall cnt ts. HList ts -> Rfs cnt ts
rfs = Indexed

-- | Reserved spare bits might contain non-zero value.
newtype AbuseSpare a = AbuseSpare a
    deriving (Eq, Show, Num)

abuseSpare :: a -> AbuseSpare a
abuseSpare = AbuseSpare

spare :: Num a => AbuseSpare a
spare = abuseSpare 0

data Fx = Fx
    deriving (Eq, Show)

fx :: Fx
fx = Fx

-- Construct string

-- | Function name overloading for different resulting types.
class MkString t where
    string :: String -> t

instance
    ( KnownNat8 o
    , KnownNat n
    , IsSchema st GStringType
    ) => MkString (Variation ('GElement o n ('GContextFree
        ('GContentString st))))
  where
    string val = Variation $ UElement $ integerToBits
        (unInt8 $ natVal8 @o Proxy)
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
    string = RuleVar . URuleVar . unVariation . string @(Variation var)

instance
    ( MkString (RuleVar rv)
    ) => MkString (NonSpare ('GNonSpare name title rv)) where
    string = NonSpare . UNonSpare . unRuleVar . string @(RuleVar rv)

instance
    ( MkString (NonSpare nsp)
    ) => MkString (Item ('GItem nsp)) where
    string = Item . UItem . unNonSpare . string @(NonSpare nsp)

instance
    ( MkString val
    , item ~ Item ('GItem nsp)
    , nsp ~ 'GNonSpare name2 title rv
    , name1 ~ name2
    ) => MkString (Named name1 val)
  where
    string = item . string @val

-- Construct quantity

newtype Quantity (unit :: Symbol) (ruleIx :: (Maybe Nat)) = Quantity
    { unQuantity :: Double
    } deriving (Eq, Ord, Show, Num, Fractional)

-- There is a lot of code just to extract the lsb out of the type.
-- This would otherwise be easy to do on value level schema, however
-- we want type safety on 'unit' matching.

-- | Helper class to extract 'lsb' value, required for quantity function.
class GetLsb unit ruleIx t where
    getLsb :: VNumber

instance
    ( IsSchema lsb VNumber
    , ruleIx ~ 'Nothing
    , unit1 ~ unit2
    ) => GetLsb unit1 ruleIx (Variation
        ('GElement o n ('GContextFree ('GContentQuantity sig lsb unit2))))
  where
    getLsb = schema @lsb Proxy

-- | Another helper class to handle dependent cases.
class GetLsb2 (flag :: Bool) (unit :: Symbol) (ix :: Nat) (rules :: [k])
  where
    getLsb2 :: Proxy flag -> Proxy unit -> Proxy ix -> Proxy rules -> VNumber

instance
    ( ix ~ 0
    , unit1 ~ unit2
    , IsSchema lsb VNumber
    ) => GetLsb2 'True unit1 ix ( '( val, 'GContentQuantity sig lsb unit2) ': ts)
  where
    getLsb2 _ _ _ _ = schema @lsb Proxy

instance
    ( iy ~ ix - 1
    , flag ~ (iy == 0)
    , GetLsb2 flag unit iy ts
    ) => GetLsb2 'False unit ix (t ': ts)
  where
    getLsb2 _ _ _ _
        = getLsb2 (Proxy @flag) (Proxy @unit) (Proxy @iy) (Proxy @ts)

-- We can now defined GetLsb instance using the auxilary GetLsb2
instance
    ( ruleIx ~ 'Just ix
    , flag ~ (ix == 0)
    , GetLsb2 flag unit ix rules
    ) => GetLsb unit ruleIx (Variation
        ('GElement o n ('GDependent sel dt rules)))
  where
    getLsb = getLsb2 (Proxy @flag) (Proxy @unit) (Proxy @ix) (Proxy @rules)

-- | Function name overloading for different output types.
class MkQuantity unit ruleIx t where
    quantity :: Quantity unit ruleIx -> t

-- | When converting from Double to t, the 'sig' field is not important.
-- It only becomes important when converting back from t to Double.
processQuantity :: Int8 -> Int -> Double -> Quantity unit ruleIx -> Variation t
processQuantity (Int8 o) n lsb = Variation . UElement . integerToBits o n
    . round . (/ lsb) . unQuantity

instance
    ( KnownNat8 o
    , KnownNat n
    , GetLsb unit ruleIx (Variation (GElement o n rc))
    ) => MkQuantity unit ruleIx (Variation ('GElement o n rc))
  where
    quantity =
        let lsb = getLsb @unit @ruleIx @(Variation ('GElement o n rc))
        in processQuantity
            (natVal8 @o Proxy)
            (fromIntegral $ natVal @n Proxy)
            (evalNum lsb)

instance
    ( MkQuantity unit ix (Variation var)
    ) => MkQuantity unit ix (RuleVar ('GContextFree var))
  where
    quantity = RuleVar . URuleVar . unVariation
        . quantity @unit @ix @(Variation var)

instance
    ( MkQuantity unit ix (RuleVar rv)
    ) => MkQuantity unit ix (NonSpare ('GNonSpare name title rv))
  where
    quantity = NonSpare . UNonSpare . unRuleVar
        . quantity @unit @ix @(RuleVar rv)

instance
    ( MkQuantity unit ix (NonSpare nsp)
    ) => MkQuantity unit ix (Item ('GItem nsp))
  where
    quantity = Item . UItem . unNonSpare
        . quantity @unit @ix @(NonSpare nsp)

instance
    ( MkQuantity unit ix val
    ) => MkQuantity unit ix (Named name val)
  where
    quantity = item . quantity @unit @ix @val

-- Group construction
--
-- It's implemented in 2 staged:
--  - CGroup instances creates [UItem]
--  - MkGroup instances creates target types
--    (Variation, NonSpare, Item, Named)

class CGroup ts2 ts1 where
    groupConstruct :: Proxy ts2 -> HList ts1 -> [UItem]

instance
    TypeError ('Text "CGroup missing spare item")
    => CGroup ('GSpare o n ': ts) '[] where
    groupConstruct = intError

instance
    TypeError ('Text "CGroup missing item: " :<>: 'ShowType name)
    => CGroup ('GItem ('GNonSpare name title rv) ': ts) '[] where
    groupConstruct = intError

instance
    TypeError ('Text "CGroup unexpected item: " :<>: 'ShowType t)
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
    groupConstruct _proxy (HCons (Indexed x) xs)
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
    group = NonSpare . UNonSpare . URuleVar . unVariation
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
    ) => MkGroup (Named name1
        (Item ('GItem ('GNonSpare name2 title ('GContextFree var))))) ts1
  where
    group = item
        . group @(Item ('GItem ('GNonSpare name2 title ('GContextFree var))))

-- Extended construction
--
-- It's implemented in 2 stages
--  - CExtended instances create [Maybe UItem]
--  - MkExtended, MkExtendedGroups create target types
--    It is possible to construct extended either by
--      1) extended (HList of individual items, fx...)
--      2) extendedGroups (HList of groups-of-items)

-- Extended construction by items

class CExtended ts2 done ts1 where
    extendedConstruct :: Proxy ts2 -> Proxy done -> HList ts1 -> [Maybe UItem]

instance
    TypeError ('Text "CExtended missing FX item")
    => CExtended ('Nothing ': ts) 'False '[] where
    extendedConstruct = intError

instance
    TypeError ('Text "CExtended missing spare item")
    => CExtended ('Just ('GSpare o n) ': ts) 'False '[] where
    extendedConstruct = intError

instance
    TypeError ('Text "CExtended missing item: " :<>: 'ShowType name)
    => CExtended ('Just ('GItem ('GNonSpare name title rv)) ': ts) 'False '[] where
    extendedConstruct = intError

instance
    TypeError ('Text "CExtended unexpected item: " :<>: 'ShowType t)
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
    ) => CExtended ('Just ('GItem ('GNonSpare name2 title rv)) ': ts2) done
        (t1 ': ts1)
  where
    extendedConstruct _p1 _p2 (HCons (Indexed x) xs)
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
    extended = NonSpare . UNonSpare . URuleVar . unVariation
        . extended @(Variation var)

instance
    ( MkExtended (NonSpare ('GNonSpare name title rv)) ts1
    ) => MkExtended (Item ('GItem ('GNonSpare name title rv))) ts1
  where
    extended = Item . UItem . unNonSpare
        . extended @(NonSpare ('GNonSpare name title rv))

instance
    ( name1 ~ name2
    , MkExtended (Item ('GItem ('GNonSpare name2 title (GContextFree var)))) ts1
    ) => MkExtended (Named name1
        (Item ('GItem ('GNonSpare name2 title ('GContextFree var))))) ts1
  where
    extended = item
        . extended @(Item ('GItem ('GNonSpare name2 title ('GContextFree var))))

-- Extended construction by groups

class CExtendedGroups ts2 ts1 where
    extendedConstructGroups :: Proxy ts2 -> HList ts1 -> [Maybe UItem]

instance
    ( TypeError ('Text
        "CExtendedGroups at least one extended group shall be provided")
    ) => CExtendedGroups ts2 '[] where
    extendedConstructGroups = intError

instance -- this instance matches last group (t1 ': '[])
    ( t1 ~ Variation ('GGroup 0 lst)
    , lst ~ ExtendedFirstGroup ts2
    , fx ~ ExtendedTrailingFx ts2
    , KnownBool fx
    ) => CExtendedGroups ts2 (t1 ': '[]) where
    extendedConstructGroups _p (HCons (Variation val) HNil) = case val of
        UGroup lst -> fmap Just lst <> trailingFx
        _ -> intError
      where
        appendFx = boolVal @fx Proxy
        trailingFx = bool [] [Nothing] appendFx

instance -- this instance matches non-last group (t1 ': t2 ': '[])
    ( t1 ~ Variation ('GGroup 0 lst)
    , lst ~ ExtendedFirstGroup ts2
    , ts3 ~ ExtendedRemainingItems ts2
    , CExtendedGroups ts3 (t2 ': ts1)
    ) => CExtendedGroups ts2 (t1 ': t2 ': ts1) where
    extendedConstructGroups _p (HCons (Variation val) xs) = case val of
        UGroup lst ->
            fmap Just lst
          <> [Nothing]
          <> extendedConstructGroups @ts3 @(t2 ': ts1) Proxy xs
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
    ) => MkExtendedGroups (NonSpare ('GNonSpare name title ('GContextFree var)))
        ts1
  where
    extendedGroups = NonSpare . UNonSpare . URuleVar . unVariation
        . extendedGroups @(Variation var)

instance
    ( MkExtendedGroups (NonSpare ('GNonSpare name title rv)) ts1
    ) => MkExtendedGroups (Item ('GItem ('GNonSpare name title rv))) ts1
  where
    extendedGroups = Item . UItem . unNonSpare
        . extendedGroups @(NonSpare ('GNonSpare name title rv))

instance
    ( name1 ~ name2
    , MkExtendedGroups (Item ('GItem ('GNonSpare name2 title (GContextFree var))))
      ts1
    ) => MkExtendedGroups (Named name1
        (Item ('GItem ('GNonSpare name2 title ('GContextFree var))))) ts1
  where
    extendedGroups = item . extendedGroups
        @(Item ('GItem ('GNonSpare name2 title ('GContextFree var))))

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
    repetitive = NonSpare . UNonSpare . URuleVar . unVariation
        . repetitive @(Variation var)

instance
    ( MkRepetitive (NonSpare ('GNonSpare name title rv)) ts1
    ) => MkRepetitive (Item ('GItem ('GNonSpare name title rv))) ts1
  where
    type RepetitiveInputList (Item ('GItem ('GNonSpare name title rv)))
        = RepetitiveInputList (NonSpare ('GNonSpare name title rv))
    repetitive = Item . UItem . unNonSpare
        . repetitive @(NonSpare ('GNonSpare name title rv))

instance
    ( name1 ~ name2
    , MkRepetitive (Item ('GItem ('GNonSpare name2 title (GContextFree var))))
      ts1
    ) => MkRepetitive (Named name1
        (Item ('GItem ('GNonSpare name2 title ('GContextFree var))))) ts1
  where
    type RepetitiveInputList
        (Named name1
            (Item ('GItem ('GNonSpare name2 title ('GContextFree var)))))
            = RepetitiveInputList
                (Item ('GItem ('GNonSpare name2 title (GContextFree var))))
    repetitive = item . repetitive
        @(Item ('GItem ('GNonSpare name2 title ('GContextFree var))))

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
    explicit = NonSpare . UNonSpare . URuleVar . unVariation
        . explicit @(Variation var)

instance
    ( MkExplicit (NonSpare ('GNonSpare name title rv)) ts1
    ) => MkExplicit (Item ('GItem ('GNonSpare name title rv))) ts1
  where
    explicit = Item . UItem . unNonSpare
        . explicit @(NonSpare ('GNonSpare name title rv))

instance
    ( name1 ~ name2
    , MkExplicit (Item ('GItem ('GNonSpare name2 title (GContextFree var))))
      ts1
    ) => MkExplicit (Named name1
        (Item ('GItem ('GNonSpare name2 title ('GContextFree var))))) ts1
  where
    explicit = item . explicit
        @(Item ('GItem ('GNonSpare name2 title ('GContextFree var))))

-- | Helper class for compound and record construction.
class CSetItem nsp ts1 where
    cSetItem :: Proxy nsp -> HList ts1 -> Maybe UNonSpare

instance CSetItem nsp '[] where
    cSetItem _p _hlist = Nothing

instance {-# OVERLAPPING #-}
    ( nsp ~ NonSpare ('GNonSpare name title rv)
    ) => CSetItem ('GNonSpare name title rv) (Named name nsp ': ts)
  where
    cSetItem _p (HCons (Indexed nsp) _ts) = Just $ unNonSpare nsp

instance
    ( CSetItem ('GNonSpare name1 title rv) ts
    ) => CSetItem ('GNonSpare name1 title rv) (Named name2 nsp ': ts)
  where
    cSetItem p (HCons _nsp ts) = cSetItem p ts

-- | Check if name is present in HList.
type family ElemName ix ts where
    ElemName ix '[] = 'False
    ElemName ix (Named ix1 a ': ts) = If (ix == ix1) 'True (ElemName ix ts)
    ElemName ix (Rfs cnt a ': ts) = ElemName ix ts
    ElemName ix ts = TypeError ('Text "ElemName unexpected argument: "
        :<>: 'ShowType ix :<>: 'Text ", " :<>: 'ShowType ts)

-- | Check if rfs index is present in HList.
type family ElemRfs ix ts where
    ElemRfs ix '[] = 'False
    ElemRfs ix (Named name a ': ts) = ElemRfs ix ts
    ElemRfs ix (Rfs cnt a ': ts) = If (ix == cnt) 'True (ElemRfs ix ts)
    ElemRfs ix ts = TypeError ('Text "ElemRfs unexpected argument: "
        :<>: 'ShowType ix :<>: 'Text ", " :<>: 'ShowType ts)

-- | Assert there are no duplicates in a HList
-- This type level function could simply return the constraint,
-- however, GHC sometimes takes it as redundant, so return types instead.
type family NoDuplicates t where
    NoDuplicates '[] = '[]
    NoDuplicates (Named name a ': ts) = If
        (ElemName name ts)
        (TypeError ('Text "NoDuplicates duplicated item: " :<>: 'ShowType name))
        (Named name a ': NoDuplicates ts)
    NoDuplicates (Rfs cnt a ': ts) = If
        (ElemRfs cnt ts)
        (TypeError ('Text "NoDuplicates duplicated rfs: " :<>: 'ShowType cnt))
        (Rfs cnt a ': NoDuplicates ts)
    NoDuplicates t
        = TypeError ('Text "NoDuplicates unexpected argument: " :<>: 'ShowType t)

-- Compound construction

type family IsDefinedCompound ts name where
    IsDefinedCompound '[] name = 'False
    IsDefinedCompound ('Nothing ': ts) name = IsDefinedCompound ts name
    IsDefinedCompound ('Just ('GNonSpare name1 title rv) ': ts) name = If
        (name1 == name)
        'True
        (IsDefinedCompound ts name)
    IsDefinedCompound ts name
        = TypeError ('Text "IsDefinedCompound unexpected argument: "
            :<>: 'ShowType ts :<>: 'Text ", " :<>: 'ShowType name)

type family AllDefinedCompound ts2 ts1 where
    AllDefinedCompound ts2 '[] = '[]
    AllDefinedCompound ts2 (Named name rv ': ts) = If
        (IsDefinedCompound ts2 name == 'True)
        (Named name rv ': AllDefinedCompound ts2 ts)
        (TypeError ('Text "AllDefinedCompound unknown item: " :<>: 'ShowType name))
    AllDefinedCompound ts2 ts1
        = TypeError ('Text "AllDefinedCompound unexpected argument: "
            :<>: 'ShowType ts2 :<>: 'Text ", " :<>: 'ShowType ts1)

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

-- Compound item is created from HList, where
--  - items are specified in any order
--  - duplicates are not allowed
--  - all items must be named and valid

-- | 'compound' function is overloaded for different output types
class MkCompound t ts1 where
    compound :: HList ts1 -> t

rebuildCompound :: Unparsing Bits a => [Maybe a] -> SBuilder
rebuildCompound items = mkFspecFx (fmap isJust items)
    <> mconcat (bitsToSBuilder . unparse <$> catMaybes items)

instance
    ( CCompound ts2 ts1
    , NoDuplicates ts1 ~ ts1
    , AllDefinedCompound ts2 ts1 ~ ts1
    ) => MkCompound (Variation ('GCompound ts2)) ts1 where
    compound lst1 = Variation $ UCompound bld items where
        items :: [Maybe UNonSpare]
        items = compoundConstruct @ts2 @(AllDefinedCompound ts2 (NoDuplicates ts1))
            Proxy lst1

        bld :: SBuilder
        bld = rebuildCompound items

instance
    ( MkCompound (Variation var) ts1
    ) => MkCompound (NonSpare ('GNonSpare name title ('GContextFree var))) ts1
  where
    compound = NonSpare . UNonSpare . URuleVar . unVariation
        . compound @(Variation var)

instance
    ( MkCompound (NonSpare ('GNonSpare name title rv)) ts1
    ) => MkCompound (Item ('GItem ('GNonSpare name title rv))) ts1
  where
    compound = Item . UItem . unNonSpare
        . compound @(NonSpare ('GNonSpare name title rv))

instance
    ( name1 ~ name2
    , MkCompound (Item ('GItem ('GNonSpare name2 title (GContextFree var)))) ts1
    ) => MkCompound (Named name1
        (Item ('GItem ('GNonSpare name2 title ('GContextFree var))))) ts1
  where
    compound = item
        . compound @(Item ('GItem ('GNonSpare name2 title ('GContextFree var))))

-- Record construction
--
-- Record is constructed similar to 'compound' item, except:
--  - Result is always a Record, so no need to overload 'record' for that
--    reason, but there are cases, depending on number of allowed RFS blocks.
--  - Input HList may include RFS blocks too.
--  - Regular items follow the same rules (any order, no duplication)
--  - RFS blocks rules:
--    - If the record spec does not include RFS, there shall be no RFS
--      block in input HList.
--    - If the record spec contains 1 RFS flag, the can only be 0 or 1
--      RFS block in the input HList. RFS block might be enumerated
--      with '@0' tag or without.
--    - Otherwise, if the record spec contains multiple RFS flags, they
--      must be enumerated in HList, but the order of specification
--      is not important. They should not be duplicated.
--    - The items inside RFS block may be duplicated and specified in the
--      order or required result.

type family MaxRfs ts where
    MaxRfs '[] = 0
    MaxRfs ('GUapItem nsp ': ts) = MaxRfs ts
    MaxRfs ('GUapItemSpare ': ts) = MaxRfs ts
    MaxRfs ('GUapItemRFS ': ts) = 1 + MaxRfs ts
    MaxRfs ts = TypeError ('Text "MaxRfs unexpected argument: " :<>: 'ShowType ts)

type family IsDefinedRecord ts name where
    IsDefinedRecord '[] name = 'False
    IsDefinedRecord ('GUapItem ('GNonSpare name1 title rv) ': ts) name2 = If
        (name1 == name2)
        'True
        (IsDefinedRecord ts name2)
    IsDefinedRecord ('GUapItemSpare ': ts) name2 = IsDefinedRecord ts name2
    IsDefinedRecord ('GUapItemRFS ': ts) name2 = IsDefinedRecord ts name2
    IsDefinedRecord ts name
        = TypeError ('Text "IsDefinedRecord unexpected argument: "
            :<>: 'ShowType ts :<>: 'Text ", " :<>: 'ShowType name)

type family IsDefinedRfs ix ts cnt where
    IsDefinedRfs ix '[] cnt = 'False
    IsDefinedRfs ix ('GUapItem nsp ': ts) cnt = IsDefinedRfs ix ts cnt
    IsDefinedRfs ix ('GUapItemSpare ': ts) cnt = IsDefinedRfs ix ts cnt
    IsDefinedRfs ix ('GUapItemRFS ': ts) cnt = If
        (ix == cnt)
        'True
        (IsDefinedRfs (ix+1) ts cnt)
    IsDefinedRfs ix ts cnt
        = TypeError ('Text "IsDefinedRfs unexpected argument: "
            :<>: 'ShowType ts :<>: 'Text ", " :<>: 'ShowType cnt)

type family AllDefinedRecord ts2 ts1 where
    AllDefinedRecord ts2 '[] = '[]
    AllDefinedRecord ts2 (Named name rv ': ts) = If
        (IsDefinedRecord ts2 name == 'True)
        (Named name rv ': AllDefinedRecord ts2 ts)
        (TypeError ('Text "AllDefinedRecord unknown item: " :<>: 'ShowType name))
    AllDefinedRecord ts2 (Rfs cnt a ': ts) = If
        (IsDefinedRfs 0 ts2 cnt == 'True)
        (Rfs cnt a ': AllDefinedRecord ts2 ts)
        (TypeError ('Text "AllDefinedRecord unknown rfs: " :<>: 'ShowType cnt))
    AllDefinedRecord ts2 ts1
        = TypeError ('Text "AllDefinedRecord unexpected argument: "
            :<>: 'ShowType ts2 :<>: 'Text ", " :<>: 'ShowType ts1)

rebuildRecord :: [Maybe (RecordItem UNonSpare)] -> SBuilder
rebuildRecord items
    = mkFspecFx (fmap isJust items)
   <> mconcat (unparse <$> catMaybes items)

getFrn :: Enum n => n -> VText -> [VUapItem] -> n
getFrn acc name = \case
    [] -> intError
    (GUapItem (GNonSpare name2 _title _rv) : ts) -> case name == name2 of
        True -> acc
        False -> getFrn (succ acc) name ts
    (GUapItemSpare : ts) -> getFrn (succ acc) name ts
    (GUapItemRFS : ts) -> getFrn (succ acc) name ts

class MkRfs tsAll ts2 ts1 where
    mkRfs :: HList ts1 -> [(FRN, UNonSpare)]

instance MkRfs tsAll ts2 '[] where
    mkRfs HNil = []

instance
    ( TypeError ('Text "MkRfs undefined item: " :<>: 'ShowType name)
    ) => MkRfs tsAll '[] (Named name nsp ': ts) where
    mkRfs = intError

instance {-# OVERLAPPING #-}
    ( nsp ~ NonSpare ('GNonSpare name title rv)
    , MkRfs tsAll tsAll ts2
    , KnownSymbol name
    , IsSchema tsAll [VUapItem]
    ) => MkRfs tsAll ('GUapItem ('GNonSpare name title rv) ': ts1)
        (Named name nsp ': ts2) where
    mkRfs (HCons (Indexed (NonSpare x)) xs)
        = (getFrn 1 (schema @name Proxy) (schema @tsAll Proxy), x)
        : mkRfs @tsAll @tsAll xs

instance
    ( MkRfs tsAll ts1 (t2 : ts2)
    ) => MkRfs tsAll (t1 ': ts1) (t2 ': ts2) where
    mkRfs = mkRfs @tsAll @ts1

class RSetItem nsp ts1 where
    rSetItem :: HList ts1 -> Maybe (RecordItem UNonSpare)

instance RSetItem nsp '[] where
    rSetItem _lst = Nothing

instance {-# OVERLAPPING #-}
    ( nsp ~ NonSpare ('GNonSpare name title rv)
    ) => RSetItem ('GNonSpare name title rv) (Named name nsp ': ts) where
    rSetItem (HCons (Indexed (NonSpare x)) _xs) = Just (RecordItem x)

instance
    ( RSetItem ('GNonSpare name title rv) ts
    ) => RSetItem ('GNonSpare name title rv) (t ': ts) where
    rSetItem (HCons _x xs) = rSetItem @('GNonSpare name title rv) xs

class RSetRfs (rfs :: Nat) tsAll ts1 where
    rSetRfs :: HList ts1 -> Maybe (RecordItem UNonSpare)

instance RSetRfs rfs tsAll '[] where
    rSetRfs _lst = Nothing

instance {-# OVERLAPPING #-}
    ( MkRfs tsAll tsAll ts1
    ) => RSetRfs rfs tsAll (Indexed ('IxRfs rfs) (HList ts1) ': ts) where
    rSetRfs (HCons (Indexed lst) _xs)
        = Just $ RecordItemRFS (mkRfs @tsAll @tsAll lst)

instance
    ( RSetRfs rfs tsAll ts
    ) => RSetRfs rfs tsAll (t ': ts) where
    rSetRfs (HCons _x xs) = rSetRfs @rfs @tsAll xs

class CRecord maxRfs rfs tsAll ts2 ts1 where
    recordConstruct :: HList ts1 -> [Maybe (RecordItem UNonSpare)]

instance CRecord (maxRfs :: Nat) (rfs :: Nat) tsAll '[] ts1 where
    recordConstruct _lst = []

instance
    ( CRecord maxRfs rfs tsAll ts ts1
    , RSetItem nsp ts1
    ) => CRecord maxRfs rfs tsAll ('GUapItem nsp ': ts) ts1 where
    recordConstruct lst
        = rSetItem @nsp lst
        : recordConstruct @maxRfs @rfs @tsAll @ts lst

instance
    ( CRecord maxRfs rfs tsAll ts ts1
    ) => CRecord maxRfs rfs tsAll ('GUapItemSpare ': ts) ts1 where
    recordConstruct lst
        = Nothing
        : recordConstruct @maxRfs @rfs @tsAll @ts lst

instance
    ( CRecord maxRfs (rfs+1) tsAll ts ts1
    , RSetRfs rfs tsAll ts1
    ) => CRecord maxRfs rfs tsAll ('GUapItemRFS ': ts) ts1 where
    recordConstruct lst
        = rSetRfs @rfs @tsAll lst
        : recordConstruct @maxRfs @(rfs+1) @tsAll @ts lst

-- In case of a single rfs, we explicitly set it to 'rfs @0',
-- so that inside the application this type application '@0'
-- becomes optional.
class EnumerateRfs (single :: Bool) ts1 where
    type EnumerateRfsR single ts1 :: [Type]
    enumerateRfs :: HList ts1 -> HList (EnumerateRfsR single ts1)

instance EnumerateRfs 'False ts1 where
    type (EnumerateRfsR 'False ts1) = ts1
    enumerateRfs lst = lst

instance EnumerateRfs 'True '[] where
    type (EnumerateRfsR 'True '[]) = '[]
    enumerateRfs lst = lst

instance
    ( EnumerateRfs 'True ts
    ) => EnumerateRfs 'True (Named name a ': ts) where
    type (EnumerateRfsR 'True (Named name a ': ts))
        = Named name a ': EnumerateRfsR 'True ts
    enumerateRfs (HCons x xs) = HCons x (enumerateRfs @'True @ts xs)

instance
    ( EnumerateRfs 'True ts
    , cnt ~ 0
    ) => EnumerateRfs 'True (Rfs cnt a ': ts) where
    type (EnumerateRfsR 'True (Rfs cnt a ': ts))
        = Rfs 0 a ': EnumerateRfsR 'True ts
    enumerateRfs (HCons x xs) = HCons x (enumerateRfs @'True @ts xs)

record :: forall ts2 ts1 ts1'.
    ( ts1' ~ EnumerateRfsR (MaxRfs ts2 == 1) ts1
    , AllDefinedRecord ts2 (NoDuplicates ts1) ~ ts1'
    , CRecord (MaxRfs ts2) 0 ts2 ts2 ts1'
    , EnumerateRfs (MaxRfs ts2 == 1) ts1
    , NoDuplicates ts1 ~ ts1
    ) => HList ts1 -> Record ('GRecord ts2)
record lst = Record $ URecord bld items
  where
    items :: [Maybe (RecordItem UNonSpare)]
    items = recordConstruct @(MaxRfs ts2) @0 @ts2 @ts2
        @(AllDefinedRecord ts2 (NoDuplicates ts1))
            (enumerateRfs @(MaxRfs ts2 == 1) lst)

    bld :: SBuilder
    bld = rebuildRecord items

-- Expansion construction

expansion :: forall mn ts2 ts1.
    ( CCompound ts2 ts1
    , NoDuplicates ts1 ~ ts1
    , AllDefinedCompound ts2 ts1 ~ ts1
    , IsSchema mn (Maybe Int)
    ) => HList ts1 -> Expansion ('GExpansion mn ts2)
expansion lst1 = Expansion $ UExpansion bld items
  where
    items :: [Maybe UNonSpare]
    items = compoundConstruct @ts2 @(AllDefinedCompound ts2 (NoDuplicates ts1))
        Proxy lst1

    fxbits :: SBuilder
    fxbits = case schema @mn Proxy of
        Nothing -> mkFspecFx (fmap isJust items)
        Just nBytes -> mkFspecFixed nBytes (fmap isJust items)

    bld :: SBuilder
    bld = fxbits
        <> mconcat (bitsToSBuilder . unparse <$> catMaybes items)

-- Datablock construction

class CDatablock multi dbt ts where
    datablockConstruct ::
        Proxy multi       -- multi UAP datablock (Bool)
        -> Proxy dbt      -- datablock type
        -> HList ts       -- list of records
        -> [(Maybe VText, URecord)]

instance CDatablock multi dbt '[] where
    datablockConstruct _p1 _p2 _hlist = []

-- For single UAP situation, the type of record is determined
instance
    ( CDatablock 'False dbt ts
    , dbt ~ 'GDatablock cat uap
    , t ~ TypeOfRecord dbt  -- record must match
    ) => CDatablock 'False dbt (Record t ': ts) where
    datablockConstruct p1 p2 (HCons x xs)
        = (Nothing, unRecord x)
        : datablockConstruct p1 p2 xs

-- For multi UAP situation, the type of record must be explicit
instance
    ( CDatablock 'True dbt ts
    , dbt ~ 'GDatablock cat uap
    , uap ~ 'GUaps lst sel
    , name ~ UapName t lst
    , KnownSymbol name
    ) => CDatablock 'True dbt (Record t ': ts) where
    datablockConstruct p1 p2 (HCons x xs)
        = (Just uapName, unRecord x)
        : datablockConstruct p1 p2 xs
      where
        uapName = T.pack $ symbolVal (Proxy @name)

datablockBuilder :: Integral n => n -> [URecord] -> SBuilder
datablockBuilder cat lst =
    let bldData = mconcat (unparse <$> lst)
        n = sbByteSize bldData + 3
        (n1, n2) = divMod n 256
    in word8ToSBuilder (fromIntegral cat)
     <> word8ToSBuilder (fromIntegral n1)
     <> word8ToSBuilder (fromIntegral n2)
     <> bldData

datablock :: forall dbt ts.
    ( CDatablock (IsMultiUap dbt) dbt ts
    , KnownNat (CategoryOf dbt)
    ) => HList ts -> Datablock dbt
datablock lst = Datablock $ UDatablock bld records where
    records :: [(Maybe VText, URecord)]
    records = datablockConstruct (Proxy @(IsMultiUap dbt)) (Proxy @dbt) lst

    bld :: SBuilder
    bld = datablockBuilder (natVal (Proxy @(CategoryOf dbt))) (fmap snd records)

class ToString ruleIx t where
    asString :: t -> String

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
        . Variation . unURuleVar . unRuleVar

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
        . Variation . unURuleVar . unRuleVar

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
        . Variation . unURuleVar . unRuleVar

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

type family GetVariationResult t where
    GetVariationResult ('GNonSpare name title ('GContextFree var)) = var
    GetVariationResult ('GNonSpare name title ('GDependent lst1 d lst2)) = d
    GetVariationResult t
        = TypeError ('Text "GetVariationResult unexpected argument: "
            :<>: 'ShowType t)

getVariation :: NonSpare nsp -> Variation (GetVariationResult nsp)
getVariation = Variation . unURuleVar . unUNonSpare . unNonSpare

getDepVariation :: forall ix nsp.
    ( IsSchema (DepRule nsp ix) VVariation
    ) => NonSpare nsp -> Either ParsingError (Variation (DepRule nsp ix))
getDepVariation nsp = do
    let Bits bs o1 size1 = unparse @Bits nsp
        act = parseVariation (schema @(DepRule nsp ix) Proxy)
        env = Env parsingStore bs
    (var, o2) <- runParsing act env o1
    let size2 = coerce $ o2 - o1
    when (size2 /= size1) $ Left $ ParsingError "remaining bits"
    pure $ Variation var

-- | Extract all spare items from group.
getGroupSpares :: Variation ('GGroup o lst) -> [Bits]
getGroupSpares (Variation var) = case var of
    UGroup lst -> lst >>= \case
        USpare val -> pure val
        _ -> empty
    _ -> intError

getGroupItem :: forall name o lst.
    ( KnownNat (LookupGroup name lst)
    ) => Variation ('GGroup o lst) -> NonSpare ('GGroup o lst ~> name)
getGroupItem (Variation var) = case var of
    UGroup lst ->
        let ix = fromIntegral (natVal @(LookupGroup name lst) Proxy)
            getNsp = \case
                UItem nsp -> NonSpare nsp
                _ -> intError
        in getNsp $ lst !! ix
    _ -> intError

-- | Extract all present spare items from extended.
getExtendedSpares :: Variation ('GExtended lst) -> [Bits]
getExtendedSpares (Variation var) = case var of
    UExtended _bld lst -> lst >>= \case
        Just (USpare val) -> pure val
        _ -> empty
    _ -> intError

getExtendedItem :: forall name lst.
    ( KnownNat (LookupExtended name lst)
    ) => Variation ('GExtended lst)
        -> Maybe (NonSpare ('GExtended lst ~> name))
getExtendedItem (Variation var) = case var of
    UExtended _bld lst -> do
        let n = fromIntegral $ natVal (Proxy @(LookupExtended name lst))
        guard (n < length lst)
        case lst !! n of
            Just (UItem nsp) -> Just (NonSpare nsp)
            _ -> intError
    _ -> intError

getRepetitiveItems :: Variation ('GRepetitive rt var) -> [Variation var]
getRepetitiveItems (Variation var) = case var of
    URepetitive _bld lst -> fmap Variation lst
    _ -> intError

getExplicitData :: Variation ('GExplicit mt) -> Bits
getExplicitData (Variation var) = case var of
    UExplicit _bld val -> val
    _ -> intError

getCompoundItem :: forall name lst. -- t lst ix.
    ( KnownNat (LookupCompound name lst)
    ) => Variation ('GCompound lst)
        -> Maybe (NonSpare ('GCompound lst ~> name))
getCompoundItem (Variation var) = case var of
    UCompound _bld lst ->
        let n = fromIntegral $ natVal (Proxy @(LookupCompound name lst))
        in NonSpare <$> (lst !! n)
    _ -> intError

setCompoundItem :: forall name lst.
    ( KnownNat (LookupCompound name lst)
    ) => NonSpare ('GCompound lst ~> name) -> Variation ('GCompound lst)
        -> Variation ('GCompound lst)
setCompoundItem (NonSpare nsp) (Variation var) = case var of
    UCompound _bld lst1 ->
        -- replace element in a list and rebuild
        let n = fromIntegral $ natVal (Proxy @(LookupCompound name lst))
            (a, b) = splitAt n lst1
            lst2 = a <> [Just nsp] <> tail b
            bld = rebuildCompound lst2
        in Variation (UCompound bld lst2)
    _ -> intError

delCompoundItem :: forall name lst.
    ( KnownNat (LookupCompound name lst)
    ) => Variation ('GCompound lst) -> Variation ('GCompound lst)
delCompoundItem (Variation var) = case var of
    UCompound _bld lst1 ->
        let n = fromIntegral $ natVal (Proxy @(LookupCompound name lst))
            (a, b) = splitAt n lst1
            lst2 = a <> [Nothing] <> tail b
            bld = rebuildCompound lst2
        in Variation (UCompound bld lst2)
    _ -> intError

getRecordItem :: forall name lst.
    ( KnownNat (LookupRecord name lst)
    ) => Record ('GRecord lst) -> Maybe (NonSpare ('GRecord lst ~> name))
getRecordItem (Record (URecord _bld lst)) = f <$> lst !! n
  where
    n = fromIntegral (natVal (Proxy @(LookupRecord name lst)))
    f :: RecordItem UNonSpare
        -> NonSpare (Lookup name (PrependName (RecordNonSpares lst)))
    f = \case
        RecordItem nsp -> NonSpare nsp
        _ -> intError

setRecordItem :: forall name lst.
    ( KnownNat (LookupRecord name lst)
    ) => NonSpare ('GRecord lst ~> name) -> Record ('GRecord lst)
        -> Record ('GRecord lst)
setRecordItem (NonSpare nsp) (Record (URecord _bld lst1))
    = Record (URecord bld lst2)
  where
    n = fromIntegral (natVal (Proxy @(LookupRecord name lst)))
    (a, b) = splitAt n lst1
    lst2 = a <> [Just $ RecordItem nsp] <> tail b
    bld = rebuildRecord lst2

delRecordItem :: forall name lst.
    ( KnownNat (LookupRecord name lst)
    ) => Record ('GRecord lst) -> Record ('GRecord lst)
delRecordItem (Record (URecord _bld lst1)) = Record (URecord bld lst2)
  where
    n = fromIntegral (natVal (Proxy @(LookupRecord name lst)))
    (a, b) = splitAt n lst1
    lst2 = a <> [Nothing] <> tail b
    bld = rebuildRecord lst2

getRfsItem :: forall name lst.
    ( KnownSymbol name
    , IsSchema lst [VUapItem]
    ) => Record ('GRecord lst) -> [NonSpare ('GRecord lst ~> name)]
getRfsItem (Record (URecord _bld lst)) = mconcat $ fmap f lst
  where
    frnRequired = getFrn 1 (schema @name Proxy) (schema @lst Proxy)
    f Nothing = []
    f (Just ri) = case ri of
        RecordItem _nsp -> []
        RecordItemSpare -> []
        RecordItemRFS lst2 -> do
            (frn, nsp) <- lst2
            guard $ frn == frnRequired
            pure $ NonSpare nsp

getExpansionItem :: forall name mn lst.
    ( KnownNat (LookupCompound name lst)
    ) => Expansion ('GExpansion mn lst)
        -> Maybe (NonSpare ('GExpansion mn lst ~> name))
getExpansionItem (Expansion (UExpansion _bld lst))
    = NonSpare <$> (lst !! n)
  where
    n = fromIntegral (natVal (Proxy @(LookupCompound name lst)))

-- Item access overloaded functions

class GetItem name parent child | name parent -> child where
    getItem :: parent -> child

instance
    ( GetItem name (Variation (GetVariationResult nsp1)) (NonSpare nsp2)
    ) => GetItem name (NonSpare nsp1) (NonSpare nsp2) where
    getItem obj = getItem @name $ getVariation obj

instance
    ( r ~ 'GRecord lst
    , nsp ~ Lookup name (PrependName (RecordNonSpares lst))
    , KnownNat (LookupRecord name lst)
    ) => GetItem name (Record r) (Maybe (NonSpare nsp)) where
    getItem = getRecordItem @name

instance
    ( nsp ~ 'GGroup o lst ~> name
    , KnownNat (LookupGroup name lst)
    ) => GetItem name (Variation ('GGroup o lst)) (NonSpare nsp) where
    getItem = getGroupItem @name

instance
    ( nsp ~ 'GExtended lst ~> name
    , KnownNat (LookupExtended name lst)
    ) => GetItem name (Variation ('GExtended lst)) (Maybe (NonSpare nsp)) where
    getItem = getExtendedItem @name

instance
    ( nsp ~ 'GCompound lst ~> name
    , KnownNat (LookupCompound name lst)
    ) => GetItem name (Variation ('GCompound lst)) (Maybe (NonSpare nsp)) where
    getItem = getCompoundItem @name

instance
    ( nsp ~ 'GExpansion mn lst ~> name
    , KnownNat (LookupCompound name lst)
    ) => GetItem name (Expansion ('GExpansion mn lst)) (Maybe (NonSpare nsp)) where
    getItem = getExpansionItem @name

class GetSpares t where
    getSpares :: t -> [Bits]

instance
    ( GetSpares (Variation (GetVariationResult nsp))
    ) => GetSpares (NonSpare nsp) where
    getSpares = getSpares . getVariation

instance GetSpares (Variation ('GGroup o lst)) where
    getSpares = getGroupSpares

class SetItem name parent child | name parent -> child where
    setItem :: child -> parent -> parent

instance
    ( r ~ 'GRecord lst
    , nsp ~ Lookup name (PrependName (RecordNonSpares lst))
    , KnownNat (LookupRecord name lst)
    ) => SetItem name (Record r) (NonSpare nsp) where
    setItem = setRecordItem @name

instance
    ( nsp ~ 'GCompound lst ~> name
    , KnownNat (LookupCompound name lst)
    , var ~ 'GCompound lst
    ) => SetItem name (Variation var) (NonSpare nsp) where
    setItem = setCompoundItem @name

class DelItem name parent where
    delItem :: parent -> parent

instance
    ( r ~ 'GRecord lst
    , KnownNat (LookupRecord name lst)
    ) => DelItem name (Record r) where
    delItem = delRecordItem @name

instance
    ( var ~ 'GCompound lst
    , KnownNat (LookupCompound name lst)
    ) => DelItem name (Variation var) where
    delItem = delCompoundItem @name

