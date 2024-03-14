{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Asterix.Schema
( module Asterix.Schema
, module GHC.TypeLits
, module Data.Proxy
, module Data.Coerce
, module Data.Reflection
, module Data.Some
) where

import           Data.Coerce
import           Data.Proxy
import           Data.Reflection
import           Data.Some
import           Data.String
import           Data.Text       (Text)
import           GHC.TypeLits

newtype BitOffset = BitOffset Int
    deriving (Show)

newtype BitOffsetMod8 = BitOffsetMod8 Int
    deriving (Show)

newtype BitSize = BitSize Int
    deriving (Show)

data ExplicitType
    = ReservedExpansion
    | SpecialPurpose
    deriving (Show, Eq, Ord)

data TVariation
    = TElement
        Nat -- bit offsetMod
        Nat -- bit length
        -- TODO (TRule TContent)
    | TGroup
        [TItem]
    | TExtended
        [Maybe TItem] -- Nothing = FX bit
    | TRepetitive
        (Maybe Nat) -- header length (Nothing for FX type)
        TVariation
    | TExplicit
        (Maybe ExplicitType)
    | TCompound
        [Maybe TItem] -- Nothing = Spare bit

data TItem
    = TSpare
        Nat -- bit offsetMod8
        Nat -- bit length
    | TItem
        Symbol -- name
        Symbol -- title
        TVariation -- TODO (TRule TVariation)

data VariationType
    = VTElement
    | VTGroup
    | VTExtended
    | VTRepetitive
    | VTExplicit
    | VTCompound
    deriving (Show, Eq, Ord)

data VVariation (t :: VariationType) where
    VElement    :: BitOffsetMod8 -> BitSize -> VVariation 'VTElement
    VGroup      :: [Some VItem] -> VVariation 'VTGroup
    VExtended   :: [Maybe (Some VItem)] -> VVariation 'VTExtended
    VRepetitive :: Maybe Int -> (Some VVariation) -> VVariation 'VTRepetitive
    VExplicit   :: Maybe ExplicitType -> VVariation 'VTExplicit
    VCompound   :: [Maybe (Some VItem)] -> VVariation 'VTCompound

data ItemType
    = ITSpare
    | ITItem
    deriving (Show, Eq, Ord)

data VItem (t :: ItemType) where
    VSpare :: BitOffsetMod8 -> BitSize -> VItem 'ITSpare
    VItem  :: Text -> Text -> Some VVariation -> VItem 'ITItem

instance
    ( KnownNat o
    , KnownNat n
    ) => Reifies ('TElement o n) (VVariation 'VTElement) where
    reflect _ = VElement
        (BitOffsetMod8 $ fromIntegral $ reflect @o Proxy)
        (BitSize $ fromIntegral $ reflect @n Proxy)

instance
    ( Reifies ts [Some VItem]
    ) => Reifies ('TGroup ts) (VVariation 'VTGroup) where
    reflect _ = VGroup (reflect @ts Proxy)

instance
    ( KnownNat o
    , KnownNat n
    ) => Reifies ('TSpare o n) (VItem 'ITSpare) where
    reflect _ = VSpare
        (BitOffsetMod8 $ fromIntegral $ reflect @o Proxy)
        (BitSize $ fromIntegral $ reflect @n Proxy)

instance
    ( KnownSymbol name
    , KnownSymbol title
    , Reifies var (VVariation t)
    ) => Reifies ('TItem name title var) (VItem 'ITItem) where
    reflect _ = VItem
        (fromString $ reflect @name Proxy)
        (fromString $ reflect @title Proxy)
        (mkSome $ reflect @var Proxy)
