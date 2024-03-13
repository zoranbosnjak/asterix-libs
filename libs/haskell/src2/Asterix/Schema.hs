
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}

module Asterix.Schema
( module Asterix.Schema
, module GHC.TypeLits
) where

import           Data.Some
import           Data.Text    (Text)
import           GHC.TypeLits

class IsSchema k t where
    schema :: t

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
        Nat -- bit offsetMod
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
