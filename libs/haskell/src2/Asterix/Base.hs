
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Asterix.Base
( module Asterix.Base
, module Asterix.Schema
) where

import           Data.ByteString
import           Data.Some
import           Data.GADT.Show

import           Asterix.Schema

data BitString = BitString
    { bsData   :: !ByteString
    , bsOffset :: !BitOffset
    , bsSize   :: !BitSize
    } deriving (Show)

data Variation t where
    Element :: BitString -> Variation ('TElement o n)
    Group :: [Some Item] -> Variation ('TGroup ts)
    Extended :: [Some Item] -> Variation ('TExtended ts)
    Repetitive :: [Variation t2] -> Variation ('TRepetitive mn t2)
    Explicit :: ByteString -> Variation ('TExplicit met)
    Compound :: [Maybe (Some Item)] -> Variation ('TCompound ts)

deriving instance Show (Variation t)
instance GShow Variation where gshowsPrec = defaultGshowsPrec

data Item t where
    Spare :: BitString -> Item ('TSpare o n)
    Item :: Variation t -> Item ('TItem name title t)

deriving instance Show (Item t)
instance GShow Item where gshowsPrec = defaultGshowsPrec

variationSize :: Variation t -> BitSize
variationSize = \case
    _ -> undefined

itemSize :: Item t -> BitSize
itemSize = \case
    Spare b -> bsSize b
    Item var -> variationSize var
