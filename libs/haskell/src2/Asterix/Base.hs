
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Asterix.Base where

import           Data.ByteString
import           Data.Some

import           Asterix.Schema

data Bits = Bits
    { bitsData   :: !ByteString
    , bitsOffset :: !BitOffset
    , bitsSize   :: !BitSize
    }

data Variation t where
    Element :: Bits -> Variation ('TElement o n)
    Group :: [Some Item] -> Variation ('TGroup ts)
    Extended :: [Some Item] -> Variation ('TExtended ts)
    Repetitive :: [Variation t2] -> Variation ('TRepetitive mn t2)
    Explicit :: ByteString -> Variation ('TExplicit met)
    Compound :: [Maybe (Some Item)] -> Variation ('TCompound ts)

data Item t where
    Spare :: Bits -> Item ('TSpare o n)
    Item :: Variation t -> Item ('TItem name title t)

variationSize :: Variation t -> BitSize
variationSize = \case
    _ -> undefined

itemSize :: Item t -> BitSize
itemSize = \case
    Spare b -> bitsSize b
    Item var -> variationSize var
