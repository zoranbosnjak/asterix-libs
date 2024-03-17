
-- This file will be auto-generated...

{-# LANGUAGE DataKinds #-}

module Asterix.Generated where

import Asterix.Schema

type TVariation_0 = 'TElement 0 4
type TVariation_1 = 'TElement 4 4
type TVariation_2 = 'TElement 0 16
type TVariation_3 = 'TGroup '[ TItem_0, TItem_1]
type TVariation_4 = 'TCompound '[ 'Just TItem_1, 'Nothing, 'Just TItem_2]

type TItem_0 = 'TSpare 0 8
type TItem_1 = 'TItem "001" "Test item" TVariation_2
type TItem_2 = 'TItem "002" "Group test" TVariation_3
type TItem_3 = 'TItem "003" "Compound test" TVariation_4

manifest :: [VItem]
manifest =
    [ reflect @TItem_0 Proxy
    , reflect @TItem_1 Proxy
    , reflect @TItem_2 Proxy
    , reflect @TItem_3 Proxy
    ]
