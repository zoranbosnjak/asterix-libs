
-- This file will be auto-generated...

{-# LANGUAGE DataKinds #-}

module Asterix.Generated where

import Asterix.Schema

type TVariation_0 = 'TElement 0 2
type TVariation_1 = 'TElement 6 6
type TVariation_2 = 'TElement 0 16
type TVariation_3 = 'TGroup '[ TItem_0, TItem_1]
type TVariation_4 = 'TCompound '[ 'Just TNonSpare_0, 'Nothing, 'Just TNonSpare_1]

type TNonSpare_0 = 'TNonSpare "001" "Test item" TVariation_0
type TNonSpare_1 = 'TNonSpare "002" "Group test" TVariation_3
type TNonSpare_2 = 'TNonSpare "003" "Compound test" TVariation_4

type TItem_0 = 'TSpare 0 8
type TItem_1 = 'TItem TNonSpare_0
type TItem_2 = 'TItem TNonSpare_1
type TItem_3 = 'TItem TNonSpare_2

manifest :: [VItem]
manifest =
    [ reflect @TItem_0 Proxy
    , reflect @TItem_1 Proxy
    , reflect @TItem_2 Proxy
    , reflect @TItem_3 Proxy
    ]
