
-- This file will be auto-generated...

{-# LANGUAGE DataKinds #-}

module Asterix.Generated where

import Asterix.Schema

type TVariation_0 = 'TElement 0 8
type TVariation_1 = 'TElement 1 3
type TVariation_2 = 'TElement 2 2
type TVariation_3 = 'TGroup '[ TItem_0, TItem_1]
type TVariation_4 = 'TCompound
    '[ 'Just TNonSpare_0, 'Nothing, 'Just TNonSpare_2, 'Just TNonSpare_3]

type TNonSpare_0 = 'TNonSpare "SAC" "Sac" TVariation_0
type TNonSpare_1 = 'TNonSpare "SIC" "Sic" TVariation_0
type TNonSpare_2 = 'TNonSpare "010" "Sac/Sic" TVariation_3
type TNonSpare_3 = 'TNonSpare "020" "Test" TVariation_0
type TNonSpare_4 = 'TNonSpare "030" "Test" TVariation_4

type TItem_0 = 'TItem TNonSpare_0
type TItem_1 = 'TItem TNonSpare_1
type TItem_2 = 'TItem TNonSpare_3
type TItem_3 = 'TSpare 0 8
type TItem_4 = 'TItem TNonSpare_4

items :: [VItem]
items =
    [ reflect @TItem_0 Proxy
    , reflect @TItem_1 Proxy
    , reflect @TItem_2 Proxy
    , reflect @TItem_3 Proxy
    , reflect @TItem_4 Proxy
    ]
