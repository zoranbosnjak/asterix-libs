
-- This file will be auto-generated...

{-# LANGUAGE DataKinds #-}

module Asterix.Generated where

import Asterix.Schema

type TVariation_0 = 'TElement 0 4
type TVariation_1 = 'TElement 4 4
type TVariation_2 = 'TElement 0 16

type TItem_0 = 'TSpare 0 8
type TItem_1 = 'TItem "001" "Test item" TVariation_2

manifest :: [Some VItem]
manifest =
    [ Some $ reflect @TItem_0 Proxy
    , Some $ reflect @TItem_1 Proxy
    ]
