
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

data TVariation
    = TElement
        Nat -- bit offsetMod
        Nat -- bit length
        -- TODO (TRule TContent)
    | TGroup
        [TItem]
    -- ... TODO
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

data VVariation
    = VElement BitOffsetMod8 BitSize
    | VGroup [VItem]
    -- ... TODO
    | VCompound [Maybe VItem]
    deriving (Show)

data VItem
    = VSpare BitOffsetMod8 BitSize
    | VItem Text Text VVariation
    deriving (Show)

instance
    ( KnownNat o
    , KnownNat n
    ) => Reifies ('TElement o n) VVariation where
    reflect _ = VElement
        (BitOffsetMod8 $ fromIntegral $ reflect @o Proxy)
        (BitSize $ fromIntegral $ reflect @n Proxy)

instance Reifies '[] [VItem] where
    reflect _ = []

instance
    ( Reifies t VItem
    , Reifies ts [VItem]
    ) => Reifies (t ': ts) [VItem] where
    reflect _ = reflect @t Proxy : reflect @ts Proxy

instance Reifies 'Nothing (Maybe VItem) where
    reflect _ = Nothing

instance
    ( Reifies t VItem
    ) => Reifies ('Just t) (Maybe VItem) where
    reflect _ = Just (reflect @t Proxy)

instance Reifies '(Maybe, '[]) [Maybe VItem] where
    reflect _ = []

instance
    ( Reifies t (Maybe VItem)
    , Reifies '(Maybe, ts) [Maybe VItem]
    ) => Reifies '(Maybe, t ': ts) [Maybe VItem] where
    reflect _ = reflect @t Proxy : reflect @'(Maybe, ts) Proxy

instance
    ( Reifies ts [VItem]
    ) => Reifies ('TGroup ts) VVariation where
    reflect _ = VGroup (reflect @ts Proxy)

-- ... TODO other T*...

instance
    ( Reifies '(Maybe, ts) [Maybe VItem]
    ) => Reifies ('TCompound ts) VVariation where
    reflect _ = VCompound (reflect @'(Maybe, ts) Proxy)

instance
    ( KnownNat o
    , KnownNat n
    ) => Reifies ('TSpare o n) VItem where
    reflect _ = VSpare
        (BitOffsetMod8 $ fromIntegral $ reflect @o Proxy)
        (BitSize $ fromIntegral $ reflect @n Proxy)

instance
    ( KnownSymbol name
    , KnownSymbol title
    , Reifies var VVariation
    ) => Reifies ('TItem name title var) VItem where
    reflect _ = VItem
        (fromString $ reflect @name Proxy)
        (fromString $ reflect @title Proxy)
        (reflect @var Proxy)
