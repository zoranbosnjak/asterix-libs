
{-# LANGUAGE DataKinds  #-}
-- {-# LANGUAGE LambdaCase #-}

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

data Variation t
    = Element BitString
    | Group [Some Item]
    -- ... TODO
    | Compound [Maybe (Some Item)]

deriving instance Show (Variation t)
instance GShow Variation where gshowsPrec = defaultGshowsPrec

data Item t
    = Spare BitString
    | Item (Some Variation)

deriving instance Show (Item t)
instance GShow Item where gshowsPrec = defaultGshowsPrec

-- | A fancy named pair of 2 values.
-- To be used with data filtering for speedup, for example:
--   - parse into 'Augmented ByteString Record'
--   - check Record by some predicate
--   - use ByteString directly, avoid unparsing 'Record'
data Augmented s a = Augmented s a
