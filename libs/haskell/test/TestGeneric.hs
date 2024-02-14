-- | Generic/dynamic usecase
--
-- Specification is selected at run-time
--
-- For generic manipulation, subitem names are normally not explicitely
-- mentioned in the source code (decoder, convert to json, random samples...).
--
-- However, subitems can still be selected at run-time by item name (Text),
-- with the possible run-time failure if item is not actually defined.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- TODO: remove this
-- {-# OPTIONS_GHC -Wno-all #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module TestGeneric where

import Data.Map
import Data.ByteString
import Test.Tasty
-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.HUnit

import BitString as B
import BitString.Builder as BB
import Asterix.Base
import Asterix.Schema
--import Generated

-- Examples --

{-
-- | Generate sample datagram, of all categories, all data bits set to 'val'
sampleDatagram :: Bool -> [(Int, VUap)] -> Map Int (VVariation 0 0) -> Builder 0 0
sampleDatagram val cats _refs
    = toByteStrings $ mconcat $ undefined -- fmap (unDatablock . genCat) cats
  where
    {-
    genCat :: (Int, VUap) -> UDatablock Builder
    genCat (cat, uap) = case uap of
        VUapSingle vvar -> mkUDatablockSingle cat
            [genRecord vvar]
        VUapMultiple lst -> mkUDatablockMultiple cat
            [(n, genRecord vvar) | (n, vvar) <- lst]

    genRecord :: VVariation -> URecord Builder
    genRecord = URecord . genVariation

    genVariation :: VVariation -> UVariation Builder
    genVariation = \case
        VElement o n vrule ->
            let bs = mkElement o n 0
            in Element bs vrule
        VGroup lst ->
            let items = fmap genItem lst
            in Group (mkGroup items) items
        VExtended lst ->
            let items = fmap (fmap genItem) lst
            in Extended (mkExtended items) items
        VRepetitive rt vvar ->
            let vars = Prelude.replicate 4 (genVariation vvar)
            in Repetitive (mkRepetitive rt vars) rt vars
        VExplicit met -> case met of
            Nothing ->
                Explicit (mkExplicit mempty) met mempty
            Just ReservedExpansion ->
                undefined
            Just SpecialPurpose ->
                undefined
                -- Explicit (mkExplicit "test") met
        VCompound mn lst -> undefined
        VRandomFieldSequencing -> undefined

    genItem :: VItem -> UItem Builder
    genItem = \case
        VSpare o n ->
            let bs = fromBits $ fromUInteger o n 0
            in USpare bs
        VItem name title vvar ->
            UItem name title (genVariation vvar)
-}

{-
-- | Try to parse, check if valid
isValidAsterix :: Map Int VUap -> Map Int VVariation -> ByteString -> Bool
isValidAsterix _cats _refs _s = undefined

-- Example: decode to text

-- Example: convert to json/xml...

-- Example: same example as in AppSpecific, but with run-time (init time)
-- instead of compile time error (create Focus at init stage)

-- Example: create single record datablocks, that is:
-- If a datablock contains multiple records, create multiple datablocks.
-- Use complete category: editions mapping, try all editions in turn and
-- use the first (newest) edition for which the parsing is successfull.
-- Fail if no matching edition is found.
--convertToSingleRecordDatablocks :: ByteString -> Maybe Builder
--convertToSingleRecordDatablocks = parse >=> undefined

-- Tests --

-- golden test for sample datagram
-- sample datagram must be valid

tests :: TestTree
tests = testGroup "Generic asterix tests"
    [
    ]
-}
-}
