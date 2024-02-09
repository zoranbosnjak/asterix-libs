-- | Generic/dynamic usecase
--
-- Specification is selected at run-time
--
-- For generic manipulation, subitem names are normally not explicitely
-- mentioned in the source code (decoder, convert to json, random samples...).
--
-- However, subitems can still be selected at run-time by item name (Text),
-- with the possible run-time failure if item is not actually defined.

module TestGeneric where

import Test.Tasty
-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.HUnit

-- Examples --

-- Example: inspect (isValid)

-- Example: decode to text

-- Example: convert to json/xml...

-- Example: generate full/random samples for a given spec

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

tests :: TestTree
tests = testGroup "Generic asterix tests"
    [
    ]
