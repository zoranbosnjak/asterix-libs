-- | Working with datagrams and raw datablocks

module TestDatablock where

import           Control.Monad
import           Data.ByteString as BS
import           Data.Either (isRight)
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import           Asterix.Parsing
import           Asterix.RawDatablock
import           Bits

-- Examples --

-- | Determine if input bytestring is valid asterix.
isValidAsterix :: ByteString -> Bool
isValidAsterix = isRight . runParser parseDatablocks . mkBits

-- | Parse datagram, reverse datablocks, convert to builder
reverseDatablocks :: ByteString -> Either AsterixError Builder
reverseDatablocks
    = fmap (mconcat . fmap toBuilder . Prelude.reverse . checkRemaining)
    . runParser parseDatablocks
    . mkBits
  where
    checkRemaining :: ([Datablock Bits], Bits) -> [Datablock Bits]
    checkRemaining (lst, r)
        | Bits.null r = lst
        | otherwise = error "unexpected remaining bits"
    toBuilder :: Datablock Bits -> Builder
    toBuilder = fromBits . unDatablock

-- | Filter datablocks by some predicate
filterDatablocks :: (Datablock Bits -> Bool) -> ByteString -> Either AsterixError Builder
filterDatablocks predicate
    = fmap (mconcat . fmap toBuilder . Prelude.filter predicate)
    . runParser' parseDatablocks
    . mkBits
  where
    toBuilder :: Datablock Bits -> Builder
    toBuilder = fromBits . unDatablock

-- | Concrete datablock filter.
onlyCat062 :: ByteString -> Either AsterixError Builder
onlyCat062 = filterDatablocks (\db -> dbCategory db == (62::Int))

-- | Generate datagram from raw bytes.
generateDatagram :: Int -> ByteString -> [ByteString]
generateDatagram cat
    = toByteStrings
    . unDatablock
    . mkDatablock cat
    . fromBits
    . mkBits

-- | Group records from multiple datagrams
multiGroupRecords :: Bool -> [ByteString] -> Either AsterixError [ByteString]
multiGroupRecords allowReorder
    = fmap (toByteStrings . mconcat . fmap unDatablock . groupRecords allowReorder . join)
    . sequence
    . fmap (runParser' parseDatablocks . mkBits)

-- Tests --

-- | Every generated datablock must be valid
propValid :: TestTree
propValid = QC.testProperty "valid" $ \cat lst -> do
    let bs = mconcat $ generateDatagram cat $ BS.pack lst
    isValidAsterix bs

-- | Adding 1 or 2 bytes to a valid datablock makes it invalid.
propInvalid :: TestTree
propInvalid = QC.testProperty "invalid" $ \cat lst err -> do
    let bs1 = mconcat $ generateDatagram cat $ BS.pack lst
        bs2 = case err of
            Left b -> BS.singleton b
            Right (b1, b2) -> BS.singleton b1 <> BS.singleton b2
    not $ isValidAsterix (bs1 <> bs2)

testReverse :: TestTree
testReverse = testCase "reverse" $ do
    let bs = [dg "test1", dg "test2", dg "test3"]
    assertEqual "reverse"
        (mconcat $ Prelude.reverse bs)
        (render $ reverseDatablocks (mconcat bs))
  where
    dg = mconcat . generateDatagram 1
    render = either (error . show) (mconcat . toByteStrings)

testFilter :: TestTree
testFilter = testCase "filter" $ do
    let bs61 = dg 61 "test"
        bs62 = dg 62 "test"
        bs63 = dg 63 "test"
    assertEqual "filter" bs62 (render $ onlyCat062 (bs61 <> bs62 <> bs63))
  where
    dg cat = mconcat . generateDatagram cat
    render = either (error . show) (mconcat . toByteStrings)

testGroupRecords :: TestTree
testGroupRecords = testCase "group" $ do
    let bs = [dg 1 "R11R12", dg 1 "R13", dg 2 "R2", dg 1 "R14"]
    assertEqual "allow reorder"
        (mconcat [dg 1 "R11R12R13R14", dg 2 "R2"])
        (render $ multiGroupRecords True bs)
    assertEqual "no reorder"
        (mconcat [dg 1 "R11R12R13", dg 2 "R2", dg 1 "R14"])
        (render $ multiGroupRecords False bs)
  where
    dg cat = mconcat . generateDatagram cat
    render = either (error . show) mconcat

tests :: TestTree
tests = testGroup "Datablock test"
    [ propValid
    , propInvalid
    , testReverse
    , testFilter
    , testGroupRecords
    ]
