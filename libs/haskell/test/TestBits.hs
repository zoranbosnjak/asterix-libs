
{-# LANGUAGE LambdaCase #-}

module TestBits where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Bits

-- | For test purposes
unpack :: Bits.Bits -> [Bool]
unpack x
    | Bits.null x = []
    | otherwise = Bits.head x : unpack (Bits.drop 1 x)

propSize :: TestTree
propSize = QC.testProperty "size" $ \lst -> do
    let s :: ByteString
        s = BS.pack lst
    bitLength (mkBits s) == 8 * BS.length s

splitTest :: TestTree
splitTest = testCase "Bits split" $ do
    let bs1 = [0x01, 0x02]
        bs2 = [0x03, 0x04, 0x05]
        s = mkBits $ BS.pack (bs1 <> bs2)
        (a, b) = Bits.splitAt 16 s
    assertEqual "s" (unpack s) (join $ fmap unpackWord8 (bs1 <> bs2))
    assertEqual "a" (unpack a) (join $ fmap unpackWord8 bs1)
    assertEqual "b" (unpack b) (join $ fmap unpackWord8 bs2)

propSplit :: TestTree
propSplit = QC.testProperty "split" $ \lst -> do
    let orig :: [Bool]
        orig = join (fmap unpackWord8 lst)
        bs :: ByteString
        bs = BS.pack lst
        bits = mkBits bs
        n = bitLength bits
    ix <- choose (0, n)
    let (a, b) = Bits.splitAt ix bits
    pure
        ( unpack a == Prelude.take ix orig
       && unpack b == Prelude.drop ix orig
        )

propIndexByte :: TestTree
propIndexByte = QC.testProperty "index byte" $ do
    forAll (arbitrary `suchThat` (\lst -> length lst > 0)) $ \lst -> do
        let bs :: ByteString
            bs = BS.pack lst
            bits = mkBits bs
        ix <- choose (0, pred $ length lst)
        pure (indexByte ix bits == (lst !! ix))

numTest :: TestTree
numTest = testCase "Num" $ do
    let x = [0x01, 0x02, 0x03]
        n = 0x01*0x10000 + 0x02*0x100 + 0x03
        bits = mkBits $ BS.pack x
    assertEqual "n" (n :: Integer) (getNumberAligned bits)

testWord8 :: TestTree
testWord8 = testCase "test word8" $ do
    let x = 0x01
        lst = toByteStrings $ word8 x
    assertEqual "lst" [BS.pack [0x01]] lst

propWord8 :: TestTree
propWord8 = QC.testProperty "prop word8" $ \x -> do
    let a = word8 x
        b = word8 x
    toByteStrings a == toByteStrings b

randomList :: Arbitrary a => Int -> Gen [a]
randomList n
    | n <= 0 = pure []
    | otherwise = (:) <$> arbitrary <*> randomList (pred n)

propRoundTrip :: TestTree
propRoundTrip = QC.testProperty "round trip" $ \ins -> do
    chunkSizes' :: [Positive Int] <- arbitrary
    let chunkSizes = fmap getPositive chunkSizes'
        approxBitSize = sum chunkSizes + 8
        byteSize = div approxBitSize 8
    orig <- BS.pack <$> randomList byteSize
    let bits = mkBits orig
        bitChunks = sliceBits ins bits chunkSizes
        builderChunks = fmap fromBits bitChunks
        result = mconcat $ toByteStrings $ mconcat builderChunks
    pure (result == orig)
  where
    sliceBits ins s = \case
        [] -> [s]
        (x:xs) -> case ins of -- optionally insert empty bits, to prevent optimization
            False -> Bits.take x s : sliceBits ins (Bits.drop x s) xs
            True -> Bits.take x s : Bits.take 0 s : sliceBits ins (Bits.drop x s) xs

testConcat :: TestTree
testConcat = testCase "Concat" $ do
    do
        let b1 = BS.singleton 0xaa
            b2 = BS.singleton 0x55
        assertEqual "aligned"
            (b1 <> b2)
            (mconcat $ toByteStrings $ fromBits (mkBits b1) <> fromBits (mkBits b2))
    do
        let b1 = fromBits $ Bits.take 4 $ mkBits $ BS.singleton 0xaa
            b2 = fromBits $ Bits.drop 4 $ mkBits $ BS.singleton 0x55
        assertEqual "non-aligned"
            (BS.singleton 0xa5)
            (mconcat $ toByteStrings (b1 <> b2))
    do
        let b1 = fromBits $ Bits.take 4 $ mkBits $ BS.singleton 0xaa
            b0 = fromBits $ Bits.take 0 $ Bits.drop 4 $ mkBits $ BS.singleton 0xff
            b2 = fromBits $ Bits.drop 4 $ mkBits $ BS.singleton 0x55
        assertEqual "non-aligned-with-empty"
            (BS.singleton 0xa5)
            (mconcat $ toByteStrings (b1 <> b0 <> b2))

tests :: TestTree
tests = testGroup "Bits test"
    [ testGroup "Bits"
        [ propSize
        , splitTest
        , propSplit
        , propIndexByte
        , numTest
        ]
    , testGroup "Builder"
        [ testWord8
        , propWord8
        , propRoundTrip
        , testConcat
        ]
    ]
