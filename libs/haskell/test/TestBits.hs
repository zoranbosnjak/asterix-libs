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
        [ -- TODO: fromBits, word8, toByteStrings
        ]
    ]
