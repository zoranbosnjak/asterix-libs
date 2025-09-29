{-# LANGUAGE LambdaCase #-}

module TestBits (tests) where

import qualified Data.ByteString       as BS

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Asterix.BitString

tests :: TestTree
tests = testGroup "Bits"
    [ testBitsConversion
    , testBitsNumConversion
    , testBitsDebug
    , testBsToBools
    , testIntToBools
    , testIntToBoolsProp
    , testBitsBoolsProp
    , testBitsAppend
    , testBitsAppendProp
    ]

testBitsConversion :: TestTree
testBitsConversion = QC.testProperty "conversion prop" $ \lst ->
    let bs1 = BS.pack lst
        bs2 = builderToByteStringSlow
            $ bitsToBuilder
            $ byteStringToBits bs1
    in bs1 == bs2

testBitsNumConversion :: TestTree
testBitsNumConversion = QC.testProperty "num conversion" $ \o lst ->
    let o8 = mod (getNonNegative o) 8
        s = boolsToBits o8 lst
        n = sum $ zipWith f [0..] (reverse lst)
    in bitsToNum s == n
  where
    f i = \case
        False -> 0
        True -> (2 :: Integer) ^ (i :: Int)

testBitsDebug :: TestTree
testBitsDebug = testGroup "debugBits" (fmap check samples)
  where
    check (ix::Int, (x, y)) = testCase ("bits-" <> show ix) $
        debugBits x @?= y
    samples = zip [1..]
        [ (byteStringToBits $ BS.pack [], "")
        , (byteStringToBits $ BS.pack [0xa5], "10100101")
        , (byteStringToBits $ BS.pack [0xa5, 0x12], "10100101 00010010")
        , (integerToBits 0 8 0x5a, "01011010")
        , (integerToBits 1 8 0x5a, ".0101101 0.......")
        , (integerToBits 2 8 0x5a, "..010110 10......")
        , (integerToBits 2 7 0x5a, "..101101 0.......")
        , (integerToBits 2 6 0x5a, "..011010")
        , (integerToBits 1 6 0x5a, ".011010.")
        , (integerToBits 0 1 0x01, "1.......")
        , (integerToBits 1 1 0x01, ".1......")
        , (integerToBits 2 1 0x01, "..1.....")
        , (integerToBits 2 1 0x00, "..0.....")
        , (integerToBits 0 16 0xff00, "11111111 00000000")
        , (integerToBits 0 16 0x00ff, "00000000 11111111")
        , (integerToBits 1 16 0x00ff, ".0000000 01111111 1.......")
        ]

testBsToBools :: TestTree
testBsToBools = testGroup "BsToBools" (fmap check samples)
  where
    check (x, y) = testCase (hexlify $ BS.pack x) $
        bitsToBools (byteStringToBits $ BS.pack x) @?= y
    samples =
        [ ([], [])
        , ([0], [False, False, False, False, False, False, False, False])
        , ([0x5a], [False, True, False, True, True, False, True, False])
        , ([1,2],
            [ False, False, False, False, False, False, False, True
            , False, False, False, False, False, False, True, False
            ])
        ]

testIntToBools :: TestTree
testIntToBools = testGroup "IntToBools" (fmap check samples)
  where
    check (o, n, x, lst) = testCase (show (o, n, x)) $
        bitsToBools (integerToBits o n x) @?= lst
    samples =
        [ (0, 0, 0, [])
        , (0, 8, 0x00, [False, False, False, False, False, False, False, False])
        , (0, 8, 0x5a, [False, True, False, True, True, False, True, False])
        , (1, 8, 0x5a, [False, True, False, True, True, False, True, False])
        , (1, 7, 0x01, [False, False, False, False, False, False, True])
        , (1, 7, 0x5a, [True, False, True, True, False, True, False])
        , (0, 8, 0xff, [True, True, True, True, True, True, True, True])
        ]

testIntToBoolsProp :: TestTree
testIntToBoolsProp = QC.testProperty "intToBitsProp" $ \(NonNegative n) ->
    forAll (elements [0..7]) $ \o ->
        forAll (arbitrary `suchThat` (\val -> (Prelude.length val * 8) >= n)) $ \lst ->
            let x = BS.foldl (\a w -> a * 256 + fromIntegral w) 0 (BS.pack lst)
                b = integerToBits o n x
                i = mconcat (fmap word8ToBools lst)
            in bitsToBools b
                === reverse (take n $ reverse i)

testBitsBoolsProp :: TestTree
testBitsBoolsProp = QC.testProperty "Bits Bools prop" $ \lst ->
    forAll (elements [0..7]) $ \o8 ->
        bitsToBools (boolsToBits o8 lst) == lst

testBitsAppend :: TestTree
testBitsAppend = testGroup "append" (fmap check samples)
  where
    (<+>) = appendBits
    check (ix::Int, (x, y)) = testCase ("test-" <> show ix) $
        debugBits x @?= debugBits y
    samples = zip [1..]
        [ (integerToBits 0 0 0 <+> integerToBits 0 0 0, integerToBits 0 0 0)
        , (integerToBits 0 8 0x12 <+> integerToBits 0 8 0x34, integerToBits 0 16 0x1234)
        , (integerToBits 0 8 0x12 <+> integerToBits 0 12 0x345, integerToBits 0 20 0x12345)
        , (integerToBits 0 4 0xa <+> integerToBits 4 4 0x5, integerToBits 0 8 0xa5)
        , (integerToBits 1 2 1 <+> integerToBits 3 4 1, integerToBits 1 6 0x11)
        ]

testBitsAppendProp :: TestTree
testBitsAppendProp = QC.testProperty "append prop" $ \lst1 lst2 ->
    forAll (elements [0..7]) $ \o8 ->
    let b1 = boolsToBits o8 lst1
        b2 = boolsToBits (rightAlignment b1) lst2
        b = boolsToBits o8 (lst1 <> lst2)
    in appendBits b1 b2 == b

