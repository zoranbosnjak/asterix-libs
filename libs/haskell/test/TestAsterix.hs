-- asterix item manipulation unit tests

-- Remark: Keep asterix test scenarios synchronized between implementations.

{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module TestAsterix (tests) where

import           Control.Monad
import qualified Data.ByteString    as BS
import           Data.Either
import           Data.Function      ((&))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Prelude            hiding (head, tail)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Asterix.Base
import           Asterix.Coding

import           Common
import           Generated          as Gen

tests :: TestTree
tests = testGroup "Asterix"
    [ testCase "create" testCreate
    , testCase "ruleVar context free" testRuleVariationContextFree
    , testCase "ruleVar dependent" testRuleVariationDependent
    , testCase "ruleCont context free" testRuleContentContextFree
    , testCase "ruleCont dependent1" testRuleContentDependent1
    , testCase "ruleCont dependent2" testRuleContentDependent2
    , testCase "contentRaw" testContentRaw
    , testCase "contentTable" testContentTable
    , testCase "contentStringAscii" testContentStringAscii
    , testCase "contentStringICAO" testContentStringICAO
    , testCase "contentStringOctal" testContentStringOctal
    , testCase "contentIntegerUnsigned" testContentIntegerUnsigned
    , testCase "contentIntegerSigned" testContentIntegerSigned
    , testCase "contentQuantityUnsigned" testContentQuantityUnsigned
    , testCase "contentQuantitySigned" testContentQuantitySigned
    , testCase "contentBdsWithAddress" testContntBdsWithAddress
    , testCase "testGroup1" testGroup1
    , testCase "testGroup2" testGroup2
    , testCase "testGroup3" testGroup3
    , testCase "testExtended1" testExtended1
    , testCase "testExtended2" testExtended2
    , testCase "testExtended3" testExtended3
    , testCase "testExtended4" testExtended4
    , testCase "testExtended5" testExtended5
    , testCase "testRepetitive1" testRepetitive1
    , testCase "testRepetitive2" testRepetitive2
    , testCase "testRepetitive3" testRepetitive3
    , testCase "testExplicit0" testExplicit0
    , testCase "testExplicit1" testExplicit1
    , testCase "testExplicit2" testExplicit2
    , testCase "testExplicit3a" testExplicit3a
    , testCase "testExplicit3b" testExplicit3b
    , testCase "testExplicit3c" testExplicit3c
    , testCase "testCompound0" testCompound0
    , testCase "testCompound1" testCompound1
    , testCase "testCompoundSet" testCompoundSet
    , testCase "testCompoundDel" testCompoundDel
    , testCase "testRecordEmpty" testRecordEmpty
    , testCase "testRecord0RFS" testRecord0RFS
    , testCase "testRecord1RFS" testRecord1RFS
    , testCase "testRecordMultipleRFS" testRecordMultipleRFS
    , testCase "testRecordSetItem" testRecordSetItem
    , testCase "testRecordDelItem" testRecordDelItem
    , testCase "testMultipleUAPS" testMultipleUAPS
    , testCase "testCreateDatagram" testCreateDatagram
    , testCase "testParse1" testParse1
    , testCase "testParse2" testParse2
    , testCase "testParse3" testParse3
    , testCase "testParse4" testParse4
    , testCase "testParse5" testParse5
    , testCase "testParse6" testParse6
    , testCase "testParseNonblocking" testParseNonblocking
    ]

testCreate :: Assertion
testCreate = do
    let _ = record nil :: Record (RecordOf Cat_000_1_0)
        _ = record ( item @"010" 1 *: nil) :: Record (RecordOf Cat_000_1_0)
        _ = record ( item @"000" 1 *: item @"010" 2 *: nil)
            :: Record (RecordOf Cat_000_1_0)
        -- The following line (if uncommented) shall not compile
        -- _ = record ( item @"nonexistingitem" 1 : nil) :: Record (RecordOf Cat_000_1_0)
    pure ()

testRuleVariationContextFree :: Assertion
testRuleVariationContextFree = do
    let obj :: NonSpare (Cat_000_1_0 ~> "000") = 0x01
    checkBits "i000" obj (Hex "01")
    assertUint 1 obj

testRuleVariationDependent :: Assertion
testRuleVariationDependent = do
    let obj :: Variation (DepRule (Cat_000_1_0 ~> "032" ~> "CC" ~> "CP") '[ 1, 2]) = 4
    assertUint 4 obj
    -- create complete record with that object
    let rec1 :: Record (RecordOf Cat_000_1_0)
        rec1 = record
            ( item @"032" ( compound
                ( item @"CC" ( group
                    ( item @"TID" 0
                   *: item @"CP" (fromInteger $ asUint obj)
                   *: item @"CS" 0
                   *: nil))
               *: nil))
           *: nil)
        -- try to unparse and parse the record
        bs = toByteString $ unparse @SBuilder rec1
        act = parseRecord @StrictParsing (schema @(RecordOf Cat_000_1_0) Proxy)
        result = parse act bs
    urec2 <- either (assertFailure . show) pure result
    let rec2 :: Record (RecordOf Cat_000_1_0) = Record urec2
        i032 = fromJust $ getItem @"032" rec2
        itemCC = fromJust $ getItem @"CC" $ getVariation i032
        itemCP = getItem @"CP" $ getVariation itemCC
    -- try to cast to all variation cases
    let fromRight' = \case
            Left (ParsingError e) -> error (show e)
            Right val -> val
        var0 = getVariation itemCP
        var1 = fromRight' $ getDepVariation @'[ 1, 1] itemCP
        var2 = fromRight' $ getDepVariation @'[ 1, 2] itemCP
        var3 = fromRight' $ getDepVariation @'[ 2, 1] itemCP
    assertUint 4 var0
    assertUint 4 var1
    assertUint 4 var2
    assertUint 4 var3
    assertUint 1 (getItem @"I1" var3)
    assertEqual "spares" [0::Integer] (bitsToNum <$> getSpares var3)

testRuleContentContextFree :: Assertion
testRuleContentContextFree = do
    let obj :: NonSpare (Cat_000_1_0 ~> "030" ~> "IM") = 1
    assertUint 1 obj

testRuleContentDependent1 :: Assertion
testRuleContentDependent1 = do
    let obj :: NonSpare (Cat_000_1_0 ~> "030" ~> "IAS") = 1
    assertUint 1 obj
    let obj1 :: NonSpare (Cat_000_1_0 ~> "030" ~> "IAS")
        obj1 = quantity @"NM/s" @('Just 0) 1.2
    assertApproximately "conv1" 0.01 1.2 (asQuantity @"NM/s" @('Just 0) obj1)
    let obj2 :: NonSpare (Cat_000_1_0 ~> "030" ~> "IAS")
        obj2 = quantity @"Mach" @('Just 1) 0.8
    assertApproximately "conv2" 0.01 0.8 (asQuantity @"Mach" @('Just 1) obj2)

testRuleContentDependent2 :: Assertion
testRuleContentDependent2 = do
    let obj :: NonSpare (Cat_000_1_0 ~> "031") = 1
    assertUint 1 obj
    let obj0 :: NonSpare (Cat_000_1_0 ~> "031")
        obj0 = quantity @"unit1" @('Just 0) 12.3
    assertApproximately "obj0" 0.02 12.3 (asQuantity @"unit1" @('Just 0) obj0)
    let obj1 :: NonSpare (Cat_000_1_0 ~> "031")
        obj1 = quantity @"unit2" @('Just 1) 13.4
    assertApproximately "obj1" 0.01 13.4 (asQuantity @"unit2" @('Just 1) obj1)
    let obj2 :: NonSpare (Cat_000_1_0 ~> "031")
        obj2 = quantity @"unit3" @('Just 2) 10.0
    assertApproximately "obj2" 0.01 10.0 (asQuantity @"unit3" @('Just 2) obj2)

testContentRaw :: Assertion
testContentRaw = do
    let obj :: NonSpare (Cat_000_1_0 ~> "010" ~> "SAC") = 0x01
    assertUnparse "01" obj
    assertUint 1 obj

testContentTable :: Assertion
testContentTable = do
    let obj :: NonSpare (Cat_000_1_0 ~> "000") = 0x01
    assertUnparse "01" obj
    assertUint 1 obj

testContentStringAscii :: Assertion
testContentStringAscii = do
    let obj1 :: NonSpare (Cat_000_1_0 ~> "020" ~> "S1") = 0x01
    assertUnparse "00000000000001" obj1
    assertUint 1 obj1
    let sample = "test"
        obj2 :: NonSpare (Cat_000_1_0 ~> "020" ~> "S1") = string sample
    assertUnparse "74657374202020" obj2
    assertUint 0x74657374202020 obj2
    assertEqual "string" sample (rStrip $ asString obj2)

testContentStringICAO :: Assertion
testContentStringICAO = do
    let obj1 :: NonSpare (Cat_000_1_0 ~> "020" ~> "S2") = 0x01
    assertUint 1 obj1
    assertUnparse "000000000001" obj1
    let sample = "S5LJ"
        obj2 :: NonSpare (Cat_000_1_0 ~> "020" ~> "S2") = string sample
    assertUnparse "4F530A820820" obj2
    assertUint 0x4F530A820820 obj2
    assertEqual "string" sample (rStrip $ asString obj2)

testContentStringOctal :: Assertion
testContentStringOctal = do
    let obj1 :: NonSpare (Cat_000_1_0 ~> "020" ~> "S3") = 0x01
    assertUnparse "000001" obj1
    assertUint 1 obj1
    let sample = "1234"
        obj2 :: NonSpare (Cat_000_1_0 ~> "020" ~> "S3") = string sample
    assertUnparse "29C000" obj2
    assertUint 0x29C000 obj2
    assertEqual "string" (sample <> "0000") (asString obj2)

testContentIntegerUnsigned :: Assertion
testContentIntegerUnsigned = do
    let obj1 :: NonSpare (Cat_000_1_0 ~> "020" ~> "I1") = 0x01
    assertUnparse "01" obj1
    assertUint 1 obj1
    let obj2 :: NonSpare (Cat_000_1_0 ~> "020" ~> "I1") = -1
    assertUnparse "FF" obj2
    assertUint 255 obj2
    assertEqual "integer" (255::Integer) (asInteger obj2)

testContentIntegerSigned :: Assertion
testContentIntegerSigned = do
    let obj1 :: NonSpare (Cat_000_1_0 ~> "020" ~> "I2") = 0x01
    assertUnparse "01" obj1
    let obj2 :: NonSpare (Cat_000_1_0 ~> "020" ~> "I2") = -1
    assertUnparse "FF" obj2
    assertUint 0xff obj2
    assertEqual "integer" ((-1)::Integer) (asInteger obj2)

testContentQuantityUnsigned :: Assertion
testContentQuantityUnsigned = do
    let obj1 :: NonSpare (Cat_000_1_0 ~> "020" ~> "Q3") = 0x01
    assertUnparse "0001" obj1
    let sample = 123.4
        obj2 :: NonSpare (Cat_000_1_0 ~> "020" ~> "Q3") = quantity sample
    assertUint 0x007B obj2
    assertUnparse "007B" obj2
    let obj3 :: NonSpare (Cat_000_1_0 ~> "020" ~> "Q3") = quantity @"kt" sample
    assertUnparse "007B" obj3
    assertEqual "conv1"
        ((round $ unQuantity sample) :: Integer)
        (round $ unQuantity $ asQuantity obj3)
    assertEqual "conv1"
        ((round $ unQuantity sample) :: Integer)
        (round $ unQuantity $ asQuantity @"kt" obj3)

testContentQuantitySigned :: Assertion
testContentQuantitySigned = do
    let obj1 :: NonSpare (Cat_000_1_0 ~> "020" ~> "Q2LON") = 0x01
        one = asQuantity obj1
    assertUnparse "000001" obj1
    let sample = 123.4
        obj2 :: NonSpare (Cat_000_1_0 ~> "020" ~> "Q2LON") = quantity sample
    assertEqual "conv1"
        (round (unQuantity sample / (180 / (2 ^ (23::Int)))))
        (asUint @Integer obj2)
    let obj3 :: NonSpare (Cat_000_1_0 ~> "020" ~> "Q2LON") = quantity @"°" sample
    assertEqual "conv1"
        (round (unQuantity sample / (180 / (2 ^ (23::Int)))))
        (asUint @Integer obj3)
    assertEqual "conv2"
        ((round $ unQuantity sample) :: Integer)
        (round $ unQuantity $ asQuantity obj3)
    assertEqual "conv2"
        ((round $ unQuantity sample) :: Integer)
        (round $ unQuantity $ asQuantity @"°" obj3)
    let obj4 :: NonSpare (Cat_000_1_0 ~> "020" ~> "Q2LON") = 0xffffff
    assertUnparse "ffffff" obj4
    assertEqual "compare1" LT (compare (asQuantity obj4) 0)
    assertEqual "compare2" EQ (compare (asQuantity obj4) (-one))

testContntBdsWithAddress :: Assertion
testContntBdsWithAddress = do
    let obj :: NonSpare (Cat_000_1_0 ~> "020" ~> "B1") = 0x01
    assertUnparse "0000000000000001" obj
    assertUint 0x01 obj

testGroup1 :: Assertion
testGroup1 = do
    let obj1 :: NonSpare (Cat_000_1_0 ~> "010") = 0x0102
    assertUnparse "0102" obj1
    assertUint 0x0102 obj1
    let obj2 :: NonSpare (Cat_000_1_0 ~> "010") = group ( 0x01 *: 0x02 *: nil)
    assertUnparse "0102" obj2
    assertUint 0x0102 obj2
    let obj3 :: NonSpare (Cat_000_1_0 ~> "010") = group
            ( item @"SAC" 0x01
           *: 0x02
           *: nil)
    assertUnparse "0102" obj3
    assertUint 0x0102 obj3
    let obj4 :: NonSpare (Cat_000_1_0 ~> "010") = group
            ( item @"SAC" 0x01
           *: item @"SIC" 0x02
           *: nil)
    assertUnparse "0102" obj4
    assertUint 0x0102 obj4
    assertUint 0x01 (getItem @"SAC" obj4)
    assertUint 0x02 (getItem @"SIC" obj4)
    assertEqual "spares" [] (getSpares obj4)

testGroup2 :: Assertion
testGroup2 = do
    let obj :: NonSpare (Cat_000_1_0 ~> "040") = group
            ( 1 *: 2 *: item @"I2" 3 *: 4 *: nil)
    assertUnparse "030604" obj
    assertUint 1 (getItem @"I1" obj)
    assertUint 3 (getItem @"I2" obj)
    assertEqual "spares" [2::Integer, 4] (bitsToNum <$> getSpares obj)

testGroup3 :: Assertion
testGroup3 = do
    let obj :: NonSpare (Cat_000_1_0 ~> "101") = 0x12
    assertUnparse "12" obj

testExtended1 :: Assertion
testExtended1 = do
    let act = parseNonSpare (schema @(RecordOf Cat_000_1_0 ~> "053") Proxy)
        bs = fromJust $ unhexlify "80"
        env = Env parsingStore bs
        result = runParsing act env 0
    (_obj, o) <- either (assertFailure . show) pure result
    assertEqual "leftover" True (o == endOffset bs)
    let obj2 :: NonSpare (Cat_000_1_0 ~> "053") = extended
            ( 1 *: item @"I2" 2 *: fx *: nil)
    assertUnparse "84" obj2
    let var = getVariation obj2
    assertUint 1 (fromJust $ getItem @"I1" var)
    assertUint 2 (fromJust $ getItem @"I2" var)
    assertEqual "I3" True (isNothing $ getItem @"I3" var)
    assertEqual "I4" True (isNothing $ getItem @"I4" var)
    assertEqual "I5" True (isNothing $ getItem @"I5" var)
    assertEqual "spares" [] (getExtendedSpares var)

    let obj2a :: NonSpare (Cat_000_1_0 ~> "053") = extendedGroups
            ( 0x42 *: nil) -- 0x42 = div 0x84 2, but we use Num instance
    assertEqual "unparse" (unparse @Bits obj2) (unparse @Bits obj2a)

    let obj3 :: NonSpare (Cat_000_1_0 ~> "053") = extendedGroups
            ( group (item @"I1" 1 *: item @"I2" 2 *: nil) *: nil)
    assertEqual "unparse" (unparse @Bits obj2) (unparse @Bits obj3)
    assertEqual "spares" [] (getExtendedSpares $ getVariation obj3)

    let obj4 :: NonSpare (Cat_000_1_0 ~> "053") = extendedGroups
            ( group (1 *: 2 *: nil)
           *: group (1 *: 0 *: 2 *: nil)
           *: nil )
    assertUnparse "8544" obj4
    assertEqual "spares" [0::Integer]
        (fmap bitsToNum $ getExtendedSpares $ getVariation obj4)

    let obj5 :: NonSpare (Cat_000_1_0 ~> "053") = extendedGroups
            (1 *: 2 *: nil)
    assertUnparse "0304" obj5

    let obj6 :: NonSpare (Cat_000_1_0 ~> "053") = extendedGroups
            (1 *: 2 *: 3 *: nil)
    assertUnparse "030506" obj6
    let obj6a :: NonSpare (Cat_000_1_0 ~> "053") = extendedGroups
            (1 *: 2 *: group (3 *: nil) *: nil)
        var6 = getVariation obj6
    assertEqual "unparse" (unparse @Bits obj6) (unparse @Bits obj6a)
    assertEqual "I3" True (isJust $ getItem @"I3" var6)
    assertEqual "I4" True (isJust $ getItem @"I4" var6)
    assertEqual "I5" True (isJust $ getItem @"I5" var6)

testExtended2 :: Assertion
testExtended2 = do
    let act = parseNonSpare (schema @(RecordOf Cat_000_1_0 ~> "054") Proxy)
        bs = fromJust $ unhexlify "80"
        env = Env parsingStore bs
        result = runParsing act env 0
    (_obj, o) <- either (assertFailure . show) pure result
    assertEqual "leftover" True (o == endOffset bs)
    let obj2 :: NonSpare (Cat_000_1_0 ~> "054") = extended
            ( 1 *: item @"I2" 2 *: fx *: nil)
    assertUnparse "84" obj2
    let obj2a :: NonSpare (Cat_000_1_0 ~> "054") = extendedGroups
            ( 0x42 *: nil) -- 0x42 = div 0x84 2, but we use Num instance
    assertEqual "unparse" (unparse @Bits obj2) (unparse @Bits obj2a)
    let obj3 :: NonSpare (Cat_000_1_0 ~> "054") = extendedGroups
            ( group (item @"I1" 1 *: 2 *: nil)
           *: nil)
    assertEqual "unparse" (unparse @Bits obj2) (unparse @Bits obj3)
    let obj4 :: NonSpare (Cat_000_1_0 ~> "054") = extendedGroups
            ( group (1 *: 2 *: nil)
           *: group (1 *: 0 *: 2 *: nil)
           *: nil)
    assertUnparse "8544" obj4
    let obj5 :: NonSpare (Cat_000_1_0 ~> "054") = extendedGroups
            ( 1 *: 2 *: nil)
    assertUnparse "0304" obj5
    let obj6 :: NonSpare (Cat_000_1_0 ~> "054") = extendedGroups
            ( 1 *: 2 *: 3 *: nil)
    assertUnparse "030503" obj6
    let obj7 :: NonSpare (Cat_000_1_0 ~> "054") = extendedGroups
            ( group (1 *: 2 *: nil)
           *: group (3 *: 0 *: 4 *: nil)
           *: group (5 *: nil)
           *: nil)
        s7 = toByteString $ bitsToSBuilder $ unparse @Bits obj7
        act8 = parseNonSpare (schema @(RecordOf Cat_000_1_0 ~> "054") Proxy)
        res8 = parse @StrictParsing act8 s7
    uobj8 <- either (assertFailure . show) pure res8
    let obj8 :: NonSpare (Cat_000_1_0 ~> "054") = NonSpare uobj8
        var8 = getVariation obj8
    assertUint 1 (fromJust $ getItem @"I1" var8)
    assertUint 2 (fromJust $ getItem @"I2" var8)
    assertUint 3 (fromJust $ getItem @"I3" var8)
    assertUint 4 (fromJust $ getItem @"I4" var8)
    assertUint 5 (fromJust $ getItem @"I5" var8)

testExtended3 :: Assertion
testExtended3 = do
    do
        let obj :: NonSpare (Cat_000_1_0 ~> "102") = extendedGroups
                ( 1 *: 2 *: nil)
        assertUnparse "0304" obj
    do
        let obj :: NonSpare (Cat_000_1_0 ~> "102") = extendedGroups
                ( group (1 *: nil)
               *: group (2 *: nil)
               *: nil )
        assertUnparse "0304" obj
    do
        let obj :: NonSpare (Cat_000_1_0 ~> "102") = extendedGroups
                ( group (1 *: nil)
               *: group ( group (0 *: 0 *: 2 *: nil) *: nil)
               *: nil )
        assertUnparse "0304" obj
    do
        let obj :: NonSpare (Cat_000_1_0 ~> "102") = extendedGroups
                ( group (1 *: nil)
               *: group ( group (item @"SG1" 0 *: 0 *: 2 *: nil) *: nil)
               *: nil )
        assertUnparse "0304" obj

testExtended4 :: Assertion
testExtended4 = do
    -- Create extended sample with last FX bit set to '1' (wrong).
    -- Parsing shall fail in this case.
    let act = parseNonSpare (schema @(RecordOf Cat_000_1_0 ~> "053") Proxy)
        bs = fromJust $ unhexlify "010101"
        env = Env parsingStore bs
        result = runParsing act env 0
    assertEqual "failure" True (isLeft result)

testExtended5 :: Assertion
testExtended5 = do
    -- check modifyExtendedSubitemIfPresent function
    let o1, o2, o3 :: NonSpare (Cat_000_1_0 ~> "053")
        o1 = extended ( 1 *: item @"I2" 2 *: fx *: nil)
        o2 = extended ( 1 *: item @"I2" 5 *: fx *: nil)
        o3 = modifyExtendedSubitemIfPresent @"I2" (const 2) o2 -- present
        o4 = modifyExtendedSubitemIfPresent @"I3" (const 0) o1 -- not present
        o5 = modifyExtendedSubitemIfPresent @"I5" (const 0) o1 -- not present
    assertEqual "failure o3" (unparse @Bits o1) (unparse o3)
    assertEqual "failure o4" (unparse @Bits o1) (unparse o4)
    assertEqual "failure o5" (unparse @Bits o1) (unparse o5)
    let o6, o7, o8 :: NonSpare (Cat_000_1_0 ~> "053")
        o6 = extended ( 1 *: 2 *: fx *: item @"I3" 3 *: spare *: 4 *: fx *: nil)
        o7 = extended ( 1 *: 2 *: fx *: item @"I3" 0 *: spare *: 4 *: fx *: nil)
        o8 = modifyExtendedSubitemIfPresent @"I3" (const 3) o7 -- present
    assertEqual "failure o8" (unparse @Bits o6) (unparse o8)

testRepetitive1 :: Assertion
testRepetitive1 = do
    let obj :: NonSpare (RecordOf Cat_000_1_0 ~> "061") = repetitive [1,2,3]
    assertUnparse "03010203" obj
    let lst = getRepetitiveItems $ getVariation obj
    assertEqual "len" 3 (length lst)
    assertUint 1 (head lst)
    assertUint 2 (lst !! 1)
    assertUint 3 (lst !! 2)

testRepetitive2 :: Assertion
testRepetitive2 = do
    let obj :: NonSpare (RecordOf Cat_000_1_0 ~> "062") = repetitive
            [ 1
            , group (item @"I1" 2 *: 3 *: nil)
            , 4
            ]
    assertUnparse "03000102030004" obj
    let lst = getRepetitiveItems $ getVariation obj
    assertEqual "len" 3 (length lst)
    assertUint 1 (head lst)
    assertUint 0x0203 (lst !! 1)
    assertUint 4 (lst !! 2)

testRepetitive3 :: Assertion
testRepetitive3 = do
    let obj :: NonSpare (RecordOf Cat_000_1_0 ~> "063") = repetitive (1 NE.:| [2, 3])
    assertUnparse "030506" obj
    let lst = getRepetitiveItems $ getVariation obj
    assertEqual "len" 3 (length lst)
    assertUint 1 (head lst)
    assertUint 2 (lst !! 1)
    assertUint 3 (lst !! 2)

testExplicit0 :: Assertion
testExplicit0 = do
    let bs = fromJust $ unhexlify "00"
        act = parseNonSpare (schema @(RecordOf Cat_000_1_0 ~> "071") Proxy)
        result = parse @StrictParsing act bs
    assertEqual "result" True (isLeft result)

testExplicit1 :: Assertion
testExplicit1 = do
    let obj :: NonSpare (RecordOf Cat_000_1_0 ~> "071")
        obj = explicit $ fromJust $ unhexlify "010203"
    assertUnparse "04010203" obj
    assertEqual "data"
        (byteStringToBits (fromJust $ unhexlify "010203"))
        (getExplicitData $ getVariation obj)

testExplicit2 :: Assertion
testExplicit2 = do
    let obj :: Expansion (ExpansionOf Ref_000_1_0)
        obj = expansion
            ( item @"I1" 1
           *: item @"I2" 2
           *: nil )
        bs = toByteString $ unparse @SBuilder obj
        act = parseExpansion (schema @(ExpansionOf Ref_000_1_0) Proxy)
        result = parse @StrictParsing act bs
    uobj2 <- either (assertFailure . show) pure result
    let obj2 :: Expansion (ExpansionOf Ref_000_1_0) = Expansion uobj2
        i1 = fromJust $ getItem @"I1" obj2
        i2 = fromJust $ getItem @"I2" obj2
    assertUint 1 i1
    assertUint 2 i2

testExplicit3a :: Assertion
testExplicit3a = do
    let re :: Expansion (ExpansionOf Ref_000_1_1) -- without FX
        re = expansion
            ( item @"I1" 1
           *: item @"I2" 2
           *: item @"I3" 3
           *: nil )
        r :: Record (RecordOf Cat_000_1_0)
        r = record
            ( item @"010" 0x0102
           *: item @"072" (explicit re)
           *: nil )
    let act = parseRecord (schema @(RecordOf Cat_000_1_0) Proxy)
        bs = toByteString $ unparse @SBuilder r
        result = parse @StrictParsing act bs
    ur2 <- either (assertFailure . show) pure result
    let r2 :: Record (RecordOf Cat_000_1_0) = Record ur2
        i072 = fromJust $ getItem @"072" r2
        s = getExplicitData $ getVariation i072
    assertUnparse "9020010203" s
    let act3 = parseExpansion (schema @(ExpansionOf Ref_000_1_1) Proxy)
        bs3 = toByteString $ bitsToBuilder s
        result3 = parse @StrictParsing act3 bs3
    uobj3 <- either (assertFailure . show) pure result3
    let obj3 :: Expansion (ExpansionOf Ref_000_1_1) = Expansion uobj3
        i1 = fromJust $ getItem @"I1" obj3
        i2 = fromJust $ getItem @"I2" obj3
        i3 = fromJust $ getItem @"I3" obj3
    assertUint 1 i1
    assertUint 2 i2
    assertUint 3 i3

testExplicit3b :: Assertion
testExplicit3b = do
    let re :: Expansion (ExpansionOf Ref_000_1_2) -- with FX
        re = expansion
            ( item @"I1" 1
           *: item @"I2" 2
           *: item @"I3" 3
           *: nil )
        r :: Record (RecordOf Cat_000_1_0)
        r = record
            ( item @"010" 0x0102
           *: item @"072" (explicit re)
           *: nil )
    let act = parseRecord (schema @(RecordOf Cat_000_1_0) Proxy)
        bs = toByteString $ unparse @SBuilder r
        result = parse @StrictParsing act bs
    ur2 <- either (assertFailure . show) pure result
    let r2 :: Record (RecordOf Cat_000_1_0) = Record ur2
        i072 = fromJust $ getItem @"072" r2
        s = getExplicitData $ getVariation i072
    assertUnparse "9110010203" s
    let act3 = parseExpansion (schema @(ExpansionOf Ref_000_1_2) Proxy)
        bs3 = toByteString $ bitsToBuilder s
        result3 = parse @StrictParsing act3 bs3
    uobj3 <- either (assertFailure . show) pure result3
    let obj3 :: Expansion (ExpansionOf Ref_000_1_2) = Expansion uobj3
        i1 = fromJust $ getItem @"I1" obj3
        i2 = fromJust $ getItem @"I2" obj3
        i3 = fromJust $ getItem @"I3" obj3
    assertUint 1 i1
    assertUint 2 i2
    assertUint 3 i3

testExplicit3c :: Assertion
testExplicit3c = do
    let re :: Expansion (ExpansionOf Ref_000_1_2) -- with FX
        re = expansion
            ( item @"I1" 1
           *: item @"I2" 2
           *: nil )
        r :: Record (RecordOf Cat_000_1_0)
        r = record
            ( item @"010" 0x0102
           *: item @"072" (explicit re)
           *: nil )
    let act = parseRecord (schema @(RecordOf Cat_000_1_0) Proxy)
        bs = toByteString $ unparse @SBuilder r
        result = parse @StrictParsing act bs
    ur2 <- either (assertFailure . show) pure result
    let r2 :: Record (RecordOf Cat_000_1_0) = Record ur2
        i072 = fromJust $ getItem @"072" r2
        s = getExplicitData $ getVariation i072
    assertUnparse "900102" s
    let act3 = parseExpansion (schema @(ExpansionOf Ref_000_1_2) Proxy)
        bs3 = toByteString $ bitsToBuilder s
        result3 = parse @StrictParsing act3 bs3
    uobj3 <- either (assertFailure . show) pure result3
    let obj3 :: Expansion (ExpansionOf Ref_000_1_2) = Expansion uobj3
        i1 = fromJust $ getItem @"I1" obj3
        i2 = fromJust $ getItem @"I2" obj3
        i3 = getItem @"I3" obj3
    assertUint 1 i1
    assertUint 2 i2
    assertEqual "i3" True (isNothing i3)

testCompound0 :: Assertion
testCompound0 = do
    let bs = fromJust $ unhexlify "0100"
        act = parseNonSpare (schema @(RecordOf Cat_000_1_0 ~> "091") Proxy)
        result = parse @StrictParsing act bs
    assertEqual "result" True (isLeft result)

testCompound1 :: Assertion
testCompound1 = do
    let obj :: NonSpare (RecordOf Cat_000_1_0 ~> "093")
        -- The following line (if uncommented) shall not compile
        -- obj = compound (item @"nonexistingitem" 1 *: nil)
        obj = compound nil
    assertUnparse "00" obj
    let obj1 :: NonSpare (RecordOf Cat_000_1_0 ~> "093")
        obj1 = compound
            ( item @"I1" 1
           -- The following line (if uncommented) shall not compile
           -- *: item @"I1" 2
           *: nil )
    assertEqual "I1" True (isJust $ getItem @"I1" $ getVariation obj1)
    assertEqual "I2" True (isNothing $ getItem @"I2" $ getVariation obj1)
    assertEqual "I3" True (isNothing $ getItem @"I3" $ getVariation obj1)
    assertUnparse "8001" obj1
    let obj12 :: NonSpare (RecordOf Cat_000_1_0 ~> "093")
        obj12 = compound
            ( item @"I1" 1
           *: item @"I2" 2
           *: nil )
    assertEqual "I1" True (isJust $ getItem @"I1" $ getVariation obj12)
    assertEqual "I2" True (isJust $ getItem @"I2" $ getVariation obj12)
    assertEqual "I3" True (isNothing $ getItem @"I3" $ getVariation obj12)
    assertUnparse "A00102" obj12
    let obj13 :: NonSpare (RecordOf Cat_000_1_0 ~> "093")
        obj13 = compound
            ( item @"I1" 1
           *: item @"I3" 3
           *: nil )
    assertEqual "I1" True (isJust $ getItem @"I1" $ getVariation obj13)
    assertEqual "I2" True (isNothing $ getItem @"I2" $ getVariation obj13)
    assertEqual "I3" True (isJust $ getItem @"I3" $ getVariation obj13)
    assertUnparse "81800103" obj13
    let obj123 :: NonSpare (RecordOf Cat_000_1_0 ~> "093")
        obj123 = compound
            ( item @"I1" 1
           *: item @"I2" 2
           *: item @"I3" 3
           *: nil )
    assertEqual "I1" True (isJust $ getItem @"I1" $ getVariation obj123)
    assertEqual "I2" True (isJust $ getItem @"I2" $ getVariation obj123)
    assertEqual "I3" True (isJust $ getItem @"I3" $ getVariation obj123)
    assertUnparse "A180010203" obj123
    -- order shall make no difference
    let t2, t3, t4, t5, t6 :: NonSpare (RecordOf Cat_000_1_0 ~> "093")
        t2 = compound ( item @"I1" 1 *: item @"I3" 3 *: item @"I2" 2 *: nil )
        t3 = compound ( item @"I2" 2 *: item @"I1" 1 *: item @"I3" 3 *: nil )
        t4 = compound ( item @"I2" 2 *: item @"I3" 3 *: item @"I1" 1 *: nil )
        t5 = compound ( item @"I3" 3 *: item @"I1" 1 *: item @"I2" 2 *: nil )
        t6 = compound ( item @"I3" 3 *: item @"I2" 2 *: item @"I1" 1 *: nil )
    assertEqual "t2" (unparse @Bits obj123) (unparse t2)
    assertEqual "t3" (unparse @Bits obj123) (unparse t3)
    assertEqual "t4" (unparse @Bits obj123) (unparse t4)
    assertEqual "t5" (unparse @Bits obj123) (unparse t5)
    assertEqual "t6" (unparse @Bits obj123) (unparse t6)

testCompoundSet :: Assertion
testCompoundSet = do
    let obj1, obj2, obj2a, obj2b :: NonSpare (RecordOf Cat_000_1_0 ~> "093")
        obj1 = compound ( item @"I1" 1 *: nil )
        obj2 = compound ( item @"I1" 1 *: item @"I2" 2 *: nil )
        obj2a = compound nil & setItem @"I1" 1 & setItem @"I2" 2
        obj2b = compound nil & setItem @"I1" 1 & maybeSetItem @"I2" (Just 2)
        var1 = getVariation obj1
        var2Ref = getVariation obj2
        var2 = setItem @"I2" 2 var1
    assertEqual "setItemA" (unparse @Bits obj2) (unparse obj2a)
    assertEqual "setItemB" (unparse @Bits obj2) (unparse obj2b)
    assertEqual "setItem" (unparse @Bits var2Ref) (unparse var2)

testCompoundDel :: Assertion
testCompoundDel = do
    let obj123, obj13 :: NonSpare (RecordOf Cat_000_1_0 ~> "093")
        obj123 = compound ( item @"I1" 1 *: item @"I2" 2 *: item @"I3" 3 *: nil )
        obj13 = compound ( item @"I1" 1 *: item @"I3" 3 *: nil )
        varRef = getVariation obj13
        var = delItem @"I2" (getVariation obj123)
    assertEqual "delItem" (unparse @Bits varRef) (unparse var)

testRecordEmpty :: Assertion
testRecordEmpty = do
    let bs = fromJust $ unhexlify "0101010100"
        env = Env parsingStore bs
        actStrict  = parseRecord @StrictParsing (schema @(RecordOf Cat_000_1_0) Proxy)
        actPartial = parseRecord @PartialParsing (schema @(RecordOf Cat_000_1_0) Proxy)
    assertEqual "strict" True $ isLeft $ runParsing actStrict env 0
    assertEqual "partial" True $ isRight $ runParsing actPartial env 0

testRecord0RFS :: Assertion
testRecord0RFS = do
    let r0 :: Record (RecordOf Cat_004_1_0) = record nil
    assertUnparse "00" r0
    assertEqual "item 010" True (isNothing $ getItem @"010" r0)

    let r1 :: Record (RecordOf Cat_004_1_0) = record
            ( item @"010" 0x03
           *: nil )
    assertUnparse "8003" r1
    let i010 = fromJust $ getItem @"010" r1
    assertUint 0x03 i010

    -- The following line (if uncommented) shall not compile
    -- void $ pure (record (rfs @0 nil *: nil) :: (Record (RecordOf Cat_004_1_0)))

testRecord1RFS :: Assertion
testRecord1RFS = do
    let r0 :: Record (RecordOf Cat_000_1_0) = record nil
    assertUnparse "00" r0
    assertEqual "item 000" True (isNothing $ getItem @"000" r0)

    let r1 :: Record (RecordOf Cat_000_1_0) = record
            ( item @"000" 0x03
           *: nil )
    assertUnparse "4003" r1
    let i0 = fromJust $ getItem @"000" r1
    assertUint 0x03 i0

    let r2a :: Record (RecordOf Cat_000_1_0) = record
            ( item @"010" (group (item @"SAC" 0x01 *: item @"SIC" 0x02 *: nil))
           *: item @"000" 0x03
           *: nil )
    assertEqual "000" True (isJust $ getItem @"000" r2a)
    let r2a010 = fromJust $ getItem @"010" r2a
    assertUint 0x01 (getItem @"SAC" $ getVariation r2a010)
    assertUint 0x02 (getItem @"SIC" $ getVariation r2a010)

    let r2b :: Record (RecordOf Cat_000_1_0) = record
            ( item @"010" (group (item @"SAC" 0x01 *: 0x02 *: nil))
           *: item @"000" 0x03
           *: nil )
        r2c :: Record (RecordOf Cat_000_1_0) = record
            ( item @"010" 0x0102
           *: item @"000" 0x03
           *: nil )
    forM_ [r2a, r2b, r2c] $ \r -> do
        assertUnparse "C0010203" r

    let r3 :: Record (RecordOf Cat_000_1_0) = record
            ( item @"032" (compound
                ( item @"I1" 0x11
               *: item @"CC" (group
                    ( item @"TID" 5
                   *: item @"CP" 3
                   *: item @"CS" 1
                   *: nil ))
               *: nil ))
           *: nil )
    assertUnparse "04C01157" r3

    let withRfs :: Record (RecordOf Cat_000_1_0) = record
            ( item @"000" 0x03
           *: rfs -- or rfs @0, to be explicit which rfs, but there is only one
                ( item @"000" 0xAA
               *: item @"000" 0x55
               *: item @"010" 0x1234
               *: item @"053" (extendedGroups (1 *: 2 *: nil))
               *: item @"054" (extendedGroups (1 *: 2 *: nil))
               *: item @"061" (repetitive [0xFF])
               *: nil )
           *: nil )
        bs = "410104030602AA02550112340A03040B03040C01FF"
        bs' = fromJust $ unhexlify bs
    assertUnparse bs withRfs

    let _i000 = fromJust $ getItem @"000" withRfs
    assertEqual "010" True (isNothing $ getItem @"010" withRfs)

    assertEqual "rfs 000" [0xAA, 0x55]
        (asUint @Integer <$> getRfsItem @"000" withRfs)
    assertEqual "rfs 010" [0x1234]
        (asUint @Integer <$> getRfsItem @"010" withRfs)
    assertEqual "rfs 020" 0 (length $ getRfsItem @"020" withRfs)
    assertEqual "rfs 053" 1 (length $ getRfsItem @"053" withRfs)

    -- Check parsing (with RFS)
    let act = parseRecord (schema @(RecordOf Cat_000_1_0) Proxy)
        result = parse @StrictParsing act bs'
    withRfs2 <- either (assertFailure . show) pure result
    assertUnparse bs withRfs2

testRecordMultipleRFS :: Assertion
testRecordMultipleRFS = do
    let check r expected = do
            assertUnparse expected r
            let act = parseRecord (schema @(RecordOf Cat_003_1_0) Proxy)
                bs = fromJust $ unhexlify expected
                result = parse @StrictParsing act bs
            readback <- either (assertFailure . show) pure result
            assertEqual "readback" (toBits bs) (unparse readback)
        r0 :: Record (RecordOf Cat_003_1_0) = record
            ( item @"010" 0x01
           *: nil )
        r1 :: Record (RecordOf Cat_003_1_0) = record
            ( item @"010" 0x01
           *: rfs @0
                ( item @"101" 0xAA
               *: item @"102" 0x55
               *: nil )
           *: nil )
        r2 :: Record (RecordOf Cat_003_1_0) = record
            ( item @"010" 0x01
           *: rfs @0
                ( item @"101" 0xAA
               *: item @"102" 0x55
               *: nil )
           *: rfs @1
                ( item @"201" 0xF1
               *: item @"202" 0xF2
               *: nil )
           *: nil )
        r3 :: Record (RecordOf Cat_003_1_0) = record
            ( item @"010" 0x01
           -- *: rfs @0 ... not specified
           *: rfs @1
                ( item @"201" 0xF1
               *: item @"202" 0xF2
               *: nil )
           *: nil )
        r4 :: Record (RecordOf Cat_003_1_0) = record
            ( item @"010" 0x01
           *: rfs @0 nil -- specified, but empty
           -- The following line (if uncommented) shall not compile
           -- *: rfs @0 nil
           *: rfs @1
                ( item @"201" 0xF1
               *: item @"202" 0xF2
               *: nil )
           *: nil )
    check r0 "8001"
    check r1 "90010202aa0355"
    check r2 "94010202aa0355020500f10700f2"
    check r3 "8401020500f10700f2"
    check r4 "940100020500f10700f2"

testRecordSetItem :: Assertion
testRecordSetItem = do
    let r0, r1, r2 :: Record (RecordOf Cat_000_1_0)
        r0 = setItem @"000" 0x03 (record nil)
        r1 = record (item @"000" 0x03 *: nil)
        r2 = record nil & maybeSetItem @"000" (Just 0x03)
    assertEqual "unparse" (unparse @Bits r0) (unparse r1)
    assertEqual "unparse" (unparse @Bits r0) (unparse r2)

testRecordDelItem :: Assertion
testRecordDelItem = do
    let r0, r1 :: Record (RecordOf Cat_000_1_0)
        r0 = delItem @"010" (record
            ( item @"010" ( group
                ( item @"SAC" 0x01 *: 0x02 *: nil))
           *: item @"000" 0x03
           *: nil ))
        r1 = record (item @"000" 0x03 *: nil)
    assertEqual "unparse" (unparse @Bits r0) (unparse r1)

testMultipleUAPS :: Assertion
testMultipleUAPS = do
    let recPlot :: Record (RecordOfUap Cat_001_1_0 "plot") = record
            ( item @"010" 1
           *: item @"020" (extended
                ( item @"TYP" 0 *: item @"I1" 1 *: fx *: nil))
           *: item @"031" 0x03
           *: nil )
    assertUnparse "E000010203" recPlot

    let recTrack :: Record (RecordOfUap Cat_001_1_0 "track") = record
            ( item @"010" 1
           *: item @"020" (extended
                ( item @"TYP" 1 *: item @"I1" 2 *: fx *: nil))
           *: item @"032" 0x04
           *: nil )
    assertUnparse "D00001840004" recTrack

testCreateDatagram :: Assertion
testCreateDatagram = do
    let db0 :: Datablock (DatablockOf Cat_000_1_0) = datablock
            ( record ( item @"010" 1 *: nil)
           *: record ( item @"010" 2 *: nil)
           *: nil )
        db1 :: Datablock (DatablockOf Cat_001_1_0) = datablock
            ( (record ( item @"010" 1 *: nil) :: Record (RecordOfUap Cat_001_1_0 "plot"))
           *: (record ( item @"010" 2 *: nil) :: Record (RecordOfUap Cat_001_1_0 "track"))
           *: nil )
        datagram :: SBuilder = unparse db0 <> unparse db1
    assertEqual "unparse"
        (fromJust $ unhexlify "000009800001800002010009800001800002")
        (toByteString datagram)

testParse1 :: Assertion
testParse1 = forM_ samples $ \sample -> do
    let act = parseRecord (schema @(RecordOf Cat_000_1_0) Proxy)
        bs = fromJust $ unhexlify sample
        env = Env @StrictParsing parsingStore bs
        result = runParsing act env 0
    (r, o) <- either (assertFailure . show) pure result
    assertEqual "leftover" True (o == endOffset bs)
    assertEqual "readback" (hexlify bs)
        (hexlify $ builderToByteStringSlow $ sbData $ unparse r)
  where
    samples =
        [ "4003"
        , "C0010203"
        , "04C01157"
        , "410104030502000112340A03040B03040C01FF"
        , "410104030602AA02550112340A03040B03040C01FF"
        ]

testParse2 :: Assertion
testParse2 = do
    let r = record ( item @"010" 1 *: nil) :: Record (RecordOf Cat_000_1_0)
        d :: Datablock (DatablockOf Cat_000_1_0)
        d = datablock (r *: r *: r *: nil)
        bs = toByteString $ unparse @SBuilder d
        result1 = parseRawDatablocks bs
    dbs <- either (assertFailure . show) pure result1
    assertEqual "length" 1 (length dbs)
    let db = head dbs
        act = parseRecords (schema @(RecordOf Cat_000_1_0) Proxy)
        result = parse @StrictParsing act (getRawRecords db)
    records <- either (assertFailure . show) pure result
    assertEqual "len" 3 (length records)
    forM_ (zip [0::Int ..] records) $ \(cnt, i) -> do
        assertEqual ("unparse " <> show cnt)
            (debugBits $ unparse @Bits r)
            (debugBits $ unparse @Bits i)

testParse3 :: Assertion
testParse3 = do
    let recPlot :: Record (RecordOfUap Cat_001_1_0 "plot")
        recPlot = record
            ( item @"010" 0x0102
           *: item @"020" (extended
                ( item @"TYP" 0 *: 0 *: fx *: nil))
           *: item @"031" 0
           *: nil )
        recTrack :: Record (RecordOfUap Cat_001_1_0 "track")
        recTrack = record
            ( item @"010" 0x0102
           *: item @"020" (extended
                ( item @"TYP" 1 *: 0 *: fx *: nil))
           *: item @"032" 0
           *: nil )

    -- plots
    do
        let _db :: Datablock (DatablockOf Cat_001_1_0) = datablock
                ( recPlot *: recPlot *: nil)
            bs = toByteString $ unparse @SBuilder _db
        dbs <- either (assertFailure . show) pure (parseRawDatablocks bs)
        assertEqual "plots length" 1 (length dbs)
        let db = head dbs
            bs2 = getRawRecords db

        -- try to parse as 'plots'
        let act1 = parseRecords (schema @(RecordOfUap Cat_001_1_0 "plot") Proxy)
        result1 <- either (assertFailure . show) pure
            (parse @StrictParsing act1 bs2)
        assertEqual "plots length plots" 2 (length result1)
        forM_ (zip [0::Int ..] result1) $ \(cnt, r1) -> do
            assertEqual ("unparse " <> show cnt)
                (debugBits $ unparse @Bits recPlot)
                (debugBits $ unparse @Bits r1)

        -- try to parse as 'tracks'
        let act2 = parseRecords (schema @(RecordOfUap Cat_001_1_0 "track") Proxy)
            result2 = parse @StrictParsing act2 bs2
        assertEqual "result2" True (isLeft result2)

        -- try to parse as any defined UAP
        let act3 = parseRecordsTry Nothing (schema @Cat_001_1_0 Proxy)
        result3s <- either (assertFailure . show) pure
            (parse @StrictParsing act3 bs2)
        assertEqual "plots length outer" 1 (length result3s)
        let result3 = head result3s
        assertEqual "plots length inner" 2 (length result3)
        forM_ (zip [0::Int ..] result3) $ \(cnt, (_name, r2)) -> do
            assertEqual ("unparse " <> show cnt)
                (debugBits $ unparse @Bits recPlot)
                (debugBits $ unparse @Bits r2)

    -- tracks
    do
        let _db :: Datablock (DatablockOf Cat_001_1_0) = datablock
                ( recTrack *: recTrack *: recTrack *: nil)
            bs = toByteString $ unparse @SBuilder _db
        dbs <- either (assertFailure . show) pure (parseRawDatablocks bs)
        assertEqual "tracks length" 1 (length dbs)
        let db = head dbs
            bs2 = getRawRecords db

        -- try to parse as 'plots'
        let act1 = parseRecords (schema @(RecordOfUap Cat_001_1_0 "plot") Proxy)
            result1 = parse @StrictParsing act1 bs2
        assertEqual "result1" True (isLeft result1)

        -- try to parse as 'tracks'
        let act2 = parseRecords (schema @(RecordOfUap Cat_001_1_0 "track") Proxy)
        result2 <- either (assertFailure . show) pure
            (parse @StrictParsing act2 bs2)
        assertEqual "tracks length plots" 3 (length result2)
        forM_ (zip [0::Int ..] result2) $ \(cnt, r2) -> do
            assertEqual ("unparse " <> show cnt)
                (debugBits $ unparse @Bits recTrack)
                (debugBits $ unparse @Bits r2)

        -- try to parse as any defined UAP
        let act3 = parseRecordsTry Nothing (schema @Cat_001_1_0 Proxy)
        result3s <- either (assertFailure . show) pure
            (parse @StrictParsing act3 bs2)
        assertEqual "tracks length outer" 1 (length result3s)
        let result3 = head result3s
        assertEqual "tracks length inner" 3 (length result3)
        forM_ (zip [0::Int ..] result3) $ \(cnt, (_name, r2)) -> do
            assertEqual ("unparse " <> show cnt)
                (debugBits $ unparse @Bits recTrack)
                (debugBits $ unparse @Bits r2)

    -- mixed
    do
        let records
                = recPlot *: recPlot
               *: recTrack *: recTrack *: recTrack *: nil
            _db :: Datablock (DatablockOf Cat_001_1_0) = datablock records
            bs = toByteString $ unparse @SBuilder _db
        dbs <- either (assertFailure . show) pure (parseRawDatablocks bs)
        assertEqual "mixed length" 1 (length dbs)
        let db = head dbs
            bs2 = getRawRecords db

        -- try to parse as 'plots'
        let act1 = parseRecords (schema @(RecordOfUap Cat_001_1_0 "plot") Proxy)
            result1 = parse @StrictParsing act1 bs2
        assertEqual "result1" True (isLeft result1)

        -- try to parse as 'tracks'
        let act2 = parseRecords (schema @(RecordOfUap Cat_001_1_0 "track") Proxy)
            result2 = parse @StrictParsing act2 bs2
        assertEqual "result2" True (isLeft result2)

        -- try to parse as any defined UAP
        let act3 = parseRecordsTry Nothing (schema @Cat_001_1_0 Proxy)
        result3s <- either (assertFailure . show) pure
            (parse @StrictParsing act3 bs2)
        assertEqual "mixed length outer" 1 (length result3s)
        let result3 = head result3s
            recordsRaw = foldHList @(Unparsing Bits)
                (\r -> [unparse @Bits r])
                records
        assertEqual "mixed length inner" (length recordsRaw) (length result3)
        forM_ (zip recordsRaw result3) $ \(r1, (_name, r2)) -> do
            assertEqual "unparse" r1 (unparse @Bits r2)

testParse4 :: Assertion
testParse4 = do
    let recPlot :: Record (RecordOfUap Cat_001_1_0 "plot")
        recPlot = record ( item @"010" 0x0102 *: nil )
        recTrack :: Record (RecordOfUap Cat_001_1_0 "track")
        recTrack = record ( item @"010" 0x0102 *: nil )

    -- from '010' item, both records look the same
    assertEqual "unparse" (unparse @Bits recPlot) (unparse @Bits recTrack)

    do -- one
        let _db :: Datablock (DatablockOf Cat_001_1_0) = datablock (recPlot *: nil)
            bs = toByteString $ unparse @SBuilder _db
        dbs <- either (assertFailure . show) pure (parseRawDatablocks bs)
        assertEqual "results" 1 (length dbs)
        let db = head dbs
            bs2 = getRawRecords db
            act = parseRecordsTry Nothing (schema @Cat_001_1_0 Proxy)
        results <- either (assertFailure . show) pure
            (parse @StrictParsing act bs2)
        assertEqual "results" 2 (length results)
        forM_ results $ \result -> do
            assertEqual "result" 1 (length result)
        let act1 = parseRecords (schema @(RecordOfUap Cat_001_1_0 "plot") Proxy)
        result1 <- either (assertFailure . show) pure
            (parse @StrictParsing act1 bs2)
        assertEqual "result" 1 (length result1)
        let act2 = parseRecords (schema @(RecordOfUap Cat_001_1_0 "track") Proxy)
        result2 <- either (assertFailure . show) pure
            (parse @StrictParsing act2 bs2)
        assertEqual "result" 1 (length result2)

    do -- two
        let _db :: Datablock (DatablockOf Cat_001_1_0) = datablock
                (recPlot *: recPlot *: nil)
            bs = toByteString $ unparse @SBuilder _db
        dbs <- either (assertFailure . show) pure (parseRawDatablocks bs)
        assertEqual "results" 1 (length dbs)
        let db = head dbs
            bs2 = getRawRecords db
            act = parseRecordsTry Nothing (schema @Cat_001_1_0 Proxy)
        results <- either (assertFailure . show) pure
            (parse @StrictParsing act bs2)
        assertEqual "results" 4 (length results)
        forM_ results $ \result -> do
            assertEqual "result" 2 (length result)
        let act1 = parseRecords (schema @(RecordOfUap Cat_001_1_0 "plot") Proxy)
        result1 <- either (assertFailure . show) pure
            (parse @StrictParsing act1 bs2)
        assertEqual "result" 2 (length result1)
        let act2 = parseRecords (schema @(RecordOfUap Cat_001_1_0 "track") Proxy)
        result2 <- either (assertFailure . show) pure
            (parse @StrictParsing act2 bs2)
        assertEqual "result" 2 (length result2)

    do -- three
        let _db :: Datablock (DatablockOf Cat_001_1_0) = datablock
                (recPlot *: recPlot *: recPlot *: nil)
            bs = toByteString $ unparse @SBuilder _db
        dbs <- either (assertFailure . show) pure (parseRawDatablocks bs)
        assertEqual "results" 1 (length dbs)
        let db = head dbs
            bs2 = getRawRecords db
            act = parseRecordsTry Nothing (schema @Cat_001_1_0 Proxy)
        results <- either (assertFailure . show) pure
            (parse @StrictParsing act bs2)
        assertEqual "results" (2 ^ (3::Int)) (length results)
        forM_ results $ \result -> do
            assertEqual "result" 3 (length result)
        let act1 = parseRecords (schema @(RecordOfUap Cat_001_1_0 "plot") Proxy)
        result1 <- either (assertFailure . show) pure
            (parse @StrictParsing act1 bs2)
        assertEqual "result" 3 (length result1)
        let act2 = parseRecords (schema @(RecordOfUap Cat_001_1_0 "track") Proxy)
        result2 <- either (assertFailure . show) pure
            (parse @StrictParsing act2 bs2)
        assertEqual "result" 3 (length result2)

testParse5 :: Assertion
testParse5 = do
    -- Non-compatible items in multiple uaps.
    let recPlot :: Record (RecordOfUap Cat_001_1_0 "plot")
        recPlot = record ( item @"041" 0xff *: nil )
        recTrack :: Record (RecordOfUap Cat_001_1_0 "track")
        recTrack = record ( item @"042" 0xffff *: nil )

        bsPlot = toByteString $ unparse @SBuilder recPlot
        bsTrack = toByteString $ unparse @SBuilder recTrack

    do
        let act = parseRecordsTry Nothing (schema @Cat_001_1_0 Proxy)
        results <- either (assertFailure . show) pure
            (parse @StrictParsing act bsPlot)
        assertEqual "result" 1 (length results)
        assertEqual "result" 1 (length $ head results)

    do
        let act = parseRecordsTry Nothing (schema @Cat_001_1_0 Proxy)
        results <- either (assertFailure . show) pure
            (parse @StrictParsing act bsTrack)
        assertEqual "result" 1 (length results)
        assertEqual "result" 1 (length $ head results)

testParse6 :: Assertion
testParse6 = do
    -- Check parse_any_uap with max_depth.
    let sample = BS.pack (replicate 10 0)
        sch = schema @Cat_001_1_0 Proxy
        act1 = parseRecordsTry (Just 9) sch
        act2 = parseRecordsTry (Just 10) sch
        result1 = parse @StrictParsing act1 sample
    assertEqual "failure" True (isLeft result1)
    result2 <- either (assertFailure . show) pure
        (parse @StrictParsing act2 sample)
    assertEqual "result2" 1024 (length result2)

testParseNonblocking :: Assertion
testParseNonblocking = do
    -- We encode a single record with a new edition, where some items
    -- are added to the spec. We try to parse it as an old edition.

    let
        -- old edition record
        record0 :: Record (RecordOf Cat_000_1_0)
        record0 = record
            ( item @"000" 0x00
           *: item @"010" 0x0102
           *: nil )

        -- new edition record (added item '200')
        record1 :: Record (RecordOf Cat_000_1_1)
        record1 = record
            ( item @"000" 0x00
           *: item @"010" 0x0102
           *: item @"200" 0xff
           *: nil )

        -- encode new record
        s = toByteString $ unparse @SBuilder record1

    -- try to parse as a list with both editions
    assertEqual "result1" True (isLeft $ parse @StrictParsing
        (parseRecords $ schema @(RecordOf Cat_000_1_0) Proxy) s)
    assertEqual "result2" True (isRight $ parse @StrictParsing
        (parseRecords $ schema @(RecordOf Cat_000_1_1) Proxy) s)

    -- single record parsing combinations

    -- parsing with new edition shall not fail, regardless of parsing mode
    r1a <- either (assertFailure . show) pure
        (parse @StrictParsing (parseRecord $ schema @(RecordOf Cat_000_1_1) Proxy) s)
    r1b <- either (assertFailure . show) pure
        (parse @PartialParsing (parseRecord $ schema @(RecordOf Cat_000_1_1) Proxy) s)
    assertEqual "unparse" (unparse @Bits record1) (unparse r1a)
    assertEqual "unparse" (unparse @Bits record1) (unparse r1b)

    -- old edition, partial parsing shall not fail
    r2 <- either (assertFailure . show) pure (parse @PartialParsing
        (parseRecord $ schema @(RecordOf Cat_000_1_0) Proxy) s)
    assertEqual "unparse" (unparse @Bits record0) (unparse @Bits r2)

