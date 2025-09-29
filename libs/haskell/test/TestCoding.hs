{-# LANGUAGE DataKinds #-}

-- | Unsorted Coding tests

module TestCoding (tests) where

import           Data.Word

import           Test.Tasty
import           Test.Tasty.HUnit

import           Asterix.Coding
import           Common
import           Generated        as Gen

type Cat0_1_0 = Gen.Cat_000_1_0
type Cat1_1_0 = Gen.Cat_001_1_0

tests :: TestTree
tests = testGroup "Coding"
    [ testCase "create" testCreate
    ]

mkSac :: Word8 -> NonSpare (Cat0_1_0 ~> "010" ~> "SAC")
mkSac = fromIntegral

mkSic :: Word8 -> NonSpare (Cat0_1_0 ~> "010" ~> "SIC")
mkSic = fromIntegral

toItem :: NonSpare t -> Item ('GItem  t)
toItem = Item . UItem . unNonSpare

mkSacSic :: Word8 -> Word8 -> NonSpare (Cat0_1_0 ~> "010")
mkSacSic a b = group
    ( item @"SAC" (toItem $ mkSac a)
   *: item @"SIC" (toItem $ mkSic b)
   *: nil)

testCreate :: Assertion
testCreate = do
    checkBits "i000" (0xa5 :: NonSpare (Cat0_1_0 ~> "000")) (Bin "10100101")
    checkBits "i010" (0x0102 :: NonSpare (Cat0_1_0 ~> "010")) (Hex "0102")
    checkBits "mkSacSic" (mkSacSic 0x03 0x04) (Hex "0304")
    do
        let nsp :: NonSpare (Cat0_1_0 ~> "010")
            nsp = group
                ( item @"SAC" 0x01
               *: item @"SIC" 0x02
               *: nil)
        checkBits "i010b" nsp (Hex "0102")
    do
        let nsp :: NonSpare (Cat0_1_0 ~> "010")
            nsp = group
                ( item 0x01
               *: 0x02
               *: nil)
        checkBits "i010c" nsp (Hex "0102")

    do
        let nsp1 :: NonSpare (Cat0_1_0 ~> "040")
            nsp1 = group
                ( item @"I1" 0x7f
               *: spare
               *: item @"I2" 0x3f
               *: abuseSpare 0x12
               *: nil)
            nsp2 :: NonSpare (Cat0_1_0 ~> "040")
            nsp2 = group
                ( item @"I1" 0x7f
               *: spare
               *: item @"I2" 0x3f
               *: 0x12
               *: nil)
            result = Bin "11111110 01111110 00010010"
        checkBits "i040a" nsp1 result
        checkBits "i040b" nsp2 result

    do
        let db :: Datablock (DatablockOf Cat0_1_0)
            db = datablock nil
        checkBits "empty db" db (Hex "000003")

    do
        let
            sacsic :: NonSpare (Cat0_1_0 ~> "010")
            sacsic = group
                        ( item @"SAC" 0x02
                       *: item @"SIC" 0x03
                       *: nil )
            db :: Datablock (DatablockOf Cat0_1_0)
            db = datablock
                ( record
                    ( item @"000" 0x01
                   *: item @"010" sacsic
                   *: nil )
               *: nil )
        checkBits "simple db" db (Hex "000007c0020301")

    do
        let
            r :: Record (RecordOf Cat0_1_0)
            r = record (item @"000" 0x01 *: nil)
            db :: Datablock (DatablockOf Cat0_1_0)
            db = datablock (r *: r *: nil)
        checkBits "2 records" db (Hex "00000740014001")

    do
        let
            r1 :: Record (RecordOfUap Cat1_1_0 "plot")
            r1 = record (item @"010" 0x0102 *: nil)

            r2 :: Record (RecordOfUap Cat1_1_0 "track")
            r2 = record (item @"010" 0x0102 *: nil)

            db :: Datablock (DatablockOf Cat1_1_0)
            db = datablock (r1 *: r2 *: nil)
        checkBits "multi uap" db (Hex "010009800102800102")

