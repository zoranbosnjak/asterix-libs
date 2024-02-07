-- | Asterix specifications

-- This file is generated, DO NOT EDIT!
-- For more details, see:
--    - https://github.com/zoranbosnjak/asterix-specs

-- Types are BIG, disable depth checking.
{-# OPTIONS_GHC -freduction-depth=0 #-}

import           Data.Text

import           Asterix.Schema

reference :: Text
reference = "unknown"

version :: Text
version = "19700101.0"

-- | Content set
type TContent_0 = 'TContentRaw
type TContent_1 = 'TContentTable '[ '( 0, "Air Speed = IAS, LSB (Bit-1) = 2^-14 NM/s"), '( 1, "Air Speed = Mach, LSB (Bit-1) = 0.001")]
type TContent_2 = 'TContentTable '[ '( 0, "Plot"), '( 1, "Track")]
type TContent_3 = 'TContentTable '[ '( 0, "Test 0"), '( 1, "Test 1"), '( 2, "Test 2"), '( 3, "Test 3")]
type TContent_4 = 'TContentString 'StringAscii
type TContent_5 = 'TContentString 'StringICAO
type TContent_6 = 'TContentString 'StringOctal
type TContent_7 = 'TContentInteger 'Signed
type TContent_8 = 'TContentInteger 'Unsigned
type TContent_9 = 'TContentQuantity 'Signed ('TNumDiv ('TNumInt 'TPlus 180) ('TNumPow 2 23)) "°"
type TContent_10 = 'TContentQuantity 'Unsigned ('TNumInt 'TPlus 1) ""
type TContent_11 = 'TContentQuantity 'Unsigned ('TNumInt 'TPlus 1) "kt"
type TContent_12 = 'TContentQuantity 'Unsigned ('TNumDiv ('TNumInt 'TMinus 1) ('TNumInt 'TPlus 2)) ""

-- | Variation set
type TVariation_0 = 'TElement 1 TContent_1
type TVariation_1 = 'TElement 1 TContent_2
type TVariation_2 = 'TElement 4 TContent_0
type TVariation_3 = 'TElement 6 TContent_0
type TVariation_4 = 'TElement 7 TContent_0
type TVariation_5 = 'TElement 8 TContent_0
type TVariation_6 = 'TElement 8 TContent_3
type TVariation_7 = 'TElement 8 TContent_7
type TVariation_8 = 'TElement 8 TContent_8
type TVariation_9 = 'TElement 8 TContent_10
type TVariation_10 = 'TElement 8 TContent_12
type TVariation_11 = 'TElement 12 TContent_6
type TVariation_12 = 'TElement 16 TContent_0
type TVariation_13 = 'TElement 16 TContent_11
type TVariation_14 = 'TElement 24 TContent_9
type TVariation_15 = 'TElement 32 TContent_0
type TVariation_16 = 'TElement 48 TContent_5
type TVariation_17 = 'TElement 56 TContent_0
type TVariation_18 = 'TElement 56 TContent_4
type TVariation_19 = 'TElement 64 TContent_0
type TVariation_20 = 'TElement 6 TContent_0
type TVariation_21 = 'TElement 15 TContent_0
type TVariation_22 = 'TElement 4 TContent_0
type TVariation_23 = 'TGroup '[ TItem_33, TItem_2, TItem_39, TItem_42, TItem_1, TItem_45]
type TVariation_24 = 'TGroup '[ TItem_34, TItem_3, TItem_40, TItem_4]
type TVariation_25 = 'TGroup '[ TItem_35, TItem_39]
type TVariation_26 = 'TGroup '[ TItem_47, TItem_46]
type TVariation_27 = 'TGroup '[ TItem_53, TItem_59, TItem_54, TItem_55, TItem_56, TItem_0, TItem_37, TItem_41, TItem_48, TItem_49, TItem_50, TItem_51, TItem_52, TItem_30, TItem_31, TItem_32]
type TVariation_28 = 'TGroup '[ TItem_57, TItem_58]
type TVariation_29 = 'TExtended '[ 'Just TItem_34, 'Nothing, 'Just TItem_38, 'Nothing, 'Just TItem_43, 'Nothing]
type TVariation_30 = 'TExtended '[ 'Just TItem_34, 'Nothing, 'Just TItem_38, 'Nothing, 'Just TItem_44]
type TVariation_31 = 'TExtended '[ 'Just TItem_60, 'Just TItem_36, 'Nothing, 'Just TItem_38, 'Nothing]
type TVariation_32 = 'TRepetitive ('Just 1) TVariation_5
type TVariation_33 = 'TRepetitive ('Just 1) TVariation_25
type TVariation_34 = 'TRepetitive 'Nothing TVariation_4
type TVariation_35 = 'TExplicit
type TVariation_36 = 'TExplicit
type TVariation_37 = 'TExplicit
type TVariation_38 = 'TCompound 'Nothing '[ 'Just TItem_5]
type TVariation_39 = 'TCompound 'Nothing '[ 'Just TItem_5, 'Just TItem_25, 'Just TItem_26]
type TVariation_40 = 'TCompound 'Nothing '[ 'Just TItem_5, 'Just TItem_27, 'Just TItem_28]
type TVariation_41 = 'TCompound 'Nothing '[ 'Just TItem_5, 'Just TItem_29]
type TVariation_42 = 'TCompound 'Nothing '[ 'Just TItem_6, 'Just TItem_7, 'Just TItem_9, 'Just TItem_12, 'Just TItem_13, 'Just TItem_14, 'Just TItem_15, 'Just TItem_16, 'Just TItem_17, 'Just TItem_18, 'Just TItem_19, 'Just TItem_20, 'Just TItem_21, 'Just TItem_22, 'Nothing, 'Just TItem_23, 'Just TItem_24]
type TVariation_43 = 'TCompound 'Nothing '[ 'Just TItem_6, 'Just TItem_8, 'Just TItem_10]
type TVariation_44 = 'TCompound 'Nothing '[ 'Just TItem_6, 'Just TItem_8, 'Just TItem_11]
type TVariation_45 = 'TCompound 'Nothing '[ 'Just TItem_35, 'Nothing, 'Just TItem_39]
type TVariation_46 = 'TCompound ('Just 1) '[ 'Just TItem_35, 'Nothing, 'Just TItem_39]
type TVariation_47 = 'TCompound ('Just 1) '[ 'Just TItem_35, 'Just TItem_39]

-- | Item set
type TItem_0 = 'TSpare 4
type TItem_1 = 'TSpare 8
type TItem_2 = 'TSpare 2
type TItem_3 = 'TSpare 2
type TItem_4 = 'TSpare 9
type TItem_5 = 'TItem "010" "" TVariation_5
type TItem_6 = 'TItem "010" "Data Source Identifier" TVariation_28
type TItem_7 = 'TItem "020" "Different Contents" TVariation_27
type TItem_8 = 'TItem "020" "Target Report Descriptor" TVariation_31
type TItem_9 = 'TItem "030" "Dependent Item" TVariation_26
type TItem_10 = 'TItem "031" "For Plots Only" TVariation_5
type TItem_11 = 'TItem "032" "For Tracks Only" TVariation_12
type TItem_12 = 'TItem "040" "Spare Items" TVariation_24
type TItem_13 = 'TItem "051" "Element" TVariation_5
type TItem_14 = 'TItem "052" "Group" TVariation_23
type TItem_15 = 'TItem "053" "Extended With Trailing Fx" TVariation_29
type TItem_16 = 'TItem "054" "Extended Without Trailing Fx" TVariation_30
type TItem_17 = 'TItem "061" "Repetitive Regular" TVariation_32
type TItem_18 = 'TItem "062" "Repetitive With Group" TVariation_33
type TItem_19 = 'TItem "063" "Repetitive Fx" TVariation_34
type TItem_20 = 'TItem "071" "Explicit None" TVariation_35
type TItem_21 = 'TItem "072" "Explicit RE" TVariation_36
type TItem_22 = 'TItem "073" "Explicit SP" TVariation_37
type TItem_23 = 'TItem "091" "Compound Fspec With Fx" TVariation_45
type TItem_24 = 'TItem "092" "Compound Fixed Size Fspec" TVariation_46
type TItem_25 = 'TItem "101" "" TVariation_5
type TItem_26 = 'TItem "102" "" TVariation_5
type TItem_27 = 'TItem "201" "" TVariation_12
type TItem_28 = 'TItem "202" "" TVariation_12
type TItem_29 = 'TItem "301" "" TVariation_15
type TItem_30 = 'TItem "B1" "Bds With Address" TVariation_19
type TItem_31 = 'TItem "B2" "Bds At Unknown Address" TVariation_17
type TItem_32 = 'TItem "B3" "Bds At Known Address" TVariation_17
type TItem_33 = 'TItem "I1" "" TVariation_3
type TItem_34 = 'TItem "I1" "" TVariation_4
type TItem_35 = 'TItem "I1" "" TVariation_5
type TItem_36 = 'TItem "I1" "" TVariation_20
type TItem_37 = 'TItem "I1" "Unsigned Integer" TVariation_8
type TItem_38 = 'TItem "I2" "" TVariation_4
type TItem_39 = 'TItem "I2" "" TVariation_5
type TItem_40 = 'TItem "I2" "" TVariation_20
type TItem_41 = 'TItem "I2" "Signed Integer" TVariation_7
type TItem_42 = 'TItem "I3" "" TVariation_2
type TItem_43 = 'TItem "I3" "" TVariation_4
type TItem_44 = 'TItem "I3" "" TVariation_5
type TItem_45 = 'TItem "I4" "" TVariation_22
type TItem_46 = 'TItem "IAS" "" TVariation_21
type TItem_47 = 'TItem "IM" "" TVariation_0
type TItem_48 = 'TItem "Q1LAT" "Latitude in WGS.84 in Two's Complement Form" TVariation_14
type TItem_49 = 'TItem "Q2LON" "Longitude in WGS.84 in Two's Complement Form" TVariation_14
type TItem_50 = 'TItem "Q3" "Unsigned Quantity" TVariation_13
type TItem_51 = 'TItem "Q4" "Quantity No Unit" TVariation_9
type TItem_52 = 'TItem "Q5" "Negative Lsb" TVariation_10
type TItem_53 = 'TItem "R" "Raw" TVariation_5
type TItem_54 = 'TItem "S1" "String Ascii" TVariation_18
type TItem_55 = 'TItem "S2" "String ICAO" TVariation_16
type TItem_56 = 'TItem "S3" "String Octal" TVariation_11
type TItem_57 = 'TItem "SAC" "System Area Code" TVariation_5
type TItem_58 = 'TItem "SIC" "System Identification Code" TVariation_5
type TItem_59 = 'TItem "T" "Table" TVariation_6
type TItem_60 = 'TItem "TYP" "" TVariation_1

-- | Uap set
type TUap_0 = 'TUapSingle TVariation_42
type TUap_1 = 'TUapMultiple '[ '("plot", TVariation_43), '("track", TVariation_44)]
type TUap_2 = 'TUapMultiple '[ '("uap1", TVariation_39), '("uap2", TVariation_40), '("uap3", TVariation_41), '("uap4", TVariation_38)]

-- | Spec set
type TSpec_0 = 'TCat TUap_0
type TSpec_1 = 'TCat TUap_1
type TSpec_2 = 'TCat TUap_2
type TSpec_3 = 'TRef TVariation_47

-- | Asterix set
type TAsterix_0 = 'TAsterix 0 ('TEdition 1 0) TSpec_0
type TAsterix_1 = 'TAsterix 0 ('TEdition 1 0) TSpec_3
type TAsterix_2 = 'TAsterix 1 ('TEdition 1 0) TSpec_1
type TAsterix_3 = 'TAsterix 2 ('TEdition 1 0) TSpec_2

-- | Aliases
type Cat_000_1_0 = TAsterix_0
type Ref_000_1_0 = TAsterix_1
type Cat_001_1_0 = TAsterix_2
type Cat_002_1_0 = TAsterix_3

-- | Manifest
manifest :: [VAsterix]
manifest =
    [ schema @Cat_000_1_0
    , schema @Ref_000_1_0
    , schema @Cat_001_1_0
    , schema @Cat_002_1_0
    ]

