-- | Asterix specifications

-- This file is generated, DO NOT EDIT!
-- For more details, see:
--    - https://github.com/zoranbosnjak/asterix-specs

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- Types are BIG, disable depth checking.
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Generated where

import           Asterix.Schema

reference :: Text
reference = "unknown"

version :: Text
version = "19700101.0"

-- | Content set
type TContent_0 = 'ContentRaw
type TContent_1 = 'ContentTable '[ '( 0, "Air Speed = IAS, LSB (Bit-1) = 2^-14 NM/s"), '( 1, "Air Speed = Mach, LSB (Bit-1) = 0.001")]
type TContent_2 = 'ContentTable '[ '( 0, "LOW"), '( 1, "HIGH")]
type TContent_3 = 'ContentTable '[ '( 0, "Plot"), '( 1, "Track")]
type TContent_4 = 'ContentTable '[ '( 0, "Test 0"), '( 1, "Test 1"), '( 2, "Test 2"), '( 3, "Test 3")]
type TContent_5 = 'ContentTable '[ '( 0, "Test0"), '( 1, "Test1")]
type TContent_6 = 'ContentTable '[ '( 0, "Test0"), '( 1, "Test1"), '( 2, "Test2")]
type TContent_7 = 'ContentTable '[ '( 1, "Message 1"), '( 2, "Message 2"), '( 3, "Message 3")]
type TContent_8 = 'ContentTable '[ '( 3, "Test3"), '( 4, "Test4")]
type TContent_9 = 'ContentString 'StringAscii
type TContent_10 = 'ContentString 'StringICAO
type TContent_11 = 'ContentString 'StringOctal
type TContent_12 = 'ContentInteger 'Signed '[ 'GreaterThanOrEqualTo ('TNumInt 'Minus 10), 'LessThanOrEqualTo ('TNumInt 'Plus 10)]
type TContent_13 = 'ContentInteger 'Unsigned '[ 'GreaterThanOrEqualTo ('TNumInt 'Plus 10), 'LessThanOrEqualTo ('TNumInt 'Plus 15)]
type TContent_14 = 'ContentQuantity 'Signed ('TNumDiv ('TNumInt 'Plus 180) ('TNumPow 2 23)) "°" '[ 'GreaterThanOrEqualTo ('TNumInt 'Minus 180), 'LessThan ('TNumInt 'Plus 180)]
type TContent_15 = 'ContentQuantity 'Signed ('TNumDiv ('TNumInt 'Plus 180) ('TNumPow 2 23)) "°" '[ 'GreaterThanOrEqualTo ('TNumInt 'Minus 90), 'LessThanOrEqualTo ('TNumInt 'Plus 90)]
type TContent_16 = 'ContentQuantity 'Unsigned ('TNumInt 'Plus 1) "" '[ ]
type TContent_17 = 'ContentQuantity 'Unsigned ('TNumInt 'Plus 1) "kt" '[ 'GreaterThanOrEqualTo ('TNumInt 'Plus 0), 'LessThanOrEqualTo ('TNumInt 'Plus 1100)]
type TContent_18 = 'ContentQuantity 'Unsigned ('TNumDiv ('TNumInt 'Minus 1) ('TNumInt 'Plus 2)) "" '[ ]
type TContent_19 = 'ContentQuantity 'Unsigned ('TNumDiv ('TNumInt 'Plus 1) ('TNumInt 'Plus 1000)) "Mach" '[ ]
type TContent_20 = 'ContentQuantity 'Unsigned ('TNumDiv ('TNumInt 'Plus 1) ('TNumPow 2 14)) "NM/s" '[ ]
type TContent_21 = 'ContentBds 'BdsWithAddress
type TContent_22 = 'ContentBds ('BdsAt 'Nothing)
type TContent_23 = 'ContentBds ('BdsAt ('Just 48))

-- | Rule Content set
type TRuleContent_0 = 'TContextFree TContent_0
type TRuleContent_1 = 'TContextFree TContent_1
type TRuleContent_2 = 'TContextFree TContent_2
type TRuleContent_3 = 'TContextFree TContent_3
type TRuleContent_4 = 'TContextFree TContent_4
type TRuleContent_5 = 'TContextFree TContent_5
type TRuleContent_6 = 'TContextFree TContent_6
type TRuleContent_7 = 'TContextFree TContent_7
type TRuleContent_8 = 'TContextFree TContent_8
type TRuleContent_9 = 'TContextFree TContent_9
type TRuleContent_10 = 'TContextFree TContent_10
type TRuleContent_11 = 'TContextFree TContent_11
type TRuleContent_12 = 'TContextFree TContent_12
type TRuleContent_13 = 'TContextFree TContent_13
type TRuleContent_14 = 'TContextFree TContent_14
type TRuleContent_15 = 'TContextFree TContent_15
type TRuleContent_16 = 'TContextFree TContent_16
type TRuleContent_17 = 'TContextFree TContent_17
type TRuleContent_18 = 'TContextFree TContent_18
type TRuleContent_19 = 'TContextFree TContent_21
type TRuleContent_20 = 'TContextFree TContent_22
type TRuleContent_21 = 'TContextFree TContent_23
type TRuleContent_22 = 'TDependent '[ '[ "030", "IM"]] TContent_0 '[ '( '[ 0], TContent_20), '( '[ 1], TContent_19)]

-- | Variation set
type TVariation_0 = 'TElement 0 1 TRuleContent_1
type TVariation_1 = 'TElement 0 1 TRuleContent_3
type TVariation_2 = 'TElement 0 4 TRuleContent_0
type TVariation_3 = 'TElement 0 6 TRuleContent_0
type TVariation_4 = 'TElement 0 7 TRuleContent_0
type TVariation_5 = 'TElement 0 8 TRuleContent_0
type TVariation_6 = 'TElement 0 8 TRuleContent_4
type TVariation_7 = 'TElement 0 8 TRuleContent_7
type TVariation_8 = 'TElement 0 8 TRuleContent_12
type TVariation_9 = 'TElement 0 8 TRuleContent_13
type TVariation_10 = 'TElement 0 8 TRuleContent_16
type TVariation_11 = 'TElement 0 8 TRuleContent_18
type TVariation_12 = 'TElement 0 12 TRuleContent_11
type TVariation_13 = 'TElement 0 16 TRuleContent_0
type TVariation_14 = 'TElement 0 16 TRuleContent_17
type TVariation_15 = 'TElement 0 24 TRuleContent_14
type TVariation_16 = 'TElement 0 24 TRuleContent_15
type TVariation_17 = 'TElement 0 32 TRuleContent_0
type TVariation_18 = 'TElement 0 48 TRuleContent_10
type TVariation_19 = 'TElement 0 56 TRuleContent_9
type TVariation_20 = 'TElement 0 56 TRuleContent_20
type TVariation_21 = 'TElement 0 56 TRuleContent_21
type TVariation_22 = 'TElement 0 64 TRuleContent_19
type TVariation_23 = 'TElement 1 6 TRuleContent_0
type TVariation_24 = 'TElement 1 15 TRuleContent_22
type TVariation_25 = 'TElement 4 1 TRuleContent_5
type TVariation_26 = 'TElement 4 3 TRuleContent_0
type TVariation_27 = 'TElement 4 3 TRuleContent_6
type TVariation_28 = 'TElement 4 3 TRuleContent_8
type TVariation_29 = 'TElement 4 4 TRuleContent_0
type TVariation_30 = 'TElement 7 1 TRuleContent_2
type TVariation_31 = 'TGroup '[ TItem_39, TItem_3, TItem_46, TItem_49, TItem_1, TItem_52]
type TVariation_32 = 'TGroup '[ TItem_40, TItem_4, TItem_47, TItem_5]
type TVariation_33 = 'TGroup '[ TItem_41, TItem_46]
type TVariation_34 = 'TGroup '[ TItem_43, TItem_2]
type TVariation_35 = 'TGroup '[ TItem_54, TItem_53]
type TVariation_36 = 'TGroup '[ TItem_60, TItem_66, TItem_61, TItem_62, TItem_63, TItem_0, TItem_44, TItem_48, TItem_55, TItem_56, TItem_57, TItem_58, TItem_59, TItem_33, TItem_34, TItem_35]
type TVariation_37 = 'TGroup '[ TItem_64, TItem_65]
type TVariation_38 = 'TGroup '[ TItem_67, TItem_37, TItem_38]
type TVariation_39 = 'TExtended '[ 'Just TItem_40, 'Nothing, 'Just TItem_45, 'Nothing, 'Just TItem_50, 'Nothing]
type TVariation_40 = 'TExtended '[ 'Just TItem_40, 'Nothing, 'Just TItem_45, 'Nothing, 'Just TItem_51]
type TVariation_41 = 'TExtended '[ 'Just TItem_68, 'Just TItem_42, 'Nothing, 'Just TItem_45, 'Nothing]
type TVariation_42 = 'TRepetitive ('Just 1) TVariation_5
type TVariation_43 = 'TRepetitive ('Just 1) TVariation_33
type TVariation_44 = 'TRepetitive 'Nothing TVariation_4
type TVariation_45 = 'TExplicit 'Nothing
type TVariation_46 = 'TExplicit ('Just 'ReservedExpansion)
type TVariation_47 = 'TExplicit ('Just 'SpecialPurpose)
type TVariation_48 = 'TCompound '[ 'Just TItem_41, 'Nothing, 'Just TItem_46]
type TVariation_49 = 'TCompound '[ 'Just TItem_41, 'Just TItem_36]

-- | Rule Variation set
type TRuleVariation_0 = 'TContextFree TVariation_0
type TRuleVariation_1 = 'TContextFree TVariation_1
type TRuleVariation_2 = 'TContextFree TVariation_2
type TRuleVariation_3 = 'TContextFree TVariation_3
type TRuleVariation_4 = 'TContextFree TVariation_4
type TRuleVariation_5 = 'TContextFree TVariation_5
type TRuleVariation_6 = 'TContextFree TVariation_6
type TRuleVariation_7 = 'TContextFree TVariation_7
type TRuleVariation_8 = 'TContextFree TVariation_8
type TRuleVariation_9 = 'TContextFree TVariation_9
type TRuleVariation_10 = 'TContextFree TVariation_10
type TRuleVariation_11 = 'TContextFree TVariation_11
type TRuleVariation_12 = 'TContextFree TVariation_12
type TRuleVariation_13 = 'TContextFree TVariation_13
type TRuleVariation_14 = 'TContextFree TVariation_14
type TRuleVariation_15 = 'TContextFree TVariation_15
type TRuleVariation_16 = 'TContextFree TVariation_16
type TRuleVariation_17 = 'TContextFree TVariation_17
type TRuleVariation_18 = 'TContextFree TVariation_18
type TRuleVariation_19 = 'TContextFree TVariation_19
type TRuleVariation_20 = 'TContextFree TVariation_20
type TRuleVariation_21 = 'TContextFree TVariation_21
type TRuleVariation_22 = 'TContextFree TVariation_22
type TRuleVariation_23 = 'TContextFree TVariation_23
type TRuleVariation_24 = 'TContextFree TVariation_24
type TRuleVariation_25 = 'TContextFree TVariation_25
type TRuleVariation_26 = 'TContextFree TVariation_29
type TRuleVariation_27 = 'TContextFree TVariation_30
type TRuleVariation_28 = 'TContextFree TVariation_31
type TRuleVariation_29 = 'TContextFree TVariation_32
type TRuleVariation_30 = 'TContextFree TVariation_35
type TRuleVariation_31 = 'TContextFree TVariation_36
type TRuleVariation_32 = 'TContextFree TVariation_37
type TRuleVariation_33 = 'TContextFree TVariation_38
type TRuleVariation_34 = 'TContextFree TVariation_39
type TRuleVariation_35 = 'TContextFree TVariation_40
type TRuleVariation_36 = 'TContextFree TVariation_41
type TRuleVariation_37 = 'TContextFree TVariation_42
type TRuleVariation_38 = 'TContextFree TVariation_43
type TRuleVariation_39 = 'TContextFree TVariation_44
type TRuleVariation_40 = 'TContextFree TVariation_45
type TRuleVariation_41 = 'TContextFree TVariation_46
type TRuleVariation_42 = 'TContextFree TVariation_47
type TRuleVariation_43 = 'TContextFree TVariation_48
type TRuleVariation_44 = 'TContextFree TVariation_49
type TRuleVariation_45 = 'TDependent '[ '[ "000"], '[ "031", "CC", "TID"]] TVariation_26 '[ '( '[ 1, 1], TVariation_27), '( '[ 1, 2], TVariation_28), '( '[ 2, 1], TVariation_34)]

-- | Item set
type TItem_0 = 'TSpare 4 4
type TItem_1 = 'TSpare 4 8
type TItem_2 = 'TSpare 5 2
type TItem_3 = 'TSpare 6 2
type TItem_4 = 'TSpare 7 2
type TItem_5 = 'TSpare 7 9
type TItem_6 = 'TItem "000" "Message Type" TRuleVariation_7
type TItem_7 = 'TItem "010" "" TRuleVariation_5
type TItem_8 = 'TItem "010" "Data Source Identifier" TRuleVariation_32
type TItem_9 = 'TItem "010" "Data Source Identifier" TRuleVariation_32
type TItem_10 = 'TItem "020" "Different Contents" TRuleVariation_31
type TItem_11 = 'TItem "020" "Target Report Descriptor" TRuleVariation_36
type TItem_12 = 'TItem "030" "Dependent Item" TRuleVariation_30
type TItem_13 = 'TItem "031" "For Plots Only" TRuleVariation_5
type TItem_14 = 'TItem "031" "Nested Dependent Item" TRuleVariation_44
type TItem_15 = 'TItem "032" "For Tracks Only" TRuleVariation_13
type TItem_16 = 'TItem "040" "Spare Items" TRuleVariation_29
type TItem_17 = 'TItem "051" "Element" TRuleVariation_5
type TItem_18 = 'TItem "052" "Group" TRuleVariation_28
type TItem_19 = 'TItem "053" "Extended With Trailing Fx" TRuleVariation_34
type TItem_20 = 'TItem "054" "Extended Without Trailing Fx" TRuleVariation_35
type TItem_21 = 'TItem "061" "Repetitive Regular" TRuleVariation_37
type TItem_22 = 'TItem "062" "Repetitive With Group" TRuleVariation_38
type TItem_23 = 'TItem "063" "Repetitive Fx" TRuleVariation_39
type TItem_24 = 'TItem "071" "Explicit None" TRuleVariation_40
type TItem_25 = 'TItem "072" "Explicit RE" TRuleVariation_41
type TItem_26 = 'TItem "073" "Explicit SP" TRuleVariation_42
type TItem_27 = 'TItem "091" "Compound Fspec With Fx" TRuleVariation_43
type TItem_28 = 'TItem "101" "" TRuleVariation_5
type TItem_29 = 'TItem "102" "" TRuleVariation_5
type TItem_30 = 'TItem "201" "" TRuleVariation_13
type TItem_31 = 'TItem "202" "" TRuleVariation_13
type TItem_32 = 'TItem "301" "" TRuleVariation_17
type TItem_33 = 'TItem "B1" "Bds With Address" TRuleVariation_22
type TItem_34 = 'TItem "B2" "Bds At Unknown Address" TRuleVariation_20
type TItem_35 = 'TItem "B3" "Bds At Known Address" TRuleVariation_21
type TItem_36 = 'TItem "CC" "Conflict Classification" TRuleVariation_33
type TItem_37 = 'TItem "CP" "Conflict Properties Class" TRuleVariation_45
type TItem_38 = 'TItem "CS" "Conflict Severity" TRuleVariation_27
type TItem_39 = 'TItem "I1" "" TRuleVariation_3
type TItem_40 = 'TItem "I1" "" TRuleVariation_4
type TItem_41 = 'TItem "I1" "" TRuleVariation_5
type TItem_42 = 'TItem "I1" "" TRuleVariation_23
type TItem_43 = 'TItem "I1" "" TRuleVariation_25
type TItem_44 = 'TItem "I1" "Unsigned Integer" TRuleVariation_9
type TItem_45 = 'TItem "I2" "" TRuleVariation_4
type TItem_46 = 'TItem "I2" "" TRuleVariation_5
type TItem_47 = 'TItem "I2" "" TRuleVariation_23
type TItem_48 = 'TItem "I2" "Signed Integer" TRuleVariation_8
type TItem_49 = 'TItem "I3" "" TRuleVariation_2
type TItem_50 = 'TItem "I3" "" TRuleVariation_4
type TItem_51 = 'TItem "I3" "" TRuleVariation_5
type TItem_52 = 'TItem "I4" "" TRuleVariation_26
type TItem_53 = 'TItem "IAS" "" TRuleVariation_24
type TItem_54 = 'TItem "IM" "" TRuleVariation_0
type TItem_55 = 'TItem "Q1LAT" "Latitude in WGS.84 in Two's Complement Form" TRuleVariation_16
type TItem_56 = 'TItem "Q2LON" "Longitude in WGS.84 in Two's Complement Form" TRuleVariation_15
type TItem_57 = 'TItem "Q3" "Unsigned Quantity" TRuleVariation_14
type TItem_58 = 'TItem "Q4" "Quantity No Unit" TRuleVariation_10
type TItem_59 = 'TItem "Q5" "Negative Lsb" TRuleVariation_11
type TItem_60 = 'TItem "R" "Raw" TRuleVariation_5
type TItem_61 = 'TItem "S1" "String Ascii" TRuleVariation_19
type TItem_62 = 'TItem "S2" "String ICAO" TRuleVariation_18
type TItem_63 = 'TItem "S3" "String Octal" TRuleVariation_12
type TItem_64 = 'TItem "SAC" "System Area Code" TRuleVariation_5
type TItem_65 = 'TItem "SIC" "System Identification Code" TRuleVariation_5
type TItem_66 = 'TItem "T" "Table" TRuleVariation_6
type TItem_67 = 'TItem "TID" "Identification of Conflict Categories Definition Table" TRuleVariation_2
type TItem_68 = 'TItem "TYP" "" TRuleVariation_1

-- | Record set
type TRecord_0 = 'GRecord '[ 'UapItem TItem_7]
type TRecord_1 = 'GRecord '[ 'UapItem TItem_7, 'UapItem TItem_28, 'UapItem TItem_29]
type TRecord_2 = 'GRecord '[ 'UapItem TItem_7, 'UapItem TItem_30, 'UapItem TItem_31]
type TRecord_3 = 'GRecord '[ 'UapItem TItem_7, 'UapItem TItem_32]
type TRecord_4 = 'GRecord '[ 'UapItem TItem_8, 'UapItem TItem_11, 'UapItem TItem_13]
type TRecord_5 = 'GRecord '[ 'UapItem TItem_8, 'UapItem TItem_11, 'UapItem TItem_15]
type TRecord_6 = 'GRecord '[ 'UapItem TItem_9, 'UapItem TItem_6, 'UapItem TItem_10, 'UapItem TItem_12, 'UapItem TItem_14, 'UapItem TItem_16, 'UapItem TItem_17, 'UapItem TItem_18, 'UapItem TItem_19, 'UapItem TItem_20, 'UapItem TItem_21, 'UapItem TItem_22, 'UapItem TItem_23, 'UapItem TItem_24, 'UapItem TItem_25, 'UapItem TItem_26, 'UapItemRFS, 'UapItem TItem_27]

-- | Expansion set
type TExpansion_0 = 'GExpansion 1 '[ 'Just TItem_41, 'Just TItem_46]
type TExpansion_1 = 'GExpansion 16 '[ 'Just TItem_41, 'Nothing, 'Nothing, 'Just TItem_46, 'Nothing, 'Nothing, 'Nothing, 'Nothing, 'Nothing, 'Nothing, 'Just TItem_51]

-- | Uap set
type TUap_0 = 'TUapSingle TRecord_6
type TUap_1 = 'TUapMultiple '[ '( "plot", TRecord_4), '( "track", TRecord_5)]
type TUap_2 = 'TUapMultiple '[ '( "uap1", TRecord_1), '( "uap2", TRecord_2), '( "uap3", TRecord_3), '( "uap4", TRecord_0)]

-- | Asterix spec set
type TAsterix_0 = 'TBasic 0 ('Edition 1 0) TUap_0
type TAsterix_1 = 'TExpansion 0 ('Edition 1 0) TExpansion_0
type TAsterix_2 = 'TExpansion 0 ('Edition 1 1) TExpansion_1
type TAsterix_3 = 'TBasic 1 ('Edition 1 0) TUap_1
type TAsterix_4 = 'TBasic 2 ('Edition 1 0) TUap_2

-- | Aliases
type Cat_000_1_0 = TAsterix_0
type Ref_000_1_0 = TAsterix_1
type Ref_000_1_1 = TAsterix_2
type Cat_001_1_0 = TAsterix_3
type Cat_002_1_0 = TAsterix_4

-- | Manifest
manifest :: [Some VAsterix]
manifest =
    [ schema @Cat_000_1_0
    , schema @Ref_000_1_0
    , schema @Ref_000_1_1
    , schema @Cat_001_1_0
    , schema @Cat_002_1_0
    ]

