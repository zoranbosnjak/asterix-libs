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
type TContent_2 = 'ContentTable '[ '( 0, "Plot"), '( 1, "Track")]
type TContent_3 = 'ContentTable '[ '( 0, "Test 0"), '( 1, "Test 1"), '( 2, "Test 2"), '( 3, "Test 3")]
type TContent_4 = 'ContentString 'StringAscii
type TContent_5 = 'ContentString 'StringICAO
type TContent_6 = 'ContentString 'StringOctal
type TContent_7 = 'ContentInteger 'Signed '[ '( 'GreaterThanOrEqualTo, 'TNumInt 'Minus 10), '( 'LessThanOrEqualTo, 'TNumInt 'Plus 10)]
type TContent_8 = 'ContentInteger 'Unsigned '[ '( 'GreaterThanOrEqualTo, 'TNumInt 'Plus 10), '( 'LessThanOrEqualTo, 'TNumInt 'Plus 15)]
type TContent_9 = 'ContentQuantity 'Signed ('TNumDiv ('TNumInt 'Plus 180) ('TNumPow 2 23)) "°" '[ '( 'GreaterThanOrEqualTo, 'TNumInt 'Minus 180), '( 'LessThan, 'TNumInt 'Plus 180)]
type TContent_10 = 'ContentQuantity 'Signed ('TNumDiv ('TNumInt 'Plus 180) ('TNumPow 2 23)) "°" '[ '( 'GreaterThanOrEqualTo, 'TNumInt 'Minus 90), '( 'LessThanOrEqualTo, 'TNumInt 'Plus 90)]
type TContent_11 = 'ContentQuantity 'Unsigned ('TNumInt 'Plus 1) "" '[ ]
type TContent_12 = 'ContentQuantity 'Unsigned ('TNumInt 'Plus 1) "kt" '[ '( 'GreaterThanOrEqualTo, 'TNumInt 'Plus 0), '( 'LessThanOrEqualTo, 'TNumInt 'Plus 1100)]
type TContent_13 = 'ContentQuantity 'Unsigned ('TNumDiv ('TNumInt 'Minus 1) ('TNumInt 'Plus 2)) "" '[ ]
type TContent_14 = 'ContentQuantity 'Unsigned ('TNumDiv ('TNumInt 'Plus 1) ('TNumInt 'Plus 1000)) "Mach" '[ ]
type TContent_15 = 'ContentQuantity 'Unsigned ('TNumDiv ('TNumInt 'Plus 1) ('TNumPow 2 14)) "NM/s" '[ ]
type TContent_16 = 'ContentBds 'BdsWithAddress
type TContent_17 = 'ContentBds ('BdsAt 'Nothing)
type TContent_18 = 'ContentBds ('BdsAt ('Just 48))

-- | Rule set
type TRule_0 = 'TContextFree TContent_0
type TRule_1 = 'TContextFree TContent_1
type TRule_2 = 'TContextFree TContent_2
type TRule_3 = 'TContextFree TContent_3
type TRule_4 = 'TContextFree TContent_4
type TRule_5 = 'TContextFree TContent_5
type TRule_6 = 'TContextFree TContent_6
type TRule_7 = 'TContextFree TContent_7
type TRule_8 = 'TContextFree TContent_8
type TRule_9 = 'TContextFree TContent_9
type TRule_10 = 'TContextFree TContent_10
type TRule_11 = 'TContextFree TContent_11
type TRule_12 = 'TContextFree TContent_12
type TRule_13 = 'TContextFree TContent_13
type TRule_14 = 'TContextFree TContent_16
type TRule_15 = 'TContextFree TContent_17
type TRule_16 = 'TContextFree TContent_18
type TRule_17 = 'TDependent '[ "030", "IM"] '[ '(0, TContent_15), '(1, TContent_14)]

-- | Variation set
type TVariation_0 = 'TElement 0 1 TRule_1
type TVariation_1 = 'TElement 0 1 TRule_2
type TVariation_2 = 'TElement 0 4 TRule_0
type TVariation_3 = 'TElement 0 6 TRule_0
type TVariation_4 = 'TElement 0 7 TRule_0
type TVariation_5 = 'TElement 0 8 TRule_0
type TVariation_6 = 'TElement 0 8 TRule_3
type TVariation_7 = 'TElement 0 8 TRule_7
type TVariation_8 = 'TElement 0 8 TRule_8
type TVariation_9 = 'TElement 0 8 TRule_11
type TVariation_10 = 'TElement 0 8 TRule_13
type TVariation_11 = 'TElement 0 12 TRule_6
type TVariation_12 = 'TElement 0 16 TRule_0
type TVariation_13 = 'TElement 0 16 TRule_12
type TVariation_14 = 'TElement 0 24 TRule_9
type TVariation_15 = 'TElement 0 24 TRule_10
type TVariation_16 = 'TElement 0 32 TRule_0
type TVariation_17 = 'TElement 0 48 TRule_5
type TVariation_18 = 'TElement 0 56 TRule_4
type TVariation_19 = 'TElement 0 56 TRule_15
type TVariation_20 = 'TElement 0 56 TRule_16
type TVariation_21 = 'TElement 0 64 TRule_14
type TVariation_22 = 'TElement 1 6 TRule_0
type TVariation_23 = 'TElement 1 15 TRule_17
type TVariation_24 = 'TElement 4 4 TRule_0
type TVariation_25 = 'TGroup '[ TItem_33, TItem_2, TItem_39, TItem_42, TItem_1, TItem_45]
type TVariation_26 = 'TGroup '[ TItem_34, TItem_3, TItem_40, TItem_4]
type TVariation_27 = 'TGroup '[ TItem_35, TItem_39]
type TVariation_28 = 'TGroup '[ TItem_47, TItem_46]
type TVariation_29 = 'TGroup '[ TItem_53, TItem_59, TItem_54, TItem_55, TItem_56, TItem_0, TItem_37, TItem_41, TItem_48, TItem_49, TItem_50, TItem_51, TItem_52, TItem_30, TItem_31, TItem_32]
type TVariation_30 = 'TGroup '[ TItem_57, TItem_58]
type TVariation_31 = 'TExtended '[ 'Just TItem_34, 'Nothing, 'Just TItem_38, 'Nothing, 'Just TItem_43, 'Nothing]
type TVariation_32 = 'TExtended '[ 'Just TItem_34, 'Nothing, 'Just TItem_38, 'Nothing, 'Just TItem_44]
type TVariation_33 = 'TExtended '[ 'Just TItem_60, 'Just TItem_36, 'Nothing, 'Just TItem_38, 'Nothing]
type TVariation_34 = 'TRepetitive ('Just 1) TVariation_5
type TVariation_35 = 'TRepetitive ('Just 1) TVariation_27
type TVariation_36 = 'TRepetitive 'Nothing TVariation_4
type TVariation_37 = 'TExplicit 'Nothing
type TVariation_38 = 'TExplicit ('Just 'ReservedExpansion)
type TVariation_39 = 'TExplicit ('Just 'SpecialPurpose)
type TVariation_40 = 'TCompound 'Nothing '[ 'CompoundSubitem TItem_5]
type TVariation_41 = 'TCompound 'Nothing '[ 'CompoundSubitem TItem_5, 'CompoundSubitem TItem_25, 'CompoundSubitem TItem_26]
type TVariation_42 = 'TCompound 'Nothing '[ 'CompoundSubitem TItem_5, 'CompoundSubitem TItem_27, 'CompoundSubitem TItem_28]
type TVariation_43 = 'TCompound 'Nothing '[ 'CompoundSubitem TItem_5, 'CompoundSubitem TItem_29]
type TVariation_44 = 'TCompound 'Nothing '[ 'CompoundSubitem TItem_6, 'CompoundSubitem TItem_7, 'CompoundSubitem TItem_9, 'CompoundSubitem TItem_12, 'CompoundSubitem TItem_13, 'CompoundSubitem TItem_14, 'CompoundSubitem TItem_15, 'CompoundSubitem TItem_16, 'CompoundSubitem TItem_17, 'CompoundSubitem TItem_18, 'CompoundSubitem TItem_19, 'CompoundSubitem TItem_20, 'CompoundSubitem TItem_21, 'CompoundSubitem TItem_22, 'CompoundRFS, 'CompoundSubitem TItem_23, 'CompoundSubitem TItem_24]
type TVariation_45 = 'TCompound 'Nothing '[ 'CompoundSubitem TItem_6, 'CompoundSubitem TItem_8, 'CompoundSubitem TItem_10]
type TVariation_46 = 'TCompound 'Nothing '[ 'CompoundSubitem TItem_6, 'CompoundSubitem TItem_8, 'CompoundSubitem TItem_11]
type TVariation_47 = 'TCompound 'Nothing '[ 'CompoundSubitem TItem_35, 'CompoundSpare, 'CompoundSubitem TItem_39]
type TVariation_48 = 'TCompound ('Just 1) '[ 'CompoundSubitem TItem_35, 'CompoundSubitem TItem_39]
type TVariation_49 = 'TCompound ('Just 1) '[ 'CompoundSubitem TItem_35, 'CompoundSpare, 'CompoundSubitem TItem_39]
type TVariation_50 = 'TCompound ('Just 2) '[ 'CompoundSubitem TItem_35, 'CompoundSpare, 'CompoundSpare, 'CompoundSubitem TItem_39, 'CompoundSpare, 'CompoundSpare, 'CompoundSpare, 'CompoundSpare, 'CompoundSpare, 'CompoundSpare, 'CompoundSubitem TItem_44]

-- | Item set
type TItem_0 = 'TSpare 4 4
type TItem_1 = 'TSpare 4 8
type TItem_2 = 'TSpare 6 2
type TItem_3 = 'TSpare 7 2
type TItem_4 = 'TSpare 7 9
type TItem_5 = 'TItem "010" "" TVariation_5
type TItem_6 = 'TItem "010" "Data Source Identifier" TVariation_30
type TItem_7 = 'TItem "020" "Different Contents" TVariation_29
type TItem_8 = 'TItem "020" "Target Report Descriptor" TVariation_33
type TItem_9 = 'TItem "030" "Dependent Item" TVariation_28
type TItem_10 = 'TItem "031" "For Plots Only" TVariation_5
type TItem_11 = 'TItem "032" "For Tracks Only" TVariation_12
type TItem_12 = 'TItem "040" "Spare Items" TVariation_26
type TItem_13 = 'TItem "051" "Element" TVariation_5
type TItem_14 = 'TItem "052" "Group" TVariation_25
type TItem_15 = 'TItem "053" "Extended With Trailing Fx" TVariation_31
type TItem_16 = 'TItem "054" "Extended Without Trailing Fx" TVariation_32
type TItem_17 = 'TItem "061" "Repetitive Regular" TVariation_34
type TItem_18 = 'TItem "062" "Repetitive With Group" TVariation_35
type TItem_19 = 'TItem "063" "Repetitive Fx" TVariation_36
type TItem_20 = 'TItem "071" "Explicit None" TVariation_37
type TItem_21 = 'TItem "072" "Explicit RE" TVariation_38
type TItem_22 = 'TItem "073" "Explicit SP" TVariation_39
type TItem_23 = 'TItem "091" "Compound Fspec With Fx" TVariation_47
type TItem_24 = 'TItem "092" "Compound Fixed Size Fspec" TVariation_49
type TItem_25 = 'TItem "101" "" TVariation_5
type TItem_26 = 'TItem "102" "" TVariation_5
type TItem_27 = 'TItem "201" "" TVariation_12
type TItem_28 = 'TItem "202" "" TVariation_12
type TItem_29 = 'TItem "301" "" TVariation_16
type TItem_30 = 'TItem "B1" "Bds With Address" TVariation_21
type TItem_31 = 'TItem "B2" "Bds At Unknown Address" TVariation_19
type TItem_32 = 'TItem "B3" "Bds At Known Address" TVariation_20
type TItem_33 = 'TItem "I1" "" TVariation_3
type TItem_34 = 'TItem "I1" "" TVariation_4
type TItem_35 = 'TItem "I1" "" TVariation_5
type TItem_36 = 'TItem "I1" "" TVariation_22
type TItem_37 = 'TItem "I1" "Unsigned Integer" TVariation_8
type TItem_38 = 'TItem "I2" "" TVariation_4
type TItem_39 = 'TItem "I2" "" TVariation_5
type TItem_40 = 'TItem "I2" "" TVariation_22
type TItem_41 = 'TItem "I2" "Signed Integer" TVariation_7
type TItem_42 = 'TItem "I3" "" TVariation_2
type TItem_43 = 'TItem "I3" "" TVariation_4
type TItem_44 = 'TItem "I3" "" TVariation_5
type TItem_45 = 'TItem "I4" "" TVariation_24
type TItem_46 = 'TItem "IAS" "" TVariation_23
type TItem_47 = 'TItem "IM" "" TVariation_0
type TItem_48 = 'TItem "Q1LAT" "Latitude in WGS.84 in Two's Complement Form" TVariation_15
type TItem_49 = 'TItem "Q2LON" "Longitude in WGS.84 in Two's Complement Form" TVariation_14
type TItem_50 = 'TItem "Q3" "Unsigned Quantity" TVariation_13
type TItem_51 = 'TItem "Q4" "Quantity No Unit" TVariation_9
type TItem_52 = 'TItem "Q5" "Negative Lsb" TVariation_10
type TItem_53 = 'TItem "R" "Raw" TVariation_5
type TItem_54 = 'TItem "S1" "String Ascii" TVariation_18
type TItem_55 = 'TItem "S2" "String ICAO" TVariation_17
type TItem_56 = 'TItem "S3" "String Octal" TVariation_11
type TItem_57 = 'TItem "SAC" "System Area Code" TVariation_5
type TItem_58 = 'TItem "SIC" "System Identification Code" TVariation_5
type TItem_59 = 'TItem "T" "Table" TVariation_6
type TItem_60 = 'TItem "TYP" "" TVariation_1

-- | Record set
type TRecord_0 = 'Record '[ 'CompoundSubitem TItem_5]
type TRecord_1 = 'Record '[ 'CompoundSubitem TItem_5, 'CompoundSubitem TItem_25, 'CompoundSubitem TItem_26]
type TRecord_2 = 'Record '[ 'CompoundSubitem TItem_5, 'CompoundSubitem TItem_27, 'CompoundSubitem TItem_28]
type TRecord_3 = 'Record '[ 'CompoundSubitem TItem_5, 'CompoundSubitem TItem_29]
type TRecord_4 = 'Record '[ 'CompoundSubitem TItem_6, 'CompoundSubitem TItem_7, 'CompoundSubitem TItem_9, 'CompoundSubitem TItem_12, 'CompoundSubitem TItem_13, 'CompoundSubitem TItem_14, 'CompoundSubitem TItem_15, 'CompoundSubitem TItem_16, 'CompoundSubitem TItem_17, 'CompoundSubitem TItem_18, 'CompoundSubitem TItem_19, 'CompoundSubitem TItem_20, 'CompoundSubitem TItem_21, 'CompoundSubitem TItem_22, 'CompoundRFS, 'CompoundSubitem TItem_23, 'CompoundSubitem TItem_24]
type TRecord_5 = 'Record '[ 'CompoundSubitem TItem_6, 'CompoundSubitem TItem_8, 'CompoundSubitem TItem_10]
type TRecord_6 = 'Record '[ 'CompoundSubitem TItem_6, 'CompoundSubitem TItem_8, 'CompoundSubitem TItem_11]

-- | Expansion set
type TExpansion_0 = 'Expansion 1 '[ 'CompoundSubitem TItem_35, 'CompoundSubitem TItem_39]
type TExpansion_1 = 'Expansion 2 '[ 'CompoundSubitem TItem_35, 'CompoundSpare, 'CompoundSpare, 'CompoundSubitem TItem_39, 'CompoundSpare, 'CompoundSpare, 'CompoundSpare, 'CompoundSpare, 'CompoundSpare, 'CompoundSpare, 'CompoundSubitem TItem_44]

-- | Uap set
type TUap_0 = 'TUapSingle TRecord_4
type TUap_1 = 'TUapMultiple '[ '("plot", TRecord_5), '("track", TRecord_6)]
type TUap_2 = 'TUapMultiple '[ '("uap1", TRecord_1), '("uap2", TRecord_2), '("uap3", TRecord_3), '("uap4", TRecord_0)]

-- | Asterix spec set
type TAstSpec_0 = 'TCat 0 ('Edition 1 0) TUap_0
type TAstSpec_1 = 'TRef 0 ('Edition 1 0) TExpansion_0
type TAstSpec_2 = 'TRef 0 ('Edition 1 1) TExpansion_1
type TAstSpec_3 = 'TCat 1 ('Edition 1 0) TUap_1
type TAstSpec_4 = 'TCat 2 ('Edition 1 0) TUap_2

-- | Aliases
type Cat_000_1_0 = TAstSpec_0
type Ref_000_1_0 = TAstSpec_1
type Ref_000_1_1 = TAstSpec_2
type Cat_001_1_0 = TAstSpec_3
type Cat_002_1_0 = TAstSpec_4

-- | Manifest
manifest :: [Some VAstSpec]
manifest =
    [ schema @Cat_000_1_0
    , schema @Ref_000_1_0
    , schema @Ref_000_1_1
    , schema @Cat_001_1_0
    , schema @Cat_002_1_0
    ]

