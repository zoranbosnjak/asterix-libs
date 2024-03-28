# Asterix specifications

# This file is generated, DO NOT EDIT!
# For more details, see:
#     - https://github.com/zoranbosnjak/asterix-specs

from asterix.base import *

reference = "unknown"
version = "19700101.0"

# Asterix types

class Content_0(ContentRaw):
    pass

class RuleContent_0(RuleContentContextFree):
    variation = Content_0

class Variation_5(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_0

class RuleVariation_5(RuleVariationContextFree):
    variation = Variation_5

class NonSpare_58(NonSpare):
    name = "SAC"
    title = "System Area Code"
    rule = RuleVariation_5

class Item_36(Item):
    non_spare = NonSpare_58

class NonSpare_59(NonSpare):
    name = "SIC"
    title = "System Identification Code"
    rule = RuleVariation_5

class Item_37(Item):
    non_spare = NonSpare_59

class Variation_37(Group):
    bit_size = 16
    items_list = [Item_36, Item_37]
    items_dict = {"SAC": RuleVariation_5, "SIC": RuleVariation_5}

class RuleVariation_32(RuleVariationContextFree):
    variation = Variation_37

class NonSpare_3(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_32

class Content_7(ContentTable):
    values = {1: "Message 1", 2: "Message 2", 3: "Message 3"}

class RuleContent_7(RuleContentContextFree):
    variation = Content_7

class Variation_7(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_7

class RuleVariation_7(RuleVariationContextFree):
    variation = Variation_7

class NonSpare_0(NonSpare):
    name = "000"
    title = "Message Type"
    rule = RuleVariation_7

class NonSpare_54(NonSpare):
    name = "R"
    title = "Raw"
    rule = RuleVariation_5

class Item_32(Item):
    non_spare = NonSpare_54

class Content_4(ContentTable):
    values = {0: "Test 0", 1: "Test 1", 2: "Test 2", 3: "Test 3"}

class RuleContent_4(RuleContentContextFree):
    variation = Content_4

class Variation_6(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_4

class RuleVariation_6(RuleVariationContextFree):
    variation = Variation_6

class NonSpare_60(NonSpare):
    name = "T"
    title = "Table"
    rule = RuleVariation_6

class Item_38(Item):
    non_spare = NonSpare_60

class Content_9(ContentString):
    string_type = StringAscii

class RuleContent_9(RuleContentContextFree):
    variation = Content_9

class Variation_19(Element):
    bit_offset8 = 0
    bit_size = 56
    rule = RuleContent_9

class RuleVariation_19(RuleVariationContextFree):
    variation = Variation_19

class NonSpare_55(NonSpare):
    name = "S1"
    title = "String Ascii"
    rule = RuleVariation_19

class Item_33(Item):
    non_spare = NonSpare_55

class Content_10(ContentString):
    string_type = StringICAO

class RuleContent_10(RuleContentContextFree):
    variation = Content_10

class Variation_18(Element):
    bit_offset8 = 0
    bit_size = 48
    rule = RuleContent_10

class RuleVariation_18(RuleVariationContextFree):
    variation = Variation_18

class NonSpare_56(NonSpare):
    name = "S2"
    title = "String ICAO"
    rule = RuleVariation_18

class Item_34(Item):
    non_spare = NonSpare_56

class Content_11(ContentString):
    string_type = StringOctal

class RuleContent_11(RuleContentContextFree):
    variation = Content_11

class Variation_12(Element):
    bit_offset8 = 0
    bit_size = 12
    rule = RuleContent_11

class RuleVariation_12(RuleVariationContextFree):
    variation = Variation_12

class NonSpare_57(NonSpare):
    name = "S3"
    title = "String Octal"
    rule = RuleVariation_12

class Item_35(Item):
    non_spare = NonSpare_57

class Item_0(Spare):
    bit_offset8 = 4
    bit_size = 4

class Content_13(ContentInteger):
    signedness = Unsigned

class RuleContent_13(RuleContentContextFree):
    variation = Content_13

class Variation_9(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_13

class RuleVariation_9(RuleVariationContextFree):
    variation = Variation_9

class NonSpare_38(NonSpare):
    name = "I1"
    title = "Unsigned Integer"
    rule = RuleVariation_9

class Item_16(Item):
    non_spare = NonSpare_38

class Content_12(ContentInteger):
    signedness = Signed

class RuleContent_12(RuleContentContextFree):
    variation = Content_12

class Variation_8(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_12

class RuleVariation_8(RuleVariationContextFree):
    variation = Variation_8

class NonSpare_42(NonSpare):
    name = "I2"
    title = "Signed Integer"
    rule = RuleVariation_8

class Item_20(Item):
    non_spare = NonSpare_42

class Content_15(ContentQuantity):
    signedness = Signed
    lsb = 2.1457672119140625e-5
    unit = "°"

class RuleContent_15(RuleContentContextFree):
    variation = Content_15

class Variation_16(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_15

class RuleVariation_16(RuleVariationContextFree):
    variation = Variation_16

class NonSpare_49(NonSpare):
    name = "Q1LAT"
    title = "Latitude in WGS.84 in Two's Complement Form"
    rule = RuleVariation_16

class Item_27(Item):
    non_spare = NonSpare_49

class Content_14(ContentQuantity):
    signedness = Signed
    lsb = 2.1457672119140625e-5
    unit = "°"

class RuleContent_14(RuleContentContextFree):
    variation = Content_14

class Variation_15(Element):
    bit_offset8 = 0
    bit_size = 24
    rule = RuleContent_14

class RuleVariation_15(RuleVariationContextFree):
    variation = Variation_15

class NonSpare_50(NonSpare):
    name = "Q2LON"
    title = "Longitude in WGS.84 in Two's Complement Form"
    rule = RuleVariation_15

class Item_28(Item):
    non_spare = NonSpare_50

class Content_17(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = "kt"

class RuleContent_17(RuleContentContextFree):
    variation = Content_17

class Variation_14(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_17

class RuleVariation_14(RuleVariationContextFree):
    variation = Variation_14

class NonSpare_51(NonSpare):
    name = "Q3"
    title = "Unsigned Quantity"
    rule = RuleVariation_14

class Item_29(Item):
    non_spare = NonSpare_51

class Content_16(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0
    unit = ""

class RuleContent_16(RuleContentContextFree):
    variation = Content_16

class Variation_10(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_16

class RuleVariation_10(RuleVariationContextFree):
    variation = Variation_10

class NonSpare_52(NonSpare):
    name = "Q4"
    title = "Quantity No Unit"
    rule = RuleVariation_10

class Item_30(Item):
    non_spare = NonSpare_52

class Content_18(ContentQuantity):
    signedness = Unsigned
    lsb = -0.5
    unit = ""

class RuleContent_18(RuleContentContextFree):
    variation = Content_18

class Variation_11(Element):
    bit_offset8 = 0
    bit_size = 8
    rule = RuleContent_18

class RuleVariation_11(RuleVariationContextFree):
    variation = Variation_11

class NonSpare_53(NonSpare):
    name = "Q5"
    title = "Negative Lsb"
    rule = RuleVariation_11

class Item_31(Item):
    non_spare = NonSpare_53

class Content_21(ContentBds):
    bds_type = BdsWithAddress

class RuleContent_19(RuleContentContextFree):
    variation = Content_21

class Variation_22(Element):
    bit_offset8 = 0
    bit_size = 64
    rule = RuleContent_19

class RuleVariation_22(RuleVariationContextFree):
    variation = Variation_22

class NonSpare_27(NonSpare):
    name = "B1"
    title = "Bds With Address"
    rule = RuleVariation_22

class Item_6(Item):
    non_spare = NonSpare_27

class Content_22(ContentBds):
    bds_type = (BdsAt, None)

class RuleContent_20(RuleContentContextFree):
    variation = Content_22

class Variation_20(Element):
    bit_offset8 = 0
    bit_size = 56
    rule = RuleContent_20

class RuleVariation_20(RuleVariationContextFree):
    variation = Variation_20

class NonSpare_28(NonSpare):
    name = "B2"
    title = "Bds At Unknown Address"
    rule = RuleVariation_20

class Item_7(Item):
    non_spare = NonSpare_28

class Content_23(ContentBds):
    bds_type = (BdsAt, 48)

class RuleContent_21(RuleContentContextFree):
    variation = Content_23

class Variation_21(Element):
    bit_offset8 = 0
    bit_size = 56
    rule = RuleContent_21

class RuleVariation_21(RuleVariationContextFree):
    variation = Variation_21

class NonSpare_29(NonSpare):
    name = "B3"
    title = "Bds At Known Address"
    rule = RuleVariation_21

class Item_8(Item):
    non_spare = NonSpare_29

class Variation_36(Group):
    bit_size = 408
    items_list = [Item_32, Item_38, Item_33, Item_34, Item_35, Item_0, Item_16, Item_20, Item_27, Item_28, Item_29, Item_30, Item_31, Item_6, Item_7, Item_8]
    items_dict = {"R": RuleVariation_5, "T": RuleVariation_6, "S1": RuleVariation_19, "S2": RuleVariation_18, "S3": RuleVariation_12, "I1": RuleVariation_9, "I2": RuleVariation_8, "Q1LAT": RuleVariation_16, "Q2LON": RuleVariation_15, "Q3": RuleVariation_14, "Q4": RuleVariation_10, "Q5": RuleVariation_11, "B1": RuleVariation_22, "B2": RuleVariation_20, "B3": RuleVariation_21}

class RuleVariation_31(RuleVariationContextFree):
    variation = Variation_36

class NonSpare_4(NonSpare):
    name = "020"
    title = "Different Contents"
    rule = RuleVariation_31

class Content_1(ContentTable):
    values = {0: "Air Speed = IAS, LSB (Bit-1) = 2^-14 NM/s", 1: "Air Speed = Mach, LSB (Bit-1) = 0.001"}

class RuleContent_1(RuleContentContextFree):
    variation = Content_1

class Variation_0(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_1

class RuleVariation_0(RuleVariationContextFree):
    variation = Variation_0

class NonSpare_48(NonSpare):
    name = "IM"
    title = ""
    rule = RuleVariation_0

class Item_26(Item):
    non_spare = NonSpare_48

class Content_20(ContentQuantity):
    signedness = Unsigned
    lsb = 6.103515625e-5
    unit = "NM/s"

class Content_19(ContentQuantity):
    signedness = Unsigned
    lsb = 1.0e-3
    unit = "Mach"

class RuleContent_22(RuleContentDependent):
    depends_on = [["030", "IM"]]
    default_variation = Content_0
    cases = [
        ([0], Content_20),
        ([1], Content_19),
    ]

class Variation_24(Element):
    bit_offset8 = 1
    bit_size = 15
    rule = RuleContent_22

class RuleVariation_24(RuleVariationContextFree):
    variation = Variation_24

class NonSpare_47(NonSpare):
    name = "IAS"
    title = ""
    rule = RuleVariation_24

class Item_25(Item):
    non_spare = NonSpare_47

class Variation_35(Group):
    bit_size = 16
    items_list = [Item_26, Item_25]
    items_dict = {"IM": RuleVariation_0, "IAS": RuleVariation_24}

class RuleVariation_30(RuleVariationContextFree):
    variation = Variation_35

class NonSpare_6(NonSpare):
    name = "030"
    title = "Dependent Item"
    rule = RuleVariation_30

class NonSpare_35(NonSpare):
    name = "I1"
    title = ""
    rule = RuleVariation_5

class Variation_2(Element):
    bit_offset8 = 0
    bit_size = 4
    rule = RuleContent_0

class RuleVariation_2(RuleVariationContextFree):
    variation = Variation_2

class NonSpare_61(NonSpare):
    name = "TID"
    title = "Identification of Conflict Categories Definition Table"
    rule = RuleVariation_2

class Item_39(Item):
    non_spare = NonSpare_61

class Variation_26(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_0

class Content_6(ContentTable):
    values = {0: "Test0", 1: "Test1", 2: "Test2"}

class RuleContent_6(RuleContentContextFree):
    variation = Content_6

class Variation_27(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_6

class Content_8(ContentTable):
    values = {3: "Test3", 4: "Test4"}

class RuleContent_8(RuleContentContextFree):
    variation = Content_8

class Variation_28(Element):
    bit_offset8 = 4
    bit_size = 3
    rule = RuleContent_8

class Content_5(ContentTable):
    values = {0: "Test0", 1: "Test1"}

class RuleContent_5(RuleContentContextFree):
    variation = Content_5

class Variation_25(Element):
    bit_offset8 = 4
    bit_size = 1
    rule = RuleContent_5

class RuleVariation_25(RuleVariationContextFree):
    variation = Variation_25

class NonSpare_37(NonSpare):
    name = "I1"
    title = ""
    rule = RuleVariation_25

class Item_15(Item):
    non_spare = NonSpare_37

class Item_2(Spare):
    bit_offset8 = 5
    bit_size = 2

class Variation_34(Group):
    bit_size = 3
    items_list = [Item_15, Item_2]
    items_dict = {"I1": RuleVariation_25}

class RuleVariation_45(RuleVariationDependent):
    depends_on = [["000"], ["031", "CC", "TID"]]
    default_variation = Variation_26
    cases = [
        ([1, 1], Variation_27),
        ([1, 2], Variation_28),
        ([2, 1], Variation_34),
    ]

class NonSpare_31(NonSpare):
    name = "CP"
    title = "Conflict Properties Class"
    rule = RuleVariation_45

class Item_9(Item):
    non_spare = NonSpare_31

class Content_2(ContentTable):
    values = {0: "LOW", 1: "HIGH"}

class RuleContent_2(RuleContentContextFree):
    variation = Content_2

class Variation_30(Element):
    bit_offset8 = 7
    bit_size = 1
    rule = RuleContent_2

class RuleVariation_27(RuleVariationContextFree):
    variation = Variation_30

class NonSpare_32(NonSpare):
    name = "CS"
    title = "Conflict Severity"
    rule = RuleVariation_27

class Item_10(Item):
    non_spare = NonSpare_32

class Variation_38(Group):
    bit_size = 8
    items_list = [Item_39, Item_9, Item_10]
    items_dict = {"TID": RuleVariation_2, "CP": RuleVariation_45, "CS": RuleVariation_27}

class RuleVariation_33(RuleVariationContextFree):
    variation = Variation_38

class NonSpare_30(NonSpare):
    name = "CC"
    title = "Conflict Classification"
    rule = RuleVariation_33

class Variation_49(Compound):
    items_list = [NonSpare_35, NonSpare_30]
    items_dict = {"I1": RuleVariation_5, "CC": RuleVariation_33}

class RuleVariation_44(RuleVariationContextFree):
    variation = Variation_49

class NonSpare_8(NonSpare):
    name = "031"
    title = "Nested Dependent Item"
    rule = RuleVariation_44

class Variation_4(Element):
    bit_offset8 = 0
    bit_size = 7
    rule = RuleContent_0

class RuleVariation_4(RuleVariationContextFree):
    variation = Variation_4

class NonSpare_34(NonSpare):
    name = "I1"
    title = ""
    rule = RuleVariation_4

class Item_12(Item):
    non_spare = NonSpare_34

class Item_4(Spare):
    bit_offset8 = 7
    bit_size = 2

class Variation_23(Element):
    bit_offset8 = 1
    bit_size = 6
    rule = RuleContent_0

class RuleVariation_23(RuleVariationContextFree):
    variation = Variation_23

class NonSpare_41(NonSpare):
    name = "I2"
    title = ""
    rule = RuleVariation_23

class Item_19(Item):
    non_spare = NonSpare_41

class Item_5(Spare):
    bit_offset8 = 7
    bit_size = 9

class Variation_32(Group):
    bit_size = 24
    items_list = [Item_12, Item_4, Item_19, Item_5]
    items_dict = {"I1": RuleVariation_4, "I2": RuleVariation_23}

class RuleVariation_29(RuleVariationContextFree):
    variation = Variation_32

class NonSpare_10(NonSpare):
    name = "040"
    title = "Spare Items"
    rule = RuleVariation_29

class NonSpare_11(NonSpare):
    name = "051"
    title = "Element"
    rule = RuleVariation_5

class Variation_3(Element):
    bit_offset8 = 0
    bit_size = 6
    rule = RuleContent_0

class RuleVariation_3(RuleVariationContextFree):
    variation = Variation_3

class NonSpare_33(NonSpare):
    name = "I1"
    title = ""
    rule = RuleVariation_3

class Item_11(Item):
    non_spare = NonSpare_33

class Item_3(Spare):
    bit_offset8 = 6
    bit_size = 2

class NonSpare_40(NonSpare):
    name = "I2"
    title = ""
    rule = RuleVariation_5

class Item_18(Item):
    non_spare = NonSpare_40

class NonSpare_43(NonSpare):
    name = "I3"
    title = ""
    rule = RuleVariation_2

class Item_21(Item):
    non_spare = NonSpare_43

class Item_1(Spare):
    bit_offset8 = 4
    bit_size = 8

class Variation_29(Element):
    bit_offset8 = 4
    bit_size = 4
    rule = RuleContent_0

class RuleVariation_26(RuleVariationContextFree):
    variation = Variation_29

class NonSpare_46(NonSpare):
    name = "I4"
    title = ""
    rule = RuleVariation_26

class Item_24(Item):
    non_spare = NonSpare_46

class Variation_31(Group):
    bit_size = 32
    items_list = [Item_11, Item_3, Item_18, Item_21, Item_1, Item_24]
    items_dict = {"I1": RuleVariation_3, "I2": RuleVariation_5, "I3": RuleVariation_2, "I4": RuleVariation_26}

class RuleVariation_28(RuleVariationContextFree):
    variation = Variation_31

class NonSpare_12(NonSpare):
    name = "052"
    title = "Group"
    rule = RuleVariation_28

class NonSpare_39(NonSpare):
    name = "I2"
    title = ""
    rule = RuleVariation_4

class Item_17(Item):
    non_spare = NonSpare_39

class NonSpare_44(NonSpare):
    name = "I3"
    title = ""
    rule = RuleVariation_4

class Item_22(Item):
    non_spare = NonSpare_44

class Variation_39(Extended):
    items = [Item_12, None, Item_17, None, Item_22, None]

class RuleVariation_34(RuleVariationContextFree):
    variation = Variation_39

class NonSpare_13(NonSpare):
    name = "053"
    title = "Extended With Trailing Fx"
    rule = RuleVariation_34

class NonSpare_45(NonSpare):
    name = "I3"
    title = ""
    rule = RuleVariation_5

class Item_23(Item):
    non_spare = NonSpare_45

class Variation_40(Extended):
    items = [Item_12, None, Item_17, None, Item_23]

class RuleVariation_35(RuleVariationContextFree):
    variation = Variation_40

class NonSpare_14(NonSpare):
    name = "054"
    title = "Extended Without Trailing Fx"
    rule = RuleVariation_35

class Variation_42(Repetitive):
    rep_bytes = 1
    variation = Variation_5

class RuleVariation_37(RuleVariationContextFree):
    variation = Variation_42

class NonSpare_15(NonSpare):
    name = "061"
    title = "Repetitive Regular"
    rule = RuleVariation_37

class Item_13(Item):
    non_spare = NonSpare_35

class Variation_33(Group):
    bit_size = 16
    items_list = [Item_13, Item_18]
    items_dict = {"I1": RuleVariation_5, "I2": RuleVariation_5}

class Variation_43(Repetitive):
    rep_bytes = 1
    variation = Variation_33

class RuleVariation_38(RuleVariationContextFree):
    variation = Variation_43

class NonSpare_16(NonSpare):
    name = "062"
    title = "Repetitive With Group"
    rule = RuleVariation_38

class Variation_44(Repetitive):
    rep_bytes = None
    variation = Variation_4

class RuleVariation_39(RuleVariationContextFree):
    variation = Variation_44

class NonSpare_17(NonSpare):
    name = "063"
    title = "Repetitive Fx"
    rule = RuleVariation_39

class Variation_45(Explicit):
    explicit_type = None

class RuleVariation_40(RuleVariationContextFree):
    variation = Variation_45

class NonSpare_18(NonSpare):
    name = "071"
    title = "Explicit None"
    rule = RuleVariation_40

class Variation_46(Explicit):
    explicit_type = ReservedExpansion

class RuleVariation_41(RuleVariationContextFree):
    variation = Variation_46

class NonSpare_19(NonSpare):
    name = "072"
    title = "Explicit RE"
    rule = RuleVariation_41

class Variation_47(Explicit):
    explicit_type = SpecialPurpose

class RuleVariation_42(RuleVariationContextFree):
    variation = Variation_47

class NonSpare_20(NonSpare):
    name = "073"
    title = "Explicit SP"
    rule = RuleVariation_42

class Variation_48(Compound):
    items_list = [NonSpare_35, None, NonSpare_40]
    items_dict = {"I1": RuleVariation_5, "I2": RuleVariation_5}

class RuleVariation_43(RuleVariationContextFree):
    variation = Variation_48

class NonSpare_21(NonSpare):
    name = "091"
    title = "Compound Fspec With Fx"
    rule = RuleVariation_43

class Record_6(Record):
    items_list = [NonSpare_3, NonSpare_0, NonSpare_4, NonSpare_6, NonSpare_8, NonSpare_10, NonSpare_11, NonSpare_12, NonSpare_13, NonSpare_14, NonSpare_15, NonSpare_16, NonSpare_17, NonSpare_18, NonSpare_19, NonSpare_20, UapItemRFS, NonSpare_21]
    items_dict = {"010": RuleVariation_32, "000": RuleVariation_7, "020": RuleVariation_31, "030": RuleVariation_30, "031": RuleVariation_44, "040": RuleVariation_29, "051": RuleVariation_5, "052": RuleVariation_28, "053": RuleVariation_34, "054": RuleVariation_35, "061": RuleVariation_37, "062": RuleVariation_38, "063": RuleVariation_39, "071": RuleVariation_40, "072": RuleVariation_41, "073": RuleVariation_42, "091": RuleVariation_43}

class Uap_0(UapSingle):
    record = Record_6

class Asterix_0(AstCat):
    category = 0
    edition = (1, 0)
    uap = Uap_0

class Expansion_0(Expansion):
    fspec_bytes = 1
    items = [NonSpare_35, NonSpare_40]

class Asterix_1(AstRef):
    category = 0
    edition = (1, 0)
    expansion = Expansion_0

class Expansion_1(Expansion):
    fspec_bytes = 2
    items = [NonSpare_35, None, None, NonSpare_40, None, None, None, None, None, None, NonSpare_45]

class Asterix_2(AstRef):
    category = 0
    edition = (1, 1)
    expansion = Expansion_1

class NonSpare_2(NonSpare):
    name = "010"
    title = "Data Source Identifier"
    rule = RuleVariation_32

class Content_3(ContentTable):
    values = {0: "Plot", 1: "Track"}

class RuleContent_3(RuleContentContextFree):
    variation = Content_3

class Variation_1(Element):
    bit_offset8 = 0
    bit_size = 1
    rule = RuleContent_3

class RuleVariation_1(RuleVariationContextFree):
    variation = Variation_1

class NonSpare_62(NonSpare):
    name = "TYP"
    title = ""
    rule = RuleVariation_1

class Item_40(Item):
    non_spare = NonSpare_62

class NonSpare_36(NonSpare):
    name = "I1"
    title = ""
    rule = RuleVariation_23

class Item_14(Item):
    non_spare = NonSpare_36

class Variation_41(Extended):
    items = [Item_40, Item_14, None, Item_17, None]

class RuleVariation_36(RuleVariationContextFree):
    variation = Variation_41

class NonSpare_5(NonSpare):
    name = "020"
    title = "Target Report Descriptor"
    rule = RuleVariation_36

class NonSpare_7(NonSpare):
    name = "031"
    title = "For Plots Only"
    rule = RuleVariation_5

class Record_4(Record):
    items_list = [NonSpare_2, NonSpare_5, NonSpare_7]
    items_dict = {"010": RuleVariation_32, "020": RuleVariation_36, "031": RuleVariation_5}

class Variation_13(Element):
    bit_offset8 = 0
    bit_size = 16
    rule = RuleContent_0

class RuleVariation_13(RuleVariationContextFree):
    variation = Variation_13

class NonSpare_9(NonSpare):
    name = "032"
    title = "For Tracks Only"
    rule = RuleVariation_13

class Record_5(Record):
    items_list = [NonSpare_2, NonSpare_5, NonSpare_9]
    items_dict = {"010": RuleVariation_32, "020": RuleVariation_36, "032": RuleVariation_13}

class Uap_1(UapMultiple):
    uaps = {"plot": Record_4, "track": Record_5}
    selector = (["020", "TYP"], {0: "plot", 1: "track"})

class Asterix_3(AstCat):
    category = 1
    edition = (1, 0)
    uap = Uap_1

class NonSpare_1(NonSpare):
    name = "010"
    title = ""
    rule = RuleVariation_5

class NonSpare_22(NonSpare):
    name = "101"
    title = ""
    rule = RuleVariation_5

class NonSpare_23(NonSpare):
    name = "102"
    title = ""
    rule = RuleVariation_5

class Record_1(Record):
    items_list = [NonSpare_1, NonSpare_22, NonSpare_23]
    items_dict = {"010": RuleVariation_5, "101": RuleVariation_5, "102": RuleVariation_5}

class NonSpare_24(NonSpare):
    name = "201"
    title = ""
    rule = RuleVariation_13

class NonSpare_25(NonSpare):
    name = "202"
    title = ""
    rule = RuleVariation_13

class Record_2(Record):
    items_list = [NonSpare_1, NonSpare_24, NonSpare_25]
    items_dict = {"010": RuleVariation_5, "201": RuleVariation_13, "202": RuleVariation_13}

class Variation_17(Element):
    bit_offset8 = 0
    bit_size = 32
    rule = RuleContent_0

class RuleVariation_17(RuleVariationContextFree):
    variation = Variation_17

class NonSpare_26(NonSpare):
    name = "301"
    title = ""
    rule = RuleVariation_17

class Record_3(Record):
    items_list = [NonSpare_1, NonSpare_26]
    items_dict = {"010": RuleVariation_5, "301": RuleVariation_17}

class Record_0(Record):
    items_list = [NonSpare_1]
    items_dict = {"010": RuleVariation_5}

class Uap_2(UapMultiple):
    uaps = {"uap1": Record_1, "uap2": Record_2, "uap3": Record_3, "uap4": Record_0}
    selector = None

class Asterix_4(AstCat):
    category = 2
    edition = (1, 0)
    uap = Uap_2

# Aliases

Cat_000_1_0 : TypeAlias = Asterix_0
Ref_000_1_0 : TypeAlias = Asterix_1
Ref_000_1_1 : TypeAlias = Asterix_2
Cat_001_1_0 : TypeAlias = Asterix_3
Cat_002_1_0 : TypeAlias = Asterix_4

# Manifest

manifest = {
    'CATS': {
        0: {
            '1.0': Cat_000_1_0,
        },
        1: {
            '1.0': Cat_001_1_0,
        },
        2: {
            '1.0': Cat_002_1_0,
        },
    },
    'REFS': {
        0: {
            '1.0': Ref_000_1_0,
            '1.1': Ref_000_1_1,
        },
    },
}
