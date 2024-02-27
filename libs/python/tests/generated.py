# Asterix specifications

# This file is generated, DO NOT EDIT!
# For more details, see:
#     - https://github.com/zoranbosnjak/asterix-specs

from asterix.base import *

reference = "unknown"
version = "19700101.0"

# Content set

Content_0_Arg : TypeAlias = Raw
class Content_0(ContentRaw):
    pass

Content_1_Arg : TypeAlias = Raw
class Content_1(ContentTable):
    tab = { 0: "Air Speed = IAS, LSB (Bit-1) = 2^-14 NM/s", 1: "Air Speed = Mach, LSB (Bit-1) = 0.001" }

Content_2_Arg : TypeAlias = Raw
class Content_2(ContentTable):
    tab = { 0: "LOW", 1: "HIGH" }

Content_3_Arg : TypeAlias = Raw
class Content_3(ContentTable):
    tab = { 0: "Plot", 1: "Track" }

Content_4_Arg : TypeAlias = Raw
class Content_4(ContentTable):
    tab = { 0: "Test 0", 1: "Test 1", 2: "Test 2", 3: "Test 3" }

Content_5_Arg : TypeAlias = Raw
class Content_5(ContentTable):
    tab = { 0: "Test0", 1: "Test1" }

Content_6_Arg : TypeAlias = Raw
class Content_6(ContentTable):
    tab = { 0: "Test0", 1: "Test1", 2: "Test2" }

Content_7_Arg : TypeAlias = Raw
class Content_7(ContentTable):
    tab = { 1: "Message 1", 2: "Message 2", 3: "Message 3" }

Content_8_Arg : TypeAlias = Raw
class Content_8(ContentTable):
    tab = { 3: "Test3", 4: "Test4" }

Content_9_Arg : TypeAlias = Union[Raw, str]
class Content_9(ContentString):
    t = StringAscii

Content_10_Arg : TypeAlias = Union[Raw, str]
class Content_10(ContentString):
    t = StringICAO

Content_11_Arg : TypeAlias = Union[Raw, str]
class Content_11(ContentString):
    t = StringOctal

Content_12_Arg : TypeAlias = Raw
class Content_12(ContentInteger):
    sig = Signed

Content_13_Arg : TypeAlias = Raw
class Content_13(ContentInteger):
    sig = Unsigned

Content_14_Arg : TypeAlias = Union[Raw, float, Tuple[float, Literal["°"]]]
class Content_14(ContentQuantity):
    sig = Signed
    lsb = 2.1457672119140625e-5
    unit = "°"

Content_15_Arg : TypeAlias = Union[Raw, float, Tuple[float, Literal["°"]]]
class Content_15(ContentQuantity):
    sig = Signed
    lsb = 2.1457672119140625e-5
    unit = "°"

Content_16_Arg : TypeAlias = Union[Raw, float, Tuple[float, Literal[""]]]
class Content_16(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = ""

Content_17_Arg : TypeAlias = Union[Raw, float, Tuple[float, Literal["kt"]]]
class Content_17(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "kt"

Content_18_Arg : TypeAlias = Union[Raw, float, Tuple[float, Literal[""]]]
class Content_18(ContentQuantity):
    sig = Unsigned
    lsb = -0.5
    unit = ""

Content_19_Arg : TypeAlias = Union[Raw, float, Tuple[float, Literal["Mach"]]]
class Content_19(ContentQuantity):
    sig = Unsigned
    lsb = 1.0e-3
    unit = "Mach"

Content_20_Arg : TypeAlias = Union[Raw, float, Tuple[float, Literal["NM/s"]]]
class Content_20(ContentQuantity):
    sig = Unsigned
    lsb = 6.103515625e-5
    unit = "NM/s"

Content_21_Arg : TypeAlias = Raw
class Content_21(ContentBds):
    t = BdsWithAddress

Content_22_Arg : TypeAlias = Raw
class Content_22(ContentBds):
    t = BdsAt
    addr = None

Content_23_Arg : TypeAlias = Raw
class Content_23(ContentBds):
    t = BdsAt
    addr = 48

# Variation and Item set

Variation_0_Arg : TypeAlias = Content_1_Arg
class Variation_0(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_1

    def __init__(self, arg : Union[Bits, Content_1_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_1.from_arg(n, arg))
        super().__init__(val)

Variation_1_Arg : TypeAlias = Content_3_Arg
class Variation_1(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_3

    def __init__(self, arg : Union[Bits, Content_3_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_3.from_arg(n, arg))
        super().__init__(val)

Variation_2_Arg : TypeAlias = Content_0_Arg
class Variation_2(Element):
    bit_offset8 = 0
    bit_size = 4
    content = Content_0

    def __init__(self, arg : Union[Bits, Content_0_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_0.from_arg(n, arg))
        super().__init__(val)

Variation_3_Arg : TypeAlias = Content_0_Arg
class Variation_3(Element):
    bit_offset8 = 0
    bit_size = 6
    content = Content_0

    def __init__(self, arg : Union[Bits, Content_0_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_0.from_arg(n, arg))
        super().__init__(val)

Variation_4_Arg : TypeAlias = Content_0_Arg
class Variation_4(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_0

    def __init__(self, arg : Union[Bits, Content_0_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_0.from_arg(n, arg))
        super().__init__(val)

Variation_5_Arg : TypeAlias = Content_0_Arg
class Variation_5(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_0

    def __init__(self, arg : Union[Bits, Content_0_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_0.from_arg(n, arg))
        super().__init__(val)

Variation_6_Arg : TypeAlias = Content_4_Arg
class Variation_6(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_4

    def __init__(self, arg : Union[Bits, Content_4_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_4.from_arg(n, arg))
        super().__init__(val)

Variation_7_Arg : TypeAlias = Content_7_Arg
class Variation_7(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_7

    def __init__(self, arg : Union[Bits, Content_7_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_7.from_arg(n, arg))
        super().__init__(val)

Variation_8_Arg : TypeAlias = Content_12_Arg
class Variation_8(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_12

    def __init__(self, arg : Union[Bits, Content_12_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_12.from_arg(n, arg))
        super().__init__(val)

Variation_9_Arg : TypeAlias = Content_13_Arg
class Variation_9(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_13

    def __init__(self, arg : Union[Bits, Content_13_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_13.from_arg(n, arg))
        super().__init__(val)

Variation_10_Arg : TypeAlias = Content_16_Arg
class Variation_10(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_16

    def __init__(self, arg : Union[Bits, Content_16_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_16.from_arg(n, arg))
        super().__init__(val)

Variation_11_Arg : TypeAlias = Content_18_Arg
class Variation_11(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_18

    def __init__(self, arg : Union[Bits, Content_18_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_18.from_arg(n, arg))
        super().__init__(val)

Variation_12_Arg : TypeAlias = Content_11_Arg
class Variation_12(Element):
    bit_offset8 = 0
    bit_size = 12
    content = Content_11

    def __init__(self, arg : Union[Bits, Content_11_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_11.from_arg(n, arg))
        super().__init__(val)

Variation_13_Arg : TypeAlias = Content_0_Arg
class Variation_13(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_0

    def __init__(self, arg : Union[Bits, Content_0_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_0.from_arg(n, arg))
        super().__init__(val)

Variation_14_Arg : TypeAlias = Content_17_Arg
class Variation_14(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_17

    def __init__(self, arg : Union[Bits, Content_17_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_17.from_arg(n, arg))
        super().__init__(val)

Variation_15_Arg : TypeAlias = Content_14_Arg
class Variation_15(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_14

    def __init__(self, arg : Union[Bits, Content_14_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_14.from_arg(n, arg))
        super().__init__(val)

Variation_16_Arg : TypeAlias = Content_15_Arg
class Variation_16(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_15

    def __init__(self, arg : Union[Bits, Content_15_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_15.from_arg(n, arg))
        super().__init__(val)

Variation_17_Arg : TypeAlias = Content_0_Arg
class Variation_17(Element):
    bit_offset8 = 0
    bit_size = 32
    content = Content_0

    def __init__(self, arg : Union[Bits, Content_0_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_0.from_arg(n, arg))
        super().__init__(val)

Variation_18_Arg : TypeAlias = Content_10_Arg
class Variation_18(Element):
    bit_offset8 = 0
    bit_size = 48
    content = Content_10

    def __init__(self, arg : Union[Bits, Content_10_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_10.from_arg(n, arg))
        super().__init__(val)

Variation_19_Arg : TypeAlias = Content_9_Arg
class Variation_19(Element):
    bit_offset8 = 0
    bit_size = 56
    content = Content_9

    def __init__(self, arg : Union[Bits, Content_9_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_9.from_arg(n, arg))
        super().__init__(val)

Variation_20_Arg : TypeAlias = Content_22_Arg
class Variation_20(Element):
    bit_offset8 = 0
    bit_size = 56
    content = Content_22

    def __init__(self, arg : Union[Bits, Content_22_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_22.from_arg(n, arg))
        super().__init__(val)

Variation_21_Arg : TypeAlias = Content_23_Arg
class Variation_21(Element):
    bit_offset8 = 0
    bit_size = 56
    content = Content_23

    def __init__(self, arg : Union[Bits, Content_23_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_23.from_arg(n, arg))
        super().__init__(val)

Variation_22_Arg : TypeAlias = Content_21_Arg
class Variation_22(Element):
    bit_offset8 = 0
    bit_size = 64
    content = Content_21

    def __init__(self, arg : Union[Bits, Content_21_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_21.from_arg(n, arg))
        super().__init__(val)

Variation_23_Arg : TypeAlias = Content_0_Arg
class Variation_23(Element):
    bit_offset8 = 1
    bit_size = 6
    content = Content_0

    def __init__(self, arg : Union[Bits, Content_0_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_0.from_arg(n, arg))
        super().__init__(val)

Variation_24_Arg : TypeAlias = Content_0_Arg
class Variation_24(Element):
    bit_offset8 = 1
    bit_size = 15
    content = Content_0

    def __init__(self, arg : Union[Bits, Content_0_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_0.from_arg(n, arg))
        super().__init__(val)

Variation_25_Arg : TypeAlias = Content_5_Arg
class Variation_25(Element):
    bit_offset8 = 4
    bit_size = 1
    content = Content_5

    def __init__(self, arg : Union[Bits, Content_5_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_5.from_arg(n, arg))
        super().__init__(val)

Variation_26_Arg : TypeAlias = Content_0_Arg
class Variation_26(Element):
    bit_offset8 = 4
    bit_size = 3
    content = Content_0

    def __init__(self, arg : Union[Bits, Content_0_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_0.from_arg(n, arg))
        super().__init__(val)

Variation_27_Arg : TypeAlias = Content_6_Arg
class Variation_27(Element):
    bit_offset8 = 4
    bit_size = 3
    content = Content_6

    def __init__(self, arg : Union[Bits, Content_6_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_6.from_arg(n, arg))
        super().__init__(val)

Variation_28_Arg : TypeAlias = Content_8_Arg
class Variation_28(Element):
    bit_offset8 = 4
    bit_size = 3
    content = Content_8

    def __init__(self, arg : Union[Bits, Content_8_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_8.from_arg(n, arg))
        super().__init__(val)

Variation_29_Arg : TypeAlias = Content_0_Arg
class Variation_29(Element):
    bit_offset8 = 4
    bit_size = 4
    content = Content_0

    def __init__(self, arg : Union[Bits, Content_0_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_0.from_arg(n, arg))
        super().__init__(val)

Variation_30_Arg : TypeAlias = Content_2_Arg
class Variation_30(Element):
    bit_offset8 = 7
    bit_size = 1
    content = Content_2

    def __init__(self, arg : Union[Bits, Content_2_Arg]) -> None:
        if isinstance(arg, Bits):
            super().__init__(arg)
            return
        n = self.__class__.bit_size
        val = self.__class__.from_raw(Content_2.from_arg(n, arg))
        super().__init__(val)

class Item_39(Item):
    name = "I1"
    title = ""
    var = Variation_3

class Item_3(Spare):
    bit_offset8 = 6
    bit_size = 2

class Item_46(Item):
    name = "I2"
    title = ""
    var = Variation_5

class Item_49(Item):
    name = "I3"
    title = ""
    var = Variation_2

class Item_1(Spare):
    bit_offset8 = 4
    bit_size = 8

class Item_52(Item):
    name = "I4"
    title = ""
    var = Variation_29

Variation_31_Arg_Group = TypedDict('Variation_31_Arg_Group', {
    "I1": Union[Variation_3, Variation_3_Arg],
    "I2": Union[Variation_5, Variation_5_Arg],
    "I3": Union[Variation_2, Variation_2_Arg],
    "I4": Union[Variation_29, Variation_29_Arg],
})
Variation_31_Arg : TypeAlias = Union[Raw, Variation_31_Arg_Group]
class Variation_31(Group):
    bit_size = 32
    items_list = [Item_39, Item_3, Item_46, Item_49, Item_1, Item_52]
    items_dict = {"I1": (Item_39), "I2": (Item_46), "I3": (Item_49), "I4": (Item_52)}

class Item_40(Item):
    name = "I1"
    title = ""
    var = Variation_4

class Item_4(Spare):
    bit_offset8 = 7
    bit_size = 2

class Item_47(Item):
    name = "I2"
    title = ""
    var = Variation_23

class Item_5(Spare):
    bit_offset8 = 7
    bit_size = 9

Variation_32_Arg_Group = TypedDict('Variation_32_Arg_Group', {
    "I1": Union[Variation_4, Variation_4_Arg],
    "I2": Union[Variation_23, Variation_23_Arg],
})
Variation_32_Arg : TypeAlias = Union[Raw, Variation_32_Arg_Group]
class Variation_32(Group):
    bit_size = 24
    items_list = [Item_40, Item_4, Item_47, Item_5]
    items_dict = {"I1": (Item_40), "I2": (Item_47)}

class Item_41(Item):
    name = "I1"
    title = ""
    var = Variation_5

Variation_33_Arg_Group = TypedDict('Variation_33_Arg_Group', {
    "I1": Union[Variation_5, Variation_5_Arg],
    "I2": Union[Variation_5, Variation_5_Arg],
})
Variation_33_Arg : TypeAlias = Union[Raw, Variation_33_Arg_Group]
class Variation_33(Group):
    bit_size = 16
    items_list = [Item_41, Item_46]
    items_dict = {"I1": (Item_41), "I2": (Item_46)}

class Item_43(Item):
    name = "I1"
    title = ""
    var = Variation_25

class Item_2(Spare):
    bit_offset8 = 5
    bit_size = 2

Variation_34_Arg_Group = TypedDict('Variation_34_Arg_Group', {
    "I1": Union[Variation_25, Variation_25_Arg],
})
Variation_34_Arg : TypeAlias = Union[Raw, Variation_34_Arg_Group]
class Variation_34(Group):
    bit_size = 3
    items_list = [Item_43, Item_2]
    items_dict = {"I1": (Item_43)}

class Item_54(Item):
    name = "IM"
    title = ""
    var = Variation_0

class Item_53(Item):
    name = "IAS"
    title = ""
    var = Variation_24

Variation_35_Arg_Group = TypedDict('Variation_35_Arg_Group', {
    "IM": Union[Variation_0, Variation_0_Arg],
    "IAS": Union[Variation_24, Variation_24_Arg],
})
Variation_35_Arg : TypeAlias = Union[Raw, Variation_35_Arg_Group]
class Variation_35(Group):
    bit_size = 16
    items_list = [Item_54, Item_53]
    items_dict = {"IM": (Item_54), "IAS": (Item_53)}

class Item_60(Item):
    name = "R"
    title = "Raw"
    var = Variation_5

class Item_66(Item):
    name = "T"
    title = "Table"
    var = Variation_6

class Item_61(Item):
    name = "S1"
    title = "String Ascii"
    var = Variation_19

class Item_62(Item):
    name = "S2"
    title = "String ICAO"
    var = Variation_18

class Item_63(Item):
    name = "S3"
    title = "String Octal"
    var = Variation_12

class Item_0(Spare):
    bit_offset8 = 4
    bit_size = 4

class Item_44(Item):
    name = "I1"
    title = "Unsigned Integer"
    var = Variation_9

class Item_48(Item):
    name = "I2"
    title = "Signed Integer"
    var = Variation_8

class Item_55(Item):
    name = "Q1LAT"
    title = "Latitude in WGS.84 in Two's Complement Form"
    var = Variation_16

class Item_56(Item):
    name = "Q2LON"
    title = "Longitude in WGS.84 in Two's Complement Form"
    var = Variation_15

class Item_57(Item):
    name = "Q3"
    title = "Unsigned Quantity"
    var = Variation_14

class Item_58(Item):
    name = "Q4"
    title = "Quantity No Unit"
    var = Variation_10

class Item_59(Item):
    name = "Q5"
    title = "Negative Lsb"
    var = Variation_11

class Item_33(Item):
    name = "B1"
    title = "Bds With Address"
    var = Variation_22

class Item_34(Item):
    name = "B2"
    title = "Bds At Unknown Address"
    var = Variation_20

class Item_35(Item):
    name = "B3"
    title = "Bds At Known Address"
    var = Variation_21

Variation_36_Arg_Group = TypedDict('Variation_36_Arg_Group', {
    "R": Union[Variation_5, Variation_5_Arg],
    "T": Union[Variation_6, Variation_6_Arg],
    "S1": Union[Variation_19, Variation_19_Arg],
    "S2": Union[Variation_18, Variation_18_Arg],
    "S3": Union[Variation_12, Variation_12_Arg],
    "I1": Union[Variation_9, Variation_9_Arg],
    "I2": Union[Variation_8, Variation_8_Arg],
    "Q1LAT": Union[Variation_16, Variation_16_Arg],
    "Q2LON": Union[Variation_15, Variation_15_Arg],
    "Q3": Union[Variation_14, Variation_14_Arg],
    "Q4": Union[Variation_10, Variation_10_Arg],
    "Q5": Union[Variation_11, Variation_11_Arg],
    "B1": Union[Variation_22, Variation_22_Arg],
    "B2": Union[Variation_20, Variation_20_Arg],
    "B3": Union[Variation_21, Variation_21_Arg],
})
Variation_36_Arg : TypeAlias = Union[Raw, Variation_36_Arg_Group]
class Variation_36(Group):
    bit_size = 408
    items_list = [Item_60, Item_66, Item_61, Item_62, Item_63, Item_0, Item_44, Item_48, Item_55, Item_56, Item_57, Item_58, Item_59, Item_33, Item_34, Item_35]
    items_dict = {"R": (Item_60), "T": (Item_66), "S1": (Item_61), "S2": (Item_62), "S3": (Item_63), "I1": (Item_44), "I2": (Item_48), "Q1LAT": (Item_55), "Q2LON": (Item_56), "Q3": (Item_57), "Q4": (Item_58), "Q5": (Item_59), "B1": (Item_33), "B2": (Item_34), "B3": (Item_35)}

class Item_64(Item):
    name = "SAC"
    title = "System Area Code"
    var = Variation_5

class Item_65(Item):
    name = "SIC"
    title = "System Identification Code"
    var = Variation_5

Variation_37_Arg_Group = TypedDict('Variation_37_Arg_Group', {
    "SAC": Union[Variation_5, Variation_5_Arg],
    "SIC": Union[Variation_5, Variation_5_Arg],
})
Variation_37_Arg : TypeAlias = Union[Raw, Variation_37_Arg_Group]
class Variation_37(Group):
    bit_size = 16
    items_list = [Item_64, Item_65]
    items_dict = {"SAC": (Item_64), "SIC": (Item_65)}

class Item_67(Item):
    name = "TID"
    title = "Identification of Conflict Categories Definition Table"
    var = Variation_2

class Item_37(Item):
    name = "CP"
    title = "Conflict Properties Class"
    var = Variation_26

class Item_38(Item):
    name = "CS"
    title = "Conflict Severity"
    var = Variation_30

Variation_38_Arg_Group = TypedDict('Variation_38_Arg_Group', {
    "TID": Union[Variation_2, Variation_2_Arg],
    "CP": Union[Variation_26, Variation_26_Arg],
    "CS": Union[Variation_30, Variation_30_Arg],
})
Variation_38_Arg : TypeAlias = Union[Raw, Variation_38_Arg_Group]
class Variation_38(Group):
    bit_size = 8
    items_list = [Item_67, Item_37, Item_38]
    items_dict = {"TID": (Item_67), "CP": (Item_37), "CS": (Item_38)}

class Item_45(Item):
    name = "I2"
    title = ""
    var = Variation_4

class Item_50(Item):
    name = "I3"
    title = ""
    var = Variation_4

class Variation_39(Extended):
    items = [Item_40, None, Item_45, None, Item_50, None]

class Item_51(Item):
    name = "I3"
    title = ""
    var = Variation_5

class Variation_40(Extended):
    items = [Item_40, None, Item_45, None, Item_51]

class Item_68(Item):
    name = "TYP"
    title = ""
    var = Variation_1

class Item_42(Item):
    name = "I1"
    title = ""
    var = Variation_23

class Variation_41(Extended):
    items = [Item_68, Item_42, None, Item_45, None]

class Variation_42(Repetitive):
    rep = 1
    var = Variation_5

class Variation_43(Repetitive):
    rep = 1
    var = Variation_33

class Variation_44(Repetitive):
    rep = None
    var = Variation_4

class Variation_45(Explicit):
    t = None

class Variation_46(Explicit):
    t = ReservedExpansion

class Variation_47(Explicit):
    t = SpecialPurpose

class Item_7(Item):
    name = "010"
    title = ""
    var = Variation_5

class Variation_48(Compound):
    fspec_size = None
    items_list = [Item_7]
    items_dict = {"010": (Item_7, 0x80)}
    @classmethod
    def spec(cls, key : Literal["010"]) -> Variation_5:
        return cls.items_dict[key][1] # type: ignore

class Item_28(Item):
    name = "101"
    title = ""
    var = Variation_5

class Item_29(Item):
    name = "102"
    title = ""
    var = Variation_5

class Variation_49(Compound):
    fspec_size = None
    items_list = [Item_7, Item_28, Item_29]
    items_dict = {"010": (Item_7, 0x80), "101": (Item_28, 0x40), "102": (Item_29, 0x20)}
    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["101"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["102"]) -> Variation_5: ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls.items_dict[key][1]

class Item_30(Item):
    name = "201"
    title = ""
    var = Variation_13

class Item_31(Item):
    name = "202"
    title = ""
    var = Variation_13

class Variation_50(Compound):
    fspec_size = None
    items_list = [Item_7, Item_30, Item_31]
    items_dict = {"010": (Item_7, 0x80), "201": (Item_30, 0x40), "202": (Item_31, 0x20)}
    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["201"]) -> Variation_13: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["202"]) -> Variation_13: ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls.items_dict[key][1]

class Item_32(Item):
    name = "301"
    title = ""
    var = Variation_17

class Variation_51(Compound):
    fspec_size = None
    items_list = [Item_7, Item_32]
    items_dict = {"010": (Item_7, 0x80), "301": (Item_32, 0x40)}
    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["301"]) -> Variation_17: ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls.items_dict[key][1]

class Item_8(Item):
    name = "010"
    title = "Data Source Identifier"
    var = Variation_37

class Item_6(Item):
    name = "000"
    title = "Message Type"
    var = Variation_7

class Item_9(Item):
    name = "020"
    title = "Different Contents"
    var = Variation_36

class Item_11(Item):
    name = "030"
    title = "Dependent Item"
    var = Variation_35

class Item_36(Item):
    name = "CC"
    title = "Conflict Classification"
    var = Variation_38

class Variation_55(Compound):
    fspec_size = None
    items_list = [Item_41, Item_36]
    items_dict = {"I1": (Item_41, 0x80), "CC": (Item_36, 0x40)}
    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["CC"]) -> Variation_38: ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls.items_dict[key][1]

class Item_13(Item):
    name = "031"
    title = "Nested Dependent Item"
    var = Variation_55

class Item_15(Item):
    name = "040"
    title = "Spare Items"
    var = Variation_32

class Item_16(Item):
    name = "051"
    title = "Element"
    var = Variation_5

class Item_17(Item):
    name = "052"
    title = "Group"
    var = Variation_31

class Item_18(Item):
    name = "053"
    title = "Extended With Trailing Fx"
    var = Variation_39

class Item_19(Item):
    name = "054"
    title = "Extended Without Trailing Fx"
    var = Variation_40

class Item_20(Item):
    name = "061"
    title = "Repetitive Regular"
    var = Variation_42

class Item_21(Item):
    name = "062"
    title = "Repetitive With Group"
    var = Variation_43

class Item_22(Item):
    name = "063"
    title = "Repetitive Fx"
    var = Variation_44

class Item_23(Item):
    name = "071"
    title = "Explicit None"
    var = Variation_45

class Item_24(Item):
    name = "072"
    title = "Explicit RE"
    var = Variation_46

class Item_25(Item):
    name = "073"
    title = "Explicit SP"
    var = Variation_47

class Variation_56(Compound):
    fspec_size = None
    items_list = [Item_41, None, Item_46]
    items_dict = {"I1": (Item_41, 0x80), "I2": (Item_46, 0x20)}
    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Variation_5: ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls.items_dict[key][1]

class Item_26(Item):
    name = "091"
    title = "Compound Fspec With Fx"
    var = Variation_56

class Variation_58(Compound):
    fspec_size = 1
    items_list = [Item_41, None, Item_46]
    items_dict = {"I1": (Item_41, 0x80), "I2": (Item_46, 0x20)}
    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Variation_5: ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls.items_dict[key][1]

class Item_27(Item):
    name = "092"
    title = "Compound Fixed Size Fspec"
    var = Variation_58

class Variation_52(Compound):
    fspec_size = None
    items_list = [Item_8, Item_6, Item_9, Item_11, Item_13, Item_15, Item_16, Item_17, Item_18, Item_19, Item_20, Item_21, Item_22, Item_23, Item_24, Item_25, None, Item_26, Item_27]
    items_dict = {"010": (Item_8, 0x800000), "000": (Item_6, 0x400000), "020": (Item_9, 0x200000), "030": (Item_11, 0x100000), "031": (Item_13, 0x080000), "040": (Item_15, 0x040000), "051": (Item_16, 0x020000), "052": (Item_17, 0x018000), "053": (Item_18, 0x014000), "054": (Item_19, 0x012000), "061": (Item_20, 0x011000), "062": (Item_21, 0x010800), "063": (Item_22, 0x010400), "071": (Item_23, 0x010200), "072": (Item_24, 0x010180), "073": (Item_25, 0x010140), "091": (Item_26, 0x010110), "092": (Item_27, 0x010108)}
    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Variation_37: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["000"]) -> Variation_7: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["020"]) -> Variation_36: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["030"]) -> Variation_35: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["031"]) -> Variation_55: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["040"]) -> Variation_32: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["051"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["052"]) -> Variation_31: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["053"]) -> Variation_39: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["054"]) -> Variation_40: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["061"]) -> Variation_42: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["062"]) -> Variation_43: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["063"]) -> Variation_44: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["071"]) -> Variation_45: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["072"]) -> Variation_46: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["073"]) -> Variation_47: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["091"]) -> Variation_56: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["092"]) -> Variation_58: ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls.items_dict[key][1]

class Item_10(Item):
    name = "020"
    title = "Target Report Descriptor"
    var = Variation_41

class Item_12(Item):
    name = "031"
    title = "For Plots Only"
    var = Variation_5

class Variation_53(Compound):
    fspec_size = None
    items_list = [Item_8, Item_10, Item_12]
    items_dict = {"010": (Item_8, 0x80), "020": (Item_10, 0x40), "031": (Item_12, 0x20)}
    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Variation_37: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["020"]) -> Variation_41: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["031"]) -> Variation_5: ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls.items_dict[key][1]

class Item_14(Item):
    name = "032"
    title = "For Tracks Only"
    var = Variation_13

class Variation_54(Compound):
    fspec_size = None
    items_list = [Item_8, Item_10, Item_14]
    items_dict = {"010": (Item_8, 0x80), "020": (Item_10, 0x40), "032": (Item_14, 0x20)}
    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Variation_37: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["020"]) -> Variation_41: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["032"]) -> Variation_13: ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls.items_dict[key][1]

class Variation_57(Compound):
    fspec_size = 1
    items_list = [Item_41, Item_46]
    items_dict = {"I1": (Item_41, 0x80), "I2": (Item_46, 0x40)}
    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Variation_5: ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls.items_dict[key][1]

class Variation_59(Compound):
    fspec_size = 2
    items_list = [Item_41, None, None, Item_46, None, None, None, None, None, None, Item_51]
    items_dict = {"I1": (Item_41, 0x8000), "I2": (Item_46, 0x1000), "I3": (Item_51, 0x0020)}
    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I3"]) -> Variation_5: ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls.items_dict[key][1]

# Record set

class Record_0(Record):
    items = [Item_7]

class Record_1(Record):
    items = [Item_7, Item_28, Item_29]

class Record_2(Record):
    items = [Item_7, Item_30, Item_31]

class Record_3(Record):
    items = [Item_7, Item_32]

class Record_4(Record):
    items = [Item_8, Item_6, Item_9, Item_11, Item_13, Item_15, Item_16, Item_17, Item_18, Item_19, Item_20, Item_21, Item_22, Item_23, Item_24, Item_25, None, Item_26, Item_27]

class Record_5(Record):
    items = [Item_8, Item_10, Item_12]

class Record_6(Record):
    items = [Item_8, Item_10, Item_14]

# Expansion set

class Expansion_0(Expansion):
    fspec_bytes = 1
    items = [Item_41, Item_46]

class Expansion_1(Expansion):
    fspec_bytes = 2
    items = [Item_41, None, None, Item_46, None, None, None, None, None, None, Item_51]

# Uap set

class Uap_0(UapSingle):
    record = Record_4

class Uap_1(UapMultiple):
    uaps = {"plot": Record_5, "track": Record_6}
    selector = (["020", "TYP"], {0: "plot", 1: "track"})

class Uap_2(UapMultiple):
    uaps = {"uap1": Record_1, "uap2": Record_2, "uap3": Record_3, "uap4": Record_0}
    selector = None

# Asterix spec set

class AstSpec_0(AstCat):
    cat = 0
    edition = (1, 0)
    uap = Uap_0

class AstSpec_1(AstRef):
    cat = 0
    edition = (1, 0)
    expansion = Expansion_0

class AstSpec_2(AstRef):
    cat = 0
    edition = (1, 1)
    expansion = Expansion_1

class AstSpec_3(AstCat):
    cat = 1
    edition = (1, 0)
    uap = Uap_1

class AstSpec_4(AstCat):
    cat = 2
    edition = (1, 0)
    uap = Uap_2

# Aliases

Cat_000_1_0: TypeAlias = AstSpec_0
Ref_000_1_0: TypeAlias = AstSpec_1
Ref_000_1_1: TypeAlias = AstSpec_2
Cat_001_1_0: TypeAlias = AstSpec_3
Cat_002_1_0: TypeAlias = AstSpec_4

# Manifest

manifest = [Cat_000_1_0, Ref_000_1_0, Ref_000_1_1, Cat_001_1_0, Cat_002_1_0]
