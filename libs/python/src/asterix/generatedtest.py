# Asterix specifications

# This file is generated, DO NOT EDIT!
# For more details, see:
#     - https://github.com/zoranbosnjak/asterix-specs

from asterix.base import *

reference = "unknown"
version = "19700101.0"

# Content set
class Content_0(ContentRaw):
    pass
class Content_1(ContentTable):
    values = { 0: "Air Speed = IAS, LSB (Bit-1) = 2^-14 NM/s", 1: "Air Speed = Mach, LSB (Bit-1) = 0.001" }
class Content_2(ContentTable):
    values = { 0: "Plot", 1: "Track" }
class Content_3(ContentTable):
    values = { 0: "Test 0", 1: "Test 1", 2: "Test 2", 3: "Test 3" }
class Content_4(ContentString):
    t = StringAscii
class Content_5(ContentString):
    t = StringICAO
class Content_6(ContentString):
    t = StringOctal
class Content_7(ContentInteger):
    sig = Signed
class Content_8(ContentInteger):
    sig = Unsigned
class Content_9(ContentQuantity):
    sig = Signed
    lsb = 2.1457672119140625e-5
    unit = "°"
class Content_10(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = ""
class Content_11(ContentQuantity):
    sig = Unsigned
    lsb = 1.0
    unit = "kt"
class Content_12(ContentQuantity):
    sig = Unsigned
    lsb = -0.5
    unit = ""

# Variation and Item set
class Variation_0(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_1
class Variation_1(Element):
    bit_offset8 = 0
    bit_size = 1
    content = Content_2
class Variation_2(Element):
    bit_offset8 = 0
    bit_size = 4
    content = Content_0
class Variation_3(Element):
    bit_offset8 = 0
    bit_size = 6
    content = Content_0
class Variation_4(Element):
    bit_offset8 = 0
    bit_size = 7
    content = Content_0
class Variation_5(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_0
class Variation_6(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_3
class Variation_7(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_7
class Variation_8(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_8
class Variation_9(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_10
class Variation_10(Element):
    bit_offset8 = 0
    bit_size = 8
    content = Content_12
class Variation_11(Element):
    bit_offset8 = 0
    bit_size = 12
    content = Content_6
class Variation_12(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_0
class Variation_13(Element):
    bit_offset8 = 0
    bit_size = 16
    content = Content_11
class Variation_14(Element):
    bit_offset8 = 0
    bit_size = 24
    content = Content_9
class Variation_15(Element):
    bit_offset8 = 0
    bit_size = 32
    content = Content_0
class Variation_16(Element):
    bit_offset8 = 0
    bit_size = 48
    content = Content_5
class Variation_17(Element):
    bit_offset8 = 0
    bit_size = 56
    content = Content_0
class Variation_18(Element):
    bit_offset8 = 0
    bit_size = 56
    content = Content_4
class Variation_19(Element):
    bit_offset8 = 0
    bit_size = 64
    content = Content_0
class Variation_20(Element):
    bit_offset8 = 1
    bit_size = 6
    content = Content_0
class Variation_21(Element):
    bit_offset8 = 1
    bit_size = 15
    content = Content_0
class Variation_22(Element):
    bit_offset8 = 4
    bit_size = 4
    content = Content_0
class Item_33(Item):
    name = "I1"
    title = ""
    var = Variation_3
class Item_2(Spare):
    bit_offset8 = 6
    bit_size = 2
class Item_39(Item):
    name = "I2"
    title = ""
    var = Variation_5
class Item_42(Item):
    name = "I3"
    title = ""
    var = Variation_2
class Item_1(Spare):
    bit_offset8 = 4
    bit_size = 8
class Item_45(Item):
    name = "I4"
    title = ""
    var = Variation_22
class Variation_23(Group):
    items = [Item_33, Item_2, Item_39, Item_42, Item_1, Item_45]
class Item_34(Item):
    name = "I1"
    title = ""
    var = Variation_4
class Item_3(Spare):
    bit_offset8 = 7
    bit_size = 2
class Item_40(Item):
    name = "I2"
    title = ""
    var = Variation_20
class Item_4(Spare):
    bit_offset8 = 7
    bit_size = 9
class Variation_24(Group):
    items = [Item_34, Item_3, Item_40, Item_4]
class Item_35(Item):
    name = "I1"
    title = ""
    var = Variation_5
class Variation_25(Group):
    items = [Item_35, Item_39]
class Item_47(Item):
    name = "IM"
    title = ""
    var = Variation_0
class Item_46(Item):
    name = "IAS"
    title = ""
    var = Variation_21
class Variation_26(Group):
    items = [Item_47, Item_46]
class Item_53(Item):
    name = "R"
    title = "Raw"
    var = Variation_5
class Item_59(Item):
    name = "T"
    title = "Table"
    var = Variation_6
class Item_54(Item):
    name = "S1"
    title = "String Ascii"
    var = Variation_18
class Item_55(Item):
    name = "S2"
    title = "String ICAO"
    var = Variation_16
class Item_56(Item):
    name = "S3"
    title = "String Octal"
    var = Variation_11
class Item_0(Spare):
    bit_offset8 = 4
    bit_size = 4
class Item_37(Item):
    name = "I1"
    title = "Unsigned Integer"
    var = Variation_8
class Item_41(Item):
    name = "I2"
    title = "Signed Integer"
    var = Variation_7
class Item_48(Item):
    name = "Q1LAT"
    title = "Latitude in WGS.84 in Two's Complement Form"
    var = Variation_14
class Item_49(Item):
    name = "Q2LON"
    title = "Longitude in WGS.84 in Two's Complement Form"
    var = Variation_14
class Item_50(Item):
    name = "Q3"
    title = "Unsigned Quantity"
    var = Variation_13
class Item_51(Item):
    name = "Q4"
    title = "Quantity No Unit"
    var = Variation_9
class Item_52(Item):
    name = "Q5"
    title = "Negative Lsb"
    var = Variation_10
class Item_30(Item):
    name = "B1"
    title = "Bds With Address"
    var = Variation_19
class Item_31(Item):
    name = "B2"
    title = "Bds At Unknown Address"
    var = Variation_17
class Item_32(Item):
    name = "B3"
    title = "Bds At Known Address"
    var = Variation_17
class Variation_27(Group):
    items = [Item_53, Item_59, Item_54, Item_55, Item_56, Item_0, Item_37, Item_41, Item_48, Item_49, Item_50, Item_51, Item_52, Item_30, Item_31, Item_32]
class Item_57(Item):
    name = "SAC"
    title = "System Area Code"
    var = Variation_5
class Item_58(Item):
    name = "SIC"
    title = "System Identification Code"
    var = Variation_5
class Variation_28(Group):
    items = [Item_57, Item_58]
class Item_38(Item):
    name = "I2"
    title = ""
    var = Variation_4
class Item_43(Item):
    name = "I3"
    title = ""
    var = Variation_4
class Variation_29(Extended):
    items = [Item_34, None, Item_38, None, Item_43, None]
class Item_44(Item):
    name = "I3"
    title = ""
    var = Variation_5
class Variation_30(Extended):
    items = [Item_34, None, Item_38, None, Item_44]
class Item_60(Item):
    name = "TYP"
    title = ""
    var = Variation_1
class Item_36(Item):
    name = "I1"
    title = ""
    var = Variation_20
class Variation_31(Extended):
    items = [Item_60, Item_36, None, Item_38, None]
class Variation_32(Repetitive):
    rep = 1
    var = Variation_5
class Variation_33(Repetitive):
    rep = 1
    var = Variation_25
class Variation_34(Repetitive):
    rep = None
    var = Variation_4
class Variation_35(Explicit):
    t = None
class Variation_36(Explicit):
    t = ReservedExpansion
class Variation_37(Explicit):
    t = SpecialPurpose
class Item_5(Item):
    name = "010"
    title = ""
    var = Variation_5
class Variation_38(Compound):
    fspec_size = None
    items_list = [Item_5]
    items_dict = {"010": ("", Variation_5, 0x80)}
    @classmethod
    def spec(cls, key : Literal["010"]) -> Variation_5:
        return cls.items_dict[key][1] # type: ignore
class Item_25(Item):
    name = "101"
    title = ""
    var = Variation_5
class Item_26(Item):
    name = "102"
    title = ""
    var = Variation_5
class Variation_39(Compound):
    fspec_size = None
    items_list = [Item_5, Item_25, Item_26]
    items_dict = {"010": ("", Variation_5, 0x80), "101": ("", Variation_5, 0x40), "102": ("", Variation_5, 0x20)}
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
    def spec(cls, key : Any) -> Any: return cls.items_dict[key][1]
class Item_27(Item):
    name = "201"
    title = ""
    var = Variation_12
class Item_28(Item):
    name = "202"
    title = ""
    var = Variation_12
class Variation_40(Compound):
    fspec_size = None
    items_list = [Item_5, Item_27, Item_28]
    items_dict = {"010": ("", Variation_5, 0x80), "201": ("", Variation_12, 0x40), "202": ("", Variation_12, 0x20)}
    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["201"]) -> Variation_12: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["202"]) -> Variation_12: ...
    @classmethod
    def spec(cls, key : Any) -> Any: return cls.items_dict[key][1]
class Item_29(Item):
    name = "301"
    title = ""
    var = Variation_15
class Variation_41(Compound):
    fspec_size = None
    items_list = [Item_5, Item_29]
    items_dict = {"010": ("", Variation_5, 0x80), "301": ("", Variation_15, 0x40)}
    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["301"]) -> Variation_15: ...
    @classmethod
    def spec(cls, key : Any) -> Any: return cls.items_dict[key][1]
class Item_6(Item):
    name = "010"
    title = "Data Source Identifier"
    var = Variation_28
class Item_7(Item):
    name = "020"
    title = "Different Contents"
    var = Variation_27
class Item_9(Item):
    name = "030"
    title = "Dependent Item"
    var = Variation_26
class Item_12(Item):
    name = "040"
    title = "Spare Items"
    var = Variation_24
class Item_13(Item):
    name = "051"
    title = "Element"
    var = Variation_5
class Item_14(Item):
    name = "052"
    title = "Group"
    var = Variation_23
class Item_15(Item):
    name = "053"
    title = "Extended With Trailing Fx"
    var = Variation_29
class Item_16(Item):
    name = "054"
    title = "Extended Without Trailing Fx"
    var = Variation_30
class Item_17(Item):
    name = "061"
    title = "Repetitive Regular"
    var = Variation_32
class Item_18(Item):
    name = "062"
    title = "Repetitive With Group"
    var = Variation_33
class Item_19(Item):
    name = "063"
    title = "Repetitive Fx"
    var = Variation_34
class Item_20(Item):
    name = "071"
    title = "Explicit None"
    var = Variation_35
class Item_21(Item):
    name = "072"
    title = "Explicit RE"
    var = Variation_36
class Item_22(Item):
    name = "073"
    title = "Explicit SP"
    var = Variation_37
class Variation_45(Compound):
    fspec_size = None
    items_list = [Item_35, None, Item_39]
    items_dict = {"I1": ("", Variation_5, 0x80), "I2": ("", Variation_5, 0x20)}
    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Variation_5: ...
    @classmethod
    def spec(cls, key : Any) -> Any: return cls.items_dict[key][1]
class Item_23(Item):
    name = "091"
    title = "Compound Fspec With Fx"
    var = Variation_45
class Variation_46(Compound):
    fspec_size = 1
    items_list = [Item_35, None, Item_39]
    items_dict = {"I1": ("", Variation_5, 0x80), "I2": ("", Variation_5, 0x20)}
    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Variation_5: ...
    @classmethod
    def spec(cls, key : Any) -> Any: return cls.items_dict[key][1]
class Item_24(Item):
    name = "092"
    title = "Compound Fixed Size Fspec"
    var = Variation_46
class Variation_42(Compound):
    fspec_size = None
    items_list = [Item_6, Item_7, Item_9, Item_12, Item_13, Item_14, Item_15, Item_16, Item_17, Item_18, Item_19, Item_20, Item_21, Item_22, None, Item_23, Item_24]
    items_dict = {"010": ("Data Source Identifier", Variation_28, 0x800000), "020": ("Different Contents", Variation_27, 0x400000), "030": ("Dependent Item", Variation_26, 0x200000), "040": ("Spare Items", Variation_24, 0x100000), "051": ("Element", Variation_5, 0x080000), "052": ("Group", Variation_23, 0x040000), "053": ("Extended With Trailing Fx", Variation_29, 0x020000), "054": ("Extended Without Trailing Fx", Variation_30, 0x018000), "061": ("Repetitive Regular", Variation_32, 0x014000), "062": ("Repetitive With Group", Variation_33, 0x012000), "063": ("Repetitive Fx", Variation_34, 0x011000), "071": ("Explicit None", Variation_35, 0x010800), "072": ("Explicit RE", Variation_36, 0x010400), "073": ("Explicit SP", Variation_37, 0x010200), "091": ("Compound Fspec With Fx", Variation_45, 0x010140), "092": ("Compound Fixed Size Fspec", Variation_46, 0x010120)}
    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Variation_28: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["020"]) -> Variation_27: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["030"]) -> Variation_26: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["040"]) -> Variation_24: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["051"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["052"]) -> Variation_23: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["053"]) -> Variation_29: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["054"]) -> Variation_30: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["061"]) -> Variation_32: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["062"]) -> Variation_33: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["063"]) -> Variation_34: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["071"]) -> Variation_35: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["072"]) -> Variation_36: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["073"]) -> Variation_37: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["091"]) -> Variation_45: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["092"]) -> Variation_46: ...
    @classmethod
    def spec(cls, key : Any) -> Any: return cls.items_dict[key][1]
class Item_8(Item):
    name = "020"
    title = "Target Report Descriptor"
    var = Variation_31
class Item_10(Item):
    name = "031"
    title = "For Plots Only"
    var = Variation_5
class Variation_43(Compound):
    fspec_size = None
    items_list = [Item_6, Item_8, Item_10]
    items_dict = {"010": ("Data Source Identifier", Variation_28, 0x80), "020": ("Target Report Descriptor", Variation_31, 0x40), "031": ("For Plots Only", Variation_5, 0x20)}
    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Variation_28: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["020"]) -> Variation_31: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["031"]) -> Variation_5: ...
    @classmethod
    def spec(cls, key : Any) -> Any: return cls.items_dict[key][1]
class Item_11(Item):
    name = "032"
    title = "For Tracks Only"
    var = Variation_12
class Variation_44(Compound):
    fspec_size = None
    items_list = [Item_6, Item_8, Item_11]
    items_dict = {"010": ("Data Source Identifier", Variation_28, 0x80), "020": ("Target Report Descriptor", Variation_31, 0x40), "032": ("For Tracks Only", Variation_12, 0x20)}
    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Variation_28: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["020"]) -> Variation_31: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["032"]) -> Variation_12: ...
    @classmethod
    def spec(cls, key : Any) -> Any: return cls.items_dict[key][1]
class Variation_47(Compound):
    fspec_size = 1
    items_list = [Item_35, Item_39]
    items_dict = {"I1": ("", Variation_5, 0x80), "I2": ("", Variation_5, 0x40)}
    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Variation_5: ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Variation_5: ...
    @classmethod
    def spec(cls, key : Any) -> Any: return cls.items_dict[key][1]

# Uap set
class Uap_0(UapSingle):
    var = Variation_42
class Uap_1(UapMultiple):
    uaps = {"plot": Variation_43, "track": Variation_44}
    selector = (["020", "TYP"], {0: "plot", 1: "track"})
class Uap_2(UapMultiple):
    uaps = {"uap1": Variation_39, "uap2": Variation_40, "uap3": Variation_41, "uap4": Variation_38}
    selector = None

# Spec set
class AstSpec_0(AstCat):
    uap = Uap_0
class AstSpec_1(AstCat):
    uap = Uap_1
class AstSpec_2(AstCat):
    uap = Uap_2
class AstSpec_3(AstRef):
    var = Variation_47

# Asterix set
class Asterix_0(Asterix):
    cat = 0
    edition = (1, 0)
    astspec = AstSpec_0
class Asterix_1(Asterix):
    cat = 0
    edition = (1, 0)
    astspec = AstSpec_3
class Asterix_2(Asterix):
    cat = 1
    edition = (1, 0)
    astspec = AstSpec_1
class Asterix_3(Asterix):
    cat = 2
    edition = (1, 0)
    astspec = AstSpec_2

# Aliases
Cat_000_1_0: TypeAlias = Asterix_0
Ref_000_1_0: TypeAlias = Asterix_1
Cat_001_1_0: TypeAlias = Asterix_2
Cat_002_1_0: TypeAlias = Asterix_3

# Manifest
manifest = [Cat_000_1_0, Ref_000_1_0, Cat_001_1_0, Cat_002_1_0]
