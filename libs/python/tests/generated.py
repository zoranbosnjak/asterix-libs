# Asterix specifications

# This file is generated, DO NOT EDIT!
# For more details, see:
#     - https://github.com/zoranbosnjak/asterix-specs

from asterix.base import *

# Asterix types

Content_0_Arg: TypeAlias = int
class Content_0(ContentRaw):
    pass

RuleContent_0_Arg: TypeAlias = Content_0_Arg
class RuleContent_0(RuleContentContextFree):
    cv_content = Content_0

    @property
    def content(self) -> Content_0:
        return self._get_content() # type: ignore

Variation_7_Arg: TypeAlias = RuleContent_0_Arg
class Variation_7(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_7_Arg) -> "Variation_7":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_7_Arg: TypeAlias = Variation_7_Arg
class RuleVariation_7(RuleVariationContextFree):
    cv_variation = Variation_7

    @classmethod
    def create(cls, arg : RuleVariation_7_Arg) -> "RuleVariation_7":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_7:
        return self.arg # type: ignore

NonSpare_71_Arg: TypeAlias = RuleVariation_7_Arg
class NonSpare_71(NonSpare):
    cv_name = "SAC"
    cv_title = "System Area Code"
    cv_rule = RuleVariation_7

    @classmethod
    def create(cls, arg : NonSpare_71_Arg) -> "NonSpare_71":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

Item_30_Arg: TypeAlias = NonSpare_71_Arg
class Item_30(Item):
    cv_non_spare = NonSpare_71

    @classmethod
    def create(cls, arg : Item_30_Arg) -> "Item_30":
        return cls._create(arg) # type: ignore

NonSpare_74_Arg: TypeAlias = RuleVariation_7_Arg
class NonSpare_74(NonSpare):
    cv_name = "SIC"
    cv_title = "System Identification Code"
    cv_rule = RuleVariation_7

    @classmethod
    def create(cls, arg : NonSpare_74_Arg) -> "NonSpare_74":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

Item_33_Arg: TypeAlias = NonSpare_74_Arg
class Item_33(Item):
    cv_non_spare = NonSpare_74

    @classmethod
    def create(cls, arg : Item_33_Arg) -> "Item_33":
        return cls._create(arg) # type: ignore

Variation_43_Arg_Group = Tuple[Union[RuleVariation_7_Arg, Tuple[Literal["SAC"], RuleVariation_7_Arg]], Union[RuleVariation_7_Arg, Tuple[Literal["SIC"], RuleVariation_7_Arg]]]
Variation_43_Arg: TypeAlias = Union[int, Variation_43_Arg_Group]
class Variation_43(Group):
    cv_bit_offset8 = 0
    cv_bit_size = 16
    cv_items_list = [(Item_30, 8), (Item_33, 8)]
    cv_items_dict = {"SAC": RuleVariation_7, "SIC": RuleVariation_7}

    @overload
    @classmethod
    def spec(cls, key : Literal["SAC"]) -> RuleVariation_7:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["SIC"]) -> RuleVariation_7:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["SAC"], Literal["SIC"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["SAC"]) -> RuleVariation_7:
        ...
    @overload
    def get_item(self, key : Literal["SIC"]) -> RuleVariation_7:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg : Variation_43_Arg) -> 'Variation_43':
        return cls._create(arg) # type: ignore

RuleVariation_39_Arg: TypeAlias = Variation_43_Arg
class RuleVariation_39(RuleVariationContextFree):
    cv_variation = Variation_43

    @classmethod
    def create(cls, arg : RuleVariation_39_Arg) -> "RuleVariation_39":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_43:
        return self.arg # type: ignore

NonSpare_3_Arg: TypeAlias = RuleVariation_39_Arg
class NonSpare_3(NonSpare):
    cv_name = "010"
    cv_title = "Data Source Identifier"
    cv_rule = RuleVariation_39

    @classmethod
    def create(cls, arg : NonSpare_3_Arg) -> "NonSpare_3":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_39:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_43:
        return self.rule.variation

class UapItem_3(UapItem):
    cv_non_spare = NonSpare_3

Content_7_Arg: TypeAlias = int
class Content_7(ContentTable):
    cv_values = {1: "Message 1", 2: "Message 2", 3: "Message 3"}

RuleContent_7_Arg: TypeAlias = Content_7_Arg
class RuleContent_7(RuleContentContextFree):
    cv_content = Content_7

    @property
    def content(self) -> Content_7:
        return self._get_content() # type: ignore

Variation_9_Arg: TypeAlias = RuleContent_7_Arg
class Variation_9(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_7

    @classmethod
    def create(cls, arg : Variation_9_Arg) -> "Variation_9":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_7:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_7:
        return self.rule.content

RuleVariation_9_Arg: TypeAlias = Variation_9_Arg
class RuleVariation_9(RuleVariationContextFree):
    cv_variation = Variation_9

    @classmethod
    def create(cls, arg : RuleVariation_9_Arg) -> "RuleVariation_9":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_9:
        return self.arg # type: ignore

NonSpare_0_Arg: TypeAlias = RuleVariation_9_Arg
class NonSpare_0(NonSpare):
    cv_name = "000"
    cv_title = "Message Type"
    cv_rule = RuleVariation_9

    @classmethod
    def create(cls, arg : NonSpare_0_Arg) -> "NonSpare_0":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_9:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_9:
        return self.rule.variation

class UapItem_0(UapItem):
    cv_non_spare = NonSpare_0

NonSpare_67_Arg: TypeAlias = RuleVariation_7_Arg
class NonSpare_67(NonSpare):
    cv_name = "R"
    cv_title = "Raw"
    cv_rule = RuleVariation_7

    @classmethod
    def create(cls, arg : NonSpare_67_Arg) -> "NonSpare_67":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

Content_4_Arg: TypeAlias = int
class Content_4(ContentTable):
    cv_values = {0: "Test 0", 1: "Test 1", 2: "Test 2", 3: "Test 3"}

RuleContent_4_Arg: TypeAlias = Content_4_Arg
class RuleContent_4(RuleContentContextFree):
    cv_content = Content_4

    @property
    def content(self) -> Content_4:
        return self._get_content() # type: ignore

Variation_8_Arg: TypeAlias = RuleContent_4_Arg
class Variation_8(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_4

    @classmethod
    def create(cls, arg : Variation_8_Arg) -> "Variation_8":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_4:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_4:
        return self.rule.content

RuleVariation_8_Arg: TypeAlias = Variation_8_Arg
class RuleVariation_8(RuleVariationContextFree):
    cv_variation = Variation_8

    @classmethod
    def create(cls, arg : RuleVariation_8_Arg) -> "RuleVariation_8":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_8:
        return self.arg # type: ignore

NonSpare_75_Arg: TypeAlias = RuleVariation_8_Arg
class NonSpare_75(NonSpare):
    cv_name = "T"
    cv_title = "Table"
    cv_rule = RuleVariation_8

    @classmethod
    def create(cls, arg : NonSpare_75_Arg) -> "NonSpare_75":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_8:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_8:
        return self.rule.variation

Content_9_Arg: TypeAlias = Union[int, str]
class Content_9(ContentString):
    cv_string_type = StringAscii

RuleContent_9_Arg: TypeAlias = Content_9_Arg
class RuleContent_9(RuleContentContextFree):
    cv_content = Content_9

    @property
    def content(self) -> Content_9:
        return self._get_content() # type: ignore

Variation_22_Arg: TypeAlias = RuleContent_9_Arg
class Variation_22(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 56
    cv_rule = RuleContent_9

    @classmethod
    def create(cls, arg : Variation_22_Arg) -> "Variation_22":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_9:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_9:
        return self.rule.content

RuleVariation_22_Arg: TypeAlias = Variation_22_Arg
class RuleVariation_22(RuleVariationContextFree):
    cv_variation = Variation_22

    @classmethod
    def create(cls, arg : RuleVariation_22_Arg) -> "RuleVariation_22":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_22:
        return self.arg # type: ignore

NonSpare_68_Arg: TypeAlias = RuleVariation_22_Arg
class NonSpare_68(NonSpare):
    cv_name = "S1"
    cv_title = "String Ascii"
    cv_rule = RuleVariation_22

    @classmethod
    def create(cls, arg : NonSpare_68_Arg) -> "NonSpare_68":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_22:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_22:
        return self.rule.variation

Content_10_Arg: TypeAlias = Union[int, str]
class Content_10(ContentString):
    cv_string_type = StringICAO

RuleContent_10_Arg: TypeAlias = Content_10_Arg
class RuleContent_10(RuleContentContextFree):
    cv_content = Content_10

    @property
    def content(self) -> Content_10:
        return self._get_content() # type: ignore

Variation_21_Arg: TypeAlias = RuleContent_10_Arg
class Variation_21(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 48
    cv_rule = RuleContent_10

    @classmethod
    def create(cls, arg : Variation_21_Arg) -> "Variation_21":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_10:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_10:
        return self.rule.content

RuleVariation_21_Arg: TypeAlias = Variation_21_Arg
class RuleVariation_21(RuleVariationContextFree):
    cv_variation = Variation_21

    @classmethod
    def create(cls, arg : RuleVariation_21_Arg) -> "RuleVariation_21":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_21:
        return self.arg # type: ignore

NonSpare_69_Arg: TypeAlias = RuleVariation_21_Arg
class NonSpare_69(NonSpare):
    cv_name = "S2"
    cv_title = "String ICAO"
    cv_rule = RuleVariation_21

    @classmethod
    def create(cls, arg : NonSpare_69_Arg) -> "NonSpare_69":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_21:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_21:
        return self.rule.variation

Content_11_Arg: TypeAlias = Union[int, str]
class Content_11(ContentString):
    cv_string_type = StringOctal

RuleContent_11_Arg: TypeAlias = Content_11_Arg
class RuleContent_11(RuleContentContextFree):
    cv_content = Content_11

    @property
    def content(self) -> Content_11:
        return self._get_content() # type: ignore

Variation_17_Arg: TypeAlias = RuleContent_11_Arg
class Variation_17(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 24
    cv_rule = RuleContent_11

    @classmethod
    def create(cls, arg : Variation_17_Arg) -> "Variation_17":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_11:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_11:
        return self.rule.content

RuleVariation_17_Arg: TypeAlias = Variation_17_Arg
class RuleVariation_17(RuleVariationContextFree):
    cv_variation = Variation_17

    @classmethod
    def create(cls, arg : RuleVariation_17_Arg) -> "RuleVariation_17":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_17:
        return self.arg # type: ignore

NonSpare_70_Arg: TypeAlias = RuleVariation_17_Arg
class NonSpare_70(NonSpare):
    cv_name = "S3"
    cv_title = "String Octal"
    cv_rule = RuleVariation_17

    @classmethod
    def create(cls, arg : NonSpare_70_Arg) -> "NonSpare_70":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_17:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_17:
        return self.rule.variation

Content_13_Arg: TypeAlias = int
class Content_13(ContentInteger):
    cv_signedness = Unsigned

RuleContent_13_Arg: TypeAlias = Content_13_Arg
class RuleContent_13(RuleContentContextFree):
    cv_content = Content_13

    @property
    def content(self) -> Content_13:
        return self._get_content() # type: ignore

Variation_11_Arg: TypeAlias = RuleContent_13_Arg
class Variation_11(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_13

    @classmethod
    def create(cls, arg : Variation_11_Arg) -> "Variation_11":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_13:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_13:
        return self.rule.content

RuleVariation_11_Arg: TypeAlias = Variation_11_Arg
class RuleVariation_11(RuleVariationContextFree):
    cv_variation = Variation_11

    @classmethod
    def create(cls, arg : RuleVariation_11_Arg) -> "RuleVariation_11":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_11:
        return self.arg # type: ignore

NonSpare_47_Arg: TypeAlias = RuleVariation_11_Arg
class NonSpare_47(NonSpare):
    cv_name = "I1"
    cv_title = "Unsigned Integer"
    cv_rule = RuleVariation_11

    @classmethod
    def create(cls, arg : NonSpare_47_Arg) -> "NonSpare_47":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_11:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_11:
        return self.rule.variation

Content_12_Arg: TypeAlias = int
class Content_12(ContentInteger):
    cv_signedness = Signed

RuleContent_12_Arg: TypeAlias = Content_12_Arg
class RuleContent_12(RuleContentContextFree):
    cv_content = Content_12

    @property
    def content(self) -> Content_12:
        return self._get_content() # type: ignore

Variation_10_Arg: TypeAlias = RuleContent_12_Arg
class Variation_10(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_12

    @classmethod
    def create(cls, arg : Variation_10_Arg) -> "Variation_10":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_12:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_12:
        return self.rule.content

RuleVariation_10_Arg: TypeAlias = Variation_10_Arg
class RuleVariation_10(RuleVariationContextFree):
    cv_variation = Variation_10

    @classmethod
    def create(cls, arg : RuleVariation_10_Arg) -> "RuleVariation_10":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_10:
        return self.arg # type: ignore

NonSpare_52_Arg: TypeAlias = RuleVariation_10_Arg
class NonSpare_52(NonSpare):
    cv_name = "I2"
    cv_title = "Signed Integer"
    cv_rule = RuleVariation_10

    @classmethod
    def create(cls, arg : NonSpare_52_Arg) -> "NonSpare_52":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_10:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_10:
        return self.rule.variation

Content_15_Arg: TypeAlias = Union[int, float, Tuple[float, Literal["°"]]]
class Content_15(ContentQuantity):
    cv_signedness = Signed
    cv_lsb = 2.1457672119140625e-5
    cv_unit = "°"

    def as_quantity(self, cv_unit : Optional[Literal["°"]] = None) -> float:
        return self._as_quantity()

RuleContent_15_Arg: TypeAlias = Content_15_Arg
class RuleContent_15(RuleContentContextFree):
    cv_content = Content_15

    @property
    def content(self) -> Content_15:
        return self._get_content() # type: ignore

Variation_19_Arg: TypeAlias = RuleContent_15_Arg
class Variation_19(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 24
    cv_rule = RuleContent_15

    @classmethod
    def create(cls, arg : Variation_19_Arg) -> "Variation_19":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_15:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_15:
        return self.rule.content

RuleVariation_19_Arg: TypeAlias = Variation_19_Arg
class RuleVariation_19(RuleVariationContextFree):
    cv_variation = Variation_19

    @classmethod
    def create(cls, arg : RuleVariation_19_Arg) -> "RuleVariation_19":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_19:
        return self.arg # type: ignore

NonSpare_62_Arg: TypeAlias = RuleVariation_19_Arg
class NonSpare_62(NonSpare):
    cv_name = "Q1LAT"
    cv_title = "Latitude in WGS.84 in Two's Complement Form"
    cv_rule = RuleVariation_19

    @classmethod
    def create(cls, arg : NonSpare_62_Arg) -> "NonSpare_62":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_19:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_19:
        return self.rule.variation

Content_14_Arg: TypeAlias = Union[int, float, Tuple[float, Literal["°"]]]
class Content_14(ContentQuantity):
    cv_signedness = Signed
    cv_lsb = 2.1457672119140625e-5
    cv_unit = "°"

    def as_quantity(self, cv_unit : Optional[Literal["°"]] = None) -> float:
        return self._as_quantity()

RuleContent_14_Arg: TypeAlias = Content_14_Arg
class RuleContent_14(RuleContentContextFree):
    cv_content = Content_14

    @property
    def content(self) -> Content_14:
        return self._get_content() # type: ignore

Variation_18_Arg: TypeAlias = RuleContent_14_Arg
class Variation_18(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 24
    cv_rule = RuleContent_14

    @classmethod
    def create(cls, arg : Variation_18_Arg) -> "Variation_18":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_14:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_14:
        return self.rule.content

RuleVariation_18_Arg: TypeAlias = Variation_18_Arg
class RuleVariation_18(RuleVariationContextFree):
    cv_variation = Variation_18

    @classmethod
    def create(cls, arg : RuleVariation_18_Arg) -> "RuleVariation_18":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_18:
        return self.arg # type: ignore

NonSpare_63_Arg: TypeAlias = RuleVariation_18_Arg
class NonSpare_63(NonSpare):
    cv_name = "Q2LON"
    cv_title = "Longitude in WGS.84 in Two's Complement Form"
    cv_rule = RuleVariation_18

    @classmethod
    def create(cls, arg : NonSpare_63_Arg) -> "NonSpare_63":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_18:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_18:
        return self.rule.variation

Content_17_Arg: TypeAlias = Union[int, float, Tuple[float, Literal["kt"]]]
class Content_17(ContentQuantity):
    cv_signedness = Unsigned
    cv_lsb = 1.0
    cv_unit = "kt"

    def as_quantity(self, cv_unit : Optional[Literal["kt"]] = None) -> float:
        return self._as_quantity()

RuleContent_17_Arg: TypeAlias = Content_17_Arg
class RuleContent_17(RuleContentContextFree):
    cv_content = Content_17

    @property
    def content(self) -> Content_17:
        return self._get_content() # type: ignore

Variation_16_Arg: TypeAlias = RuleContent_17_Arg
class Variation_16(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 16
    cv_rule = RuleContent_17

    @classmethod
    def create(cls, arg : Variation_16_Arg) -> "Variation_16":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_17:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_17:
        return self.rule.content

RuleVariation_16_Arg: TypeAlias = Variation_16_Arg
class RuleVariation_16(RuleVariationContextFree):
    cv_variation = Variation_16

    @classmethod
    def create(cls, arg : RuleVariation_16_Arg) -> "RuleVariation_16":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_16:
        return self.arg # type: ignore

NonSpare_64_Arg: TypeAlias = RuleVariation_16_Arg
class NonSpare_64(NonSpare):
    cv_name = "Q3"
    cv_title = "Unsigned Quantity"
    cv_rule = RuleVariation_16

    @classmethod
    def create(cls, arg : NonSpare_64_Arg) -> "NonSpare_64":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_16:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_16:
        return self.rule.variation

Content_16_Arg: TypeAlias = Union[int, float, Tuple[float, Literal[""]]]
class Content_16(ContentQuantity):
    cv_signedness = Unsigned
    cv_lsb = 1.0
    cv_unit = ""

    def as_quantity(self, cv_unit : Optional[Literal[""]] = None) -> float:
        return self._as_quantity()

RuleContent_16_Arg: TypeAlias = Content_16_Arg
class RuleContent_16(RuleContentContextFree):
    cv_content = Content_16

    @property
    def content(self) -> Content_16:
        return self._get_content() # type: ignore

Variation_12_Arg: TypeAlias = RuleContent_16_Arg
class Variation_12(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_16

    @classmethod
    def create(cls, arg : Variation_12_Arg) -> "Variation_12":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_16:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_16:
        return self.rule.content

RuleVariation_12_Arg: TypeAlias = Variation_12_Arg
class RuleVariation_12(RuleVariationContextFree):
    cv_variation = Variation_12

    @classmethod
    def create(cls, arg : RuleVariation_12_Arg) -> "RuleVariation_12":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_12:
        return self.arg # type: ignore

NonSpare_65_Arg: TypeAlias = RuleVariation_12_Arg
class NonSpare_65(NonSpare):
    cv_name = "Q4"
    cv_title = "Quantity No Unit"
    cv_rule = RuleVariation_12

    @classmethod
    def create(cls, arg : NonSpare_65_Arg) -> "NonSpare_65":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_12:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_12:
        return self.rule.variation

Content_18_Arg: TypeAlias = Union[int, float, Tuple[float, Literal[""]]]
class Content_18(ContentQuantity):
    cv_signedness = Unsigned
    cv_lsb = -0.5
    cv_unit = ""

    def as_quantity(self, cv_unit : Optional[Literal[""]] = None) -> float:
        return self._as_quantity()

RuleContent_18_Arg: TypeAlias = Content_18_Arg
class RuleContent_18(RuleContentContextFree):
    cv_content = Content_18

    @property
    def content(self) -> Content_18:
        return self._get_content() # type: ignore

Variation_13_Arg: TypeAlias = RuleContent_18_Arg
class Variation_13(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_18

    @classmethod
    def create(cls, arg : Variation_13_Arg) -> "Variation_13":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_18:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_18:
        return self.rule.content

RuleVariation_13_Arg: TypeAlias = Variation_13_Arg
class RuleVariation_13(RuleVariationContextFree):
    cv_variation = Variation_13

    @classmethod
    def create(cls, arg : RuleVariation_13_Arg) -> "RuleVariation_13":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_13:
        return self.arg # type: ignore

NonSpare_66_Arg: TypeAlias = RuleVariation_13_Arg
class NonSpare_66(NonSpare):
    cv_name = "Q5"
    cv_title = "Negative Lsb"
    cv_rule = RuleVariation_13

    @classmethod
    def create(cls, arg : NonSpare_66_Arg) -> "NonSpare_66":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_13:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_13:
        return self.rule.variation

Content_24_Arg: TypeAlias = int
class Content_24(ContentBds):
    cv_bds_type = BdsWithAddress

RuleContent_19_Arg: TypeAlias = Content_24_Arg
class RuleContent_19(RuleContentContextFree):
    cv_content = Content_24

    @property
    def content(self) -> Content_24:
        return self._get_content() # type: ignore

Variation_25_Arg: TypeAlias = RuleContent_19_Arg
class Variation_25(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 64
    cv_rule = RuleContent_19

    @classmethod
    def create(cls, arg : Variation_25_Arg) -> "Variation_25":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_19:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_24:
        return self.rule.content

RuleVariation_25_Arg: TypeAlias = Variation_25_Arg
class RuleVariation_25(RuleVariationContextFree):
    cv_variation = Variation_25

    @classmethod
    def create(cls, arg : RuleVariation_25_Arg) -> "RuleVariation_25":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_25:
        return self.arg # type: ignore

NonSpare_33_Arg: TypeAlias = RuleVariation_25_Arg
class NonSpare_33(NonSpare):
    cv_name = "B1"
    cv_title = "Bds With Address"
    cv_rule = RuleVariation_25

    @classmethod
    def create(cls, arg : NonSpare_33_Arg) -> "NonSpare_33":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_25:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_25:
        return self.rule.variation

Content_25_Arg: TypeAlias = int
class Content_25(ContentBds):
    cv_bds_type = (BdsAt, None)

RuleContent_20_Arg: TypeAlias = Content_25_Arg
class RuleContent_20(RuleContentContextFree):
    cv_content = Content_25

    @property
    def content(self) -> Content_25:
        return self._get_content() # type: ignore

Variation_23_Arg: TypeAlias = RuleContent_20_Arg
class Variation_23(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 56
    cv_rule = RuleContent_20

    @classmethod
    def create(cls, arg : Variation_23_Arg) -> "Variation_23":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_20:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_25:
        return self.rule.content

RuleVariation_23_Arg: TypeAlias = Variation_23_Arg
class RuleVariation_23(RuleVariationContextFree):
    cv_variation = Variation_23

    @classmethod
    def create(cls, arg : RuleVariation_23_Arg) -> "RuleVariation_23":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_23:
        return self.arg # type: ignore

NonSpare_34_Arg: TypeAlias = RuleVariation_23_Arg
class NonSpare_34(NonSpare):
    cv_name = "B2"
    cv_title = "Bds At Unknown Address"
    cv_rule = RuleVariation_23

    @classmethod
    def create(cls, arg : NonSpare_34_Arg) -> "NonSpare_34":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_23:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_23:
        return self.rule.variation

Content_26_Arg: TypeAlias = int
class Content_26(ContentBds):
    cv_bds_type = (BdsAt, 48)

RuleContent_21_Arg: TypeAlias = Content_26_Arg
class RuleContent_21(RuleContentContextFree):
    cv_content = Content_26

    @property
    def content(self) -> Content_26:
        return self._get_content() # type: ignore

Variation_24_Arg: TypeAlias = RuleContent_21_Arg
class Variation_24(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 56
    cv_rule = RuleContent_21

    @classmethod
    def create(cls, arg : Variation_24_Arg) -> "Variation_24":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_21:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_26:
        return self.rule.content

RuleVariation_24_Arg: TypeAlias = Variation_24_Arg
class RuleVariation_24(RuleVariationContextFree):
    cv_variation = Variation_24

    @classmethod
    def create(cls, arg : RuleVariation_24_Arg) -> "RuleVariation_24":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_24:
        return self.arg # type: ignore

NonSpare_35_Arg: TypeAlias = RuleVariation_24_Arg
class NonSpare_35(NonSpare):
    cv_name = "B3"
    cv_title = "Bds At Known Address"
    cv_rule = RuleVariation_24

    @classmethod
    def create(cls, arg : NonSpare_35_Arg) -> "NonSpare_35":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_24:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_24:
        return self.rule.variation

Variation_63_Arg = TypedDict('Variation_63_Arg', {
    "R": NonSpare_67_Arg,
    "T": NonSpare_75_Arg,
    "S1": NonSpare_68_Arg,
    "S2": NonSpare_69_Arg,
    "S3": NonSpare_70_Arg,
    "I1": NonSpare_47_Arg,
    "I2": NonSpare_52_Arg,
    "Q1LAT": NonSpare_62_Arg,
    "Q2LON": NonSpare_63_Arg,
    "Q3": NonSpare_64_Arg,
    "Q4": NonSpare_65_Arg,
    "Q5": NonSpare_66_Arg,
    "B1": NonSpare_33_Arg,
    "B2": NonSpare_34_Arg,
    "B3": NonSpare_35_Arg,
}, total=False)
class Variation_63(Compound):
    cv_fspec_max_bytes = 3
    cv_items_list = [NonSpare_67, NonSpare_75, NonSpare_68, NonSpare_69, NonSpare_70, NonSpare_47, NonSpare_52, NonSpare_62, NonSpare_63, NonSpare_64, NonSpare_65, NonSpare_66, NonSpare_33, NonSpare_34, NonSpare_35]
    cv_items_dict = {"R": NonSpare_67, "T": NonSpare_75, "S1": NonSpare_68, "S2": NonSpare_69, "S3": NonSpare_70, "I1": NonSpare_47, "I2": NonSpare_52, "Q1LAT": NonSpare_62, "Q2LON": NonSpare_63, "Q3": NonSpare_64, "Q4": NonSpare_65, "Q5": NonSpare_66, "B1": NonSpare_33, "B2": NonSpare_34, "B3": NonSpare_35}

    @overload
    @classmethod
    def spec(cls, key : Literal["R"]) -> NonSpare_67:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["T"]) -> NonSpare_75:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["S1"]) -> NonSpare_68:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["S2"]) -> NonSpare_69:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["S3"]) -> NonSpare_70:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> NonSpare_47:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> NonSpare_52:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["Q1LAT"]) -> NonSpare_62:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["Q2LON"]) -> NonSpare_63:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["Q3"]) -> NonSpare_64:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["Q4"]) -> NonSpare_65:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["Q5"]) -> NonSpare_66:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["B1"]) -> NonSpare_33:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["B2"]) -> NonSpare_34:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["B3"]) -> NonSpare_35:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["R"], Literal["T"], Literal["S1"], Literal["S2"], Literal["S3"], Literal["I1"], Literal["I2"], Literal["Q1LAT"], Literal["Q2LON"], Literal["Q3"], Literal["Q4"], Literal["Q5"], Literal["B1"], Literal["B2"], Literal["B3"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["R"]) -> Optional[NonSpare_67]:
        ...
    @overload
    def get_item(self, key : Literal["T"]) -> Optional[NonSpare_75]:
        ...
    @overload
    def get_item(self, key : Literal["S1"]) -> Optional[NonSpare_68]:
        ...
    @overload
    def get_item(self, key : Literal["S2"]) -> Optional[NonSpare_69]:
        ...
    @overload
    def get_item(self, key : Literal["S3"]) -> Optional[NonSpare_70]:
        ...
    @overload
    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_47]:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[NonSpare_52]:
        ...
    @overload
    def get_item(self, key : Literal["Q1LAT"]) -> Optional[NonSpare_62]:
        ...
    @overload
    def get_item(self, key : Literal["Q2LON"]) -> Optional[NonSpare_63]:
        ...
    @overload
    def get_item(self, key : Literal["Q3"]) -> Optional[NonSpare_64]:
        ...
    @overload
    def get_item(self, key : Literal["Q4"]) -> Optional[NonSpare_65]:
        ...
    @overload
    def get_item(self, key : Literal["Q5"]) -> Optional[NonSpare_66]:
        ...
    @overload
    def get_item(self, key : Literal["B1"]) -> Optional[NonSpare_33]:
        ...
    @overload
    def get_item(self, key : Literal["B2"]) -> Optional[NonSpare_34]:
        ...
    @overload
    def get_item(self, key : Literal["B3"]) -> Optional[NonSpare_35]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["R"], val : NonSpare_67_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["T"], val : NonSpare_75_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["S1"], val : NonSpare_68_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["S2"], val : NonSpare_69_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["S3"], val : NonSpare_70_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["I1"], val : NonSpare_47_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["I2"], val : NonSpare_52_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["Q1LAT"], val : NonSpare_62_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["Q2LON"], val : NonSpare_63_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["Q3"], val : NonSpare_64_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["Q4"], val : NonSpare_65_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["Q5"], val : NonSpare_66_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["B1"], val : NonSpare_33_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["B2"], val : NonSpare_34_Arg) -> "Variation_63":
        ...
    @overload
    def set_item(self, key : Literal["B3"], val : NonSpare_35_Arg) -> "Variation_63":
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["R"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["T"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["S1"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["S2"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["S3"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["I1"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["I2"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["Q1LAT"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["Q2LON"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["Q3"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["Q4"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["Q5"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["B1"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["B2"]) -> "Variation_63":
        ...
    @overload
    def del_item(self, key : Literal["B3"]) -> "Variation_63":
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg : Variation_63_Arg) -> 'Variation_63':
        return cls._create(arg) # type: ignore

RuleVariation_58_Arg: TypeAlias = Variation_63_Arg
class RuleVariation_58(RuleVariationContextFree):
    cv_variation = Variation_63

    @classmethod
    def create(cls, arg : RuleVariation_58_Arg) -> "RuleVariation_58":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_63:
        return self.arg # type: ignore

NonSpare_4_Arg: TypeAlias = RuleVariation_58_Arg
class NonSpare_4(NonSpare):
    cv_name = "020"
    cv_title = "Different Contents"
    cv_rule = RuleVariation_58

    @classmethod
    def create(cls, arg : NonSpare_4_Arg) -> "NonSpare_4":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_58:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_63:
        return self.rule.variation

class UapItem_4(UapItem):
    cv_non_spare = NonSpare_4

Content_1_Arg: TypeAlias = int
class Content_1(ContentTable):
    cv_values = {0: "Air Speed = IAS, LSB (Bit-1) = 2^-14 NM/s", 1: "Air Speed = Mach, LSB (Bit-1) = 0.001"}

RuleContent_1_Arg: TypeAlias = Content_1_Arg
class RuleContent_1(RuleContentContextFree):
    cv_content = Content_1

    @property
    def content(self) -> Content_1:
        return self._get_content() # type: ignore

Variation_1_Arg: TypeAlias = RuleContent_1_Arg
class Variation_1(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 1
    cv_rule = RuleContent_1

    @classmethod
    def create(cls, arg : Variation_1_Arg) -> "Variation_1":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_1:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_1:
        return self.rule.content

RuleVariation_1_Arg: TypeAlias = Variation_1_Arg
class RuleVariation_1(RuleVariationContextFree):
    cv_variation = Variation_1

    @classmethod
    def create(cls, arg : RuleVariation_1_Arg) -> "RuleVariation_1":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_1:
        return self.arg # type: ignore

NonSpare_61_Arg: TypeAlias = RuleVariation_1_Arg
class NonSpare_61(NonSpare):
    cv_name = "IM"
    cv_title = ""
    cv_rule = RuleVariation_1

    @classmethod
    def create(cls, arg : NonSpare_61_Arg) -> "NonSpare_61":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_1:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_1:
        return self.rule.variation

Item_29_Arg: TypeAlias = NonSpare_61_Arg
class Item_29(Item):
    cv_non_spare = NonSpare_61

    @classmethod
    def create(cls, arg : Item_29_Arg) -> "Item_29":
        return cls._create(arg) # type: ignore

Content_23_Arg: TypeAlias = Union[int, float, Tuple[float, Literal["NM/s"]]]
class Content_23(ContentQuantity):
    cv_signedness = Unsigned
    cv_lsb = 6.103515625e-5
    cv_unit = "NM/s"

    def as_quantity(self, cv_unit : Optional[Literal["NM/s"]] = None) -> float:
        return self._as_quantity()

Content_20_Arg: TypeAlias = Union[int, float, Tuple[float, Literal["Mach"]]]
class Content_20(ContentQuantity):
    cv_signedness = Unsigned
    cv_lsb = 1.0e-3
    cv_unit = "Mach"

    def as_quantity(self, cv_unit : Optional[Literal["Mach"]] = None) -> float:
        return self._as_quantity()

RuleContent_23_Arg = Union[
    Tuple[None, Content_0_Arg],
    Tuple[Tuple[Literal[0]], Content_23_Arg],
    Tuple[Tuple[Literal[1]], Content_20_Arg],
]
class RuleContent_23(RuleContentDependent):
    cv_depends_on = [["030", "IM"]]
    cv_default_content = Content_0
    cv_cases = [
        ([0], Content_23),
        ([1], Content_20),
    ]

    @overload
    def content(self, arg : Literal[None]) -> Content_0:
        ...
    @overload
    def content(self, arg : Tuple[Literal[0]]) -> Content_23:
        ...
    @overload
    def content(self, arg : Tuple[Literal[1]]) -> Content_20:
        ...
    def content(self, arg : Any) -> Any:
        return self._get_content(arg)

Variation_28_Arg: TypeAlias = RuleContent_23_Arg
class Variation_28(Element):
    cv_bit_offset8 = 1
    cv_bit_size = 15
    cv_rule = RuleContent_23

    @classmethod
    def create(cls, arg : Variation_28_Arg) -> "Variation_28":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_23:
        return self._get_rule() # type: ignore

RuleVariation_28_Arg: TypeAlias = Variation_28_Arg
class RuleVariation_28(RuleVariationContextFree):
    cv_variation = Variation_28

    @classmethod
    def create(cls, arg : RuleVariation_28_Arg) -> "RuleVariation_28":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_28:
        return self.arg # type: ignore

NonSpare_60_Arg: TypeAlias = RuleVariation_28_Arg
class NonSpare_60(NonSpare):
    cv_name = "IAS"
    cv_title = ""
    cv_rule = RuleVariation_28

    @classmethod
    def create(cls, arg : NonSpare_60_Arg) -> "NonSpare_60":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_28:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_28:
        return self.rule.variation

Item_28_Arg: TypeAlias = NonSpare_60_Arg
class Item_28(Item):
    cv_non_spare = NonSpare_60

    @classmethod
    def create(cls, arg : Item_28_Arg) -> "Item_28":
        return cls._create(arg) # type: ignore

Variation_42_Arg_Group = Tuple[Union[RuleVariation_1_Arg, Tuple[Literal["IM"], RuleVariation_1_Arg]], Union[RuleVariation_28_Arg, Tuple[Literal["IAS"], RuleVariation_28_Arg]]]
Variation_42_Arg: TypeAlias = Union[int, Variation_42_Arg_Group]
class Variation_42(Group):
    cv_bit_offset8 = 0
    cv_bit_size = 16
    cv_items_list = [(Item_29, 1), (Item_28, 15)]
    cv_items_dict = {"IM": RuleVariation_1, "IAS": RuleVariation_28}

    @overload
    @classmethod
    def spec(cls, key : Literal["IM"]) -> RuleVariation_1:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["IAS"]) -> RuleVariation_28:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["IM"], Literal["IAS"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["IM"]) -> RuleVariation_1:
        ...
    @overload
    def get_item(self, key : Literal["IAS"]) -> RuleVariation_28:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg : Variation_42_Arg) -> 'Variation_42':
        return cls._create(arg) # type: ignore

RuleVariation_38_Arg: TypeAlias = Variation_42_Arg
class RuleVariation_38(RuleVariationContextFree):
    cv_variation = Variation_42

    @classmethod
    def create(cls, arg : RuleVariation_38_Arg) -> "RuleVariation_38":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_42:
        return self.arg # type: ignore

NonSpare_6_Arg: TypeAlias = RuleVariation_38_Arg
class NonSpare_6(NonSpare):
    cv_name = "030"
    cv_title = "Simple Dependent Item"
    cv_rule = RuleVariation_38

    @classmethod
    def create(cls, arg : NonSpare_6_Arg) -> "NonSpare_6":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_38:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_42:
        return self.rule.variation

class UapItem_6(UapItem):
    cv_non_spare = NonSpare_6

Content_19_Arg: TypeAlias = Union[int, float, Tuple[float, Literal["unit1"]]]
class Content_19(ContentQuantity):
    cv_signedness = Unsigned
    cv_lsb = 0.5
    cv_unit = "unit1"

    def as_quantity(self, cv_unit : Optional[Literal["unit1"]] = None) -> float:
        return self._as_quantity()

Content_21_Arg: TypeAlias = Union[int, float, Tuple[float, Literal["unit2"]]]
class Content_21(ContentQuantity):
    cv_signedness = Unsigned
    cv_lsb = 0.25
    cv_unit = "unit2"

    def as_quantity(self, cv_unit : Optional[Literal["unit2"]] = None) -> float:
        return self._as_quantity()

Content_22_Arg: TypeAlias = Union[int, float, Tuple[float, Literal["unit3"]]]
class Content_22(ContentQuantity):
    cv_signedness = Unsigned
    cv_lsb = 0.125
    cv_unit = "unit3"

    def as_quantity(self, cv_unit : Optional[Literal["unit3"]] = None) -> float:
        return self._as_quantity()

RuleContent_22_Arg = Union[
    Tuple[None, Content_0_Arg],
    Tuple[Tuple[Literal[1], Literal[1]], Content_19_Arg],
    Tuple[Tuple[Literal[1], Literal[2]], Content_21_Arg],
    Tuple[Tuple[Literal[2], Literal[1]], Content_22_Arg],
]
class RuleContent_22(RuleContentDependent):
    cv_depends_on = [["010", "SAC"], ["010", "SIC"]]
    cv_default_content = Content_0
    cv_cases = [
        ([1, 1], Content_19),
        ([1, 2], Content_21),
        ([2, 1], Content_22),
    ]

    @overload
    def content(self, arg : Literal[None]) -> Content_0:
        ...
    @overload
    def content(self, arg : Tuple[Literal[1], Literal[1]]) -> Content_19:
        ...
    @overload
    def content(self, arg : Tuple[Literal[1], Literal[2]]) -> Content_21:
        ...
    @overload
    def content(self, arg : Tuple[Literal[2], Literal[1]]) -> Content_22:
        ...
    def content(self, arg : Any) -> Any:
        return self._get_content(arg)

Variation_14_Arg: TypeAlias = RuleContent_22_Arg
class Variation_14(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_22

    @classmethod
    def create(cls, arg : Variation_14_Arg) -> "Variation_14":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_22:
        return self._get_rule() # type: ignore

RuleVariation_14_Arg: TypeAlias = Variation_14_Arg
class RuleVariation_14(RuleVariationContextFree):
    cv_variation = Variation_14

    @classmethod
    def create(cls, arg : RuleVariation_14_Arg) -> "RuleVariation_14":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_14:
        return self.arg # type: ignore

NonSpare_7_Arg: TypeAlias = RuleVariation_14_Arg
class NonSpare_7(NonSpare):
    cv_name = "031"
    cv_title = "Double Dependent Item"
    cv_rule = RuleVariation_14

    @classmethod
    def create(cls, arg : NonSpare_7_Arg) -> "NonSpare_7":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_14:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_14:
        return self.rule.variation

class UapItem_7(UapItem):
    cv_non_spare = NonSpare_7

NonSpare_44_Arg: TypeAlias = RuleVariation_7_Arg
class NonSpare_44(NonSpare):
    cv_name = "I1"
    cv_title = ""
    cv_rule = RuleVariation_7

    @classmethod
    def create(cls, arg : NonSpare_44_Arg) -> "NonSpare_44":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

Variation_4_Arg: TypeAlias = RuleContent_0_Arg
class Variation_4(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 4
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_4_Arg) -> "Variation_4":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_4_Arg: TypeAlias = Variation_4_Arg
class RuleVariation_4(RuleVariationContextFree):
    cv_variation = Variation_4

    @classmethod
    def create(cls, arg : RuleVariation_4_Arg) -> "RuleVariation_4":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_4:
        return self.arg # type: ignore

NonSpare_76_Arg: TypeAlias = RuleVariation_4_Arg
class NonSpare_76(NonSpare):
    cv_name = "TID"
    cv_title = "Identification of Conflict Categories Definition Table"
    cv_rule = RuleVariation_4

    @classmethod
    def create(cls, arg : NonSpare_76_Arg) -> "NonSpare_76":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_4:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_4:
        return self.rule.variation

Item_34_Arg: TypeAlias = NonSpare_76_Arg
class Item_34(Item):
    cv_non_spare = NonSpare_76

    @classmethod
    def create(cls, arg : Item_34_Arg) -> "Item_34":
        return cls._create(arg) # type: ignore

Variation_33_Arg: TypeAlias = RuleContent_0_Arg
class Variation_33(Element):
    cv_bit_offset8 = 4
    cv_bit_size = 3
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_33_Arg) -> "Variation_33":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

Content_6_Arg: TypeAlias = int
class Content_6(ContentTable):
    cv_values = {0: "Test0", 1: "Test1", 2: "Test2"}

RuleContent_6_Arg: TypeAlias = Content_6_Arg
class RuleContent_6(RuleContentContextFree):
    cv_content = Content_6

    @property
    def content(self) -> Content_6:
        return self._get_content() # type: ignore

Variation_34_Arg: TypeAlias = RuleContent_6_Arg
class Variation_34(Element):
    cv_bit_offset8 = 4
    cv_bit_size = 3
    cv_rule = RuleContent_6

    @classmethod
    def create(cls, arg : Variation_34_Arg) -> "Variation_34":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_6:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_6:
        return self.rule.content

Content_8_Arg: TypeAlias = int
class Content_8(ContentTable):
    cv_values = {3: "Test3", 4: "Test4"}

RuleContent_8_Arg: TypeAlias = Content_8_Arg
class RuleContent_8(RuleContentContextFree):
    cv_content = Content_8

    @property
    def content(self) -> Content_8:
        return self._get_content() # type: ignore

Variation_35_Arg: TypeAlias = RuleContent_8_Arg
class Variation_35(Element):
    cv_bit_offset8 = 4
    cv_bit_size = 3
    cv_rule = RuleContent_8

    @classmethod
    def create(cls, arg : Variation_35_Arg) -> "Variation_35":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_8:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_8:
        return self.rule.content

Content_5_Arg: TypeAlias = int
class Content_5(ContentTable):
    cv_values = {0: "Test0", 1: "Test1"}

RuleContent_5_Arg: TypeAlias = Content_5_Arg
class RuleContent_5(RuleContentContextFree):
    cv_content = Content_5

    @property
    def content(self) -> Content_5:
        return self._get_content() # type: ignore

Variation_32_Arg: TypeAlias = RuleContent_5_Arg
class Variation_32(Element):
    cv_bit_offset8 = 4
    cv_bit_size = 1
    cv_rule = RuleContent_5

    @classmethod
    def create(cls, arg : Variation_32_Arg) -> "Variation_32":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_5:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_5:
        return self.rule.content

RuleVariation_32_Arg: TypeAlias = Variation_32_Arg
class RuleVariation_32(RuleVariationContextFree):
    cv_variation = Variation_32

    @classmethod
    def create(cls, arg : RuleVariation_32_Arg) -> "RuleVariation_32":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_32:
        return self.arg # type: ignore

NonSpare_46_Arg: TypeAlias = RuleVariation_32_Arg
class NonSpare_46(NonSpare):
    cv_name = "I1"
    cv_title = ""
    cv_rule = RuleVariation_32

    @classmethod
    def create(cls, arg : NonSpare_46_Arg) -> "NonSpare_46":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_32:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_32:
        return self.rule.variation

Item_17_Arg: TypeAlias = NonSpare_46_Arg
class Item_17(Item):
    cv_non_spare = NonSpare_46

    @classmethod
    def create(cls, arg : Item_17_Arg) -> "Item_17":
        return cls._create(arg) # type: ignore

Item_4_Arg: TypeAlias = int
class Item_4(Spare):
    cv_bit_offset8 = 5
    cv_bit_size = 2

Variation_48_Arg_Group = Tuple[Union[RuleVariation_32_Arg, Tuple[Literal["I1"], RuleVariation_32_Arg]], int]
Variation_48_Arg: TypeAlias = Union[int, Variation_48_Arg_Group]
class Variation_48(Group):
    cv_bit_offset8 = 4
    cv_bit_size = 3
    cv_items_list = [(Item_17, 1), (Item_4, 2)]
    cv_items_dict = {"I1": RuleVariation_32}

    @classmethod
    def spec(cls, key : Literal["I1"]) -> RuleVariation_32:
        return cls._spec(arg) # type: ignore

    def get_item(self, key : Literal["I1"]) -> RuleVariation_32:
        return self._get_item(key) # type: ignore

    @classmethod
    def create(cls, arg : Variation_48_Arg) -> 'Variation_48':
        return cls._create(arg) # type: ignore

RuleVariation_59_Arg: TypeAlias = Union[
    Variation_33_Arg,
    Variation_34_Arg,
    Variation_35_Arg,
    Variation_48_Arg,
]
class RuleVariation_59(RuleVariationDependent):
    cv_depends_on = [["000"], ["031", "CC", "TID"]]
    cv_default_variation = Variation_33
    cv_cases = [
        ([1, 1], Variation_34),
        ([1, 2], Variation_35),
        ([2, 1], Variation_48),
    ]

    @overload
    @classmethod
    def variation(cls, key : Tuple[Literal[1], Literal[1]]) -> Variation_34:
        ...
    @overload
    @classmethod
    def variation(cls, key : Tuple[Literal[1], Literal[2]]) -> Variation_35:
        ...
    @overload
    @classmethod
    def variation(cls, key : Tuple[Literal[2], Literal[1]]) -> Variation_48:
        ...

    @classmethod
    def variation(cls, key : Any) -> Any:
        return cls._variation(key)

    @classmethod
    def create(cls, arg : RuleVariation_59_Arg) -> "RuleVariation_59":
        return cls._create(arg) # type: ignore

NonSpare_37_Arg: TypeAlias = RuleVariation_59_Arg
class NonSpare_37(NonSpare):
    cv_name = "CP"
    cv_title = "Conflict Properties Class"
    cv_rule = RuleVariation_59

    @classmethod
    def create(cls, arg : NonSpare_37_Arg) -> "NonSpare_37":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_59:
        return self.arg # type: ignore

Item_8_Arg: TypeAlias = NonSpare_37_Arg
class Item_8(Item):
    cv_non_spare = NonSpare_37

    @classmethod
    def create(cls, arg : Item_8_Arg) -> "Item_8":
        return cls._create(arg) # type: ignore

Content_2_Arg: TypeAlias = int
class Content_2(ContentTable):
    cv_values = {0: "LOW", 1: "HIGH"}

RuleContent_2_Arg: TypeAlias = Content_2_Arg
class RuleContent_2(RuleContentContextFree):
    cv_content = Content_2

    @property
    def content(self) -> Content_2:
        return self._get_content() # type: ignore

Variation_37_Arg: TypeAlias = RuleContent_2_Arg
class Variation_37(Element):
    cv_bit_offset8 = 7
    cv_bit_size = 1
    cv_rule = RuleContent_2

    @classmethod
    def create(cls, arg : Variation_37_Arg) -> "Variation_37":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_2:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_2:
        return self.rule.content

RuleVariation_34_Arg: TypeAlias = Variation_37_Arg
class RuleVariation_34(RuleVariationContextFree):
    cv_variation = Variation_37

    @classmethod
    def create(cls, arg : RuleVariation_34_Arg) -> "RuleVariation_34":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_37:
        return self.arg # type: ignore

NonSpare_38_Arg: TypeAlias = RuleVariation_34_Arg
class NonSpare_38(NonSpare):
    cv_name = "CS"
    cv_title = "Conflict Severity"
    cv_rule = RuleVariation_34

    @classmethod
    def create(cls, arg : NonSpare_38_Arg) -> "NonSpare_38":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_34:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_37:
        return self.rule.variation

Item_9_Arg: TypeAlias = NonSpare_38_Arg
class Item_9(Item):
    cv_non_spare = NonSpare_38

    @classmethod
    def create(cls, arg : Item_9_Arg) -> "Item_9":
        return cls._create(arg) # type: ignore

Variation_46_Arg_Group = Tuple[Union[RuleVariation_4_Arg, Tuple[Literal["TID"], RuleVariation_4_Arg]], Union[RuleVariation_59_Arg, Tuple[Literal["CP"], RuleVariation_59_Arg]], Union[RuleVariation_34_Arg, Tuple[Literal["CS"], RuleVariation_34_Arg]]]
Variation_46_Arg: TypeAlias = Union[int, Variation_46_Arg_Group]
class Variation_46(Group):
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_items_list = [(Item_34, 4), (Item_8, 3), (Item_9, 1)]
    cv_items_dict = {"TID": RuleVariation_4, "CP": RuleVariation_59, "CS": RuleVariation_34}

    @overload
    @classmethod
    def spec(cls, key : Literal["TID"]) -> RuleVariation_4:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["CP"]) -> RuleVariation_59:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["CS"]) -> RuleVariation_34:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["TID"], Literal["CP"], Literal["CS"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["TID"]) -> RuleVariation_4:
        ...
    @overload
    def get_item(self, key : Literal["CP"]) -> RuleVariation_59:
        ...
    @overload
    def get_item(self, key : Literal["CS"]) -> RuleVariation_34:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg : Variation_46_Arg) -> 'Variation_46':
        return cls._create(arg) # type: ignore

RuleVariation_42_Arg: TypeAlias = Variation_46_Arg
class RuleVariation_42(RuleVariationContextFree):
    cv_variation = Variation_46

    @classmethod
    def create(cls, arg : RuleVariation_42_Arg) -> "RuleVariation_42":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_46:
        return self.arg # type: ignore

NonSpare_36_Arg: TypeAlias = RuleVariation_42_Arg
class NonSpare_36(NonSpare):
    cv_name = "CC"
    cv_title = "Conflict Classification"
    cv_rule = RuleVariation_42

    @classmethod
    def create(cls, arg : NonSpare_36_Arg) -> "NonSpare_36":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_42:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_46:
        return self.rule.variation

Variation_62_Arg = TypedDict('Variation_62_Arg', {
    "I1": NonSpare_44_Arg,
    "CC": NonSpare_36_Arg,
}, total=False)
class Variation_62(Compound):
    cv_fspec_max_bytes = 1
    cv_items_list = [NonSpare_44, NonSpare_36]
    cv_items_dict = {"I1": NonSpare_44, "CC": NonSpare_36}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> NonSpare_44:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["CC"]) -> NonSpare_36:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["CC"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_44]:
        ...
    @overload
    def get_item(self, key : Literal["CC"]) -> Optional[NonSpare_36]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["I1"], val : NonSpare_44_Arg) -> "Variation_62":
        ...
    @overload
    def set_item(self, key : Literal["CC"], val : NonSpare_36_Arg) -> "Variation_62":
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["I1"]) -> "Variation_62":
        ...
    @overload
    def del_item(self, key : Literal["CC"]) -> "Variation_62":
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg : Variation_62_Arg) -> 'Variation_62':
        return cls._create(arg) # type: ignore

RuleVariation_57_Arg: TypeAlias = Variation_62_Arg
class RuleVariation_57(RuleVariationContextFree):
    cv_variation = Variation_62

    @classmethod
    def create(cls, arg : RuleVariation_57_Arg) -> "RuleVariation_57":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_62:
        return self.arg # type: ignore

NonSpare_10_Arg: TypeAlias = RuleVariation_57_Arg
class NonSpare_10(NonSpare):
    cv_name = "032"
    cv_title = "Nested Dependent Item"
    cv_rule = RuleVariation_57

    @classmethod
    def create(cls, arg : NonSpare_10_Arg) -> "NonSpare_10":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_57:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_62:
        return self.rule.variation

class UapItem_10(UapItem):
    cv_non_spare = NonSpare_10

Variation_6_Arg: TypeAlias = RuleContent_0_Arg
class Variation_6(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 7
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_6_Arg) -> "Variation_6":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_6_Arg: TypeAlias = Variation_6_Arg
class RuleVariation_6(RuleVariationContextFree):
    cv_variation = Variation_6

    @classmethod
    def create(cls, arg : RuleVariation_6_Arg) -> "RuleVariation_6":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_6:
        return self.arg # type: ignore

NonSpare_43_Arg: TypeAlias = RuleVariation_6_Arg
class NonSpare_43(NonSpare):
    cv_name = "I1"
    cv_title = ""
    cv_rule = RuleVariation_6

    @classmethod
    def create(cls, arg : NonSpare_43_Arg) -> "NonSpare_43":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_6:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_6:
        return self.rule.variation

Item_14_Arg: TypeAlias = NonSpare_43_Arg
class Item_14(Item):
    cv_non_spare = NonSpare_43

    @classmethod
    def create(cls, arg : Item_14_Arg) -> "Item_14":
        return cls._create(arg) # type: ignore

Item_6_Arg: TypeAlias = int
class Item_6(Spare):
    cv_bit_offset8 = 7
    cv_bit_size = 2

Variation_27_Arg: TypeAlias = RuleContent_0_Arg
class Variation_27(Element):
    cv_bit_offset8 = 1
    cv_bit_size = 6
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_27_Arg) -> "Variation_27":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_27_Arg: TypeAlias = Variation_27_Arg
class RuleVariation_27(RuleVariationContextFree):
    cv_variation = Variation_27

    @classmethod
    def create(cls, arg : RuleVariation_27_Arg) -> "RuleVariation_27":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_27:
        return self.arg # type: ignore

NonSpare_50_Arg: TypeAlias = RuleVariation_27_Arg
class NonSpare_50(NonSpare):
    cv_name = "I2"
    cv_title = ""
    cv_rule = RuleVariation_27

    @classmethod
    def create(cls, arg : NonSpare_50_Arg) -> "NonSpare_50":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_27:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_27:
        return self.rule.variation

Item_20_Arg: TypeAlias = NonSpare_50_Arg
class Item_20(Item):
    cv_non_spare = NonSpare_50

    @classmethod
    def create(cls, arg : Item_20_Arg) -> "Item_20":
        return cls._create(arg) # type: ignore

Item_7_Arg: TypeAlias = int
class Item_7(Spare):
    cv_bit_offset8 = 7
    cv_bit_size = 9

Variation_40_Arg_Group = Tuple[Union[RuleVariation_6_Arg, Tuple[Literal["I1"], RuleVariation_6_Arg]], int, Union[RuleVariation_27_Arg, Tuple[Literal["I2"], RuleVariation_27_Arg]], int]
Variation_40_Arg: TypeAlias = Union[int, Variation_40_Arg_Group]
class Variation_40(Group):
    cv_bit_offset8 = 0
    cv_bit_size = 24
    cv_items_list = [(Item_14, 7), (Item_6, 2), (Item_20, 6), (Item_7, 9)]
    cv_items_dict = {"I1": RuleVariation_6, "I2": RuleVariation_27}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> RuleVariation_6:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> RuleVariation_27:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> RuleVariation_6:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> RuleVariation_27:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg : Variation_40_Arg) -> 'Variation_40':
        return cls._create(arg) # type: ignore

RuleVariation_37_Arg: TypeAlias = Variation_40_Arg
class RuleVariation_37(RuleVariationContextFree):
    cv_variation = Variation_40

    @classmethod
    def create(cls, arg : RuleVariation_37_Arg) -> "RuleVariation_37":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_40:
        return self.arg # type: ignore

NonSpare_12_Arg: TypeAlias = RuleVariation_37_Arg
class NonSpare_12(NonSpare):
    cv_name = "040"
    cv_title = "Spare Items"
    cv_rule = RuleVariation_37

    @classmethod
    def create(cls, arg : NonSpare_12_Arg) -> "NonSpare_12":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_37:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_40:
        return self.rule.variation

class UapItem_12(UapItem):
    cv_non_spare = NonSpare_12

NonSpare_13_Arg: TypeAlias = RuleVariation_7_Arg
class NonSpare_13(NonSpare):
    cv_name = "051"
    cv_title = "Element"
    cv_rule = RuleVariation_7

    @classmethod
    def create(cls, arg : NonSpare_13_Arg) -> "NonSpare_13":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_13(UapItem):
    cv_non_spare = NonSpare_13

Variation_5_Arg: TypeAlias = RuleContent_0_Arg
class Variation_5(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 6
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_5_Arg) -> "Variation_5":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_5_Arg: TypeAlias = Variation_5_Arg
class RuleVariation_5(RuleVariationContextFree):
    cv_variation = Variation_5

    @classmethod
    def create(cls, arg : RuleVariation_5_Arg) -> "RuleVariation_5":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_5:
        return self.arg # type: ignore

NonSpare_42_Arg: TypeAlias = RuleVariation_5_Arg
class NonSpare_42(NonSpare):
    cv_name = "I1"
    cv_title = ""
    cv_rule = RuleVariation_5

    @classmethod
    def create(cls, arg : NonSpare_42_Arg) -> "NonSpare_42":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_5:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_5:
        return self.rule.variation

Item_13_Arg: TypeAlias = NonSpare_42_Arg
class Item_13(Item):
    cv_non_spare = NonSpare_42

    @classmethod
    def create(cls, arg : Item_13_Arg) -> "Item_13":
        return cls._create(arg) # type: ignore

Item_5_Arg: TypeAlias = int
class Item_5(Spare):
    cv_bit_offset8 = 6
    cv_bit_size = 2

NonSpare_49_Arg: TypeAlias = RuleVariation_7_Arg
class NonSpare_49(NonSpare):
    cv_name = "I2"
    cv_title = ""
    cv_rule = RuleVariation_7

    @classmethod
    def create(cls, arg : NonSpare_49_Arg) -> "NonSpare_49":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

Item_19_Arg: TypeAlias = NonSpare_49_Arg
class Item_19(Item):
    cv_non_spare = NonSpare_49

    @classmethod
    def create(cls, arg : Item_19_Arg) -> "Item_19":
        return cls._create(arg) # type: ignore

NonSpare_54_Arg: TypeAlias = RuleVariation_4_Arg
class NonSpare_54(NonSpare):
    cv_name = "I3"
    cv_title = ""
    cv_rule = RuleVariation_4

    @classmethod
    def create(cls, arg : NonSpare_54_Arg) -> "NonSpare_54":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_4:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_4:
        return self.rule.variation

Item_23_Arg: TypeAlias = NonSpare_54_Arg
class Item_23(Item):
    cv_non_spare = NonSpare_54

    @classmethod
    def create(cls, arg : Item_23_Arg) -> "Item_23":
        return cls._create(arg) # type: ignore

Item_3_Arg: TypeAlias = int
class Item_3(Spare):
    cv_bit_offset8 = 4
    cv_bit_size = 8

Variation_36_Arg: TypeAlias = RuleContent_0_Arg
class Variation_36(Element):
    cv_bit_offset8 = 4
    cv_bit_size = 4
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_36_Arg) -> "Variation_36":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_33_Arg: TypeAlias = Variation_36_Arg
class RuleVariation_33(RuleVariationContextFree):
    cv_variation = Variation_36

    @classmethod
    def create(cls, arg : RuleVariation_33_Arg) -> "RuleVariation_33":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_36:
        return self.arg # type: ignore

NonSpare_57_Arg: TypeAlias = RuleVariation_33_Arg
class NonSpare_57(NonSpare):
    cv_name = "I4"
    cv_title = ""
    cv_rule = RuleVariation_33

    @classmethod
    def create(cls, arg : NonSpare_57_Arg) -> "NonSpare_57":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_33:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_36:
        return self.rule.variation

Item_25_Arg: TypeAlias = NonSpare_57_Arg
class Item_25(Item):
    cv_non_spare = NonSpare_57

    @classmethod
    def create(cls, arg : Item_25_Arg) -> "Item_25":
        return cls._create(arg) # type: ignore

Variation_39_Arg_Group = Tuple[Union[RuleVariation_5_Arg, Tuple[Literal["I1"], RuleVariation_5_Arg]], int, Union[RuleVariation_7_Arg, Tuple[Literal["I2"], RuleVariation_7_Arg]], Union[RuleVariation_4_Arg, Tuple[Literal["I3"], RuleVariation_4_Arg]], int, Union[RuleVariation_33_Arg, Tuple[Literal["I4"], RuleVariation_33_Arg]]]
Variation_39_Arg: TypeAlias = Union[int, Variation_39_Arg_Group]
class Variation_39(Group):
    cv_bit_offset8 = 0
    cv_bit_size = 32
    cv_items_list = [(Item_13, 6), (Item_5, 2), (Item_19, 8), (Item_23, 4), (Item_3, 8), (Item_25, 4)]
    cv_items_dict = {"I1": RuleVariation_5, "I2": RuleVariation_7, "I3": RuleVariation_4, "I4": RuleVariation_33}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> RuleVariation_5:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> RuleVariation_7:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I3"]) -> RuleVariation_4:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I4"]) -> RuleVariation_33:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"], Literal["I3"], Literal["I4"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> RuleVariation_5:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> RuleVariation_7:
        ...
    @overload
    def get_item(self, key : Literal["I3"]) -> RuleVariation_4:
        ...
    @overload
    def get_item(self, key : Literal["I4"]) -> RuleVariation_33:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg : Variation_39_Arg) -> 'Variation_39':
        return cls._create(arg) # type: ignore

RuleVariation_36_Arg: TypeAlias = Variation_39_Arg
class RuleVariation_36(RuleVariationContextFree):
    cv_variation = Variation_39

    @classmethod
    def create(cls, arg : RuleVariation_36_Arg) -> "RuleVariation_36":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_39:
        return self.arg # type: ignore

NonSpare_14_Arg: TypeAlias = RuleVariation_36_Arg
class NonSpare_14(NonSpare):
    cv_name = "052"
    cv_title = "Group"
    cv_rule = RuleVariation_36

    @classmethod
    def create(cls, arg : NonSpare_14_Arg) -> "NonSpare_14":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_36:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_39:
        return self.rule.variation

class UapItem_14(UapItem):
    cv_non_spare = NonSpare_14

Variation_0_Arg: TypeAlias = RuleContent_0_Arg
class Variation_0(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 1
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_0_Arg) -> "Variation_0":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_0_Arg: TypeAlias = Variation_0_Arg
class RuleVariation_0(RuleVariationContextFree):
    cv_variation = Variation_0

    @classmethod
    def create(cls, arg : RuleVariation_0_Arg) -> "RuleVariation_0":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_0:
        return self.arg # type: ignore

Variation_3_Arg: TypeAlias = RuleContent_0_Arg
class Variation_3(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 2
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_3_Arg) -> "Variation_3":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_3_Arg: TypeAlias = Variation_3_Arg
class RuleVariation_3(RuleVariationContextFree):
    cv_variation = Variation_3

    @classmethod
    def create(cls, arg : RuleVariation_3_Arg) -> "RuleVariation_3":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_3:
        return self.arg # type: ignore

Variation_31_Arg: TypeAlias = RuleContent_0_Arg
class Variation_31(Element):
    cv_bit_offset8 = 3
    cv_bit_size = 4
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_31_Arg) -> "Variation_31":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_31_Arg: TypeAlias = Variation_31_Arg
class RuleVariation_31(RuleVariationContextFree):
    cv_variation = Variation_31

    @classmethod
    def create(cls, arg : RuleVariation_31_Arg) -> "RuleVariation_31":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_31:
        return self.arg # type: ignore

NonSpare_41_Arg: TypeAlias = RuleVariation_0_Arg
class NonSpare_41(NonSpare):
    cv_name = "I1"
    cv_title = ""
    cv_rule = RuleVariation_0

    @classmethod
    def create(cls, arg : NonSpare_41_Arg) -> "NonSpare_41":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_0:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_0:
        return self.rule.variation

Item_12_Arg: TypeAlias = NonSpare_41_Arg
class Item_12(Item):
    cv_non_spare = NonSpare_41

    @classmethod
    def create(cls, arg : Item_12_Arg) -> "Item_12":
        return cls._create(arg) # type: ignore

NonSpare_53_Arg: TypeAlias = RuleVariation_3_Arg
class NonSpare_53(NonSpare):
    cv_name = "I3"
    cv_title = ""
    cv_rule = RuleVariation_3

    @classmethod
    def create(cls, arg : NonSpare_53_Arg) -> "NonSpare_53":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_3:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_3:
        return self.rule.variation

Item_22_Arg: TypeAlias = NonSpare_53_Arg
class Item_22(Item):
    cv_non_spare = NonSpare_53

    @classmethod
    def create(cls, arg : Item_22_Arg) -> "Item_22":
        return cls._create(arg) # type: ignore

Item_0_Arg: TypeAlias = int
class Item_0(Spare):
    cv_bit_offset8 = 2
    cv_bit_size = 1

NonSpare_56_Arg: TypeAlias = RuleVariation_31_Arg
class NonSpare_56(NonSpare):
    cv_name = "I4"
    cv_title = ""
    cv_rule = RuleVariation_31

    @classmethod
    def create(cls, arg : NonSpare_56_Arg) -> "NonSpare_56":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_31:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_31:
        return self.rule.variation

Item_24_Arg: TypeAlias = NonSpare_56_Arg
class Item_24(Item):
    cv_non_spare = NonSpare_56

    @classmethod
    def create(cls, arg : Item_24_Arg) -> "Item_24":
        return cls._create(arg) # type: ignore

NonSpare_58_Arg: TypeAlias = RuleVariation_6_Arg
class NonSpare_58(NonSpare):
    cv_name = "I5"
    cv_title = ""
    cv_rule = RuleVariation_6

    @classmethod
    def create(cls, arg : NonSpare_58_Arg) -> "NonSpare_58":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_6:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_6:
        return self.rule.variation

Item_26_Arg: TypeAlias = NonSpare_58_Arg
class Item_26(Item):
    cv_non_spare = NonSpare_58

    @classmethod
    def create(cls, arg : Item_26_Arg) -> "Item_26":
        return cls._create(arg) # type: ignore

Variation_49_Arg_Group_1: TypeAlias = Union[int, Tuple[Union[RuleVariation_0_Arg, Tuple[Literal["I1"], RuleVariation_0_Arg]], Union[RuleVariation_27_Arg, Tuple[Literal["I2"], RuleVariation_27_Arg]], None]]
Variation_49_Arg_Group_2: TypeAlias = Union[int, Tuple[Union[RuleVariation_3_Arg, Tuple[Literal["I3"], RuleVariation_3_Arg]], int, Union[RuleVariation_31_Arg, Tuple[Literal["I4"], RuleVariation_31_Arg]], None]]
Variation_49_Arg_Group_3: TypeAlias = Union[int, Tuple[Union[RuleVariation_6_Arg, Tuple[Literal["I5"], RuleVariation_6_Arg]], None]]
Variation_49_Arg: TypeAlias = Union[
    Tuple[Variation_49_Arg_Group_1],
    Tuple[Variation_49_Arg_Group_1, Variation_49_Arg_Group_2],
    Tuple[Variation_49_Arg_Group_1, Variation_49_Arg_Group_2, Variation_49_Arg_Group_3],
]
class Variation_49(Extended):
    cv_items_list = [[(Item_12, 1), (Item_20, 6), None], [(Item_22, 2), (Item_0, 1), (Item_24, 4), None], [(Item_26, 7), None]]

    @classmethod
    def create(cls, arg : Variation_49_Arg) -> 'Variation_49':
        return cls._create(arg) # type: ignore

    @overload
    def get_item(self, key : Literal["I1"]) -> RuleVariation_0:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> RuleVariation_27:
        ...
    @overload
    def get_item(self, key : Literal["I3"]) -> Optional[RuleVariation_3]:
        ...
    @overload
    def get_item(self, key : Literal["I4"]) -> Optional[RuleVariation_31]:
        ...
    @overload
    def get_item(self, key : Literal["I5"]) -> Optional[RuleVariation_6]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

RuleVariation_44_Arg: TypeAlias = Variation_49_Arg
class RuleVariation_44(RuleVariationContextFree):
    cv_variation = Variation_49

    @classmethod
    def create(cls, arg : RuleVariation_44_Arg) -> "RuleVariation_44":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_49:
        return self.arg # type: ignore

NonSpare_15_Arg: TypeAlias = RuleVariation_44_Arg
class NonSpare_15(NonSpare):
    cv_name = "053"
    cv_title = "Extended With Trailing Fx"
    cv_rule = RuleVariation_44

    @classmethod
    def create(cls, arg : NonSpare_15_Arg) -> "NonSpare_15":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_44:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_49:
        return self.rule.variation

class UapItem_15(UapItem):
    cv_non_spare = NonSpare_15

NonSpare_59_Arg: TypeAlias = RuleVariation_7_Arg
class NonSpare_59(NonSpare):
    cv_name = "I5"
    cv_title = ""
    cv_rule = RuleVariation_7

    @classmethod
    def create(cls, arg : NonSpare_59_Arg) -> "NonSpare_59":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

Item_27_Arg: TypeAlias = NonSpare_59_Arg
class Item_27(Item):
    cv_non_spare = NonSpare_59

    @classmethod
    def create(cls, arg : Item_27_Arg) -> "Item_27":
        return cls._create(arg) # type: ignore

Variation_50_Arg_Group_1: TypeAlias = Union[int, Tuple[Union[RuleVariation_0_Arg, Tuple[Literal["I1"], RuleVariation_0_Arg]], Union[RuleVariation_27_Arg, Tuple[Literal["I2"], RuleVariation_27_Arg]], None]]
Variation_50_Arg_Group_2: TypeAlias = Union[int, Tuple[Union[RuleVariation_3_Arg, Tuple[Literal["I3"], RuleVariation_3_Arg]], int, Union[RuleVariation_31_Arg, Tuple[Literal["I4"], RuleVariation_31_Arg]], None]]
Variation_50_Arg_Group_3: TypeAlias = Union[int, Tuple[Union[RuleVariation_7_Arg, Tuple[Literal["I5"], RuleVariation_7_Arg]]]]
Variation_50_Arg: TypeAlias = Union[
    Tuple[Variation_50_Arg_Group_1],
    Tuple[Variation_50_Arg_Group_1, Variation_50_Arg_Group_2],
    Tuple[Variation_50_Arg_Group_1, Variation_50_Arg_Group_2, Variation_50_Arg_Group_3],
]
class Variation_50(Extended):
    cv_items_list = [[(Item_12, 1), (Item_20, 6), None], [(Item_22, 2), (Item_0, 1), (Item_24, 4), None], [(Item_27, 8)]]

    @classmethod
    def create(cls, arg : Variation_50_Arg) -> 'Variation_50':
        return cls._create(arg) # type: ignore

    @overload
    def get_item(self, key : Literal["I1"]) -> RuleVariation_0:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> RuleVariation_27:
        ...
    @overload
    def get_item(self, key : Literal["I3"]) -> Optional[RuleVariation_3]:
        ...
    @overload
    def get_item(self, key : Literal["I4"]) -> Optional[RuleVariation_31]:
        ...
    @overload
    def get_item(self, key : Literal["I5"]) -> Optional[RuleVariation_7]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

RuleVariation_45_Arg: TypeAlias = Variation_50_Arg
class RuleVariation_45(RuleVariationContextFree):
    cv_variation = Variation_50

    @classmethod
    def create(cls, arg : RuleVariation_45_Arg) -> "RuleVariation_45":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_50:
        return self.arg # type: ignore

NonSpare_16_Arg: TypeAlias = RuleVariation_45_Arg
class NonSpare_16(NonSpare):
    cv_name = "054"
    cv_title = "Extended Without Trailing Fx"
    cv_rule = RuleVariation_45

    @classmethod
    def create(cls, arg : NonSpare_16_Arg) -> "NonSpare_16":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_45:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_50:
        return self.rule.variation

class UapItem_16(UapItem):
    cv_non_spare = NonSpare_16

Variation_53_Arg: TypeAlias = List[Variation_7_Arg]
class Variation_53(Repetitive):
    cv_rep_bytes = 1
    cv_variation = Variation_7

    @classmethod
    def create(cls, arg : Variation_53_Arg) -> 'Variation_53':
        return cls._create(arg) # type: ignore

    def get_list(self) -> List[Variation_7]:
        return self._get_list() # type: ignore

RuleVariation_48_Arg: TypeAlias = Variation_53_Arg
class RuleVariation_48(RuleVariationContextFree):
    cv_variation = Variation_53

    @classmethod
    def create(cls, arg : RuleVariation_48_Arg) -> "RuleVariation_48":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_53:
        return self.arg # type: ignore

NonSpare_17_Arg: TypeAlias = RuleVariation_48_Arg
class NonSpare_17(NonSpare):
    cv_name = "061"
    cv_title = "Repetitive Regular"
    cv_rule = RuleVariation_48

    @classmethod
    def create(cls, arg : NonSpare_17_Arg) -> "NonSpare_17":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_48:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_53:
        return self.rule.variation

class UapItem_17(UapItem):
    cv_non_spare = NonSpare_17

Item_15_Arg: TypeAlias = NonSpare_44_Arg
class Item_15(Item):
    cv_non_spare = NonSpare_44

    @classmethod
    def create(cls, arg : Item_15_Arg) -> "Item_15":
        return cls._create(arg) # type: ignore

Variation_41_Arg_Group = Tuple[Union[RuleVariation_7_Arg, Tuple[Literal["I1"], RuleVariation_7_Arg]], Union[RuleVariation_7_Arg, Tuple[Literal["I2"], RuleVariation_7_Arg]]]
Variation_41_Arg: TypeAlias = Union[int, Variation_41_Arg_Group]
class Variation_41(Group):
    cv_bit_offset8 = 0
    cv_bit_size = 16
    cv_items_list = [(Item_15, 8), (Item_19, 8)]
    cv_items_dict = {"I1": RuleVariation_7, "I2": RuleVariation_7}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> RuleVariation_7:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> RuleVariation_7:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> RuleVariation_7:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> RuleVariation_7:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg : Variation_41_Arg) -> 'Variation_41':
        return cls._create(arg) # type: ignore

Variation_54_Arg: TypeAlias = List[Variation_41_Arg]
class Variation_54(Repetitive):
    cv_rep_bytes = 1
    cv_variation = Variation_41

    @classmethod
    def create(cls, arg : Variation_54_Arg) -> 'Variation_54':
        return cls._create(arg) # type: ignore

    def get_list(self) -> List[Variation_41]:
        return self._get_list() # type: ignore

RuleVariation_49_Arg: TypeAlias = Variation_54_Arg
class RuleVariation_49(RuleVariationContextFree):
    cv_variation = Variation_54

    @classmethod
    def create(cls, arg : RuleVariation_49_Arg) -> "RuleVariation_49":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_54:
        return self.arg # type: ignore

NonSpare_18_Arg: TypeAlias = RuleVariation_49_Arg
class NonSpare_18(NonSpare):
    cv_name = "062"
    cv_title = "Repetitive With Group"
    cv_rule = RuleVariation_49

    @classmethod
    def create(cls, arg : NonSpare_18_Arg) -> "NonSpare_18":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_49:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_54:
        return self.rule.variation

class UapItem_18(UapItem):
    cv_non_spare = NonSpare_18

Variation_55_Arg: TypeAlias = List[Variation_6_Arg]
class Variation_55(Repetitive):
    cv_rep_bytes = None
    cv_variation = Variation_6

    @classmethod
    def create(cls, arg : Variation_55_Arg) -> 'Variation_55':
        return cls._create(arg) # type: ignore

    def get_list(self) -> List[Variation_6]:
        return self._get_list() # type: ignore

RuleVariation_50_Arg: TypeAlias = Variation_55_Arg
class RuleVariation_50(RuleVariationContextFree):
    cv_variation = Variation_55

    @classmethod
    def create(cls, arg : RuleVariation_50_Arg) -> "RuleVariation_50":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_55:
        return self.arg # type: ignore

NonSpare_19_Arg: TypeAlias = RuleVariation_50_Arg
class NonSpare_19(NonSpare):
    cv_name = "063"
    cv_title = "Repetitive Fx"
    cv_rule = RuleVariation_50

    @classmethod
    def create(cls, arg : NonSpare_19_Arg) -> "NonSpare_19":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_50:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_55:
        return self.rule.variation

class UapItem_19(UapItem):
    cv_non_spare = NonSpare_19

Variation_56_Arg: TypeAlias = bytes
class Variation_56(Explicit):
    cv_explicit_type = None

    @classmethod
    def create(cls, arg : Variation_56_Arg) -> 'Variation_56':
        return cls._create(arg) # type: ignore

RuleVariation_51_Arg: TypeAlias = Variation_56_Arg
class RuleVariation_51(RuleVariationContextFree):
    cv_variation = Variation_56

    @classmethod
    def create(cls, arg : RuleVariation_51_Arg) -> "RuleVariation_51":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_56:
        return self.arg # type: ignore

NonSpare_20_Arg: TypeAlias = RuleVariation_51_Arg
class NonSpare_20(NonSpare):
    cv_name = "071"
    cv_title = "Explicit None"
    cv_rule = RuleVariation_51

    @classmethod
    def create(cls, arg : NonSpare_20_Arg) -> "NonSpare_20":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_51:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_56:
        return self.rule.variation

class UapItem_20(UapItem):
    cv_non_spare = NonSpare_20

Variation_57_Arg: TypeAlias = bytes
class Variation_57(Explicit):
    cv_explicit_type = ReservedExpansion

    @classmethod
    def create(cls, arg : Variation_57_Arg) -> 'Variation_57':
        return cls._create(arg) # type: ignore

RuleVariation_52_Arg: TypeAlias = Variation_57_Arg
class RuleVariation_52(RuleVariationContextFree):
    cv_variation = Variation_57

    @classmethod
    def create(cls, arg : RuleVariation_52_Arg) -> "RuleVariation_52":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_57:
        return self.arg # type: ignore

NonSpare_21_Arg: TypeAlias = RuleVariation_52_Arg
class NonSpare_21(NonSpare):
    cv_name = "072"
    cv_title = "Explicit RE"
    cv_rule = RuleVariation_52

    @classmethod
    def create(cls, arg : NonSpare_21_Arg) -> "NonSpare_21":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_52:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_57:
        return self.rule.variation

class UapItem_21(UapItem):
    cv_non_spare = NonSpare_21

Variation_58_Arg: TypeAlias = bytes
class Variation_58(Explicit):
    cv_explicit_type = SpecialPurpose

    @classmethod
    def create(cls, arg : Variation_58_Arg) -> 'Variation_58':
        return cls._create(arg) # type: ignore

RuleVariation_53_Arg: TypeAlias = Variation_58_Arg
class RuleVariation_53(RuleVariationContextFree):
    cv_variation = Variation_58

    @classmethod
    def create(cls, arg : RuleVariation_53_Arg) -> "RuleVariation_53":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_58:
        return self.arg # type: ignore

NonSpare_22_Arg: TypeAlias = RuleVariation_53_Arg
class NonSpare_22(NonSpare):
    cv_name = "073"
    cv_title = "Explicit SP"
    cv_rule = RuleVariation_53

    @classmethod
    def create(cls, arg : NonSpare_22_Arg) -> "NonSpare_22":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_53:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_58:
        return self.rule.variation

class UapItem_22(UapItem):
    cv_non_spare = NonSpare_22

class UapItem_33(UapItemSpare):
    pass

class UapItem_34(UapItemRFS):
    pass

Variation_59_Arg = TypedDict('Variation_59_Arg', {
    "I1": NonSpare_44_Arg,
}, total=False)
class Variation_59(Compound):
    cv_fspec_max_bytes = 1
    cv_items_list = [NonSpare_44]
    cv_items_dict = {"I1": NonSpare_44}

    @classmethod
    def spec(cls, key : Literal["I1"]) -> NonSpare_44:
        return cls._spec(arg) # type: ignore

    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_44]:
        return self._get_item(key) # type: ignore

    def set_item(self, key : Literal["I1"], val : NonSpare_44_Arg) -> 'Variation_59':
        return self._set_item(key, val) # type: ignore

    def del_item(self, key : Literal["I1"]) -> 'Variation_59':
        return self._del_item(key) # type: ignore

    @classmethod
    def create(cls, arg : Variation_59_Arg) -> 'Variation_59':
        return cls._create(arg) # type: ignore

RuleVariation_54_Arg: TypeAlias = Variation_59_Arg
class RuleVariation_54(RuleVariationContextFree):
    cv_variation = Variation_59

    @classmethod
    def create(cls, arg : RuleVariation_54_Arg) -> "RuleVariation_54":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_59:
        return self.arg # type: ignore

NonSpare_23_Arg: TypeAlias = RuleVariation_54_Arg
class NonSpare_23(NonSpare):
    cv_name = "091"
    cv_title = "Compound With One Element"
    cv_rule = RuleVariation_54

    @classmethod
    def create(cls, arg : NonSpare_23_Arg) -> "NonSpare_23":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_54:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_59:
        return self.rule.variation

class UapItem_23(UapItem):
    cv_non_spare = NonSpare_23

Variation_60_Arg = TypedDict('Variation_60_Arg', {
    "I1": NonSpare_44_Arg,
    "I2": NonSpare_49_Arg,
}, total=False)
class Variation_60(Compound):
    cv_fspec_max_bytes = 1
    cv_items_list = [NonSpare_44, None, NonSpare_49]
    cv_items_dict = {"I1": NonSpare_44, "I2": NonSpare_49}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> NonSpare_44:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> NonSpare_49:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_44]:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[NonSpare_49]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["I1"], val : NonSpare_44_Arg) -> "Variation_60":
        ...
    @overload
    def set_item(self, key : Literal["I2"], val : NonSpare_49_Arg) -> "Variation_60":
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["I1"]) -> "Variation_60":
        ...
    @overload
    def del_item(self, key : Literal["I2"]) -> "Variation_60":
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg : Variation_60_Arg) -> 'Variation_60':
        return cls._create(arg) # type: ignore

RuleVariation_55_Arg: TypeAlias = Variation_60_Arg
class RuleVariation_55(RuleVariationContextFree):
    cv_variation = Variation_60

    @classmethod
    def create(cls, arg : RuleVariation_55_Arg) -> "RuleVariation_55":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_60:
        return self.arg # type: ignore

NonSpare_24_Arg: TypeAlias = RuleVariation_55_Arg
class NonSpare_24(NonSpare):
    cv_name = "092"
    cv_title = "Compound With Two Elements"
    cv_rule = RuleVariation_55

    @classmethod
    def create(cls, arg : NonSpare_24_Arg) -> "NonSpare_24":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_55:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_60:
        return self.rule.variation

class UapItem_24(UapItem):
    cv_non_spare = NonSpare_24

NonSpare_55_Arg: TypeAlias = RuleVariation_7_Arg
class NonSpare_55(NonSpare):
    cv_name = "I3"
    cv_title = ""
    cv_rule = RuleVariation_7

    @classmethod
    def create(cls, arg : NonSpare_55_Arg) -> "NonSpare_55":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

Variation_61_Arg = TypedDict('Variation_61_Arg', {
    "I1": NonSpare_44_Arg,
    "I2": NonSpare_49_Arg,
    "I3": NonSpare_55_Arg,
}, total=False)
class Variation_61(Compound):
    cv_fspec_max_bytes = 2
    cv_items_list = [NonSpare_44, None, NonSpare_49, None, None, None, None, NonSpare_55]
    cv_items_dict = {"I1": NonSpare_44, "I2": NonSpare_49, "I3": NonSpare_55}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> NonSpare_44:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> NonSpare_49:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I3"]) -> NonSpare_55:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"], Literal["I3"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_44]:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[NonSpare_49]:
        ...
    @overload
    def get_item(self, key : Literal["I3"]) -> Optional[NonSpare_55]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["I1"], val : NonSpare_44_Arg) -> "Variation_61":
        ...
    @overload
    def set_item(self, key : Literal["I2"], val : NonSpare_49_Arg) -> "Variation_61":
        ...
    @overload
    def set_item(self, key : Literal["I3"], val : NonSpare_55_Arg) -> "Variation_61":
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["I1"]) -> "Variation_61":
        ...
    @overload
    def del_item(self, key : Literal["I2"]) -> "Variation_61":
        ...
    @overload
    def del_item(self, key : Literal["I3"]) -> "Variation_61":
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg : Variation_61_Arg) -> 'Variation_61':
        return cls._create(arg) # type: ignore

RuleVariation_56_Arg: TypeAlias = Variation_61_Arg
class RuleVariation_56(RuleVariationContextFree):
    cv_variation = Variation_61

    @classmethod
    def create(cls, arg : RuleVariation_56_Arg) -> "RuleVariation_56":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_61:
        return self.arg # type: ignore

NonSpare_25_Arg: TypeAlias = RuleVariation_56_Arg
class NonSpare_25(NonSpare):
    cv_name = "093"
    cv_title = "Compound With Three Elements"
    cv_rule = RuleVariation_56

    @classmethod
    def create(cls, arg : NonSpare_25_Arg) -> "NonSpare_25":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_56:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_61:
        return self.rule.variation

class UapItem_25(UapItem):
    cv_non_spare = NonSpare_25

NonSpare_39_Arg: TypeAlias = RuleVariation_0_Arg
class NonSpare_39(NonSpare):
    cv_name = "EP"
    cv_title = "Element Populated Bit"
    cv_rule = RuleVariation_0

    @classmethod
    def create(cls, arg : NonSpare_39_Arg) -> "NonSpare_39":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_0:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_0:
        return self.rule.variation

Item_10_Arg: TypeAlias = NonSpare_39_Arg
class Item_10(Item):
    cv_non_spare = NonSpare_39

    @classmethod
    def create(cls, arg : Item_10_Arg) -> "Item_10":
        return cls._create(arg) # type: ignore

Variation_26_Arg: TypeAlias = RuleContent_0_Arg
class Variation_26(Element):
    cv_bit_offset8 = 1
    cv_bit_size = 1
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_26_Arg) -> "Variation_26":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_26_Arg: TypeAlias = Variation_26_Arg
class RuleVariation_26(RuleVariationContextFree):
    cv_variation = Variation_26

    @classmethod
    def create(cls, arg : RuleVariation_26_Arg) -> "RuleVariation_26":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_26:
        return self.arg # type: ignore

NonSpare_78_Arg: TypeAlias = RuleVariation_26_Arg
class NonSpare_78(NonSpare):
    cv_name = "VAL"
    cv_title = "Value"
    cv_rule = RuleVariation_26

    @classmethod
    def create(cls, arg : NonSpare_78_Arg) -> "NonSpare_78":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_26:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_26:
        return self.rule.variation

Item_36_Arg: TypeAlias = NonSpare_78_Arg
class Item_36(Item):
    cv_non_spare = NonSpare_78

    @classmethod
    def create(cls, arg : Item_36_Arg) -> "Item_36":
        return cls._create(arg) # type: ignore

Variation_38_Arg_Group = Tuple[Union[RuleVariation_0_Arg, Tuple[Literal["EP"], RuleVariation_0_Arg]], Union[RuleVariation_26_Arg, Tuple[Literal["VAL"], RuleVariation_26_Arg]]]
Variation_38_Arg: TypeAlias = Union[int, Variation_38_Arg_Group]
class Variation_38(Group):
    cv_bit_offset8 = 0
    cv_bit_size = 2
    cv_items_list = [(Item_10, 1), (Item_36, 1)]
    cv_items_dict = {"EP": RuleVariation_0, "VAL": RuleVariation_26}

    @overload
    @classmethod
    def spec(cls, key : Literal["EP"]) -> RuleVariation_0:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["VAL"]) -> RuleVariation_26:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["EP"], Literal["VAL"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["EP"]) -> RuleVariation_0:
        ...
    @overload
    def get_item(self, key : Literal["VAL"]) -> RuleVariation_26:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg : Variation_38_Arg) -> 'Variation_38':
        return cls._create(arg) # type: ignore

RuleVariation_35_Arg: TypeAlias = Variation_38_Arg
class RuleVariation_35(RuleVariationContextFree):
    cv_variation = Variation_38

    @classmethod
    def create(cls, arg : RuleVariation_35_Arg) -> "RuleVariation_35":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_38:
        return self.arg # type: ignore

NonSpare_72_Arg: TypeAlias = RuleVariation_35_Arg
class NonSpare_72(NonSpare):
    cv_name = "SG1"
    cv_title = ""
    cv_rule = RuleVariation_35

    @classmethod
    def create(cls, arg : NonSpare_72_Arg) -> "NonSpare_72":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_35:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_38:
        return self.rule.variation

Item_31_Arg: TypeAlias = NonSpare_72_Arg
class Item_31(Item):
    cv_non_spare = NonSpare_72

    @classmethod
    def create(cls, arg : Item_31_Arg) -> "Item_31":
        return cls._create(arg) # type: ignore

Variation_29_Arg: TypeAlias = RuleContent_0_Arg
class Variation_29(Element):
    cv_bit_offset8 = 2
    cv_bit_size = 1
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_29_Arg) -> "Variation_29":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_29_Arg: TypeAlias = Variation_29_Arg
class RuleVariation_29(RuleVariationContextFree):
    cv_variation = Variation_29

    @classmethod
    def create(cls, arg : RuleVariation_29_Arg) -> "RuleVariation_29":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_29:
        return self.arg # type: ignore

NonSpare_40_Arg: TypeAlias = RuleVariation_29_Arg
class NonSpare_40(NonSpare):
    cv_name = "EP"
    cv_title = "Element Populated Bit"
    cv_rule = RuleVariation_29

    @classmethod
    def create(cls, arg : NonSpare_40_Arg) -> "NonSpare_40":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_29:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_29:
        return self.rule.variation

Item_11_Arg: TypeAlias = NonSpare_40_Arg
class Item_11(Item):
    cv_non_spare = NonSpare_40

    @classmethod
    def create(cls, arg : Item_11_Arg) -> "Item_11":
        return cls._create(arg) # type: ignore

Variation_30_Arg: TypeAlias = RuleContent_0_Arg
class Variation_30(Element):
    cv_bit_offset8 = 3
    cv_bit_size = 1
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_30_Arg) -> "Variation_30":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_30_Arg: TypeAlias = Variation_30_Arg
class RuleVariation_30(RuleVariationContextFree):
    cv_variation = Variation_30

    @classmethod
    def create(cls, arg : RuleVariation_30_Arg) -> "RuleVariation_30":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_30:
        return self.arg # type: ignore

NonSpare_79_Arg: TypeAlias = RuleVariation_30_Arg
class NonSpare_79(NonSpare):
    cv_name = "VAL"
    cv_title = "Value"
    cv_rule = RuleVariation_30

    @classmethod
    def create(cls, arg : NonSpare_79_Arg) -> "NonSpare_79":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_30:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_30:
        return self.rule.variation

Item_37_Arg: TypeAlias = NonSpare_79_Arg
class Item_37(Item):
    cv_non_spare = NonSpare_79

    @classmethod
    def create(cls, arg : Item_37_Arg) -> "Item_37":
        return cls._create(arg) # type: ignore

Variation_47_Arg_Group = Tuple[Union[RuleVariation_29_Arg, Tuple[Literal["EP"], RuleVariation_29_Arg]], Union[RuleVariation_30_Arg, Tuple[Literal["VAL"], RuleVariation_30_Arg]]]
Variation_47_Arg: TypeAlias = Union[int, Variation_47_Arg_Group]
class Variation_47(Group):
    cv_bit_offset8 = 2
    cv_bit_size = 2
    cv_items_list = [(Item_11, 1), (Item_37, 1)]
    cv_items_dict = {"EP": RuleVariation_29, "VAL": RuleVariation_30}

    @overload
    @classmethod
    def spec(cls, key : Literal["EP"]) -> RuleVariation_29:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["VAL"]) -> RuleVariation_30:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["EP"], Literal["VAL"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["EP"]) -> RuleVariation_29:
        ...
    @overload
    def get_item(self, key : Literal["VAL"]) -> RuleVariation_30:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg : Variation_47_Arg) -> 'Variation_47':
        return cls._create(arg) # type: ignore

RuleVariation_43_Arg: TypeAlias = Variation_47_Arg
class RuleVariation_43(RuleVariationContextFree):
    cv_variation = Variation_47

    @classmethod
    def create(cls, arg : RuleVariation_43_Arg) -> "RuleVariation_43":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_47:
        return self.arg # type: ignore

NonSpare_73_Arg: TypeAlias = RuleVariation_43_Arg
class NonSpare_73(NonSpare):
    cv_name = "SG2"
    cv_title = ""
    cv_rule = RuleVariation_43

    @classmethod
    def create(cls, arg : NonSpare_73_Arg) -> "NonSpare_73":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_43:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_47:
        return self.rule.variation

Item_32_Arg: TypeAlias = NonSpare_73_Arg
class Item_32(Item):
    cv_non_spare = NonSpare_73

    @classmethod
    def create(cls, arg : Item_32_Arg) -> "Item_32":
        return cls._create(arg) # type: ignore

Item_2_Arg: TypeAlias = int
class Item_2(Spare):
    cv_bit_offset8 = 4
    cv_bit_size = 4

Variation_45_Arg_Group = Tuple[Union[RuleVariation_35_Arg, Tuple[Literal["SG1"], RuleVariation_35_Arg]], Union[RuleVariation_43_Arg, Tuple[Literal["SG2"], RuleVariation_43_Arg]], int]
Variation_45_Arg: TypeAlias = Union[int, Variation_45_Arg_Group]
class Variation_45(Group):
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_items_list = [(Item_31, 2), (Item_32, 2), (Item_2, 4)]
    cv_items_dict = {"SG1": RuleVariation_35, "SG2": RuleVariation_43}

    @overload
    @classmethod
    def spec(cls, key : Literal["SG1"]) -> RuleVariation_35:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["SG2"]) -> RuleVariation_43:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["SG1"], Literal["SG2"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["SG1"]) -> RuleVariation_35:
        ...
    @overload
    def get_item(self, key : Literal["SG2"]) -> RuleVariation_43:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg : Variation_45_Arg) -> 'Variation_45':
        return cls._create(arg) # type: ignore

RuleVariation_41_Arg: TypeAlias = Variation_45_Arg
class RuleVariation_41(RuleVariationContextFree):
    cv_variation = Variation_45

    @classmethod
    def create(cls, arg : RuleVariation_41_Arg) -> "RuleVariation_41":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_45:
        return self.arg # type: ignore

NonSpare_27_Arg: TypeAlias = RuleVariation_41_Arg
class NonSpare_27(NonSpare):
    cv_name = "101"
    cv_title = "Nested Groups"
    cv_rule = RuleVariation_41

    @classmethod
    def create(cls, arg : NonSpare_27_Arg) -> "NonSpare_27":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_41:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_45:
        return self.rule.variation

class UapItem_27(UapItem):
    cv_non_spare = NonSpare_27

Item_1_Arg: TypeAlias = int
class Item_1(Spare):
    cv_bit_offset8 = 4
    cv_bit_size = 3

Variation_44_Arg_Group = Tuple[Union[RuleVariation_35_Arg, Tuple[Literal["SG1"], RuleVariation_35_Arg]], Union[RuleVariation_43_Arg, Tuple[Literal["SG2"], RuleVariation_43_Arg]], int]
Variation_44_Arg: TypeAlias = Union[int, Variation_44_Arg_Group]
class Variation_44(Group):
    cv_bit_offset8 = 0
    cv_bit_size = 7
    cv_items_list = [(Item_31, 2), (Item_32, 2), (Item_1, 3)]
    cv_items_dict = {"SG1": RuleVariation_35, "SG2": RuleVariation_43}

    @overload
    @classmethod
    def spec(cls, key : Literal["SG1"]) -> RuleVariation_35:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["SG2"]) -> RuleVariation_43:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["SG1"], Literal["SG2"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["SG1"]) -> RuleVariation_35:
        ...
    @overload
    def get_item(self, key : Literal["SG2"]) -> RuleVariation_43:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg : Variation_44_Arg) -> 'Variation_44':
        return cls._create(arg) # type: ignore

RuleVariation_40_Arg: TypeAlias = Variation_44_Arg
class RuleVariation_40(RuleVariationContextFree):
    cv_variation = Variation_44

    @classmethod
    def create(cls, arg : RuleVariation_40_Arg) -> "RuleVariation_40":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_44:
        return self.arg # type: ignore

NonSpare_51_Arg: TypeAlias = RuleVariation_40_Arg
class NonSpare_51(NonSpare):
    cv_name = "I2"
    cv_title = ""
    cv_rule = RuleVariation_40

    @classmethod
    def create(cls, arg : NonSpare_51_Arg) -> "NonSpare_51":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_40:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_44:
        return self.rule.variation

Item_21_Arg: TypeAlias = NonSpare_51_Arg
class Item_21(Item):
    cv_non_spare = NonSpare_51

    @classmethod
    def create(cls, arg : Item_21_Arg) -> "Item_21":
        return cls._create(arg) # type: ignore

Variation_51_Arg_Group_1: TypeAlias = Union[int, Tuple[Union[RuleVariation_6_Arg, Tuple[Literal["I1"], RuleVariation_6_Arg]], None]]
Variation_51_Arg_Group_2: TypeAlias = Union[int, Tuple[Union[RuleVariation_40_Arg, Tuple[Literal["I2"], RuleVariation_40_Arg]], None]]
Variation_51_Arg: TypeAlias = Union[
    Tuple[Variation_51_Arg_Group_1],
    Tuple[Variation_51_Arg_Group_1, Variation_51_Arg_Group_2],
]
class Variation_51(Extended):
    cv_items_list = [[(Item_14, 7), None], [(Item_21, 7), None]]

    @classmethod
    def create(cls, arg : Variation_51_Arg) -> 'Variation_51':
        return cls._create(arg) # type: ignore

    @overload
    def get_item(self, key : Literal["I1"]) -> RuleVariation_6:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[RuleVariation_40]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

RuleVariation_46_Arg: TypeAlias = Variation_51_Arg
class RuleVariation_46(RuleVariationContextFree):
    cv_variation = Variation_51

    @classmethod
    def create(cls, arg : RuleVariation_46_Arg) -> "RuleVariation_46":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_51:
        return self.arg # type: ignore

NonSpare_29_Arg: TypeAlias = RuleVariation_46_Arg
class NonSpare_29(NonSpare):
    cv_name = "102"
    cv_title = "Nested Groups Extended"
    cv_rule = RuleVariation_46

    @classmethod
    def create(cls, arg : NonSpare_29_Arg) -> "NonSpare_29":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_46:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_51:
        return self.rule.variation

class UapItem_29(UapItem):
    cv_non_spare = NonSpare_29

Record_6_Arg = TypedDict('Record_6_Arg', {
    "010": NonSpare_3_Arg,
    "000": NonSpare_0_Arg,
    "020": NonSpare_4_Arg,
    "030": NonSpare_6_Arg,
    "031": NonSpare_7_Arg,
    "032": NonSpare_10_Arg,
    "040": NonSpare_12_Arg,
    "051": NonSpare_13_Arg,
    "052": NonSpare_14_Arg,
    "053": NonSpare_15_Arg,
    "054": NonSpare_16_Arg,
    "061": NonSpare_17_Arg,
    "062": NonSpare_18_Arg,
    "063": NonSpare_19_Arg,
    "071": NonSpare_20_Arg,
    "072": NonSpare_21_Arg,
    "073": NonSpare_22_Arg,
    "091": NonSpare_23_Arg,
    "092": NonSpare_24_Arg,
    "093": NonSpare_25_Arg,
    "101": NonSpare_27_Arg,
    "102": NonSpare_29_Arg,
}, total=False)
Record_6_Union = Union[
    Tuple[Literal["010"], NonSpare_3_Arg],
    Tuple[Literal["000"], NonSpare_0_Arg],
    Tuple[Literal["020"], NonSpare_4_Arg],
    Tuple[Literal["030"], NonSpare_6_Arg],
    Tuple[Literal["031"], NonSpare_7_Arg],
    Tuple[Literal["032"], NonSpare_10_Arg],
    Tuple[Literal["040"], NonSpare_12_Arg],
    Tuple[Literal["051"], NonSpare_13_Arg],
    Tuple[Literal["052"], NonSpare_14_Arg],
    Tuple[Literal["053"], NonSpare_15_Arg],
    Tuple[Literal["054"], NonSpare_16_Arg],
    Tuple[Literal["061"], NonSpare_17_Arg],
    Tuple[Literal["062"], NonSpare_18_Arg],
    Tuple[Literal["063"], NonSpare_19_Arg],
    Tuple[Literal["071"], NonSpare_20_Arg],
    Tuple[Literal["072"], NonSpare_21_Arg],
    Tuple[Literal["073"], NonSpare_22_Arg],
    Tuple[Literal["091"], NonSpare_23_Arg],
    Tuple[Literal["092"], NonSpare_24_Arg],
    Tuple[Literal["093"], NonSpare_25_Arg],
    Tuple[Literal["101"], NonSpare_27_Arg],
    Tuple[Literal["102"], NonSpare_29_Arg],
]
class Record_6(Record):
    cv_fspec_max_bytes = 4
    cv_items_list = [UapItem_3, UapItem_0, UapItem_4, UapItem_6, UapItem_7, UapItem_10, UapItem_12, UapItem_13, UapItem_14, UapItem_15, UapItem_16, UapItem_17, UapItem_18, UapItem_19, UapItem_20, UapItem_21, UapItem_22, UapItem_33, UapItem_34, UapItem_23, UapItem_24, UapItem_25, UapItem_27, UapItem_29]
    cv_items_dict = {"010": NonSpare_3, "000": NonSpare_0, "020": NonSpare_4, "030": NonSpare_6, "031": NonSpare_7, "032": NonSpare_10, "040": NonSpare_12, "051": NonSpare_13, "052": NonSpare_14, "053": NonSpare_15, "054": NonSpare_16, "061": NonSpare_17, "062": NonSpare_18, "063": NonSpare_19, "071": NonSpare_20, "072": NonSpare_21, "073": NonSpare_22, "091": NonSpare_23, "092": NonSpare_24, "093": NonSpare_25, "101": NonSpare_27, "102": NonSpare_29}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> NonSpare_3:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["000"]) -> NonSpare_0:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["020"]) -> NonSpare_4:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["030"]) -> NonSpare_6:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["031"]) -> NonSpare_7:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["032"]) -> NonSpare_10:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["040"]) -> NonSpare_12:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["051"]) -> NonSpare_13:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["052"]) -> NonSpare_14:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["053"]) -> NonSpare_15:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["054"]) -> NonSpare_16:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["061"]) -> NonSpare_17:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["062"]) -> NonSpare_18:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["063"]) -> NonSpare_19:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["071"]) -> NonSpare_20:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["072"]) -> NonSpare_21:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["073"]) -> NonSpare_22:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["091"]) -> NonSpare_23:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["092"]) -> NonSpare_24:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["093"]) -> NonSpare_25:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["101"]) -> NonSpare_27:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["102"]) -> NonSpare_29:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["000"], Literal["020"], Literal["030"], Literal["031"], Literal["032"], Literal["040"], Literal["051"], Literal["052"], Literal["053"], Literal["054"], Literal["061"], Literal["062"], Literal["063"], Literal["071"], Literal["072"], Literal["073"], Literal["091"], Literal["092"], Literal["093"], Literal["101"], Literal["102"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["010"]) -> Optional[NonSpare_3]:
        ...
    @overload
    def get_item(self, key : Literal["000"]) -> Optional[NonSpare_0]:
        ...
    @overload
    def get_item(self, key : Literal["020"]) -> Optional[NonSpare_4]:
        ...
    @overload
    def get_item(self, key : Literal["030"]) -> Optional[NonSpare_6]:
        ...
    @overload
    def get_item(self, key : Literal["031"]) -> Optional[NonSpare_7]:
        ...
    @overload
    def get_item(self, key : Literal["032"]) -> Optional[NonSpare_10]:
        ...
    @overload
    def get_item(self, key : Literal["040"]) -> Optional[NonSpare_12]:
        ...
    @overload
    def get_item(self, key : Literal["051"]) -> Optional[NonSpare_13]:
        ...
    @overload
    def get_item(self, key : Literal["052"]) -> Optional[NonSpare_14]:
        ...
    @overload
    def get_item(self, key : Literal["053"]) -> Optional[NonSpare_15]:
        ...
    @overload
    def get_item(self, key : Literal["054"]) -> Optional[NonSpare_16]:
        ...
    @overload
    def get_item(self, key : Literal["061"]) -> Optional[NonSpare_17]:
        ...
    @overload
    def get_item(self, key : Literal["062"]) -> Optional[NonSpare_18]:
        ...
    @overload
    def get_item(self, key : Literal["063"]) -> Optional[NonSpare_19]:
        ...
    @overload
    def get_item(self, key : Literal["071"]) -> Optional[NonSpare_20]:
        ...
    @overload
    def get_item(self, key : Literal["072"]) -> Optional[NonSpare_21]:
        ...
    @overload
    def get_item(self, key : Literal["073"]) -> Optional[NonSpare_22]:
        ...
    @overload
    def get_item(self, key : Literal["091"]) -> Optional[NonSpare_23]:
        ...
    @overload
    def get_item(self, key : Literal["092"]) -> Optional[NonSpare_24]:
        ...
    @overload
    def get_item(self, key : Literal["093"]) -> Optional[NonSpare_25]:
        ...
    @overload
    def get_item(self, key : Literal["101"]) -> Optional[NonSpare_27]:
        ...
    @overload
    def get_item(self, key : Literal["102"]) -> Optional[NonSpare_29]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : NonSpare_3_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["000"], val : NonSpare_0_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["020"], val : NonSpare_4_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["030"], val : NonSpare_6_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["031"], val : NonSpare_7_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["032"], val : NonSpare_10_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["040"], val : NonSpare_12_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["051"], val : NonSpare_13_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["052"], val : NonSpare_14_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["053"], val : NonSpare_15_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["054"], val : NonSpare_16_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["061"], val : NonSpare_17_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["062"], val : NonSpare_18_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["063"], val : NonSpare_19_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["071"], val : NonSpare_20_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["072"], val : NonSpare_21_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["073"], val : NonSpare_22_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["091"], val : NonSpare_23_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["092"], val : NonSpare_24_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["093"], val : NonSpare_25_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["101"], val : NonSpare_27_Arg) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["102"], val : NonSpare_29_Arg) -> 'Record_6':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["000"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["020"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["030"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["031"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["032"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["040"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["051"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["052"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["053"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["054"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["061"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["062"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["063"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["071"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["072"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["073"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["091"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["092"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["093"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["101"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["102"]) -> 'Record_6':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @overload
    def get_rfs_item(self, arg : Literal["010"]) -> List[NonSpare_3]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["000"]) -> List[NonSpare_0]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["020"]) -> List[NonSpare_4]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["030"]) -> List[NonSpare_6]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["031"]) -> List[NonSpare_7]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["032"]) -> List[NonSpare_10]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["040"]) -> List[NonSpare_12]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["051"]) -> List[NonSpare_13]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["052"]) -> List[NonSpare_14]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["053"]) -> List[NonSpare_15]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["054"]) -> List[NonSpare_16]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["061"]) -> List[NonSpare_17]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["062"]) -> List[NonSpare_18]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["063"]) -> List[NonSpare_19]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["071"]) -> List[NonSpare_20]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["072"]) -> List[NonSpare_21]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["073"]) -> List[NonSpare_22]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["091"]) -> List[NonSpare_23]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["092"]) -> List[NonSpare_24]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["093"]) -> List[NonSpare_25]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["101"]) -> List[NonSpare_27]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["102"]) -> List[NonSpare_29]:
        ...
    def get_rfs_item(self, arg : Any) -> Any:
        return self._get_rfs_item(arg)

    @classmethod
    def create(cls, arg : Record_6_Arg, rfs : Optional[List[Record_6_Union]] = None) -> 'Record_6':
        return cls._create(arg, rfs) # type: ignore

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, Tuple["Record_6", Bits]]:
        return cls._parse(bs) # type: ignore

Uap_0_Arg: TypeAlias = Record_6
class Uap_0(UapSingle):
    cv_record = Record_6

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, List[Record_6]]:
        return cls._parse(bs)

class Asterix_0(AstCat):
    cv_category = 0
    cv_edition = (1, 0)
    cv_uap = Uap_0

    # shortcut to record
    cv_record = cv_uap.cv_record

    @classmethod
    def create(cls, records : List[Uap_0_Arg]) -> "Asterix_0":
        return cls._create(records) # type: ignore

Expansion_0_Arg = TypedDict('Expansion_0_Arg', {
    "I1": NonSpare_44_Arg,
    "I2": NonSpare_49_Arg,
}, total=False)
class Expansion_0(Expansion):
    cv_fspec_bytes = 1
    cv_items_list = [NonSpare_44, NonSpare_49]
    items_dict = {"I1": NonSpare_44, "I2": NonSpare_49}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> NonSpare_44:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> NonSpare_49:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_44]:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[NonSpare_49]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["I1"], val : NonSpare_44_Arg) -> "Expansion_0":
        ...
    @overload
    def set_item(self, key : Literal["I2"], val : NonSpare_49_Arg) -> "Expansion_0":
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["I1"]) -> "Expansion_0":
        ...
    @overload
    def del_item(self, key : Literal["I2"]) -> "Expansion_0":
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg : Expansion_0_Arg) -> 'Expansion_0':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, Tuple["Expansion_0", Bits]]:
        return cls._parse(bs) # type: ignore

class Asterix_1(AstRef):
    cv_category = 0
    cv_edition = (1, 0)
    cv_expansion = Expansion_0

Expansion_1_Arg = TypedDict('Expansion_1_Arg', {
    "I1": NonSpare_44_Arg,
    "I2": NonSpare_49_Arg,
    "I3": NonSpare_55_Arg,
}, total=False)
class Expansion_1(Expansion):
    cv_fspec_bytes = 2
    cv_items_list = [NonSpare_44, None, None, NonSpare_49, None, None, None, None, None, None, NonSpare_55]
    items_dict = {"I1": NonSpare_44, "I2": NonSpare_49, "I3": NonSpare_55}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> NonSpare_44:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> NonSpare_49:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I3"]) -> NonSpare_55:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"], Literal["I3"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_44]:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[NonSpare_49]:
        ...
    @overload
    def get_item(self, key : Literal["I3"]) -> Optional[NonSpare_55]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["I1"], val : NonSpare_44_Arg) -> "Expansion_1":
        ...
    @overload
    def set_item(self, key : Literal["I2"], val : NonSpare_49_Arg) -> "Expansion_1":
        ...
    @overload
    def set_item(self, key : Literal["I3"], val : NonSpare_55_Arg) -> "Expansion_1":
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["I1"]) -> "Expansion_1":
        ...
    @overload
    def del_item(self, key : Literal["I2"]) -> "Expansion_1":
        ...
    @overload
    def del_item(self, key : Literal["I3"]) -> "Expansion_1":
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg : Expansion_1_Arg) -> 'Expansion_1':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, Tuple["Expansion_1", Bits]]:
        return cls._parse(bs) # type: ignore

class Asterix_2(AstRef):
    cv_category = 0
    cv_edition = (1, 1)
    cv_expansion = Expansion_1

NonSpare_2_Arg: TypeAlias = RuleVariation_39_Arg
class NonSpare_2(NonSpare):
    cv_name = "010"
    cv_title = "Data Source Identifier"
    cv_rule = RuleVariation_39

    @classmethod
    def create(cls, arg : NonSpare_2_Arg) -> "NonSpare_2":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_39:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_43:
        return self.rule.variation

class UapItem_2(UapItem):
    cv_non_spare = NonSpare_2

Content_3_Arg: TypeAlias = int
class Content_3(ContentTable):
    cv_values = {0: "Plot", 1: "Track"}

RuleContent_3_Arg: TypeAlias = Content_3_Arg
class RuleContent_3(RuleContentContextFree):
    cv_content = Content_3

    @property
    def content(self) -> Content_3:
        return self._get_content() # type: ignore

Variation_2_Arg: TypeAlias = RuleContent_3_Arg
class Variation_2(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 1
    cv_rule = RuleContent_3

    @classmethod
    def create(cls, arg : Variation_2_Arg) -> "Variation_2":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_3:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_3:
        return self.rule.content

RuleVariation_2_Arg: TypeAlias = Variation_2_Arg
class RuleVariation_2(RuleVariationContextFree):
    cv_variation = Variation_2

    @classmethod
    def create(cls, arg : RuleVariation_2_Arg) -> "RuleVariation_2":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_2:
        return self.arg # type: ignore

NonSpare_77_Arg: TypeAlias = RuleVariation_2_Arg
class NonSpare_77(NonSpare):
    cv_name = "TYP"
    cv_title = ""
    cv_rule = RuleVariation_2

    @classmethod
    def create(cls, arg : NonSpare_77_Arg) -> "NonSpare_77":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_2:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_2:
        return self.rule.variation

Item_35_Arg: TypeAlias = NonSpare_77_Arg
class Item_35(Item):
    cv_non_spare = NonSpare_77

    @classmethod
    def create(cls, arg : Item_35_Arg) -> "Item_35":
        return cls._create(arg) # type: ignore

NonSpare_45_Arg: TypeAlias = RuleVariation_27_Arg
class NonSpare_45(NonSpare):
    cv_name = "I1"
    cv_title = ""
    cv_rule = RuleVariation_27

    @classmethod
    def create(cls, arg : NonSpare_45_Arg) -> "NonSpare_45":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_27:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_27:
        return self.rule.variation

Item_16_Arg: TypeAlias = NonSpare_45_Arg
class Item_16(Item):
    cv_non_spare = NonSpare_45

    @classmethod
    def create(cls, arg : Item_16_Arg) -> "Item_16":
        return cls._create(arg) # type: ignore

NonSpare_48_Arg: TypeAlias = RuleVariation_6_Arg
class NonSpare_48(NonSpare):
    cv_name = "I2"
    cv_title = ""
    cv_rule = RuleVariation_6

    @classmethod
    def create(cls, arg : NonSpare_48_Arg) -> "NonSpare_48":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_6:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_6:
        return self.rule.variation

Item_18_Arg: TypeAlias = NonSpare_48_Arg
class Item_18(Item):
    cv_non_spare = NonSpare_48

    @classmethod
    def create(cls, arg : Item_18_Arg) -> "Item_18":
        return cls._create(arg) # type: ignore

Variation_52_Arg_Group_1: TypeAlias = Union[int, Tuple[Union[RuleVariation_2_Arg, Tuple[Literal["TYP"], RuleVariation_2_Arg]], Union[RuleVariation_27_Arg, Tuple[Literal["I1"], RuleVariation_27_Arg]], None]]
Variation_52_Arg_Group_2: TypeAlias = Union[int, Tuple[Union[RuleVariation_6_Arg, Tuple[Literal["I2"], RuleVariation_6_Arg]], None]]
Variation_52_Arg: TypeAlias = Union[
    Tuple[Variation_52_Arg_Group_1],
    Tuple[Variation_52_Arg_Group_1, Variation_52_Arg_Group_2],
]
class Variation_52(Extended):
    cv_items_list = [[(Item_35, 1), (Item_16, 6), None], [(Item_18, 7), None]]

    @classmethod
    def create(cls, arg : Variation_52_Arg) -> 'Variation_52':
        return cls._create(arg) # type: ignore

    @overload
    def get_item(self, key : Literal["TYP"]) -> RuleVariation_2:
        ...
    @overload
    def get_item(self, key : Literal["I1"]) -> RuleVariation_27:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[RuleVariation_6]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

RuleVariation_47_Arg: TypeAlias = Variation_52_Arg
class RuleVariation_47(RuleVariationContextFree):
    cv_variation = Variation_52

    @classmethod
    def create(cls, arg : RuleVariation_47_Arg) -> "RuleVariation_47":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_52:
        return self.arg # type: ignore

NonSpare_5_Arg: TypeAlias = RuleVariation_47_Arg
class NonSpare_5(NonSpare):
    cv_name = "020"
    cv_title = "Target Report Descriptor"
    cv_rule = RuleVariation_47

    @classmethod
    def create(cls, arg : NonSpare_5_Arg) -> "NonSpare_5":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_47:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_52:
        return self.rule.variation

class UapItem_5(UapItem):
    cv_non_spare = NonSpare_5

NonSpare_8_Arg: TypeAlias = RuleVariation_7_Arg
class NonSpare_8(NonSpare):
    cv_name = "031"
    cv_title = "For Plots Only"
    cv_rule = RuleVariation_7

    @classmethod
    def create(cls, arg : NonSpare_8_Arg) -> "NonSpare_8":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_8(UapItem):
    cv_non_spare = NonSpare_8

NonSpare_11_Arg: TypeAlias = RuleVariation_7_Arg
class NonSpare_11(NonSpare):
    cv_name = "040"
    cv_title = "Common"
    cv_rule = RuleVariation_7

    @classmethod
    def create(cls, arg : NonSpare_11_Arg) -> "NonSpare_11":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_11(UapItem):
    cv_non_spare = NonSpare_11

Record_4_Arg = TypedDict('Record_4_Arg', {
    "010": NonSpare_2_Arg,
    "020": NonSpare_5_Arg,
    "031": NonSpare_8_Arg,
    "040": NonSpare_11_Arg,
}, total=False)
class Record_4(Record):
    cv_fspec_max_bytes = 1
    cv_items_list = [UapItem_2, UapItem_5, UapItem_8, UapItem_33, UapItem_11]
    cv_items_dict = {"010": NonSpare_2, "020": NonSpare_5, "031": NonSpare_8, "040": NonSpare_11}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> NonSpare_2:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["020"]) -> NonSpare_5:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["031"]) -> NonSpare_8:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["040"]) -> NonSpare_11:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["020"], Literal["031"], Literal["040"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["010"]) -> Optional[NonSpare_2]:
        ...
    @overload
    def get_item(self, key : Literal["020"]) -> Optional[NonSpare_5]:
        ...
    @overload
    def get_item(self, key : Literal["031"]) -> Optional[NonSpare_8]:
        ...
    @overload
    def get_item(self, key : Literal["040"]) -> Optional[NonSpare_11]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : NonSpare_2_Arg) -> 'Record_4':
        ...
    @overload
    def set_item(self, key : Literal["020"], val : NonSpare_5_Arg) -> 'Record_4':
        ...
    @overload
    def set_item(self, key : Literal["031"], val : NonSpare_8_Arg) -> 'Record_4':
        ...
    @overload
    def set_item(self, key : Literal["040"], val : NonSpare_11_Arg) -> 'Record_4':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_4':
        ...
    @overload
    def del_item(self, key : Literal["020"]) -> 'Record_4':
        ...
    @overload
    def del_item(self, key : Literal["031"]) -> 'Record_4':
        ...
    @overload
    def del_item(self, key : Literal["040"]) -> 'Record_4':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg : Record_4_Arg) -> 'Record_4':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, Tuple["Record_4", Bits]]:
        return cls._parse(bs) # type: ignore

Variation_15_Arg: TypeAlias = RuleContent_0_Arg
class Variation_15(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 16
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_15_Arg) -> "Variation_15":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_15_Arg: TypeAlias = Variation_15_Arg
class RuleVariation_15(RuleVariationContextFree):
    cv_variation = Variation_15

    @classmethod
    def create(cls, arg : RuleVariation_15_Arg) -> "RuleVariation_15":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_15:
        return self.arg # type: ignore

NonSpare_9_Arg: TypeAlias = RuleVariation_15_Arg
class NonSpare_9(NonSpare):
    cv_name = "032"
    cv_title = "For Tracks Only"
    cv_rule = RuleVariation_15

    @classmethod
    def create(cls, arg : NonSpare_9_Arg) -> "NonSpare_9":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_15:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_15:
        return self.rule.variation

class UapItem_9(UapItem):
    cv_non_spare = NonSpare_9

Record_5_Arg = TypedDict('Record_5_Arg', {
    "010": NonSpare_2_Arg,
    "020": NonSpare_5_Arg,
    "032": NonSpare_9_Arg,
    "040": NonSpare_11_Arg,
}, total=False)
class Record_5(Record):
    cv_fspec_max_bytes = 1
    cv_items_list = [UapItem_2, UapItem_5, UapItem_33, UapItem_9, UapItem_11]
    cv_items_dict = {"010": NonSpare_2, "020": NonSpare_5, "032": NonSpare_9, "040": NonSpare_11}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> NonSpare_2:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["020"]) -> NonSpare_5:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["032"]) -> NonSpare_9:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["040"]) -> NonSpare_11:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["020"], Literal["032"], Literal["040"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["010"]) -> Optional[NonSpare_2]:
        ...
    @overload
    def get_item(self, key : Literal["020"]) -> Optional[NonSpare_5]:
        ...
    @overload
    def get_item(self, key : Literal["032"]) -> Optional[NonSpare_9]:
        ...
    @overload
    def get_item(self, key : Literal["040"]) -> Optional[NonSpare_11]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : NonSpare_2_Arg) -> 'Record_5':
        ...
    @overload
    def set_item(self, key : Literal["020"], val : NonSpare_5_Arg) -> 'Record_5':
        ...
    @overload
    def set_item(self, key : Literal["032"], val : NonSpare_9_Arg) -> 'Record_5':
        ...
    @overload
    def set_item(self, key : Literal["040"], val : NonSpare_11_Arg) -> 'Record_5':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_5':
        ...
    @overload
    def del_item(self, key : Literal["020"]) -> 'Record_5':
        ...
    @overload
    def del_item(self, key : Literal["032"]) -> 'Record_5':
        ...
    @overload
    def del_item(self, key : Literal["040"]) -> 'Record_5':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg : Record_5_Arg) -> 'Record_5':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, Tuple["Record_5", Bits]]:
        return cls._parse(bs) # type: ignore

Uap_1_Arg: TypeAlias = Union[Record_4, Record_5]
class Uap_1(UapMultiple):
    cv_uaps = {"plot": Record_4, "track": Record_5}
    cv_selector = (["020", "TYP"], {0: "plot", 1: "track"})

    @overload
    @classmethod
    def spec(cls, key : Literal["plot"]) -> Record_4:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["track"]) -> Record_5:
        ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls._spec(key)

    @overload
    @classmethod
    def parse(cls, uap : Literal["plot"], bs : Bits) -> Union[ValueError, List[Record_4]]:
        ...
    @overload
    @classmethod
    def parse(cls, uap : Literal["track"], bs : Bits) -> Union[ValueError, List[Record_5]]:
        ...
    @classmethod
    def parse(cls, uap : str, bs : Bits) -> Any:
        return cls._parse(uap, bs)

    @classmethod
    def parse_any_uap(cls, bs : Bits) -> List[List[Uap_1_Arg]]:
        return cls._parse_any_uap(bs)

class Asterix_3(AstCat):
    cv_category = 1
    cv_edition = (1, 0)
    cv_uap = Uap_1

    @classmethod
    def create(cls, records : List[Uap_1_Arg]) -> "Asterix_3":
        return cls._create(records) # type: ignore

NonSpare_1_Arg: TypeAlias = RuleVariation_7_Arg
class NonSpare_1(NonSpare):
    cv_name = "010"
    cv_title = ""
    cv_rule = RuleVariation_7

    @classmethod
    def create(cls, arg : NonSpare_1_Arg) -> "NonSpare_1":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_1(UapItem):
    cv_non_spare = NonSpare_1

NonSpare_26_Arg: TypeAlias = RuleVariation_7_Arg
class NonSpare_26(NonSpare):
    cv_name = "101"
    cv_title = ""
    cv_rule = RuleVariation_7

    @classmethod
    def create(cls, arg : NonSpare_26_Arg) -> "NonSpare_26":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_26(UapItem):
    cv_non_spare = NonSpare_26

NonSpare_28_Arg: TypeAlias = RuleVariation_7_Arg
class NonSpare_28(NonSpare):
    cv_name = "102"
    cv_title = ""
    cv_rule = RuleVariation_7

    @classmethod
    def create(cls, arg : NonSpare_28_Arg) -> "NonSpare_28":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_28(UapItem):
    cv_non_spare = NonSpare_28

Record_1_Arg = TypedDict('Record_1_Arg', {
    "010": NonSpare_1_Arg,
    "101": NonSpare_26_Arg,
    "102": NonSpare_28_Arg,
}, total=False)
class Record_1(Record):
    cv_fspec_max_bytes = 1
    cv_items_list = [UapItem_1, UapItem_26, UapItem_28]
    cv_items_dict = {"010": NonSpare_1, "101": NonSpare_26, "102": NonSpare_28}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> NonSpare_1:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["101"]) -> NonSpare_26:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["102"]) -> NonSpare_28:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["101"], Literal["102"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["010"]) -> Optional[NonSpare_1]:
        ...
    @overload
    def get_item(self, key : Literal["101"]) -> Optional[NonSpare_26]:
        ...
    @overload
    def get_item(self, key : Literal["102"]) -> Optional[NonSpare_28]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : NonSpare_1_Arg) -> 'Record_1':
        ...
    @overload
    def set_item(self, key : Literal["101"], val : NonSpare_26_Arg) -> 'Record_1':
        ...
    @overload
    def set_item(self, key : Literal["102"], val : NonSpare_28_Arg) -> 'Record_1':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_1':
        ...
    @overload
    def del_item(self, key : Literal["101"]) -> 'Record_1':
        ...
    @overload
    def del_item(self, key : Literal["102"]) -> 'Record_1':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg : Record_1_Arg) -> 'Record_1':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, Tuple["Record_1", Bits]]:
        return cls._parse(bs) # type: ignore

NonSpare_30_Arg: TypeAlias = RuleVariation_15_Arg
class NonSpare_30(NonSpare):
    cv_name = "201"
    cv_title = ""
    cv_rule = RuleVariation_15

    @classmethod
    def create(cls, arg : NonSpare_30_Arg) -> "NonSpare_30":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_15:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_15:
        return self.rule.variation

class UapItem_30(UapItem):
    cv_non_spare = NonSpare_30

NonSpare_31_Arg: TypeAlias = RuleVariation_15_Arg
class NonSpare_31(NonSpare):
    cv_name = "202"
    cv_title = ""
    cv_rule = RuleVariation_15

    @classmethod
    def create(cls, arg : NonSpare_31_Arg) -> "NonSpare_31":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_15:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_15:
        return self.rule.variation

class UapItem_31(UapItem):
    cv_non_spare = NonSpare_31

Record_2_Arg = TypedDict('Record_2_Arg', {
    "010": NonSpare_1_Arg,
    "201": NonSpare_30_Arg,
    "202": NonSpare_31_Arg,
}, total=False)
class Record_2(Record):
    cv_fspec_max_bytes = 1
    cv_items_list = [UapItem_1, UapItem_30, UapItem_31]
    cv_items_dict = {"010": NonSpare_1, "201": NonSpare_30, "202": NonSpare_31}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> NonSpare_1:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["201"]) -> NonSpare_30:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["202"]) -> NonSpare_31:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["201"], Literal["202"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["010"]) -> Optional[NonSpare_1]:
        ...
    @overload
    def get_item(self, key : Literal["201"]) -> Optional[NonSpare_30]:
        ...
    @overload
    def get_item(self, key : Literal["202"]) -> Optional[NonSpare_31]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : NonSpare_1_Arg) -> 'Record_2':
        ...
    @overload
    def set_item(self, key : Literal["201"], val : NonSpare_30_Arg) -> 'Record_2':
        ...
    @overload
    def set_item(self, key : Literal["202"], val : NonSpare_31_Arg) -> 'Record_2':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_2':
        ...
    @overload
    def del_item(self, key : Literal["201"]) -> 'Record_2':
        ...
    @overload
    def del_item(self, key : Literal["202"]) -> 'Record_2':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg : Record_2_Arg) -> 'Record_2':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, Tuple["Record_2", Bits]]:
        return cls._parse(bs) # type: ignore

Variation_20_Arg: TypeAlias = RuleContent_0_Arg
class Variation_20(Element):
    cv_bit_offset8 = 0
    cv_bit_size = 32
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg : Variation_20_Arg) -> "Variation_20":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut to content
    @property
    def content(self) -> Content_0:
        return self.rule.content

RuleVariation_20_Arg: TypeAlias = Variation_20_Arg
class RuleVariation_20(RuleVariationContextFree):
    cv_variation = Variation_20

    @classmethod
    def create(cls, arg : RuleVariation_20_Arg) -> "RuleVariation_20":
        return cls._create(arg) # type: ignore

    @property
    def variation(self) -> Variation_20:
        return self.arg # type: ignore

NonSpare_32_Arg: TypeAlias = RuleVariation_20_Arg
class NonSpare_32(NonSpare):
    cv_name = "301"
    cv_title = ""
    cv_rule = RuleVariation_20

    @classmethod
    def create(cls, arg : NonSpare_32_Arg) -> "NonSpare_32":
        return cls._create(arg) # type: ignore

    @property
    def rule(self) -> RuleVariation_20:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_20:
        return self.rule.variation

class UapItem_32(UapItem):
    cv_non_spare = NonSpare_32

Record_3_Arg = TypedDict('Record_3_Arg', {
    "010": NonSpare_1_Arg,
    "301": NonSpare_32_Arg,
}, total=False)
class Record_3(Record):
    cv_fspec_max_bytes = 1
    cv_items_list = [UapItem_1, UapItem_32]
    cv_items_dict = {"010": NonSpare_1, "301": NonSpare_32}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> NonSpare_1:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["301"]) -> NonSpare_32:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["301"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["010"]) -> Optional[NonSpare_1]:
        ...
    @overload
    def get_item(self, key : Literal["301"]) -> Optional[NonSpare_32]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : NonSpare_1_Arg) -> 'Record_3':
        ...
    @overload
    def set_item(self, key : Literal["301"], val : NonSpare_32_Arg) -> 'Record_3':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_3':
        ...
    @overload
    def del_item(self, key : Literal["301"]) -> 'Record_3':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg : Record_3_Arg) -> 'Record_3':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, Tuple["Record_3", Bits]]:
        return cls._parse(bs) # type: ignore

Record_0_Arg = TypedDict('Record_0_Arg', {
    "010": NonSpare_1_Arg,
}, total=False)
class Record_0(Record):
    cv_fspec_max_bytes = 1
    cv_items_list = [UapItem_1]
    cv_items_dict = {"010": NonSpare_1}

    @classmethod
    def spec(cls, key : Literal["010"]) -> NonSpare_1:
        return cls._spec(arg) # type: ignore

    def get_item(self, key : Literal["010"]) -> Optional[NonSpare_1]:
        return self._get_item(key) # type: ignore

    def set_item(self, key : Literal["010"], val : NonSpare_1_Arg) -> 'Record_0':
        return self._set_item(key, val) # type: ignore

    def del_item(self, key : Literal["010"]) -> 'Record_0':
        return self._del_item(key) # type: ignore

    @classmethod
    def create(cls, arg : Record_0_Arg) -> 'Record_0':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, Tuple["Record_0", Bits]]:
        return cls._parse(bs) # type: ignore

Uap_2_Arg: TypeAlias = Union[Record_1, Record_2, Record_3, Record_0]
class Uap_2(UapMultiple):
    cv_uaps = {"uap1": Record_1, "uap2": Record_2, "uap3": Record_3, "uap4": Record_0}
    cv_selector = None

    @overload
    @classmethod
    def spec(cls, key : Literal["uap1"]) -> Record_1:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["uap2"]) -> Record_2:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["uap3"]) -> Record_3:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["uap4"]) -> Record_0:
        ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls._spec(key)

    @overload
    @classmethod
    def parse(cls, uap : Literal["uap1"], bs : Bits) -> Union[ValueError, List[Record_1]]:
        ...
    @overload
    @classmethod
    def parse(cls, uap : Literal["uap2"], bs : Bits) -> Union[ValueError, List[Record_2]]:
        ...
    @overload
    @classmethod
    def parse(cls, uap : Literal["uap3"], bs : Bits) -> Union[ValueError, List[Record_3]]:
        ...
    @overload
    @classmethod
    def parse(cls, uap : Literal["uap4"], bs : Bits) -> Union[ValueError, List[Record_0]]:
        ...
    @classmethod
    def parse(cls, uap : str, bs : Bits) -> Any:
        return cls._parse(uap, bs)

    @classmethod
    def parse_any_uap(cls, bs : Bits) -> List[List[Uap_2_Arg]]:
        return cls._parse_any_uap(bs)

class Asterix_4(AstCat):
    cv_category = 2
    cv_edition = (1, 0)
    cv_uap = Uap_2

    @classmethod
    def create(cls, records : List[Uap_2_Arg]) -> "Asterix_4":
        return cls._create(records) # type: ignore

# Aliases

Cat_000_1_0: TypeAlias = Asterix_0
Ref_000_1_0: TypeAlias = Asterix_1
Ref_000_1_1: TypeAlias = Asterix_2
Cat_001_1_0: TypeAlias = Asterix_3
Cat_002_1_0: TypeAlias = Asterix_4

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
