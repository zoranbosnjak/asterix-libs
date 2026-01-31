# Asterix specifications

# This file is generated, DO NOT EDIT!
# For more details, see:
#     - https://github.com/zoranbosnjak/asterix-specs

from asterix.base import *

# Asterix types

class Content_0(ContentRaw):
    cv_arg: TypeAlias = int

class RuleContent_0(RuleContentContextFree):
    cv_arg: TypeAlias = Content_0.cv_arg
    cv_content: TypeAlias = Content_0

    @property
    def content(self) -> Content_0:
        return self._get_content() # type: ignore

class Variation_7(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_7.cv_arg") -> "Variation_7":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_7", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_7(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_7.cv_arg
    cv_variation: TypeAlias = Variation_7

    @classmethod
    def create(cls, arg : "RuleVariation_7.cv_arg") -> "RuleVariation_7":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_7", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_7:
        return self.arg # type: ignore

class NonSpare_84(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "SAC"
    cv_title = "System Area Code"
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_84.cv_arg") -> "NonSpare_84":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_84", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class Item_34(Item):
    cv_arg: TypeAlias = NonSpare_84.cv_arg
    cv_non_spare: TypeAlias = NonSpare_84

    @classmethod
    def create(cls, arg : "Item_34.cv_arg") -> "Item_34":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_34", Bits]]:
        return cls._parse(bs) # type: ignore

class NonSpare_87(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "SIC"
    cv_title = "System Identification Code"
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_87.cv_arg") -> "NonSpare_87":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_87", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class Item_37(Item):
    cv_arg: TypeAlias = NonSpare_87.cv_arg
    cv_non_spare: TypeAlias = NonSpare_87

    @classmethod
    def create(cls, arg : "Item_37.cv_arg") -> "Item_37":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_37", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_44(Group):
    cv_arg_group: TypeAlias = Tuple[Union[RuleVariation_7.cv_arg, Tuple[Literal["SAC"], RuleVariation_7.cv_arg]], Union[RuleVariation_7.cv_arg, Tuple[Literal["SIC"], RuleVariation_7.cv_arg]]]
    cv_arg: TypeAlias = Union[int, "Variation_44.cv_arg_group"]
    cv_bit_offset8 = 0
    cv_bit_size = 16
    cv_items_list = [(Item_34, 8), (Item_37, 8)]
    cv_items_dict = {"SAC": RuleVariation_7, "SIC": RuleVariation_7}

    @overload
    @classmethod
    def spec(cls, key : Literal["SAC"]) -> Type[RuleVariation_7]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["SIC"]) -> Type[RuleVariation_7]:
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
    def create(cls, arg: "Variation_44.cv_arg") -> "Variation_44":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_44", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_40(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_44.cv_arg
    cv_variation: TypeAlias = Variation_44

    @classmethod
    def create(cls, arg : "RuleVariation_40.cv_arg") -> "RuleVariation_40":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_40", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_44:
        return self.arg # type: ignore

class NonSpare_3(NonSpare):
    cv_arg: TypeAlias = RuleVariation_40.cv_arg
    cv_name = "010"
    cv_title = "Data Source Identifier"
    cv_rule: TypeAlias = RuleVariation_40

    @classmethod
    def create(cls, arg : "NonSpare_3.cv_arg") -> "NonSpare_3":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_3", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_40:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_44:
        return self.rule.variation

class UapItem_3(UapItem):
    cv_non_spare: TypeAlias = NonSpare_3

class Content_7(ContentTable):
    cv_arg: TypeAlias = int
    cv_values = {1: "Message 1", 2: "Message 2", 3: "Message 3"}

class RuleContent_7(RuleContentContextFree):
    cv_arg: TypeAlias = Content_7.cv_arg
    cv_content: TypeAlias = Content_7

    @property
    def content(self) -> Content_7:
        return self._get_content() # type: ignore

class Variation_9(Element):
    cv_arg: TypeAlias = RuleContent_7.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_7

    @classmethod
    def create(cls, arg: "Variation_9.cv_arg") -> "Variation_9":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_9", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_7:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_7:
        return self.rule.content

class RuleVariation_9(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_9.cv_arg
    cv_variation: TypeAlias = Variation_9

    @classmethod
    def create(cls, arg : "RuleVariation_9.cv_arg") -> "RuleVariation_9":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_9", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_9:
        return self.arg # type: ignore

class NonSpare_0(NonSpare):
    cv_arg: TypeAlias = RuleVariation_9.cv_arg
    cv_name = "000"
    cv_title = "Message Type"
    cv_rule: TypeAlias = RuleVariation_9

    @classmethod
    def create(cls, arg : "NonSpare_0.cv_arg") -> "NonSpare_0":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_0", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_9:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_9:
        return self.rule.variation

class UapItem_0(UapItem):
    cv_non_spare: TypeAlias = NonSpare_0

class NonSpare_80(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "R"
    cv_title = "Raw"
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_80.cv_arg") -> "NonSpare_80":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_80", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class Content_4(ContentTable):
    cv_arg: TypeAlias = int
    cv_values = {0: "Test 0", 1: "Test 1", 2: "Test 2", 3: "Test 3"}

class RuleContent_4(RuleContentContextFree):
    cv_arg: TypeAlias = Content_4.cv_arg
    cv_content: TypeAlias = Content_4

    @property
    def content(self) -> Content_4:
        return self._get_content() # type: ignore

class Variation_8(Element):
    cv_arg: TypeAlias = RuleContent_4.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_4

    @classmethod
    def create(cls, arg: "Variation_8.cv_arg") -> "Variation_8":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_8", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_4:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_4:
        return self.rule.content

class RuleVariation_8(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_8.cv_arg
    cv_variation: TypeAlias = Variation_8

    @classmethod
    def create(cls, arg : "RuleVariation_8.cv_arg") -> "RuleVariation_8":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_8", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_8:
        return self.arg # type: ignore

class NonSpare_88(NonSpare):
    cv_arg: TypeAlias = RuleVariation_8.cv_arg
    cv_name = "T"
    cv_title = "Table"
    cv_rule: TypeAlias = RuleVariation_8

    @classmethod
    def create(cls, arg : "NonSpare_88.cv_arg") -> "NonSpare_88":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_88", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_8:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_8:
        return self.rule.variation

class Content_9(ContentString):
    cv_arg: TypeAlias = Union[int, str]
    cv_string_type: TypeAlias = StringAscii

class RuleContent_9(RuleContentContextFree):
    cv_arg: TypeAlias = Content_9.cv_arg
    cv_content: TypeAlias = Content_9

    @property
    def content(self) -> Content_9:
        return self._get_content() # type: ignore

class Variation_22(Element):
    cv_arg: TypeAlias = RuleContent_9.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 56
    cv_rule = RuleContent_9

    @classmethod
    def create(cls, arg: "Variation_22.cv_arg") -> "Variation_22":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_22", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_9:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_9:
        return self.rule.content

class RuleVariation_22(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_22.cv_arg
    cv_variation: TypeAlias = Variation_22

    @classmethod
    def create(cls, arg : "RuleVariation_22.cv_arg") -> "RuleVariation_22":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_22", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_22:
        return self.arg # type: ignore

class NonSpare_81(NonSpare):
    cv_arg: TypeAlias = RuleVariation_22.cv_arg
    cv_name = "S1"
    cv_title = "String Ascii"
    cv_rule: TypeAlias = RuleVariation_22

    @classmethod
    def create(cls, arg : "NonSpare_81.cv_arg") -> "NonSpare_81":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_81", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_22:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_22:
        return self.rule.variation

class Content_10(ContentString):
    cv_arg: TypeAlias = Union[int, str]
    cv_string_type: TypeAlias = StringICAO

class RuleContent_10(RuleContentContextFree):
    cv_arg: TypeAlias = Content_10.cv_arg
    cv_content: TypeAlias = Content_10

    @property
    def content(self) -> Content_10:
        return self._get_content() # type: ignore

class Variation_21(Element):
    cv_arg: TypeAlias = RuleContent_10.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 48
    cv_rule = RuleContent_10

    @classmethod
    def create(cls, arg: "Variation_21.cv_arg") -> "Variation_21":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_21", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_10:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_10:
        return self.rule.content

class RuleVariation_21(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_21.cv_arg
    cv_variation: TypeAlias = Variation_21

    @classmethod
    def create(cls, arg : "RuleVariation_21.cv_arg") -> "RuleVariation_21":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_21", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_21:
        return self.arg # type: ignore

class NonSpare_82(NonSpare):
    cv_arg: TypeAlias = RuleVariation_21.cv_arg
    cv_name = "S2"
    cv_title = "String ICAO"
    cv_rule: TypeAlias = RuleVariation_21

    @classmethod
    def create(cls, arg : "NonSpare_82.cv_arg") -> "NonSpare_82":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_82", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_21:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_21:
        return self.rule.variation

class Content_11(ContentString):
    cv_arg: TypeAlias = Union[int, str]
    cv_string_type: TypeAlias = StringOctal

class RuleContent_11(RuleContentContextFree):
    cv_arg: TypeAlias = Content_11.cv_arg
    cv_content: TypeAlias = Content_11

    @property
    def content(self) -> Content_11:
        return self._get_content() # type: ignore

class Variation_17(Element):
    cv_arg: TypeAlias = RuleContent_11.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 24
    cv_rule = RuleContent_11

    @classmethod
    def create(cls, arg: "Variation_17.cv_arg") -> "Variation_17":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_17", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_11:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_11:
        return self.rule.content

class RuleVariation_17(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_17.cv_arg
    cv_variation: TypeAlias = Variation_17

    @classmethod
    def create(cls, arg : "RuleVariation_17.cv_arg") -> "RuleVariation_17":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_17", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_17:
        return self.arg # type: ignore

class NonSpare_83(NonSpare):
    cv_arg: TypeAlias = RuleVariation_17.cv_arg
    cv_name = "S3"
    cv_title = "String Octal"
    cv_rule: TypeAlias = RuleVariation_17

    @classmethod
    def create(cls, arg : "NonSpare_83.cv_arg") -> "NonSpare_83":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_83", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_17:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_17:
        return self.rule.variation

class Content_13(ContentInteger):
    cv_arg: TypeAlias = int
    cv_signedness: TypeAlias = Unsigned

class RuleContent_13(RuleContentContextFree):
    cv_arg: TypeAlias = Content_13.cv_arg
    cv_content: TypeAlias = Content_13

    @property
    def content(self) -> Content_13:
        return self._get_content() # type: ignore

class Variation_11(Element):
    cv_arg: TypeAlias = RuleContent_13.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_13

    @classmethod
    def create(cls, arg: "Variation_11.cv_arg") -> "Variation_11":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_11", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_13:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_13:
        return self.rule.content

class RuleVariation_11(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_11.cv_arg
    cv_variation: TypeAlias = Variation_11

    @classmethod
    def create(cls, arg : "RuleVariation_11.cv_arg") -> "RuleVariation_11":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_11", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_11:
        return self.arg # type: ignore

class NonSpare_60(NonSpare):
    cv_arg: TypeAlias = RuleVariation_11.cv_arg
    cv_name = "I1"
    cv_title = "Unsigned Integer"
    cv_rule: TypeAlias = RuleVariation_11

    @classmethod
    def create(cls, arg : "NonSpare_60.cv_arg") -> "NonSpare_60":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_60", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_11:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_11:
        return self.rule.variation

class Content_12(ContentInteger):
    cv_arg: TypeAlias = int
    cv_signedness: TypeAlias = Signed

class RuleContent_12(RuleContentContextFree):
    cv_arg: TypeAlias = Content_12.cv_arg
    cv_content: TypeAlias = Content_12

    @property
    def content(self) -> Content_12:
        return self._get_content() # type: ignore

class Variation_10(Element):
    cv_arg: TypeAlias = RuleContent_12.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_12

    @classmethod
    def create(cls, arg: "Variation_10.cv_arg") -> "Variation_10":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_10", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_12:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_12:
        return self.rule.content

class RuleVariation_10(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_10.cv_arg
    cv_variation: TypeAlias = Variation_10

    @classmethod
    def create(cls, arg : "RuleVariation_10.cv_arg") -> "RuleVariation_10":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_10", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_10:
        return self.arg # type: ignore

class NonSpare_65(NonSpare):
    cv_arg: TypeAlias = RuleVariation_10.cv_arg
    cv_name = "I2"
    cv_title = "Signed Integer"
    cv_rule: TypeAlias = RuleVariation_10

    @classmethod
    def create(cls, arg : "NonSpare_65.cv_arg") -> "NonSpare_65":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_65", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_10:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_10:
        return self.rule.variation

class Content_15(ContentQuantity):
    cv_arg: TypeAlias = Union[int, float, Tuple[float, Literal["°"]]]
    cv_signedness: TypeAlias = Signed
    cv_lsb = 2.1457672119140625e-5
    cv_unit = "°"

    def as_quantity(self, cv_unit : Optional[Literal["°"]] = None) -> float:
        return self._as_quantity()

class RuleContent_15(RuleContentContextFree):
    cv_arg: TypeAlias = Content_15.cv_arg
    cv_content: TypeAlias = Content_15

    @property
    def content(self) -> Content_15:
        return self._get_content() # type: ignore

class Variation_19(Element):
    cv_arg: TypeAlias = RuleContent_15.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 24
    cv_rule = RuleContent_15

    @classmethod
    def create(cls, arg: "Variation_19.cv_arg") -> "Variation_19":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_19", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_15:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_15:
        return self.rule.content

class RuleVariation_19(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_19.cv_arg
    cv_variation: TypeAlias = Variation_19

    @classmethod
    def create(cls, arg : "RuleVariation_19.cv_arg") -> "RuleVariation_19":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_19", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_19:
        return self.arg # type: ignore

class NonSpare_75(NonSpare):
    cv_arg: TypeAlias = RuleVariation_19.cv_arg
    cv_name = "Q1LAT"
    cv_title = "Latitude in WGS.84 in Two's Complement Form"
    cv_rule: TypeAlias = RuleVariation_19

    @classmethod
    def create(cls, arg : "NonSpare_75.cv_arg") -> "NonSpare_75":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_75", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_19:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_19:
        return self.rule.variation

class Content_14(ContentQuantity):
    cv_arg: TypeAlias = Union[int, float, Tuple[float, Literal["°"]]]
    cv_signedness: TypeAlias = Signed
    cv_lsb = 2.1457672119140625e-5
    cv_unit = "°"

    def as_quantity(self, cv_unit : Optional[Literal["°"]] = None) -> float:
        return self._as_quantity()

class RuleContent_14(RuleContentContextFree):
    cv_arg: TypeAlias = Content_14.cv_arg
    cv_content: TypeAlias = Content_14

    @property
    def content(self) -> Content_14:
        return self._get_content() # type: ignore

class Variation_18(Element):
    cv_arg: TypeAlias = RuleContent_14.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 24
    cv_rule = RuleContent_14

    @classmethod
    def create(cls, arg: "Variation_18.cv_arg") -> "Variation_18":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_18", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_14:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_14:
        return self.rule.content

class RuleVariation_18(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_18.cv_arg
    cv_variation: TypeAlias = Variation_18

    @classmethod
    def create(cls, arg : "RuleVariation_18.cv_arg") -> "RuleVariation_18":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_18", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_18:
        return self.arg # type: ignore

class NonSpare_76(NonSpare):
    cv_arg: TypeAlias = RuleVariation_18.cv_arg
    cv_name = "Q2LON"
    cv_title = "Longitude in WGS.84 in Two's Complement Form"
    cv_rule: TypeAlias = RuleVariation_18

    @classmethod
    def create(cls, arg : "NonSpare_76.cv_arg") -> "NonSpare_76":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_76", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_18:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_18:
        return self.rule.variation

class Content_17(ContentQuantity):
    cv_arg: TypeAlias = Union[int, float, Tuple[float, Literal["kt"]]]
    cv_signedness: TypeAlias = Unsigned
    cv_lsb = 1.0
    cv_unit = "kt"

    def as_quantity(self, cv_unit : Optional[Literal["kt"]] = None) -> float:
        return self._as_quantity()

class RuleContent_17(RuleContentContextFree):
    cv_arg: TypeAlias = Content_17.cv_arg
    cv_content: TypeAlias = Content_17

    @property
    def content(self) -> Content_17:
        return self._get_content() # type: ignore

class Variation_16(Element):
    cv_arg: TypeAlias = RuleContent_17.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 16
    cv_rule = RuleContent_17

    @classmethod
    def create(cls, arg: "Variation_16.cv_arg") -> "Variation_16":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_16", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_17:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_17:
        return self.rule.content

class RuleVariation_16(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_16.cv_arg
    cv_variation: TypeAlias = Variation_16

    @classmethod
    def create(cls, arg : "RuleVariation_16.cv_arg") -> "RuleVariation_16":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_16", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_16:
        return self.arg # type: ignore

class NonSpare_77(NonSpare):
    cv_arg: TypeAlias = RuleVariation_16.cv_arg
    cv_name = "Q3"
    cv_title = "Unsigned Quantity"
    cv_rule: TypeAlias = RuleVariation_16

    @classmethod
    def create(cls, arg : "NonSpare_77.cv_arg") -> "NonSpare_77":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_77", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_16:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_16:
        return self.rule.variation

class Content_16(ContentQuantity):
    cv_arg: TypeAlias = Union[int, float, Tuple[float, Literal[""]]]
    cv_signedness: TypeAlias = Unsigned
    cv_lsb = 1.0
    cv_unit = ""

    def as_quantity(self, cv_unit : Optional[Literal[""]] = None) -> float:
        return self._as_quantity()

class RuleContent_16(RuleContentContextFree):
    cv_arg: TypeAlias = Content_16.cv_arg
    cv_content: TypeAlias = Content_16

    @property
    def content(self) -> Content_16:
        return self._get_content() # type: ignore

class Variation_12(Element):
    cv_arg: TypeAlias = RuleContent_16.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_16

    @classmethod
    def create(cls, arg: "Variation_12.cv_arg") -> "Variation_12":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_12", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_16:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_16:
        return self.rule.content

class RuleVariation_12(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_12.cv_arg
    cv_variation: TypeAlias = Variation_12

    @classmethod
    def create(cls, arg : "RuleVariation_12.cv_arg") -> "RuleVariation_12":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_12", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_12:
        return self.arg # type: ignore

class NonSpare_78(NonSpare):
    cv_arg: TypeAlias = RuleVariation_12.cv_arg
    cv_name = "Q4"
    cv_title = "Quantity No Unit"
    cv_rule: TypeAlias = RuleVariation_12

    @classmethod
    def create(cls, arg : "NonSpare_78.cv_arg") -> "NonSpare_78":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_78", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_12:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_12:
        return self.rule.variation

class Content_18(ContentQuantity):
    cv_arg: TypeAlias = Union[int, float, Tuple[float, Literal[""]]]
    cv_signedness: TypeAlias = Unsigned
    cv_lsb = -0.5
    cv_unit = ""

    def as_quantity(self, cv_unit : Optional[Literal[""]] = None) -> float:
        return self._as_quantity()

class RuleContent_18(RuleContentContextFree):
    cv_arg: TypeAlias = Content_18.cv_arg
    cv_content: TypeAlias = Content_18

    @property
    def content(self) -> Content_18:
        return self._get_content() # type: ignore

class Variation_13(Element):
    cv_arg: TypeAlias = RuleContent_18.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_18

    @classmethod
    def create(cls, arg: "Variation_13.cv_arg") -> "Variation_13":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_13", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_18:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_18:
        return self.rule.content

class RuleVariation_13(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_13.cv_arg
    cv_variation: TypeAlias = Variation_13

    @classmethod
    def create(cls, arg : "RuleVariation_13.cv_arg") -> "RuleVariation_13":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_13", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_13:
        return self.arg # type: ignore

class NonSpare_79(NonSpare):
    cv_arg: TypeAlias = RuleVariation_13.cv_arg
    cv_name = "Q5"
    cv_title = "Negative Lsb"
    cv_rule: TypeAlias = RuleVariation_13

    @classmethod
    def create(cls, arg : "NonSpare_79.cv_arg") -> "NonSpare_79":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_79", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_13:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_13:
        return self.rule.variation

class Content_24(ContentBds):
    cv_arg: TypeAlias = int
    cv_bds_type = BdsWithAddress

class RuleContent_19(RuleContentContextFree):
    cv_arg: TypeAlias = Content_24.cv_arg
    cv_content: TypeAlias = Content_24

    @property
    def content(self) -> Content_24:
        return self._get_content() # type: ignore

class Variation_25(Element):
    cv_arg: TypeAlias = RuleContent_19.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 64
    cv_rule = RuleContent_19

    @classmethod
    def create(cls, arg: "Variation_25.cv_arg") -> "Variation_25":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_25", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_19:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_24:
        return self.rule.content

class RuleVariation_25(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_25.cv_arg
    cv_variation: TypeAlias = Variation_25

    @classmethod
    def create(cls, arg : "RuleVariation_25.cv_arg") -> "RuleVariation_25":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_25", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_25:
        return self.arg # type: ignore

class NonSpare_42(NonSpare):
    cv_arg: TypeAlias = RuleVariation_25.cv_arg
    cv_name = "B1"
    cv_title = "Bds With Address"
    cv_rule: TypeAlias = RuleVariation_25

    @classmethod
    def create(cls, arg : "NonSpare_42.cv_arg") -> "NonSpare_42":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_42", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_25:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_25:
        return self.rule.variation

class Content_25(ContentBds):
    cv_arg: TypeAlias = int
    cv_bds_type = (BdsAt, None)

class RuleContent_20(RuleContentContextFree):
    cv_arg: TypeAlias = Content_25.cv_arg
    cv_content: TypeAlias = Content_25

    @property
    def content(self) -> Content_25:
        return self._get_content() # type: ignore

class Variation_23(Element):
    cv_arg: TypeAlias = RuleContent_20.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 56
    cv_rule = RuleContent_20

    @classmethod
    def create(cls, arg: "Variation_23.cv_arg") -> "Variation_23":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_23", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_20:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_25:
        return self.rule.content

class RuleVariation_23(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_23.cv_arg
    cv_variation: TypeAlias = Variation_23

    @classmethod
    def create(cls, arg : "RuleVariation_23.cv_arg") -> "RuleVariation_23":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_23", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_23:
        return self.arg # type: ignore

class NonSpare_43(NonSpare):
    cv_arg: TypeAlias = RuleVariation_23.cv_arg
    cv_name = "B2"
    cv_title = "Bds At Unknown Address"
    cv_rule: TypeAlias = RuleVariation_23

    @classmethod
    def create(cls, arg : "NonSpare_43.cv_arg") -> "NonSpare_43":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_43", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_23:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_23:
        return self.rule.variation

class Content_26(ContentBds):
    cv_arg: TypeAlias = int
    cv_bds_type = (BdsAt, 48)

class RuleContent_21(RuleContentContextFree):
    cv_arg: TypeAlias = Content_26.cv_arg
    cv_content: TypeAlias = Content_26

    @property
    def content(self) -> Content_26:
        return self._get_content() # type: ignore

class Variation_24(Element):
    cv_arg: TypeAlias = RuleContent_21.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 56
    cv_rule = RuleContent_21

    @classmethod
    def create(cls, arg: "Variation_24.cv_arg") -> "Variation_24":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_24", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_21:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_26:
        return self.rule.content

class RuleVariation_24(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_24.cv_arg
    cv_variation: TypeAlias = Variation_24

    @classmethod
    def create(cls, arg : "RuleVariation_24.cv_arg") -> "RuleVariation_24":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_24", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_24:
        return self.arg # type: ignore

class NonSpare_44(NonSpare):
    cv_arg: TypeAlias = RuleVariation_24.cv_arg
    cv_name = "B3"
    cv_title = "Bds At Known Address"
    cv_rule: TypeAlias = RuleVariation_24

    @classmethod
    def create(cls, arg : "NonSpare_44.cv_arg") -> "NonSpare_44":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_44", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_24:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_24:
        return self.rule.variation

class Variation_69(Compound):
    cv_arg = TypedDict('cv_arg', {
        "R": Optional[NonSpare_80.cv_arg],
        "T": Optional[NonSpare_88.cv_arg],
        "S1": Optional[NonSpare_81.cv_arg],
        "S2": Optional[NonSpare_82.cv_arg],
        "S3": Optional[NonSpare_83.cv_arg],
        "I1": Optional[NonSpare_60.cv_arg],
        "I2": Optional[NonSpare_65.cv_arg],
        "Q1LAT": Optional[NonSpare_75.cv_arg],
        "Q2LON": Optional[NonSpare_76.cv_arg],
        "Q3": Optional[NonSpare_77.cv_arg],
        "Q4": Optional[NonSpare_78.cv_arg],
        "Q5": Optional[NonSpare_79.cv_arg],
        "B1": Optional[NonSpare_42.cv_arg],
        "B2": Optional[NonSpare_43.cv_arg],
        "B3": Optional[NonSpare_44.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 3
    cv_items_list = [NonSpare_80, NonSpare_88, NonSpare_81, NonSpare_82, NonSpare_83, NonSpare_60, NonSpare_65, NonSpare_75, NonSpare_76, NonSpare_77, NonSpare_78, NonSpare_79, NonSpare_42, NonSpare_43, NonSpare_44]
    cv_items_dict = {"R": NonSpare_80, "T": NonSpare_88, "S1": NonSpare_81, "S2": NonSpare_82, "S3": NonSpare_83, "I1": NonSpare_60, "I2": NonSpare_65, "Q1LAT": NonSpare_75, "Q2LON": NonSpare_76, "Q3": NonSpare_77, "Q4": NonSpare_78, "Q5": NonSpare_79, "B1": NonSpare_42, "B2": NonSpare_43, "B3": NonSpare_44}

    @overload
    @classmethod
    def spec(cls, key : Literal["R"]) -> Type[NonSpare_80]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["T"]) -> Type[NonSpare_88]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["S1"]) -> Type[NonSpare_81]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["S2"]) -> Type[NonSpare_82]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["S3"]) -> Type[NonSpare_83]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_60]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[NonSpare_65]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["Q1LAT"]) -> Type[NonSpare_75]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["Q2LON"]) -> Type[NonSpare_76]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["Q3"]) -> Type[NonSpare_77]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["Q4"]) -> Type[NonSpare_78]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["Q5"]) -> Type[NonSpare_79]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["B1"]) -> Type[NonSpare_42]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["B2"]) -> Type[NonSpare_43]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["B3"]) -> Type[NonSpare_44]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["R"], Literal["T"], Literal["S1"], Literal["S2"], Literal["S3"], Literal["I1"], Literal["I2"], Literal["Q1LAT"], Literal["Q2LON"], Literal["Q3"], Literal["Q4"], Literal["Q5"], Literal["B1"], Literal["B2"], Literal["B3"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["R"]) -> Optional[NonSpare_80]:
        ...
    @overload
    def get_item(self, key : Literal["T"]) -> Optional[NonSpare_88]:
        ...
    @overload
    def get_item(self, key : Literal["S1"]) -> Optional[NonSpare_81]:
        ...
    @overload
    def get_item(self, key : Literal["S2"]) -> Optional[NonSpare_82]:
        ...
    @overload
    def get_item(self, key : Literal["S3"]) -> Optional[NonSpare_83]:
        ...
    @overload
    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_60]:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[NonSpare_65]:
        ...
    @overload
    def get_item(self, key : Literal["Q1LAT"]) -> Optional[NonSpare_75]:
        ...
    @overload
    def get_item(self, key : Literal["Q2LON"]) -> Optional[NonSpare_76]:
        ...
    @overload
    def get_item(self, key : Literal["Q3"]) -> Optional[NonSpare_77]:
        ...
    @overload
    def get_item(self, key : Literal["Q4"]) -> Optional[NonSpare_78]:
        ...
    @overload
    def get_item(self, key : Literal["Q5"]) -> Optional[NonSpare_79]:
        ...
    @overload
    def get_item(self, key : Literal["B1"]) -> Optional[NonSpare_42]:
        ...
    @overload
    def get_item(self, key : Literal["B2"]) -> Optional[NonSpare_43]:
        ...
    @overload
    def get_item(self, key : Literal["B3"]) -> Optional[NonSpare_44]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["R"], val : Optional[NonSpare_80.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["T"], val : Optional[NonSpare_88.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["S1"], val : Optional[NonSpare_81.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["S2"], val : Optional[NonSpare_82.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["S3"], val : Optional[NonSpare_83.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["I1"], val : Optional[NonSpare_60.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["I2"], val : Optional[NonSpare_65.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["Q1LAT"], val : Optional[NonSpare_75.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["Q2LON"], val : Optional[NonSpare_76.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["Q3"], val : Optional[NonSpare_77.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["Q4"], val : Optional[NonSpare_78.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["Q5"], val : Optional[NonSpare_79.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["B1"], val : Optional[NonSpare_42.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["B2"], val : Optional[NonSpare_43.cv_arg]) -> "Variation_69":
        ...
    @overload
    def set_item(self, key : Literal["B3"], val : Optional[NonSpare_44.cv_arg]) -> "Variation_69":
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["R"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["T"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["S1"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["S2"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["S3"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["I1"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["I2"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["Q1LAT"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["Q2LON"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["Q3"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["Q4"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["Q5"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["B1"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["B2"]) -> "Variation_69":
        ...
    @overload
    def del_item(self, key : Literal["B3"]) -> "Variation_69":
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Variation_69.cv_arg") -> "Variation_69":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_69", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_64(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_69.cv_arg
    cv_variation: TypeAlias = Variation_69

    @classmethod
    def create(cls, arg : "RuleVariation_64.cv_arg") -> "RuleVariation_64":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_64", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_69:
        return self.arg # type: ignore

class NonSpare_4(NonSpare):
    cv_arg: TypeAlias = RuleVariation_64.cv_arg
    cv_name = "020"
    cv_title = "Different Contents"
    cv_rule: TypeAlias = RuleVariation_64

    @classmethod
    def create(cls, arg : "NonSpare_4.cv_arg") -> "NonSpare_4":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_4", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_64:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_69:
        return self.rule.variation

class UapItem_4(UapItem):
    cv_non_spare: TypeAlias = NonSpare_4

class Content_1(ContentTable):
    cv_arg: TypeAlias = int
    cv_values = {0: "Air Speed = IAS, LSB (Bit-1) = 2^-14 NM/s", 1: "Air Speed = Mach, LSB (Bit-1) = 0.001"}

class RuleContent_1(RuleContentContextFree):
    cv_arg: TypeAlias = Content_1.cv_arg
    cv_content: TypeAlias = Content_1

    @property
    def content(self) -> Content_1:
        return self._get_content() # type: ignore

class Variation_1(Element):
    cv_arg: TypeAlias = RuleContent_1.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 1
    cv_rule = RuleContent_1

    @classmethod
    def create(cls, arg: "Variation_1.cv_arg") -> "Variation_1":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_1", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_1:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_1:
        return self.rule.content

class RuleVariation_1(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_1.cv_arg
    cv_variation: TypeAlias = Variation_1

    @classmethod
    def create(cls, arg : "RuleVariation_1.cv_arg") -> "RuleVariation_1":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_1", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_1:
        return self.arg # type: ignore

class NonSpare_74(NonSpare):
    cv_arg: TypeAlias = RuleVariation_1.cv_arg
    cv_name = "IM"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_1

    @classmethod
    def create(cls, arg : "NonSpare_74.cv_arg") -> "NonSpare_74":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_74", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_1:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_1:
        return self.rule.variation

class Item_33(Item):
    cv_arg: TypeAlias = NonSpare_74.cv_arg
    cv_non_spare: TypeAlias = NonSpare_74

    @classmethod
    def create(cls, arg : "Item_33.cv_arg") -> "Item_33":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_33", Bits]]:
        return cls._parse(bs) # type: ignore

class Content_23(ContentQuantity):
    cv_arg: TypeAlias = Union[int, float, Tuple[float, Literal["NM/s"]]]
    cv_signedness: TypeAlias = Unsigned
    cv_lsb = 6.103515625e-5
    cv_unit = "NM/s"

    def as_quantity(self, cv_unit : Optional[Literal["NM/s"]] = None) -> float:
        return self._as_quantity()

class Content_20(ContentQuantity):
    cv_arg: TypeAlias = Union[int, float, Tuple[float, Literal["Mach"]]]
    cv_signedness: TypeAlias = Unsigned
    cv_lsb = 1.0e-3
    cv_unit = "Mach"

    def as_quantity(self, cv_unit : Optional[Literal["Mach"]] = None) -> float:
        return self._as_quantity()

class RuleContent_23(RuleContentDependent):
    cv_arg: TypeAlias = Union[
        Tuple[None, Content_0.cv_arg],
        Tuple[Tuple[Literal[0]], Content_23.cv_arg],
        Tuple[Tuple[Literal[1]], Content_20.cv_arg],
    ]
    cv_depends_on = [["030", "IM"]]
    cv_default_content: TypeAlias = Content_0
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

class Variation_28(Element):
    cv_arg: TypeAlias = RuleContent_23.cv_arg
    cv_bit_offset8 = 1
    cv_bit_size = 15
    cv_rule = RuleContent_23

    @classmethod
    def create(cls, arg: "Variation_28.cv_arg") -> "Variation_28":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_28", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_23:
        return self._get_rule() # type: ignore

class RuleVariation_28(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_28.cv_arg
    cv_variation: TypeAlias = Variation_28

    @classmethod
    def create(cls, arg : "RuleVariation_28.cv_arg") -> "RuleVariation_28":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_28", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_28:
        return self.arg # type: ignore

class NonSpare_73(NonSpare):
    cv_arg: TypeAlias = RuleVariation_28.cv_arg
    cv_name = "IAS"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_28

    @classmethod
    def create(cls, arg : "NonSpare_73.cv_arg") -> "NonSpare_73":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_73", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_28:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_28:
        return self.rule.variation

class Item_32(Item):
    cv_arg: TypeAlias = NonSpare_73.cv_arg
    cv_non_spare: TypeAlias = NonSpare_73

    @classmethod
    def create(cls, arg : "Item_32.cv_arg") -> "Item_32":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_32", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_43(Group):
    cv_arg_group: TypeAlias = Tuple[Union[RuleVariation_1.cv_arg, Tuple[Literal["IM"], RuleVariation_1.cv_arg]], Union[RuleVariation_28.cv_arg, Tuple[Literal["IAS"], RuleVariation_28.cv_arg]]]
    cv_arg: TypeAlias = Union[int, "Variation_43.cv_arg_group"]
    cv_bit_offset8 = 0
    cv_bit_size = 16
    cv_items_list = [(Item_33, 1), (Item_32, 15)]
    cv_items_dict = {"IM": RuleVariation_1, "IAS": RuleVariation_28}

    @overload
    @classmethod
    def spec(cls, key : Literal["IM"]) -> Type[RuleVariation_1]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["IAS"]) -> Type[RuleVariation_28]:
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
    def create(cls, arg: "Variation_43.cv_arg") -> "Variation_43":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_43", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_39(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_43.cv_arg
    cv_variation: TypeAlias = Variation_43

    @classmethod
    def create(cls, arg : "RuleVariation_39.cv_arg") -> "RuleVariation_39":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_39", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_43:
        return self.arg # type: ignore

class NonSpare_6(NonSpare):
    cv_arg: TypeAlias = RuleVariation_39.cv_arg
    cv_name = "030"
    cv_title = "Simple Dependent Item"
    cv_rule: TypeAlias = RuleVariation_39

    @classmethod
    def create(cls, arg : "NonSpare_6.cv_arg") -> "NonSpare_6":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_6", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_39:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_43:
        return self.rule.variation

class UapItem_6(UapItem):
    cv_non_spare: TypeAlias = NonSpare_6

class Content_19(ContentQuantity):
    cv_arg: TypeAlias = Union[int, float, Tuple[float, Literal["unit1"]]]
    cv_signedness: TypeAlias = Unsigned
    cv_lsb = 0.5
    cv_unit = "unit1"

    def as_quantity(self, cv_unit : Optional[Literal["unit1"]] = None) -> float:
        return self._as_quantity()

class Content_21(ContentQuantity):
    cv_arg: TypeAlias = Union[int, float, Tuple[float, Literal["unit2"]]]
    cv_signedness: TypeAlias = Unsigned
    cv_lsb = 0.25
    cv_unit = "unit2"

    def as_quantity(self, cv_unit : Optional[Literal["unit2"]] = None) -> float:
        return self._as_quantity()

class Content_22(ContentQuantity):
    cv_arg: TypeAlias = Union[int, float, Tuple[float, Literal["unit3"]]]
    cv_signedness: TypeAlias = Unsigned
    cv_lsb = 0.125
    cv_unit = "unit3"

    def as_quantity(self, cv_unit : Optional[Literal["unit3"]] = None) -> float:
        return self._as_quantity()

class RuleContent_22(RuleContentDependent):
    cv_arg: TypeAlias = Union[
        Tuple[None, Content_0.cv_arg],
        Tuple[Tuple[Literal[1], Literal[1]], Content_19.cv_arg],
        Tuple[Tuple[Literal[1], Literal[2]], Content_21.cv_arg],
        Tuple[Tuple[Literal[2], Literal[1]], Content_22.cv_arg],
    ]
    cv_depends_on = [["010", "SAC"], ["010", "SIC"]]
    cv_default_content: TypeAlias = Content_0
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

class Variation_14(Element):
    cv_arg: TypeAlias = RuleContent_22.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_rule = RuleContent_22

    @classmethod
    def create(cls, arg: "Variation_14.cv_arg") -> "Variation_14":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_14", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_22:
        return self._get_rule() # type: ignore

class RuleVariation_14(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_14.cv_arg
    cv_variation: TypeAlias = Variation_14

    @classmethod
    def create(cls, arg : "RuleVariation_14.cv_arg") -> "RuleVariation_14":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_14", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_14:
        return self.arg # type: ignore

class NonSpare_7(NonSpare):
    cv_arg: TypeAlias = RuleVariation_14.cv_arg
    cv_name = "031"
    cv_title = "Double Dependent Item"
    cv_rule: TypeAlias = RuleVariation_14

    @classmethod
    def create(cls, arg : "NonSpare_7.cv_arg") -> "NonSpare_7":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_7", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_14:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_14:
        return self.rule.variation

class UapItem_7(UapItem):
    cv_non_spare: TypeAlias = NonSpare_7

class NonSpare_53(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "I1"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_53.cv_arg") -> "NonSpare_53":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_53", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class Variation_4(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 4
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_4.cv_arg") -> "Variation_4":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_4", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_4(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_4.cv_arg
    cv_variation: TypeAlias = Variation_4

    @classmethod
    def create(cls, arg : "RuleVariation_4.cv_arg") -> "RuleVariation_4":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_4", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_4:
        return self.arg # type: ignore

class NonSpare_89(NonSpare):
    cv_arg: TypeAlias = RuleVariation_4.cv_arg
    cv_name = "TID"
    cv_title = "Identification of Conflict Categories Definition Table"
    cv_rule: TypeAlias = RuleVariation_4

    @classmethod
    def create(cls, arg : "NonSpare_89.cv_arg") -> "NonSpare_89":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_89", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_4:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_4:
        return self.rule.variation

class Item_38(Item):
    cv_arg: TypeAlias = NonSpare_89.cv_arg
    cv_non_spare: TypeAlias = NonSpare_89

    @classmethod
    def create(cls, arg : "Item_38.cv_arg") -> "Item_38":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_38", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_33(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 4
    cv_bit_size = 3
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_33.cv_arg") -> "Variation_33":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_33", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class Content_6(ContentTable):
    cv_arg: TypeAlias = int
    cv_values = {0: "Test0", 1: "Test1", 2: "Test2"}

class RuleContent_6(RuleContentContextFree):
    cv_arg: TypeAlias = Content_6.cv_arg
    cv_content: TypeAlias = Content_6

    @property
    def content(self) -> Content_6:
        return self._get_content() # type: ignore

class Variation_34(Element):
    cv_arg: TypeAlias = RuleContent_6.cv_arg
    cv_bit_offset8 = 4
    cv_bit_size = 3
    cv_rule = RuleContent_6

    @classmethod
    def create(cls, arg: "Variation_34.cv_arg") -> "Variation_34":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_34", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_6:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_6:
        return self.rule.content

class Content_8(ContentTable):
    cv_arg: TypeAlias = int
    cv_values = {3: "Test3", 4: "Test4"}

class RuleContent_8(RuleContentContextFree):
    cv_arg: TypeAlias = Content_8.cv_arg
    cv_content: TypeAlias = Content_8

    @property
    def content(self) -> Content_8:
        return self._get_content() # type: ignore

class Variation_35(Element):
    cv_arg: TypeAlias = RuleContent_8.cv_arg
    cv_bit_offset8 = 4
    cv_bit_size = 3
    cv_rule = RuleContent_8

    @classmethod
    def create(cls, arg: "Variation_35.cv_arg") -> "Variation_35":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_35", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_8:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_8:
        return self.rule.content

class Content_5(ContentTable):
    cv_arg: TypeAlias = int
    cv_values = {0: "Test0", 1: "Test1"}

class RuleContent_5(RuleContentContextFree):
    cv_arg: TypeAlias = Content_5.cv_arg
    cv_content: TypeAlias = Content_5

    @property
    def content(self) -> Content_5:
        return self._get_content() # type: ignore

class Variation_32(Element):
    cv_arg: TypeAlias = RuleContent_5.cv_arg
    cv_bit_offset8 = 4
    cv_bit_size = 1
    cv_rule = RuleContent_5

    @classmethod
    def create(cls, arg: "Variation_32.cv_arg") -> "Variation_32":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_32", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_5:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_5:
        return self.rule.content

class RuleVariation_32(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_32.cv_arg
    cv_variation: TypeAlias = Variation_32

    @classmethod
    def create(cls, arg : "RuleVariation_32.cv_arg") -> "RuleVariation_32":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_32", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_32:
        return self.arg # type: ignore

class NonSpare_55(NonSpare):
    cv_arg: TypeAlias = RuleVariation_32.cv_arg
    cv_name = "I1"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_32

    @classmethod
    def create(cls, arg : "NonSpare_55.cv_arg") -> "NonSpare_55":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_55", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_32:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_32:
        return self.rule.variation

class Item_18(Item):
    cv_arg: TypeAlias = NonSpare_55.cv_arg
    cv_non_spare: TypeAlias = NonSpare_55

    @classmethod
    def create(cls, arg : "Item_18.cv_arg") -> "Item_18":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_18", Bits]]:
        return cls._parse(bs) # type: ignore

class Item_5(Spare):
    cv_arg: TypeAlias = int
    cv_bit_offset8 = 5
    cv_bit_size = 2

class Variation_49(Group):
    cv_arg_group: TypeAlias = Tuple[Union[RuleVariation_32.cv_arg, Tuple[Literal["I1"], RuleVariation_32.cv_arg]], int]
    cv_arg: TypeAlias = Union[int, "Variation_49.cv_arg_group"]
    cv_bit_offset8 = 4
    cv_bit_size = 3
    cv_items_list = [(Item_18, 1), (Item_5, 2)]
    cv_items_dict = {"I1": RuleVariation_32}

    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[RuleVariation_32]:
        return cls._spec(key) # type: ignore

    def get_item(self, key : Literal["I1"]) -> RuleVariation_32:
        return self._get_item(key) # type: ignore

    @classmethod
    def create(cls, arg:"Variation_49.cv_arg") -> "Variation_49":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_49", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_65(RuleVariationDependent):
    cv_arg: TypeAlias = Union[
        Variation_33.cv_arg,
        Variation_34.cv_arg,
        Variation_35.cv_arg,
        Variation_49.cv_arg,
    ]
    cv_depends_on = [["000"], ["031", "CC", "TID"]]
    cv_default_variation: TypeAlias = Variation_33
    cv_cases = [
        ([1, 1], Variation_34),
        ([1, 2], Variation_35),
        ([2, 1], Variation_49),
    ]

    @overload
    @classmethod
    def spec(cls) -> Type[Variation_33]:
        ...
    @overload
    @classmethod
    def spec(cls, key : None) -> Type[Variation_33]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Tuple[Literal[1], Literal[1]]) -> Type[Variation_34]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Tuple[Literal[1], Literal[2]]) -> Type[Variation_35]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Tuple[Literal[2], Literal[1]]) -> Type[Variation_49]:
        ...

    @classmethod
    def spec(cls, key : Optional[Any]=None) -> Any:
        return cls._spec(key)

    @overload
    def variation(self) -> Union[ValueError, Variation_33]:
        ...
    @overload
    def variation(self, key : None) -> Union[ValueError, Variation_33]:
        ...
    @overload
    def variation(self, key : Tuple[Literal[1], Literal[1]]) -> Union[ValueError, Variation_34]:
        ...
    @overload
    def variation(self, key : Tuple[Literal[1], Literal[2]]) -> Union[ValueError, Variation_35]:
        ...
    @overload
    def variation(self, key : Tuple[Literal[2], Literal[1]]) -> Union[ValueError, Variation_49]:
        ...

    def variation(self, key : Optional[Any]=None) -> Any:
        return self._variation(key)

    @classmethod
    def create(cls, arg : "RuleVariation_65.cv_arg") -> "RuleVariation_65":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_65", Bits]]:
        return cls._parse(bs) # type: ignore

class NonSpare_46(NonSpare):
    cv_arg: TypeAlias = RuleVariation_65.cv_arg
    cv_name = "CP"
    cv_title = "Conflict Properties Class"
    cv_rule: TypeAlias = RuleVariation_65

    @classmethod
    def create(cls, arg : "NonSpare_46.cv_arg") -> "NonSpare_46":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_46", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_65:
        return self.arg # type: ignore

class Item_9(Item):
    cv_arg: TypeAlias = NonSpare_46.cv_arg
    cv_non_spare: TypeAlias = NonSpare_46

    @classmethod
    def create(cls, arg : "Item_9.cv_arg") -> "Item_9":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_9", Bits]]:
        return cls._parse(bs) # type: ignore

class Content_2(ContentTable):
    cv_arg: TypeAlias = int
    cv_values = {0: "LOW", 1: "HIGH"}

class RuleContent_2(RuleContentContextFree):
    cv_arg: TypeAlias = Content_2.cv_arg
    cv_content: TypeAlias = Content_2

    @property
    def content(self) -> Content_2:
        return self._get_content() # type: ignore

class Variation_37(Element):
    cv_arg: TypeAlias = RuleContent_2.cv_arg
    cv_bit_offset8 = 7
    cv_bit_size = 1
    cv_rule = RuleContent_2

    @classmethod
    def create(cls, arg: "Variation_37.cv_arg") -> "Variation_37":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_37", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_2:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_2:
        return self.rule.content

class RuleVariation_35(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_37.cv_arg
    cv_variation: TypeAlias = Variation_37

    @classmethod
    def create(cls, arg : "RuleVariation_35.cv_arg") -> "RuleVariation_35":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_35", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_37:
        return self.arg # type: ignore

class NonSpare_47(NonSpare):
    cv_arg: TypeAlias = RuleVariation_35.cv_arg
    cv_name = "CS"
    cv_title = "Conflict Severity"
    cv_rule: TypeAlias = RuleVariation_35

    @classmethod
    def create(cls, arg : "NonSpare_47.cv_arg") -> "NonSpare_47":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_47", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_35:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_37:
        return self.rule.variation

class Item_10(Item):
    cv_arg: TypeAlias = NonSpare_47.cv_arg
    cv_non_spare: TypeAlias = NonSpare_47

    @classmethod
    def create(cls, arg : "Item_10.cv_arg") -> "Item_10":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_10", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_47(Group):
    cv_arg_group: TypeAlias = Tuple[Union[RuleVariation_4.cv_arg, Tuple[Literal["TID"], RuleVariation_4.cv_arg]], Union[RuleVariation_65.cv_arg, Tuple[Literal["CP"], RuleVariation_65.cv_arg]], Union[RuleVariation_35.cv_arg, Tuple[Literal["CS"], RuleVariation_35.cv_arg]]]
    cv_arg: TypeAlias = Union[int, "Variation_47.cv_arg_group"]
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_items_list = [(Item_38, 4), (Item_9, 3), (Item_10, 1)]
    cv_items_dict = {"TID": RuleVariation_4, "CP": RuleVariation_65, "CS": RuleVariation_35}

    @overload
    @classmethod
    def spec(cls, key : Literal["TID"]) -> Type[RuleVariation_4]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["CP"]) -> Type[RuleVariation_65]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["CS"]) -> Type[RuleVariation_35]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["TID"], Literal["CP"], Literal["CS"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["TID"]) -> RuleVariation_4:
        ...
    @overload
    def get_item(self, key : Literal["CP"]) -> RuleVariation_65:
        ...
    @overload
    def get_item(self, key : Literal["CS"]) -> RuleVariation_35:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg: "Variation_47.cv_arg") -> "Variation_47":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_47", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_43(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_47.cv_arg
    cv_variation: TypeAlias = Variation_47

    @classmethod
    def create(cls, arg : "RuleVariation_43.cv_arg") -> "RuleVariation_43":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_43", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_47:
        return self.arg # type: ignore

class NonSpare_45(NonSpare):
    cv_arg: TypeAlias = RuleVariation_43.cv_arg
    cv_name = "CC"
    cv_title = "Conflict Classification"
    cv_rule: TypeAlias = RuleVariation_43

    @classmethod
    def create(cls, arg : "NonSpare_45.cv_arg") -> "NonSpare_45":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_45", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_43:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_47:
        return self.rule.variation

class Variation_67(Compound):
    cv_arg = TypedDict('cv_arg', {
        "I1": Optional[NonSpare_53.cv_arg],
        "CC": Optional[NonSpare_45.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 1
    cv_items_list = [NonSpare_53, NonSpare_45]
    cv_items_dict = {"I1": NonSpare_53, "CC": NonSpare_45}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_53]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["CC"]) -> Type[NonSpare_45]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["CC"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_53]:
        ...
    @overload
    def get_item(self, key : Literal["CC"]) -> Optional[NonSpare_45]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["I1"], val : Optional[NonSpare_53.cv_arg]) -> "Variation_67":
        ...
    @overload
    def set_item(self, key : Literal["CC"], val : Optional[NonSpare_45.cv_arg]) -> "Variation_67":
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["I1"]) -> "Variation_67":
        ...
    @overload
    def del_item(self, key : Literal["CC"]) -> "Variation_67":
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Variation_67.cv_arg") -> "Variation_67":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_67", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_62(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_67.cv_arg
    cv_variation: TypeAlias = Variation_67

    @classmethod
    def create(cls, arg : "RuleVariation_62.cv_arg") -> "RuleVariation_62":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_62", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_67:
        return self.arg # type: ignore

class NonSpare_10(NonSpare):
    cv_arg: TypeAlias = RuleVariation_62.cv_arg
    cv_name = "032"
    cv_title = "Nested Dependent Item"
    cv_rule: TypeAlias = RuleVariation_62

    @classmethod
    def create(cls, arg : "NonSpare_10.cv_arg") -> "NonSpare_10":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_10", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_62:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_67:
        return self.rule.variation

class UapItem_10(UapItem):
    cv_non_spare: TypeAlias = NonSpare_10

class Variation_6(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 7
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_6.cv_arg") -> "Variation_6":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_6", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_6(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_6.cv_arg
    cv_variation: TypeAlias = Variation_6

    @classmethod
    def create(cls, arg : "RuleVariation_6.cv_arg") -> "RuleVariation_6":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_6", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_6:
        return self.arg # type: ignore

class NonSpare_52(NonSpare):
    cv_arg: TypeAlias = RuleVariation_6.cv_arg
    cv_name = "I1"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_6

    @classmethod
    def create(cls, arg : "NonSpare_52.cv_arg") -> "NonSpare_52":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_52", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_6:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_6:
        return self.rule.variation

class Item_15(Item):
    cv_arg: TypeAlias = NonSpare_52.cv_arg
    cv_non_spare: TypeAlias = NonSpare_52

    @classmethod
    def create(cls, arg : "Item_15.cv_arg") -> "Item_15":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_15", Bits]]:
        return cls._parse(bs) # type: ignore

class Item_7(Spare):
    cv_arg: TypeAlias = int
    cv_bit_offset8 = 7
    cv_bit_size = 2

class Variation_27(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 1
    cv_bit_size = 6
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_27.cv_arg") -> "Variation_27":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_27", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_27(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_27.cv_arg
    cv_variation: TypeAlias = Variation_27

    @classmethod
    def create(cls, arg : "RuleVariation_27.cv_arg") -> "RuleVariation_27":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_27", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_27:
        return self.arg # type: ignore

class NonSpare_63(NonSpare):
    cv_arg: TypeAlias = RuleVariation_27.cv_arg
    cv_name = "I2"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_27

    @classmethod
    def create(cls, arg : "NonSpare_63.cv_arg") -> "NonSpare_63":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_63", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_27:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_27:
        return self.rule.variation

class Item_24(Item):
    cv_arg: TypeAlias = NonSpare_63.cv_arg
    cv_non_spare: TypeAlias = NonSpare_63

    @classmethod
    def create(cls, arg : "Item_24.cv_arg") -> "Item_24":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_24", Bits]]:
        return cls._parse(bs) # type: ignore

class Item_8(Spare):
    cv_arg: TypeAlias = int
    cv_bit_offset8 = 7
    cv_bit_size = 9

class Variation_40(Group):
    cv_arg_group: TypeAlias = Tuple[Union[RuleVariation_6.cv_arg, Tuple[Literal["I1"], RuleVariation_6.cv_arg]], int, Union[RuleVariation_27.cv_arg, Tuple[Literal["I2"], RuleVariation_27.cv_arg]], int]
    cv_arg: TypeAlias = Union[int, "Variation_40.cv_arg_group"]
    cv_bit_offset8 = 0
    cv_bit_size = 24
    cv_items_list = [(Item_15, 7), (Item_7, 2), (Item_24, 6), (Item_8, 9)]
    cv_items_dict = {"I1": RuleVariation_6, "I2": RuleVariation_27}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[RuleVariation_6]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[RuleVariation_27]:
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
    def create(cls, arg: "Variation_40.cv_arg") -> "Variation_40":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_40", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_38(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_40.cv_arg
    cv_variation: TypeAlias = Variation_40

    @classmethod
    def create(cls, arg : "RuleVariation_38.cv_arg") -> "RuleVariation_38":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_38", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_40:
        return self.arg # type: ignore

class NonSpare_12(NonSpare):
    cv_arg: TypeAlias = RuleVariation_38.cv_arg
    cv_name = "040"
    cv_title = "Spare Items"
    cv_rule: TypeAlias = RuleVariation_38

    @classmethod
    def create(cls, arg : "NonSpare_12.cv_arg") -> "NonSpare_12":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_12", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_38:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_40:
        return self.rule.variation

class UapItem_12(UapItem):
    cv_non_spare: TypeAlias = NonSpare_12

class NonSpare_15(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "051"
    cv_title = "Element"
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_15.cv_arg") -> "NonSpare_15":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_15", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_15(UapItem):
    cv_non_spare: TypeAlias = NonSpare_15

class Variation_5(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 6
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_5.cv_arg") -> "Variation_5":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_5", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_5(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_5.cv_arg
    cv_variation: TypeAlias = Variation_5

    @classmethod
    def create(cls, arg : "RuleVariation_5.cv_arg") -> "RuleVariation_5":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_5", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_5:
        return self.arg # type: ignore

class NonSpare_51(NonSpare):
    cv_arg: TypeAlias = RuleVariation_5.cv_arg
    cv_name = "I1"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_5

    @classmethod
    def create(cls, arg : "NonSpare_51.cv_arg") -> "NonSpare_51":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_51", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_5:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_5:
        return self.rule.variation

class Item_14(Item):
    cv_arg: TypeAlias = NonSpare_51.cv_arg
    cv_non_spare: TypeAlias = NonSpare_51

    @classmethod
    def create(cls, arg : "Item_14.cv_arg") -> "Item_14":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_14", Bits]]:
        return cls._parse(bs) # type: ignore

class Item_6(Spare):
    cv_arg: TypeAlias = int
    cv_bit_offset8 = 6
    cv_bit_size = 2

class NonSpare_62(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "I2"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_62.cv_arg") -> "NonSpare_62":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_62", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class Item_23(Item):
    cv_arg: TypeAlias = NonSpare_62.cv_arg
    cv_non_spare: TypeAlias = NonSpare_62

    @classmethod
    def create(cls, arg : "Item_23.cv_arg") -> "Item_23":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_23", Bits]]:
        return cls._parse(bs) # type: ignore

class NonSpare_67(NonSpare):
    cv_arg: TypeAlias = RuleVariation_4.cv_arg
    cv_name = "I3"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_4

    @classmethod
    def create(cls, arg : "NonSpare_67.cv_arg") -> "NonSpare_67":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_67", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_4:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_4:
        return self.rule.variation

class Item_27(Item):
    cv_arg: TypeAlias = NonSpare_67.cv_arg
    cv_non_spare: TypeAlias = NonSpare_67

    @classmethod
    def create(cls, arg : "Item_27.cv_arg") -> "Item_27":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_27", Bits]]:
        return cls._parse(bs) # type: ignore

class Item_4(Spare):
    cv_arg: TypeAlias = int
    cv_bit_offset8 = 4
    cv_bit_size = 8

class Variation_36(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 4
    cv_bit_size = 4
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_36.cv_arg") -> "Variation_36":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_36", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_34(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_36.cv_arg
    cv_variation: TypeAlias = Variation_36

    @classmethod
    def create(cls, arg : "RuleVariation_34.cv_arg") -> "RuleVariation_34":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_34", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_36:
        return self.arg # type: ignore

class NonSpare_70(NonSpare):
    cv_arg: TypeAlias = RuleVariation_34.cv_arg
    cv_name = "I4"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_34

    @classmethod
    def create(cls, arg : "NonSpare_70.cv_arg") -> "NonSpare_70":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_70", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_34:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_36:
        return self.rule.variation

class Item_29(Item):
    cv_arg: TypeAlias = NonSpare_70.cv_arg
    cv_non_spare: TypeAlias = NonSpare_70

    @classmethod
    def create(cls, arg : "Item_29.cv_arg") -> "Item_29":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_29", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_39(Group):
    cv_arg_group: TypeAlias = Tuple[Union[RuleVariation_5.cv_arg, Tuple[Literal["I1"], RuleVariation_5.cv_arg]], int, Union[RuleVariation_7.cv_arg, Tuple[Literal["I2"], RuleVariation_7.cv_arg]], Union[RuleVariation_4.cv_arg, Tuple[Literal["I3"], RuleVariation_4.cv_arg]], int, Union[RuleVariation_34.cv_arg, Tuple[Literal["I4"], RuleVariation_34.cv_arg]]]
    cv_arg: TypeAlias = Union[int, "Variation_39.cv_arg_group"]
    cv_bit_offset8 = 0
    cv_bit_size = 32
    cv_items_list = [(Item_14, 6), (Item_6, 2), (Item_23, 8), (Item_27, 4), (Item_4, 8), (Item_29, 4)]
    cv_items_dict = {"I1": RuleVariation_5, "I2": RuleVariation_7, "I3": RuleVariation_4, "I4": RuleVariation_34}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[RuleVariation_5]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[RuleVariation_7]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I3"]) -> Type[RuleVariation_4]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I4"]) -> Type[RuleVariation_34]:
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
    def get_item(self, key : Literal["I4"]) -> RuleVariation_34:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg: "Variation_39.cv_arg") -> "Variation_39":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_39", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_37(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_39.cv_arg
    cv_variation: TypeAlias = Variation_39

    @classmethod
    def create(cls, arg : "RuleVariation_37.cv_arg") -> "RuleVariation_37":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_37", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_39:
        return self.arg # type: ignore

class NonSpare_16(NonSpare):
    cv_arg: TypeAlias = RuleVariation_37.cv_arg
    cv_name = "052"
    cv_title = "Group"
    cv_rule: TypeAlias = RuleVariation_37

    @classmethod
    def create(cls, arg : "NonSpare_16.cv_arg") -> "NonSpare_16":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_16", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_37:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_39:
        return self.rule.variation

class UapItem_16(UapItem):
    cv_non_spare: TypeAlias = NonSpare_16

class Variation_0(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 1
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_0.cv_arg") -> "Variation_0":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_0", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_0(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_0.cv_arg
    cv_variation: TypeAlias = Variation_0

    @classmethod
    def create(cls, arg : "RuleVariation_0.cv_arg") -> "RuleVariation_0":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_0", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_0:
        return self.arg # type: ignore

class Variation_3(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 2
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_3.cv_arg") -> "Variation_3":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_3", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_3(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_3.cv_arg
    cv_variation: TypeAlias = Variation_3

    @classmethod
    def create(cls, arg : "RuleVariation_3.cv_arg") -> "RuleVariation_3":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_3", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_3:
        return self.arg # type: ignore

class Variation_31(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 3
    cv_bit_size = 4
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_31.cv_arg") -> "Variation_31":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_31", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_31(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_31.cv_arg
    cv_variation: TypeAlias = Variation_31

    @classmethod
    def create(cls, arg : "RuleVariation_31.cv_arg") -> "RuleVariation_31":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_31", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_31:
        return self.arg # type: ignore

class NonSpare_50(NonSpare):
    cv_arg: TypeAlias = RuleVariation_0.cv_arg
    cv_name = "I1"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_0

    @classmethod
    def create(cls, arg : "NonSpare_50.cv_arg") -> "NonSpare_50":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_50", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_0:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_0:
        return self.rule.variation

class Item_13(Item):
    cv_arg: TypeAlias = NonSpare_50.cv_arg
    cv_non_spare: TypeAlias = NonSpare_50

    @classmethod
    def create(cls, arg : "Item_13.cv_arg") -> "Item_13":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_13", Bits]]:
        return cls._parse(bs) # type: ignore

class NonSpare_66(NonSpare):
    cv_arg: TypeAlias = RuleVariation_3.cv_arg
    cv_name = "I3"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_3

    @classmethod
    def create(cls, arg : "NonSpare_66.cv_arg") -> "NonSpare_66":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_66", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_3:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_3:
        return self.rule.variation

class Item_26(Item):
    cv_arg: TypeAlias = NonSpare_66.cv_arg
    cv_non_spare: TypeAlias = NonSpare_66

    @classmethod
    def create(cls, arg : "Item_26.cv_arg") -> "Item_26":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_26", Bits]]:
        return cls._parse(bs) # type: ignore

class Item_1(Spare):
    cv_arg: TypeAlias = int
    cv_bit_offset8 = 2
    cv_bit_size = 1

class NonSpare_69(NonSpare):
    cv_arg: TypeAlias = RuleVariation_31.cv_arg
    cv_name = "I4"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_31

    @classmethod
    def create(cls, arg : "NonSpare_69.cv_arg") -> "NonSpare_69":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_69", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_31:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_31:
        return self.rule.variation

class Item_28(Item):
    cv_arg: TypeAlias = NonSpare_69.cv_arg
    cv_non_spare: TypeAlias = NonSpare_69

    @classmethod
    def create(cls, arg : "Item_28.cv_arg") -> "Item_28":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_28", Bits]]:
        return cls._parse(bs) # type: ignore

class NonSpare_71(NonSpare):
    cv_arg: TypeAlias = RuleVariation_6.cv_arg
    cv_name = "I5"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_6

    @classmethod
    def create(cls, arg : "NonSpare_71.cv_arg") -> "NonSpare_71":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_71", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_6:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_6:
        return self.rule.variation

class Item_30(Item):
    cv_arg: TypeAlias = NonSpare_71.cv_arg
    cv_non_spare: TypeAlias = NonSpare_71

    @classmethod
    def create(cls, arg : "Item_30.cv_arg") -> "Item_30":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_30", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_52(Extended):
    cv_arg_group_1: TypeAlias = Union[int, Tuple[Union[RuleVariation_0.cv_arg, Tuple[Literal["I1"], RuleVariation_0.cv_arg]], Union[RuleVariation_27.cv_arg, Tuple[Literal["I2"], RuleVariation_27.cv_arg]], None]]
    cv_arg_group_2: TypeAlias = Union[int, Tuple[Union[RuleVariation_3.cv_arg, Tuple[Literal["I3"], RuleVariation_3.cv_arg]], int, Union[RuleVariation_31.cv_arg, Tuple[Literal["I4"], RuleVariation_31.cv_arg]], None]]
    cv_arg_group_3: TypeAlias = Union[int, Tuple[Union[RuleVariation_6.cv_arg, Tuple[Literal["I5"], RuleVariation_6.cv_arg]], None]]
    cv_arg: TypeAlias = Union[
        Tuple["Variation_52.cv_arg_group_1"],
        Tuple["Variation_52.cv_arg_group_1", "Variation_52.cv_arg_group_2"],
        Tuple["Variation_52.cv_arg_group_1", "Variation_52.cv_arg_group_2", "Variation_52.cv_arg_group_3"],
    ]
    cv_items_list = [[(Item_13, 1), (Item_24, 6), None], [(Item_26, 2), (Item_1, 1), (Item_28, 4), None], [(Item_30, 7), None]]
    cv_items_dict = {"I1": NonSpare_50, "I2": NonSpare_63, "I3": NonSpare_66, "I4": NonSpare_69, "I5": NonSpare_71}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_50]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[NonSpare_63]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I3"]) -> Type[NonSpare_66]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I4"]) -> Type[NonSpare_69]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I5"]) -> Type[NonSpare_71]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"], Literal["I3"], Literal["I4"], Literal["I5"]]) -> Any:
        return cls._spec(key)

    @classmethod
    def create(cls, arg: "Variation_52.cv_arg") -> "Variation_52":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_52", Bits]]:
        return cls._parse(bs) # type: ignore

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

    @overload
    def modify_subitem_if_present(self, key : Literal["I1"], f: Callable[[RuleVariation_0], RuleVariation_0]) -> "Variation_52":
        ...
    @overload
    def modify_subitem_if_present(self, key : Literal["I2"], f: Callable[[RuleVariation_27], RuleVariation_27]) -> "Variation_52":
        ...
    @overload
    def modify_subitem_if_present(self, key : Literal["I3"], f: Callable[[RuleVariation_3], RuleVariation_3]) -> "Variation_52":
        ...
    @overload
    def modify_subitem_if_present(self, key : Literal["I4"], f: Callable[[RuleVariation_31], RuleVariation_31]) -> "Variation_52":
        ...
    @overload
    def modify_subitem_if_present(self, key : Literal["I5"], f: Callable[[RuleVariation_6], RuleVariation_6]) -> "Variation_52":
        ...
    def modify_subitem_if_present(self, key : Any, f : Any) -> Any:
        return self._modify_subitem_if_present(key, f)

class RuleVariation_47(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_52.cv_arg
    cv_variation: TypeAlias = Variation_52

    @classmethod
    def create(cls, arg : "RuleVariation_47.cv_arg") -> "RuleVariation_47":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_47", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_52:
        return self.arg # type: ignore

class NonSpare_17(NonSpare):
    cv_arg: TypeAlias = RuleVariation_47.cv_arg
    cv_name = "053"
    cv_title = "Extended With Trailing Fx"
    cv_rule: TypeAlias = RuleVariation_47

    @classmethod
    def create(cls, arg : "NonSpare_17.cv_arg") -> "NonSpare_17":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_17", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_47:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_52:
        return self.rule.variation

class UapItem_17(UapItem):
    cv_non_spare: TypeAlias = NonSpare_17

class NonSpare_72(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "I5"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_72.cv_arg") -> "NonSpare_72":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_72", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class Item_31(Item):
    cv_arg: TypeAlias = NonSpare_72.cv_arg
    cv_non_spare: TypeAlias = NonSpare_72

    @classmethod
    def create(cls, arg : "Item_31.cv_arg") -> "Item_31":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_31", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_53(Extended):
    cv_arg_group_1: TypeAlias = Union[int, Tuple[Union[RuleVariation_0.cv_arg, Tuple[Literal["I1"], RuleVariation_0.cv_arg]], Union[RuleVariation_27.cv_arg, Tuple[Literal["I2"], RuleVariation_27.cv_arg]], None]]
    cv_arg_group_2: TypeAlias = Union[int, Tuple[Union[RuleVariation_3.cv_arg, Tuple[Literal["I3"], RuleVariation_3.cv_arg]], int, Union[RuleVariation_31.cv_arg, Tuple[Literal["I4"], RuleVariation_31.cv_arg]], None]]
    cv_arg_group_3: TypeAlias = Union[int, Tuple[Union[RuleVariation_7.cv_arg, Tuple[Literal["I5"], RuleVariation_7.cv_arg]]]]
    cv_arg: TypeAlias = Union[
        Tuple["Variation_53.cv_arg_group_1"],
        Tuple["Variation_53.cv_arg_group_1", "Variation_53.cv_arg_group_2"],
        Tuple["Variation_53.cv_arg_group_1", "Variation_53.cv_arg_group_2", "Variation_53.cv_arg_group_3"],
    ]
    cv_items_list = [[(Item_13, 1), (Item_24, 6), None], [(Item_26, 2), (Item_1, 1), (Item_28, 4), None], [(Item_31, 8)]]
    cv_items_dict = {"I1": NonSpare_50, "I2": NonSpare_63, "I3": NonSpare_66, "I4": NonSpare_69, "I5": NonSpare_72}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_50]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[NonSpare_63]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I3"]) -> Type[NonSpare_66]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I4"]) -> Type[NonSpare_69]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I5"]) -> Type[NonSpare_72]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"], Literal["I3"], Literal["I4"], Literal["I5"]]) -> Any:
        return cls._spec(key)

    @classmethod
    def create(cls, arg: "Variation_53.cv_arg") -> "Variation_53":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_53", Bits]]:
        return cls._parse(bs) # type: ignore

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

    @overload
    def modify_subitem_if_present(self, key : Literal["I1"], f: Callable[[RuleVariation_0], RuleVariation_0]) -> "Variation_53":
        ...
    @overload
    def modify_subitem_if_present(self, key : Literal["I2"], f: Callable[[RuleVariation_27], RuleVariation_27]) -> "Variation_53":
        ...
    @overload
    def modify_subitem_if_present(self, key : Literal["I3"], f: Callable[[RuleVariation_3], RuleVariation_3]) -> "Variation_53":
        ...
    @overload
    def modify_subitem_if_present(self, key : Literal["I4"], f: Callable[[RuleVariation_31], RuleVariation_31]) -> "Variation_53":
        ...
    @overload
    def modify_subitem_if_present(self, key : Literal["I5"], f: Callable[[RuleVariation_7], RuleVariation_7]) -> "Variation_53":
        ...
    def modify_subitem_if_present(self, key : Any, f : Any) -> Any:
        return self._modify_subitem_if_present(key, f)

class RuleVariation_48(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_53.cv_arg
    cv_variation: TypeAlias = Variation_53

    @classmethod
    def create(cls, arg : "RuleVariation_48.cv_arg") -> "RuleVariation_48":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_48", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_53:
        return self.arg # type: ignore

class NonSpare_18(NonSpare):
    cv_arg: TypeAlias = RuleVariation_48.cv_arg
    cv_name = "054"
    cv_title = "Extended Without Trailing Fx"
    cv_rule: TypeAlias = RuleVariation_48

    @classmethod
    def create(cls, arg : "NonSpare_18.cv_arg") -> "NonSpare_18":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_18", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_48:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_53:
        return self.rule.variation

class UapItem_18(UapItem):
    cv_non_spare: TypeAlias = NonSpare_18

class Variation_57(Repetitive):
    cv_arg: TypeAlias = List[Variation_7.cv_arg]
    cv_rep_bytes = 1
    cv_variation: TypeAlias = Variation_7

    @classmethod
    def create(cls, arg: "Variation_57.cv_arg") -> "Variation_57":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_57", Bits]]:
        return cls._parse(bs) # type: ignore

    def get_list(self) -> List[Variation_7]:
        return self._get_list() # type: ignore

class RuleVariation_52(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_57.cv_arg
    cv_variation: TypeAlias = Variation_57

    @classmethod
    def create(cls, arg : "RuleVariation_52.cv_arg") -> "RuleVariation_52":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_52", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_57:
        return self.arg # type: ignore

class NonSpare_19(NonSpare):
    cv_arg: TypeAlias = RuleVariation_52.cv_arg
    cv_name = "061"
    cv_title = "Repetitive Regular"
    cv_rule: TypeAlias = RuleVariation_52

    @classmethod
    def create(cls, arg : "NonSpare_19.cv_arg") -> "NonSpare_19":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_19", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_52:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_57:
        return self.rule.variation

class UapItem_19(UapItem):
    cv_non_spare: TypeAlias = NonSpare_19

class Item_16(Item):
    cv_arg: TypeAlias = NonSpare_53.cv_arg
    cv_non_spare: TypeAlias = NonSpare_53

    @classmethod
    def create(cls, arg : "Item_16.cv_arg") -> "Item_16":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_16", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_42(Group):
    cv_arg_group: TypeAlias = Tuple[Union[RuleVariation_7.cv_arg, Tuple[Literal["I1"], RuleVariation_7.cv_arg]], Union[RuleVariation_7.cv_arg, Tuple[Literal["I2"], RuleVariation_7.cv_arg]]]
    cv_arg: TypeAlias = Union[int, "Variation_42.cv_arg_group"]
    cv_bit_offset8 = 0
    cv_bit_size = 16
    cv_items_list = [(Item_16, 8), (Item_23, 8)]
    cv_items_dict = {"I1": RuleVariation_7, "I2": RuleVariation_7}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[RuleVariation_7]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[RuleVariation_7]:
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
    def create(cls, arg: "Variation_42.cv_arg") -> "Variation_42":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_42", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_58(Repetitive):
    cv_arg: TypeAlias = List[Variation_42.cv_arg]
    cv_rep_bytes = 1
    cv_variation: TypeAlias = Variation_42

    @classmethod
    def create(cls, arg: "Variation_58.cv_arg") -> "Variation_58":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_58", Bits]]:
        return cls._parse(bs) # type: ignore

    def get_list(self) -> List[Variation_42]:
        return self._get_list() # type: ignore

class RuleVariation_53(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_58.cv_arg
    cv_variation: TypeAlias = Variation_58

    @classmethod
    def create(cls, arg : "RuleVariation_53.cv_arg") -> "RuleVariation_53":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_53", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_58:
        return self.arg # type: ignore

class NonSpare_20(NonSpare):
    cv_arg: TypeAlias = RuleVariation_53.cv_arg
    cv_name = "062"
    cv_title = "Repetitive With Group"
    cv_rule: TypeAlias = RuleVariation_53

    @classmethod
    def create(cls, arg : "NonSpare_20.cv_arg") -> "NonSpare_20":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_20", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_53:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_58:
        return self.rule.variation

class UapItem_20(UapItem):
    cv_non_spare: TypeAlias = NonSpare_20

class Variation_59(Repetitive):
    cv_arg: TypeAlias = List[Variation_6.cv_arg]
    cv_rep_bytes = None
    cv_variation: TypeAlias = Variation_6

    @classmethod
    def create(cls, arg: "Variation_59.cv_arg") -> "Variation_59":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_59", Bits]]:
        return cls._parse(bs) # type: ignore

    def get_list(self) -> List[Variation_6]:
        return self._get_list() # type: ignore

class RuleVariation_54(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_59.cv_arg
    cv_variation: TypeAlias = Variation_59

    @classmethod
    def create(cls, arg : "RuleVariation_54.cv_arg") -> "RuleVariation_54":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_54", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_59:
        return self.arg # type: ignore

class NonSpare_21(NonSpare):
    cv_arg: TypeAlias = RuleVariation_54.cv_arg
    cv_name = "063"
    cv_title = "Repetitive Fx"
    cv_rule: TypeAlias = RuleVariation_54

    @classmethod
    def create(cls, arg : "NonSpare_21.cv_arg") -> "NonSpare_21":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_21", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_54:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_59:
        return self.rule.variation

class UapItem_21(UapItem):
    cv_non_spare: TypeAlias = NonSpare_21

class NonSpare_61(NonSpare):
    cv_arg: TypeAlias = RuleVariation_6.cv_arg
    cv_name = "I2"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_6

    @classmethod
    def create(cls, arg : "NonSpare_61.cv_arg") -> "NonSpare_61":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_61", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_6:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_6:
        return self.rule.variation

class Item_22(Item):
    cv_arg: TypeAlias = NonSpare_61.cv_arg
    cv_non_spare: TypeAlias = NonSpare_61

    @classmethod
    def create(cls, arg : "Item_22.cv_arg") -> "Item_22":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_22", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_41(Group):
    cv_arg_group: TypeAlias = Tuple[Union[RuleVariation_7.cv_arg, Tuple[Literal["I1"], RuleVariation_7.cv_arg]], Union[RuleVariation_6.cv_arg, Tuple[Literal["I2"], RuleVariation_6.cv_arg]]]
    cv_arg: TypeAlias = Union[int, "Variation_41.cv_arg_group"]
    cv_bit_offset8 = 0
    cv_bit_size = 15
    cv_items_list = [(Item_16, 8), (Item_22, 7)]
    cv_items_dict = {"I1": RuleVariation_7, "I2": RuleVariation_6}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[RuleVariation_7]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[RuleVariation_6]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> RuleVariation_7:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> RuleVariation_6:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg: "Variation_41.cv_arg") -> "Variation_41":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_41", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_60(Repetitive):
    cv_arg: TypeAlias = List[Variation_41.cv_arg]
    cv_rep_bytes = None
    cv_variation: TypeAlias = Variation_41

    @classmethod
    def create(cls, arg: "Variation_60.cv_arg") -> "Variation_60":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_60", Bits]]:
        return cls._parse(bs) # type: ignore

    def get_list(self) -> List[Variation_41]:
        return self._get_list() # type: ignore

class RuleVariation_55(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_60.cv_arg
    cv_variation: TypeAlias = Variation_60

    @classmethod
    def create(cls, arg : "RuleVariation_55.cv_arg") -> "RuleVariation_55":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_55", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_60:
        return self.arg # type: ignore

class NonSpare_22(NonSpare):
    cv_arg: TypeAlias = RuleVariation_55.cv_arg
    cv_name = "064"
    cv_title = "Repetitive Fx With Group"
    cv_rule: TypeAlias = RuleVariation_55

    @classmethod
    def create(cls, arg : "NonSpare_22.cv_arg") -> "NonSpare_22":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_22", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_55:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_60:
        return self.rule.variation

class UapItem_22(UapItem):
    cv_non_spare: TypeAlias = NonSpare_22

class Variation_61(Explicit):
    cv_arg: TypeAlias = bytes
    cv_explicit_type: TypeAlias = None

    @classmethod
    def create(cls, arg: "Variation_61.cv_arg") -> "Variation_61":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_61", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_56(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_61.cv_arg
    cv_variation: TypeAlias = Variation_61

    @classmethod
    def create(cls, arg : "RuleVariation_56.cv_arg") -> "RuleVariation_56":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_56", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_61:
        return self.arg # type: ignore

class NonSpare_23(NonSpare):
    cv_arg: TypeAlias = RuleVariation_56.cv_arg
    cv_name = "071"
    cv_title = "Explicit None"
    cv_rule: TypeAlias = RuleVariation_56

    @classmethod
    def create(cls, arg : "NonSpare_23.cv_arg") -> "NonSpare_23":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_23", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_56:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_61:
        return self.rule.variation

class UapItem_23(UapItem):
    cv_non_spare: TypeAlias = NonSpare_23

class Variation_62(Explicit):
    cv_arg: TypeAlias = bytes
    cv_explicit_type: TypeAlias = ReservedExpansion

    @classmethod
    def create(cls, arg: "Variation_62.cv_arg") -> "Variation_62":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_62", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_57(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_62.cv_arg
    cv_variation: TypeAlias = Variation_62

    @classmethod
    def create(cls, arg : "RuleVariation_57.cv_arg") -> "RuleVariation_57":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_57", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_62:
        return self.arg # type: ignore

class NonSpare_24(NonSpare):
    cv_arg: TypeAlias = RuleVariation_57.cv_arg
    cv_name = "072"
    cv_title = "Explicit RE"
    cv_rule: TypeAlias = RuleVariation_57

    @classmethod
    def create(cls, arg : "NonSpare_24.cv_arg") -> "NonSpare_24":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_24", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_57:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_62:
        return self.rule.variation

class UapItem_24(UapItem):
    cv_non_spare: TypeAlias = NonSpare_24

class Variation_63(Explicit):
    cv_arg: TypeAlias = bytes
    cv_explicit_type: TypeAlias = SpecialPurpose

    @classmethod
    def create(cls, arg: "Variation_63.cv_arg") -> "Variation_63":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_63", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_58(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_63.cv_arg
    cv_variation: TypeAlias = Variation_63

    @classmethod
    def create(cls, arg : "RuleVariation_58.cv_arg") -> "RuleVariation_58":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_58", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_63:
        return self.arg # type: ignore

class NonSpare_25(NonSpare):
    cv_arg: TypeAlias = RuleVariation_58.cv_arg
    cv_name = "073"
    cv_title = "Explicit SP"
    cv_rule: TypeAlias = RuleVariation_58

    @classmethod
    def create(cls, arg : "NonSpare_25.cv_arg") -> "NonSpare_25":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_25", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_58:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_63:
        return self.rule.variation

class UapItem_25(UapItem):
    cv_non_spare: TypeAlias = NonSpare_25

class UapItem_42(UapItemSpare):
    pass

class UapItem_43(UapItemRFS):
    pass

class Variation_64(Compound):
    cv_arg = TypedDict('cv_arg', {
        "I1": Optional[NonSpare_53.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 1
    cv_items_list = [NonSpare_53]
    cv_items_dict = {"I1": NonSpare_53}

    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_53]:
        return cls._spec(key) # type: ignore

    def get_item(self, key : Literal["I1"]) -> Optional[Type[NonSpare_53]]:
        return self._get_item(key) # type: ignore

    def set_item(self, key : Literal["I1"], val : Optional[NonSpare_53.cv_arg]) -> "Variation_64":
        return self._set_item(key, val) # type: ignore

    def del_item(self, key : Literal["I1"]) -> "Variation_64":
        return self._del_item(key) # type: ignore

    @classmethod
    def create(cls, arg: "Variation_64.cv_arg") -> "Variation_64":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_64", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_59(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_64.cv_arg
    cv_variation: TypeAlias = Variation_64

    @classmethod
    def create(cls, arg : "RuleVariation_59.cv_arg") -> "RuleVariation_59":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_59", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_64:
        return self.arg # type: ignore

class NonSpare_26(NonSpare):
    cv_arg: TypeAlias = RuleVariation_59.cv_arg
    cv_name = "091"
    cv_title = "Compound With One Element"
    cv_rule: TypeAlias = RuleVariation_59

    @classmethod
    def create(cls, arg : "NonSpare_26.cv_arg") -> "NonSpare_26":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_26", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_59:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_64:
        return self.rule.variation

class UapItem_26(UapItem):
    cv_non_spare: TypeAlias = NonSpare_26

class Variation_65(Compound):
    cv_arg = TypedDict('cv_arg', {
        "I1": Optional[NonSpare_53.cv_arg],
        "I2": Optional[NonSpare_62.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 1
    cv_items_list = [NonSpare_53, None, NonSpare_62]
    cv_items_dict = {"I1": NonSpare_53, "I2": NonSpare_62}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_53]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[NonSpare_62]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_53]:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[NonSpare_62]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["I1"], val : Optional[NonSpare_53.cv_arg]) -> "Variation_65":
        ...
    @overload
    def set_item(self, key : Literal["I2"], val : Optional[NonSpare_62.cv_arg]) -> "Variation_65":
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["I1"]) -> "Variation_65":
        ...
    @overload
    def del_item(self, key : Literal["I2"]) -> "Variation_65":
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Variation_65.cv_arg") -> "Variation_65":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_65", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_60(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_65.cv_arg
    cv_variation: TypeAlias = Variation_65

    @classmethod
    def create(cls, arg : "RuleVariation_60.cv_arg") -> "RuleVariation_60":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_60", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_65:
        return self.arg # type: ignore

class NonSpare_27(NonSpare):
    cv_arg: TypeAlias = RuleVariation_60.cv_arg
    cv_name = "092"
    cv_title = "Compound With Two Elements"
    cv_rule: TypeAlias = RuleVariation_60

    @classmethod
    def create(cls, arg : "NonSpare_27.cv_arg") -> "NonSpare_27":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_27", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_60:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_65:
        return self.rule.variation

class UapItem_27(UapItem):
    cv_non_spare: TypeAlias = NonSpare_27

class NonSpare_68(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "I3"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_68.cv_arg") -> "NonSpare_68":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_68", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class Variation_66(Compound):
    cv_arg = TypedDict('cv_arg', {
        "I1": Optional[NonSpare_53.cv_arg],
        "I2": Optional[NonSpare_62.cv_arg],
        "I3": Optional[NonSpare_68.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 2
    cv_items_list = [NonSpare_53, None, NonSpare_62, None, None, None, None, NonSpare_68]
    cv_items_dict = {"I1": NonSpare_53, "I2": NonSpare_62, "I3": NonSpare_68}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_53]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[NonSpare_62]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I3"]) -> Type[NonSpare_68]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"], Literal["I3"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_53]:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[NonSpare_62]:
        ...
    @overload
    def get_item(self, key : Literal["I3"]) -> Optional[NonSpare_68]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["I1"], val : Optional[NonSpare_53.cv_arg]) -> "Variation_66":
        ...
    @overload
    def set_item(self, key : Literal["I2"], val : Optional[NonSpare_62.cv_arg]) -> "Variation_66":
        ...
    @overload
    def set_item(self, key : Literal["I3"], val : Optional[NonSpare_68.cv_arg]) -> "Variation_66":
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["I1"]) -> "Variation_66":
        ...
    @overload
    def del_item(self, key : Literal["I2"]) -> "Variation_66":
        ...
    @overload
    def del_item(self, key : Literal["I3"]) -> "Variation_66":
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Variation_66.cv_arg") -> "Variation_66":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_66", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_61(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_66.cv_arg
    cv_variation: TypeAlias = Variation_66

    @classmethod
    def create(cls, arg : "RuleVariation_61.cv_arg") -> "RuleVariation_61":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_61", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_66:
        return self.arg # type: ignore

class NonSpare_28(NonSpare):
    cv_arg: TypeAlias = RuleVariation_61.cv_arg
    cv_name = "093"
    cv_title = "Compound With Three Elements"
    cv_rule: TypeAlias = RuleVariation_61

    @classmethod
    def create(cls, arg : "NonSpare_28.cv_arg") -> "NonSpare_28":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_28", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_61:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_66:
        return self.rule.variation

class UapItem_28(UapItem):
    cv_non_spare: TypeAlias = NonSpare_28

class NonSpare_48(NonSpare):
    cv_arg: TypeAlias = RuleVariation_0.cv_arg
    cv_name = "EP"
    cv_title = "Element Populated Bit"
    cv_rule: TypeAlias = RuleVariation_0

    @classmethod
    def create(cls, arg : "NonSpare_48.cv_arg") -> "NonSpare_48":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_48", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_0:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_0:
        return self.rule.variation

class Item_11(Item):
    cv_arg: TypeAlias = NonSpare_48.cv_arg
    cv_non_spare: TypeAlias = NonSpare_48

    @classmethod
    def create(cls, arg : "Item_11.cv_arg") -> "Item_11":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_11", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_26(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 1
    cv_bit_size = 1
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_26.cv_arg") -> "Variation_26":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_26", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_26(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_26.cv_arg
    cv_variation: TypeAlias = Variation_26

    @classmethod
    def create(cls, arg : "RuleVariation_26.cv_arg") -> "RuleVariation_26":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_26", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_26:
        return self.arg # type: ignore

class NonSpare_91(NonSpare):
    cv_arg: TypeAlias = RuleVariation_26.cv_arg
    cv_name = "VAL"
    cv_title = "Value"
    cv_rule: TypeAlias = RuleVariation_26

    @classmethod
    def create(cls, arg : "NonSpare_91.cv_arg") -> "NonSpare_91":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_91", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_26:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_26:
        return self.rule.variation

class Item_40(Item):
    cv_arg: TypeAlias = NonSpare_91.cv_arg
    cv_non_spare: TypeAlias = NonSpare_91

    @classmethod
    def create(cls, arg : "Item_40.cv_arg") -> "Item_40":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_40", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_38(Group):
    cv_arg_group: TypeAlias = Tuple[Union[RuleVariation_0.cv_arg, Tuple[Literal["EP"], RuleVariation_0.cv_arg]], Union[RuleVariation_26.cv_arg, Tuple[Literal["VAL"], RuleVariation_26.cv_arg]]]
    cv_arg: TypeAlias = Union[int, "Variation_38.cv_arg_group"]
    cv_bit_offset8 = 0
    cv_bit_size = 2
    cv_items_list = [(Item_11, 1), (Item_40, 1)]
    cv_items_dict = {"EP": RuleVariation_0, "VAL": RuleVariation_26}

    @overload
    @classmethod
    def spec(cls, key : Literal["EP"]) -> Type[RuleVariation_0]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["VAL"]) -> Type[RuleVariation_26]:
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
    def create(cls, arg: "Variation_38.cv_arg") -> "Variation_38":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_38", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_36(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_38.cv_arg
    cv_variation: TypeAlias = Variation_38

    @classmethod
    def create(cls, arg : "RuleVariation_36.cv_arg") -> "RuleVariation_36":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_36", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_38:
        return self.arg # type: ignore

class NonSpare_85(NonSpare):
    cv_arg: TypeAlias = RuleVariation_36.cv_arg
    cv_name = "SG1"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_36

    @classmethod
    def create(cls, arg : "NonSpare_85.cv_arg") -> "NonSpare_85":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_85", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_36:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_38:
        return self.rule.variation

class Item_35(Item):
    cv_arg: TypeAlias = NonSpare_85.cv_arg
    cv_non_spare: TypeAlias = NonSpare_85

    @classmethod
    def create(cls, arg : "Item_35.cv_arg") -> "Item_35":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_35", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_29(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 2
    cv_bit_size = 1
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_29.cv_arg") -> "Variation_29":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_29", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_29(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_29.cv_arg
    cv_variation: TypeAlias = Variation_29

    @classmethod
    def create(cls, arg : "RuleVariation_29.cv_arg") -> "RuleVariation_29":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_29", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_29:
        return self.arg # type: ignore

class NonSpare_49(NonSpare):
    cv_arg: TypeAlias = RuleVariation_29.cv_arg
    cv_name = "EP"
    cv_title = "Element Populated Bit"
    cv_rule: TypeAlias = RuleVariation_29

    @classmethod
    def create(cls, arg : "NonSpare_49.cv_arg") -> "NonSpare_49":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_49", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_29:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_29:
        return self.rule.variation

class Item_12(Item):
    cv_arg: TypeAlias = NonSpare_49.cv_arg
    cv_non_spare: TypeAlias = NonSpare_49

    @classmethod
    def create(cls, arg : "Item_12.cv_arg") -> "Item_12":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_12", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_30(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 3
    cv_bit_size = 1
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_30.cv_arg") -> "Variation_30":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_30", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_30(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_30.cv_arg
    cv_variation: TypeAlias = Variation_30

    @classmethod
    def create(cls, arg : "RuleVariation_30.cv_arg") -> "RuleVariation_30":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_30", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_30:
        return self.arg # type: ignore

class NonSpare_92(NonSpare):
    cv_arg: TypeAlias = RuleVariation_30.cv_arg
    cv_name = "VAL"
    cv_title = "Value"
    cv_rule: TypeAlias = RuleVariation_30

    @classmethod
    def create(cls, arg : "NonSpare_92.cv_arg") -> "NonSpare_92":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_92", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_30:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_30:
        return self.rule.variation

class Item_41(Item):
    cv_arg: TypeAlias = NonSpare_92.cv_arg
    cv_non_spare: TypeAlias = NonSpare_92

    @classmethod
    def create(cls, arg : "Item_41.cv_arg") -> "Item_41":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_41", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_48(Group):
    cv_arg_group: TypeAlias = Tuple[Union[RuleVariation_29.cv_arg, Tuple[Literal["EP"], RuleVariation_29.cv_arg]], Union[RuleVariation_30.cv_arg, Tuple[Literal["VAL"], RuleVariation_30.cv_arg]]]
    cv_arg: TypeAlias = Union[int, "Variation_48.cv_arg_group"]
    cv_bit_offset8 = 2
    cv_bit_size = 2
    cv_items_list = [(Item_12, 1), (Item_41, 1)]
    cv_items_dict = {"EP": RuleVariation_29, "VAL": RuleVariation_30}

    @overload
    @classmethod
    def spec(cls, key : Literal["EP"]) -> Type[RuleVariation_29]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["VAL"]) -> Type[RuleVariation_30]:
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
    def create(cls, arg: "Variation_48.cv_arg") -> "Variation_48":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_48", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_44(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_48.cv_arg
    cv_variation: TypeAlias = Variation_48

    @classmethod
    def create(cls, arg : "RuleVariation_44.cv_arg") -> "RuleVariation_44":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_44", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_48:
        return self.arg # type: ignore

class NonSpare_86(NonSpare):
    cv_arg: TypeAlias = RuleVariation_44.cv_arg
    cv_name = "SG2"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_44

    @classmethod
    def create(cls, arg : "NonSpare_86.cv_arg") -> "NonSpare_86":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_86", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_44:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_48:
        return self.rule.variation

class Item_36(Item):
    cv_arg: TypeAlias = NonSpare_86.cv_arg
    cv_non_spare: TypeAlias = NonSpare_86

    @classmethod
    def create(cls, arg : "Item_36.cv_arg") -> "Item_36":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_36", Bits]]:
        return cls._parse(bs) # type: ignore

class Item_3(Spare):
    cv_arg: TypeAlias = int
    cv_bit_offset8 = 4
    cv_bit_size = 4

class Variation_46(Group):
    cv_arg_group: TypeAlias = Tuple[Union[RuleVariation_36.cv_arg, Tuple[Literal["SG1"], RuleVariation_36.cv_arg]], Union[RuleVariation_44.cv_arg, Tuple[Literal["SG2"], RuleVariation_44.cv_arg]], int]
    cv_arg: TypeAlias = Union[int, "Variation_46.cv_arg_group"]
    cv_bit_offset8 = 0
    cv_bit_size = 8
    cv_items_list = [(Item_35, 2), (Item_36, 2), (Item_3, 4)]
    cv_items_dict = {"SG1": RuleVariation_36, "SG2": RuleVariation_44}

    @overload
    @classmethod
    def spec(cls, key : Literal["SG1"]) -> Type[RuleVariation_36]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["SG2"]) -> Type[RuleVariation_44]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["SG1"], Literal["SG2"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["SG1"]) -> RuleVariation_36:
        ...
    @overload
    def get_item(self, key : Literal["SG2"]) -> RuleVariation_44:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg: "Variation_46.cv_arg") -> "Variation_46":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_46", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_42(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_46.cv_arg
    cv_variation: TypeAlias = Variation_46

    @classmethod
    def create(cls, arg : "RuleVariation_42.cv_arg") -> "RuleVariation_42":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_42", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_46:
        return self.arg # type: ignore

class NonSpare_30(NonSpare):
    cv_arg: TypeAlias = RuleVariation_42.cv_arg
    cv_name = "101"
    cv_title = "Nested Groups"
    cv_rule: TypeAlias = RuleVariation_42

    @classmethod
    def create(cls, arg : "NonSpare_30.cv_arg") -> "NonSpare_30":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_30", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_42:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_46:
        return self.rule.variation

class UapItem_30(UapItem):
    cv_non_spare: TypeAlias = NonSpare_30

class Item_2(Spare):
    cv_arg: TypeAlias = int
    cv_bit_offset8 = 4
    cv_bit_size = 3

class Variation_45(Group):
    cv_arg_group: TypeAlias = Tuple[Union[RuleVariation_36.cv_arg, Tuple[Literal["SG1"], RuleVariation_36.cv_arg]], Union[RuleVariation_44.cv_arg, Tuple[Literal["SG2"], RuleVariation_44.cv_arg]], int]
    cv_arg: TypeAlias = Union[int, "Variation_45.cv_arg_group"]
    cv_bit_offset8 = 0
    cv_bit_size = 7
    cv_items_list = [(Item_35, 2), (Item_36, 2), (Item_2, 3)]
    cv_items_dict = {"SG1": RuleVariation_36, "SG2": RuleVariation_44}

    @overload
    @classmethod
    def spec(cls, key : Literal["SG1"]) -> Type[RuleVariation_36]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["SG2"]) -> Type[RuleVariation_44]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["SG1"], Literal["SG2"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["SG1"]) -> RuleVariation_36:
        ...
    @overload
    def get_item(self, key : Literal["SG2"]) -> RuleVariation_44:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @classmethod
    def create(cls, arg: "Variation_45.cv_arg") -> "Variation_45":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_45", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_41(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_45.cv_arg
    cv_variation: TypeAlias = Variation_45

    @classmethod
    def create(cls, arg : "RuleVariation_41.cv_arg") -> "RuleVariation_41":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_41", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_45:
        return self.arg # type: ignore

class NonSpare_64(NonSpare):
    cv_arg: TypeAlias = RuleVariation_41.cv_arg
    cv_name = "I2"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_41

    @classmethod
    def create(cls, arg : "NonSpare_64.cv_arg") -> "NonSpare_64":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_64", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_41:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_45:
        return self.rule.variation

class Item_25(Item):
    cv_arg: TypeAlias = NonSpare_64.cv_arg
    cv_non_spare: TypeAlias = NonSpare_64

    @classmethod
    def create(cls, arg : "Item_25.cv_arg") -> "Item_25":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_25", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_54(Extended):
    cv_arg_group_1: TypeAlias = Union[int, Tuple[Union[RuleVariation_6.cv_arg, Tuple[Literal["I1"], RuleVariation_6.cv_arg]], None]]
    cv_arg_group_2: TypeAlias = Union[int, Tuple[Union[RuleVariation_41.cv_arg, Tuple[Literal["I2"], RuleVariation_41.cv_arg]], None]]
    cv_arg: TypeAlias = Union[
        Tuple["Variation_54.cv_arg_group_1"],
        Tuple["Variation_54.cv_arg_group_1", "Variation_54.cv_arg_group_2"],
    ]
    cv_items_list = [[(Item_15, 7), None], [(Item_25, 7), None]]
    cv_items_dict = {"I1": NonSpare_52, "I2": NonSpare_64}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_52]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[NonSpare_64]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"]]) -> Any:
        return cls._spec(key)

    @classmethod
    def create(cls, arg: "Variation_54.cv_arg") -> "Variation_54":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_54", Bits]]:
        return cls._parse(bs) # type: ignore

    @overload
    def get_item(self, key : Literal["I1"]) -> RuleVariation_6:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[RuleVariation_41]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def modify_subitem_if_present(self, key : Literal["I1"], f: Callable[[RuleVariation_6], RuleVariation_6]) -> "Variation_54":
        ...
    @overload
    def modify_subitem_if_present(self, key : Literal["I2"], f: Callable[[RuleVariation_41], RuleVariation_41]) -> "Variation_54":
        ...
    def modify_subitem_if_present(self, key : Any, f : Any) -> Any:
        return self._modify_subitem_if_present(key, f)

class RuleVariation_49(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_54.cv_arg
    cv_variation: TypeAlias = Variation_54

    @classmethod
    def create(cls, arg : "RuleVariation_49.cv_arg") -> "RuleVariation_49":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_49", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_54:
        return self.arg # type: ignore

class NonSpare_33(NonSpare):
    cv_arg: TypeAlias = RuleVariation_49.cv_arg
    cv_name = "102"
    cv_title = "Nested Groups Extended"
    cv_rule: TypeAlias = RuleVariation_49

    @classmethod
    def create(cls, arg : "NonSpare_33.cv_arg") -> "NonSpare_33":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_33", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_49:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_54:
        return self.rule.variation

class UapItem_33(UapItem):
    cv_non_spare: TypeAlias = NonSpare_33

class Record_9(Record):
    cv_arg = TypedDict('cv_arg', {
        "010": Optional[NonSpare_3.cv_arg],
        "000": Optional[NonSpare_0.cv_arg],
        "020": Optional[NonSpare_4.cv_arg],
        "030": Optional[NonSpare_6.cv_arg],
        "031": Optional[NonSpare_7.cv_arg],
        "032": Optional[NonSpare_10.cv_arg],
        "040": Optional[NonSpare_12.cv_arg],
        "051": Optional[NonSpare_15.cv_arg],
        "052": Optional[NonSpare_16.cv_arg],
        "053": Optional[NonSpare_17.cv_arg],
        "054": Optional[NonSpare_18.cv_arg],
        "061": Optional[NonSpare_19.cv_arg],
        "062": Optional[NonSpare_20.cv_arg],
        "063": Optional[NonSpare_21.cv_arg],
        "064": Optional[NonSpare_22.cv_arg],
        "071": Optional[NonSpare_23.cv_arg],
        "072": Optional[NonSpare_24.cv_arg],
        "073": Optional[NonSpare_25.cv_arg],
        "091": Optional[NonSpare_26.cv_arg],
        "092": Optional[NonSpare_27.cv_arg],
        "093": Optional[NonSpare_28.cv_arg],
        "101": Optional[NonSpare_30.cv_arg],
        "102": Optional[NonSpare_33.cv_arg],
    }, total=False)
    cv_union: TypeAlias = Union[
        Tuple[Literal["010"], NonSpare_3.cv_arg],
        Tuple[Literal["000"], NonSpare_0.cv_arg],
        Tuple[Literal["020"], NonSpare_4.cv_arg],
        Tuple[Literal["030"], NonSpare_6.cv_arg],
        Tuple[Literal["031"], NonSpare_7.cv_arg],
        Tuple[Literal["032"], NonSpare_10.cv_arg],
        Tuple[Literal["040"], NonSpare_12.cv_arg],
        Tuple[Literal["051"], NonSpare_15.cv_arg],
        Tuple[Literal["052"], NonSpare_16.cv_arg],
        Tuple[Literal["053"], NonSpare_17.cv_arg],
        Tuple[Literal["054"], NonSpare_18.cv_arg],
        Tuple[Literal["061"], NonSpare_19.cv_arg],
        Tuple[Literal["062"], NonSpare_20.cv_arg],
        Tuple[Literal["063"], NonSpare_21.cv_arg],
        Tuple[Literal["064"], NonSpare_22.cv_arg],
        Tuple[Literal["071"], NonSpare_23.cv_arg],
        Tuple[Literal["072"], NonSpare_24.cv_arg],
        Tuple[Literal["073"], NonSpare_25.cv_arg],
        Tuple[Literal["091"], NonSpare_26.cv_arg],
        Tuple[Literal["092"], NonSpare_27.cv_arg],
        Tuple[Literal["093"], NonSpare_28.cv_arg],
        Tuple[Literal["101"], NonSpare_30.cv_arg],
        Tuple[Literal["102"], NonSpare_33.cv_arg],
    ]
    cv_fspec_max_bytes = 4
    cv_items_list = [UapItem_3, UapItem_0, UapItem_4, UapItem_6, UapItem_7, UapItem_10, UapItem_12, UapItem_15, UapItem_16, UapItem_17, UapItem_18, UapItem_19, UapItem_20, UapItem_21, UapItem_22, UapItem_23, UapItem_24, UapItem_25, UapItem_42, UapItem_43, UapItem_26, UapItem_27, UapItem_28, UapItem_30, UapItem_33]
    cv_items_dict = {"010": NonSpare_3, "000": NonSpare_0, "020": NonSpare_4, "030": NonSpare_6, "031": NonSpare_7, "032": NonSpare_10, "040": NonSpare_12, "051": NonSpare_15, "052": NonSpare_16, "053": NonSpare_17, "054": NonSpare_18, "061": NonSpare_19, "062": NonSpare_20, "063": NonSpare_21, "064": NonSpare_22, "071": NonSpare_23, "072": NonSpare_24, "073": NonSpare_25, "091": NonSpare_26, "092": NonSpare_27, "093": NonSpare_28, "101": NonSpare_30, "102": NonSpare_33}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Type[NonSpare_3]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["000"]) -> Type[NonSpare_0]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["020"]) -> Type[NonSpare_4]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["030"]) -> Type[NonSpare_6]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["031"]) -> Type[NonSpare_7]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["032"]) -> Type[NonSpare_10]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["040"]) -> Type[NonSpare_12]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["051"]) -> Type[NonSpare_15]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["052"]) -> Type[NonSpare_16]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["053"]) -> Type[NonSpare_17]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["054"]) -> Type[NonSpare_18]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["061"]) -> Type[NonSpare_19]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["062"]) -> Type[NonSpare_20]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["063"]) -> Type[NonSpare_21]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["064"]) -> Type[NonSpare_22]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["071"]) -> Type[NonSpare_23]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["072"]) -> Type[NonSpare_24]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["073"]) -> Type[NonSpare_25]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["091"]) -> Type[NonSpare_26]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["092"]) -> Type[NonSpare_27]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["093"]) -> Type[NonSpare_28]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["101"]) -> Type[NonSpare_30]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["102"]) -> Type[NonSpare_33]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["000"], Literal["020"], Literal["030"], Literal["031"], Literal["032"], Literal["040"], Literal["051"], Literal["052"], Literal["053"], Literal["054"], Literal["061"], Literal["062"], Literal["063"], Literal["064"], Literal["071"], Literal["072"], Literal["073"], Literal["091"], Literal["092"], Literal["093"], Literal["101"], Literal["102"]]) -> Any:
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
    def get_item(self, key : Literal["051"]) -> Optional[NonSpare_15]:
        ...
    @overload
    def get_item(self, key : Literal["052"]) -> Optional[NonSpare_16]:
        ...
    @overload
    def get_item(self, key : Literal["053"]) -> Optional[NonSpare_17]:
        ...
    @overload
    def get_item(self, key : Literal["054"]) -> Optional[NonSpare_18]:
        ...
    @overload
    def get_item(self, key : Literal["061"]) -> Optional[NonSpare_19]:
        ...
    @overload
    def get_item(self, key : Literal["062"]) -> Optional[NonSpare_20]:
        ...
    @overload
    def get_item(self, key : Literal["063"]) -> Optional[NonSpare_21]:
        ...
    @overload
    def get_item(self, key : Literal["064"]) -> Optional[NonSpare_22]:
        ...
    @overload
    def get_item(self, key : Literal["071"]) -> Optional[NonSpare_23]:
        ...
    @overload
    def get_item(self, key : Literal["072"]) -> Optional[NonSpare_24]:
        ...
    @overload
    def get_item(self, key : Literal["073"]) -> Optional[NonSpare_25]:
        ...
    @overload
    def get_item(self, key : Literal["091"]) -> Optional[NonSpare_26]:
        ...
    @overload
    def get_item(self, key : Literal["092"]) -> Optional[NonSpare_27]:
        ...
    @overload
    def get_item(self, key : Literal["093"]) -> Optional[NonSpare_28]:
        ...
    @overload
    def get_item(self, key : Literal["101"]) -> Optional[NonSpare_30]:
        ...
    @overload
    def get_item(self, key : Literal["102"]) -> Optional[NonSpare_33]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : Optional[NonSpare_3.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["000"], val : Optional[NonSpare_0.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["020"], val : Optional[NonSpare_4.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["030"], val : Optional[NonSpare_6.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["031"], val : Optional[NonSpare_7.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["032"], val : Optional[NonSpare_10.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["040"], val : Optional[NonSpare_12.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["051"], val : Optional[NonSpare_15.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["052"], val : Optional[NonSpare_16.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["053"], val : Optional[NonSpare_17.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["054"], val : Optional[NonSpare_18.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["061"], val : Optional[NonSpare_19.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["062"], val : Optional[NonSpare_20.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["063"], val : Optional[NonSpare_21.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["064"], val : Optional[NonSpare_22.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["071"], val : Optional[NonSpare_23.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["072"], val : Optional[NonSpare_24.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["073"], val : Optional[NonSpare_25.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["091"], val : Optional[NonSpare_26.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["092"], val : Optional[NonSpare_27.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["093"], val : Optional[NonSpare_28.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["101"], val : Optional[NonSpare_30.cv_arg]) -> 'Record_9':
        ...
    @overload
    def set_item(self, key : Literal["102"], val : Optional[NonSpare_33.cv_arg]) -> 'Record_9':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["000"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["020"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["030"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["031"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["032"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["040"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["051"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["052"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["053"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["054"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["061"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["062"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["063"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["064"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["071"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["072"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["073"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["091"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["092"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["093"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["101"]) -> 'Record_9':
        ...
    @overload
    def del_item(self, key : Literal["102"]) -> 'Record_9':
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
    def get_rfs_item(self, arg : Literal["051"]) -> List[NonSpare_15]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["052"]) -> List[NonSpare_16]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["053"]) -> List[NonSpare_17]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["054"]) -> List[NonSpare_18]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["061"]) -> List[NonSpare_19]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["062"]) -> List[NonSpare_20]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["063"]) -> List[NonSpare_21]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["064"]) -> List[NonSpare_22]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["071"]) -> List[NonSpare_23]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["072"]) -> List[NonSpare_24]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["073"]) -> List[NonSpare_25]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["091"]) -> List[NonSpare_26]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["092"]) -> List[NonSpare_27]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["093"]) -> List[NonSpare_28]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["101"]) -> List[NonSpare_30]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["102"]) -> List[NonSpare_33]:
        ...
    def get_rfs_item(self, arg : Any) -> Any:
        return self._get_rfs_item(arg)

    @classmethod
    def create(cls, arg: "Record_9.cv_arg", rfs: Optional[List["Record_9.cv_union"]] = None) -> 'Record_9':
        return cls._create(arg, rfs) # type: ignore

    @classmethod
    def parse(cls, pm: ParsingMode, bs : Bits) -> Union[ValueError, Tuple["Record_9", Bits]]:
        return cls._parse(pm, bs) # type: ignore

class Uap_3(UapSingle):
    cv_arg: TypeAlias = Record_9
    cv_record: TypeAlias = Record_9

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, List[Record_9]]:
        return cls._parse(bs)

class Asterix_0(AstCat):
    cv_category = 0
    cv_edition = (1, 0)
    cv_uap: TypeAlias = Uap_3
    cv_record: TypeAlias = cv_uap.cv_record # shortcut

    @classmethod
    def create(cls, records : List[Uap_3.cv_arg]) -> "Asterix_0":
        return cls._create(records) # type: ignore

class Expansion_1(Expansion):
    cv_arg = TypedDict('cv_arg', {
        "I1": NonSpare_53.cv_arg,
        "I2": NonSpare_62.cv_arg,
    }, total=False)
    cv_type = (FspecFixed, 1)
    cv_items_list = [NonSpare_53, NonSpare_62]
    cv_items_dict = {"I1": NonSpare_53, "I2": NonSpare_62}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_53]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[NonSpare_62]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_53]:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[NonSpare_62]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["I1"], val : NonSpare_53.cv_arg) -> "Expansion_1":
        ...
    @overload
    def set_item(self, key : Literal["I2"], val : NonSpare_62.cv_arg) -> "Expansion_1":
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["I1"]) -> "Expansion_1":
        ...
    @overload
    def del_item(self, key : Literal["I2"]) -> "Expansion_1":
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Expansion_1.cv_arg") -> "Expansion_1":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, Tuple["Expansion_1", Bits]]:
        return cls._parse(bs) # type: ignore

class Asterix_1(AstRef):
    cv_category = 0
    cv_edition = (1, 0)
    cv_expansion: TypeAlias = Expansion_1

class NonSpare_37(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "200"
    cv_title = "Test"
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_37.cv_arg") -> "NonSpare_37":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_37", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_37(UapItem):
    cv_non_spare: TypeAlias = NonSpare_37

class Record_10(Record):
    cv_arg = TypedDict('cv_arg', {
        "010": Optional[NonSpare_3.cv_arg],
        "000": Optional[NonSpare_0.cv_arg],
        "020": Optional[NonSpare_4.cv_arg],
        "030": Optional[NonSpare_6.cv_arg],
        "031": Optional[NonSpare_7.cv_arg],
        "032": Optional[NonSpare_10.cv_arg],
        "040": Optional[NonSpare_12.cv_arg],
        "051": Optional[NonSpare_15.cv_arg],
        "052": Optional[NonSpare_16.cv_arg],
        "053": Optional[NonSpare_17.cv_arg],
        "054": Optional[NonSpare_18.cv_arg],
        "061": Optional[NonSpare_19.cv_arg],
        "062": Optional[NonSpare_20.cv_arg],
        "063": Optional[NonSpare_21.cv_arg],
        "071": Optional[NonSpare_23.cv_arg],
        "072": Optional[NonSpare_24.cv_arg],
        "073": Optional[NonSpare_25.cv_arg],
        "091": Optional[NonSpare_26.cv_arg],
        "092": Optional[NonSpare_27.cv_arg],
        "093": Optional[NonSpare_28.cv_arg],
        "101": Optional[NonSpare_30.cv_arg],
        "102": Optional[NonSpare_33.cv_arg],
        "200": Optional[NonSpare_37.cv_arg],
    }, total=False)
    cv_union: TypeAlias = Union[
        Tuple[Literal["010"], NonSpare_3.cv_arg],
        Tuple[Literal["000"], NonSpare_0.cv_arg],
        Tuple[Literal["020"], NonSpare_4.cv_arg],
        Tuple[Literal["030"], NonSpare_6.cv_arg],
        Tuple[Literal["031"], NonSpare_7.cv_arg],
        Tuple[Literal["032"], NonSpare_10.cv_arg],
        Tuple[Literal["040"], NonSpare_12.cv_arg],
        Tuple[Literal["051"], NonSpare_15.cv_arg],
        Tuple[Literal["052"], NonSpare_16.cv_arg],
        Tuple[Literal["053"], NonSpare_17.cv_arg],
        Tuple[Literal["054"], NonSpare_18.cv_arg],
        Tuple[Literal["061"], NonSpare_19.cv_arg],
        Tuple[Literal["062"], NonSpare_20.cv_arg],
        Tuple[Literal["063"], NonSpare_21.cv_arg],
        Tuple[Literal["071"], NonSpare_23.cv_arg],
        Tuple[Literal["072"], NonSpare_24.cv_arg],
        Tuple[Literal["073"], NonSpare_25.cv_arg],
        Tuple[Literal["091"], NonSpare_26.cv_arg],
        Tuple[Literal["092"], NonSpare_27.cv_arg],
        Tuple[Literal["093"], NonSpare_28.cv_arg],
        Tuple[Literal["101"], NonSpare_30.cv_arg],
        Tuple[Literal["102"], NonSpare_33.cv_arg],
        Tuple[Literal["200"], NonSpare_37.cv_arg],
    ]
    cv_fspec_max_bytes = 4
    cv_items_list = [UapItem_3, UapItem_0, UapItem_4, UapItem_6, UapItem_7, UapItem_10, UapItem_12, UapItem_15, UapItem_16, UapItem_17, UapItem_18, UapItem_19, UapItem_20, UapItem_21, UapItem_23, UapItem_24, UapItem_25, UapItem_42, UapItem_43, UapItem_26, UapItem_27, UapItem_28, UapItem_30, UapItem_33, UapItem_37]
    cv_items_dict = {"010": NonSpare_3, "000": NonSpare_0, "020": NonSpare_4, "030": NonSpare_6, "031": NonSpare_7, "032": NonSpare_10, "040": NonSpare_12, "051": NonSpare_15, "052": NonSpare_16, "053": NonSpare_17, "054": NonSpare_18, "061": NonSpare_19, "062": NonSpare_20, "063": NonSpare_21, "071": NonSpare_23, "072": NonSpare_24, "073": NonSpare_25, "091": NonSpare_26, "092": NonSpare_27, "093": NonSpare_28, "101": NonSpare_30, "102": NonSpare_33, "200": NonSpare_37}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Type[NonSpare_3]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["000"]) -> Type[NonSpare_0]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["020"]) -> Type[NonSpare_4]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["030"]) -> Type[NonSpare_6]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["031"]) -> Type[NonSpare_7]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["032"]) -> Type[NonSpare_10]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["040"]) -> Type[NonSpare_12]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["051"]) -> Type[NonSpare_15]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["052"]) -> Type[NonSpare_16]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["053"]) -> Type[NonSpare_17]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["054"]) -> Type[NonSpare_18]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["061"]) -> Type[NonSpare_19]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["062"]) -> Type[NonSpare_20]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["063"]) -> Type[NonSpare_21]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["071"]) -> Type[NonSpare_23]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["072"]) -> Type[NonSpare_24]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["073"]) -> Type[NonSpare_25]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["091"]) -> Type[NonSpare_26]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["092"]) -> Type[NonSpare_27]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["093"]) -> Type[NonSpare_28]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["101"]) -> Type[NonSpare_30]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["102"]) -> Type[NonSpare_33]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["200"]) -> Type[NonSpare_37]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["000"], Literal["020"], Literal["030"], Literal["031"], Literal["032"], Literal["040"], Literal["051"], Literal["052"], Literal["053"], Literal["054"], Literal["061"], Literal["062"], Literal["063"], Literal["071"], Literal["072"], Literal["073"], Literal["091"], Literal["092"], Literal["093"], Literal["101"], Literal["102"], Literal["200"]]) -> Any:
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
    def get_item(self, key : Literal["051"]) -> Optional[NonSpare_15]:
        ...
    @overload
    def get_item(self, key : Literal["052"]) -> Optional[NonSpare_16]:
        ...
    @overload
    def get_item(self, key : Literal["053"]) -> Optional[NonSpare_17]:
        ...
    @overload
    def get_item(self, key : Literal["054"]) -> Optional[NonSpare_18]:
        ...
    @overload
    def get_item(self, key : Literal["061"]) -> Optional[NonSpare_19]:
        ...
    @overload
    def get_item(self, key : Literal["062"]) -> Optional[NonSpare_20]:
        ...
    @overload
    def get_item(self, key : Literal["063"]) -> Optional[NonSpare_21]:
        ...
    @overload
    def get_item(self, key : Literal["071"]) -> Optional[NonSpare_23]:
        ...
    @overload
    def get_item(self, key : Literal["072"]) -> Optional[NonSpare_24]:
        ...
    @overload
    def get_item(self, key : Literal["073"]) -> Optional[NonSpare_25]:
        ...
    @overload
    def get_item(self, key : Literal["091"]) -> Optional[NonSpare_26]:
        ...
    @overload
    def get_item(self, key : Literal["092"]) -> Optional[NonSpare_27]:
        ...
    @overload
    def get_item(self, key : Literal["093"]) -> Optional[NonSpare_28]:
        ...
    @overload
    def get_item(self, key : Literal["101"]) -> Optional[NonSpare_30]:
        ...
    @overload
    def get_item(self, key : Literal["102"]) -> Optional[NonSpare_33]:
        ...
    @overload
    def get_item(self, key : Literal["200"]) -> Optional[NonSpare_37]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : Optional[NonSpare_3.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["000"], val : Optional[NonSpare_0.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["020"], val : Optional[NonSpare_4.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["030"], val : Optional[NonSpare_6.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["031"], val : Optional[NonSpare_7.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["032"], val : Optional[NonSpare_10.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["040"], val : Optional[NonSpare_12.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["051"], val : Optional[NonSpare_15.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["052"], val : Optional[NonSpare_16.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["053"], val : Optional[NonSpare_17.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["054"], val : Optional[NonSpare_18.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["061"], val : Optional[NonSpare_19.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["062"], val : Optional[NonSpare_20.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["063"], val : Optional[NonSpare_21.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["071"], val : Optional[NonSpare_23.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["072"], val : Optional[NonSpare_24.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["073"], val : Optional[NonSpare_25.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["091"], val : Optional[NonSpare_26.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["092"], val : Optional[NonSpare_27.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["093"], val : Optional[NonSpare_28.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["101"], val : Optional[NonSpare_30.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["102"], val : Optional[NonSpare_33.cv_arg]) -> 'Record_10':
        ...
    @overload
    def set_item(self, key : Literal["200"], val : Optional[NonSpare_37.cv_arg]) -> 'Record_10':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["000"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["020"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["030"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["031"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["032"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["040"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["051"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["052"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["053"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["054"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["061"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["062"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["063"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["071"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["072"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["073"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["091"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["092"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["093"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["101"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["102"]) -> 'Record_10':
        ...
    @overload
    def del_item(self, key : Literal["200"]) -> 'Record_10':
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
    def get_rfs_item(self, arg : Literal["051"]) -> List[NonSpare_15]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["052"]) -> List[NonSpare_16]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["053"]) -> List[NonSpare_17]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["054"]) -> List[NonSpare_18]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["061"]) -> List[NonSpare_19]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["062"]) -> List[NonSpare_20]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["063"]) -> List[NonSpare_21]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["071"]) -> List[NonSpare_23]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["072"]) -> List[NonSpare_24]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["073"]) -> List[NonSpare_25]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["091"]) -> List[NonSpare_26]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["092"]) -> List[NonSpare_27]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["093"]) -> List[NonSpare_28]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["101"]) -> List[NonSpare_30]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["102"]) -> List[NonSpare_33]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["200"]) -> List[NonSpare_37]:
        ...
    def get_rfs_item(self, arg : Any) -> Any:
        return self._get_rfs_item(arg)

    @classmethod
    def create(cls, arg: "Record_10.cv_arg", rfs: Optional[List["Record_10.cv_union"]] = None) -> 'Record_10':
        return cls._create(arg, rfs) # type: ignore

    @classmethod
    def parse(cls, pm: ParsingMode, bs : Bits) -> Union[ValueError, Tuple["Record_10", Bits]]:
        return cls._parse(pm, bs) # type: ignore

class Uap_4(UapSingle):
    cv_arg: TypeAlias = Record_10
    cv_record: TypeAlias = Record_10

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, List[Record_10]]:
        return cls._parse(bs)

class Asterix_2(AstCat):
    cv_category = 0
    cv_edition = (1, 1)
    cv_uap: TypeAlias = Uap_4
    cv_record: TypeAlias = cv_uap.cv_record # shortcut

    @classmethod
    def create(cls, records : List[Uap_4.cv_arg]) -> "Asterix_2":
        return cls._create(records) # type: ignore

class Expansion_2(Expansion):
    cv_arg = TypedDict('cv_arg', {
        "I1": NonSpare_53.cv_arg,
        "I2": NonSpare_62.cv_arg,
        "I3": NonSpare_68.cv_arg,
    }, total=False)
    cv_type = (FspecFixed, 2)
    cv_items_list = [NonSpare_53, None, None, NonSpare_62, None, None, None, None, None, None, NonSpare_68]
    cv_items_dict = {"I1": NonSpare_53, "I2": NonSpare_62, "I3": NonSpare_68}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_53]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[NonSpare_62]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I3"]) -> Type[NonSpare_68]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"], Literal["I3"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_53]:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[NonSpare_62]:
        ...
    @overload
    def get_item(self, key : Literal["I3"]) -> Optional[NonSpare_68]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["I1"], val : NonSpare_53.cv_arg) -> "Expansion_2":
        ...
    @overload
    def set_item(self, key : Literal["I2"], val : NonSpare_62.cv_arg) -> "Expansion_2":
        ...
    @overload
    def set_item(self, key : Literal["I3"], val : NonSpare_68.cv_arg) -> "Expansion_2":
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["I1"]) -> "Expansion_2":
        ...
    @overload
    def del_item(self, key : Literal["I2"]) -> "Expansion_2":
        ...
    @overload
    def del_item(self, key : Literal["I3"]) -> "Expansion_2":
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Expansion_2.cv_arg") -> "Expansion_2":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, Tuple["Expansion_2", Bits]]:
        return cls._parse(bs) # type: ignore

class Asterix_3(AstRef):
    cv_category = 0
    cv_edition = (1, 1)
    cv_expansion: TypeAlias = Expansion_2

class Expansion_0(Expansion):
    cv_arg = TypedDict('cv_arg', {
        "I1": NonSpare_53.cv_arg,
        "I2": NonSpare_62.cv_arg,
        "I3": NonSpare_68.cv_arg,
    }, total=False)
    cv_type = (FspecFx, 2)
    cv_items_list = [NonSpare_53, None, None, NonSpare_62, None, None, None, None, None, None, NonSpare_68]
    cv_items_dict = {"I1": NonSpare_53, "I2": NonSpare_62, "I3": NonSpare_68}

    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_53]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[NonSpare_62]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I3"]) -> Type[NonSpare_68]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["I1"], Literal["I2"], Literal["I3"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["I1"]) -> Optional[NonSpare_53]:
        ...
    @overload
    def get_item(self, key : Literal["I2"]) -> Optional[NonSpare_62]:
        ...
    @overload
    def get_item(self, key : Literal["I3"]) -> Optional[NonSpare_68]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["I1"], val : NonSpare_53.cv_arg) -> "Expansion_0":
        ...
    @overload
    def set_item(self, key : Literal["I2"], val : NonSpare_62.cv_arg) -> "Expansion_0":
        ...
    @overload
    def set_item(self, key : Literal["I3"], val : NonSpare_68.cv_arg) -> "Expansion_0":
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["I1"]) -> "Expansion_0":
        ...
    @overload
    def del_item(self, key : Literal["I2"]) -> "Expansion_0":
        ...
    @overload
    def del_item(self, key : Literal["I3"]) -> "Expansion_0":
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Expansion_0.cv_arg") -> "Expansion_0":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, Tuple["Expansion_0", Bits]]:
        return cls._parse(bs) # type: ignore

class Asterix_4(AstRef):
    cv_category = 0
    cv_edition = (1, 2)
    cv_expansion: TypeAlias = Expansion_0

class NonSpare_2(NonSpare):
    cv_arg: TypeAlias = RuleVariation_40.cv_arg
    cv_name = "010"
    cv_title = "Data Source Identifier"
    cv_rule: TypeAlias = RuleVariation_40

    @classmethod
    def create(cls, arg : "NonSpare_2.cv_arg") -> "NonSpare_2":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_2", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_40:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_44:
        return self.rule.variation

class UapItem_2(UapItem):
    cv_non_spare: TypeAlias = NonSpare_2

class Content_3(ContentTable):
    cv_arg: TypeAlias = int
    cv_values = {0: "Plot", 1: "Track"}

class RuleContent_3(RuleContentContextFree):
    cv_arg: TypeAlias = Content_3.cv_arg
    cv_content: TypeAlias = Content_3

    @property
    def content(self) -> Content_3:
        return self._get_content() # type: ignore

class Variation_2(Element):
    cv_arg: TypeAlias = RuleContent_3.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 1
    cv_rule = RuleContent_3

    @classmethod
    def create(cls, arg: "Variation_2.cv_arg") -> "Variation_2":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_2", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_3:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_3:
        return self.rule.content

class RuleVariation_2(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_2.cv_arg
    cv_variation: TypeAlias = Variation_2

    @classmethod
    def create(cls, arg : "RuleVariation_2.cv_arg") -> "RuleVariation_2":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_2", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_2:
        return self.arg # type: ignore

class NonSpare_90(NonSpare):
    cv_arg: TypeAlias = RuleVariation_2.cv_arg
    cv_name = "TYP"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_2

    @classmethod
    def create(cls, arg : "NonSpare_90.cv_arg") -> "NonSpare_90":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_90", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_2:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_2:
        return self.rule.variation

class Item_39(Item):
    cv_arg: TypeAlias = NonSpare_90.cv_arg
    cv_non_spare: TypeAlias = NonSpare_90

    @classmethod
    def create(cls, arg : "Item_39.cv_arg") -> "Item_39":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_39", Bits]]:
        return cls._parse(bs) # type: ignore

class NonSpare_54(NonSpare):
    cv_arg: TypeAlias = RuleVariation_27.cv_arg
    cv_name = "I1"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_27

    @classmethod
    def create(cls, arg : "NonSpare_54.cv_arg") -> "NonSpare_54":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_54", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_27:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_27:
        return self.rule.variation

class Item_17(Item):
    cv_arg: TypeAlias = NonSpare_54.cv_arg
    cv_non_spare: TypeAlias = NonSpare_54

    @classmethod
    def create(cls, arg : "Item_17.cv_arg") -> "Item_17":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_17", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_56(Extended):
    cv_arg_group_1: TypeAlias = Union[int, Tuple[Union[RuleVariation_2.cv_arg, Tuple[Literal["TYP"], RuleVariation_2.cv_arg]], Union[RuleVariation_27.cv_arg, Tuple[Literal["I1"], RuleVariation_27.cv_arg]], None]]
    cv_arg_group_2: TypeAlias = Union[int, Tuple[Union[RuleVariation_6.cv_arg, Tuple[Literal["I2"], RuleVariation_6.cv_arg]], None]]
    cv_arg: TypeAlias = Union[
        Tuple["Variation_56.cv_arg_group_1"],
        Tuple["Variation_56.cv_arg_group_1", "Variation_56.cv_arg_group_2"],
    ]
    cv_items_list = [[(Item_39, 1), (Item_17, 6), None], [(Item_22, 7), None]]
    cv_items_dict = {"TYP": NonSpare_90, "I1": NonSpare_54, "I2": NonSpare_61}

    @overload
    @classmethod
    def spec(cls, key : Literal["TYP"]) -> Type[NonSpare_90]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_54]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["I2"]) -> Type[NonSpare_61]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["TYP"], Literal["I1"], Literal["I2"]]) -> Any:
        return cls._spec(key)

    @classmethod
    def create(cls, arg: "Variation_56.cv_arg") -> "Variation_56":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_56", Bits]]:
        return cls._parse(bs) # type: ignore

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

    @overload
    def modify_subitem_if_present(self, key : Literal["TYP"], f: Callable[[RuleVariation_2], RuleVariation_2]) -> "Variation_56":
        ...
    @overload
    def modify_subitem_if_present(self, key : Literal["I1"], f: Callable[[RuleVariation_27], RuleVariation_27]) -> "Variation_56":
        ...
    @overload
    def modify_subitem_if_present(self, key : Literal["I2"], f: Callable[[RuleVariation_6], RuleVariation_6]) -> "Variation_56":
        ...
    def modify_subitem_if_present(self, key : Any, f : Any) -> Any:
        return self._modify_subitem_if_present(key, f)

class RuleVariation_51(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_56.cv_arg
    cv_variation: TypeAlias = Variation_56

    @classmethod
    def create(cls, arg : "RuleVariation_51.cv_arg") -> "RuleVariation_51":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_51", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_56:
        return self.arg # type: ignore

class NonSpare_5(NonSpare):
    cv_arg: TypeAlias = RuleVariation_51.cv_arg
    cv_name = "020"
    cv_title = "Target Report Descriptor"
    cv_rule: TypeAlias = RuleVariation_51

    @classmethod
    def create(cls, arg : "NonSpare_5.cv_arg") -> "NonSpare_5":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_5", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_51:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_56:
        return self.rule.variation

class UapItem_5(UapItem):
    cv_non_spare: TypeAlias = NonSpare_5

class NonSpare_8(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "031"
    cv_title = "For Plots Only"
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_8.cv_arg") -> "NonSpare_8":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_8", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_8(UapItem):
    cv_non_spare: TypeAlias = NonSpare_8

class NonSpare_11(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "040"
    cv_title = "Common"
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_11.cv_arg") -> "NonSpare_11":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_11", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_11(UapItem):
    cv_non_spare: TypeAlias = NonSpare_11

class NonSpare_13(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "041"
    cv_title = "For Plots Only"
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_13.cv_arg") -> "NonSpare_13":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_13", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_13(UapItem):
    cv_non_spare: TypeAlias = NonSpare_13

class Record_7(Record):
    cv_arg = TypedDict('cv_arg', {
        "010": Optional[NonSpare_2.cv_arg],
        "020": Optional[NonSpare_5.cv_arg],
        "031": Optional[NonSpare_8.cv_arg],
        "040": Optional[NonSpare_11.cv_arg],
        "041": Optional[NonSpare_13.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 1
    cv_items_list = [UapItem_2, UapItem_5, UapItem_8, UapItem_42, UapItem_11, UapItem_13]
    cv_items_dict = {"010": NonSpare_2, "020": NonSpare_5, "031": NonSpare_8, "040": NonSpare_11, "041": NonSpare_13}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Type[NonSpare_2]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["020"]) -> Type[NonSpare_5]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["031"]) -> Type[NonSpare_8]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["040"]) -> Type[NonSpare_11]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["041"]) -> Type[NonSpare_13]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["020"], Literal["031"], Literal["040"], Literal["041"]]) -> Any:
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
    @overload
    def get_item(self, key : Literal["041"]) -> Optional[NonSpare_13]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : Optional[NonSpare_2.cv_arg]) -> 'Record_7':
        ...
    @overload
    def set_item(self, key : Literal["020"], val : Optional[NonSpare_5.cv_arg]) -> 'Record_7':
        ...
    @overload
    def set_item(self, key : Literal["031"], val : Optional[NonSpare_8.cv_arg]) -> 'Record_7':
        ...
    @overload
    def set_item(self, key : Literal["040"], val : Optional[NonSpare_11.cv_arg]) -> 'Record_7':
        ...
    @overload
    def set_item(self, key : Literal["041"], val : Optional[NonSpare_13.cv_arg]) -> 'Record_7':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_7':
        ...
    @overload
    def del_item(self, key : Literal["020"]) -> 'Record_7':
        ...
    @overload
    def del_item(self, key : Literal["031"]) -> 'Record_7':
        ...
    @overload
    def del_item(self, key : Literal["040"]) -> 'Record_7':
        ...
    @overload
    def del_item(self, key : Literal["041"]) -> 'Record_7':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Record_7.cv_arg") -> 'Record_7':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, pm: ParsingMode, bs : Bits) -> Union[ValueError, Tuple["Record_7", Bits]]:
        return cls._parse(pm, bs) # type: ignore

class Variation_15(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 16
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_15.cv_arg") -> "Variation_15":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_15", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_15(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_15.cv_arg
    cv_variation: TypeAlias = Variation_15

    @classmethod
    def create(cls, arg : "RuleVariation_15.cv_arg") -> "RuleVariation_15":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_15", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_15:
        return self.arg # type: ignore

class NonSpare_9(NonSpare):
    cv_arg: TypeAlias = RuleVariation_15.cv_arg
    cv_name = "032"
    cv_title = "For Tracks Only"
    cv_rule: TypeAlias = RuleVariation_15

    @classmethod
    def create(cls, arg : "NonSpare_9.cv_arg") -> "NonSpare_9":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_9", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_15:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_15:
        return self.rule.variation

class UapItem_9(UapItem):
    cv_non_spare: TypeAlias = NonSpare_9

class NonSpare_14(NonSpare):
    cv_arg: TypeAlias = RuleVariation_15.cv_arg
    cv_name = "042"
    cv_title = "For Tracks Only"
    cv_rule: TypeAlias = RuleVariation_15

    @classmethod
    def create(cls, arg : "NonSpare_14.cv_arg") -> "NonSpare_14":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_14", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_15:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_15:
        return self.rule.variation

class UapItem_14(UapItem):
    cv_non_spare: TypeAlias = NonSpare_14

class Record_8(Record):
    cv_arg = TypedDict('cv_arg', {
        "010": Optional[NonSpare_2.cv_arg],
        "020": Optional[NonSpare_5.cv_arg],
        "032": Optional[NonSpare_9.cv_arg],
        "040": Optional[NonSpare_11.cv_arg],
        "042": Optional[NonSpare_14.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 1
    cv_items_list = [UapItem_2, UapItem_5, UapItem_42, UapItem_9, UapItem_11, UapItem_14]
    cv_items_dict = {"010": NonSpare_2, "020": NonSpare_5, "032": NonSpare_9, "040": NonSpare_11, "042": NonSpare_14}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Type[NonSpare_2]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["020"]) -> Type[NonSpare_5]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["032"]) -> Type[NonSpare_9]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["040"]) -> Type[NonSpare_11]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["042"]) -> Type[NonSpare_14]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["020"], Literal["032"], Literal["040"], Literal["042"]]) -> Any:
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
    @overload
    def get_item(self, key : Literal["042"]) -> Optional[NonSpare_14]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : Optional[NonSpare_2.cv_arg]) -> 'Record_8':
        ...
    @overload
    def set_item(self, key : Literal["020"], val : Optional[NonSpare_5.cv_arg]) -> 'Record_8':
        ...
    @overload
    def set_item(self, key : Literal["032"], val : Optional[NonSpare_9.cv_arg]) -> 'Record_8':
        ...
    @overload
    def set_item(self, key : Literal["040"], val : Optional[NonSpare_11.cv_arg]) -> 'Record_8':
        ...
    @overload
    def set_item(self, key : Literal["042"], val : Optional[NonSpare_14.cv_arg]) -> 'Record_8':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_8':
        ...
    @overload
    def del_item(self, key : Literal["020"]) -> 'Record_8':
        ...
    @overload
    def del_item(self, key : Literal["032"]) -> 'Record_8':
        ...
    @overload
    def del_item(self, key : Literal["040"]) -> 'Record_8':
        ...
    @overload
    def del_item(self, key : Literal["042"]) -> 'Record_8':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Record_8.cv_arg") -> 'Record_8':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, pm: ParsingMode, bs : Bits) -> Union[ValueError, Tuple["Record_8", Bits]]:
        return cls._parse(pm, bs) # type: ignore

class Uap_5(UapMultiple):
    cv_arg: TypeAlias = Union[Record_7, Record_8]
    cv_uaps = {"plot": Record_7, "track": Record_8}
    cv_selector = (["020", "TYP"], {0: "plot", 1: "track"})

    @overload
    @classmethod
    def spec(cls, key : Literal["plot"]) -> Type[Record_7]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["track"]) -> Type[Record_8]:
        ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls._spec(key)

    @overload
    @classmethod
    def parse(cls, uap : Literal["plot"], bs : Bits) -> Union[ValueError, List[Record_7]]:
        ...
    @overload
    @classmethod
    def parse(cls, uap : Literal["track"], bs : Bits) -> Union[ValueError, List[Record_8]]:
        ...
    @classmethod
    def parse(cls, uap : str, bs : Bits) -> Any:
        return cls._parse(uap, bs)

    @classmethod
    def parse_any_uap(cls, bs : Bits, max_depth: Optional[int]) -> "Union[ValueError, List[List[Uap_5.cv_arg]]]":
        return cls._parse_any_uap(bs, max_depth)

class Asterix_5(AstCat):
    cv_category = 1
    cv_edition = (1, 0)
    cv_uap: TypeAlias = Uap_5

    @classmethod
    def create(cls, records : List[Uap_5.cv_arg]) -> "Asterix_5":
        return cls._create(records) # type: ignore

class NonSpare_1(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "010"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_1.cv_arg") -> "NonSpare_1":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_1", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_1(UapItem):
    cv_non_spare: TypeAlias = NonSpare_1

class NonSpare_29(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "101"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_29.cv_arg") -> "NonSpare_29":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_29", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_29(UapItem):
    cv_non_spare: TypeAlias = NonSpare_29

class NonSpare_32(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "102"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_32.cv_arg") -> "NonSpare_32":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_32", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class UapItem_32(UapItem):
    cv_non_spare: TypeAlias = NonSpare_32

class Record_2(Record):
    cv_arg = TypedDict('cv_arg', {
        "010": Optional[NonSpare_1.cv_arg],
        "101": Optional[NonSpare_29.cv_arg],
        "102": Optional[NonSpare_32.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 1
    cv_items_list = [UapItem_1, UapItem_29, UapItem_32]
    cv_items_dict = {"010": NonSpare_1, "101": NonSpare_29, "102": NonSpare_32}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Type[NonSpare_1]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["101"]) -> Type[NonSpare_29]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["102"]) -> Type[NonSpare_32]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["101"], Literal["102"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["010"]) -> Optional[NonSpare_1]:
        ...
    @overload
    def get_item(self, key : Literal["101"]) -> Optional[NonSpare_29]:
        ...
    @overload
    def get_item(self, key : Literal["102"]) -> Optional[NonSpare_32]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : Optional[NonSpare_1.cv_arg]) -> 'Record_2':
        ...
    @overload
    def set_item(self, key : Literal["101"], val : Optional[NonSpare_29.cv_arg]) -> 'Record_2':
        ...
    @overload
    def set_item(self, key : Literal["102"], val : Optional[NonSpare_32.cv_arg]) -> 'Record_2':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_2':
        ...
    @overload
    def del_item(self, key : Literal["101"]) -> 'Record_2':
        ...
    @overload
    def del_item(self, key : Literal["102"]) -> 'Record_2':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Record_2.cv_arg") -> 'Record_2':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, pm: ParsingMode, bs : Bits) -> Union[ValueError, Tuple["Record_2", Bits]]:
        return cls._parse(pm, bs) # type: ignore

class NonSpare_38(NonSpare):
    cv_arg: TypeAlias = RuleVariation_15.cv_arg
    cv_name = "201"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_15

    @classmethod
    def create(cls, arg : "NonSpare_38.cv_arg") -> "NonSpare_38":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_38", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_15:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_15:
        return self.rule.variation

class UapItem_38(UapItem):
    cv_non_spare: TypeAlias = NonSpare_38

class NonSpare_39(NonSpare):
    cv_arg: TypeAlias = RuleVariation_15.cv_arg
    cv_name = "202"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_15

    @classmethod
    def create(cls, arg : "NonSpare_39.cv_arg") -> "NonSpare_39":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_39", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_15:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_15:
        return self.rule.variation

class UapItem_39(UapItem):
    cv_non_spare: TypeAlias = NonSpare_39

class Record_5(Record):
    cv_arg = TypedDict('cv_arg', {
        "010": Optional[NonSpare_1.cv_arg],
        "201": Optional[NonSpare_38.cv_arg],
        "202": Optional[NonSpare_39.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 1
    cv_items_list = [UapItem_1, UapItem_38, UapItem_39]
    cv_items_dict = {"010": NonSpare_1, "201": NonSpare_38, "202": NonSpare_39}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Type[NonSpare_1]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["201"]) -> Type[NonSpare_38]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["202"]) -> Type[NonSpare_39]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["201"], Literal["202"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["010"]) -> Optional[NonSpare_1]:
        ...
    @overload
    def get_item(self, key : Literal["201"]) -> Optional[NonSpare_38]:
        ...
    @overload
    def get_item(self, key : Literal["202"]) -> Optional[NonSpare_39]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : Optional[NonSpare_1.cv_arg]) -> 'Record_5':
        ...
    @overload
    def set_item(self, key : Literal["201"], val : Optional[NonSpare_38.cv_arg]) -> 'Record_5':
        ...
    @overload
    def set_item(self, key : Literal["202"], val : Optional[NonSpare_39.cv_arg]) -> 'Record_5':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_5':
        ...
    @overload
    def del_item(self, key : Literal["201"]) -> 'Record_5':
        ...
    @overload
    def del_item(self, key : Literal["202"]) -> 'Record_5':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Record_5.cv_arg") -> 'Record_5':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, pm: ParsingMode, bs : Bits) -> Union[ValueError, Tuple["Record_5", Bits]]:
        return cls._parse(pm, bs) # type: ignore

class Variation_20(Element):
    cv_arg: TypeAlias = RuleContent_0.cv_arg
    cv_bit_offset8 = 0
    cv_bit_size = 32
    cv_rule = RuleContent_0

    @classmethod
    def create(cls, arg: "Variation_20.cv_arg") -> "Variation_20":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_20", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleContent_0:
        return self._get_rule() # type: ignore

    # shortcut
    @property
    def content(self) -> Content_0:
        return self.rule.content

class RuleVariation_20(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_20.cv_arg
    cv_variation: TypeAlias = Variation_20

    @classmethod
    def create(cls, arg : "RuleVariation_20.cv_arg") -> "RuleVariation_20":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_20", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_20:
        return self.arg # type: ignore

class NonSpare_40(NonSpare):
    cv_arg: TypeAlias = RuleVariation_20.cv_arg
    cv_name = "301"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_20

    @classmethod
    def create(cls, arg : "NonSpare_40.cv_arg") -> "NonSpare_40":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_40", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_20:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_20:
        return self.rule.variation

class UapItem_40(UapItem):
    cv_non_spare: TypeAlias = NonSpare_40

class Record_6(Record):
    cv_arg = TypedDict('cv_arg', {
        "010": Optional[NonSpare_1.cv_arg],
        "301": Optional[NonSpare_40.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 1
    cv_items_list = [UapItem_1, UapItem_40]
    cv_items_dict = {"010": NonSpare_1, "301": NonSpare_40}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Type[NonSpare_1]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["301"]) -> Type[NonSpare_40]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["301"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["010"]) -> Optional[NonSpare_1]:
        ...
    @overload
    def get_item(self, key : Literal["301"]) -> Optional[NonSpare_40]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : Optional[NonSpare_1.cv_arg]) -> 'Record_6':
        ...
    @overload
    def set_item(self, key : Literal["301"], val : Optional[NonSpare_40.cv_arg]) -> 'Record_6':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_6':
        ...
    @overload
    def del_item(self, key : Literal["301"]) -> 'Record_6':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Record_6.cv_arg") -> 'Record_6':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, pm: ParsingMode, bs : Bits) -> Union[ValueError, Tuple["Record_6", Bits]]:
        return cls._parse(pm, bs) # type: ignore

class Record_1(Record):
    cv_arg = TypedDict('cv_arg', {
        "010": Optional[NonSpare_1.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 1
    cv_items_list = [UapItem_1]
    cv_items_dict = {"010": NonSpare_1}

    @classmethod
    def spec(cls, key : Literal["010"]) -> Type[NonSpare_1]:
        return cls._spec(key) # type: ignore

    def get_item(self, key : Literal["010"]) -> Optional[Type[NonSpare_1]]:
        return self._get_item(key) # type: ignore

    def set_item(self, key : Literal["010"], val : Optional[NonSpare_1.cv_arg]) -> 'Record_1':
        return self._set_item(key, val) # type: ignore

    def del_item(self, key : Literal["010"]) -> 'Record_1':
        return self._del_item(key) # type: ignore

    @classmethod
    def create(cls, arg: "Record_1.cv_arg") -> 'Record_1':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, pm: ParsingMode, bs : Bits) -> Union[ValueError, Tuple["Record_1", Bits]]:
        return cls._parse(pm, bs) # type: ignore

class Uap_6(UapMultiple):
    cv_arg: TypeAlias = Union[Record_2, Record_5, Record_6, Record_1]
    cv_uaps = {"uap1": Record_2, "uap2": Record_5, "uap3": Record_6, "uap4": Record_1}
    cv_selector = None

    @overload
    @classmethod
    def spec(cls, key : Literal["uap1"]) -> Type[Record_2]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["uap2"]) -> Type[Record_5]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["uap3"]) -> Type[Record_6]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["uap4"]) -> Type[Record_1]:
        ...
    @classmethod
    def spec(cls, key : Any) -> Any:
        return cls._spec(key)

    @overload
    @classmethod
    def parse(cls, uap : Literal["uap1"], bs : Bits) -> Union[ValueError, List[Record_2]]:
        ...
    @overload
    @classmethod
    def parse(cls, uap : Literal["uap2"], bs : Bits) -> Union[ValueError, List[Record_5]]:
        ...
    @overload
    @classmethod
    def parse(cls, uap : Literal["uap3"], bs : Bits) -> Union[ValueError, List[Record_6]]:
        ...
    @overload
    @classmethod
    def parse(cls, uap : Literal["uap4"], bs : Bits) -> Union[ValueError, List[Record_1]]:
        ...
    @classmethod
    def parse(cls, uap : str, bs : Bits) -> Any:
        return cls._parse(uap, bs)

    @classmethod
    def parse_any_uap(cls, bs : Bits, max_depth: Optional[int]) -> "Union[ValueError, List[List[Uap_6.cv_arg]]]":
        return cls._parse_any_uap(bs, max_depth)

class Asterix_6(AstCat):
    cv_category = 2
    cv_edition = (1, 0)
    cv_uap: TypeAlias = Uap_6

    @classmethod
    def create(cls, records : List[Uap_6.cv_arg]) -> "Asterix_6":
        return cls._create(records) # type: ignore

class NonSpare_41(NonSpare):
    cv_arg: TypeAlias = RuleVariation_20.cv_arg
    cv_name = "302"
    cv_title = ""
    cv_rule: TypeAlias = RuleVariation_20

    @classmethod
    def create(cls, arg : "NonSpare_41.cv_arg") -> "NonSpare_41":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_41", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_20:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_20:
        return self.rule.variation

class UapItem_41(UapItem):
    cv_non_spare: TypeAlias = NonSpare_41

class Record_4(Record):
    cv_arg = TypedDict('cv_arg', {
        "010": Optional[NonSpare_1.cv_arg],
        "101": Optional[NonSpare_29.cv_arg],
        "102": Optional[NonSpare_32.cv_arg],
        "201": Optional[NonSpare_38.cv_arg],
        "202": Optional[NonSpare_39.cv_arg],
        "301": Optional[NonSpare_40.cv_arg],
        "302": Optional[NonSpare_41.cv_arg],
    }, total=False)
    cv_union: TypeAlias = Union[
        Tuple[Literal["010"], NonSpare_1.cv_arg],
        Tuple[Literal["101"], NonSpare_29.cv_arg],
        Tuple[Literal["102"], NonSpare_32.cv_arg],
        Tuple[Literal["201"], NonSpare_38.cv_arg],
        Tuple[Literal["202"], NonSpare_39.cv_arg],
        Tuple[Literal["301"], NonSpare_40.cv_arg],
        Tuple[Literal["302"], NonSpare_41.cv_arg],
    ]
    cv_fspec_max_bytes = 2
    cv_items_list = [UapItem_1, UapItem_29, UapItem_32, UapItem_43, UapItem_38, UapItem_43, UapItem_39, UapItem_40, UapItem_43, UapItem_42, UapItem_41]
    cv_items_dict = {"010": NonSpare_1, "101": NonSpare_29, "102": NonSpare_32, "201": NonSpare_38, "202": NonSpare_39, "301": NonSpare_40, "302": NonSpare_41}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Type[NonSpare_1]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["101"]) -> Type[NonSpare_29]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["102"]) -> Type[NonSpare_32]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["201"]) -> Type[NonSpare_38]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["202"]) -> Type[NonSpare_39]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["301"]) -> Type[NonSpare_40]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["302"]) -> Type[NonSpare_41]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["101"], Literal["102"], Literal["201"], Literal["202"], Literal["301"], Literal["302"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["010"]) -> Optional[NonSpare_1]:
        ...
    @overload
    def get_item(self, key : Literal["101"]) -> Optional[NonSpare_29]:
        ...
    @overload
    def get_item(self, key : Literal["102"]) -> Optional[NonSpare_32]:
        ...
    @overload
    def get_item(self, key : Literal["201"]) -> Optional[NonSpare_38]:
        ...
    @overload
    def get_item(self, key : Literal["202"]) -> Optional[NonSpare_39]:
        ...
    @overload
    def get_item(self, key : Literal["301"]) -> Optional[NonSpare_40]:
        ...
    @overload
    def get_item(self, key : Literal["302"]) -> Optional[NonSpare_41]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : Optional[NonSpare_1.cv_arg]) -> 'Record_4':
        ...
    @overload
    def set_item(self, key : Literal["101"], val : Optional[NonSpare_29.cv_arg]) -> 'Record_4':
        ...
    @overload
    def set_item(self, key : Literal["102"], val : Optional[NonSpare_32.cv_arg]) -> 'Record_4':
        ...
    @overload
    def set_item(self, key : Literal["201"], val : Optional[NonSpare_38.cv_arg]) -> 'Record_4':
        ...
    @overload
    def set_item(self, key : Literal["202"], val : Optional[NonSpare_39.cv_arg]) -> 'Record_4':
        ...
    @overload
    def set_item(self, key : Literal["301"], val : Optional[NonSpare_40.cv_arg]) -> 'Record_4':
        ...
    @overload
    def set_item(self, key : Literal["302"], val : Optional[NonSpare_41.cv_arg]) -> 'Record_4':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_4':
        ...
    @overload
    def del_item(self, key : Literal["101"]) -> 'Record_4':
        ...
    @overload
    def del_item(self, key : Literal["102"]) -> 'Record_4':
        ...
    @overload
    def del_item(self, key : Literal["201"]) -> 'Record_4':
        ...
    @overload
    def del_item(self, key : Literal["202"]) -> 'Record_4':
        ...
    @overload
    def del_item(self, key : Literal["301"]) -> 'Record_4':
        ...
    @overload
    def del_item(self, key : Literal["302"]) -> 'Record_4':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @overload
    def get_rfs_item(self, arg : Literal["010"]) -> List[NonSpare_1]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["101"]) -> List[NonSpare_29]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["102"]) -> List[NonSpare_32]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["201"]) -> List[NonSpare_38]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["202"]) -> List[NonSpare_39]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["301"]) -> List[NonSpare_40]:
        ...
    @overload
    def get_rfs_item(self, arg : Literal["302"]) -> List[NonSpare_41]:
        ...
    def get_rfs_item(self, arg : Any) -> Any:
        return self._get_rfs_item(arg)

    @overload
    @classmethod
    def create(cls, arg: "Record_4.cv_arg") -> 'Record_4':
        ...
    @overload
    @classmethod
    def create(cls, arg: "Record_4.cv_arg", rfs1 : Optional[List["Record_4.cv_union"]] = None) -> 'Record_4':
        ...
    @overload
    @classmethod
    def create(cls, arg: "Record_4.cv_arg", rfs1: Optional[List["Record_4.cv_union"]], rfs2 : Optional[List["Record_4.cv_union"]] = None) -> 'Record_4':
        ...
    @overload
    @classmethod
    def create(cls, arg: "Record_4.cv_arg", rfs1: Optional[List["Record_4.cv_union"]], rfs2: Optional[List["Record_4.cv_union"]], rfs3 : Optional[List["Record_4.cv_union"]] = None) -> 'Record_4':
        ...
    @classmethod # type: ignore
    def create(cls, arg: "Record_4.cv_arg", *rfs: Any) -> 'Record_4':
        return cls._create(arg, *rfs) # type: ignore

    @classmethod
    def parse(cls, pm: ParsingMode, bs : Bits) -> Union[ValueError, Tuple["Record_4", Bits]]:
        return cls._parse(pm, bs) # type: ignore

class Uap_2(UapSingle):
    cv_arg: TypeAlias = Record_4
    cv_record: TypeAlias = Record_4

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, List[Record_4]]:
        return cls._parse(bs)

class Asterix_7(AstCat):
    cv_category = 3
    cv_edition = (1, 0)
    cv_uap: TypeAlias = Uap_2
    cv_record: TypeAlias = cv_uap.cv_record # shortcut

    @classmethod
    def create(cls, records : List[Uap_2.cv_arg]) -> "Asterix_7":
        return cls._create(records) # type: ignore

class Record_3(Record):
    cv_arg = TypedDict('cv_arg', {
        "010": Optional[NonSpare_1.cv_arg],
        "101": Optional[NonSpare_29.cv_arg],
        "102": Optional[NonSpare_32.cv_arg],
        "201": Optional[NonSpare_38.cv_arg],
        "202": Optional[NonSpare_39.cv_arg],
        "301": Optional[NonSpare_40.cv_arg],
        "302": Optional[NonSpare_41.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 2
    cv_items_list = [UapItem_1, UapItem_29, UapItem_32, UapItem_38, UapItem_39, UapItem_40, UapItem_42, UapItem_41]
    cv_items_dict = {"010": NonSpare_1, "101": NonSpare_29, "102": NonSpare_32, "201": NonSpare_38, "202": NonSpare_39, "301": NonSpare_40, "302": NonSpare_41}

    @overload
    @classmethod
    def spec(cls, key : Literal["010"]) -> Type[NonSpare_1]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["101"]) -> Type[NonSpare_29]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["102"]) -> Type[NonSpare_32]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["201"]) -> Type[NonSpare_38]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["202"]) -> Type[NonSpare_39]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["301"]) -> Type[NonSpare_40]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["302"]) -> Type[NonSpare_41]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["010"], Literal["101"], Literal["102"], Literal["201"], Literal["202"], Literal["301"], Literal["302"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["010"]) -> Optional[NonSpare_1]:
        ...
    @overload
    def get_item(self, key : Literal["101"]) -> Optional[NonSpare_29]:
        ...
    @overload
    def get_item(self, key : Literal["102"]) -> Optional[NonSpare_32]:
        ...
    @overload
    def get_item(self, key : Literal["201"]) -> Optional[NonSpare_38]:
        ...
    @overload
    def get_item(self, key : Literal["202"]) -> Optional[NonSpare_39]:
        ...
    @overload
    def get_item(self, key : Literal["301"]) -> Optional[NonSpare_40]:
        ...
    @overload
    def get_item(self, key : Literal["302"]) -> Optional[NonSpare_41]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["010"], val : Optional[NonSpare_1.cv_arg]) -> 'Record_3':
        ...
    @overload
    def set_item(self, key : Literal["101"], val : Optional[NonSpare_29.cv_arg]) -> 'Record_3':
        ...
    @overload
    def set_item(self, key : Literal["102"], val : Optional[NonSpare_32.cv_arg]) -> 'Record_3':
        ...
    @overload
    def set_item(self, key : Literal["201"], val : Optional[NonSpare_38.cv_arg]) -> 'Record_3':
        ...
    @overload
    def set_item(self, key : Literal["202"], val : Optional[NonSpare_39.cv_arg]) -> 'Record_3':
        ...
    @overload
    def set_item(self, key : Literal["301"], val : Optional[NonSpare_40.cv_arg]) -> 'Record_3':
        ...
    @overload
    def set_item(self, key : Literal["302"], val : Optional[NonSpare_41.cv_arg]) -> 'Record_3':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["010"]) -> 'Record_3':
        ...
    @overload
    def del_item(self, key : Literal["101"]) -> 'Record_3':
        ...
    @overload
    def del_item(self, key : Literal["102"]) -> 'Record_3':
        ...
    @overload
    def del_item(self, key : Literal["201"]) -> 'Record_3':
        ...
    @overload
    def del_item(self, key : Literal["202"]) -> 'Record_3':
        ...
    @overload
    def del_item(self, key : Literal["301"]) -> 'Record_3':
        ...
    @overload
    def del_item(self, key : Literal["302"]) -> 'Record_3':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Record_3.cv_arg") -> 'Record_3':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, pm: ParsingMode, bs : Bits) -> Union[ValueError, Tuple["Record_3", Bits]]:
        return cls._parse(pm, bs) # type: ignore

class Uap_1(UapSingle):
    cv_arg: TypeAlias = Record_3
    cv_record: TypeAlias = Record_3

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, List[Record_3]]:
        return cls._parse(bs)

class Asterix_8(AstCat):
    cv_category = 4
    cv_edition = (1, 0)
    cv_uap: TypeAlias = Uap_1
    cv_record: TypeAlias = cv_uap.cv_record # shortcut

    @classmethod
    def create(cls, records : List[Uap_1.cv_arg]) -> "Asterix_8":
        return cls._create(records) # type: ignore

class NonSpare_56(NonSpare):
    cv_arg: TypeAlias = RuleVariation_6.cv_arg
    cv_name = "I1"
    cv_title = "Test"
    cv_rule: TypeAlias = RuleVariation_6

    @classmethod
    def create(cls, arg : "NonSpare_56.cv_arg") -> "NonSpare_56":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_56", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_6:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_6:
        return self.rule.variation

class Item_19(Item):
    cv_arg: TypeAlias = NonSpare_56.cv_arg
    cv_non_spare: TypeAlias = NonSpare_56

    @classmethod
    def create(cls, arg : "Item_19.cv_arg") -> "Item_19":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_19", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_55(Extended):
    cv_arg_group_1: TypeAlias = Union[int, Tuple[Union[RuleVariation_6.cv_arg, Tuple[Literal["I1"], RuleVariation_6.cv_arg]], None]]
    cv_arg: TypeAlias = Union[
        Tuple["Variation_55.cv_arg_group_1"],
    ]
    cv_items_list = [[(Item_19, 7), None]]
    cv_items_dict = {"I1": NonSpare_56}

    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_56]:
        return cls._spec(key) # type: ignore

    @classmethod
    def create(cls, arg: "Variation_55.cv_arg") -> "Variation_55":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_55", Bits]]:
        return cls._parse(bs) # type: ignore

    @overload # type: ignore
    def get_item(self, key : Literal["I1"]) -> RuleVariation_6:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload # type: ignore
    def modify_subitem_if_present(self, key : Literal["I1"], f: Callable[[RuleVariation_6], RuleVariation_6]) -> "Variation_55":
        ...
    def modify_subitem_if_present(self, key : Any, f : Any) -> Any:
        return self._modify_subitem_if_present(key, f)

class RuleVariation_50(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_55.cv_arg
    cv_variation: TypeAlias = Variation_55

    @classmethod
    def create(cls, arg : "RuleVariation_50.cv_arg") -> "RuleVariation_50":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_50", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_55:
        return self.arg # type: ignore

class NonSpare_31(NonSpare):
    cv_arg: TypeAlias = RuleVariation_50.cv_arg
    cv_name = "101"
    cv_title = "Single1"
    cv_rule: TypeAlias = RuleVariation_50

    @classmethod
    def create(cls, arg : "NonSpare_31.cv_arg") -> "NonSpare_31":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_31", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_50:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_55:
        return self.rule.variation

class UapItem_31(UapItem):
    cv_non_spare: TypeAlias = NonSpare_31

class RuleVariation_33(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_33.cv_arg
    cv_variation: TypeAlias = Variation_33

    @classmethod
    def create(cls, arg : "RuleVariation_33.cv_arg") -> "RuleVariation_33":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_33", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_33:
        return self.arg # type: ignore

class Item_0(Spare):
    cv_arg: TypeAlias = int
    cv_bit_offset8 = 0
    cv_bit_size = 4

class NonSpare_58(NonSpare):
    cv_arg: TypeAlias = RuleVariation_33.cv_arg
    cv_name = "I1"
    cv_title = "Test"
    cv_rule: TypeAlias = RuleVariation_33

    @classmethod
    def create(cls, arg : "NonSpare_58.cv_arg") -> "NonSpare_58":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_58", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_33:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_33:
        return self.rule.variation

class Item_20(Item):
    cv_arg: TypeAlias = NonSpare_58.cv_arg
    cv_non_spare: TypeAlias = NonSpare_58

    @classmethod
    def create(cls, arg : "Item_20.cv_arg") -> "Item_20":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_20", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_50(Extended):
    cv_arg_group_1: TypeAlias = Union[int, Tuple[int, Union[RuleVariation_33.cv_arg, Tuple[Literal["I1"], RuleVariation_33.cv_arg]], None]]
    cv_arg: TypeAlias = Union[
        Tuple["Variation_50.cv_arg_group_1"],
    ]
    cv_items_list = [[(Item_0, 4), (Item_20, 3), None]]
    cv_items_dict = {"I1": NonSpare_58}

    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_58]:
        return cls._spec(key) # type: ignore

    @classmethod
    def create(cls, arg: "Variation_50.cv_arg") -> "Variation_50":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_50", Bits]]:
        return cls._parse(bs) # type: ignore

    @overload # type: ignore
    def get_item(self, key : Literal["I1"]) -> RuleVariation_33:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload # type: ignore
    def modify_subitem_if_present(self, key : Literal["I1"], f: Callable[[RuleVariation_33], RuleVariation_33]) -> "Variation_50":
        ...
    def modify_subitem_if_present(self, key : Any, f : Any) -> Any:
        return self._modify_subitem_if_present(key, f)

class RuleVariation_45(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_50.cv_arg
    cv_variation: TypeAlias = Variation_50

    @classmethod
    def create(cls, arg : "RuleVariation_45.cv_arg") -> "RuleVariation_45":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_45", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_50:
        return self.arg # type: ignore

class NonSpare_34(NonSpare):
    cv_arg: TypeAlias = RuleVariation_45.cv_arg
    cv_name = "102"
    cv_title = "Single2"
    cv_rule: TypeAlias = RuleVariation_45

    @classmethod
    def create(cls, arg : "NonSpare_34.cv_arg") -> "NonSpare_34":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_34", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_45:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_50:
        return self.rule.variation

class UapItem_34(UapItem):
    cv_non_spare: TypeAlias = NonSpare_34

class NonSpare_59(NonSpare):
    cv_arg: TypeAlias = RuleVariation_34.cv_arg
    cv_name = "I1"
    cv_title = "Test"
    cv_rule: TypeAlias = RuleVariation_34

    @classmethod
    def create(cls, arg : "NonSpare_59.cv_arg") -> "NonSpare_59":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_59", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_34:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_36:
        return self.rule.variation

class Item_21(Item):
    cv_arg: TypeAlias = NonSpare_59.cv_arg
    cv_non_spare: TypeAlias = NonSpare_59

    @classmethod
    def create(cls, arg : "Item_21.cv_arg") -> "Item_21":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Item_21", Bits]]:
        return cls._parse(bs) # type: ignore

class Variation_51(Extended):
    cv_arg_group_1: TypeAlias = Union[int, Tuple[int, Union[RuleVariation_34.cv_arg, Tuple[Literal["I1"], RuleVariation_34.cv_arg]]]]
    cv_arg: TypeAlias = Union[
        Tuple["Variation_51.cv_arg_group_1"],
    ]
    cv_items_list = [[(Item_0, 4), (Item_21, 4)]]
    cv_items_dict = {"I1": NonSpare_59}

    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_59]:
        return cls._spec(key) # type: ignore

    @classmethod
    def create(cls, arg: "Variation_51.cv_arg") -> "Variation_51":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_51", Bits]]:
        return cls._parse(bs) # type: ignore

    @overload # type: ignore
    def get_item(self, key : Literal["I1"]) -> RuleVariation_34:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload # type: ignore
    def modify_subitem_if_present(self, key : Literal["I1"], f: Callable[[RuleVariation_34], RuleVariation_34]) -> "Variation_51":
        ...
    def modify_subitem_if_present(self, key : Any, f : Any) -> Any:
        return self._modify_subitem_if_present(key, f)

class RuleVariation_46(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_51.cv_arg
    cv_variation: TypeAlias = Variation_51

    @classmethod
    def create(cls, arg : "RuleVariation_46.cv_arg") -> "RuleVariation_46":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_46", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_51:
        return self.arg # type: ignore

class NonSpare_35(NonSpare):
    cv_arg: TypeAlias = RuleVariation_46.cv_arg
    cv_name = "103"
    cv_title = "Single3"
    cv_rule: TypeAlias = RuleVariation_46

    @classmethod
    def create(cls, arg : "NonSpare_35.cv_arg") -> "NonSpare_35":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_35", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_46:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_51:
        return self.rule.variation

class UapItem_35(UapItem):
    cv_non_spare: TypeAlias = NonSpare_35

class NonSpare_57(NonSpare):
    cv_arg: TypeAlias = RuleVariation_7.cv_arg
    cv_name = "I1"
    cv_title = "Test"
    cv_rule: TypeAlias = RuleVariation_7

    @classmethod
    def create(cls, arg : "NonSpare_57.cv_arg") -> "NonSpare_57":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_57", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_7:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_7:
        return self.rule.variation

class Variation_68(Compound):
    cv_arg = TypedDict('cv_arg', {
        "I1": Optional[NonSpare_57.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 1
    cv_items_list = [NonSpare_57]
    cv_items_dict = {"I1": NonSpare_57}

    @classmethod
    def spec(cls, key : Literal["I1"]) -> Type[NonSpare_57]:
        return cls._spec(key) # type: ignore

    def get_item(self, key : Literal["I1"]) -> Optional[Type[NonSpare_57]]:
        return self._get_item(key) # type: ignore

    def set_item(self, key : Literal["I1"], val : Optional[NonSpare_57.cv_arg]) -> "Variation_68":
        return self._set_item(key, val) # type: ignore

    def del_item(self, key : Literal["I1"]) -> "Variation_68":
        return self._del_item(key) # type: ignore

    @classmethod
    def create(cls, arg: "Variation_68.cv_arg") -> "Variation_68":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["Variation_68", Bits]]:
        return cls._parse(bs) # type: ignore

class RuleVariation_63(RuleVariationContextFree):
    cv_arg: TypeAlias = Variation_68.cv_arg
    cv_variation: TypeAlias = Variation_68

    @classmethod
    def create(cls, arg : "RuleVariation_63.cv_arg") -> "RuleVariation_63":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["RuleVariation_63", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def variation(self) -> Variation_68:
        return self.arg # type: ignore

class NonSpare_36(NonSpare):
    cv_arg: TypeAlias = RuleVariation_63.cv_arg
    cv_name = "104"
    cv_title = "Single4"
    cv_rule: TypeAlias = RuleVariation_63

    @classmethod
    def create(cls, arg : "NonSpare_36.cv_arg") -> "NonSpare_36":
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple["NonSpare_36", Bits]]:
        return cls._parse(bs) # type: ignore

    @property
    def rule(self) -> RuleVariation_63:
        return self.arg # type: ignore

    # shortcut to variation
    @property
    def variation(self) -> Variation_68:
        return self.rule.variation

class UapItem_36(UapItem):
    cv_non_spare: TypeAlias = NonSpare_36

class Record_0(Record):
    cv_arg = TypedDict('cv_arg', {
        "000": Optional[NonSpare_0.cv_arg],
        "101": Optional[NonSpare_31.cv_arg],
        "102": Optional[NonSpare_34.cv_arg],
        "103": Optional[NonSpare_35.cv_arg],
        "104": Optional[NonSpare_36.cv_arg],
    }, total=False)
    cv_fspec_max_bytes = 1
    cv_items_list = [UapItem_0, UapItem_31, UapItem_34, UapItem_35, UapItem_36]
    cv_items_dict = {"000": NonSpare_0, "101": NonSpare_31, "102": NonSpare_34, "103": NonSpare_35, "104": NonSpare_36}

    @overload
    @classmethod
    def spec(cls, key : Literal["000"]) -> Type[NonSpare_0]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["101"]) -> Type[NonSpare_31]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["102"]) -> Type[NonSpare_34]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["103"]) -> Type[NonSpare_35]:
        ...
    @overload
    @classmethod
    def spec(cls, key : Literal["104"]) -> Type[NonSpare_36]:
        ...
    @classmethod
    def spec(cls, key : Union[Literal["000"], Literal["101"], Literal["102"], Literal["103"], Literal["104"]]) -> Any:
        return cls._spec(key)

    @overload
    def get_item(self, key : Literal["000"]) -> Optional[NonSpare_0]:
        ...
    @overload
    def get_item(self, key : Literal["101"]) -> Optional[NonSpare_31]:
        ...
    @overload
    def get_item(self, key : Literal["102"]) -> Optional[NonSpare_34]:
        ...
    @overload
    def get_item(self, key : Literal["103"]) -> Optional[NonSpare_35]:
        ...
    @overload
    def get_item(self, key : Literal["104"]) -> Optional[NonSpare_36]:
        ...
    def get_item(self, key : Any) -> Any:
        return self._get_item(key)

    @overload
    def set_item(self, key : Literal["000"], val : Optional[NonSpare_0.cv_arg]) -> 'Record_0':
        ...
    @overload
    def set_item(self, key : Literal["101"], val : Optional[NonSpare_31.cv_arg]) -> 'Record_0':
        ...
    @overload
    def set_item(self, key : Literal["102"], val : Optional[NonSpare_34.cv_arg]) -> 'Record_0':
        ...
    @overload
    def set_item(self, key : Literal["103"], val : Optional[NonSpare_35.cv_arg]) -> 'Record_0':
        ...
    @overload
    def set_item(self, key : Literal["104"], val : Optional[NonSpare_36.cv_arg]) -> 'Record_0':
        ...
    def set_item(self, key : Any, val : Any) -> Any:
        return self._set_item(key, val)

    @overload
    def del_item(self, key : Literal["000"]) -> 'Record_0':
        ...
    @overload
    def del_item(self, key : Literal["101"]) -> 'Record_0':
        ...
    @overload
    def del_item(self, key : Literal["102"]) -> 'Record_0':
        ...
    @overload
    def del_item(self, key : Literal["103"]) -> 'Record_0':
        ...
    @overload
    def del_item(self, key : Literal["104"]) -> 'Record_0':
        ...
    def del_item(self, key : Any) -> Any:
        return self._del_item(key)

    @classmethod
    def create(cls, arg: "Record_0.cv_arg") -> 'Record_0':
        return cls._create(arg) # type: ignore

    @classmethod
    def parse(cls, pm: ParsingMode, bs : Bits) -> Union[ValueError, Tuple["Record_0", Bits]]:
        return cls._parse(pm, bs) # type: ignore

class Uap_0(UapSingle):
    cv_arg: TypeAlias = Record_0
    cv_record: TypeAlias = Record_0

    @classmethod
    def parse(cls, bs : Bits) -> Union[ValueError, List[Record_0]]:
        return cls._parse(bs)

class Asterix_9(AstCat):
    cv_category = 5
    cv_edition = (1, 0)
    cv_uap: TypeAlias = Uap_0
    cv_record: TypeAlias = cv_uap.cv_record # shortcut

    @classmethod
    def create(cls, records : List[Uap_0.cv_arg]) -> "Asterix_9":
        return cls._create(records) # type: ignore

# Aliases

Cat_000_1_0: TypeAlias = Asterix_0
Ref_000_1_0: TypeAlias = Asterix_1
Cat_000_1_1: TypeAlias = Asterix_2
Ref_000_1_1: TypeAlias = Asterix_3
Ref_000_1_2: TypeAlias = Asterix_4
Cat_001_1_0: TypeAlias = Asterix_5
Cat_002_1_0: TypeAlias = Asterix_6
Cat_003_1_0: TypeAlias = Asterix_7
Cat_004_1_0: TypeAlias = Asterix_8
Cat_005_1_0: TypeAlias = Asterix_9

# Manifest

manifest = {
    'CATS': {
        0: [
            Cat_000_1_0,
            Cat_000_1_1,
        ],
        1: [
            Cat_001_1_0,
        ],
        2: [
            Cat_002_1_0,
        ],
        3: [
            Cat_003_1_0,
        ],
        4: [
            Cat_004_1_0,
        ],
        5: [
            Cat_005_1_0,
        ],
    },
    'REFS': {
        0: [
            Ref_000_1_0,
            Ref_000_1_1,
            Ref_000_1_2,
        ],
    },
}
