"""Asterix data processing module (generic code)
"""

from itertools import chain, dropwhile
from dataclasses import dataclass
from binascii import hexlify, unhexlify
from typing import *
import sys
import enum
if sys.version_info < (3, 10):
    from typing_extensions import TypeAlias
from abc import abstractmethod
from functools import reduce
import math


class Bits:
    """Bit string, a wrapper around bytes (bytes, offset1, offset2)."""

    def __init__(self, bs: bytes, bit_offset1: int, bit_offset2: int):
        self.bs = bs
        self.bit_offset1 = bit_offset1
        self.bit_offset2 = bit_offset2

    @classmethod
    def from_bytes(cls, val: bytes) -> 'Bits':
        return cls(val, 0, len(val) * 8)

    @classmethod
    def from_uinteger(cls, raw: int, o: int, n: int) -> 'Bits':
        (a, b) = divmod(o + n, 8)
        rem = 8 - b if b else 0
        if b:
            a += 1
            raw *= pow(2, rem)
        raw = raw % pow(2, a * 8)
        bs = raw.to_bytes(a, 'big')
        return cls(bs, o, o + n)

    @classmethod
    def fx(cls, val: bool) -> 'Bits':
        return cls.from_uinteger(1 if val else 0, 7, 1)

    @classmethod
    def concat(cls, lst: List['Bits']) -> 'Bits':
        if len(lst) == 0:
            return cls.from_bytes(b'')
        if len(lst) == 1:
            return lst[0]
        # TODO: optimize...
        #   - try to avoid bytes concatination
        #   - create groups of the same 'bs'
        #   - use b''.join(...) instead if (+)
        return reduce(lambda a, b: a + b, lst)

    def __len__(self) -> int:
        return self.bit_offset2 - self.bit_offset1

    def null(self) -> bool:
        return len(self) <= 0

    def __iter__(self) -> Iterator[bool]:
        o = self.bit_offset1
        n = len(self)
        (a, b) = divmod(o, 8)
        m = math.ceil(n / 8)
        bs = self.bs[a:a + m]
        s2 = ''.join([bin(i)[2:].zfill(8) for i in bs])
        o2 = o % 8
        s3 = s2[o2:o2 + n]
        for i in s3:
            yield False if i == '0' else True

    def __eq__(self, other: Any) -> bool:
        return list(self) == list(other)

    def _offsets(self) -> Tuple[int, int, int, int]:
        """Helper function for byte/bit offsets"""
        (a, b) = divmod(self.bit_offset1, 8)
        (c, d) = divmod(self.bit_offset2, 8)
        if d:
            c += 1
        # (start byte_offset/bit_offset, end byte_offset/bit_offset)
        return (a, b, c, d)

    def __str__(self) -> str:
        a, _, c, _ = self._offsets()

        def f(i: int) -> str:  # handle one octet
            s = bin(self.bs[i])[2:].zfill(8)
            bit_offsets = [i * 8 + ix for ix in range(8)]
            mask = [ix >= self.bit_offset1 and ix <
                    self.bit_offset2 for ix in bit_offsets]
            return ''.join([b if m else '.' for (b, m) in zip(s, mask)])
        return ' '.join([f(i) for i in range(a, c)])

    def split_at(self, n: int) -> Tuple['Bits', 'Bits']:
        s1 = self.__class__(self.bs, self.bit_offset1, self.bit_offset1 + n)
        s2 = self.__class__(self.bs, self.bit_offset1 + n, self.bit_offset2)
        return (s1, s2)

    def take(self, x: int) -> 'Bits':
        return self.split_at(x)[0]

    def drop(self, x: int) -> 'Bits':
        return self.split_at(x)[1]

    def __add__(self, other: 'Bits') -> 'Bits':
        o1 = self._offsets()
        o2 = other._offsets()
        assert o1[3] == o2[1], "Bits alignment error"
        o = o2[1]
        bs1 = self.bs[:o1[2]]
        bs2 = other.bs[o2[0]:]
        n = len(self) + len(other)
        if o:  # not byte aligned
            (a1, x1) = bs1[:-1], bs1[-1]
            (x2, b2) = bs2[0], bs2[1:]
            mask2 = 0xff >> o
            mask1 = 0xff - mask2
            x = (x1 & mask1) | (x2 & mask2)
            bs = a1 + x.to_bytes(1, 'big') + b2
            return self.__class__(bs, self.bit_offset1, self.bit_offset1 + n)
        else:  # byte aligned
            bs = bs1 + bs2
            return self.__class__(bs, self.bit_offset1, self.bit_offset1 + n)

    def to_bytes(self) -> bytes:
        (a, o) = divmod(self.bit_offset1, 8)
        assert o == 0
        (b, o) = divmod(self.bit_offset2, 8)
        assert o == 0
        return self.bs[a:b]

    def to_uinteger(self) -> int:
        (a, o) = divmod(self.bit_offset1, 8)
        bs = self.bs[a:]
        if o != 0:
            x = bs[0] & (0xff >> o)
            bs = x.to_bytes(1, 'big') + bs[1:]
        (c, d) = divmod(o + len(self), 8)
        if d == 0:
            return int.from_bytes(bs[0:c], 'big')
        else:
            return (int.from_bytes(bs[0:(c + 1)], 'big') >> (8 - d))

    def head_tail(self) -> Tuple[bool, 'Bits']:
        a, b = self.split_at(1)
        return (a.to_uinteger() != 0, b)

    def append_bit(self, arg: bool) -> 'Bits':
        o = self.bit_offset2
        b = self.__class__.from_uinteger(1 if arg else 0, o, 1)
        return self + b


class RawDatablock:
    """Size verified raw datablock."""

    def __init__(self, bs: Bits):
        self.bs = bs

    @classmethod
    def parse_single(cls, s: Bits) -> \
            Union[ValueError, Tuple['RawDatablock', Bits]]:
        """Parse the first level of asterix and the remaining, that is:
        [cat|len|records...|cat|len|records...|...]
        -------------------|-----------------------
            ^--result           ^--remaining
        """
        if len(s) < 24:
            return ValueError('datablock header')
        (a, b) = s.split_at(24)
        n = a.drop(8).to_uinteger() * 8
        if n < 24:
            return ValueError('datablock length')
        if len(s) < n:
            return ValueError('datablock records')
        (a, b) = s.split_at(n)
        return (cls(a), b)

    @classmethod
    def parse(cls, s: Bits) -> Union[ValueError, List['RawDatablock']]:
        """Parse the first level of asterix to the list of results."""
        def go(acc: List['RawDatablock'], val: Bits) \
                -> Union[ValueError, Tuple[List['RawDatablock'], Bits]]:
            assert len(val) >= 0
            if len(val) == 0:
                return (acc, val)
            result = cls.parse_single(val)
            if isinstance(result, ValueError):
                return result
            (db, rest) = result
            return go(acc + [db], rest)
        result = go([], s)
        if isinstance(result, ValueError):
            return result
        return result[0]

    def unparse(self) -> Bits:
        return self.bs

    def get_category(self) -> int:
        return self.bs.take(8).to_uinteger()

    def get_length(self) -> int:
        return self.bs.drop(8).take(16).to_uinteger()

    def get_raw_records(self) -> Bits:
        return self.bs.drop(24)


class StringType:
    cv_bits_per_char: ClassVar[int]
    cv_pad_char: ClassVar[str]

    @classmethod
    @abstractmethod
    def from_char(cls, ch: str) -> int: ...

    @classmethod
    @abstractmethod
    def to_char(cls, x: int) -> str: ...

    @classmethod
    def from_string(cls, bit_size: int, s: str) -> int:
        n = bit_size // cls.cv_bits_per_char
        s = s[0:n]  # left adjust and pad
        s = s + cls.cv_pad_char * (n - len(s))
        p = pow(2, cls.cv_bits_per_char)
        acc = 0
        for (ix, ch) in enumerate(reversed(s)):
            acc += cls.from_char(ch) * pow(p, ix)
        return acc

    @classmethod
    def to_string(cls, bit_size: int, x: int) -> str:
        p = pow(2, cls.cv_bits_per_char)
        n = bit_size // cls.cv_bits_per_char
        acc = ''
        for i in range(n):
            (x, i) = divmod(x, p)
            acc = cls.to_char(i) + acc
        return acc


class StringAscii(StringType):
    cv_bits_per_char = 8
    cv_pad_char = ' '

    @classmethod
    def from_char(cls, ch: str) -> int:
        return ord(ch)

    @classmethod
    def to_char(cls, x: int) -> str:
        return chr(x)


class StringICAO(StringType):
    """
    Valid range is:
        - 'A'..'Z'  -> [0x01..]
        - space     -> 0x20
        - '0'..'9'  -> [0x30..]
    """

    cv_bits_per_char = 6
    cv_pad_char = ' '

    @classmethod
    def from_char(cls, ch: str) -> int:
        if ch >= 'A' and ch <= 'Z':
            return 0x01 + ord(ch) - ord('A')
        if ch == ' ':
            return 0x20
        if ch >= '0' and ch <= '9':
            return 0x30 + ord(ch) - ord('0')
        return 0

    @classmethod
    def to_char(cls, x: int) -> str:
        if x >= 0x01 and x <= 0x1A:
            return chr(ord('A') + x - 0x01)
        if x == 0x20:
            return ' '
        if x >= 0x30 and x <= 0x39:
            return chr(ord('0') + x - 0x30)
        return ''


class StringOctal(StringType):
    """Valid character range: ['0'..'7']"""
    cv_bits_per_char = 3
    cv_pad_char = '0'

    @classmethod
    def from_char(cls, ch: str) -> int:
        return ord(ch) - ord('0')

    @classmethod
    def to_char(cls, x: int) -> str:
        return chr(ord('0') + x)


class Signedness:
    @classmethod
    @abstractmethod
    def convert(cls, bit_size: int, x: int) -> int: ...


class Signed(Signedness):
    @classmethod
    def convert(cls, bit_size: int, x: int) -> int:
        assert bit_size > 0
        half = pow(2, bit_size - 1)
        if x < half:
            return x
        else:
            return (x - int(2 * half))


class Unsigned(Signedness):
    @classmethod
    def convert(cls, bit_size: int, x: int) -> int:
        return x


class ExplicitType:
    pass


class ReservedExpansion(ExplicitType):
    pass


class SpecialPurpose(ExplicitType):
    pass


class BdsType:
    pass


class BdsWithAddress(BdsType):
    pass


class BdsAt(BdsType):
    pass


class Content:
    @classmethod
    @abstractmethod
    def _convert(cls, bit_size: int, arg: Any) -> Any:
        ...

    def __init__(self, bs: Bits) -> None:
        self.bs = bs

    def as_uint(self) -> int:
        return self.bs.to_uinteger()


class ContentRaw(Content):
    @classmethod
    def _convert(cls, bit_size: int, arg: int) -> int:
        return arg


class ContentTable(Content):
    cv_values: ClassVar[dict[int, str]]

    @classmethod
    def _convert(cls, bit_size: int, arg: int) -> int:
        return arg

    def table_value(self) -> Optional[str]:
        bs = self.bs
        return self.__class__.cv_values.get(bs.to_uinteger())


class ContentString(Content):
    cv_string_type: ClassVar[Type[StringType]]

    @classmethod
    def _convert(cls, bit_size: int, arg: Union[int, str]) -> int:
        if isinstance(arg, int):
            return arg
        if isinstance(arg, str):
            return cls.cv_string_type.from_string(bit_size, arg)
        assert_never(arg)

    def as_string(self) -> str:
        bs = self.bs
        return self.__class__.cv_string_type.to_string(
            len(bs), bs.to_uinteger())


class ContentInteger(Content):
    cv_signedness: ClassVar[Type[Signedness]]

    @classmethod
    def _convert(cls, bit_size: int, arg: int) -> int:
        return arg

    def as_integer(self) -> int:
        bs = self.bs
        return self.__class__.cv_signedness.convert(len(bs), bs.to_uinteger())


class ContentQuantity(Content):
    cv_signedness: ClassVar[Type[Signedness]]
    cv_lsb: ClassVar[float]
    cv_unit: ClassVar[str]

    @classmethod
    def _convert(cls, bit_size: int,
                 arg: Union[int, float, Tuple[float, str]]) -> int:
        if isinstance(arg, int):
            return arg
        if isinstance(arg, float):
            return cls.cv_signedness.convert(bit_size, round(arg / cls.cv_lsb))
        if isinstance(arg, tuple):
            assert arg[1] == cls.cv_unit
            return cls.cv_signedness.convert(
                bit_size, round(arg[0] / cls.cv_lsb))
        assert_never(arg)

    def _as_quantity(self) -> float:
        bs = self.bs
        x = self.__class__.cv_signedness.convert(len(bs), bs.to_uinteger())
        return (x * self.__class__.cv_lsb)


class ContentBds(Content):
    cv_bds_type: ClassVar[
        Union[Type[BdsWithAddress], Tuple[Type[BdsAt], Optional[int]]]]

    @classmethod
    def _convert(cls, bit_size: int, arg: int) -> int:
        return arg


class RuleContent:
    """
    Base class for different RuleContent possibilities.
    """

    def __init__(self, bs: Bits) -> None:
        self.bs = bs

    def as_uint(self) -> int:
        return self.bs.to_uinteger()

    @classmethod
    @abstractmethod
    def _convert(cls, bit_size: int, arg: Any) -> Any:
        ...


class RuleContentContextFree(RuleContent):
    """
    A content structure that is fully defined in the specification
    and is statically known.
    """
    cv_content: ClassVar[Type[Content]]

    @classmethod
    def _convert(cls, bit_size: int, arg: Any) -> Any:
        return cls.cv_content._convert(bit_size, arg)

    def _get_content(self) -> Content:
        return self.__class__.cv_content(self.bs)


class RuleContentDependent(RuleContent):
    """
    A content structure where all possible structures are defined
    in the specification, but where the actual concrete structure depends
    on a value of some other item, which is only known at run-time.
    """
    cv_depends_on: ClassVar[List[List[str]]]
    cv_default_content: ClassVar[Type[Content]]
    cv_cases: ClassVar[List[Tuple[List[int], Type[Content]]]]

    @classmethod
    def _find_content(cls, ix: Optional[List[int]]) -> Type[Content]:
        if ix is None:
            return cls.cv_default_content
        else:
            ix = list(ix)
            for val, content in cls.cv_cases:
                if val == ix:
                    return content
            raise ValueError('unexpected case', ix)

    @classmethod
    def _convert(cls, bit_size: int, arg: Any) -> Any:
        if not isinstance(arg, tuple):
            arg = (None, arg)
        ix, arg = arg
        content = cls._find_content(ix)
        return content._convert(bit_size, arg)

    def _get_content(self, ix: Optional[List[int]]) -> Content:
        content = self.__class__._find_content(ix)
        return content(self.bs)


class Variation:
    """
    Base class (see subclasses below).
    Subclasses follow asterix structure as defined here:
    <https://zoranbosnjak.github.io/asterix-specs/struct.html>
    """
    @classmethod
    @abstractmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple['Variation', Bits]]:
        ...

    @abstractmethod
    def unparse(self) -> Bits:
        ...


class ItemBase:
    """
    Base class for Spare or NonSpare item (see subclasses below).
    """
    @classmethod
    @abstractmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple['ItemBase', Bits]]:
        ...

    @abstractmethod
    def unparse(self) -> Bits:
        ...


class RuleVariation:
    """
    Base class for different RuleVariation possibilities.
    """

    @classmethod
    @abstractmethod
    def parse(cls, bs: Bits) -> Union[ValueError,
                                      Tuple['RuleVariation', Bits]]:
        ...

    @abstractmethod
    def unparse(self) -> Bits:
        ...


class Spare(ItemBase):
    cv_bit_offset8: ClassVar[int]
    cv_bit_size: ClassVar[int]

    def __init__(self, bs: Bits) -> None:
        self.bs = bs

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple['Spare', Bits]]:
        n = cls.cv_bit_size
        if len(bs) < n:
            return ValueError('overflow')
        a, b = bs.split_at(n)
        return (cls(a), b)

    @classmethod
    def create(cls, arg: int) -> 'Spare':
        bs = Bits.from_uinteger(arg, cls.cv_bit_offset8, cls.cv_bit_size)
        return cls(bs)

    def unparse(self) -> Bits:
        return self.bs

    def as_uint(self) -> int:
        return self.bs.to_uinteger()


class NonSpare:
    cv_name: ClassVar[str]
    cv_title: ClassVar[str]
    cv_rule: ClassVar[Type[RuleVariation]]

    def __init__(self, arg: RuleVariation) -> None:
        self.arg = arg

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple['NonSpare', Bits]]:
        result = cls.cv_rule.parse(bs)
        if isinstance(result, ValueError):
            return result
        obj, remaining = result
        return (cls(obj), remaining)

    @classmethod
    def _create(cls, arg: Any) -> Any:
        if isinstance(arg, cls):
            return arg
        # first field of a tuple is maybe item name
        if isinstance(arg, tuple):
            if len(arg) == 2:
                if isinstance(arg[0], str):
                    name, arg = arg
                    assert name == cls.cv_name
        val = cls.cv_rule.create(arg)  # type: ignore
        return cls(val)

    def unparse(self) -> Bits:
        return self.arg.unparse()

    def as_uint(self) -> int:
        return self.arg.as_uint()  # type: ignore


class Item(ItemBase):
    cv_non_spare: ClassVar[Type[NonSpare]]

    def __init__(self, arg: NonSpare) -> None:
        self.arg = arg

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple['Item', Bits]]:
        result = cls.cv_non_spare.parse(bs)
        if isinstance(result, ValueError):
            return result
        obj, remaining = result
        return (cls(obj), remaining)

    @classmethod
    def _create(cls, arg: Any) -> Any:
        if isinstance(arg, cls):
            return arg
        val = cls.cv_non_spare.create(arg)  # type: ignore
        return cls(val)

    def unparse(self) -> Bits:
        return self.arg.unparse()


class Element(Variation):
    cv_bit_offset8: ClassVar[int]
    cv_bit_size: ClassVar[int]
    cv_rule: ClassVar[Type[RuleContent]]

    def __init__(self, bs: Bits) -> None:
        self.bs = bs

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple['Element', Bits]]:
        n = cls.cv_bit_size
        if len(bs) < n:
            return ValueError('overflow')
        a, b = bs.split_at(n)
        return (cls(a), b)

    @classmethod
    def _create(cls, arg: Any) -> Any:
        if isinstance(arg, cls):
            return arg
        val = cls.cv_rule._convert(cls.cv_bit_size, arg)
        bs = Bits.from_uinteger(val, cls.cv_bit_offset8, cls.cv_bit_size)
        return cls(bs)

    def unparse(self) -> Bits:
        return self.bs

    def as_uint(self) -> int:
        return self.bs.to_uinteger()

    def _get_rule(self) -> RuleContent:
        return self.__class__.cv_rule(self.bs)


class Group(Variation):
    cv_bit_offset8: ClassVar[int]
    cv_bit_size: ClassVar[int]
    cv_items_list: ClassVar[List[Tuple[Type[ItemBase], int]]]
    cv_items_dict: ClassVar[Dict[str, Type[RuleVariation]]]

    @classmethod
    def _spec(cls, key: str) -> Type[RuleVariation]:
        return cls.cv_items_dict[key]

    def __init__(self, bs: Bits, arg: List[ItemBase]) -> None:
        self.bs = bs
        self.arg = arg

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple['Group', Bits]]:
        remaining = bs
        items: List[ItemBase] = []
        for (i, size) in cls.cv_items_list:
            result = i.parse(remaining)
            if isinstance(result, ValueError):
                return result
            obj, remaining = result
            items.append(obj)
        n = len(bs)
        m = len(remaining)
        return (cls(bs.take(n - m), items), remaining)

    @classmethod
    def _create(cls, arg: Any) -> Any:
        if isinstance(arg, cls):
            return arg
        # if arg is given as single integer, split to individual components
        if isinstance(arg, int):
            values: List[int] = []
            val = arg
            for (spec, size) in reversed(cls.cv_items_list):
                (val, x) = divmod(val, pow(2, size))
                values.insert(0, x)
            arg = tuple(values)
        assert isinstance(arg, tuple)
        items = []
        bs = Bits.from_uinteger(0, cls.cv_bit_offset8, 0)
        for ((a, size), b) in zip(cls.cv_items_list, arg):
            i = a.create(b)  # type: ignore
            items.append(i)
            bs += i.unparse()
        return cls(bs, items)

    def unparse(self) -> Bits:
        return self.bs

    def as_uint(self) -> int:
        return self.bs.to_uinteger()

    def _get_item(self, key: Any) -> Any:
        for i in self.arg:
            if isinstance(i, Item):
                nsp = i.cv_non_spare
                if nsp.cv_name == key:
                    return i.arg.arg
        raise ValueError(key, 'not found')

    def get_spares(self) -> List[int]:
        result = []
        for i in self.arg:
            if isinstance(i, Spare):
                result.append(i.as_uint())
        return result


class Extended(Variation):
    cv_items_list: ClassVar[List[List[Optional[Tuple[Type[ItemBase], int]]]]]

    def __init__(self, bs: Bits, arg: List[List[Optional[ItemBase]]]) -> None:
        self.bs = bs
        self.arg = arg

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple['Extended', Bits]]:
        remaining = bs
        items1: List[List[Optional[ItemBase]]] = []
        items2: List[Optional[ItemBase]] = []
        for i in chain.from_iterable(cls.cv_items_list):
            if i is None:
                items2.append(None)
                items1.append(items2.copy())
                items2 = []
                if len(remaining) < 1:
                    return ValueError('overflow')
                fx, remaining = remaining.head_tail()
                if not fx:
                    break
            else:
                spec, size = i
                result = spec.parse(remaining)
                if isinstance(result, ValueError):
                    return result
                obj, remaining = result
                items2.append(obj)
        n = len(bs)
        m = len(remaining)
        return (cls(bs.take(n - m), items1), remaining)

    @classmethod
    def _create(cls, groups: Any) -> Any:
        if isinstance(groups, cls):
            return groups
        all_items: List[List[Optional[ItemBase]]] = []
        bs = Bits.from_bytes(b'')
        for ix, (specs, arg) in enumerate(zip(cls.cv_items_list, groups)):
            fx = True if (ix + 1) < len(groups) else False
            if isinstance(arg, int):
                values: List[Optional[int]] = []
                val = arg
                for i in reversed(specs):
                    if i is None:  # trailing FX
                        values.insert(0, None)
                    else:
                        spec, size = i
                        (val, x) = divmod(val, pow(2, size))
                        values.insert(0, x)
                arg = tuple(values)
            assert isinstance(arg, tuple)
            items: List[Optional[ItemBase]] = []
            for (a, b) in zip(specs, arg):
                if a is None:
                    items.append(None)
                    bs += Bits.fx(fx)
                else:
                    a, size = a  # type: ignore
                    i = a.create(b)  # type: ignore
                    items.append(i)  # type: ignore
                    bs += i.unparse()  # type: ignore
            all_items.append(items)
        return cls(bs, all_items)

    def unparse(self) -> Bits:
        return self.bs

    def get_spares(self) -> List[int]:
        result: List[int] = []
        for j in self.arg:
            for i in j:
                if isinstance(i, Spare):
                    result.append(i.as_uint())
        return result

    def _get_item(self, key: Any) -> Any:
        for j in self.arg:
            for i in j:
                if isinstance(i, Item):
                    nsp = i.cv_non_spare
                    if nsp.cv_name == key:
                        return i.arg.arg
        return None


class Repetitive(Variation):
    cv_rep_bytes: ClassVar[Optional[int]]
    cv_variation: ClassVar[Type[Variation]]

    def __init__(self, bs: Bits, arg: List[Variation]) -> None:
        self.bs = bs
        self.arg = arg

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple['Repetitive', Bits]]:
        rbs = cls.cv_rep_bytes
        items: List[Variation] = []
        remaining = bs
        # parsing with FX
        if rbs is None:
            while True:
                result = cls.cv_variation.parse(remaining)
                if isinstance(result, ValueError):
                    return result
                obj, remaining = result
                if len(remaining) < 1:
                    return ValueError('overflow')
                fx, remaining = remaining.head_tail()
                items.append(obj)
                if not fx:
                    break
        # regular repetitive
        else:
            rbs = rbs * 8
            if len(remaining) < rbs:
                return ValueError('overflow')
            (cnt, remaining) = remaining.split_at(rbs)
            for i in range(cnt.to_uinteger()):
                result = cls.cv_variation.parse(remaining)
                if isinstance(result, ValueError):
                    return result
                obj, remaining = result
                items.append(obj)
        n = len(bs)
        m = len(remaining)
        return (cls(bs.take(n - m), items), remaining)

    @classmethod
    def _create(cls, lst: Any) -> Any:
        if isinstance(lst, cls):
            return lst
        if cls.cv_rep_bytes is None:
            bs = Bits.from_bytes(b'')
            items = [cls.cv_variation.create(i) for i in lst]  # type: ignore
            for (ix, obj) in enumerate(items):
                bs += obj.unparse()
                bs += Bits.fx(ix < len(lst) - 1)
            return cls(bs, items)
        else:
            bs = Bits.from_uinteger(len(lst), 0, cls.cv_rep_bytes * 8)
            items = [cls.cv_variation.create(i) for i in lst]  # type: ignore
            for obj in items:
                bs += obj.unparse()
            return cls(bs, items)

    def unparse(self) -> Bits:
        return self.bs

    def _get_list(self) -> Any:
        return self.arg


class Explicit(Variation):
    cv_explicit_type: ClassVar[Optional[Type[ExplicitType]]]

    def __init__(self, bs: Bits) -> None:
        self.bs = bs

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple['Explicit', Bits]]:
        if len(bs) < 8:
            return ValueError('overflow')
        cnt = bs.take(8).to_uinteger() * 8
        if len(bs) < cnt:
            return ValueError('overflow')
        a, b = bs.split_at(cnt)
        return (cls(a), b)

    @classmethod
    def _create(cls, s: Any) -> Any:
        if isinstance(s, cls):
            return s
        bs = Bits.from_uinteger(len(s) + 1, 0, 8)
        bs += Bits.from_bytes(s)
        return cls(bs)

    def unparse(self) -> Bits:
        return self.bs

    def get_bytes(self) -> bytes:
        return self.bs.drop(8).to_bytes()


class Fspec:
    def __init__(self, bs: Bits):
        self.bs = bs

    @classmethod
    def parse(cls, s: Bits) -> Union[ValueError, Tuple['Fspec', Bits]]:
        n = 0
        for i in s.to_bytes():
            n += 8
            if not (i & 0x01):
                break
        # empty input is not valid
        if not n:
            return ValueError('overflow')
        # 'i' contains the last byte, which must have FX=0
        if (i & 0x01):
            return ValueError('overflow')
        (a, b) = s.split_at(n)
        return (cls(a), b)

    def __iter__(self) -> Iterator[bool]:
        n = 0
        for flag in self.bs:
            if n < 7:
                yield flag
            n += 1
            n %= 8

def unparse_fspec(items_list: List[Optional[Type[NonSpare]]],
                  args: Dict[str, Any]) -> Tuple[Bits, List[Any]]:
    bs = Bits.from_bytes(b'')
    items: List[Tuple[str, NonSpare]] = []
    cnt = 0
    for i in items_list:
        if not args:
            break
        if cnt % 8 == 7:
            cnt += 1
            bs = bs.append_bit(True)  # FX
        cnt += 1
        if i is None:
            bs = bs.append_bit(False)
        else:
            name = i.cv_name
            try:
                obj = i.create(args.pop(name))  # type: ignore
                bs = bs.append_bit(True)
                items.append((name, obj))
            except KeyError:
                bs = bs.append_bit(False)
    if args:
        raise ValueError('items error', args)
    while (cnt % 8):
        cnt += 1
        bs = bs.append_bit(False)
    return (bs, items)

class Compound(Variation):
    cv_fspec_max_bytes: ClassVar[int]
    cv_items_list: ClassVar[List[Optional[Type[NonSpare]]]]
    cv_items_dict: ClassVar[Dict[str, Type[NonSpare]]]

    def __init__(self, bs: Bits, arg: Dict[str, NonSpare]) -> None:
        self.bs = bs
        self.arg = arg

    @classmethod
    def _spec(cls, key: str) -> Type[NonSpare]:
        return cls.cv_items_dict[key]

    @classmethod
    def parse(cls, bs: Bits) -> Union[ValueError, Tuple['Compound', Bits]]:
        result = Fspec.parse(bs)
        if isinstance(result, ValueError):
            return result
        fspec, remaining = result
        items = {}
        for (flag, nsp) in zip(fspec, cls.cv_items_list):
            if not flag:
                continue
            if nsp is None:
                return ValueError('fx bit set for non-defined item')
            result2 = nsp.parse(remaining)
            if isinstance(result2, ValueError):
                return result2
            obj, remaining = result2
            items[nsp.cv_name] = obj
        n = len(bs)
        m = len(remaining)
        return (cls(bs.take(n - m), items), remaining)

    @classmethod
    def _create(cls, args: Dict[str, Any]) -> Any:
        if isinstance(args, cls):
            return args
        bs, items = unparse_fspec(cls.cv_items_list, args)
        for obj in items:
            bs += obj[1].unparse()
        return cls(bs, {name: obj for name, obj in items})

    def unparse(self) -> Bits:
        return self.bs

    def _get_item(self, key: Any) -> Any:
        return self.arg.get(key)

    def _set_item(self, key: Any, val: Any) -> Any:
        d = self.arg
        d[key] = val
        return self.__class__._create(d)

    def _del_item(self, key: Any) -> Any:
        d = self.arg
        try:
            del d[key]
        except KeyError:
            pass
        return self.__class__._create(d)


class RuleVariationContextFree(RuleVariation):
    """
    Wrapper around Variation which is fully defined in the specification
    and is statically known.
    """
    cv_variation: ClassVar[Type[Variation]]

    def __init__(self, arg: Variation) -> None:
        self.arg = arg

    @classmethod
    def parse(cls, bs: Bits) -> \
            Union[ValueError, Tuple['RuleVariationContextFree', Bits]]:
        result = cls.cv_variation.parse(bs)
        if isinstance(result, ValueError):
            return result
        obj, remaining = result
        return (cls(obj), remaining)

    @classmethod
    def _create(cls, arg: Any) -> Any:
        if isinstance(arg, cls):
            return arg
        var = cls.cv_variation.create(arg)  # type: ignore
        return cls(var)

    def unparse(self) -> Bits:
        return self.arg.unparse()

    def as_uint(self) -> int:
        return self.arg.as_uint()  # type: ignore


class RuleVariationDependent(RuleVariation):
    """
    Wrapper around Variation, where all possible structures are defined
    in the specification, but where the actual concrete structure depends
    on a value of some other item, which is only known at run-time.
    """
    cv_depends_on: ClassVar[List[List[str]]]
    cv_default_variation: ClassVar[Type[Variation]]
    cv_cases: ClassVar[List[Tuple[List[int], Type[Variation]]]]

    @classmethod
    def _variation(cls, key: Any) -> Type[Variation]:
        key = list(key)
        for a, b in cls.cv_cases:
            if a == key:
                return b
        raise ValueError('Unexpected key', key)

    def __init__(self, bs: Bits) -> None:
        self.bs = bs

    @classmethod
    def parse(cls, bs: Bits) -> \
            Union[ValueError, Tuple['RuleVariationDependent', Bits]]:
        result = cls.cv_default_variation.parse(bs)
        if isinstance(result, ValueError):
            return result
        obj, remaining = result
        return (cls(obj.unparse()), remaining)

    @classmethod
    def _create(cls, arg: Any) -> Any:
        if isinstance(arg, cls):
            return arg
        var = cls.cv_default_variation.create(arg)  # type: ignore
        return cls(var.unparse())

    def unparse(self) -> Bits:
        return self.bs


class UapItemBase:
    pass


class UapItem(UapItemBase):
    cv_non_spare: ClassVar[Type[NonSpare]]


class UapItemSpare(UapItemBase):
    pass


class UapItemRFS(UapItemBase):
    pass


def get_frn(items_list: List[Type[UapItemBase]], name: str) -> int:
    frn = 1
    for i in items_list:
        if issubclass(i, UapItem):
            if i.cv_non_spare.cv_name == name:
                return frn
        frn += 1
    raise ValueError(name, 'not found')

class ParsingMode(enum.Enum):
    StrictParsing = enum.auto()
    PartialParsing = enum.auto()

class Record:
    cv_fspec_max_bytes: ClassVar[int]
    cv_items_list: ClassVar[List[Type[UapItemBase]]]
    cv_items_dict: ClassVar[Dict[str, Type[NonSpare]]]

    @classmethod
    def _spec(cls, key: str) -> Type[NonSpare]:
        return cls.cv_items_dict[key]

    def __init__(self, bs: Bits, items1: Dict[str, NonSpare],
                 items2: List[Optional[List[Tuple[str, NonSpare]]]]):
        self.bs = bs
        self.items_regular = items1
        self.items_rfs = items2

    @classmethod
    def _parse(cls, pm: ParsingMode, bs: Bits) -> \
        Union[ValueError, Tuple['Record', Bits]]:
        """Remark:
        This function is using a less known python feature,
        where the 'else' keyword is included at the end of 'for' loop.
        This pattern allows nesteed for loops to break the outer loop,
        when the inner loop breaks.
        """

        # make sure that we can use a simple 'if StrictParsing... else...'
        # in the rest of this function (that is: there are only 2 modes).
        if TYPE_CHECKING:
            match pm:
                case ParsingMode.StrictParsing: pass
                case ParsingMode.PartialParsing: pass
                case _: assert_never(pm)

        result = Fspec.parse(bs)
        if isinstance(result, ValueError):
            return result
        fspec, remaining = result
        items1: Dict[str, NonSpare] = {}
        items2: List[Optional[List[Tuple[str, NonSpare]]]] = []
        for (flag, i) in zip(fspec, cls.cv_items_list):
            if not flag:
                continue
            if issubclass(i, UapItemSpare):
                if pm == ParsingMode.StrictParsing:
                    return ValueError('fx bit set for spare item')
                else: break
            elif issubclass(i, UapItemRFS):
                items3: List[Tuple[str, NonSpare]] = []
                if len(remaining) < 8:
                    if pm == ParsingMode.StrictParsing:
                        return ValueError('overflow')
                    else: break
                a, remaining = remaining.split_at(8)
                n = a.to_uinteger()
                for j in range(n):
                    if len(remaining) < 8:
                        if pm == ParsingMode.StrictParsing:
                            return ValueError('overflow')
                        else: break
                    b, remaining = remaining.split_at(8)
                    frn = b.to_uinteger()
                    try:
                        assert frn > 0
                        T = cls.cv_items_list[frn - 1]
                        nsp = T.cv_non_spare  # type: ignore
                        name = nsp.cv_name
                    except (IndexError, AttributeError, AssertionError):
                        if pm == ParsingMode.StrictParsing:
                            return ValueError('invalid FRN')
                        else: break
                    result2 = nsp.parse(remaining)
                    if isinstance(result2, ValueError):
                        if pm == ParsingMode.StrictParsing:
                            return result2
                        else: break
                    obj, remaining = result2
                    items3.append((name, obj))
                else:
                    items2.append(items3)
                    continue
                break
            elif issubclass(i, UapItem):
                nsp = i.cv_non_spare
                result2 = nsp.parse(remaining)
                if isinstance(result2, ValueError):
                    if pm == ParsingMode.StrictParsing:
                        return result2
                    else: break
                obj, remaining = result2
                items1[nsp.cv_name] = obj
            else:
                raise Exception('Unexpected', i)
        n = len(bs)
        m = len(remaining)

        # In case of StrictParsing, we can grab the original bits,
        # but for the PartialParsing, we need to recreate the record
        # from parsed items, since the fspec might be different and
        # the original bits are not exactly the same.
        if pm == ParsingMode.StrictParsing:
            r = cls(bs.take(n - m), items1, items2)
        else:
            r = cls._create(items1, items2)
        return (r, remaining)

    @classmethod
    def _create(cls, args: Dict[str, Any], *rfs_args: Any) -> Any:
        # remove empty rfs elements from the tail of the rfss list
        rfss = list(dropwhile(lambda x: not x, reversed(rfs_args)))

        bs1 = Bits.from_bytes(b'')  # fspec
        bs2 = Bits.from_bytes(b'')  # items
        items1: Dict[str, NonSpare] = {}
        items2: List[Optional[List[Tuple[str, NonSpare]]]] = []

        def handle_rfs(rfs: List[Tuple[str, NonSpare]]) -> Any:
            items = []
            bs = Bits.from_uinteger(len(rfs), 0, 8)
            for (name, val) in rfs:
                frn = get_frn(cls.cv_items_list, name)
                T = cls.cv_items_dict[name]
                obj = T.create(val)  # type: ignore
                items.append((name, obj))
                bs += Bits.from_uinteger(frn, 0, 8)
                bs += obj.unparse()
            return (items, bs)

        cnt = 0
        for i in cls.cv_items_list:
            if (not args) and (not rfss):
                break
            if cnt % 8 == 7:
                cnt += 1
                bs1 = bs1.append_bit(True)  # FX
            cnt += 1
            if issubclass(i, UapItem):
                nsp = i.cv_non_spare
                name = nsp.cv_name
                try:
                    obj = nsp.create(args.pop(name))  # type: ignore
                    bs1 = bs1.append_bit(True)
                    bs2 += obj.unparse()
                    items1[name] = obj
                except KeyError:
                    bs1 = bs1.append_bit(False)
            elif issubclass(i, UapItemSpare):
                bs1 = bs1.append_bit(False)
            elif issubclass(i, UapItemRFS):
                if not rfss:
                    bs1 = bs1.append_bit(False)
                    items2.append(None)
                else:
                    rfs = rfss.pop(-1) # the list is reversed, pop from the tail
                    if rfs is None:
                        bs1 = bs1.append_bit(False)
                    else:
                        bs1 = bs1.append_bit(True)
                        rfs_objs, bs = handle_rfs(rfs)
                        items2.append(rfs_objs)
                        bs2 += bs
            else:
                raise ValueError('unexpected type', i)

        # at this point, there should be no more items to process
        if args or rfss:
            raise ValueError('items error', args)

        # pad fspec
        while (cnt % 8):
            cnt += 1
            bs1 = bs1.append_bit(False)

        return cls(bs1+bs2, items1, items2)

    def unparse(self) -> Bits:
        return self.bs

    def _get_item(self, key: Any) -> Any:
        return self.items_regular.get(key)

    def _get_rfs_item(self, key: Any) -> Any:
        result = []
        for lst in self.items_rfs:
            if lst is None:
                continue
            for (name, obj) in lst:
                if name == key:
                    result.append(obj)
        return result

    def _set_item(self, key: Any, val: Any) -> Any:
        d1 = self.items_regular
        d2 = self.items_rfs
        d1[key] = val
        return self.__class__._create(d1, d2 or None)

    def _del_item(self, key: Any) -> Any:
        d1 = self.items_regular
        d2 = self.items_rfs
        try:
            del d1[key]
        except KeyError:
            pass
        return self.__class__._create(d1, d2 or None)


class Uap:

    @classmethod
    def _parse_records(cls, r: Any, bs: Bits) -> Union[ValueError, List[Any]]:
        remaining = bs
        rv = []
        while len(remaining):
            result = r.parse(ParsingMode.StrictParsing, remaining)
            if isinstance(result, ValueError):
                return result
            r, remaining = result
            rv.append(r)
        return rv


class UapSingle(Uap):
    cv_record: ClassVar[Type[Record]]

    @classmethod
    def _parse(cls, bs: Bits) -> Union[ValueError, List[Any]]:
        return cls._parse_records(cls.cv_record, bs)


class UapMultiple(Uap):
    cv_uaps: ClassVar[Dict[str, Type[Record]]]
    cv_selector: ClassVar[Optional[Tuple[List[str], dict[int, str]]]]

    @classmethod
    def _spec(cls, key: str) -> Any:
        return cls.cv_uaps[key]

    @classmethod
    def _parse(cls, uap: str, bs: Bits) -> Union[ValueError, List[Any]]:
        return cls._parse_records(cls.cv_uaps[uap], bs)

    @classmethod
    def _parse_any_uap(cls, bs: Bits) -> List[List[Any]]:
        """Try to parse with all possible uap combinations.
        The result is list of possible combinations where each combination
        is itself a list (like with normal single uap _parse function).
        - Empty outer list means parsing failure.
        - Single element outer list is a normal parsing result,
          eg. [[plot, plot]]
        - Multi element outer list is a sign for a possible ambiguity,
          that is: every list element is a possible parsing result.
          eg. [[plot, plot], [track, plot]]
        """
        lst = cls.cv_uaps.values()

        def go(acc: List[Any], s: Bits) -> Any:
            if not len(s):
                yield acc
            for cls in lst:
                result = cls._parse(ParsingMode.StrictParsing, s)
                if isinstance(result, ValueError):
                    continue
                x, remaining = result
                for i in go(acc + [x], remaining):
                    yield i

        return list(go([], bs))


class FspecType:
    pass


class FspecFixed(FspecType):
    pass


class FspecFx(FspecType):
    pass


class Expansion:
    # in case of Fixed fspec, 'int' is the actual byte size,
    # in case of Fx fspec, 'int' represents max_bytes
    cv_type: ClassVar[Tuple[Union[Type[FspecFixed], Type[FspecFx]], int]]
    cv_items_list: ClassVar[List[Optional[Type[NonSpare]]]]
    cv_items_dict: ClassVar[Dict[str, Type[NonSpare]]]

    def __init__(self, bs: Bits, arg: Dict[str, NonSpare]) -> None:
        self.bs = bs
        self.arg = arg

    @classmethod
    def _spec(cls, key: str) -> Type[NonSpare]:
        return cls.cv_items_dict[key]

    @classmethod
    def _parse(cls, bs: Bits) -> Union[ValueError, Tuple['Expansion', Bits]]:
        a, b = cls.cv_type
        if a == FspecFixed:
            n = b*8
            if len(bs) < n:
                return ValueError('overflow')
            flags_bits, remaining = bs.split_at(n)
            flags = list(flags_bits)
        elif a == FspecFx:
            result1 = Fspec.parse(bs)
            if isinstance(result1, ValueError):
                return result1
            fspec, remaining = result1
            flags = list(fspec)
        else:
            raise Exception('Unexpected cv_type', a)
        items = {}
        for (flag, nsp) in zip(flags, cls.cv_items_list):
            if not flag:
                continue
            if nsp is None:
                return ValueError('fx bit set for non-defined item')
            result2 = nsp.parse(remaining)
            if isinstance(result2, ValueError):
                return result2
            obj, remaining = result2
            items[nsp.cv_name] = obj
        n = len(bs)
        m = len(remaining)
        return (cls(bs.take(n - m), items), remaining)

    @classmethod
    def _create(cls, args: Dict[str, Any]) -> Any:
        if isinstance(args, cls):
            return args
        a, b = cls.cv_type
        if a == FspecFixed:
            n = b*8
            bs = Bits.from_bytes(b'')
            items: List[Tuple[str, NonSpare]] = []
            cnt = 0
            for i in cls.cv_items_list:
                if not args:
                    break
                cnt += 1
                if i is None:
                    bs = bs.append_bit(False)
                else:
                    name = i.cv_name
                    try:
                        obj = i.create(args.pop(name))  # type: ignore
                        bs = bs.append_bit(True)
                        items.append((name, obj))
                    except KeyError:
                        bs = bs.append_bit(False)
            if args:
                raise ValueError('items error', args)
            if cnt > n:
                raise ValueError('fspec length too short')
            while cnt < n:
                cnt += 1
                bs = bs.append_bit(False)
        elif a == FspecFx:
            bs, items = unparse_fspec(cls.cv_items_list, args)
        else:
            raise Exception('Unexpected cv_type', a)
        for obj in items:
            bs += obj[1].unparse()
        return cls(bs, {name: obj for name, obj in items})

    def unparse(self) -> Bits:
        return self.bs

    def _get_item(self, key: Any) -> Any:
        return self.arg.get(key)

    def _set_item(self, key: Any, val: Any) -> Any:
        d = self.arg
        d[key] = val
        return self.__class__._create(d)

    def _del_item(self, key: Any) -> Any:
        d = self.arg
        try:
            del d[key]
        except KeyError:
            pass
        return self.__class__._create(d)


class AstSpec:
    pass


class AstCat(AstSpec):
    cv_category: ClassVar[int]
    cv_edition: ClassVar[Tuple[int, int]]
    cv_uap: ClassVar[Type[Uap]]

    def __init__(self, bs: Bits, records: List[Record]):
        self.bs = bs
        self.records = records

    @classmethod
    def _create(cls, records: Any) -> Any:
        s = Bits.concat([r.unparse() for r in records])
        cat = Bits.from_uinteger(cls.cv_category, 0, 8)
        n = Bits.from_uinteger((len(s) // 8) + 3, 0, 16)
        return cls(cat + n + s, records)

    def unparse(self) -> Bits:
        return self.bs


class AstRef(AstSpec):
    cv_category: ClassVar[int]
    cv_edition: ClassVar[Tuple[int, int]]
    cv_expansion: ClassVar[Type[Expansion]]
