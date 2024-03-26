"""Asterix data processing module (generic code)
"""

from dataclasses import dataclass
from typing import *
import sys
if sys.version_info < (3, 10):
    from typing_extensions import TypeAlias
from functools import reduce
from abc import abstractmethod
import math
import re

Raw : TypeAlias = int
ItemName : TypeAlias = str

@dataclass
class Bits:
    """Bit string, a wrapper around bytes (bytes, offset, size)."""
    bs: bytes
    bit_offset: int
    bit_size: int

    @classmethod
    def empty(cls) -> 'Bits':
        return cls(b'', 0, 0)

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
        return Bits(bs, o, n)

    @classmethod
    def fx(cls, val: bool) -> 'Bits':
        return cls.from_uinteger(1 if val else 0, 7, 1)

    def __len__(self) -> int:
        return self.bit_size

    def __iter__(self) -> Iterator[bool]:
        o = self.bit_offset
        n = self.bit_size
        (a, b) = divmod(o, 8)
        m = math.ceil(n / 8)
        bs = self.bs[a:a + m]
        s2 = ''.join([bin(i)[2:].zfill(8) for i in bs])
        o2 = o % 8
        s3 = s2[o2:o2 + n]
        for i in s3:
            yield False if i == '0' else True

    def __eq__(self, other: Any) -> bool:
        o1 = self.bit_offset % 8
        o2 = other.bit_offset % 8
        return (o1 == o2 and list(self) == list(other))

    def _compact(self) -> bytes:
        (a, b) = divmod(self.bit_offset, 8)
        n = self.bit_offset + self.bit_size
        (c, d) = divmod(n, 8)
        if d:
            c += 1
        return self.bs[a:c]

    def __str__(self) -> str:
        bs = self._compact()
        o = self.bit_offset % 8
        mask = [i >= o and i < (o + self.bit_size) for i in range(len(bs) * 8)]
        bits = ''.join([bin(i)[2:].zfill(8) for i in bs])
        out = ''.join([b if m else '.' for (b, m) in zip(bits, mask)])
        return ' '.join(re.findall('........', out))

    def split_at(self, n: int) -> Tuple['Bits', 'Bits']:
        if n < 0:
            raise ValueError('negative index')
        if n > self.bit_size:
            raise ValueError('index too large')
        a = self.__class__(self.bs, self.bit_offset, n)
        b = self.__class__(self.bs, self.bit_offset + n, self.bit_size - n)
        return (a, b)

    def take(self, x: int) -> 'Bits':
        return self.split_at(x)[0]

    def drop(self, x: int) -> 'Bits':
        return self.split_at(x)[1]

    def __add__(self, other: 'Bits') -> 'Bits':
        o = other.bit_offset % 8
        assert ((self.bit_offset + self.bit_size) %
                8) == o, "Bits alignment error"
        n1 = self.bit_size
        n2 = other.bit_size
        bs1 = self._compact()
        bs2 = other._compact()
        if o:
            (a1, x1) = bs1[:-1], bs1[-1]
            (x2, b2) = bs2[0], bs2[1:]
            mask2 = 0xff >> o
            mask1 = 0xff - mask2
            x = (x1 & mask1) | (x2 & mask2)
            bs = a1 + x.to_bytes(1, 'big') + b2
            return self.__class__(bs, self.bit_offset % 8, n1 + n2)
        else:
            return self.__class__(bs1 + bs2, self.bit_offset % 8, n1 + n2)

    def to_bytes(self) -> bytes:
        (a, o) = divmod(self.bit_offset, 8)
        assert o == 0
        (b, o) = divmod(self.bit_size, 8)
        assert o == 0
        return self.bs[a:a + b]

    def to_uinteger(self) -> int:
        (a, o) = divmod(self.bit_offset, 8)
        bs = self.bs[a:]
        if o != 0:
            x = bs[0] & (0xff >> o)
            bs = x.to_bytes(1, 'big') + bs[1:]
        (c, d) = divmod(o + self.bit_size, 8)
        if d == 0:
            return int.from_bytes(bs[0:c], 'big')
        else:
            return (int.from_bytes(bs[0:(c + 1)], 'big') >> (8 - d))

    @classmethod
    def join(cls, lst: List['Bits']) -> 'Bits':
        if len(lst) == 0:
            return Bits.empty()
        if len(lst) == 1:
            return lst[0]
        # TODO: optimize...
        #   - try to avoid bytes concatination
        #   - create groups of the same 'bs'
        #   - use b''.join(...) instead if (+)
        return reduce(lambda a, b: a + b, lst)

@dataclass
class RawDatablock:
    """Size verified raw datablock."""
    bs : bytes

    @classmethod
    def parse_single(cls, s : bytes) -> Tuple['RawDatablock', bytes]:
        """Parse the first level of asterix and the remaining, that is:
        [cat|len|records...|cat|len|records...|...]
        -------------------|-----------------------
            ^--result           ^--remaining
        """
        if len(s) < 3:
            raise ValueError('datablock header')
        n = int.from_bytes(s[1:3], 'big')
        if n < 3:
            raise ValueError('datablock length < 3')
        if len(s) < n:
            raise ValueError('datablock records')
        (a,b) = (s[0:n], s[n:])
        return(cls(a), b)

    @classmethod
    def parse(cls, s : bytes) -> List['RawDatablock']:
        """Parse the first level of asterix to the list of results."""
        def go(acc : List['RawDatablock'], val : bytes) -> Tuple[List['RawDatablock'], bytes]:
            if len(val) == 0:
                return (acc, val)
            (db, rest) = cls.parse_single(val)
            return go(acc+[db], rest)
        return go([], s)[0]

    def unparse(self) -> bytes:
        return self.bs

    @property
    def category(self) -> int:
        return self.bs[0]

    @property
    def length(self) -> int:
        return int.from_bytes(self.bs[1:3], 'big')

    @property
    def raw_records(self) -> bytes:
        return self.bs[3:]

def mk_instance(cls : Any, val : Any) -> Any:
    """Helper function to create value, either by
        - using t(val)
        - or val itself if it's of the correct type already.
    """
    if not isinstance(val, cls):
        val = cls(val)
    return val

# StringType

class StringType:
    """Baseclass for 'String' types"""
    n : ClassVar[int]

    @classmethod
    @abstractmethod
    def from_char(cls, ch : str) -> Raw: ...

    @classmethod
    @abstractmethod
    def to_char(cls, x : Raw) -> str: ...

    @classmethod
    def from_string(cls, s : str) -> Raw:
        p = pow(2, cls.n)
        acc = 0
        for (ix, ch) in enumerate(reversed(s)):
            acc += cls.from_char(ch) * pow(p, ix)
        return acc

    @classmethod
    def to_string(cls, x : Raw, bit_size : int) -> str:
        p = pow(2, cls.n)
        n = bit_size // cls.n
        acc = ''
        for i in range(n):
            (x, i) = divmod(x, p)
            acc = cls.to_char(i) + acc
        return acc

class StringAscii(StringType):
    """Ascii string."""

    n = 8

    @classmethod
    def from_char(cls, ch : str) -> Raw:
        return ord(ch)

    @classmethod
    def to_char(cls, x : Raw) -> str:
        return chr(x)

class StringICAO(StringType):
    """
    Valid range is:
        - 'A'..'Z'  -> [0x01..]
        - space     -> 0x20
        - '0'..'9'  -> [0x30..]
    """

    n = 6

    @classmethod
    def from_char(cls, ch : str) -> Raw:
        if ch >= 'A' and ch <= 'Z':
            return 0x01 + ord(ch) - ord('A')
        if ch == ' ':
            return 0x20
        if ch >= '0' and ch <= '9':
            return 0x30 + ord(ch) - ord('0')
        return 0

    @classmethod
    def to_char(cls, x : Raw) -> str:
        if x >= 0x01 and x <= 0x1A:
            return chr(ord('A') + x - 0x01)
        if x == 0x20:
            return ' '
        if x >= 0x30 and x <= 0x39:
            return chr(ord('0') + x - 0x30)
        return ''

class StringOctal(StringType):
    """Valid character range: ['0'..'7']"""
    n = 3

    @classmethod
    def from_char(cls, ch : str) -> Raw:
        return ord(ch) - ord('0')

    @classmethod
    def to_char(cls, x : Raw) -> str:
        return chr(ord('0') + x)

# Signedness

class Signedness:
    @classmethod
    @abstractmethod
    def convert(cls, bit_size : int, x : int) -> int: ...

class Signed(Signedness):
    @classmethod
    def convert(cls, bit_size : int, x : int) -> int:
        half = pow(2, bit_size-1)
        if x < half:
            return x
        else:
            return (x - int(2*half))

class Unsigned(Signedness):
    @classmethod
    def convert(cls, bit_size : int, x : int) -> int:
        return x

# ExplicitType

class ExplicitType:
    pass

class ReservedExpansion(ExplicitType):
    pass

class SpecialPurpose(ExplicitType):
    pass

# Bds Type

class BdsType:
    pass

class BdsWithAddress(BdsType):
    pass

class BdsAt(BdsType):
    pass

# Content

class Content:
    pass

class ContentRaw(Content):
    @classmethod
    def from_arg(cls, n : int, arg : int) -> int:
        return arg

class ContentTable(Content):
    tab : ClassVar[dict[int, str]]

    @classmethod
    def from_arg(cls, n : int, arg : int) -> int:
        return arg

class ContentString(Content):
    t : ClassVar[Type[StringType]]

    @classmethod
    def from_arg(cls, n : int, arg : Union[Raw, str]) -> int:
        if isinstance(arg, Raw):
            return arg
        return cls.t.from_string(arg)

class ContentInteger(Content):
    sig : ClassVar[Type[Signedness]]

    @classmethod
    def from_arg(cls, n : int, arg : int) -> int:
        return cls.sig.convert(n, arg)

class ContentQuantity(Content):
    sig : ClassVar[Type[Signedness]]
    lsb : ClassVar[float]
    unit : ClassVar[str]

    @classmethod
    def from_arg(cls, n : int, arg : Union[Raw, float, Tuple[float,str]]) -> int:
        if isinstance(arg, Raw):
            return arg
        if isinstance(arg, float):
            x = arg
        elif isinstance(arg, tuple):
            x = arg[0]
        return cls.sig.convert(n, round(x/cls.lsb))

class ContentBds(Content):
    t : ClassVar[Type[BdsType]]
    addr : ClassVar[Optional[int]]

    @classmethod
    def from_arg(cls, n : int, arg : int) -> int:
        return arg

class RuleContent:
    pass

class RuleContentContextFree(RuleContent):
    content : ClassVar[Type[Content]]

class RuleContentDependent(RuleContent):
    depends_on : List[List[str]]
    default_content : ClassVar[Type[Content]]
    cases : List[Tuple[List[int], ClassVar[Type[Content]]]]

# Variation, NonSpare, Item

class Variation:
    def __init__(self, val : Bits):
        self._val = val

    def unparse_bits(self) -> Bits:
        return self._val

    def __eq__(self, other : object) -> bool:
        if not isinstance(other, Variation):
            return False
        return self._val == other._val

    def to_uinteger(self) -> int:
        return self._val.to_uinteger()

class RuleVariation:
    pass

class NonSpare:
    name : ClassVar[str]
    title : ClassVar[str]
    var : ClassVar[Type[Variation]]

class ItemBase:
    pass

class Element(Variation):
    bit_offset8 : ClassVar[int]
    bit_size : ClassVar[int]
    rule : ClassVar[Type[RuleContent]]

    @classmethod
    def parse_bits(cls, s : Bits) -> Tuple['Element', Bits]:
        (a, b) = s.split_at(cls.bit_size)
        return (cls(a), b)

    @classmethod
    def from_raw(cls, raw : Raw) -> Bits:
        return Bits.from_uinteger(raw, cls.bit_offset8, cls.bit_size)

class Group(Variation):
    bit_size : ClassVar[int]
    items_list : ClassVar[List[Type[ItemBase]]]
    items_dict : ClassVar[Dict[str, Type[ItemBase]]]

class Extended(Variation):
    items : ClassVar[List[Optional[Type[ItemBase]]]]

class Repetitive(Variation):
    rep : ClassVar[Optional[int]]
    var : ClassVar[Type[Variation]]

class Explicit(Variation):
    t : ClassVar[Optional[Type[ExplicitType]]]

class Compound(Variation):
    fspec_size : ClassVar[Optional[int]]
    items_list : ClassVar[List[Optional[Type[ItemBase]]]]
    items_dict : ClassVar[Dict[str, Tuple[Type[ItemBase], int]]]

class Spare(ItemBase):
    bit_offset8 : ClassVar[int]
    bit_size : ClassVar[int]

class Item(ItemBase):
    nsp : ClassVar[Type[NonSpare]]

class Record:
    items : ClassVar[List[Optional[Type[ItemBase]]]]

class Expansion:
    fspec_bytes : ClassVar[int]
    items : ClassVar[List[Optional[Type[ItemBase]]]]

# Uap

class Uap:
    pass

class UapSingle(Uap):
    record : ClassVar[Type[Record]]

class UapMultiple(Uap):
    uaps : ClassVar[Dict[str, Type[Record]]]
    selector : ClassVar[Optional[Tuple[List[str], dict[int, str]]]]

# Asterix Spec

class AstSpec:
    pass

class AstCat(AstSpec):
    cat : ClassVar[int]
    edition : ClassVar[Tuple[int, int]]
    uap : ClassVar[Type[Uap]]

class AstRef(AstSpec):
    cat : ClassVar[int]
    edition : ClassVar[Tuple[int, int]]
    expansion : ClassVar[Type[Expansion]]
