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
    n : int

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
    pass

class Signed(Signedness):
    pass

class Unsigned(Signedness):
    pass

# ExplicitType

class ExplicitType:
    pass

class ReservedExpansion(ExplicitType):
    pass

class SpecialPurpose(ExplicitType):
    pass

# Content

class Content:
    pass

class ContentRaw(Content):
    pass

class ContentTable(Content):
    values : Dict[int, str]

class ContentString(Content):
    t : Type[StringType]

class ContentInteger(Content):
    sig : Type[Signedness]

class ContentQuantity(Content):
    sig : Type[Signedness]
    lsb : float
    unit : str

# Variation

class Variation:
    pass

class Element(Variation):
    bit_offset8 : int
    bit_size : int
    content : Type[Content]

class Group(Variation):
    items : List[Type[ItemBase]]

class Extended(Variation):
    items : List[Optional[Type[ItemBase]]]

class Repetitive(Variation):
    rep : Optional[int]
    var : Type[Variation]

class Explicit(Variation):
    t : Optional[Type[ExplicitType]]

class Compound(Variation):
    fspec_size : Optional[int]
    items : List[Optional[Type[ItemBase]]]

# Item

class ItemBase:
    pass

class Spare(ItemBase):
    bit_offset8 : int
    bit_size : int

class Item(ItemBase):
    name : str
    title : str
    var : Type[Variation]

'''
class Variation:
    """Baseclass for all variations."""
    variation : str

    def __init__(self, val : Bits):
        self._val = val

    def unparse_bits(self) -> Bits:
        return self._val

    def __eq__(self, other : object) -> bool:
        if not isinstance(other, Variation):
            return NotImplemented
        return self._val == other._val

    def to_uinteger(self) -> int:
        return self._val.to_uinteger()

class Element(Variation):
    bit_offset8 : int
    bit_size : int
    string_type : StringType
    quantity : Quantity

    @classmethod
    def parse_bits(cls, s : Bits) -> Any:
        n = cls.bit_size
        (a,b) = s.split_at(n)
        return (cls(a), b)

    def _from_raw(self, raw : Raw) -> Bits:
        o = self.__class__.bit_offset8
        n = self.__class__.bit_size
        assert (o >= 0) and (o < 8)
        assert n > 0
        return Bits.from_uinteger(raw, o, n)

    def _from_string(self, s : str) -> Bits:
        st = self.__class__.string_type
        return self._from_raw(st.from_string(s))

    def _sig(self, x : int) -> int:
        sig = self.__class__.quantity.sig
        if sig == 'Signed':
            half = pow(2, self.__class__.bit_size-1)
            if x < half:
                return x
            else:
                return (x - int(2*half))
        if sig == 'Unsigned':
            return x
        assert_never(sig)

    def _from_float(self, val : float) -> Bits:
        x = round(val/self.quantity.lsb)
        return self._from_raw(x)

    def _to_string(self) -> str:
        st = self.__class__.string_type
        x = self.to_uinteger()
        return st.to_string(x, self.__class__.bit_size)

    def _to_quantity(self) -> float:
        x = self._sig(self.to_uinteger())
        return (x * self.quantity.lsb)

def _items_to_group \
   ( schema_lst : List[Any], \
     items_arg : Dict[ItemName, Any], \
     spares_arg_orig : Optional[List[Any]] = None, \
     fx : Optional[bool] = None) \
    -> Tuple[Bits, Dict[ItemName, Element], List[Spare]]:
    """Helper function for 'Group' and 'Extended' items."""
    bits : List[Bits] = []
    items : Dict[ItemName, Element] = {}
    spares : List[Spare] = []
    spares_arg = spares_arg_orig if spares_arg_orig is None else spares_arg_orig.copy()
    for x in schema_lst:
        if isinstance(x, tuple):
            name, t = x
            i = mk_instance(t, items_arg[name])
            items[name] = i
            bits.append(i.unparse_bits())
            continue
        else:
            if spares_arg is None:
                i = mk_instance(x, 0)
            else:
                i = mk_instance(x, spares_arg.pop(0))
            spares.append(i)
            bits.append(i.unparse_bits())
            continue
    if fx is not None:
        bits.append(Bits.fx(fx))
    return (Bits.join(bits), items, spares)

def _raw_to_items( \
    schema_lst : List[Any], \
    raw : Raw) \
    -> Tuple[Dict[ItemName, Element], List[Spare]]:
    """Helper function for 'Group' and 'Extended' items.
    Convert raw value to individual sub-items (store raw sub-values).
    """
    items : Dict[ItemName, Element] = {}
    spares : List[Spare] = []
    for x in reversed(schema_lst):
        if isinstance(x, tuple):
            name, t = x
            raw, val = divmod(raw, pow(2, t.bit_size))
            items[name] = val
            continue
        else:
            raw, val = divmod(raw, pow(2, x.bit_size))
            spares.append(val)
            continue
    return (items, spares)

def _parse_group(s : Bits, lst : List[Any]) \
    -> Tuple[int, Dict[ItemName, Any], List[Spare]]:
    """Helper parsing function for 'Group' and 'Extended' items."""

    reminder = s
    items = {}
    spares = []
    n = 0
    for i in lst:
        if isinstance(i, tuple):
            (name, cls) = i
            (item, reminder) = cls.parse_bits(reminder)
            items[name] = item
        else:
            (item, reminder) = i.parse_bits(reminder)
            spares.append(item)
        n += len(item.unparse_bits())
    return (n, items, spares)

class Group(Variation):
    subitems_list : List[Any]
    subitems_dict : Dict[ItemName, Tuple[str, Any, int, int]] # (title, cls, groupoffset, bitsize)

    @classmethod
    def parse_bits(cls, s : Bits) -> Any:
        (n, items, spares) = _parse_group(s, cls.subitems_list)
        (a, b) = s.split_at(n)
        return (cls((a, items, spares)), b) # type: ignore

    def __init__(self, val : Bits, items : Dict[ItemName, Any], spares : List[Spare]):
        self._val = val
        self._items = items
        self._spares = spares

    def _from_items(self, items_args : Any, spares_args : Any) \
        -> Tuple[Bits, Dict[ItemName, Element], List[Spare]]:
        return _items_to_group(self.__class__.subitems_list, items_args, spares_args, fx=None)

    def _from_raw(self, raw : Raw) -> Tuple[Bits, Dict[ItemName, Element], List[Spare]]:
        (items, spares) = _raw_to_items(self.__class__.subitems_list, raw)
        return self._from_items(items, spares)

    def _get_item(self, name : Any) -> Any:
        return self._items[name]

    def _set_item(self, name : Any, val : Any) -> Any:
        items = self._items.copy()
        items[name] = val
        result = self._from_items(items, self._spares)
        return self.__class__(result) # type: ignore

    def _modify_item(self, name : Any, f : Any) -> Any:
        x = self._get_item(name)
        return self._set_item(name, f(x))

    def get_spares(self) -> List[Spare]:
        return self._spares

    def _set_spare(self, spare_ix : int, spare_val : Any) -> Any:
        spares = self._spares.copy()
        spares[spare_ix]  = spare_val
        result = self._from_items(self._items, spares)
        return self.__class__(result) # type: ignore

class Extended(Variation):
    no_trailing_fx : bool
    groups_bit_sizes : List[int]
    subitems_list : List[Any]
    subitems_dict : Dict[ItemName, Tuple[str, Any, int, int]]

    @classmethod
    def parse_bits(cls, s : Bits) -> Any:
        def is_last(grp : List[Any]) -> bool:
            return grp == cls.subitems_list[-1] # type: ignore
        reminder = s
        items : Dict[ItemName, Any] = {}
        spares : List[Spare] = []
        n = 0
        for grp in cls.subitems_list:
            (m, sub, spr) = _parse_group(reminder, grp)
            reminder = reminder.drop(m)
            items.update(sub)
            spares.extend(spr)
            if is_last(grp) and cls.no_trailing_fx:
                n += m
                break
            else:
                (fx,reminder) = reminder.split_at(1)
                n += (m+1)
                if fx.to_uinteger() == 0:
                    break
                if is_last(grp) and fx.to_uinteger() == 1:
                    raise ValueError('unexpected fx bit set')
        (a,b) = s.split_at(n)
        return (cls((a, items, spares)), b) # type: ignore

    def __init__(self, val : Bits, items : Dict[ItemName, Element], spares : List[Spare]):
        self._val = val
        self._items = items
        self._spares = spares

    def _from_single_int(self, ix: int, val : int, fx : Optional[bool] = None) \
        -> Tuple[Bits, Dict[ItemName, Element], List[Spare]]:
        lst = self.__class__.subitems_list[ix]
        (items, spares) = _raw_to_items(lst, val)
        return _items_to_group(lst, items, spares, fx)

    def _fx_absent(self, group_list : List[Any]) -> bool:
        return all ([
            self.__class__.no_trailing_fx,
            len(group_list) >= len(self.__class__.subitems_list) ])

    def _concat(self, lst : List[Any]) -> Tuple[Any, Any, Any]:
        bits = Bits.join([a for (a,_b,_c) in lst])
        items = {}
        spares = []
        for (_a,b,c) in lst:
            items.update(b)
            spares.append(c)
        return (bits, items, spares)

    def _from_tuple_int(self, val : Any) -> Tuple[Bits, Dict[ItemName, Element], List[Spare]]:
        val = list(enumerate(val))
        a = val[:-1] # fx=True
        b = val[-1]  # fx=False or absent

        result = [self._from_single_int(ix, x, fx=True) for (ix, x) in a]
        trailing_fx = None if self._fx_absent(val) else False
        result += [self._from_single_int(b[0], b[1], fx=trailing_fx)]

        return self._concat(result)

    def _from_dict(self, n : int, arg : Any, spares : Any = None) -> Tuple[Bits, Dict[ItemName, Element], List[Spare]]:
        def mk_names(lst : List[Any]) -> Any:
            for i in lst:
                if isinstance(i, tuple):
                    yield i[0]
        subitems = [mk_names(lst) for lst in self.__class__.subitems_list]
        def mk_group(lst : Any) -> Any:
            return {k: arg[k] for k in lst}
        groups = list(map(mk_group, subitems[0:n]))
        trailing_fx = None if self._fx_absent(groups) else False
        a = groups[:-1]    # fx=True
        b = groups[-1]     # fx=False
        groups = [(x, True) for x in a]
        groups.append((b, trailing_fx))
        result = []
        for (ix, (items, fx)) in enumerate(groups):
            lst = self.__class__.subitems_list[ix]
            num_of_spares = len([x for x in lst if isinstance(x, tuple)])
            sp = None
            if spares is not None:
                sp = spares[0:num_of_spares]
                spares = spares[num_of_spares:]
            result.append(_items_to_group(lst, items, sp, fx))
        return self._concat(result)

    def _get_item(self, name : Any) -> Any:
        return self._items.get(name)

    """ TODO
    def _set_item(self, name : Any, val : Any) -> Any:
        # This function may only be called on items from the primary part.
        items = self._items.copy()
        spares = self._spares.copy()
        items[name] = val
        result = self._from_dict(1, items, spares)
        return self.__class__(result)

    def _modify_item(self, name : Any, f : Any) -> Any:
        x = self._items.get(name)
        if x is None:
            return self
        return self._set_item(name, f(x))
    """

    def get_spares(self) -> List[Spare]:
        return self._spares

class Repetitive(Variation):
    rep_byte_size : Optional[int]
    variation_bit_size : int
    variation_type : Any

    @classmethod
    def parse_bits(cls, s : Bits) -> Any:
        bs = cls.rep_byte_size
        items = []
        # parsing with FX
        if bs is None:
            n = 0
            reminder = s
            while True:
                (item, reminder) = cls.variation_type.parse_bits(reminder)
                (fx, reminder) = reminder.split_at(1)
                items.append(item)
                n += len(item.unparse_bits()) + 1
                if fx.to_uinteger() == 0:
                    break
        # parsing as regular repetitive
        else:
            rbs = bs * 8
            n = rbs
            (m,reminder) = s.split_at(rbs)
            for i in range(m.to_uinteger()):
                (item, reminder) = cls.variation_type.parse_bits(reminder)
                items.append(item)
                n += len(item.unparse_bits())

        (a,b) = s.split_at(n)
        return (cls((a, items)), b) # type: ignore

    def __init__(self, val : Bits, items : List[Variation]):
        self._val = val
        self._items = items

    def _from_list(self, lst : List[Any]) -> Tuple[Bits, Any]:
        cls = self.__class__.variation_type
        items = [mk_instance(cls,arg) for arg in lst]
        bs = self.__class__.rep_byte_size
        if bs is None:
            blist = []
            for (ix, i) in enumerate(items):
                fx = Bits.fx(True if ((ix+1) < len(items)) else False)
                blist.append(i.unparse_bits() + fx)
            return (Bits.join(blist), items)
        else:
            bits = Bits.join([i.unparse_bits() for i in items])
            n = Bits.from_uinteger(len(lst), 0, bs*8)
            return(n+bits, items)

    def __len__(self) -> Any:
        return len(self._items)

    def __iter__(self) -> Any:
        return iter(self._items)

    def __getitem__(self, ix : int) -> Any:
        return self._items[ix]

    def _append_item(self, arg : Any) -> Any:
        items = self._items.copy()
        return self.__class__(items + [arg]) # type: ignore

    def _prepend_item(self, arg : Any) -> Any:
        items = self._items.copy()
        return self.__class__([arg] + items) # type: ignore

class Explicit(Variation):
    explicit_type : Optional[str]

    @classmethod
    def parse_bits(cls, s : Bits) -> Any:
        (a,b) = s.split_at(8)
        n = a.to_uinteger() * 8
        (a,b) = s.split_at(n)
        raw = a.drop(8).to_bytes()
        return (cls((a, raw)), b) # type: ignore

    def __init__(self, val : Bits, raw : bytes):
        self._val = val
        self._raw = raw

    def _from_bytes(self, arg : bytes) -> Tuple[Bits, bytes]:
        n = len(arg) + 1
        bits = Bits.from_uinteger(n, 0, 8) + Bits.from_bytes(arg)
        return (bits, arg)

    @property
    def raw(self) -> bytes:
        return self._raw

class Compound(Variation):
    fspec_fx : bool
    fspec_max_bytes : int
    subitems_list : List[Optional[Tuple[ItemName, Any]]]
    subitems_dict : Dict[ItemName, Tuple[str, Any, int]]

    @classmethod
    def _parse_fspec(cls, s : Bits) -> Any:
        reminder = s
        if cls.fspec_fx:
            cnt = 0
            flags = []
            while True:
                if cnt >= cls.fspec_max_bytes:
                    raise ValueError('fspec max bytes exceeded')
                cnt += 1
                (a, reminder) = reminder.split_at(8)
                flags.extend(list(a.take(7)))
                fx = a.drop(7).take(1).to_uinteger()
                if fx == 0:
                    break
            return (cnt*8, flags)
        else:
            n = cls.fspec_max_bytes * 8
            return (n, list(s.take(n)))

    @classmethod
    def parse_bits(cls, s : Bits) -> Any:
        result = cls._parse_fspec(s)
        (n,fspec) = result
        items = {}
        reminder = s.drop(n)
        for (flag, i) in zip(fspec, cls.subitems_list):
            if not flag:
                continue
            if i is None:
                raise ValueError('fx bit set for non-defined item')
            (subname, subcls) = i
            result = subcls.parse_bits(reminder)
            (subitem, reminder) = result
            items[subname] = subitem
            n += len(subitem.unparse_bits())
        return (cls((s.take(n), items)), reminder) # type: ignore

    def __init__(self, val : Bits = Bits.empty(), items : Dict[ItemName, Variation] = {}) -> None:
        self._val = val
        self._items = items

    def __bool__(self) -> bool:
        return bool(self._items)

    def _fspec(self, parts : List[ItemName]) -> Bits:
        d = self.__class__.subitems_dict
        fspec = reduce(lambda a,b: a|b, [d[name][2] for name in parts], 0)
        n = self.__class__.fspec_max_bytes
        if self.__class__.fspec_fx:
            while (((fspec % 256) == 0) and (n>0)):
                fspec = fspec // 256
                n -= 1
        return Bits.from_bytes(fspec.to_bytes(n, 'big'))

    def _recreate(self, items : Any) -> Any:
        bits : List[Bits] = []
        fspec = self._fspec(list(items.keys()))
        bits.append(fspec)
        for x in self.__class__.subitems_list:
            if isinstance(x, tuple):
                name, ignore = x
                i = items.get(name)
                if i is not None:
                    bits.append(i.unparse_bits())
        obj = self.__class__()
        obj._val = Bits.join(bits)
        obj._items = items
        return obj

    def _set_item(self, name : ItemName, val : Any) -> Any:
        _title, t, fspec_bit = self.__class__.subitems_dict[name]
        i = mk_instance(t, val)
        items = self._items.copy()
        items[name] = i
        return self._recreate(items)

    def _update(self, args : Any) -> Any:
        obj = self
        for name, val in args.items():
            obj = obj._set_item(name, val)
        self._val = obj._val
        self._items = obj._items

    def _del_item(self, name : ItemName) -> Any:
        items = self._items.copy()
        items.pop(name, None)
        return self._recreate(items)

    def _get_item(self, name : ItemName) -> Any:
        return self._items.get(name)

    def _modify_item(self, name : Any, f : Any) -> Any:
        x = self._items.get(name)
        if x is None:
            return self
        return self._set_item(name, f(x))

class Spare:
    """Spare bits superclass.
    Store actual value (for the cases if it's not 'zero').
    """
    bit_offset8 : int
    bit_size : int

    def __init__(self, val : Union[Bits, Raw]):
        if isinstance(val, Raw):
            val = self._from_raw(val)
        self._val = val

    @classmethod
    def parse_bits(cls, s : Bits) -> Any:
        n = cls.bit_size
        (a,b) = s.split_at(n)
        return (cls(a), b)

    def _from_raw(self, raw : Raw) -> Bits:
        o = self.__class__.bit_offset8
        n = self.__class__.bit_size
        assert (o >= 0) and (o < 8)
        assert n > 0
        return Bits.from_uinteger(raw, o, n)

    def unparse_bits(self) -> Bits:
        return self._val

    def __eq__(self, other : object) -> bool:
        return self._val == other._val # type: ignore

    def to_uinteger(self) -> int:
        return self._val.to_uinteger()

T = TypeVar('T')
class Datablock(Generic[T]):
    """Correctly constructed/parsed datablock."""
    def __init__(self, cat : int, lst : Union[T, List[T]], val : Optional[bytes] = None):
        self.cat = cat
        if not isinstance(lst, list):
            lst = [lst]
        self.lst = lst
        raw = b''.join([rec.unparse_bits().to_bytes() for rec in lst]) # type: ignore
        if val is None:
            val = b''.join([
                cat.to_bytes(1, 'big'),             # CAT
                (3 + len(raw)).to_bytes(2, 'big'),  # LEN
                raw])
        self.val = val

    def unparse(self) -> bytes:
        return self.val

    def __eq__(self, other : Any) -> bool:
        return self.val == other.val # type: ignore

    @property
    def records(self) -> List[T]:
        return self.lst

class UapSelector:
    item : List[str]
    table : List[Tuple[int, str]]

class UapBase:
    pass

class Uap(UapBase):
    var : Type[Variation]

class Uaps(UapBase):
    lst : Dict[str, Type[Variation]]
    sel : Optional[Type[UapSelector]]

class AsterixSpec:
    """Asterix base class."""
    cat : int

class Basic(AsterixSpec):
    uap : Type[UapBase]

    variation : Any
    uaps : Any
    uap_selector_item : Any
    uap_selector_table : Any

    @classmethod
    def _parse(cls, raw_db : RawDatablock, uap : Optional[str] = None) -> Any:
        if raw_db.category != cls.cat:
            raise ValueError('Wrong category')
        s = Bits.from_bytes(raw_db.raw_records)
        records = []
        while len(s) > 0:
            if hasattr(cls, 'variation'):
                (rec, s) = cls.variation.parse_bits(s)
            elif hasattr(cls, 'uaps'):
                result = None
                errors = {}
                # UAP is not specified, try each in sequence
                if uap is None:
                    for (name, var) in cls.uaps.items():
                        try:
                            (rec, s2) = var.parse_bits(s)
                            # parsing alone is not sufficient,
                            # need to confirm UAP selector
                            if cls._is_valid(rec):
                                (result, s) = (rec, s2)
                                break
                        except ValueError as e:
                            errors[name] = e
                    if result is None:
                        raise ValueError('unable to parse with any UAP, {}'.format(errors))
                # use specified UAP
                else:
                    var = cls.uaps[uap]
                    (rec, s) = var.parse_bits(s)
                    # if selector is available, validate
                    if not cls.uap_selector_item is None:
                        if not cls._is_valid(rec):
                            raise ValueError('incompatible UAP')
            else:
                raise RuntimeError('should not be here')
            records.append(rec)
        return Datablock(cls.cat, records, raw_db.bs)

    @classmethod
    def _is_valid(cls, rec : Any) -> bool:
        i = rec
        for subitem in cls.uap_selector_item:
            i = i.get_item(subitem)
            if i is None:
                return False
        val = i.to_uinteger()
        uap_name = cls.uap_selector_table[val]
        actual_var = cls.uaps[uap_name]
        return actual_var == rec.__class__ # type: ignore

class Expansion(AsterixSpec):
    var : Type[Variation]
    spec = variation.spec
    parse_bits = variation.parse_bits
    unparse_bits = variation.unparse_bits

'''
