# bitstring tests

from typing import List
from hypothesis import *
import hypothesis.strategies as st
from binascii import unhexlify
from random import randbytes, randint

from asterix.base import Bits

# small positive integer
spi = st.integers(min_value=1, max_value=20)


def test_join1() -> None:
    x = Bits.concat([
        Bits.from_uinteger(0, 0, 4),
        Bits.from_uinteger(0xf, 4, 4),
    ])
    y = Bits.from_bytes(unhexlify('0f'))
    assert x == y


@given(...)
def test_join2(lst: List[bytes]) -> None:
    a = Bits.from_bytes(b''.join(lst))
    b = Bits.concat([Bits.from_bytes(bs) for bs in lst])
    assert a == b


@given(...)
def test_convert_bytes(bs: bytes) -> None:
    assert Bits.from_bytes(bs).to_bytes() == bs
    assert Bits.from_bytes(bs).take(len(bs) * 8).to_bytes() == bs
    assert Bits.from_bytes(bs).drop(0).to_bytes() == bs


@given(spi, spi)
def test_convert_bytes2(n1: int, n2: int) -> None:
    bs1 = randbytes(n1)
    bs2 = randbytes(n2)
    bs = Bits.from_bytes(bs1 + bs2)

    assert bs.take(n1 * 8) == Bits.from_bytes(bs1)
    assert bs.take(n1 * 8).to_bytes() == bs1
    assert bs.drop(n1 * 8).to_bytes() == bs2
    assert bs.drop(n1 * 8).take(n2 * 8).to_bytes() == bs2


@given(spi)
def test_from_to_integer(n: int) -> None:
    val = randint(0, pow(2, n) - 1)
    for o in range(8):
        assert Bits.from_uinteger(val, o, n).to_uinteger() == val


@given(st.integers(min_value=0, max_value=15))
def test_single_bit_uinteger(ix: int) -> None:
    bs = Bits(bs=b'\xff\x00', bit_offset1=ix, bit_offset2=ix + 1)
    assert bs.to_uinteger() == (1 if ix < 8 else 0)


def test_add1() -> None:
    a = Bits.from_bytes(b'\xa3').take(4)
    b = Bits.from_bytes(b'\x05').drop(4)
    assert (a + b) == Bits.from_bytes(b'\xa5')


@given(...)
def test_add2(bs1: bytes, bs2: bytes) -> None:
    s1 = Bits.from_bytes(bs1)
    s2 = Bits.from_bytes(bs2)
    assert (s1 + s2) == Bits.from_bytes(bs1 + bs2)


@given(spi)
def test_add3(n: int) -> None:
    s = Bits.from_bytes(randbytes(n))
    a, b = s.split_at(n)
    assert (a + b) == s


@given(st.lists(spi), spi, st.lists(spi))
def test_parse_unparse(lst1: List[int], b: int, lst2: List[int]) -> None:
    a = sum(lst1)
    c = sum(lst2)
    bit_length = a + b + c
    (byte_length, rem) = divmod(bit_length, 8)
    byte_length += 1
    bs = Bits.from_bytes(randbytes(byte_length))

    b1 = bs.take(a)
    b2 = bs.drop(a).take(b)
    b3 = bs.drop(a + b).take(c)

    assert Bits.concat([b1, b2, b3]) == bs.take(a + b + c)

    b2new = Bits.from_uinteger(b2.to_uinteger(), a % 8, b)
    assert b2new == b2
    assert Bits.concat([b1, b2new, b3]) == bs.take(a + b + c)


def test_list_show() -> None:
    bs = Bits.from_bytes(b'\xa5\x5a')
    bs1, bs2 = bs.split_at(8)

    assert list(bs1) == [True, False, True, False, False, True, False, True]
    assert list(bs2) == [False, True, False, True, True, False, True, False]

    assert str(bs1) == '10100101'
    assert str(bs2) == '01011010'

    assert str(bs1.drop(1)) == '.0100101'
    assert str(bs1.take(1)) == '1.......'

    assert list(bs1.take(4)) == [True, False, True, False]
    assert list(bs1.drop(4)) == [False, True, False, True]

    assert str(bs.drop(9).take(1)) == '.1......'

    assert str(bs.drop(1)) == '.0100101 01011010'
    assert str(bs.drop(4)) == '....0101 01011010'
