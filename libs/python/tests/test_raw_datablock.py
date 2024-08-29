# asterix raw datablock tests

from binascii import hexlify, unhexlify
from typing import *

from asterix.base import Bits, RawDatablock


def allow_category(cat: int) -> Callable[[int], bool]:
    def f(actual: int) -> bool:
        return cat == actual
    return f


def raw_datablock_filter(predicate: Callable[[int], bool], s: Bits) -> Bits:
    datablocks = RawDatablock.parse(s)
    if isinstance(datablocks, ValueError):
        return s
    lst = [db.unparse() for db in datablocks if predicate(db.get_category())]
    return Bits.concat(lst)


def reverse_datablocks(s: Bits) -> Bits:
    datablocks = RawDatablock.parse(s)
    if isinstance(datablocks, ValueError):
        return s
    lst = [db.unparse() for db in reversed(datablocks)]
    return Bits.concat(lst)


# e.g.: received from the network
datagram_in = unhexlify(''.join([
    '01000401',  # cat1
    '01000401',  # cat1
    '02000402',  # cat2
]))


def test_parse_ok() -> None:
    datablocks = RawDatablock.parse(Bits.from_bytes(datagram_in))
    assert not isinstance(datablocks, ValueError)


def test_parse_nok() -> None:
    # drop first octet
    datablocks = RawDatablock.parse(Bits.from_bytes(datagram_in[1:]))
    assert isinstance(datablocks, ValueError)


def test_cat1() -> None:
    result = raw_datablock_filter(
        allow_category(1),
        Bits.from_bytes(datagram_in))
    assert result.to_bytes() == unhexlify(''.join([
        '01000401',  # cat1
        '01000401',  # cat1
    ]))


def test_cat2() -> None:
    result = raw_datablock_filter(
        allow_category(2),
        Bits.from_bytes(datagram_in))
    assert result.to_bytes() == unhexlify(''.join([
        '02000402',  # cat2
    ]))


def test_reverse() -> None:
    result = reverse_datablocks(Bits.from_bytes(datagram_in))
    assert result.to_bytes() == unhexlify(''.join([
        '02000402',  # cat2
        '01000401',  # cat1
        '01000401',  # cat1
    ]))
