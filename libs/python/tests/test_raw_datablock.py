# asterix raw datablock tests

from binascii import hexlify, unhexlify
from typing import *

from asterix.base import RawDatablock

def allow_category(cat : int) -> Callable[[int], bool]:
    def f(actual : int) -> bool:
        return cat == actual
    return f

def raw_datablock_filter(predicate : Callable[[int], bool], s : bytes) -> bytes:
    datablocks = RawDatablock.parse(s)
    valid_datablocks = [db.unparse() for db in datablocks if predicate(db.category)]
    return b''.join(valid_datablocks)

def reverse_datablocks(s : bytes) -> bytes:
    datablocks = RawDatablock.parse(s)
    lst = [db.unparse() for db in reversed(datablocks)]
    return b''.join(lst)

# e.g.: received from the network
datagram_in = unhexlify(''.join([
    '01000401', # cat1
    '01000401', # cat1
    '02000402', # cat2
    ]))

def test_cat1() -> None:
    result = raw_datablock_filter(allow_category(1), datagram_in)
    assert result == unhexlify(''.join([
        '01000401', # cat1
        '01000401', # cat1
        ]))

def test_cat2() -> None:
    result = raw_datablock_filter(allow_category(2), datagram_in)
    assert result == unhexlify(''.join([
        '02000402', # cat2
        ]))

def test_reverse() -> None:
    result = reverse_datablocks(datagram_in)
    assert result == unhexlify(''.join([
        '02000402', # cat2
        '01000401', # cat1
        '01000401', # cat1
        ]))
