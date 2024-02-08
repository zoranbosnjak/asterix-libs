# asterix item manipulation unit tests

import pytest

from asterix.base import Bits
'''
from generated import *

Cat = Cat_000_1_0
Ref = Ref_000_1_0
'''

'''
TODO:
- test: signed/unsigned... to/from... integer/quantity... conversion
'''

def test_raw() -> None:
    '''
    I = Cat.astspec.uap.var.spec('010')
    i0 = I(0)
    assert i0.to_uinteger() == 0
    assert str(i0.unparse_bits()) == '00000000'
    assert I.parse_bits(i0.unparse_bits()) == (i0, Bits.empty())

    i1 = I(-1)
    assert i1.to_uinteger() == 255
    assert str(i1.unparse_bits()) == '11111111'
    assert I.parse_bits(i1.unparse_bits()) == (i1, Bits.empty())

    raw = Bits.from_bytes(b'\xa5\x5a')
    assert I.parse_bits(raw) == (I(0xa5), raw.drop(8))

    for i in range(8):
        with pytest.raises(ValueError):
            I.parse_bits(raw.take(i))
    '''

'''
def test_table() -> None:
    I = Spec.spec('001')
    assert I(0).table_value == 'Test0'
    assert I(1).table_value == 'Test1'
    for i in range(2,255): assert I(i).table_value == None
    assert I(255).table_value == 'Test255'

    assert str(I(0x01).unparse_bits()) == '00000001'
    assert str(I(0x81).unparse_bits()) == '10000001'

def test_string_ascii() -> None:
    I = Spec.spec('002')
    assert I('testtest').to_string() == 'testtest'

def test_string_icao() -> None:
    I = Spec.spec('003')
    assert I('TESTTEST').to_string() == 'TESTTEST'

def test_string_octal() -> None:
    I = Spec.spec('004')
    assert I('01234567').to_string() == '01234567'

def test_quantity() -> None:
    I = Spec.spec('005')
    for sample in [10.0, 20.0, 30.0]:
        a = I(sample)
        b = I((sample, "°"))
        assert a == b
        c = a.to_quantity()
        delta = abs(c-sample)
        assert delta <= I.quantity.lsb

def test_sacsic() -> None:
    I010 = Spec.spec('010')
    sac = I010.spec('SAC')(0)
    assert str(sac.unparse_bits()) == '00000000'
    sic = I010.spec('SIC')(-1)
    assert str(sic.unparse_bits()) == '11111111'

    i010 = I010({'SAC': 0, 'SIC': sic})
    assert str(i010.unparse_bits()) == '00000000 11111111'

    i010 = I010(0)
    assert str(i010.unparse_bits()) == '00000000 00000000'

    i010 = I010(-1)
    assert str(i010.unparse_bits()) == '11111111 11111111'

    i010 = I010(0x1234)
    assert str(i010.unparse_bits()) == '00010010 00110100'

def test_group() -> None:
    S = Spec.spec('011')

    assert str(S(0).unparse_bits()) == '00000000 00000000'

    i = S(-1)
    assert str(i.unparse_bits()) == '11111111 11111111'
    assert i.to_uinteger() == 0xffff
    assert i.get_item('A').to_uinteger() == 127
    assert i.get_item('B').to_uinteger() == 127

    assert i.get_spares()[0].to_uinteger() == 3
    i = S({'A': -1, 'B': -1})
    assert i.set_item('A', 1) == S({'A': 1, 'B': -1})
    assert i.set_item('B', 1) == S({'A': -1, 'B': 1})
    assert i.set_item('A', 1).set_item('B', 1) == S({'A': 1, 'B': 1})
    assert i.set_item('A', 1).get_item('A').to_uinteger() == 1

    assert i.set_item('A', 1).modify_item('A', lambda x: x.to_uinteger()+1).get_item('A').to_uinteger() == 2

    # parse test
    raw = Bits.from_bytes(b'\x00\x00\x01')
    assert S.parse_bits(raw) == (S(0), raw.drop(16))

def test_extended1() -> None:
    S = Spec.spec('020')

    a = S(-1)
    assert str(a.unparse_bits()) == '11111110'
    b = S((-1,))
    assert a == b

    assert a.get_item('A').to_uinteger() == 127
    assert a.get_item('B') is None
    assert a.get_item('C') is None
    assert a.get_item('D') is None

    assert a.set_item('A', 1).get_item('A').to_uinteger() == 1
    assert a.modify_item('A', lambda x: 1).get_item('A').to_uinteger() == 1
    assert a.modify_item('B', lambda x: 1).get_item('B') is None

    # parse test
    raw1 = Bits.from_bytes(b'\xfe')
    raw2 = Bits.from_bytes(b'\xff')
    assert S.parse_bits(raw1) == (S(-1), Bits.empty())
    with pytest.raises(ValueError):
        S.parse_bits(raw2)

def test_extended2() -> None:
    S = Spec.spec('020')

    a = S((-1, -1))
    assert str(a.unparse_bits()) == '11111111 11111110'
    b = S({'A': -1, 'B': -1, 'C': -1})
    assert a == b

    assert a.get_item('A').to_uinteger() == 127
    assert a.get_item('B').to_uinteger() == 15
    assert a.get_item('C').to_uinteger() == 7
    assert a.get_item('D') == None

    # parse test
    raw = Bits.from_bytes(b'\xff\xfe\x00')
    assert S.parse_bits(raw) == (a, raw.drop(16))

def test_extended3() -> None:
    S = Spec.spec('020')

    a = S((-1, -1, -1))
    assert str(a.unparse_bits()) == '11111111 11111111 11111110'
    a = S((-1, -1, 0x3f))
    assert str(a.unparse_bits()) == '11111111 11111111 01111110'
    b = S({'A': -1, 'B': -1, 'C': -1, 'D': -1})
    assert a == b

    assert a.get_item('A').to_uinteger() == 127
    assert a.get_item('B').to_uinteger() == 15
    assert a.get_item('C').to_uinteger() == 7
    assert a.get_item('D').to_uinteger() == 63

    # parse test
    raw = Bits.from_bytes(b'\xff\xff\x7e')
    assert S.parse_bits(raw) == (a, raw.drop(24))

def test_extended_no_trailing_fx1() -> None:
    S = Spec.spec('021')

    a = S(-1)
    assert str(a.unparse_bits()) == '11111110'
    b = S((-1,))
    assert a == b

    assert a.get_item('A').to_uinteger() == 127
    assert a.get_item('B') == None
    assert a.get_item('C') == None
    assert a.get_item('D') == None

    # parse test
    raw1 = Bits.from_bytes(b'\xfe')
    raw2 = Bits.from_bytes(b'\xff')
    assert S.parse_bits(raw1) == (S(-1), Bits.empty())
    with pytest.raises(ValueError):
        S.parse_bits(raw2)

def test_extended_no_trailing_fx2() -> None:
    S = Spec.spec('021')

    a = S((-1, -1))
    assert str(a.unparse_bits()) == '11111111 11111110'
    b = S({'A': -1, 'B': -1, 'C': -1})
    assert a == b

    assert a.get_item('A').to_uinteger() == 127
    assert a.get_item('B').to_uinteger() == 15
    assert a.get_item('C').to_uinteger() == 7
    assert a.get_item('D') == None

    # parse test
    raw = Bits.from_bytes(b'\xff\xfe\x00')
    assert S.parse_bits(raw) == (a, raw.drop(16))

def test_extended_no_trailing_fx3() -> None:
    S = Spec.spec('021')

    a = S((-1, -1, -1))
    assert str(a.unparse_bits()) == '11111111 11111111 11111111'
    a = S((-1, -1, 0x3f))
    assert str(a.unparse_bits()) == '11111111 11111111 00111111'
    b = S({'A': -1, 'B': -1, 'C': -1, 'D': -1})
    assert a == b

    assert a.get_item('A').to_uinteger() == 127
    assert a.get_item('B').to_uinteger() == 15
    assert a.get_item('C').to_uinteger() == 7
    assert a.get_item('D').to_uinteger() == 63

    # parse test
    raw = Bits.from_bytes(b'\xff\xff\x3f')
    assert S.parse_bits(raw) == (a, raw.drop(24))

def test_repetitive() -> None:
    S = Spec.spec('030')
    I = S.spec()
    i = S([I(1), I(2), 3])
    assert str(i.unparse_bits()) == '00000011 00000001 00000010 00000011'
    assert len(i) == 3
    for (x, subitem) in zip([1,2,3], i):
        assert subitem.to_uinteger() == x
    assert i[0].to_uinteger() == 1
    assert i[1].to_uinteger() == 2
    assert i[2].to_uinteger() == 3

    assert i.append_item(0) == S([1,2,3,0])
    assert i.prepend_item(0) == S([0,1,2,3])

    # parse test
    raw = Bits.from_bytes(b'\x03\x01\x02\x03')
    assert S.parse_bits(raw) == (i, Bits.empty())

def test_repetitive_fx() -> None:
    S = Spec.spec('031')
    I = S.spec()
    i = S([I(1), I(2), 3])
    assert str(i.unparse_bits()) == '00000011 00000101 00000110'
    assert len(i) == 3
    for (x, subitem) in zip([1,2,3], i):
        assert subitem.to_uinteger() == x
    assert i[0].to_uinteger() == 1
    assert i[1].to_uinteger() == 2
    assert i[2].to_uinteger() == 3

    assert i.append_item(0) == S([1,2,3,0])
    assert i.prepend_item(0) == S([0,1,2,3])

    # parse test
    raw = Bits.from_bytes(b'\x03\x05\x06')
    assert S.parse_bits(raw) == (i, Bits.empty())

def test_explicit() -> None:
    S = Spec.spec('040')
    val = 0x01020304
    bs = val.to_bytes(4, 'big')
    i = S(bs)
    assert str(i.unparse_bits()) == '00000101 00000001 00000010 00000011 00000100'
    assert i.raw == bs

    # parse test
    raw = Bits.from_bytes(b'\x05\x01\x02\x03\x04')
    assert S.parse_bits(raw) == (i, Bits.empty())

def test_empty_compound() -> None:
    S = Spec.spec('050')

    rec = S()
    assert bool(rec) == False
    assert rec.unparse_bits() == Bits.empty()

    rec = S({})
    assert bool(rec) == False
    assert rec.unparse_bits() == Bits.empty()

    assert S() == S({})

def test_compound_create() -> None:
    S = Spec.spec('050')

    i1 = S({'C1': 1})
    assert str(i1.unparse_bits()) == '10000000 00000001'

    i2 = S({'C1': 1, 'C2': 2})
    assert str(i2.unparse_bits()) == '10100000 00000001 00000000 00000010'

    i3 = S({'C1': 1, 'C2': 2, 'C3': 3})
    assert str(i3.unparse_bits()) == '10100001 10000000 00000001 00000000 00000010 00000000 00000000 00000011'

    i4 = S({'C3': 3})
    assert str(i4.unparse_bits()) == '00000001 10000000 00000000 00000000 00000011'

    # parse test
    for i in [i1, i2, i3, i4]:
        raw = i.unparse_bits()
        (a,b) = S.parse_bits(raw)
        assert S.parse_bits(raw) == (i, Bits.empty())

def test_ref() -> None:
    S = Ref
    i = S.make_extended({'A': 1, 'B': 2})
    s_bits = i.unparse_bits()
    assert str(s_bits) == '10100000 00000001 00000010'
    assert S.parse_bits(s_bits) == (i, Bits.empty())
    assert S.parse(s_bits.to_bytes()) == i

def test_compound_items() -> None:
    S = Spec.spec('050')

    i1 = S.spec('C1')(1)

    assert S().set_item('C1', i1) == S({'C1': 1})
    assert S().set_item('C1', 1).del_item('C1') == S()
    assert S().set_item('C1', 1).set_item('C2', 2).set_item('C3', 3) == S({'C1': 1, 'C2': 2, 'C3': 3})

    assert S().set_item('C1', i1).get_item('C1') == i1
    assert S().set_item('C1', i1).get_item('C2') == None
    assert S().set_item('C1', i1).get_item('C3') == None

    assert S().modify_item('C1', lambda x: 1) == S()
    assert S({'C1': 1}).modify_item('C1', lambda x: 2) == S({'C1': 2})

def test_record() -> None:
    rec = Spec.make_record({
        '010': {'SAC': 1, 'SIC': 2}
        })
    assert str(rec.unparse_bits()) == '00000010 00000001 00000010'
    assert rec.get_item('010').get_item('SAC').to_uinteger() == 1
    assert rec.get_item('010').get_item('SIC').to_uinteger() == 2
    assert rec.get_item('020') == None

    assert Spec.parse_bits(rec.unparse_bits()) == (rec, Bits.empty())

def test_category() -> None:
    S0 = CAT_000_1_0
    S2 = CAT_002_1_0
    rec = S2.make_record_unsafe('uap0', {'000': 0})
    db = S2.make_datablock(rec)
    raw_datablocks = RawDatablock.parse(db.unparse())
    with pytest.raises(ValueError):   # wrong category
        raw = S0.parse(raw_datablocks[0])
'''
