# asterix item manipulation unit tests

from binascii import hexlify, unhexlify
from typing import *
import pytest

from . generated import *


def test_raises() -> None:
    Cat0 = Cat_000_1_0
    Cat0.cv_record.create({})
    Cat0.cv_record.create({'010': 1}),
    with pytest.raises(ValueError):
        Cat0.cv_record.create({'nonexistingitem': 1})  # type: ignore


def approximately(err: float, a: float, b: float) -> bool:
    return (abs(b - a) / a) < err


def test_rule_variation_context_free() -> None:
    Var = Cat_000_1_0.cv_record.spec('000')
    obj = Var.create(0x01)
    assert obj.unparse() == Bits.from_bytes(unhexlify('01'))
    assert obj.as_uint() == 0x01
    Rv = Cat_000_1_0.cv_record.spec('000')
    obj2 = Rv.create(0x01)
    assert obj2.unparse() == Bits.from_bytes(unhexlify('01'))
    assert obj2.as_uint() == 0x01


def test_rule_variation_dependent() -> None:
    Var = Cat_000_1_0.cv_record.spec('032').cv_rule.\
        cv_variation.spec('CC').cv_rule.cv_variation.\
        spec('CP').variation((1, 2))
    obj = Var.create(1)
    assert obj.as_uint() == 1


def test_rule_content_context_free() -> None:
    Var = Cat_000_1_0.cv_record.\
        spec('030').cv_rule.cv_variation.spec('IM').cv_variation
    obj = Var.create(1)
    assert obj.as_uint() == 1


def test_rule_content_dependent1() -> None:
    Var = Cat_000_1_0.cv_record.spec('030').\
        cv_rule.cv_variation.spec('IAS').cv_variation
    obj = Var.create((None, 1))
    assert obj.as_uint() == 1
    assert obj.rule.content(None).as_uint() == 1
    obj0 = Var.create(((0,), (1.2, 'NM/s')))
    assert approximately(
        0.01, 1.2, obj0.rule.content(
            (0,)).as_quantity())
    assert approximately(
        0.01, 1.2, obj0.rule.content(
            (0,)).as_quantity('NM/s'))
    obj1 = Var.create(((1,), (0.8, 'Mach')))
    assert approximately(
        0.01, 0.8, obj1.rule.content(
            (1,)).as_quantity('Mach'))


def test_rule_content_dependent2() -> None:
    Var = Cat_000_1_0.cv_record.spec('031').cv_rule.cv_variation
    obj = Var.create((None, 1))
    assert obj.rule.content(None).as_uint() == 1
    obj11 = Var.create(((1, 1), (12.3, 'unit1')))
    assert approximately(
        0.02, 12.3, obj11.rule.content(
            (1, 1)).as_quantity('unit1'))
    obj12 = Var.create(((1, 2), (13.4, 'unit2')))
    assert approximately(
        0.01, 13.4, obj12.rule.content(
            (1, 2)).as_quantity('unit2'))
    obj21 = Var.create(((2, 1), (10.0, 'unit3')))
    assert approximately(
        0.01, 10.0, obj21.rule.content(
            (2, 1)).as_quantity('unit3'))


def test_content_raw() -> None:
    Var = Cat_000_1_0.cv_record.\
        spec('010').cv_rule.cv_variation.spec('SAC').cv_variation
    obj = Var.create(0x01)
    assert obj.unparse() == Bits.from_bytes(unhexlify('01'))
    assert obj.as_uint() == 0x01


def test_content_table() -> None:
    Var = Cat_000_1_0.cv_record.spec('000').cv_rule.cv_variation
    obj = Var.create(0x01)
    assert obj.unparse() == Bits.from_bytes(unhexlify('01'))
    assert obj.as_uint() == 0x01


def test_content_string_ascii() -> None:
    Var = Cat_000_1_0.cv_record.spec('020').\
        cv_rule.cv_variation.spec('S1').cv_rule.cv_variation
    obj = Var.create(0x01)
    assert obj.unparse() == Bits.from_bytes(unhexlify('00000000000001'))
    assert obj.as_uint() == 1
    sample = 'test'
    obj = Var.create(sample)
    assert obj.unparse() == Bits.from_bytes(unhexlify('74657374202020'))
    assert obj.as_uint() == 0x74657374202020
    assert obj.rule.as_uint() == obj.as_uint()
    assert obj.rule.content.as_uint() == obj.as_uint()
    assert obj.content.as_uint() == obj.as_uint()
    assert obj.rule.content.as_string().rstrip() == sample


def test_content_string_icao() -> None:
    Var = Cat_000_1_0.cv_record.spec('020').\
        cv_rule.cv_variation.spec('S2').cv_rule.cv_variation
    obj = Var.create(0x01)
    assert obj.as_uint() == 1
    assert obj.unparse() == Bits.from_bytes(unhexlify('000000000001'))
    sample = 'S5LJ'
    obj = Var.create(sample)
    assert obj.unparse() == Bits.from_bytes(unhexlify('4F530A820820'))
    assert obj.as_uint() == 0x4F530A820820
    assert obj.content.as_uint() == obj.as_uint()
    assert obj.content.as_string().rstrip() == sample


def test_content_string_octal() -> None:
    Var = Cat_000_1_0.cv_record.spec('020').\
        cv_rule.cv_variation.spec('S3').cv_rule.cv_variation
    obj = Var.create(0x01)
    assert obj.unparse() == Bits.from_bytes(unhexlify('000001'))
    assert obj.as_uint() == 1
    sample = '1234'
    obj = Var.create(sample)
    assert obj.unparse() == Bits.from_bytes(unhexlify('29C000'))
    assert obj.as_uint() == 0x29C000
    assert obj.content.as_uint() == obj.as_uint()
    assert obj.content.as_string() == sample + '0000'


def test_content_integer_unsigned() -> None:
    Var = Cat_000_1_0.cv_record.spec('020').\
        cv_rule.cv_variation.spec('I1').cv_rule.cv_variation
    obj = Var.create(0x01)
    assert obj.unparse() == Bits.from_bytes(unhexlify('01'))
    assert obj.as_uint() == 1
    obj = Var.create(-1)
    assert obj.unparse() == Bits.from_bytes(unhexlify('FF'))
    assert obj.as_uint() == 0xFF
    assert obj.content.as_uint() == obj.as_uint()
    assert obj.content.as_integer() == 0xFF


def test_content_integer_signed() -> None:
    Var = Cat_000_1_0.cv_record.spec('020').\
        cv_rule.cv_variation.spec('I2').cv_rule.cv_variation
    obj = Var.create(0x01)
    assert obj.unparse() == Bits.from_bytes(unhexlify('01'))
    obj = Var.create(-1)
    assert obj.unparse() == Bits.from_bytes(unhexlify('FF'))
    assert obj.as_uint() == 0xFF
    assert obj.content.as_uint() == obj.as_uint()
    assert obj.content.as_integer() == -1


def test_content_quantity_unsigned() -> None:
    Var = Cat_000_1_0.cv_record.spec('020').\
        cv_rule.cv_variation.spec('Q3').cv_rule.cv_variation
    obj = Var.create(0x01)
    assert obj.unparse() == Bits.from_bytes(unhexlify('0001'))
    sample = 123.4
    obj = Var.create(sample)
    assert obj.as_uint() == 0x007B
    assert obj.unparse() == Bits.from_bytes(unhexlify('007B'))
    obj = Var.create((sample, 'kt'))
    assert obj.unparse() == Bits.from_bytes(unhexlify('007B'))
    assert obj.content.as_uint() == obj.as_uint()
    assert round(obj.content.as_quantity()) == round(sample)
    assert round(obj.content.as_quantity('kt')) == round(sample)


def test_content_quantity_signed() -> None:
    Var = Cat_000_1_0.cv_record.spec('020').\
        cv_rule.cv_variation.spec('Q2LON').cv_rule.cv_variation
    obj = Var.create(0x01)
    one = obj.content.as_quantity()
    assert obj.unparse() == Bits.from_bytes(unhexlify('000001'))
    sample = 123.4
    obj = Var.create(sample)
    assert obj.as_uint() == round(sample / (180 / pow(2, 23)))
    obj = Var.create((sample, '°'))
    assert obj.as_uint() == round(sample / (180 / pow(2, 23)))
    assert obj.content.as_uint() == obj.as_uint()
    assert round(obj.content.as_quantity()) == round(sample)
    assert round(obj.content.as_quantity('°')) == round(sample)
    obj = Var.create(0xffffff)
    assert obj.unparse() == Bits.from_bytes(unhexlify('ffffff'))
    assert obj.content.as_quantity() < 0.0
    assert obj.content.as_quantity() == -one


def test_content_bds_with_address() -> None:
    Var = Cat_000_1_0.cv_record.spec('020').\
        cv_rule.cv_variation.spec('B1').cv_rule.cv_variation
    obj = Var.create(0x01)
    assert obj.unparse() == Bits.from_bytes(unhexlify('0000000000000001'))
    assert obj.as_uint() == 0x01
    assert obj.content.as_uint() == obj.as_uint()


def test_group1() -> None:
    Var = Cat_000_1_0.cv_record.spec('010').cv_rule.cv_variation
    obj1 = Var.create(0x0102)
    assert obj1.unparse() == Bits.from_bytes(unhexlify('0102'))
    assert obj1.as_uint() == 0x0102
    obj2 = Var.create((0x01, 0x02))
    assert obj2.unparse() == Bits.from_bytes(unhexlify('0102'))
    assert obj2.as_uint() == 0x0102
    obj3 = Var.create((('SAC', 0x01), 0x02))
    assert obj3.unparse() == Bits.from_bytes(unhexlify('0102'))
    assert obj3.as_uint() == 0x0102
    obj4 = Var.create((('SAC', 0x01), ('SIC', 0x02)))
    assert obj4.unparse() == Bits.from_bytes(unhexlify('0102'))
    assert obj4.as_uint() == 0x0102
    assert obj4.get_item('SAC').as_uint() == 0x01
    assert obj4.get_item('SIC').as_uint() == 0x02
    assert obj4.get_spares() == []


def test_group2() -> None:
    T = Cat_000_1_0.cv_record.spec('040')
    obj = T.create((1, 2, ('I2', 3), 4))
    assert obj.unparse() == Bits.from_bytes(unhexlify('030604'))
    assert obj.variation.get_item('I1').as_uint() == 1
    assert obj.variation.get_item('I2').as_uint() == 3
    assert obj.variation.get_spares() == [2, 4]


def test_group3() -> None:
    T = Cat_000_1_0.cv_record.spec('101')
    obj = T.create(0x12)
    assert obj.unparse() == Bits.from_bytes(unhexlify('12'))


def test_extended1() -> None:
    T = Cat_000_1_0.cv_record.spec('053')
    bs = Bits.from_bytes(unhexlify('80'))
    result = T.cv_rule.cv_variation.parse(bs)
    assert not isinstance(result, ValueError)
    (obj, bs2) = result
    assert bs2.null()
    obj2 = T.create(((1, ('I2', 2), None),))
    assert obj2.unparse() == Bits.from_bytes(unhexlify('84'))
    var2 = obj2.variation
    assert var2.get_item('I1').variation.as_uint() == 1
    assert var2.get_item('I2').as_uint() == 2
    assert var2.get_item('I3') is None
    assert var2.get_item('I4') is None
    assert var2.get_item('I5') is None
    assert var2.get_spares() == []
    obj2a = T.create((0x84 // 2,))
    assert obj2a.unparse() == obj2.unparse()
    obj3 = T.create(((('I1', 1), 2, None),))
    assert obj2.unparse() == obj3.unparse()
    assert obj3.variation.get_spares() == []
    obj4 = T.create(((1, 2, None), (1, 0, 2, None)))
    assert obj4.unparse() == Bits.from_bytes(unhexlify('8544'))
    assert obj4.variation.get_spares() == [0]
    obj5 = T.create((1, 2))
    assert obj5.unparse() == Bits.from_bytes(unhexlify('0304'))
    obj6 = T.create((1, 2, 3))
    assert obj6.unparse() == Bits.from_bytes(unhexlify('030506'))
    obj6a = T.create((1, 2, (3, None)))
    assert obj6a.unparse() == obj6.unparse()
    var6 = obj6a.variation
    assert var6.get_item('I3') is not None
    assert var6.get_item('I4') is not None
    assert var6.get_item('I5') is not None


def test_extended2() -> None:
    T = Cat_000_1_0.cv_record.spec('054')
    bs = Bits.from_bytes(unhexlify('80'))
    result = T.cv_rule.cv_variation.parse(bs)
    assert not isinstance(result, ValueError)
    (obj, bs2) = result
    assert bs2.null()
    obj2 = T.create(((1, ('I2', 2), None),))
    assert obj2.unparse() == Bits.from_bytes(unhexlify('84'))
    obj2a = T.create((0x84 // 2,))
    assert obj2a.unparse() == obj2.unparse()
    obj3 = T.create(((('I1', 1), 2, None),))
    assert obj2.unparse() == obj3.unparse()
    obj4 = T.create(((1, 2, None), (1, 0, 2, None)))
    assert obj4.unparse() == Bits.from_bytes(unhexlify('8544'))
    obj5 = T.create((1, 2))
    assert obj5.unparse() == Bits.from_bytes(unhexlify('0304'))
    obj6 = T.create((1, 2, 3))
    assert obj6.unparse() == Bits.from_bytes(unhexlify('030503'))


def test_extended3() -> None:
    T = Cat_000_1_0.cv_record.spec('102')
    obj = T.create((1, 2))
    assert obj.unparse() == Bits.from_bytes(unhexlify('0304'))
    obj = T.create(((1, None), (2, None)))
    assert obj.unparse() == Bits.from_bytes(unhexlify('0304'))
    obj = T.create(((1, None), ((0, 0, 2), None)))
    assert obj.unparse() == Bits.from_bytes(unhexlify('0304'))
    obj = T.create(((1, None), ((('SG1', 0), 0, 2), None)))
    assert obj.unparse() == Bits.from_bytes(unhexlify('0304'))


def test_repetitive1() -> None:
    T = Cat_000_1_0.cv_record.spec('061')
    obj = T.create([1, 2, 3])
    assert obj.unparse() == Bits.from_bytes(unhexlify('03010203'))
    lst = obj.variation.get_list()
    assert len(lst) == 3
    assert lst[0].as_uint() == 1
    assert lst[1].as_uint() == 2
    assert lst[2].as_uint() == 3


def test_repetitive2() -> None:
    T = Cat_000_1_0.cv_record.spec('062')
    obj = T.create([1, (('I1', 2), 3), 4])
    assert obj.unparse() == Bits.from_bytes(unhexlify('03000102030004'))
    lst = obj.variation.get_list()
    assert len(lst) == 3
    assert lst[0].as_uint() == 1
    assert lst[1].as_uint() == 0x0203
    assert lst[2].as_uint() == 4


def test_repetitive3() -> None:
    T = Cat_000_1_0.cv_record.spec('063')
    obj = T.create([1, 2, 3])
    assert obj.unparse() == Bits.from_bytes(unhexlify('030506'))
    lst = obj.variation.get_list()
    assert len(lst) == 3
    assert lst[0].as_uint() == 1
    assert lst[1].as_uint() == 2
    assert lst[2].as_uint() == 3


def test_explicit1() -> None:
    T = Cat_000_1_0.cv_record.spec('071')
    obj = T.create(unhexlify('010203'))
    assert obj.unparse() == Bits.from_bytes(unhexlify('04010203'))
    s = obj.variation.get_bytes()
    assert s == unhexlify('010203')


def test_explicit2() -> None:
    Ref = Ref_000_1_0
    obj = Ref.cv_expansion.create({
        'I1': 1,
        'I2': 2,
    })
    bs = obj.unparse()
    result = Ref.cv_expansion.parse(bs)
    assert not isinstance(result, ValueError)
    (obj2, bs2) = result
    assert bs2.null()
    i1 = obj2.get_item('I1')
    assert i1 is not None
    i2 = obj2.get_item('I2')
    assert i2 is not None
    assert i1.as_uint() == 1
    assert i2.as_uint() == 2


def test_explicit3a() -> None:
    Cat = Cat_000_1_0
    Ref = Ref_000_1_1 # without FX
    re = Ref.cv_expansion.create({
        'I1': 1,
        'I2': 2,
        'I3': 3,
    })
    r = Cat.cv_record.create({
        '010': 0x0102,
        '072': re.unparse().to_bytes(),
    })
    result = Cat.cv_record.parse(ParsingMode.StrictParsing, r.unparse())
    assert not isinstance(result, ValueError)
    (r2, bs2) = result
    assert bs2.null()
    i072 = r2.get_item('072')
    assert i072 is not None
    s = i072.variation.get_bytes()
    assert hexlify(s) == b'9020010203'
    result3 = Ref.cv_expansion.parse(Bits.from_bytes(s))
    assert not isinstance(result3, ValueError)
    (obj3, bs3) = result3
    assert bs3.null()
    i1 = obj3.get_item('I1')
    i2 = obj3.get_item('I2')
    i3 = obj3.get_item('I3')
    assert i1 is not None
    assert i1.as_uint() == 1
    assert i2 is not None
    assert i2.as_uint() == 2
    assert i3 is not None
    assert i3.as_uint() == 3


def test_explicit3b() -> None:
    Cat = Cat_000_1_0
    Ref = Ref_000_1_2 # with FX
    re = Ref.cv_expansion.create({
        'I1': 1,
        'I2': 2,
        'I3': 3,
    })
    r = Cat.cv_record.create({
        '010': 0x0102,
        '072': re.unparse().to_bytes(),
    })
    result = Cat.cv_record.parse(ParsingMode.StrictParsing, r.unparse())
    assert not isinstance(result, ValueError)
    (r2, bs2) = result
    assert bs2.null()
    i072 = r2.get_item('072')
    assert i072 is not None
    s = i072.variation.get_bytes()
    assert hexlify(s) == b'9110010203'
    result3 = Ref.cv_expansion.parse(Bits.from_bytes(s))
    assert not isinstance(result3, ValueError)
    (obj3, bs3) = result3
    assert bs3.null()
    i1 = obj3.get_item('I1')
    i2 = obj3.get_item('I2')
    i3 = obj3.get_item('I3')
    assert i1 is not None
    assert i1.as_uint() == 1
    assert i2 is not None
    assert i2.as_uint() == 2
    assert i3 is not None
    assert i3.as_uint() == 3


def test_explicit3c() -> None:
    Cat = Cat_000_1_0
    Ref = Ref_000_1_2 # with FX
    re = Ref.cv_expansion.create({
        'I1': 1,
        'I2': 2,
    })
    r = Cat.cv_record.create({
        '010': 0x0102,
        '072': re.unparse().to_bytes(),
    })
    result = Cat.cv_record.parse(ParsingMode.StrictParsing, r.unparse())
    assert not isinstance(result, ValueError)
    (r2, bs2) = result
    assert bs2.null()
    i072 = r2.get_item('072')
    assert i072 is not None
    s = i072.variation.get_bytes()
    assert hexlify(s) == b'900102'
    result3 = Ref.cv_expansion.parse(Bits.from_bytes(s))
    assert not isinstance(result3, ValueError)
    (obj3, bs3) = result3
    assert bs3.null()
    i1 = obj3.get_item('I1')
    i2 = obj3.get_item('I2')
    i3 = obj3.get_item('I3')
    assert i1 is not None
    assert i1.as_uint() == 1
    assert i2 is not None
    assert i2.as_uint() == 2
    assert i3 is None


def test_compound3() -> None:
    T = Cat_000_1_0.cv_record.spec('093')
    with pytest.raises(ValueError):
        obj = T.create({'nonexistingitem': 1})  # type: ignore
    obj = T.create({})
    assert obj.unparse() == Bits.from_bytes(unhexlify(''))
    obj1 = T.create({'I1': 1})
    assert obj1.variation.get_item('I1') is not None
    assert obj1.variation.get_item('I2') is None
    assert obj1.variation.get_item('I3') is None
    assert obj1.unparse() == Bits.from_bytes(unhexlify('8001'))
    obj12 = T.create({'I1': 1, 'I2': 2})
    assert obj12.variation.get_item('I1') is not None
    assert obj12.variation.get_item('I2') is not None
    assert obj12.variation.get_item('I3') is None
    assert obj12.unparse() == Bits.from_bytes(unhexlify('A00102'))
    obj13 = T.create({'I1': 1, 'I3': 3})
    assert obj13.variation.get_item('I1') is not None
    assert obj13.variation.get_item('I2') is None
    assert obj13.variation.get_item('I3') is not None
    assert obj13.unparse() == Bits.from_bytes(unhexlify('81800103'))
    obj123 = T.create({'I1': 1, 'I2': 2, 'I3': 3})
    assert obj123.variation.get_item('I1') is not None
    assert obj123.variation.get_item('I2') is not None
    assert obj123.variation.get_item('I3') is not None
    assert obj123.unparse() == Bits.from_bytes(unhexlify('A180010203'))


def test_compound_set() -> None:
    T = Cat_000_1_0.cv_record.spec('093')
    obj1 = T.create({'I1': 1})
    var12 = obj1.variation.set_item('I2', 2)
    obj12 = T.create({'I1': 1, 'I2': 2})
    assert var12.unparse() == obj12.unparse()


def test_compound_del() -> None:
    T = Cat_000_1_0.cv_record.spec('093')
    obj123 = T.create({'I1': 1, 'I2': 2, 'I3': 3})
    var13 = obj123.variation.del_item('I2')
    obj13 = T.create({'I1': 1, 'I3': 3})
    assert var13.unparse() == obj13.unparse()


def test_record() -> None:
    T = Cat_000_1_0.cv_record
    r0 = T.create({
    })
    assert r0.unparse().to_bytes() == b''
    assert r0.get_item('000') is None

    r1 = T.create({
        '000': 0x03,
    })
    assert r1.unparse() == Bits.from_bytes(unhexlify('4003'))
    r1_000 = r1.get_item('000')
    assert r1_000 is not None
    r1_000.as_uint() == 0x03

    r2a = T.create({
        '010': (('SAC', 0x01), ('SIC', 0x02)),
        '000': 0x03,
    })
    assert r2a.get_item('000') is not None
    r2a_010 = r2a.get_item('010')
    assert r2a_010 is not None
    assert r2a_010.variation.get_item('SAC').as_uint() == 0x01
    assert r2a_010.variation.get_item('SIC').as_uint() == 0x02

    r2b = T.create({
        '010': (('SAC', 0x01), 0x02),
        '000': 0x03,
    })
    r2c = T.create({
        '010': 0x0102,
        '000': 0x03,
    })
    for r in [r2a, r2b, r2c]:
        assert r.unparse() == Bits.from_bytes(unhexlify('C0010203'))

    r3 = T.create({
        '032': {
            'I1': 0x11,
            'CC': (('TID', 5), ('CP', 3), ('CS', 1))
        },
    })
    assert r3.unparse().to_bytes() == unhexlify('04C01157')

    with_rfs = T.create({
        '000': 0x03,
    }, [
        ('000', 0xAA),
        ('000', 0x55),
        ('010', 0x1234),
        ('053', (1, 2)),
        ('054', (1, 2)),
        ('061', [0xFF]),
    ])
    bs = with_rfs.unparse()
    assert bs == Bits.from_bytes(unhexlify(
        '410108030602AA02550112340A03040B03040C01FF'))

    assert with_rfs.get_item('000') is not None
    assert with_rfs.get_item('010') is None

    assert len(with_rfs.get_rfs_item('000')) == 2
    assert len(with_rfs.get_rfs_item('010')) == 1
    assert len(with_rfs.get_rfs_item('020')) == 0
    assert len(with_rfs.get_rfs_item('053')) == 1

    # Check parsing (with RFS)
    result = T.parse(ParsingMode.StrictParsing, bs)
    assert not isinstance(result, ValueError)
    (with_rfs2, bs2) = result
    assert bs2.null()
    assert with_rfs2.unparse() == bs

def test_record_multiple_rfs() -> None:
    T = Cat_003_1_0.cv_record

    def check(r: Any, expected: str) -> None:
        bs = r.unparse()
        assert hexlify(bs.to_bytes()) == \
            hexlify(Bits.from_bytes(unhexlify(expected)).to_bytes())
        result = T.parse(ParsingMode.StrictParsing, bs)
        assert not isinstance(result, ValueError)
        (r2, bs2) = result
        assert bs2.null()
        assert r2.unparse() == bs

    r0 = T.create({
        '010': 0x01,
    })
    check(r0, '8001')

    r1 = T.create({
        '010': 0x01,
    }, [
        ('101', 0xAA),
        ('102', 0x55),
       ],
    )
    check(r1, '90010202aa0355')

    r2 = T.create({
        '010': 0x01,
    }, [
        ('101', 0xAA),
        ('102', 0x55),
       ],
       [
        ('201', 0xF1),
        ('202', 0xF2),
       ],
    )
    check(r2, '94010202aa0355020500f10700f2')

    r3 = T.create({
        '010': 0x01,
    }, None,
       [
        ('201', 0xF1),
        ('202', 0xF2),
       ],
    )
    check(r3, '8401020500f10700f2')

    r4 = T.create({
        '010': 0x01,
    }, [
       ],
       [
        ('201', 0xF1),
        ('202', 0xF2),
       ],
    )
    check(r4, '940100020500f10700f2')


def test_record_set_item() -> None:
    T = Cat_000_1_0.cv_record
    r0 = T.create({
    }).set_item('000', 0x03)

    r1 = T.create({
        '000': 0x03,
    })
    assert hexlify(r1.unparse().to_bytes()) == hexlify(r0.unparse().to_bytes())


def test_record_del_item() -> None:
    T = Cat_000_1_0.cv_record

    r0 = T.create({
        '010': (('SAC', 0x01), 0x02),
        '000': 0x03,
    }).del_item('010')

    r1 = T.create({
        '000': 0x03,
    })
    assert hexlify(r1.unparse().to_bytes()) == hexlify(r0.unparse().to_bytes())


def test_multiple_uaps() -> None:
    T = Cat_001_1_0.cv_uap
    TPlot = T.spec('plot')
    TTrack = T.spec('track')
    rec_plot = TPlot.create({
        '010': 1,
        '020': ((1,)),
        '031': 0x03,
    })
    assert rec_plot.unparse() == Bits.from_bytes(unhexlify('E000010203'))
    rec_track = TTrack.create({
        '010': 1,
        '020': ((2,)),
        '032': 0x04,
    })
    assert rec_track.unparse() == Bits.from_bytes(unhexlify('D00001040004'))


def test_create_datagram() -> None:
    Cat0 = Cat_000_1_0
    Cat1 = Cat_001_1_0
    db0 = Cat0.create([
        Cat0.cv_record.create({'010': 1}),
        Cat0.cv_record.create({'010': 2}),
    ])
    db1 = Cat1.create([
        Cat1.cv_uap.spec('plot').create({'010': 1}),
        Cat1.cv_uap.spec('track').create({'010': 2}),
    ])
    datagram = db0.unparse() + db1.unparse()
    s = datagram.to_bytes()
    assert s == unhexlify('000009800001800002010009800001800002')


def test_parse1() -> None:
    T = Cat_000_1_0.cv_record
    for sample in [
        '4003',
        'C0010203',
        '04C01157',
        '410108030502000112340A03040B03040C01FF',
        '410108030602AA02550112340A03040B03040C01FF'
    ]:
        bs = Bits.from_bytes(unhexlify(sample))
        result = T.parse(ParsingMode.StrictParsing, bs)
        assert not isinstance(result, ValueError)
        (r, bs2) = result
        assert bs2.null()
        assert r.unparse() == bs


def test_parse2() -> None:
    Cat0 = Cat_000_1_0
    r = Cat0.cv_record.create({'010': 1})
    bs = Cat0.create([r, r, r]).unparse()
    dbs = RawDatablock.parse(bs)
    assert not isinstance(dbs, ValueError)
    assert len(dbs) == 1
    db = dbs[0]
    result = Cat0.cv_uap.parse(db.get_raw_records())
    assert not isinstance(result, ValueError)
    for i in result:
        assert i.unparse() == r.unparse()


def test_parse3() -> None:
    Cat1 = Cat_001_1_0

    rec_plot = Cat1.cv_uap.spec('plot').create({
        '010': 0x0102,
        '020': ((('TYP', 0), 0, None),),
        '031': 0,
    })
    rec_track = Cat1.cv_uap.spec('track').create({
        '010': 0x0102,
        '020': ((('TYP', 1), 0, None),),
        '032': 0,
    })

    class _plots:
        bs1 = Cat1.create([
            rec_plot, rec_plot,
        ]).unparse()

        dbs = RawDatablock.parse(bs1)
        assert not isinstance(dbs, ValueError)
        assert len(dbs) == 1
        db = dbs[0]
        bs2 = db.get_raw_records()

        # try to parse as 'plots'
        result1 = Cat1.cv_uap.parse('plot', bs2)
        assert not isinstance(result1, ValueError)
        assert len(result1) == 2
        for r1 in result1:
            assert r1.unparse() == rec_plot.unparse()

        # try to parse as 'tracks'
        result2 = Cat1.cv_uap.parse('track', bs2)
        assert isinstance(result2, ValueError)

        # try to parse as any defined UAP
        result3 = Cat1.cv_uap.parse_any_uap(bs2)
        assert len(result3) == 1
        for r2 in result3[0]:
            assert r2.unparse() == rec_plot.unparse()

    class _tracks:
        bs1 = Cat1.create([
            rec_track, rec_track, rec_track,
        ]).unparse()

        dbs = RawDatablock.parse(bs1)
        assert not isinstance(dbs, ValueError)
        assert len(dbs) == 1
        db = dbs[0]
        bs2 = db.get_raw_records()

        # try to parse as 'plots'
        result1 = Cat1.cv_uap.parse('plot', bs2)
        assert isinstance(result1, ValueError)

        # try to parse as 'tracks'
        result2 = Cat1.cv_uap.parse('track', bs2)
        assert not isinstance(result2, ValueError)
        assert len(result2) == 3
        for r1 in result2:
            assert r1.unparse() == rec_track.unparse()

        # try to parse as any defined UAP
        result3 = Cat1.cv_uap.parse_any_uap(bs2)
        assert len(result3) == 1
        for r2 in result3[0]:
            assert r2.unparse() == rec_track.unparse()

    class _mixed:
        records = [
            rec_plot, rec_plot,
            rec_track, rec_track, rec_track,
        ]
        bs1 = Cat1.create(records).unparse()  # type: ignore

        dbs = RawDatablock.parse(bs1)
        assert not isinstance(dbs, ValueError)
        assert len(dbs) == 1
        db = dbs[0]
        bs2 = db.get_raw_records()

        # try to parse as 'plots'
        result1 = Cat1.cv_uap.parse('plot', bs2)
        assert isinstance(result1, ValueError)

        # try to parse as 'tracks'
        result2 = Cat1.cv_uap.parse('track', bs2)
        assert isinstance(result2, ValueError)

        # try to parse as any defined UAP
        result3 = Cat1.cv_uap.parse_any_uap(bs2)
        assert len(result3) == 1
        for (r1, r2) in zip(records, result3[0]):
            assert r1.unparse() == r2.unparse()


def test_parse4() -> None:
    Cat1 = Cat_001_1_0
    rec_plot = Cat1.cv_uap.spec('plot').create({'010': 0x0102, })
    rec_track = Cat1.cv_uap.spec('track').create({'010': 0x0102, })

    # from '010' item, both records look the same
    assert rec_plot.unparse() == rec_track.unparse()
    assert Cat1.create([rec_plot]).unparse() \
        == Cat1.create([rec_track]).unparse()

    class _one:
        bs1 = Cat1.create([rec_plot]).unparse()
        dbs = RawDatablock.parse(bs1)
        assert not isinstance(dbs, ValueError)
        assert len(dbs) == 1
        db = dbs[0]
        bs2 = db.get_raw_records()
        results = Cat1.cv_uap.parse_any_uap(bs2)
        assert len(results) == 2
        for result in results:
            assert len(result) == 1
        result1 = Cat1.cv_uap.parse('plot', bs2)
        assert not isinstance(result1, ValueError)
        assert len(result1) == 1
        result2 = Cat1.cv_uap.parse('track', bs2)
        assert not isinstance(result2, ValueError)
        assert len(result2) == 1

    class _two:
        bs1 = Cat1.create([rec_plot, rec_plot]).unparse()
        dbs = RawDatablock.parse(bs1)
        assert not isinstance(dbs, ValueError)
        assert len(dbs) == 1
        db = dbs[0]
        bs2 = db.get_raw_records()
        results = Cat1.cv_uap.parse_any_uap(bs2)
        assert len(results) == 4
        for result in results:
            assert len(result) == 2
        result1 = Cat1.cv_uap.parse('plot', bs2)
        assert not isinstance(result1, ValueError)
        assert len(result1) == 2
        result2 = Cat1.cv_uap.parse('track', bs2)
        assert not isinstance(result2, ValueError)
        assert len(result2) == 2

    class _three:
        bs1 = Cat1.create([rec_plot, rec_plot, rec_plot]).unparse()
        dbs = RawDatablock.parse(bs1)
        assert not isinstance(dbs, ValueError)
        assert len(dbs) == 1
        db = dbs[0]
        bs2 = db.get_raw_records()
        results = Cat1.cv_uap.parse_any_uap(bs2)
        assert len(results) == pow(2, 3)
        for result in results:
            assert len(result) == 3
        result1 = Cat1.cv_uap.parse('plot', bs2)
        assert not isinstance(result1, ValueError)
        assert len(result1) == 3
        result2 = Cat1.cv_uap.parse('track', bs2)
        assert not isinstance(result2, ValueError)
        assert len(result2) == 3

def test_parse5() -> None:
    """Non-compatible items in multiple uaps."""
    Cat1 = Cat_001_1_0
    rec_plot = Cat1.cv_uap.spec('plot').create({'041': 0xff})
    rec_track = Cat1.cv_uap.spec('track').create({'042': 0xffff})

    bs_plot = rec_plot.unparse()
    bs_track = rec_track.unparse()

    results_plot = Cat1.cv_uap.parse_any_uap(bs_plot)
    assert len(results_plot) == 1

    results_track = Cat1.cv_uap.parse_any_uap(bs_track)
    assert len(results_track) == 1

def test_parse_nonblocking() -> None:
    """We encode a single record with a new edition, where some items
    are added to the spec. We try to parse it as an old edition.
    """

    Ed0 = Cat_000_1_0
    Ed1 = Cat_000_1_1

    # old edition record
    record0 = Ed0.cv_record.create({
        '000': 0x00,
        '010': 0x0102,
    })

    # new edition record (added item '200')
    record1 = Ed1.cv_record.create({
        '000': 0x00,
        '010': 0x0102,
        '200': 0xff,
    })

    # encode new record
    s: Bits = record1.unparse()

    # try to parse as a list with both editions
    assert isinstance(Ed0.cv_uap.parse(s), ValueError)
    assert not isinstance(Ed1.cv_uap.parse(s), ValueError)

    # single record parsing combinations

    # parsing with new edition shall not fail, regardless of parsing mode
    for mode in ParsingMode:
        result1 = Ed1.cv_record.parse(mode, s)
        assert not isinstance(result1, ValueError)
        r1, remaining = result1
        assert remaining.null()
        assert hexlify(record1.unparse().to_bytes()) == \
                hexlify(r1.unparse().to_bytes())

    # old edition, partial parsing shall not fail
    result2 = Ed0.cv_record.parse(ParsingMode.PartialParsing, s)
    assert not isinstance(result2, ValueError)
    r2, remaining = result2
    assert hexlify(record0.unparse().to_bytes()) == \
            hexlify(r2.unparse().to_bytes())
    assert remaining == Bits.from_uinteger(0xff, 0, 8)
