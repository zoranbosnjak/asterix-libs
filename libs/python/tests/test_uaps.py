# asterix UAP tests

import pytest

'''
from asterix.generated_test import *

def test_single() -> None:
    """ Single UAP
        - building is always safe
        - 'is_valid' is not necessary
        - no problems with parsing
    """
    Spec = CAT_000_1_0

    rec = Spec.make_record({
        '000': 0,
        '001': 0,
        })

    db = Spec.make_datablock(rec)
    assert db == Spec.make_datablock([rec])
    assert db.records == [rec]
    rawdb = RawDatablock(db.unparse())
    assert Spec.parse(rawdb) == db

def test_multiple_selector() -> None:
    """ Multiple UAPS, with UAP selector
        - building is unsafe, UAP has to be selected
        - 'is_valid' property is available
        - generic parsing or with selected UAP
    """

    Spec = CAT_001_1_0

    # uap 0
    rec1a = Spec.make_record_unsafe('uap0', {
        '000': 0,
        '020': {'TYP': 0},
        '031': 0,
        })
    db = Spec.make_datablock(rec1a)
    assert db == Spec.make_datablock([rec1a])
    assert db.records == [rec1a]
    rawdb = RawDatablock(db.unparse())
    assert Spec.parse(rawdb) == db
    assert Spec.parse(rawdb, uap='uap0') == db
    with pytest.raises(ValueError):
        Spec.parse(rawdb, uap='uap1') # wrong UAP
    assert Spec.is_valid(rec1a)

    # uap 1
    rec1b = Spec.make_record_unsafe('uap1', {
        '000': 0,
        '020': {'TYP': 1},
        '032': 0,
        })
    db = Spec.make_datablock(rec1b)
    assert db == Spec.make_datablock([rec1b])
    assert db.records == [rec1b]
    rawdb = RawDatablock(db.unparse())
    assert Spec.parse(rawdb) == db
    assert Spec.parse(rawdb, uap='uap1') == db
    with pytest.raises(ValueError):
        Spec.parse(rawdb, uap='uap0') # wrong UAP
    assert Spec.is_valid(rec1b)

    # wrong UAP selection
    rec2a = Spec.make_record_unsafe('uap0', {'020': {'TYP': 1}})
    assert Spec.is_valid(rec2a) == False

    rec2b = Spec.make_record_unsafe('uap1', {'020': {'TYP': 0}})
    assert Spec.is_valid(rec2b) == False

def test_multiple_without_selector() -> None:
    """ Multiple UAPS, without UAP selector
        - building is unsafe, UAP has to be selected
        - 'is_valid' property is not available
        - parsing only with selected UAP
    """

    Spec = CAT_002_1_0

    rec1a = Spec.make_record_unsafe('uap0', {
        '000': 0,
        '020': {'TYP': 0},
        '031': 0,
        })

    db = Spec.make_datablock(rec1a)
    assert db == Spec.make_datablock([rec1a])
    assert db.records == [rec1a]
    rawdb = RawDatablock(db.unparse())
    assert Spec.parse(rawdb, uap='uap0') == db

    rec1b = Spec.make_record_unsafe('uap1', {
        '000': 0,
        '020': {'TYP': 1},
        '032': 0,
        })

    db = Spec.make_datablock(rec1b)
    assert db == Spec.make_datablock([rec1b])
    assert db.records == [rec1b]
    rawdb = RawDatablock(db.unparse())
    assert Spec.parse(rawdb, uap='uap1') == db
'''

