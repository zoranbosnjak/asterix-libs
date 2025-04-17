# Asterix data processing library for python

Features:

- asterix data parsing/decoding from bytes
- asterix data encoding/unparsing to bytes
- precise conversion functions for physical quantities
- support for many asterix categories and editions
- support for Reserved Expansion Fields (REF)
- support for Random Field Sequencing (RFS)
- support for categories with multiple UAPs, eg. cat001
- support for context dependent items, eg. I062/380/IAS
- support for strict or partial record parsing, to be used
  with so called blocking or non-blocking asterix categories
- support to encode zero, one or more records in a datablock
- pure python implementation
- type annotations for static type checking,
  including subitem access by name

## Example

Encoding and decoding asterix example.
This example also includes type annotations for static
type checking with `mypy`. In a simple untyped environment,
the type annotations and assertions could be skipped.

```python
#| file: example0.py
from typing import *
from binascii import hexlify, unhexlify
from dataclasses import dataclass

from asterix.base import *
import asterix.generated as gen

# Select particular asterix categories and editions
Cat034 = gen.Cat_034_1_29
Cat048 = gen.Cat_048_1_32

# Example messages for this application
class Token:
    pass

@dataclass
class NorthMarker(Token):
    pass

@dataclass
class SectorCrossing(Token):
    azimuth: float

@dataclass
class Plot(Token):
    rho: float
    theta: float
    ssr: str

# example message to be encoded
tx_message = [
    NorthMarker(),
    SectorCrossing(0.0),
    Plot(rho=10.0, theta=45.0, ssr='7777'),
    SectorCrossing(45.0),
]
print('sending message:', tx_message)

# encode token to datablock
def encode(token: Token) -> bytes:
    if isinstance(token, NorthMarker):
        rec034 = Cat034.cv_record.create({
            '000': 1, # North marker message
            '010': (('SAC', 1), ('SIC', 2)),
        })
        datablock034 = Cat034.create([rec034])
        return datablock034.unparse().to_bytes()
    if isinstance(token, SectorCrossing):
        rec034 = Cat034.cv_record.create({
            '000': 2, # Sector crossing message
            '010': (('SAC', 1), ('SIC', 2)),
            '020': ((token.azimuth, "°")),
        })
        datablock034 = Cat034.create([rec034])
        return datablock034.unparse().to_bytes()
    if isinstance(token, Plot):
        rec048 = Cat048.cv_record.create({
            '010': (('SAC', 1), ('SIC', 2)),
            '040': (('RHO', (token.rho, "NM")), ('THETA', (token.theta, "°"))),
            '070': (0, 0, 0, 0, ('MODE3A', token.ssr)),
        })
        datablock048= Cat048.create([rec048])
        return datablock048.unparse().to_bytes()
    raise Exception('unexpected token', token)

datablocks = [encode(token) for token in tx_message]
tx = b''.join(datablocks)
print('bytes on the wire:', hexlify(tx))

assert hexlify(tx) == \
    b'220007c0010201220008d00102020030000c9801020a0020000fff220008d001020220'

# decode bytes to message list
def decode(rx_bytes: bytes) -> List[Token]:
    message: List[Token] = []

    raw_datablocks = RawDatablock.parse(Bits.from_bytes(tx))
    assert not isinstance(raw_datablocks, ValueError)
    for db in raw_datablocks:
        cat = db.get_category()
        if cat == 34:
            result034 = Cat034.cv_uap.parse(db.get_raw_records())
            assert not isinstance(result034, ValueError)
            for rec034 in result034:
                i000 = rec034.get_item('000')
                assert i000 is not None
                val = i000.as_uint()
                if val == 1:
                    message.append(NorthMarker())
                elif val == 2:
                    i020 = rec034.get_item('020')
                    assert i020 is not None
                    azimuth = i020.variation.content.as_quantity("°")
                    message.append(SectorCrossing(azimuth = azimuth))
                else:
                    pass
        elif cat == 48:
            result048 = Cat048.cv_uap.parse(db.get_raw_records())
            assert not isinstance(result048, ValueError)
            for rec048 in result048:
                i040 = rec048.get_item('040')
                i070 = rec048.get_item('070')
                assert i040 is not None
                assert i070 is not None
                rho = i040.variation.get_item('RHO').variation.content.as_quantity("NM")
                theta = i040.variation.get_item('THETA').variation.content.as_quantity("°")
                ssr = i070.variation.get_item('MODE3A').variation.content.as_string()
                message.append(Plot(rho = rho, theta = theta, ssr = ssr))
        else:
            pass
    return message

rx = tx
rx_message = decode(rx)

# expect the same message
print('received message:', rx_message)
assert rx_message == tx_message
```

## Installation

Use any of the following methods:

### Method 1

Install from python package index <https://pypi.org/project/libasterix/>:

``` bash
pip install libasterix
```

### Method 2

Install from github:

``` bash
# (default branch)
pip install -e "git+https://github.com/zoranbosnjak/asterix-libs.git#egg=libasterix&subdirectory=libs/python"

# ('devel' branch)
pip install -e "git+https://github.com/zoranbosnjak/asterix-libs.git@devel#egg=libasterix&subdirectory=libs/python"
```

### Method 3

Manually copy library files from [repository](https://github.com/zoranbosnjak/asterix-libs/tree/main/libs/python/src/asterix).

Download and copy files either alongside your project sources or
to some location where `python` can find it.

```bash
# check default python path
python3 -c "import sys; print('\n'.join(sys.path))"
```

## Tutorial

Check library installation.

```bash
python3 -c "import asterix.base as base; print(base.AstSpec)"
python3 -c "import asterix.generated as gen; print(gen.manifest['CATS'].keys())"
```

### Import

This tutorial assumes importing complete `asterix` module into the current
namespace. In practice however only the required objects could be imported
or the module might be imported to a dedicated namespace.

```python
from asterix.base import *
from asterix.generated import *
```

### Error handling

Some operation (eg. parsing) can fail on unexpected input. In such case,
to indicate an error, this library will not raise an exception, but will
return `ValueError('problem description')` instead.

With this approach, a user can handle errors in a type safe way, for example:

```python
def parse_datablocks(s: bytes) -> List[RawDatablock]:
    dbs = RawDatablock.parse(Bits.from_bytes(s))
    if isinstance(dbs, ValueError):
        return [] # or raise exception, or ...
    return dbs
```

For clarity, the error handling part is skipped in some parts of this tutorial.

### Immutable objects

All operation on asterix objects are *immutable*.

For example:

```python
#| file: example-immutable.py
from asterix.generated import *

Spec = Cat_002_1_1

# create empty record
rec0 = Spec.cv_record.create({})

# this operation does nothing (result is not stored)
rec0.set_item('000', 1)
assert rec0.get_item('000') is None

# store result to 'rec1'
rec1 = rec0.set_item('000', 1)
assert rec1.get_item('000') is not None

# del_item, store result to 'rec1a'
rec1a = rec1.del_item('000')
assert rec1a.get_item('000') is None

# use multiple updates in sequence
rec2a = rec0.set_item('000', 1).set_item('010', (('SAC', 1), ('SIC', 2)))
rec2b = Spec.cv_record.create({'000': 1, '010': (('SAC', 1), ('SIC', 2))})
assert rec2a.unparse() == rec2b.unparse()

# mutation can be simulated by replacing old object with the new one
# (using the same variable name)
rec0 = rec0.set_item('000', 1)
assert rec0.get_item('000') is not None
```

### Miscellaneous project and source code remarks

- `cv_{name}` stands for *class variable*, to avoid name clash with
  *instance variable* with the same name (which are without prefix).
- `RuleContent` and `RuleVariation` are necessary to cope with some
  small number of irregular cases with asterix definitions
  (that is: context dependent definitions).
- `NonSpare` is (as name suggests) an item with some defined content.
  It is a separate class from `Item` and `Spare`, to reuse definition
  in different contexts, for example `Compound` subitems are `NonSpare`.

### Datagram

Datagram is a raw binary data as received for example from UDP socket.
This is represented with `bytes` data type in python.

### Raw Datablock

Raw datablock is asterix datablock in the form `cat|length|data` with the
correct byte size. A datagram can contain multiple datablocks.

This is represented in python with `class RawDatablock`.

In some cases it might be sufficient to work with raw datablocks, for example
in the case of asterix category filtering. In this case, it is not required
to fully parse asterix records.

**Example**: Category filter, drop datablocks if category == 1

```python
#| file: example1.py
from binascii import hexlify, unhexlify
from asterix.base import *

def receive_from_udp() -> bytes: # UDP rx text function
    return unhexlify(''.join([
        '01000401', # cat1 datablock
        '02000402', # cat2 datablock
        ]))

def send_to_udp(s: bytes) -> None: # UDP tx test function
    print(hexlify(s))

input_data = Bits.from_bytes(receive_from_udp())
raw_datablocks = RawDatablock.parse(input_data) # can fail on wrong input
assert not isinstance(raw_datablocks, ValueError)
valid_datablocks = [db.unparse().to_bytes() \
                    for db in raw_datablocks if db.get_category() != 1]
output_data = b''.join(valid_datablocks)
send_to_udp(output_data)
```

### Datablock, Record

Datablock (represented as `class Datablock`) is a higher level, where we
have a guarantee that all containing records are semantically correct
(asterix is fully parsed or correctly constructed).

Datablock/Record is required to work with asterix items and subitems.

**Example**: Create 2 records and combine them to a single datablock

```python
#| file: example2.py
from binascii import hexlify
from asterix.generated import *

Spec = Cat_002_1_1 # use cat002, edition 1.1

rec1 = Spec.cv_record.create({
    '000': 1,
    '010': (('SAC', 1), ('SIC', 2)),
    })

rec2 = Spec.cv_record.create({
    '000': 2,
    '010': (('SAC', 1), ('SIC', 2)),
    })

db = Spec.create([rec1, rec2])
s = db.unparse().to_bytes() # ready to send over the network
print(hexlify(s))
```

**Example**: Parse datagram (from the example above) and extract message type
from each record

```python
#| file: example3.py
from binascii import unhexlify
from asterix.base import *
from asterix.generated import *

Spec = Cat_002_1_1 # use cat002, edition 1.1

s = unhexlify(b'02000bc0010201c0010202') # ... use data from the example above
raw_datablocks = RawDatablock.parse(Bits.from_bytes(s)) # can fail on wrong input
assert not isinstance(raw_datablocks, ValueError)
for db in raw_datablocks:
    records = Spec.cv_uap.parse(db.get_raw_records()) # can fail on wrong input
    assert not isinstance(records, ValueError)
    for record in records:
        i000 = record.get_item('000') # returns None if the item is not present
        assert i000 is not None
        raw_value = i000.as_uint()
        description = i000.variation.content.table_value()
        print('{}: {}'.format(raw_value, description))
```

**Example**: Asterix filter, rewrite SAC/SIC code with random values.

```python
#| file: example4.py
import time
import random
from asterix.base import *
from asterix.generated import *

# categories/editions of interest
Specs = {
    48: Cat_048_1_31,
    62: Cat_062_1_19,
    63: Cat_063_1_6,
    # ...
    }

def process_record(sac, sic, rec):
    """Process single record."""
    return rec.set_item('010', (('SAC', sac), ('SIC', sic)))

def process_datablock(sac, sic, db):
    """Process single raw datablock."""
    cat = db.get_category()
    Spec = Specs.get(cat)
    if Spec is None:
        return db
    # second level of parsing (records are valid)
    records = Spec.cv_uap.parse(db.get_raw_records())
    new_records = [process_record(sac, sic, rec) for rec in records]
    return Spec.create(new_records)

def rewrite_sac_sic(sac : int, sic : int, s : bytes) -> bytes:
    """Process datagram."""
    # first level of parsing (datablocks are valid)
    raw_datablocks = RawDatablock.parse(Bits.from_bytes(s))
    result = [process_datablock(sac, sic, db) for db in raw_datablocks]
    output = b''.join([db.unparse().to_bytes() for db in result])
    return output

def rx_bytes_from_the_network():
    """Dummy rx function (generate valid asterix datagram)."""
    time.sleep(1)
    Spec = Cat_048_1_31
    rec = Spec.cv_record.create({'010': 0, '040': 0})
    db1 = Spec.create([rec, rec]).unparse().to_bytes()
    db2 = Spec.create([rec, rec]).unparse().to_bytes()
    return b''.join([db1, db2])

def tx_bytes_to_the_network(s_output):
    """Dummy tx function."""
    print(hexlify(s_output))

# main processing loop
cnt = 0
while True:
    s_input = rx_bytes_from_the_network()
    new_sac = random.randint(0,127)
    new_sic = random.randint(128,255)
    try:
        s_output = rewrite_sac_sic(new_sac, new_sic, s_input)
        tx_bytes_to_the_network(s_output)
    except Exception as e:
        print('Asterix exception: ', e)
    # only run a few iterations for test
    cnt += 1
    if cnt > 3:
        break
```

#### Spare items

Some bits are defined as *Spare*, which are normally set to `0`.
With this library:

- A user is able set spare bits to any value, including abusing spare bits
  to contain non-zero value.
- When parsing data, tolerate spare bits to contain any value. It is up
  to the application to check the spare bits if desired.

Multiple spare bit groups can be defined on a single item.
`get_spares` method returns the actual values of all spare bit groups.

**Example**

```python
#| file: example-spare.py
from asterix.generated import *

# I062/120 contain single group of spare bits
Spec = Cat_062_1_20

# create regular record with spare bits set to '0'
rec1 = Spec.cv_record.create({
    '120': (0, ('MODE2', 0x1234)),
})
i120a = rec1.get_item('120')
assert i120a is not None
spares1 = i120a.variation.get_spares()
assert spares1 == [0]

# create record, abuse spare bits, set to '0xf'
rec2 = Spec.cv_record.create({
    '120': (0xf, ('MODE2', 0x1234)),
})
i120b = rec2.get_item('120')
assert i120b is not None
spares2 = i120b.variation.get_spares()
assert spares2 == [0xf]
```

#### Reserved expansion fields

This library supports working with expansion fields. From the `Record`
prespective, the `RE` item contains raw bytes, without any structure,
similar to how a datablock contains raw bytes without a structure. Parsing
raw datablocks and parsing records are 2 separate steps. In the same
vain, parsing `RE` out of the record would be a third step. Once parsed,
the `RE` item  gets it's structure, and it's possible to access it's subitems,
similar to a regular record/subitem situation.

When constructing a record with the `RE` item, a user must first
construct the `RE` item itself, unparse it to bytes and insert bytes
as a value of the `RE` item of a record.

A reason for this separate stage approach is that a category and expansion
specification can remain separate to one another. In addition, a user has
a possiblity to explicitly select both editions individually.

This example demonstrates required steps for constructing and parsing:

```python
#| file: example5.py
from asterix.generated import *

Spec = Cat_062_1_20
Ref  = Ref_062_1_3

# create 'RE' subitem
ref = Ref.cv_expansion.create({
    'CST': [0],
    'CSN': [1,2],
    'V3': {
        'PS3': 0,
        },
    })

# create record, insert 'RE' subitem as bytes
rec = Spec.cv_record.create({
    '010': (('SAC', 1), ('SIC', 2)),
    'RE': ref.unparse().to_bytes(),
    })

db = Spec.create([rec])
s = db.unparse()
assert s.to_bytes().hex() == \
    '3e001b8101010104010211c8010000000000020000010000028000'

# first stage, parse to the record
raw_datablocks = RawDatablock.parse(s)
assert not isinstance(raw_datablocks, ValueError)
assert len(raw_datablocks) == 1 # expecting 1 datablock
result1 = Spec.cv_uap.parse(raw_datablocks[0].get_raw_records())
assert not isinstance(result1, ValueError)
assert len(result1) == 1 # expecting one record

# get 'RE' subitem,
re_subitem = result1[0].get_item('RE')
assert re_subitem is not None
re_bytes = re_subitem.variation.get_bytes()

# second stage: parse 'RE' structure
result2 = Ref.cv_expansion.parse(Bits.from_bytes(re_bytes))
assert not isinstance(result2, ValueError)
ref_readback, remaining = result2
assert remaining.null()
# expecting the same 'ref' as the original
assert ref.unparse() == ref_readback.unparse()
# we have a structure back and we can extract the values
result3 = ref_readback.get_item('CSN')
assert result3 is not None
lst = result3.variation.get_list()
assert len(lst) == 2
assert lst[0].as_uint() == 1
assert lst[1].as_uint() == 2
```

#### Multiple UAP-s

Make sure to use appropriate UAP name, together with a correct UAP selector
value, for example for CAT001:

- `['020', 'TYP'] = 0` for `plot`
- `['020', 'TYP'] = 1` for `track`

```python
#| file: example6.py
from asterix.base import *
from asterix.generated import *

Cat1 = Cat_001_1_4

rec01_plot = Cat1.cv_uap.spec('plot').create({
    '010': 0x0102,
    '020': ((('TYP',0),0,0,0,0,0,None),),
    '040': 0x01020304
})

rec01_track = Cat1.cv_uap.spec('track').create({
    '010': 0x0102,
    '020': ((('TYP',1),0,0,0,0,0,None),),
    '040': 0x01020304,
    })

rec01_invalid = Cat1.cv_uap.spec('plot').create({
    '010': 0x0102,
    '020': ((('TYP',1),0,0,0,0,0,None),),
    '040': 0x01020304
})

print(Cat1.create([rec01_plot]).unparse().to_bytes().hex())
print(Cat1.create([rec01_track]).unparse().to_bytes().hex())
print(Cat1.create([rec01_invalid]).unparse().to_bytes().hex())
```

### RFS handling

This library supports RFS mechanism for categories that include RFS
indicator(s). For such cases, it is possible to sequence subitems in
any order. Once such record is created or parsed, a user can extract
subitems using `get_rfs_item` method. The result in this case is
a list, since the item can be present in the record multiple times.
An empty list indicates that no such item is present in the RFS.

**Example**

```python
#| file: example-rfs.py
from binascii import hexlify
from asterix.generated import *

# cat008 contains RFS indicator, so we are able to add RFS items
Spec = Cat_008_1_3

rec1 = Spec.cv_record.create({
    '000': 1,                                   # add item '000' (regular)
    '010': (('SAC', 1), ('SIC', 2)),            # add item '010' (regular)
    },
    [
        ('010', (('SAC', 3), ('SIC', 4))),      # add item '010' as RFS
        ('010', (('SAC', 4), ('SIC', 5))),      # add another item '010' as RFS
    ]
    )

# extract regular item 010
i010_regular = rec1.get_item('010')
print(i010_regular)

# extract RFS items 010, expecting 2 such items
i010_rfs = rec1.get_rfs_item('010')
assert len(i010_rfs) == 2
for i in i010_rfs:
    print(i)

# but item '000' is not present in RFS
assert len(rec1.get_rfs_item('000')) == 0
```

### Strict and partial record parsing modes

This library supports parsing records strictly or partially.

In a strict mode, we want to make sure that all data is parsed exactly
as specified in the particular category/edition schema. The record parsing
fails if the FSPEC parsing fails or if any subsequent item parsing fails.

I a partial mode, we don't require exact parsing match. If we know where
in a bytestring a record starts, we can try to parse *some* information out of
the data stream, even in the case if the editions of the transmitter and the
receiver do not match exactly. In particular: if the transmitter sends some
additional items, unknown to the receiver. In that case, the receiver can still
parse up to some point in a datablock.

Partial record parsing means to parse the FSPEC (which might fail) followed
by parsing subitems up to the point until items parsing is successful. The
record parsing only fails if the FSPEC parsing itself fails.

This is useful in situations where a datablock contains only one record
(known as *non-blocking* in Asterix Maintenance Group vocabulary) or if
we are interested only in the first record (even if there are more). The idea
is to regain some forward compatibility on the receiver side, such that the
receiver does not need to upgrade edition immediately as the transmitter
upgrades or even before that. Whether this is safe or not, depends on the
application and the exact differences between transmitter and receiver
asterix editions.

The following parsing methods exist:

- `UapClass.parse(s: Bits) -> ValueError or List[Record]`
- `RecordClass.parse(pm: ParsingMode, s: Bits) -> ValueError or Record + remaining`

`ParsingMode` is an `Enum` with the following options:

- `StrictParsing`
- `PartialParsing`

Calling `parse` on some `Uap` class returns **list of records** on success.
This method always uses *strict* parsing and it makes sure it consumes all
input data.

Calling `parse` on some `Record` class returns that record instance and
the remaining bytes. A method also requires parsing mode to be specified.

Both methods can fail on invalid input data (return `ValueError`).

This example demonstrates various parsing modes:

```python
Spec = Cat_NNN_E_E # some category/edition spec

s: Bits = db.get_raw_records() # some input bits to be parsed

# strictly parse records
# 'parse' is called on 'Uap' class
# successful result is a List of Records
result1 = Spec.cv_uap.parse(s)
if not isinstance(result1, ValueError):
    for r1 in result1:
        print(r1)

# strictly parse a single record
# 'parse' is called on 'Record' class
# successful result is (Record + remaining bytes)
result2 = Spec.cv_record.parse(ParsingMode.StrictParsing, s)
if not isinstance(result2, ValueError):
    r2, remaining = result2

# partially parse a single record
# successful result is (Record + remaining bytes),
result3 = Spec.cv_record.parse(ParsingMode.PartialParsing, s)
if not isinstance(result3, ValueError):
    r3, remaining = result3
```

### Library manifest

This library defines a `manifest` structure in the form:

```python
manifest = {
    'CATS': {
        1: [
            Cat_001_1_2,
            Cat_001_1_3,
            Cat_001_1_4,
        ],
        2: [
            Cat_002_1_0,
            Cat_002_1_1,
        ],
...
```

This structure can be used to extract *latest* editions for each defined
category, for example:

```python
#| file: example7.py
from asterix.generated import *

Specs = {cat: manifest['CATS'][cat][-1] for cat in manifest['CATS']}

for spec in Specs.values():
    print(spec.cv_category, spec.cv_edition)
```

Alternatively, a prefered way is to be explicit about each edition,
for example:

```python
#| file: example8.py
from asterix.generated import *

Specs = {
    48: Cat_048_1_31,
    62: Cat_062_1_19,
    63: Cat_063_1_6,
    # ...
    }
```

### Generic asterix processing

*Generic processing* in this context means working with asterix data where
the subitem names and types are determined at runtime. That is: the explicit
subitem names are never mentioned in the application source code.

This is in contrast to *application specific processing*, where we are
explicit about subitems, for example ["010", "SAC"].

**Example**: Show raw content of all toplevel items of each record

```python
#| file: example9.py
from binascii import unhexlify
from asterix.generated import *

Specs = {
    48: Cat_048_1_31,
    62: Cat_062_1_19,
    63: Cat_063_1_6,
    # ...
}

# some test input bytes
s = unhexlify(''.join([
    '3e00a5254327d835a95a0d0a2baf256af940e8a8d0caa1a594e1e525f2e32bc0448b',
    '0e34c0b6211b5847038319d1b88d714b990a6e061589a414209d2e1d00ba5602248e',
    '64092c2a0410138b2c030621c2043080fe06182ee40d2fa51078192cce70e9af5435',
    'aeb2e3c74efc7107052ce9a0a721290cb5b2b566137911b5315fa412250031b95579',
    '03ed2ef47142ed8a79165c82fb803c0e38c7f7d641c1a4a77740960737']))

def handle_nonspare(cat, name, nsp):
    print('cat{}, item {}, {}'.format(cat, name, nsp.unparse()))
    # depending on the application, we might want to display
    # deep subitems, which is possible by examining 'nsp' object

for db in RawDatablock.parse(Bits.from_bytes(s)):
    cat = db.get_category()
    Spec = Specs.get(cat)
    if Spec is None:
        print('unsupported category', cat)
        continue
    for record in Spec.cv_uap.parse(db.get_raw_records()):
        for (name, nsp) in record.items_regular.items():
            handle_nonspare(cat, name, nsp)
```

**Example**: Generate dummy single record datablock with all fixed items set to zero

```python
#| file: example10.py
from binascii import hexlify
from asterix.generated import *

# we could even randomly select a category/edition from the 'manifest',
# but for simplicity just use a particular spec
Spec = Cat_062_1_20

rec = Spec.cv_record.create({})
all_items = Spec.cv_record.cv_items_dict
for name in all_items:
    if name is None:
        continue
    nsp = all_items[name]
    var = nsp.cv_rule.cv_variation
    if issubclass(var, Element):
        rec = rec.set_item(name, 0)
    elif issubclass(var, Group):
        rec = rec.set_item(name, 0)
    elif issubclass(var, Extended):
        pass # skip for this test
    elif issubclass(var, Repetitive):
        pass # skip for this test
    elif issubclass(var, Explicit):
        pass # skip for this test
    elif issubclass(var, Compound):
        pass # skip for this test
    else:
        raise Exception('unexpected subclass')

s = Spec.create([rec]).unparse().to_bytes()
print(hexlify(s))
```

## Using `mypy` static code checker

**Note**: Tested with `mypy` version `1.9.0`.

[mypy](https://www.mypy-lang.org/) is a static type checker for Python.
It is recommended to use the tool on asterix application code, to identify
some problems which would otherwise result in runtime errors.

Consider the following test program (`test.py`):

```python
from asterix.generated import *

Spec = Cat_008_1_3
rec = Spec.cv_record.create({'010': (('SA',1), ('SIC',2))})
i010 = rec.get_item('010')
print(i010.variation.get_item('SA').as_uint())
```

The program contains the following bugs:
- Misspelled item name, `SA` instead of `SAC`, on lines 4 and 5
- `get_item('010') result is not checked if the item
  is actually present, which might result in runtime error

```
$ python test.py
... results in runtime error (wrong item name)
$ pip install mypy
$ mypy test.py
... detects all problems, without actually running the program
Found 3 errors in 1 file (checked 1 source file)
```

Correct version of this program is:

```python
#| file: example11.py
from asterix.generated import *

Spec = Cat_008_1_3
rec = Spec.cv_record.create({'010': (('SAC',1), ('SIC',2))})
i010 = rec.get_item('010')
if i010 is not None:
    print(i010.variation.get_item('SAC').as_uint())
```

```
$ mypy test.py
Success: no issues found in 1 source file
$ python test.py
1
```
