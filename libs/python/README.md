# Asterix data processing library for python

Features:

- asterix data parsing/decoding from bytes
- asterix data encoding/unparsing to bytes
- precise conversion functions for physical quantities
- support for many asterix categories and editions
- support for Reserved Expansion Fields (REF)
- support for categories with multiple UAPs, eg. cat001
- support for context dependent items, eg. I062/380/IAS
- pure python implementation
- type annotations for static type checking,
  including subitem access by name

## Example

Encoding and decoding asterix example.
This example also includes type annotations for static
type checking with `mypy`. In a simple untyped environment,
the type annotations and assertions could be skipped.

```python
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

Install/update from python package index <https://pypi.org/project/libasterix/>:

``` bash
pip install libasterix
```

### Method 2

Install/update from github:

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

# use multiple updates in sequence
rec2a = rec0.set_item('000', 1).set_item('010', (('SAC', 1), ('SIC', 2)))
rec2b = Spec.cv_record.create({'000': 1, '010': (('SAC', 1), ('SIC', 2))})
assert rec2a.unparse() == rec2b.unparse()

# mutation can be simulated by replacing old object with the new one
# (using the same variable name)
rec0 = rec0.set_item('000', 1)
assert rec0.get_item('000') is not None
```

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
from binascii import hexlify, unhexlify
from asterix.base import *

def receive_from_udp():         # UDP rx text function
    return unhexlify(''.join([
        '01000401', # cat1 datablock
        '02000402', # cat2 datablock
        ]))

def send_to_udp(s):             # UDP tx test function
    print(hexlify(s))

input_data = Bits.from_bytes(receive_from_udp())
raw_datablocks = RawDatablock.parse(input_data) # can fail on wrong input
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
from asterix.base import *
from asterix.generated import *

Spec = Cat_002_1_1 # use cat002, edition 1.1

s = unhexlify(b'02000bc0010201c0010202') # ... use data from the example above
raw_datablocks = RawDatablock.parse(Bits.from_bytes(s)) # can fail on wrong input
for db in raw_datablocks:
    records = Spec.cv_uap.parse(db.get_raw_records()) # can fail on wrong input
    for record in records:
        i000 = record.get_item('000') # returns None if the item is not present
        raw_value = i000.as_uint()
        description = i000.variation.content.table_value()
        print('{}: {}'.format(raw_value, description))
```

**Example**: Asterix filter, rewrite SAC/SIC code with random values.

```python
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
while True:
    s_input = rx_bytes_from_the_network()
    new_sac = random.randint(0,127)
    new_sic = random.randint(128,255)
    try:
        s_output = rewrite_sac_sic(new_sac, new_sic, s_input)
        tx_bytes_to_the_network(s_output)
    except Exception as e:
        print('Asterix exception: ', e)
```

#### Reserved expansion fields

TODO: Add parsing/constructing expansion field example

#### Multiple UAP-s

Make sure to use appropriate UAP name, together with a correct UAP selector
value, for example for CAT001:

- `['020', 'TYP'] = 0` for `plot`
- `['020', 'TYP'] = 1` for `track`

```python
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

### Library manifest

This library defines a `manifest` structure in the form:

```python
manifest = {
    'CATS': {
        1: {
            '1.2': CAT_001_1_2,
            '1.3': CAT_001_1_3,
            '1.4': CAT_001_1_4,
        },
        2: {
            '1.0': CAT_002_1_0,
            '1.1': CAT_002_1_1,
        #...
```

This structure can be used to extract *latest* editions for each defined
category, for example:

```python
from asterix.generated import *

def to_edition(ed):
    """Convert edition string to a tuple, for example "1.2" -> (1,2)"""
    a,b = ed.split('.')
    return (int(a), int(b))

def get_latest_edition(lst):
    return sorted(lst, key=lambda pair: to_edition(pair[0]), reverse=True)[0]

Specs = {}  # will be populated with latest editions

for cat in range(1,256):
    editions = manifest['CATS'].get(cat)
    if editions is None:
        continue
    latest = get_latest_edition(editions.items())
    ed, cls = latest
    Specs[cat] = cls

print(Specs)
```

Alternatively, a prefered way is to be explicit about each edition,
for example:

```python
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

**Note**: `mypy` version `0.991` or above is required for this library.

[mypy](https://www.mypy-lang.org/) is a static type checker for Python.
It is recommended to use the tool on asterix application code, to identify
some problems which would otherwise result in runtime errors.

Consider the following test program (`test.py`):

```python
from asterix.generated import *

Spec = Cat_008_1_3
rec = Spec.cv_record.create({'010': (('SA',1), ('SIC',2))})
print(rec.get_item('010').variation.get_item('SA').as_uint())
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
