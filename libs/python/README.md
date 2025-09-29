# Asterix data processing library for python

Features:

- pure python implementation
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
- type annotations for static type checking,
  including subitem access by name

## Asterix encoding and decoding example

This example also includes optional type annotations for static
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

## Installation and library import

Use any of the following installation methods:

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

### Check library installation

```bash
python3 -c "import asterix.base as base; print(base.AstSpec)"
python3 -c "import asterix.generated as gen; print(gen.manifest['CATS'].keys())"
```

This tutorial assumes importing complete `asterix` module into the current
namespace. In practice however only the required objects could be imported
or the module might be imported to a dedicated namespace.

```python
# python application
from asterix.base import *
from asterix.generated import *
```

## Asterix object hierarchy and terminology

This library is built aroud the following concepts:

- **Datagram** is a raw binary data as received for example from UDP socket.
  This is represented with `bytes` data type in python.
- **RawDatablock** is asterix datablock in the form `cat|length|data` with
  correct byte size. A datagram can contain multiple datablocks.
  This is represented in python with `class RawDatablock`.
  In some cases it might be sufficient to work with raw datablocks, for
  example "asterix category filtering". In this case, it is not necessary
  to fully parse all asterix records, but is sufficient and faster to
  parse only up to the `RawDatablock` level.
- **Datablock/Record** (represented as `class Datablock` and `class Record`)
  is a higher level construct, where we have a guarantee that all containing
  elements (records, subitems) are semantically correct (asterix is fully
  parsed or correctly constructed).
- **Item** is a union of **regular item** or **spare item**, where the
  spare item is a wrapper around raw bits (normally zero).
- **Variation** is a union of
  `[element, group, extended, repetitive compound]` constructors.

## Constructing, parsing/unparsing, encoding/decoding

This library uses term to **construct asterix**, when a
record/datablock/datagram is "constructed" inside the application source
code, that is: not parsed.

Once the datablock is constructed, it is **unparsed** to bytes, ready to
be sent over the network. Similarly, the term **parsing** is used when we
perform oposite transformation from unstructured bytes to structured
Datablock/Record. This operation can obviously fail at runtime, so some
form of error handling is required inside application.

The terms **encoding/decoding** are used to denote conversion between
python objects from this library to application specific python objects,
for example *target reports* or *sector messages*

```
    application objects (e.g. Sector crossing message)
        ^  |
        |  |  decoding / encoding
        |  v
    asterix objects (e.g. Record)
        ^  |
        |  |  parsing / unparsing
        |  v
      (bytes)
```

## Subitem and content access

A `Record` contains `Items`, which in turn contains subitems at various
nesting levels. To access a subitem, use `get_item("item_name")` method.

Depending on the variation, the result might be `None` in case if a
particular item is not present. However, if the result is not `None`,
The `.variation` property returns "variation" which can be in turn
querried for nested subitems or converted to required value.

This is a typical usage:

```python
rec048 = # some valid asterix record
i040 = rec048.get_item('040')
i070 = rec048.get_item('070')
if i040 is None: raise Exception('can not continue')
if i070 is None: raise Exception('can not continue')
rho = i040.variation.get_item('RHO').variation.content.as_quantity("NM")
theta = i040.variation.get_item('THETA').variation.content.as_quantity("°")
ssr = i070.variation.get_item('MODE3A').variation.content.as_string()
```

## Application examples

### Category filter

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

### Rewrite SAC/SIC in item 010

**Example**: Asterix filter, rewrite SAC/SIC code.

```python
#| file: example4.py
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
    Spec = Cat_048_1_31
    rec = Spec.cv_record.create({'010': 0, '040': 0})
    db1 = Spec.create([rec, rec]).unparse().to_bytes()
    db2 = Spec.create([rec]).unparse().to_bytes()
    return b''.join([db1, db2])

def tx_bytes_to_the_network(s_output):
    """Dummy tx function."""
    print(hexlify(s_output))

# main processing loop
s_input = rx_bytes_from_the_network()
new_sac = 1
new_sic = 2
expected = unhexlify("300011900102000000009001020000000030000a90010200000000")
s_output = rewrite_sac_sic(new_sac, new_sic, s_input)
assert s_output == expected
tx_bytes_to_the_network(s_output)
```

### Spare bits

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

## Reserved expansion (RE) fields

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

## Generic asterix processing

*Generic processing* in this context means working with asterix data where
the subitem names and types are determined at runtime. That is: the explicit
subitem names are never mentioned in the application source code.

This is in contrast to *application specific processing*, where we are
explicit about subitems, for example `["010", "SAC"]`.

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

expected = unhexlify("3e0038bfe9bd5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
result = Spec.create([rec]).unparse().to_bytes()
print(hexlify(result))
assert result == expected
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

## Error handling

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

## Miscellaneous project and source code remarks

- `cv_{name}` stands for *class variable*, to avoid name clash with
  *instance variable* with the same name (which are without prefix).
- `RuleContent` and `RuleVariation` are necessary to cope with some
  small number of irregular cases with asterix definitions
  (that is: context dependent definitions).
- `NonSpare` is named item with some defined content.
  It is a separate class from `Item` and `Spare`, to reuse definition
  in different contexts, for example `Compound` subitems are `NonSpare`
  too.

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

### Asterix specifications as python classes

Asterix specifications hierarchy is reflected in python classes.
For example: Category `062` contains item `010`, which in turn contains
subitems `SAC` and `SIC`.

There is a `spec` class method which follows this structure deeper to the
hierarchy. For example:

```python
#| file: example-spec.py
from binascii import unhexlify
from asterix.generated import *

# Use cat062, edition 1.20
Spec = Cat_062_1_20

print(Spec)
print('category number', Spec.cv_category)
print('edition', Spec.cv_edition)

# Extract deeper specs
Uap = Spec.cv_uap
Record = Uap.cv_record
I010 = Record.spec('010')
SAC = I010.cv_rule.cv_variation.spec('SAC')
print(SAC)

# Use more direct way to extract subspec
SIC = Spec.cv_uap.cv_record.spec('010').cv_rule.cv_variation.spec('SIC')
print(SIC)

# SAC and SIC subitems are both 8-bits long raw values (same structure),
# so thay both map to the same class.
assert (SAC==SIC)

# With this specification it is possible to perform low level
# asterix operations, for example to parse a single subitem
sample_bits = Bits.from_bytes(unhexlify(b'ff0102fe'))
print(sample_bits)
result = SIC.parse(sample_bits)
assert not isinstance(result, ValueError)
sic_object, remaining_bits = result
print(remaining_bits)
print(sic_object)
print(sic_object.unparse())
print(sic_object.as_uint())

# Similarly, it is possible to extract other parts, for example
# extended subitems
print(Record.spec('080').cv_rule.cv_variation.spec('MON').cv_rule)
# compound subitems
print(Record.spec('110').cv_rule.cv_variation.spec('SUM').cv_rule)
```

See [base.py](https://github.com/zoranbosnjak/asterix-libs/tree/main/libs/python/src/asterix/base.py)
and [generated.py](https://github.com/zoranbosnjak/asterix-libs/tree/main/libs/python/src/asterix/generated.py)
for details.

### Using `mypy` static code checker

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
- `get_item('010')` result is not checked if the item
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

## Rare asterix cases

### Dependent specifications

In some rare cases, asterix definitions depend on a value of some other
item(s). In such cases, the asterix processing is more involved. This
dependency manifests itself in two ways:

- **content dependency**, where a content (interpretation of bits) of some
  item depends on the value of some other item(s). For example:
  `I062/380/IAS/IAS`, the structure is always 15 bits long,
  but the interpretation of bits could be either speed in `NM/s` or `Mach`,
  with different scaling factors, depending on the values of a sibling item.
- **variation dependency**, where not only the content, but also a complete
  item stucture depends on some other item(s). For example, the structure
  of item `I004/120/CC/CPC` depends on 2 other item values.

This library can handle all structure cases, however it does not automatically
correlate to the values of items that a structure depends on. When creating
records, it is a user responsibility to properly set "other item values" for
a record to be valid.
Similarly, after a record is parsed, a user shall cast a default structure to a
target structure, depending on the other items values. Whenever there is a
dependency, there is also a statically known *default* structure, which is
used during automatic record parsing.

#### Handling **content dependency**

This example demonstrates how to work with **content dependency**,
such as `I062/380/IAS`.

```python
#| file: example-dep-content.py
from asterix.base import *
from asterix.generated import *

Spec = Cat_062_1_20 # Cat 062, edition 1.20

# create records by different methods

# set raw value
rec0 = Spec.cv_record.create({
    '380': {
        'IAS': (
            ('IM', 0),  # set IM to 0
            ('IAS', 1)  # set IAS to raw value 1 (no unit conversion)
        ) },
    })

# set raw value using default case
rec1 = Spec.cv_record.create({
    '380': {
        'IAS': (
            ('IM', 0),  # set IM to 0
            ('IAS',
                (None,  # use default raw case
                 1))    # set IAS to raw value 1 (same as above)
        ) },
    })

# set IAS speed (NM/s)
rec2 = Spec.cv_record.create({
    '380': {
        'IAS': (
            ('IM', 0), # airspeed = IAS
            ('IAS',
                ((0,), # use case with index 0 (IAS)
                 (1.2, 'NM/s'))), # set IAS to 1.2 NM/s
        ) },
    })

# set Mach speed
rec3 = Spec.cv_record.create({
    '380': {
        'IAS': (
            ('IM', 1), # airspeed = Mach
            ('IAS',
                ((1,), # use case with index 1 (Mach)
                 (0.8, 'Mach'))), # set speed to 0.8 Mach
        ) },
    })

db = Spec.create([rec0, rec1, rec2, rec3])
expected_output = b'3e0017011010000101101000010110104ccd0110108320'
assert hexlify(db.unparse().to_bytes()) == expected_output

# parse and interpret data from the example above
input_bytes = unhexlify(expected_output)
raw_datablocks = RawDatablock.parse(Bits.from_bytes(input_bytes))
assert not isinstance(raw_datablocks, ValueError)
for db in raw_datablocks:
    assert db.get_category() == 62
    result = Spec.cv_uap.parse(db.get_raw_records())
    assert not isinstance(result, ValueError)
    for (cnt, rec) in enumerate(result):
        i380 = rec.get_item('380')
        assert i380 is not None
        item_IAS1 = i380.variation.get_item('IAS')
        assert item_IAS1 is not None
        item_IM = item_IAS1.variation.get_item('IM')
        item_IAS2 = item_IAS1.variation.get_item('IAS')
        assert item_IM is not None
        assert item_IAS2 is not None
        match item_IM.as_uint(): # check value of I062/380/IAS/IM and convert
            case 0: # this is IAS, convert to 'NM/s', use case with index (0,)
                value = ('NM/s', item_IAS2.variation.rule.content((0,)).as_quantity('NM/s'))
            case 1: # this is Mach, convert to 'Mach', use case with index (1,)
                value = ('Mach', item_IAS2.variation.rule.content((1,)).as_quantity('Mach'))
            case _:
                raise Exception('unexpected value')
        print('--- record', cnt, '---')
        print('I062/380/IAS/IM raw value:', item_IM.as_uint())
        print('I062/380/IAS/IAS raw value:', item_IAS2.as_uint())
        print('converted value', value)
```

#### Handling **variation dependency**

This example demonstrates how to work with **variation dependency**,
such as `I004/120/CC/CPC`.

```python
#| file: example-dep-variation.py
from asterix.base import *
from asterix.generated import *

Spec = Cat_004_1_13 # Cat 004, edition 1.13

# Item 'I004/120/CC/CPC' depends on I004/000 and I004/120/CC/TID values
# Default case is: element3, raw, but there are many other cases.
# See asterix specification for details.
# This example handles the following cases:
# case (5, 1): element 3, table
# case (9, 2): group (('RAS', element1, table), spare 2)

# case (0, 0) - invalid combination
rec0 = Spec.cv_record.create({
    '000': 0, # invalid value
    '120': {
        'CC': (
            ('TID', 0),
            ('CPC', 0), # set to raw value 0
            ('CS', 0)
            )
        }
    })

# case (5, 1)
rec1 = Spec.cv_record.create({
    '000': 5, # Area Proximity Warning (APW)
    '120': {
        'CC': (
            ('TID', 1),
            ('CPC', 0), # structure is 'raw', set to 0
            ('CS', 0)
            )
        }
    })

# case (9, 2)
# get variation structure of case (9, 2)
Var = Spec.cv_record.spec('120').cv_rule.cv_variation.spec('CC').\
    cv_rule.cv_variation.spec('CPC').spec((9,2))
# and create object of that structure ('RAS' + spare item)
obj = Var.create(( ('RAS', 1), 0))  # RAS = Stage Two Alert

# insert object into the record (as uint)
rec2 = Spec.cv_record.create({
    '000': 9, # RIMCAS Arrival / Landing Monitor (ALM)
    '120': {
        'CC': (
            ('TID', 2),
            ('CPC', obj.as_uint()),
            ('CS', 0)
            )
        }
    })

db = Spec.create([rec0, rec1, rec2])
expected_output = b'040012412000400041200540104120094028'
assert hexlify(db.unparse().to_bytes()) == expected_output

# parse and interpret data from the example above
input_bytes = unhexlify(expected_output)
raw_datablocks = RawDatablock.parse(Bits.from_bytes(input_bytes))
assert not isinstance(raw_datablocks, ValueError)
for db in raw_datablocks:
    assert db.get_category() == 4
    result = Spec.cv_uap.parse(db.get_raw_records())
    assert not isinstance(result, ValueError)
    for (cnt, rec) in enumerate(result):
        print('--- record', cnt, '---')
        i000 = rec.get_item('000')
        i120 = rec.get_item('120')
        assert i000 is not None and i120 is not None
        item_CC = i120.variation.get_item('CC')
        assert item_CC is not None
        item_TID = item_CC.variation.get_item('TID').as_uint()
        item_CPC = item_CC.variation.get_item('CPC')
        item_CS  = item_CC.variation.get_item('CS').as_uint()
        index = (i000.as_uint(), item_TID)

        try:
            var_CPC = item_CPC.variation(index)
        except Exception:
            var_CPC = None
        assert not isinstance(var_CPC, ValueError)

        match index:
            case (5, 1):
                x = var_CPC.as_uint()
                value = ('case 5,1', 'raw', x)
            case (9, 2):
                item_ras = var_CPC.get_item('RAS')
                spares = var_CPC.get_spares()
                value = ('case 9,2', 'RAS', item_ras.as_uint(), spares)
            case _:
                value = None
        print(value)
```

### Multiple UAP-s

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
indicators. For such cases, it is possible to sequence subitems in
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

In a partial mode, we don't require exact parsing match. If we know where
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

This example demonstrates both parsing modes:

```python
#| file: example-partial-parsing.py
from asterix.base import *
from asterix.generated import *

SpecOld = Cat_063_1_6
SpecNew = Cat_063_1_7

# In the new spec, item 060 is extended to contain 3 groups,
# while in the old spec it only contain2 groups.
# Create record according to the new spec
rec0 = SpecNew.cv_record.create({
    '010': (('SAC', 1), ('SIC', 2)),
    '015': 3,
    '060': ((1, 2, 3)),
    })

# This bytestring represents the record of SpecNew
bs = rec0.unparse()

expected = unhexlify("c8010203030506")
assert (bs.to_bytes() == expected)

# We should be able to parse the record, using the new spec
# and get the same record back.
result1 = SpecNew.cv_record.parse(ParsingMode.StrictParsing, bs)
assert not isinstance(result1, ValueError)
rec1, _bs1  = result1
assert (rec1.unparse() == rec0.unparse())

# Strict parsing with the old spec fails.
result2 = SpecOld.cv_record.parse(ParsingMode.StrictParsing, bs)
assert isinstance(result2, ValueError)

# However, we can still try to parse using the PartialParsing mode.
result3 = SpecOld.cv_record.parse(ParsingMode.PartialParsing, bs)
assert not isinstance(result3, ValueError)
rec3, _ignore = result3

# We accept the fact that resulting record might not be complete, but
# items "010" and "015" are valid, even if parsing using the old edition.
i010 = rec3.get_item('010')
i015 = rec3.get_item('015')
assert i010 is not None
assert i015 is not None
assert (i010.as_uint() == 0x0102)
assert (i015.as_uint() == 3)

# Note, that the result in this case in not equal to the original record.
assert (rec3.unparse() != rec0.unparse())
```

## Unit tests

For more examples using test specifications, see also project repository
[unit tests](https://github.com/zoranbosnjak/asterix-libs/tree/main/libs/python/tests).

