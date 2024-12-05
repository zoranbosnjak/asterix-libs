# Asterix data processing libraries

This project builds on top of
[asterix-specs](https://zoranbosnjak.github.io/asterix-specs/) - asterix
definition files.

The main part of this project is a [source code generator](code-generator)
for various target programmig languages. It uses asterix specification files
and generates data structures and support functions for a selected target
language. Other languages can be added, while sharing the same generic
asterix structure.

In addition to the generated code, each [target programming language](libs)
also contains additional support files, resulting in a complete library for
asterix data (parsing, creating and manipulating asterix records and
datablocks).

[Test asterix specifications](test-specs) are included for test purposes.

For more information, questions or comments, feel free to
contact project maintainer [Zoran Bošnjak](mailto:zoran.bosnjak@via.si).

## About ASTERIX data format

The name ASTERIX stands for
"All-purpose structured EUROCONTROL surveillance information exchange".

It's a binary data format, used for exchanging surveillance-related
information and other ATM (air traffic management) applications.
It was designed for communication media with limited bandwidth.
This is why it follows rules that enable it to transmit all the
information needed, with the smallest data load possible.

Instead of a single *data format*, it's better described as
*a family of data formats*, since a new format descriptions
(in the form of *category* and *edition*) are implemented from time
to time.

ASTERIX is developed and maintained by
[eurocontrol](https://www.eurocontrol.int/asterix).

### Asterix Datablock (level 1)

At the first level, asterix format defines a so called `Datablock`.
This structure is common for all asterix standards.

Each datablock is in the form  `(cat | length | records)`, where:

- `cat` is one byte long *category number*;
- `length` is two bytes long. It represents total length in bytes
  of the complete datablock, including `cat` and `length` fields;
- `records` represent the remaining bytes;

There might be one or more concatinated records encoded in the
same datablock, where all of them are encoded according to the same
asterix specification (same *category* and *edition*).

Only a *category number* is encoded in a datablock.
The *edition* (in general) needs to be agreead between sender and receiver.
Newer editions are suppose to be backward compatible with the old ones,
but this is unfortunately not always the case.

Finally, one or more `Datablocks` may be encoded (concatinated) in the
same `Datagram`. A Datagram is typically what gets sent over the
wire (for example over UDP/IP).

### Asterix Record and Items (level 2)

The `Record` structure is defined separately for each category with
some common building blocks:

- Record contains Items
- Item is either Spare (some number of reserved bits)
  or a regular non-spare item.
- There are several kind of items, called Variations. Some
  variations (recursively) include other Items.

This project defines the following Variations:

- Element: is the structure containing the actual data.
  It is always of a known bit size (which is not necessary
  byte aligned).
- Group: is concatinated list of Items (either spare or regular).
- Extended: is similar to 'Group', except that each group
  can extend with additional group. There is so called
  'FX' (extension) bit between each group. If the FX bit
  is set (1), the extension follows.
- Repetitive: This structure contains one or more substructures
  of the same kind. There are 2 kinds of repetitive variations:
    - The encoding starts with REP factor, typically
      one byte long (but may be more). This number determines
      the number of following substructures.
    - The encoding starts directly with the first substructure,
      the FX bit following the substructure determines if additional
       substructure (of the same shape) is following.
- Explicit: There is one byte determining total byte size of
  the structure, following with opaque data bytes. The structure
  of the bytes are defined separately. This is the way to extend
  the standard with another document or skip the data of known
  size.
- Compound: in this structure, the first part is FSPEC bit mapping,
  followed by the actual subitems.
  The FSPEC structure also include FX bit to extend the FSPEC mapping
  as necessary. Each bit in the FSPEC determines presence or absence
  of defined subitems. So, the Compound kind is also like a 'Group',
  with the table of contents (FSPEC) in the front.

Items can be nested. In fact, the toplevel 'Record' structure is just
one big 'Compound' item (FSPEC + items).

All variations (except Element) are always byte aligned,
so is the Record.

There are some exceptions to the rules above, unfortunately.
In particular, some structures are content dependent, but the bit
size is always defined.

### Multiple UAPs

Note: This is an exceptional case.

Some specifications do not define exactly the structure of the Record,
but provide multiple possibilities of the Record structure. This is
unfortunate and is present in the standard for historical reasons.

In general, in this case the sender and receiver must agree to the
actual structure in use.

In some cases, it might be possible (although awkward) to:

- decode the structure to some extend (just enough to determine the
  overall structure), then decode the rest of the record
- or try to decode the record with all possible structures
  (potentially resulting in multiple positive decoding outcomes)

### Expansion

Expansion is similar to normal asterix record structure,
except: The toplevel Compound item has predefined FSPEC size,
without FX bits.

Once the expansion structure is encoded as sequence of bytes,
it can be embedded into the Explicit item kind of the standard
asterix Record.

### Asterix content interpretation

In addition to the structure, the asterix specification also defines
the meaning of Element bits. Binary value can be interpreted as:

- raw binary data
- table of possible values
- string
- integer
- physical quantity
- BDS register structure (defined in another standard).
