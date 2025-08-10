# Asterix data processing library for haskell

## Asterix schema

A core part of this project is the `Asterix.Generated` module, where
all important aspect of asterix data format is captured as haskell types.
Having asterix specifications defined as types is important, to be able to
catch errors at compile time (e.g. compiler can detect access attempt
into unspecified item).

But the specifications need to be available at runtime too. For example,
we want to be able to generate random record of some category/edition or
generically convert to `json`. So, each type level specification is converted
into a value level counterpart. The `Asterix.Schema` provides the necessary
conversion functions. In short `valueLevel = schema @typeLevel Proxy`.

### Schema naming conventions

For types describing asterix schema.

```
GType u - generic data structure, parametrized over 'usecase',
          to be used on a type and value level
TType   - type TType = GType 'TypeLevel (all generated types)
VType   - type VType = GType 'ValueLevel (TType converted to value level)
```

See `Asterix.Schema` module for details.

### Data naming conventions

For types containing actual asterix data.

```
UType   - Untyped value, for example UItem, UVariation, ...
Type t  - Typed wrapper around Untyped value: (Item t), (Variation t),  ...
```

## Constructing

A special `DSL` is provided to construct asterix objects within the source
code. It's based on processing `HList` of subitems, since the subitems are not
necessary of the same type and a regular list can not be used. 'HList'
constuction is performed with `*:` as `HCons` operator and `nil` as list
termination `HNil`. For example:

```haskell
db062 :: Datablock (DatablockOf Cat062)
db062 = datablock (rec1 *: nil)
  where
    rec1 = record
        ( item @"010" 0x0102
       *: item @"120" ( group
            ( spare
           *: item @"MODE2" (string "1234")   -- octal string
           *: nil ))
       *: item @"380" ( compound
            ( item @"ID" (string "ICAO")      -- icao string
           *: nil ))
       *: item @"070" (quantity @"s" 123.4 )  -- quantity with units
       *: nil )
```

## Parsing

A main problem with asterix parsing is that in some cases the parsing result
is not 'a priori' known. There are 2 notable examples:

### Multiple UAP categories

A `Datablock` contain list of `Records` (of known `UAP`). With a single UAP
asterix specification, parsing of `RawDatablock` into a `Datablock t` is
either success or parsing failure.

However with multiple UAP categories, a parsing process tries all possible
combinations of UAPs and so the result is a 'list of datablocks':
`[Datablock t]`, where
- an empty list represents parsing failure
- single element list is the actual result, what is normally expected
- Multi element list are all valid parsing results, library user must
  decide what to do with multiple results. Typically a user might want
  to check each record in turn if it actually represents a valid record,
  based on the content of subitems.

It is also possible to 'enforce' parsing result to a particular UAP in which
case the parsing result is a single `Datablock t` with a possible
failure (same as with a single UAP categories).

For example, cat001 defines `plot` and `track` UAPs. A parsing of a particular
input string might be (all valid at parsing stage):
- `Datagram of [plot, plot, plot]`
- `Datagram of [track, track, track]`
- `Datagram of [plot, track, track]`
- ... and so on

Library user might examine each record (after parsing stage), to determine if
all records are actually valid, according to the subitem content. With this
additional step, some parsing solutions might be rejected and with some luck,
there is only one remaining `Datagram`, which is the one to be used by
application.

When the input is 'known' to contain only 'tracks' for example, a user
can enforce parsing to try only that UAP and avoid additional processing
stage.

TODO: Describe haskell types involved, in particular
- Getting type of Record from type of Datablock for all cases

### Multiple (dependant) subitem structures

In some rare cases, a subitem structure depends on a value of some other item
(where the other item might not even be present in a particular record). To
cope with this problem, such (dependend) item is always parsed according to the
'default' schema. Once the complete record is parsed, and if a particular
subitem is required, a user might try to convert a default structured value
into a required structure (this operation might fail).

TODO: Describe haskell types involved, in particular
- how to convert from default to target structure
- how to construct a particular structure (and keep the type information)

## Unparsing

`Asterix.Base` module provides `class Unparsing r t` for types that can be
unparsed into target value, such as `Bits` or `SBuilder`. Unparsing into
regular `ByteString` is not efficient (a problem is `ByteString` concatination)
and so the instances are not provided. It is however possible to
(inefficiently) convert from `SBuilder` to `ByteString` if necessary for debug
purposes.

The target type argument in a typeclass comes first, for simplified type
application when necessary, for example:

```haskell
instance Unparsing Bits UItem
instance Unparsing Bits (Item t)
instance Unparsing SBuilder (Record t)

let s1 = unparse @Bits item
    s2 = unparse @SBuilder record

-- explicit type application is not required here
print $ debugBits $ unparse record
```

## Subitem access

## Value conversion

## Generic processing

