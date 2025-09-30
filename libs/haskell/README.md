# Asterix data processing library for haskell

Features:

- pure haskell implementation
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

```haskell
-- | file: readme-samples/example0.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

import Data.Maybe
import Data.ByteString (ByteString)
import Asterix.Coding
import Asterix.Generated as Gen

assert :: Bool -> IO ()
assert True = pure ()
assert False = error "Assertion error"

-- Select particular asterix categories and editions
type Cat034 = Gen.Cat_034_1_29
type Cat048 = Gen.Cat_048_1_32

type TSacSic = SameType '[ Cat034 ~> "010", Cat048 ~> "010"]

-- Example messages for this application
data Token
    = NorthMarker
    | SectorCrossing Double     -- Azimuth
    | Plot Double Double String -- Rho, Theta, SSR
    deriving (Eq, Show)

-- example message to be encoded
txMessage :: [Token]
txMessage =
    [ NorthMarker
    , SectorCrossing 0.0
    , Plot 10.0 45.0 "7777"
    , SectorCrossing 45.0
    ]

sacSic :: NonSpare TSacSic
sacSic = group (item @"SAC" 1 *: item @"SIC" 2 *: nil)

-- encode token to datablock
encode :: Token -> SBuilder
encode = \case
    NorthMarker ->
        let db :: Datablock (DatablockOf Cat034) = datablock (r *: nil)
            r = record
                ( item @"000" 1 -- North marker message
               *: item @"010" sacSic
               *: nil )
        in unparse db
    SectorCrossing azimuth ->
        let db :: Datablock (DatablockOf Cat034) = datablock (r *: nil)
            r = record
                ( item @"000" 2 -- Sector crossing message
               *: item @"010" sacSic
               *: item @"020" (quantity @"°" (Quantity azimuth))
               *: nil )
        in unparse db
    Plot rho theta ssr ->
        let db :: Datablock (DatablockOf Cat048) = datablock (r *: nil)
            r = record
                ( item @"010" sacSic
               *: item @"040" ( group
                    ( item @"RHO" ( quantity @"NM" (Quantity rho))
                   *: item @"THETA" ( quantity @"°" (Quantity theta))
                   *: nil ))
               *: item @"070" ( group (0 *: 0 *: 0 *: 0
                   *: item @"MODE3A" (string ssr)
                   *: nil))
               *: nil )
        in unparse db

-- decode bytes to message list
decode :: ByteString -> [Token]
decode rxBytes = fromRight $ do
    rawDatablocks <- parseRawDatablocks rxBytes
    tokens <- mapM go rawDatablocks
    pure $ mconcat tokens
  where
    fromRight = \case
        Left (ParsingError e) -> error (show e)
        Right val -> val

    go :: RawDatablock -> Either ParsingError [Token]
    go rawDb = case rawDatablockCategory rawDb of
        34 -> do
            let act = parseRecords (schema @(RecordOf Cat034) Proxy)
            records <- fmap Record <$> parse @StrictParsing act (getRawRecords rawDb)
            pure (mapMaybe handleCat034 records)
        48 -> do
            let act = parseRecords (schema @(RecordOf Cat048) Proxy)
            records <- fmap Record <$> parse @StrictParsing act (getRawRecords rawDb)
            pure (mapMaybe handleCat048 records)
        _ -> pure []

    handleCat034 :: Record (RecordOf Cat034) -> Maybe Token
    handleCat034 rec = case asUint @Int i000 of
        1 -> Just NorthMarker
        2 -> Just $ SectorCrossing (unQuantity $ asQuantity @"°" i020)
        _ -> Nothing
      where
        i000 = fromMaybe (error "missing item") (getItem @"000" rec)
        i020 = fromMaybe (error "missing item") (getItem @"020" rec)

    handleCat048 :: Record (RecordOf Cat048) -> Maybe Token
    handleCat048 rec = Just $ Plot rho theta ssr where
        i040 = fromMaybe (error "missing item") (getItem @"040" rec)
        rho = unQuantity $ asQuantity @"NM" $ getItem @"RHO" i040
        theta = unQuantity $ asQuantity @"°" $ getItem @"THETA" i040
        i070 = fromMaybe (error "missing item") (getItem @"070" rec)
        ssr = asString $ getItem @"MODE3A" i070

expected :: ByteString
expected = fromJust $ unhexlify "220007c0010201220008d00102020030000c9801020a0020000fff220008d001020220"

main :: IO ()
main = do
    -- encode message to bytes
    print ("sending message: " <> show txMessage)
    let datablocks = fmap encode txMessage
        tx = toByteString $ mconcat datablocks
    putStrLn ("bytes on the wire: " <> hexlify tx)
    assert (tx == expected)

    -- decode bytes back to message, expect the same message
    let rx = tx
        rxMessage = decode rx
    assert (rxMessage == txMessage)
```

## Installation and library import

This tutorial assumes importing complete `asterix` module into the current
namespace. In practice however only the required objects could be imported
or the modules might be imported qualified.

```haskell
import Asterix.Coding
import Asterix.Generated as Gen
```

## Asterix object hierarchy and terminology

This library is built aroud the following concepts:

- **Datagram** is a raw binary data as received for example from UDP socket.
- **RawDatablock** is asterix datablock in the form `cat|length|data` with
  correct byte size. A datagram can contain multiple datablocks.
  In some cases it might be sufficient to work with raw datablocks, for
  example "asterix category filtering". In this case, it is not necessary
  to fully parse all asterix records, but is sufficient and faster to
  parse only up to the `RawDatablock` level.
- **Datablock/Record**
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
objects from this library to application specific objects,
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
nesting levels. To access a subitem, use `getItem @"item_name"` function.

The result (if not `Nothing`) can be in turn querried for nested subitems
or converted to required value.

This is a typical usage:

```haskell
-- | file: readme-samples/subitems.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

import Data.Maybe
import Asterix.Coding
import Asterix.Generated as Gen

type Cat048 = Gen.Cat_048_1_32

-- test record
rec048 :: Record (RecordOf Cat048)
rec048 = record ( item @"040" 0 *: item @"070" 0 *: nil)

main :: IO ()
main = do
    let i040 = fromJust $ getItem @"040" rec048
        i070 = fromJust $ getItem @"070" rec048

        -- access subitems
        rho :: Double = unQuantity $ asQuantity @"NM" (getItem @"RHO" i040)
        theta :: Double = unQuantity $ asQuantity @"°" (getItem @"THETA" i040)
        ssr :: String = asString $ getItem @"MODE3A" i070
    print (rho, theta, ssr)
```

## Application examples

### Category filter

**Example**: Category filter, drop datablocks if category == 1

```haskell
-- | file: readme-samples/catflt.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.Maybe
import Data.ByteString (ByteString)
import Asterix.Coding

-- UDP rx test function
receiveFromUdp :: IO ByteString
receiveFromUdp = pure $ fromJust $ unhexlify $ join
    [ "01000401" -- cat1 datablock
    , "02000402" -- cat2 datablock
    ]

-- UDP tx test function
sendToUdp :: SBuilder -> IO ()
sendToUdp = putStrLn . hexlify . toByteString

main :: IO ()
main = do
    inputData <- receiveFromUdp
    let rawDatablocks = case (parseRawDatablocks inputData) of
            Left _ -> error "unable to parse"
            Right val -> val
        validDatablocks = do
            db <- rawDatablocks
            guard $ rawDatablockCategory db /= 1
            pure $ unparse @SBuilder db
        outputData = mconcat validDatablocks
    sendToUdp outputData
```

### Rewrite SAC/SIC in item 010

**Example**: Asterix filter, rewrite SAC/SIC code.

```haskell
-- | file: readme-samples/rewrite-sacsic.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

import GHC.TypeLits
import Data.Maybe
import Data.Either
import Data.ByteString (ByteString)

import Asterix.Coding
import Asterix.Generated as Gen

-- categories/editions of interest
type Cat048 = Gen.Cat_048_1_31
type Cat062 = Gen.Cat_062_1_19
type Cat063 = Gen.Cat_063_1_6

-- All of the following types have the same item "010"
type TSacSic = SameType '[ Cat048 ~> "010", Cat062 ~> "010", Cat063 ~> "010"]

handleDatablock :: forall cat.
    ( IsSchema (RecordOf cat) VRecord
    , SetItem "010" (Record (RecordOf cat)) (NonSpare TSacSic)
    , KnownNat (CategoryOf cat)
    ) => Proxy cat -> NonSpare TSacSic -> ByteString -> SBuilder
handleDatablock _p sacSic bs =
    let act = parseRecords (schema @(RecordOf cat) Proxy)
        records :: [Record (RecordOf cat)]
        records = case parse @StrictParsing act bs of
            Left e -> error (show e)
            Right lst -> (setItem @"010" sacSic) . Record <$> lst
        urecords = fmap unRecord records
    in datablockBuilder (natVal (Proxy @(CategoryOf cat))) urecords

handleRawDatablock :: NonSpare TSacSic -> RawDatablock -> SBuilder
handleRawDatablock sacSic rawDb = case rawDatablockCategory rawDb of
    48 -> handleDatablock @Cat048 Proxy sacSic rawRecords
    62 -> handleDatablock @Cat062 Proxy sacSic rawRecords
    63 -> handleDatablock @Cat063 Proxy sacSic rawRecords
    cat -> error ("unsupported category: " <> show cat)
  where
    rawRecords = getRawRecords rawDb

rewriteSacSic :: NonSpare TSacSic -> ByteString -> SBuilder
rewriteSacSic sacSic bs = output where
    rawDatablocks = fromRight (error "unexpected") $ parseRawDatablocks bs
    result = fmap (handleRawDatablock sacSic) rawDatablocks
    output = mconcat result

-- Dummy rx function (generate valid asterix datagram).
readBytesFromTheNetwork :: IO ByteString
readBytesFromTheNetwork = do
    let rec :: Record (RecordOf Cat048)
        rec = record
            ( item @"010" 0
           *: item @"040" 0
           *: nil)
        db1, db2 :: Datablock (DatablockOf Cat048)
        db1 = datablock (rec *: rec *: nil)
        db2 = datablock (rec *: nil)
    pure $ toByteString (unparse @SBuilder db1 <> unparse @SBuilder db2)

-- Dummy tx function
txBytesToTheNetwork :: SBuilder -> IO ()
txBytesToTheNetwork = putStrLn . hexlify . toByteString

main :: IO ()
main = do
    sInput <- readBytesFromTheNetwork
    let newSacSic :: NonSpare TSacSic
        newSacSic = group (1 *: 2 *: nil)
        sOutput = rewriteSacSic newSacSic sInput
        expected = fromJust $ unhexlify $ "300011900102000000009001020000000030000a90010200000000"
    txBytesToTheNetwork sOutput
    case expected == (toByteString sOutput) of
        True -> print "OK"
        False -> error "unexpected output"
```

### Spare bits

Some bits are defined as *Spare*, which are normally set to `0`.
With this library:

- A user is able set spare bits to any value, including abusing spare bits
  to contain non-zero value.
- When parsing data, tolerate spare bits to contain any value. It is up
  to the application to check the spare bits if desired.

Multiple spare bit groups can be defined on a single item.
`getSpares` function returns the actual values of all spare bit groups.

**Example**

```haskell
-- | file: readme-samples/spares.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

import Data.Maybe

import Asterix.Coding
import Asterix.Generated as Gen

-- I062/120 contain single group of spare bits
type Spec = Gen.Cat_062_1_20

assert :: Bool -> IO ()
assert True = pure ()
assert False = error "Assertion error"

main :: IO ()
main = do
    -- create regular record with spare bits set to '0'
    let rec1 :: Record (RecordOf Spec)
        rec1 = record (item @"120" (group (0 *: item @"MODE2" 0x1234 *: nil))
                    *: nil)
        i120a = fromJust $ getItem @"120" rec1
    assert ((bitsToNum <$> getSpares i120a) == [0::Int])

    -- create record, abuse spare bits, set to '0xf'
    let rec2 :: Record (RecordOf Spec)
        rec2 = record (item @"120" (group (0xf *: item @"MODE2" 0x1234 *: nil))
                    *: nil)
        i120b = fromJust $ getItem @"120" rec2
    assert ((bitsToNum <$> getSpares i120b) == [0xf::Int])
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

```haskell
-- | file: readme-samples/ref.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

import Data.Either
import Data.Maybe
import Asterix.Coding
import Asterix.Generated as Gen

type Spec = Gen.Cat_062_1_20
type Ref  = Gen.Ref_062_1_3

assert :: Bool -> IO ()
assert True = pure ()
assert False = error "Assertion error"

-- create 'RE' subitem
ref :: Expansion (ExpansionOf Ref)
ref = expansion
    ( item @"CST" (repetitive [0])
   *: item @"CSN" (repetitive [1, 2])
   *: item @"V3" (compound
        ( item @"PS3" 0
       *: nil ))
   *: nil )

-- create record, insert 'RE' subitem
rec :: Record (RecordOf Spec)
rec = record
    ( item @"010" (group (item @"SAC" 1 *: item @"SIC" 2 *: nil))
   *: item @"RE" (explicit ref)
   *: nil )

db :: Datablock (DatablockOf Spec)
db = datablock (rec *: nil)

main :: IO ()
main = do
    let s = unparse @SBuilder db
        bs = toByteString s
        expected = fromJust $ unhexlify $ "3e001b8101010104010211c8010000000000020000010000028000"
    assert (bs == expected)

    -- first stage, parse to the record
    let rawDatablocks = fromRight (error "unexpected") (parseRawDatablocks bs)
    assert (length rawDatablocks == 1) -- expecting 1 datablock
    let rawDatablock = rawDatablocks !! 0
        act = parseRecords (schema @(RecordOf Spec) Proxy)
        result1 = fromRight (error "unexpected")
            (parse @StrictParsing act (getRawRecords rawDatablock))
    assert (length result1 == 1) -- expecting one record
    let rec2 :: Record (RecordOf Spec)
        rec2 = Record (result1 !! 0)

    -- get 'RE' subitem,
    let reSubitem = getVariation $ fromJust $ getItem @"RE" rec2
        reBytes = toByteString $ bitsToBuilder $ getExplicitData reSubitem

    -- second stage: parse 'RE' structure
    let act2 = parseExpansion (schema @(ExpansionOf Ref) Proxy)
        refReadback :: Expansion (ExpansionOf Ref)
        refReadback = Expansion (fromRight (error "unexpected")
            (parse @StrictParsing act2 reBytes))

    -- expecting the same 'ref' as the original
    assert (unparse @Bits refReadback == unparse ref)

    -- we have a structure back and we can extract the values
    let iCsn = fromJust (getItem @"CSN" refReadback)
        lst = getRepetitiveItems $ getVariation iCsn
    assert (length lst == 2)
    assert (asUint @Int (lst !! 0) == 1)
    assert (asUint @Int (lst !! 1) == 2)

    putStrLn "OK"
```

## Generic asterix processing

*Generic processing* in this context means working with asterix data where
the subitem names and types are determined at runtime. That is: the explicit
subitem names are never mentioned in the application source code.

This is in contrast to *application specific processing*, where we are
explicit about subitems, for example `["010", "SAC"]`.

**Example**: Show raw content of all toplevel items of each record

```haskell
-- | file: readme-samples/generic-names.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.Word
import Data.Maybe
import Data.Either
import Data.Map as Map
import Data.ByteString (ByteString)

import Asterix.Coding
import Asterix.Generated as Gen

specs :: Map Word8 VRecord
specs = Map.fromList
    [ (48, schema @(RecordOf Cat_048_1_31) Proxy)
    , (62, schema @(RecordOf Cat_062_1_19) Proxy)
    , (63, schema @(RecordOf Cat_063_1_6) Proxy)
    -- , ...
    ]

-- some test input bytes
s :: ByteString
s = mconcat $ fmap (fromJust . unhexlify)
    [ "3e00a5254327d835a95a0d0a2baf256af940e8a8d0caa1a594e1e525f2e32bc0448b"
    , "0e34c0b6211b5847038319d1b88d714b990a6e061589a414209d2e1d00ba5602248e"
    , "64092c2a0410138b2c030621c2043080fe06182ee40d2fa51078192cce70e9af5435"
    , "aeb2e3c74efc7107052ce9a0a721290cb5b2b566137911b5315fa412250031b95579"
    , "03ed2ef47142ed8a79165c82fb803c0e38c7f7d641c1a4a77740960737"
    ]


handleNonspare :: Word8 -> (GUapItem ValueLevel, Maybe (RecordItem UNonSpare))
    -> IO ()
handleNonspare cat = \case
    (GUapItem (GNonSpare name _title _rv), Just (RecordItem nsp)) -> do
        print (cat, name, debugBits $ unparse @Bits nsp)
        -- depending on the application, we might want to display
        -- deep subitems, which is possible by examining 'nsp' object
    _ -> pure ()

main :: IO ()
main = do
    let rawDatablocks = fromRight (error "unexpected") $ parseRawDatablocks s
    forM_ rawDatablocks $ \db -> do
        let cat = rawDatablockCategory db
        case Map.lookup cat specs of
            Nothing -> print ("unsupported category", cat)
            Just (GRecord sch) -> do
                let act = parseRecords (GRecord sch)
                    records = fromRight (error "unexpected")
                        (parse @StrictParsing act (getRawRecords db))
                forM_ records $ \rec -> do
                    mapM_ (handleNonspare cat) (zip sch $ uRecItems rec)
```

**Example**: Generate dummy single record datablock with all fixed items set to zero

```haskell
-- | file: readme-samples/generic-zero.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

import Data.Maybe
import Asterix.Coding
import Asterix.Generated as Gen

assert :: Bool -> IO ()
assert True = pure ()
assert False = error "Assertion error"

-- we could even randomly select a category/edition from the 'manifest',
-- but for simplicity just use a particular spec's schema
schAsterix :: VAsterix
schAsterix = schema @Cat_062_1_20 Proxy

schCat :: Int
schCat = case schAsterix of
    GAsterixBasic cat _ed _uap -> cat
    GAsterixExpansion cat _ed _exp -> cat

schRecord :: VRecord
schRecord = case schAsterix of
    GAsterixBasic _cat _ed (GUap r) -> r
    _ -> error "unexpected"

schItems :: [VUapItem]
schItems =
    let GRecord lst = schRecord
    in lst

rec :: URecord
rec = URecord bld items
  where
    goVar :: VVariation -> Maybe UVariation
    goVar = \case
        GElement o n _cont -> Just $ UElement $ integerToBits o n 0
        GGroup _o lst -> UGroup <$> mapM goItem lst
        _ -> Nothing -- skip for this test

    goItem :: VItem -> Maybe UItem
    goItem = \case
        GSpare o n -> Just $ USpare $ integerToBits o n 0
        GItem nsp -> UItem <$> goNsp nsp

    goRv :: VRule VVariation -> Maybe URuleVar
    goRv = \case
        GContextFree var -> URuleVar <$> goVar var
        GDependent _lst1 var _lst2 -> URuleVar <$> goVar var

    goNsp :: VNonSpare -> Maybe UNonSpare
    goNsp (GNonSpare _name _title rv) = UNonSpare <$> goRv rv

    f :: VUapItem -> Maybe (RecordItem UNonSpare)
    f = \case
        GUapItem nsp -> RecordItem <$> goNsp nsp
        _ -> Nothing
    items = fmap f schItems
    bld = rebuildRecord items

db :: UDatablock
db = UDatablock bld records
  where
    records = [(Nothing, rec)]
    bld = datablockBuilder schCat (fmap snd records)

main :: IO ()
main = do
    let expected = fromJust $ unhexlify $ "3e0038bfe9bd5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        result = toByteString $ unparse @SBuilder db
    putStrLn $ hexlify result
    assert (result == expected)
```

### Library manifest

This library defines a `manifest` structure in the form:

```haskell
manifest :: [GAsterix 'ValueLevel]
manifest =
    [ schema @Cat_001_1_2 Proxy
    , schema @Cat_001_1_3 Proxy
    , schema @Cat_001_1_4 Proxy
    -- ...
    ]
```

This structure can be used to extract *latest* editions for each defined
category, for example:

```haskell
-- | file: readme-samples/generic-latest.hs
import Control.Monad
import Data.List (sort)
import Data.Map as Map
import Data.Map.Merge.Lazy as Map
import Asterix.Schema
import Asterix.Generated as Gen

latest :: Map VInt VEdition
latest = Prelude.foldr f mempty Gen.manifest
  where
    f :: VAsterix -> Map VInt VEdition -> Map VInt VEdition
    f sch acc = case sch of
        GAsterixBasic cat ed _uap -> Map.merge
            preserveMissing
            preserveMissing
            (zipWithMatched (\_key ed1 ed2 -> max ed1 ed2))
            acc
            (Map.singleton cat ed)
        GAsterixExpansion _cat _ed _exp -> acc

main :: IO ()
main = forM_ (sort (Map.keys latest)) $ \cat -> do
    print (cat, latest Map.! cat)
```

Alternatively, a prefered way is to be explicit about each edition,
for example:

```haskell
-- | file: readme-samples/generic-editon.hs
import Data.List (sort)
import Data.Map as Map
import Asterix.Schema
import Asterix.Generated as Gen

specs :: Map VInt VAsterix
specs = Map.fromList
    [ (48, schema @Cat_048_1_31 Proxy)
    , (62, schema @Cat_062_1_19 Proxy)
    , (63, schema @Cat_063_1_6 Proxy)
    -- , ...
    ]
main :: IO ()
main = mapM_ print (sort $ Map.keys specs)
```

## Error handling

Some operation (eg. parsing) can fail on unexpected input. In such case,
this library returns `ParsingError`.

```haskell
parseDataBlocks :: ByteString -> [RawDatablock]
parseDataBlocks s = case parseRawDatablocks s of
    Left (ParsingError _err) -> [] -- decide what to do in case of error
    Right val -> val
```

For clarity, the error handling part is skipped in some parts of this tutorial.

## Miscellaneous project and source code remarks

A core part of this project is the `Asterix.Generated` module, where
all important aspect of asterix data format is captured as haskell types.
Having asterix specifications defined as types is important, to be able to
catch errors at compile time (e.g. compiler can detect access attempt
into unspecified item).

The specifications are available at runtime too. For example,
we want to be able to generate random record of some category/edition or
generically convert binary asterix data to `json`. So, each type level
specification is converted into a value level counterpart.
The `Asterix.Schema` provides the necessary conversion function `schema`
from types to term:

```haskell
valueLevel = schema @typeLevel Proxy
```

### Schema naming conventions

Naming conventions for types describing asterix schema:

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

### Constructing

A special `DSL` is provided to construct asterix objects within the source
code. It's based on processing `HList` of subitems (list of different types).
'HList' constuction is performed with `*:` as `HCons` operator and `nil` as
list termination `HNil`. For example:

```haskell
-- | file: readme-samples/construct1.hs
{-# LANGUAGE DataKinds #-}

import Data.Maybe

import Asterix.Coding
import Asterix.Generated as Gen

assert :: Bool -> IO ()
assert True = pure ()
assert False = error "Assertion error"

db062 :: Datablock (DatablockOf Cat_062_1_21)
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

main :: IO ()
main = do
    let sb :: SBuilder = unparse db062
        result = toByteString sb
        expected = fromJust $ unhexlify $ "3e0015911101100102003db34024304f820820029c"
    assert (result == expected)
    putStrLn $ hexlify result
```

### Parsing

Regular asterix parsing with this library is performed in the following stages:
- Parsing datagram (`ByteString` as received on the network) to
  `RawDatablocks`. A `RawDatablock` represents input data which is
  correctly parsed, according to *asterix datablock cat/length* schema.
- Depending on *asterix category*, each `RawDatablock` can be either
  skipped or parsed to the next level, resulting in list of records.
- Once the records are parsed according to a particular UAP schema,
  each record is normally wrapped inside typed `Record t`, such that a
  content structure is statically known and the subitems can be accessed
  by name (at the type level), using type application.

Parsing is performed using `parse` function, for example:

```haskell
-- | file: readme-samples/parsing-normal.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

import Data.ByteString (ByteString)
import Asterix.Coding
import Asterix.Generated as Gen

type Cat034 = Gen.Cat_034_1_29

db :: Datablock (DatablockOf Cat034)
db = datablock (record (item @"000" 1 *: nil ) *: nil)

sample :: ByteString
sample = toByteString $ unparse @SBuilder db

main :: IO ()
main = do
    let rawDatablocks :: [RawDatablock]
        rawDatablocks = case parseRawDatablocks sample of
            Left (ParsingError _) -> error "can not parse raw datablocks"
            Right val -> val
        rawDatablock :: RawDatablock = rawDatablocks !! 0
        rawRecords = case rawDatablockCategory rawDatablock of
            34 -> getRawRecords rawDatablock
            _ -> error "unexpected category"
        parsingAction = parseRecords (schema @(RecordOf Cat034) Proxy)
        records :: [Record (RecordOf Cat034)]
        records = case parse @StrictParsing parsingAction rawRecords of
            Left (ParsingError _) -> error "can not parse records"
            Right lst -> fmap Record lst
        record0 = records !! 0
        i000 = case getItem @"000" record0 of
            Nothing -> error "Missing item 000"
            Just val -> val

    case asUint @Integer i000 of
        1 -> print "OK"
        _ -> error "unexpected result"
```

In some cases, the parsing result type is not 'a priori' known and the
second stage of parsing becomes more complicated.

### Unparsing

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

```haskell
-- | file: readme-samples/dep-content.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.Maybe
import Data.Either
import Data.ByteString (ByteString)

import Asterix.Coding
import Asterix.Generated as Gen

type Spec = Gen.Cat_062_1_20

assert :: Bool -> IO ()
assert True = pure ()
assert False = error "Assertion error"

-- create records by different methods

-- set raw value
rec0 :: Record (RecordOf Spec)
rec0 = record
    ( item @"380" (compound
        ( item @"IAS" (group
            ( item @"IM" 0 -- set IM to 0
           *: item @"IAS" 1 -- set IAS to raw value 1 (no unit conversion)
           *: nil))
       *: nil))
   *: nil )

-- set raw value using default case
rec1 :: Record (RecordOf Spec)
rec1 = record
    ( item @"380" (compound
        ( item @"IAS" (group
            ( item @"IM" 0 -- set IM to 0
           *: item @"IAS" 1 -- same as above
           *: nil))
       *: nil))
   *: nil )

-- set IAS speed (NM/s)
rec2 :: Record (RecordOf Spec)
rec2 = record
    ( item @"380" (compound
        ( item @"IAS" (group
            ( item @"IM" 0 -- set IM to 0
           -- use case with index 0 (IAS), set IAS to 1.2 NM/s
           *: item @"IAS" (quantity @"NM/s" @('Just 0) 1.2)
           *: nil))
       *: nil))
   *: nil )

-- set Mach speed
rec3 :: Record (RecordOf Spec)
rec3 = record
    ( item @"380" (compound
        ( item @"IAS" (group
            ( item @"IM" 1 -- set IM to 1 (Mach)
           -- use case with index 1 (Mach), set IAS to 0.8 Mach
           *: item @"IAS" (quantity @"Mach" @('Just 1) 0.8)
           *: nil))
       *: nil))
   *: nil )

db0 :: Datablock (DatablockOf Spec)
db0 = datablock (rec0 *: rec1 *: rec2 *: rec3 *: nil)

expected :: ByteString
expected = fromJust $ unhexlify "3e0017011010000101101000010110104ccd0110108320"

main :: IO ()
main = do
    assert ((toByteString $ unparse @SBuilder db0) == expected)

    -- parse and interpret data from the example above
    let rx = expected
        rawDatablocks = fromRight (error "unexpected") (parseRawDatablocks rx)
    forM_ rawDatablocks $ \db -> do
        assert (rawDatablockCategory db == 62)
        let act = parseRecords (schema @(RecordOf Spec) Proxy)
            records :: [Record (RecordOf Spec)]
            records = fromRight (error "unexpected")
                (fmap Record <$> parse @StrictParsing act (getRawRecords db))
        forM_ (zip [0::Int ..] records) $ \(cnt, rec) -> do
            let i380 = fromJust $ getItem @"380" rec
                iIAS1 = fromJust $ getItem @"IAS" $ getVariation i380
                iIM = getItem @"IM" iIAS1
                iIAS2 = getItem @"IAS" iIAS1
                value :: Double
                value = case asUint @Int iIM of
                    -- this is IAS, convert to 'NM/s', use case with index (0,)
                    0 -> unQuantity $ asQuantity @"NM/s" @('Just 0) iIAS2
                    -- this is Mach, convert to 'Mach', use case with index (1,)
                    1 -> unQuantity $ asQuantity @"Mach" @('Just 1) iIAS2
                    _ -> error "unexpected value"

            print ("--- record", cnt, "---")
            print ("I062/380/IAS/IM raw value:", asUint @Integer iIM)
            print ("I062/380/IAS/IAS raw value:", asUint @Integer iIAS2)
            print ("converted value", value)
```

#### Handling **variation dependency**

This example demonstrates how to work with **variation dependency**,
such as `I004/120/CC/CPC`.

```haskell
-- | file: readme-samples/dep-variation.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.Maybe
import Data.Either
import Data.ByteString (ByteString)
import Asterix.Coding
import Asterix.Generated as Gen

type Spec = Gen.Cat_004_1_13 -- Cat 004, edition 1.13

-- Item 'I004/120/CC/CPC' depends on I004/000 and I004/120/CC/TID values
-- Default case is: element3, raw, but there are many other cases.
-- See asterix specification for details.
-- This example handles the following cases:
-- case (5, 1): element 3, table
-- case (9, 2): group (('RAS', element1, table), spare 2)

assert :: Bool -> IO ()
assert True = pure ()
assert False = error "Assertion error"

-- case (0, 0) - invalid combination
rec0 :: Record (RecordOf Spec)
rec0 = record
    ( item @"000" 0 -- invalid value
   *: item @"120" (compound
        ( item @"CC" (group
            ( item @"TID" 0
           *: item @"CPC" 0 -- set to raw value 0
           *: item @"CS" 0
           *: nil))
       *: nil))
   *: nil)

-- case (5, 1)
rec1 :: Record (RecordOf Spec)
rec1 = record
    ( item @"000" 5 -- Area Proximity Warning (APW)
   *: item @"120" (compound
        ( item @"CC" (group
            ( item @"TID" 1
           *: item @"CPC" 0 -- set to raw value 0
           *: item @"CS" 0
           *: nil))
       *: nil))
   *: nil)

-- case (9, 2)
-- get variation structure of case (9, 2)
-- and create object of that structure ('RAS' + spare item)
obj :: Variation (DepRule (Spec ~> "120" ~> "CC" ~> "CPC") '[ 9, 2])
obj = group (item @"RAS" 1 *: spare *: nil)
rec2 :: Record (RecordOf Spec)
rec2 = record
    ( item @"000" 9  -- RIMCAS Arrival / Landing Monitor (ALM)
   *: item @"120" (compound
        ( item @"CC" (group
            ( item @"TID" 2
           *: item @"CPC" (fromInteger $ asUint obj)
           *: item @"CS" 0
           *: nil))
       *: nil))
   *: nil)

db0 :: Datablock (DatablockOf Spec)
db0 = datablock (rec0 *: rec1 *: rec2 *: nil)

expected :: ByteString
expected = fromJust $ unhexlify "040012412000400041200540104120094028"

main :: IO ()
main = do
    assert ((toByteString $ unparse @SBuilder db0) == expected)

    -- parse and interpret data from the example above
    let rx = expected
        rawDatablocks = fromRight (error "unexpected") (parseRawDatablocks rx)
    forM_ rawDatablocks $ \db -> do
        assert (rawDatablockCategory db == 4)
        let act = parseRecords (schema @(RecordOf Spec) Proxy)
            records :: [Record (RecordOf Spec)]
            records = fromRight (error "unexpected")
                (fmap Record <$> parse @StrictParsing act (getRawRecords db))
        forM_ (zip [0::Int ..] records) $ \(cnt, rec) -> do
            print ("--- record", cnt, "---")
            let i000 = fromJust $ getItem @"000" rec
                i120 = fromJust $ getItem @"120" rec
                iCC = fromJust $ getItem @"CC" $ getVariation i120
                iTID = asUint @Int (getItem @"TID" iCC)
                iCPC = getItem @"CPC" iCC
                _iCS = asUint @Int (getItem @"CS" iCC)
                index = (asUint @Int i000, iTID)
                value = case index of
                    (5, 1) ->
                        let x = asUint @Int iCPC
                        in Just ("case 5,1 raw " <> show x)
                    (9, 2) ->
                        let fromRight' = fromRight (error "unexpected")
                            varCPC = fromRight' $ getDepVariation @'[ 9, 2] iCPC
                            ras = asUint @Int (getItem @"RAS" varCPC)
                            spares = asUint @Int <$> getSpares varCPC
                        in Just ("case 9,2 RAS "
                            <> show ras <> ", " <> show spares)
                    _ -> Nothing :: Maybe String
            print value
```

### Multiple UAP categories

With multiple UAP categories, it is in general not possible to unambiguously
determine parsing success or failure result.
In this case, a user has the following options:
- try to parse all possible UAP combinations and post-process results
- enforce parsing according to particular UAP and recover unambiguously
  parsing result (success or failure)

**Trying all possible combinations**

This kind of parsing is provided by `parseRecordsTry` function.
In this case, the result is a 'list of possible parsing results', where
- An empty list represents parsing failure.
- Single element list is the actual result, which is normally expected.
  The (one) element of a list is itself a list of records.
- Multi element list are all valid parsing results, library user shall
  decide what to do with multiple results. Typically a user might want
  to check each record in turn if it actually represents a valid record,
  based on the content of particular subitems.

For example, cat001 defines `plot` and `track` UAPs. Parsing of a particular
input string might be (all valid at parsing stage):
- `Datagram of [plot, plot, plot]`
- `Datagram of [track, track, track]`
- `Datagram of [plot, track, track]`
- ... and so on

Library user might examine each record (after parsing stage), to determine if
all records are actually valid, according to the subitem content. With this
additional step, some parsing solutions might be rejected and with some luck,
there is only one remaining result.

Example:

```haskell
-- | file: readme-samples/parsing-cat001-try.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.ByteString (ByteString)
import Asterix.Coding
import Asterix.Generated as Gen

type Cat001 = Gen.Cat_001_1_4

recPlot :: Record (RecordOfUap Cat001 "plot")
recPlot = record
    ( item @"020" (extended
        ( item @"TYP" 0 *: 0 *: 0 *: 0 *: 0 *: 0 *: fx *: nil))
   *: nil )

recTrack :: Record (RecordOfUap Cat001 "track")
recTrack = record
    ( item @"020" (extended
        ( item @"TYP" 1 *: 0 *: 0 *: 0 *: 0 *: 0 *: fx *: nil))
   *: nil )

db :: Datablock (DatablockOf Cat001)
db = datablock ( recPlot *: recTrack *: nil)

sample :: ByteString
sample = toByteString $ unparse @SBuilder db

handlePlot :: Record (RecordOfUap Cat001 "plot") -> IO ()
handlePlot _rec = putStrLn "got plot"

handleTrack :: Record (RecordOfUap Cat001 "track") -> IO ()
handleTrack _rec = putStrLn "got track"

main :: IO ()
main = do
    let rawDatablocks :: [RawDatablock]
        rawDatablocks = case parseRawDatablocks sample of
            Left (ParsingError _) -> error "can not parse raw datablocks"
            Right val -> val
        rawDatablock :: RawDatablock = rawDatablocks !! 0
        rawRecords = case rawDatablockCategory rawDatablock of
            1 -> getRawRecords rawDatablock
            _ -> error "unexpected category"
        parsingAction = parseRecordsTry (schema @Cat001 Proxy)
        results = case parse @StrictParsing parsingAction rawRecords of
            Left (ParsingError _) -> error "unexpected parse failure"
            Right val -> val

    case length results of
        4 -> pure ()
        _ -> error "unexpected length of results"

    forM_ results $ \result -> do
        putStrLn "possible result"
        forM_ result $ \(name, rec) -> case name of
            "plot" -> handlePlot (Record rec)
            "track" -> handleTrack (Record rec)
            _ -> error "unexpected record type"
```

**Enforce parsing according to a particular UAP***

When the input is 'known' to contain only 'tracks' for example, a user
can enforce parsing to try only that UAP and avoid additional processing
stage. In this case, the situation is similar to the regular single
UAP parsing. Example:

```haskell
-- | file: readme-samples/parsing-cat001-tracks.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Asterix.Coding
import Asterix.Generated as Gen

type Cat001 = Gen.Cat_001_1_4

recTrack :: Record (RecordOfUap Cat001 "track")
recTrack = record
    ( item @"020" (extended
        ( item @"TYP" 1 *: 0 *: 0 *: 0 *: 0 *: 0 *: fx *: nil))
   *: nil )

db :: Datablock (DatablockOf Cat001)
db = datablock ( recTrack *: recTrack *: nil)

sample :: ByteString
sample = toByteString $ unparse @SBuilder db

handleTrack :: Record (RecordOfUap Cat001 "track") -> IO ()
handleTrack _rec = putStrLn "got track"

main :: IO ()
main = do
    let rawDatablocks :: [RawDatablock]
        rawDatablocks = case parseRawDatablocks sample of
            Left (ParsingError _) -> error "can not parse raw datablocks"
            Right val -> val
        rawDatablock :: RawDatablock = rawDatablocks !! 0
        rawRecords = case rawDatablockCategory rawDatablock of
            1 -> getRawRecords rawDatablock
            _ -> error "unexpected category"
        parsingAction = parseRecords (schema @(RecordOfUap Cat001 "track") Proxy)
        records = case parse @StrictParsing parsingAction rawRecords of
            Left (ParsingError _) -> error "can not parse records"
            Right lst -> fmap Record lst
    mapM_ handleTrack records
```

### RFS handling

This library supports RFS mechanism for categories that include RFS
indicators. For such cases, it is possible to sequence subitems in
any order. Once such record is created or parsed, a user can extract
subitems using `getRfsItem` function. The result in this case is
a list, since the item can be present in the record multiple times.
An empty list indicates that no such item is present in the RFS.

**Example**

```haskell
-- | file: readme-samples/rfs.hs
{-# LANGUAGE DataKinds #-}

import Control.Monad
import Data.Maybe
import Asterix.Coding
import Asterix.Generated as Gen

-- cat008 contains RFS indicator, so we are able to add RFS items
type Spec = Gen.Cat_008_1_3

assert :: Bool -> IO ()
assert True = pure ()
assert False = error "Assertion error"

rec1 :: Record (RecordOf Spec)
rec1 = record
    -- add some regular items
    ( item @"000" 1
   *: item @"010" (group (item @"SAC" 1 *: item @"SIC" 2 *: nil))
    -- add items as RFS (may repeat)
   *: rfs
        ( item @"010" (group (item @"SAC" 1 *: item @"SIC" 2 *: nil))
       *: item @"010" (group (item @"SAC" 1 *: item @"SIC" 2 *: nil))
       *: nil)
   *: nil )

main :: IO ()
main = do
    -- extract regular item 010
    let i010Regular = fromJust $ getItem @"010" rec1
    putStrLn $ debugBits $ unparse @Bits i010Regular

    -- extract RFS items 010, expecting 2 such items
    let i010Rfs = getRfsItem @"010" rec1
    assert (length i010Rfs == 2)
    forM_ i010Rfs $ \i -> do
        putStrLn $ debugBits $ unparse @Bits i

    -- but item '000' is not present in RFS
    assert (length (getRfsItem @"000" rec1) == 0)
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

```haskell
data ParsingMode
    = StrictParsing
    | PartialParsing
```

This example demonstrates both parsing modes:

```haskell
-- | file: readme-samples/parsing-partial-mode.hs
{-# LANGUAGE DataKinds #-}

import Data.Maybe
import Data.Either
import Data.ByteString (ByteString)

import Asterix.Coding
import Asterix.Generated as Gen

type SpecOld = Gen.Cat_063_1_6
type SpecNew = Gen.Cat_063_1_7

assert :: Bool -> IO ()
assert True = pure ()
assert False = error "Assertion error"

-- In the new spec, item 060 is extended to contain 3 groups,
-- while in the old spec it only contain2 groups.
-- Create record according to the new spec
rec0 :: Record (RecordOf SpecNew)
rec0 = record
    ( item @"010" (group (item @"SAC" 1 *: item @"SIC" 2 *: nil))
   *: item @"015" 3
   *: item @"060" (extendedGroups (1 *: 2 *: 3 *: nil))
   *: nil )

-- This bytestring represents the record of SpecNew
bs :: ByteString
bs = toByteString $ unparse @SBuilder rec0

main :: IO ()
main = do
    let expected = fromJust $ unhexlify "c8010203030506"
    assert (bs == expected)

    -- We should be able to parse the record, using the new spec
    -- and get the same record back.
    let act1 = parseRecord (schema @(RecordOf SpecNew) Proxy)
        rec1 = fromRight (error "unexpected") (parse @StrictParsing act1 bs)
    assert (unparse @Bits rec1 == unparse rec0)

    -- Strict parsing with the old spec fails.
    let act2 = parseRecord (schema @(RecordOf SpecOld) Proxy)
        rec2 = parse @StrictParsing act2 bs
    assert $ isLeft rec2

    -- However, we can still try to parse using the PartialParsing mode.
    let act3 = parseRecord (schema @(RecordOf SpecOld) Proxy)
        rec3 :: Record (RecordOf SpecOld)
        rec3 = Record $ fromRight (error "unexpected")
            (parse @PartialParsing act3 bs)

    -- We accept the fact that resulting record might not be complete, but
    -- items "010" and "015" are valid, even if parsing using the old edition.
    let i010 = fromJust $ getItem @"010" rec3
        i015 = fromJust $ getItem @"015" rec3
    assert (asUint @Int i010 == 0x0102)
    assert (asUint @Int i015 == 3)

    -- Note, that the result in this case in not equal to the original record.
    assert (unparse @Bits rec3 /= unparse rec0)
```

## Unit tests

For more examples using test specifications, see also project repository
[unit tests](https://github.com/zoranbosnjak/asterix-libs/tree/main/libs/haskell/test).

