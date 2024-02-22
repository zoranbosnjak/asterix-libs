# haskell version

- when constructing, only 'Builder' is required, not a complete structure
  (no need to inspect further)
  When building, we still need a newtype wrapper over Builder, to prevent
  accidental combination of non-valid structures, or example

```haskell
-- This is different between 'Building' and 'Parsing'.
newtype Record = Record (Builder...)
newtype Datablock = Datablock (Builder...)

mkDatablock :: [Record] -> Datablock
mkRecord :: ? -> Record
```

- Builder 0 0 (but not other alignments) can have Semigroup / Monoid instance

- tests/TestGeneric, when creating complete datagram,
  specify 'val1 :: Bool' for 'Spare' items and 'val2 :: Bool' for regular
  items. This is to be able to populate spare bits too (for test)

- check 'continuation style' and 'rank-n-types'

## performance

- use criterion or (simpler) tasty-bench for testing

- check StrictData extension (enable or not)?

# python version

- return instead of raise
  It's more general, on the calling site, it can be turned (with a decorator)
  into exception or some Maybe monad or...

