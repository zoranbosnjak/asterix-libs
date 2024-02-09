# Asterix data processing library

## Development

```bash
nix-shell

# use 'ghcid'
ghcid "--command=ghci -Wall -iother -isrc src/Asterix.hs"
ghcid "--command=ghci -Wall -iother -isrc -itest test/Main.hs"

# run tests
runhaskell $EXTENSIONS -Wall -isrc -itest test/Main.hs

# build with cabal
cabal build -j

# run benchmarks
cabal bench
```
