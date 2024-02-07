# Asterix data processing library

## Development

```bash
nix-shell

# use 'ghcid'
ghcid "--command=ghci -Wall -iother -isrc src/Asterix.hs"
ghcid "--command=ghci -Wall -iother -isrc -itest test/BytesTest.hs"

# run tests
runhaskell -Wall -isrc -itest test/Main.hs
cabal bench
```
