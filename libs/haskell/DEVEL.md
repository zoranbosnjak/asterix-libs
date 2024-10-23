# Development environment

```bash
nix-shell

# use 'ghcid'
ghcid --no-title --lint "--command=ghci -Wall -iother -isrc src/Asterix.hs"
ghcid --no-title --lint "--command=ghci -Wall -iother -isrc -itest test/BytesTest.hs"

# run tests
runhaskell -Wall -isrc -itest test/Main.hs
cabal bench
```

