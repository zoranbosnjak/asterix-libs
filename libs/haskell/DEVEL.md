# Development environment

```bash
nix-shell

# lint
hlint {path}
find . | grep "\.hs$" | xargs hlint

# auto adjust style
stylish-haskell --inplace {path}
find . | grep "\.hs$" | xargs stylish-haskell --inplace

# use 'ghcid'
ghcid --no-title --lint "--command=ghci -Wall -iother -isrc src/Asterix/Coding.hs"
ghcid --no-title --lint "--command=ghci -Wall -iother -isrc -itest test/Main.hs"

# run tests
runhaskell -Wall -isrc -itest test/Main.hs
find . | grep "\.hs" | entr sh -c 'clear && date && runhaskell -Wall -isrc -itest test/Main.hs'
cabal bench
```

