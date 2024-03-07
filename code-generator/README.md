# Source code generator for processing asterix data

## Building and running

```bash
nix-build
./result/bin/ast-code-generator -h
```

## Development environment

```bash
nix-shell

# fix permissions if necessary
chmod go-w .ghci
chmod go-w .

# check defined and used references
tagref

# lint
hlint {path}
find . | grep "\.hs$" | xargs hlint

# auto adjust style
stylish-haskell --inplace {path}
find . | grep "\.hs$" | xargs stylish-haskell --inplace

# run 'ghcid'
ghcid --no-title --lint "--command=ghci -Wall -iother -isrc src/Main.hs"

# run program, show usage
runhaskell $EXTENSIONS -iother -isrc ./src/Main.hs --help

# select target language
lang=python
lang=haskell

# generate code for test run
runhaskell $EXTENSIONS -iother -isrc ./src/Main.hs \
    --test --language $lang $TESTSPECS

# generate code with all defined specs
runhaskell $EXTENSIONS -iother -isrc ./src/Main.hs \
    --language $lang $SPECS

exit
```
