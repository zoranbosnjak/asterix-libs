# Source code generator for processing asterix data

## Building and running

```bash
nix-build
./result/bin/ast-code-generator -h
```

## Development environment

```bash
nix-shell

# check defined and used references
tagref

# fix permissions if necessary
chmod go-w .ghci
chmod go-w .

# run 'ghcid'
ghcid "--command=ghci -Wall -iother -isrc src/Main.hs"

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
