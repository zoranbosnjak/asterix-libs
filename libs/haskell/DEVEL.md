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
ghcid --no-title --lint "--command=ghci \
    -Wall \
    -Wincomplete-uni-patterns \
    -Wincomplete-record-updates \
    -Wcompat \
    -Widentities \
    -Wredundant-constraints \
    -Wunused-packages \
    -Wpartial-fields \
    -iother -isrc -itest test/Main.hs"

# run tests
runhaskell -Wall -isrc -itest test/Main.hs
find . | grep "\.hs" | entr sh -c 'clear && date && runhaskell -Wall -isrc -itest test/Main.hs'
cabal bench
```

## Running examples from README.md file

The `README.md` file contain examples, which can be extracted and tested.
Unfortunately the `entangled` tool is not yet part of `nix` (see:
<https://github.com/entangled/entangled.py/issues/64>), so the process to
install the tool and check the examples is currently manual:

```bash
sudo apt install python3-venv           # install virtual env under ubuntu
python3 -m venv env                     # create new virtual environment
source env/bin/activate                 # and activate it
pip install entangled_cli               # install entangled to that environment
entangled --version
entangled tangle                        # extract code snippets from README.md
./readme-samples/run-samples.sh         # run all samples

# or monitor README.md file and re-run samples automatically
echo README.md | entr sh -c \
    'clear && date && entangled tangle && ./readme-samples/run-samples.sh'
```

