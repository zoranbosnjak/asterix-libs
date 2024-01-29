# Asterix data processing library

## Devel

```bash
nix-shell

# prettify python source code
autopep8 --in-place --aggressive --aggressive <filename> <filename>...

# run static code check once
mypy

# monitor changes in .py files, check with 'mypy' on any change
find . | grep "\.py" | entr sh -c 'clear && mypy && date'

# run tests
pytest
```
