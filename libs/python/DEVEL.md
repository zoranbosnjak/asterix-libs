# Development environment

```bash
nix-shell

# prettify python source code
autopep8 --in-place --aggressive --aggressive <filename> <filename>...

# run test from command line (as a module), for example
python -m tests.test_asterix
python -c 'import tests.test_asterix; tests.test_asterix.test_parse()'

# run static code check and tests once
mypy
pytest

# monitor changes in .py files, check automatically on any change
find . | grep "\.py" | entr sh -c 'clear && date && mypy && pytest'
```
