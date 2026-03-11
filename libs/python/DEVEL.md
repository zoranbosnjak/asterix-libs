# Development environment

```bash
nix-shell

# prettify python source code
autopep8 --in-place --aggressive --aggressive <filename> <filename>...
find . | grep "\.py$" \
    | grep -v "generated" \
    | xargs autopep8 --in-place --aggressive --aggressive

# run test from command line (as a module or via pytest), for example
python -m tests.test_asterix
python -c 'import tests.test_asterix; tests.test_asterix.test_parse1()'
pytest -v tests/test_asterix.py::test_raises

# run static code check and tests once
mypy
pytest

# monitor changes in .py files, check automatically on any change
find . | grep "\.py" | entr sh -c 'clear && date && mypy && pytest'
```

## Manually publish/update project to pypi

**Note**:
Normally this is not necessary.
The process is already running from github action.

``` bash
nix-shell
# from clean repository
git status
python3 -m build
ls -l dist/*

# upload to testpypi
twine upload --repository testpypi dist/*

# upload to production pypi
twine upload dist/*
```

## Running examples from README.md file

The `README.md` file contain examples, which can be extracted to testfiles with
the `entangled` tool. To make sure that examples actually work, run:

```bash
nix-shell
entangled tangle                        # extract code snippets from README.md
./readme-samples/run-samples.sh         # run all samples

# or monitor README.md file and re-run samples automatically
echo README.md | entr sh -c \
    'clear && date && entangled tangle && ./readme-samples/run-samples.sh'
```

