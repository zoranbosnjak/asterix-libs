# Development environment

```bash
nix-shell

# prettify python source code
autopep8 --in-place --aggressive --aggressive <filename> <filename>...

# run test from command line (as a module or via pytest), for example
python -m tests.test_asterix
python -c 'import tests.test_asterix; tests.test_asterix.test_parse()'
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

The `README.md` file contain python examples, which can be extracted to
regular python scripts.
Unfortunately the `entangled` tool is not yet part of `nix` (see:
<https://github.com/entangled/entangled.py/issues/64>), so the process to
install the tool and check the examples is currently manual:

```bash
sudo apt install python3-venv           # install virtual env under ubuntu
python3 -m venv env                     # create new virtual environment
source env/bin/activate                 # and activate it
pip install entangled_cli               # install entangled to that environment
entangled --version
entangled tangle                        # convert README.md to readme.py
# run all examples
for i in $(ls *py); do
    echo "---"
    echo "running: $i"
    nix-shell --command "python $i"
    if [ $? -ne 0 ]; then
        echo ""
        echo "Failure in $i!"
        echo ""
        break
    fi
done
```

