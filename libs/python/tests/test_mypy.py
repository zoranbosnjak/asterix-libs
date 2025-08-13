# test broken script with mypy, it shall detect errors

from typing import *
import os
import subprocess

import pytest

def run_shell(cmd: str) -> Any:
    """Run shell command and capture output/error."""
    return subprocess.run(cmd, shell=True, capture_output=True)

def test_script1() -> None:
    mydir = os.path.join(os.path.dirname(__file__))
    script = os.path.join(mydir, '..', 'samples', 'sample-invalid.py')
    result = run_shell('mypy ' + script)
    assert result.returncode != 0, "mypy should fail"
    s = result.stdout.decode() + result.stderr.decode()
    ix = s.find('Found 3 errors')
    assert ix >= 0, "expected errors not found"

