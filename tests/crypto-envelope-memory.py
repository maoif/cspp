#!/usr/bin/env -S python3 -I
"""Check native envelope ownership with OpenSSL's real memory allocation hooks."""
import os
from pathlib import Path
import shlex
import subprocess

ROOT = Path(__file__).resolve().parent.parent

if '#define CSPP_WITH_OPENSSL 1' in (ROOT / 'src/cspp/c/build-config.h').read_text():
    interpreter = shlex.split((ROOT / 'cspp').read_text().splitlines()[0][2:])
    env = os.environ.copy()
    env['LIBCSPP'] = str(ROOT / 'libcspp.so')
    result = subprocess.run(
        [*interpreter, str(ROOT / 'tests/crypto-envelope-memory.ss'),
         str(ROOT / 'tests/crypto-memory-fixture.so')],
        cwd=ROOT, env=env, capture_output=True, text=True, timeout=30)
    assert result.returncode == 0 and not result.stdout and not result.stderr, (
        result.returncode, result.stdout, result.stderr)
