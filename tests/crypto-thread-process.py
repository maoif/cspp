#!/usr/bin/env -S python3 -I
"""Run shared crypto state regressions separately so native crashes are test failures."""
from pathlib import Path
import resource
import subprocess

ROOT = Path(__file__).resolve().parent.parent
resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
for mode in ['sha256', 'blake3', 'hmac', 'buffered', 'cipher']:
    result = subprocess.run(
        [str(ROOT / 'cspp'), '--script=' + str(ROOT / 'tests/crypto-thread-process.ss'), mode],
        cwd=ROOT, capture_output=True, text=True, timeout=30)
    assert result.returncode == 0 and not result.stdout and not result.stderr, (
        mode, result.returncode, result.stdout, result.stderr)
