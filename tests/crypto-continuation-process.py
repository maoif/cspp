#!/usr/bin/env -S python3 -I
"""Isolate engine suspension regressions so native crashes fail the test suite."""
from pathlib import Path
import platform
import resource
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent

# RISC-V CI needs twice the x86 watchdog budget.
TIMEOUT_SCALE = 2 if platform.machine() == "riscv64" else 1
resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
for mode in sys.argv[1:] or ['digests', 'mac-cipher', 'certificates', 'callbacks',
                           'error-continuations', 'constructors']:
    result = subprocess.run(
        [str(ROOT / 'cspp'), '--script=' + str(ROOT / 'tests/crypto-continuation-process.ss'), mode],
        cwd=ROOT, capture_output=True, text=True, timeout=30 * TIMEOUT_SCALE)
    assert result.returncode == 0 and not result.stdout and not result.stderr, (
        mode, result.returncode, result.stdout, result.stderr)
