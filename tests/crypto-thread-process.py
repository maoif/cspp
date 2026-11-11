#!/usr/bin/env -S python3 -I
"""Run shared crypto state regressions separately so native crashes are test failures."""
from pathlib import Path
import platform
import resource
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent

# RISC-V CI needs twice the x86 watchdog budget.
TIMEOUT_SCALE = 2 if platform.machine() == "riscv64" else 1
resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
for mode in sys.argv[1:] or ['sha256', 'blake3', 'hmac', 'buffered', 'cipher', 'resources',
                           'state-lifetimes', 'resource-lifetimes', 'error-handlers']:
    # Four state types create over 600 workers; emulation can exceed 30 seconds.
    timeout = (90 if mode == 'state-lifetimes' else 30) * TIMEOUT_SCALE
    result = subprocess.run(
        [str(ROOT / 'cspp'), '--script=' + str(ROOT / 'tests/crypto-thread-process.ss'), mode],
        cwd=ROOT, capture_output=True, text=True, timeout=timeout)
    assert result.returncode == 0 and not result.stdout and not result.stderr, (
        mode, result.returncode, result.stdout, result.stderr)
