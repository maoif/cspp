#!/usr/bin/env -S python3 -I
"""Run invalid crypto input cases without risking a crash in the test runner."""
from pathlib import Path
import platform
import resource
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent

# RISC-V CI needs twice the x86 watchdog budget.
TIMEOUT_SCALE = 2 if platform.machine() == "riscv64" else 1
resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
for mode in sys.argv[1:] or ['random-lengths', 'encrypted-pem', 'input-objects',
                           'public-contracts', 'hkdf-output-limits']:
    result = subprocess.run(
        [str(ROOT / 'cspp'), '--script=' + str(ROOT / 'tests/crypto-input-process.ss'), mode],
        cwd=ROOT, stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=15 * TIMEOUT_SCALE,
        start_new_session=True)
    assert result.returncode == 0 and not result.stdout and not result.stderr, (
        mode, result.returncode, result.stdout, result.stderr)
