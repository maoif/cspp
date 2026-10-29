#!/usr/bin/env -S python3 -I
"""Exercise digest failure paths that need isolated native state or a timeout."""
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent


def run_script(path, *arguments, env=None):
    result = subprocess.run([str(ROOT / 'cspp'), f'--script={path}', *arguments],
                            cwd=ROOT, env=env, capture_output=True, text=True, timeout=15)
    assert result.returncode == 0 and not result.stdout and not result.stderr, (
        result.returncode, result.stdout, result.stderr)


with tempfile.TemporaryDirectory(prefix='cspp-digest-') as name:
    directory = Path(name)
    fifo = directory / 'input.fifo'
    os.mkfifo(fifo)
    script = directory / 'fifo.ss'
    script.write_text('''(import (chezscheme) (cspp crypto digest))
(let* ([arguments (command-line-arguments)]
       [path (car arguments)]
       [mode (cadr arguments)])
  ;; Error case: a nonregular path must be rejected before opening it to query its size.
  (unless (guard (c [else (and (who-condition? c) (eq? (condition-who c) 'pcheck))])
            (cond
              [(string=? mode "whole") (digest-file 'sha256 path)]
              [(string=? mode "slice") (digest-file 'sha256 path 0)]
              [else (sha256-file path)])
            #f)
    (error 'digest-fifo "nonregular input was not rejected")))
''')
    for mode in ['whole', 'slice', 'specialized']:
        run_script(script, str(fifo), mode)

    if '#define CSPP_WITH_OPENSSL 1' in (ROOT / 'src/cspp/c/build-config.h').read_text():
        config = directory / 'openssl.cnf'
        config.write_text('''openssl_conf = init
[init]
alg_section = properties
[properties]
default_properties = fips=yes
''')
        script = directory / 'provider.ss'
        script.write_text('''(import (chezscheme) (cspp crypto digest))
(define expect-provider-error
  (lambda (who thunk)
    ;; Error case: unavailable provider algorithms must raise attributed Scheme errors.
    (unless (guard (c [else (and (who-condition? c) (eq? who (condition-who c)))])
              (thunk)
              #f)
      (error 'digest-provider "expected provider failure" who))))
(expect-provider-error 'digest-bytevector (lambda () (digest-bytevector 'sha256 #vu8(1))))
(expect-provider-error 'digest-string (lambda () (digest-string 'sha256 "a")))
(expect-provider-error 'make-digester (lambda () (make-digester 'sha256)))
(expect-provider-error 'digest-output-size (lambda () (digest-output-size 'sha256)))
(expect-provider-error 'digest-block-size (lambda () (digest-block-size 'sha256)))
''')
        env = os.environ.copy()
        env['OPENSSL_CONF'] = str(config)
        run_script(script, env=env)
