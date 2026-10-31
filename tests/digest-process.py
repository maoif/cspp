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

    config = (ROOT / 'src/cspp/c/build-config.h').read_text()
    algorithms = [name for macro, name in [('OPENSSL', 'sha256'), ('BLAKE3', 'blake3')]
                  if f'#define CSPP_WITH_{macro} 1' in config]
    if algorithms and Path('/proc/self/fd').is_dir():
        fixture = directory / 'input.bin'
        fixture.write_bytes(bytes(range(256)) * 32)
        script = directory / 'file-ownership.ss'
        script.write_text('''(import (chezscheme) (cspp crypto digest))
(define descriptor-count (lambda () (length (directory-list "/proc/self/fd"))))
(let* ([arguments (command-line-arguments)]
       [path (car arguments)]
       [algorithm (string->symbol (cadr arguments))]
       [expected (digest-bytevector algorithm
                   (call-with-port (open-file-input-port path) get-bytevector-all))]
       [baseline (descriptor-count)])
  (for-each
    (lambda (hash)
      ;; Error case: abandon suspension during acquisition, reading, or cleanup.
      ;; File ownership must not depend on resuming the expired engine or on GC.
      (do ([ticks 1 (fx1+ ticks)]) [(fx= ticks 257)]
        ((make-engine hash) ticks (lambda results (void)) (lambda (abandoned) (void))))
      (unless (= baseline (descriptor-count))
        (error 'digest-file-ownership "abandoned digest leaked file descriptors"))
      (unless (equal? expected (hash))
        (error 'digest-file-ownership "normal file digest changed")))
    (list (lambda () (digest-file algorithm path))
          (lambda () (digest-file algorithm path 0))
          (lambda () (digest-file algorithm path 0 8192)))))
''')
        for algorithm in algorithms:
            run_script(script, str(fixture), algorithm)
