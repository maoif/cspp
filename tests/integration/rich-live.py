"""Run live worker lifecycle regressions with a timeout to catch shutdown hangs."""
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]

PAUSE = r'''
(import (cspp))
(define phase 0)
(define calls 0)
(define paused? #f)
(define value (vector 'pause))
(rich-register-renderer! (lambda (candidate) (eq? candidate value))
  (lambda (value) (set! calls (+ calls 1)) "x"))
(define output (open-output-string))
(define live (make-rich-live value (rich-console :output-port output) 100 #f))
(parameterize ([rich-sleep
                (lambda (seconds)
                  (set! phase (+ phase 1))
                  (case phase
                    [(1) (rich-live-refresh-rate-set! live 0)]
                    [(2) (set! paused? (= calls 1))
                         (rich-live-refresh-rate-set! live 100)])
                  (sleep 1))])
  (rich-live-start! live)
  (let wait ([remaining 1000])
    (when (and (< phase 4) (positive? remaining))
      (sleep 1) (wait (- remaining 1))))
  (rich-live-stop! live))
(close-port output)
(unless (and paused? (>= phase 4) (> calls 1))
  (error 'live-pause "failed to pause and resume"))
'''


def main():
    with tempfile.TemporaryDirectory(prefix="cspp-rich-live-") as directory:
        probe = Path(directory) / "pause.ss"
        probe.write_text(PAUSE)
        result = subprocess.run([str(ROOT / "cspp"), "--script=" + str(probe)],
                                cwd=ROOT, capture_output=True, timeout=5)
        assert result.returncode == 0 and not result.stdout and not result.stderr, result


if __name__ == "__main__":
    main()
