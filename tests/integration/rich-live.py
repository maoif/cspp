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


RECOVERY = r'''
(import (cspp))
(define calls 0)
(define fail? #t)
(define value (vector 'worker-recovery))
(rich-register-renderer! (lambda (candidate) (eq? candidate value))
  (lambda (value)
    (set! calls (+ calls 1))
    (if (and fail? (> calls 1)) (error 'worker-recovery "later frame failed") "ok")))
(define output (open-output-string))
(define live (make-rich-live value (rich-console :output-port output) 100 #f))
(rich-live-start! live)
(let wait ([remaining 1000])
  (when (and (< calls 2) (positive? remaining))
    ((rich-sleep) 0.001) (wait (- remaining 1))))
;; Give the worker's failure handler time to finish before restarting it.
((rich-sleep) 0.02)
(set! fail? #f)
(rich-live-start! live)
(let wait ([remaining 1000])
  (when (and (< calls 3) (positive? remaining))
    ((rich-sleep) 0.001) (wait (- remaining 1))))
(rich-live-stop! live)
(close-port output)
(unless (>= calls 3) (error 'worker-recovery "failed worker did not restart"))
'''


FAILURE = RECOVERY[:RECOVERY.index("(set! fail? #f)")] + r'''
(unless (guard (condition [else #t]) (rich-live-stop! live) #f)
  (error 'worker-failure "worker condition was not reported"))
(rich-live-stop! live)
(close-port output)
'''


def main():
    with tempfile.TemporaryDirectory(prefix="cspp-rich-live-") as directory:
        for name, source in (("pause", PAUSE), ("recovery", RECOVERY), ("failure", FAILURE)):
            probe = Path(directory) / (name + ".ss")
            probe.write_text(source)
            result = subprocess.run([str(ROOT / "cspp"), "--script=" + str(probe)],
                                    cwd=ROOT, capture_output=True, timeout=5)
            assert result.returncode == 0 and not result.stdout and not result.stderr, result


if __name__ == "__main__":
    main()
