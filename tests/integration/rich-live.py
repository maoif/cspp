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
                  (milisleep 1))])
  (rich-live-start! live)
  (let wait ([remaining 1000])
    (when (and (< phase 4) (positive? remaining))
      (milisleep 1) (wait (- remaining 1))))
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


STOP = r'''
(import (cspp))
(define output (open-output-string))
(define live (make-rich-live "x" (rich-console :output-port output) 1/10 #f))
(rich-live-start! live)
((rich-sleep) 0.05)
(define before (current-time 'time-monotonic))
(rich-live-stop! live)
(define duration (time-difference (current-time 'time-monotonic) before))
(close-port output)
(unless (< (+ (time-second duration) (/ (time-nanosecond duration) 1e9)) 0.5)
  (error 'live-stop "shutdown waited for the refresh interval"))
'''


STATUS = r'''
(import (cspp))
(define first (open-output-string))
(define second (open-output-string))
(define status (make-rich-status "Working" (rich-console :output-port first) '("*") 10))
(rich-status-start! status)
((rich-sleep) 0.02)
(rich-status-console-set! status (rich-console :output-port second))
(rich-status-interval-set! status 0.005)
((rich-sleep) 0.05)
(rich-status-stop! status)
(define result (get-output-string second))
(close-port first)
(close-port second)
(unless (string-contains? result "* Working")
  (error 'status-setters "active status ignored its new console or interval"))
'''



SELF_STOP = r'''
(import (cspp))
(define calls 0)
(define stopped? #f)
(define value (vector 'self-stop))
(define live #f)
(rich-register-renderer! (lambda (candidate) (eq? candidate value))
  (lambda (value)
    (set! calls (+ calls 1))
    ;; Stopping from a worker renderer must not join the current thread.
    (when (= calls 2) (rich-live-stop! live) (set! stopped? #t))
    "ok"))
(call-with-port (open-output-string)
  (lambda (output)
    (set! live (make-rich-live value (rich-console :output-port output) 100 #t))
    (rich-live-start! live)
    (let wait ([remaining 1000])
      (when (and (not stopped?) (positive? remaining))
        (milisleep 1) (wait (- remaining 1))))
    (rich-live-stop! live)
    (unless (and stopped? (= calls 2)) (error 'self-stop "worker did not stop"))
    ;; Its final frame must be cleaned up and the same live display must restart.
    (unless (string-contains? (get-output-string output) "\033[2K")
      (error 'self-stop "transient cleanup was skipped"))
    (rich-live-refresh-rate-set! live 0)
    (rich-live-start! live)
    (rich-live-stop! live)
    (unless (= calls 3) (error 'self-stop "display did not restart"))))
'''


# A renderer may delegate shutdown to a child and wait for that child to return.
CHILD_STOP = SELF_STOP.replace(
    '(rich-live-stop! live) (set! stopped? #t)',
    '(thread-join (fork-thread (lambda () (rich-live-stop! live)))) (set! stopped? #t)')


CLEANUP_CHILD_STOP = r'''
(import (cspp))
(define calls 0)
(define writes 0)
(define cleanup-stopped? #f)
(define live #f)
(define value (vector 'cleanup-child-stop))
(rich-register-renderer! (lambda (candidate) (eq? candidate value))
  (lambda (value)
    (set! calls (+ calls 1))
    (when (= calls 2) (rich-live-stop! live))
    "ok"))
(call-with-port
  (make-custom-textual-output-port "cleanup-child-stop"
    (lambda (text start count)
      (set! writes (+ writes 1))
      ;; The third flushed write clears the final frame after the worker stops itself.
      ;; Cleanup callbacks and their children must still recognize the active worker.
      (when (= writes 3)
        (thread-join (fork-thread (lambda () (rich-live-stop! live))))
        (rich-live-stop! live)
        (set! cleanup-stopped? #t))
      count)
    #f #f #f)
  (lambda (output)
    (set! live (make-rich-live value (rich-console :output-port output) 100 #t))
    (rich-live-start! live)
    (let wait ([remaining 1000])
      (when (and (not cleanup-stopped?) (positive? remaining))
        (milisleep 1) (wait (- remaining 1))))
    (rich-live-stop! live)
    (unless (and cleanup-stopped? (= calls 2) (= writes 3))
      (error 'cleanup-child-stop "cleanup callback did not stop safely"))))
'''


STALE_CHILD_STOP = r'''
(import (cspp))
(define calls 0)
(define child #f)
(define live #f)
(define lock (make-mutex))
(define ready (make-condition))
(define released? #f)
(define value (vector 'stale-child-stop))
(rich-register-renderer! (lambda (candidate) (eq? candidate value))
  (lambda (value)
    (set! calls (+ calls 1))
    (when (= calls 2)
      (set! child
        (fork-thread
          (lambda ()
            (with-mutex lock
              (let wait ()
                (unless released? (condition-wait ready lock) (wait))))
            (rich-live-stop! live))))
      (rich-live-stop! live))
    "ok"))
(call-with-port (open-output-string)
  (lambda (output)
    (set! live (make-rich-live value (rich-console :output-port output) 100 #t))
    (rich-live-start! live)
    (let wait ([remaining 1000])
      (when (and (not child) (positive? remaining))
        (milisleep 1) (wait (- remaining 1))))
    (unless child (error 'stale-child-stop "worker did not render"))
    (rich-live-stop! live)
    (get-output-string output)
    ;; The child retains its parameter context after its worker has exited.
    ;; It must stop a subsequent display synchronously and perform its cleanup.
    (rich-live-refresh-rate-set! live 0)
    (rich-live-start! live)
    (with-mutex lock (set! released? #t) (condition-signal ready))
    (thread-join child)
    (unless (string-contains? (get-output-string output) "\033[2K")
      (error 'stale-child-stop "expired worker context deferred shutdown"))
    (rich-live-stop! live)))
'''



STATUS_RECOVERY = r'''
(import (cspp))
(define calls 0)
(define fail? #t)
(define status #f)
;; A later worker failure must be reported once without preventing a status restart.
(rich-register-renderer! (lambda (candidate) (eq? candidate status))
  (lambda (value)
    (set! calls (+ calls 1))
    (if (and fail? (> calls 1)) (raise 'status-worker-failure) "ok")))
(call-with-port (open-output-string)
  (lambda (output)
    (set! status (make-rich-status "Working" (rich-console :output-port output) '("*") 0.005))
    (rich-status-start! status)
    (let wait ([remaining 1000])
      (when (and (< calls 2) (positive? remaining))
        (milisleep 1) (wait (- remaining 1))))
    (let ([failure (guard (condition [else condition]) (rich-status-stop! status) #f)])
      (unless (eq? failure 'status-worker-failure)
        (error 'status-recovery "worker failure was not reported")))
    (set! fail? #f)
    (rich-status-start! status)
    (rich-status-stop! status)
    (unless (>= calls 3) (error 'status-recovery "status did not restart"))))
'''



EXTREME_TIMING = r'''
(import (cspp))
(call-with-port (open-output-string)
  (lambda (output)
    (let ([console (rich-console :output-port output)])
      ;; These finite rates formerly overflowed the reciprocal or millisecond scaling.
      (for-each
       (lambda (rate)
         (let ([live (make-rich-live "ok" console rate #f)])
           (rich-live-start! live)
           (milisleep 10)
           (rich-live-stop! live)))
       '(1e-320 1e-308 1e308))
      (let ([status (make-rich-status "test" console '("A") 0.01)])
        (rich-status-start! status)
        (rich-status-interval-set! status 1e-320)
        (milisleep 10)
        (rich-status-stop! status)))))
'''


def main():
    with tempfile.TemporaryDirectory(prefix="cspp-rich-live-") as directory:
        cases = (("pause", PAUSE), ("recovery", RECOVERY), ("failure", FAILURE),
                 ("false-failure", FAILURE.replace(
                     '(error \'worker-recovery "later frame failed")', "(raise #f)")),
                 ("stop", STOP), ("status", STATUS), ("self-stop", SELF_STOP),
                 ("child-stop", CHILD_STOP), ("cleanup-child-stop", CLEANUP_CHILD_STOP),
                 ("stale-child-stop", STALE_CHILD_STOP),
                 ("status-recovery", STATUS_RECOVERY),
                 ("extreme-timing", EXTREME_TIMING))
        for name, source in cases:
            probe = Path(directory) / (name + ".ss")
            probe.write_text(source)
            result = subprocess.run([str(ROOT / "cspp"), "--script=" + str(probe)],
                                    cwd=ROOT, capture_output=True, timeout=5)
            assert result.returncode == 0 and not result.stdout and not result.stderr, result


if __name__ == "__main__":
    main()
