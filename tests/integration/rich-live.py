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


INITIAL_STOP = r'''
(import (cspp))
(for-each
 (lambda (rate)
   (for-each
    (lambda (child?)
      (for-each
       (lambda (transient?)
         (let ([calls 0] [value (vector 'initial-stop)] [live #f])
           (rich-register-renderer! (lambda (candidate) (eq? candidate value))
             (lambda (value)
               (set! calls (+ calls 1))
               (if child?
                   (thread-join (fork-thread (lambda () (rich-live-stop! live))))
                   (rich-live-stop! live))
               "ok"))
           (call-with-port (open-output-string)
             (lambda (output)
               (set! live (make-rich-live value (rich-console :output-port output)
                                          rate transient?))
               (rich-live-start! live)
               (unless (string=? (if transient? "ok\r\033[2K" "ok")
                                  (get-output-string output))
                 (error 'initial-stop "initial frame was not cleaned up"))
               (rich-live-stop! live)
               (unless (string=? "" (get-output-string output))
                 (error 'initial-stop "second stop repeated cleanup"))
               (rich-live-start! live)
               (unless (= calls 2) (error 'initial-stop "display did not restart"))
               (rich-live-stop! live)))))
       '(#f #t)))
    '(#f #t)))
 '(0 100))
'''


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


CONCURRENT_CAPTURE = r'''
(import (cspp))
(define lock (make-mutex))
(define signal (make-condition))
(define entered (vector #f #f))
(define released (vector #f #f))
(define a (vector 'a))
(define b (vector 'b))
(define enter-and-wait
 (lambda (index)
   (with-mutex lock
     (vector-set! entered index #t)
     (condition-broadcast signal)
     (let wait ()
       (unless (vector-ref released index) (condition-wait signal lock) (wait))))))
(define wait-entered
 (lambda (index)
   (with-mutex lock
     (let wait ()
       (unless (vector-ref entered index) (condition-wait signal lock) (wait))))))
(define release
 (lambda (index)
   (with-mutex lock (vector-set! released index #t) (condition-broadcast signal))))
(rich-register-renderer! (lambda (v) (eq? v a)) (lambda (v) (enter-and-wait 0) "aaaa"))
(rich-register-renderer! (lambda (v) (eq? v b)) (lambda (v) (enter-and-wait 1) "bbbb"))
(call-with-port (open-output-string)
 (lambda (first)
   (call-with-port (open-output-string)
    (lambda (second)
      (let ([ta (fork-thread (lambda () (rich-print (rich-console :width 2 :output-port first) a)))])
        (wait-entered 0)
        (let ([tb (fork-thread (lambda () (rich-print (rich-console :width 2 :output-port second) b)))])
          (wait-entered 1)
          (release 0)
          (thread-join ta)
          (release 1)
          (thread-join tb)
          (unless (equal? (list (get-output-string first) (get-output-string second))
                          '("aa\naa" "bb\nbb"))
            (error 'concurrent-capture "hard-wrap capture crossed threads"))
          (rich-fprint first "AFTER")
          (unless (string=? (get-output-string first) "AFTER")
            (error 'concurrent-capture "stale capture swallowed later output"))))))))
'''

CONCURRENT_WIDTH = r'''
(import (cspp))
(define lock (make-mutex))
(define change (make-condition))
(define entered? #f)
(define release? #f)
(define done? #f)
(define a (vector 'a))
(define b (vector 'b))
(rich-register-renderer! (lambda (x) (eq? x a))
 (lambda (_)
  (with-mutex lock
   (set! entered? #t) (condition-broadcast change)
   (let wait () (unless release? (condition-wait change lock) (wait))))
  (rich-rule-render (make-rich-rule))))
(rich-register-renderer! (lambda (x) (eq? x b))
 (lambda (_)
  (with-mutex lock
   (set! release? #t) (condition-broadcast change)
   (let wait () (unless done? (condition-wait change lock) (wait))))
  (rich-rule-render (make-rich-rule))))
(call-with-port (open-output-string)
 (lambda (first)
  (call-with-port (open-output-string)
   (lambda (second)
    (let ([worker (fork-thread (lambda ()
      (rich-print (rich-console :width 7 :output-port first :soft-wrap? #t) a)
      (with-mutex lock (set! done? #t) (condition-broadcast change))))])
     (with-mutex lock (let wait () (unless entered? (condition-wait change lock) (wait))))
     (rich-print (rich-console :width 19 :output-port second :soft-wrap? #t) b)
     (thread-join worker)
     (unless (equal? (list (get-output-string first) (get-output-string second))
                         '("-------" "-------------------"))
       (error 'concurrent-width "available widths crossed threads")))))))
'''

EXTERNAL_STOP = r'''
(import (cspp))
(define lock (make-mutex))
(define changed (make-condition))
(define entered? #f)
(define released? #f)
(define calls 0)
(define value (vector 'blocked-frame))
(rich-register-renderer! (lambda (x) (eq? x value))
  (lambda (_)
    (set! calls (+ calls 1))
    (when (= calls 2)
      (with-mutex lock
        (set! entered? #t) (condition-broadcast changed)
        (let wait () (unless released? (condition-wait changed lock) (wait)))))
    "frame"))
(call-with-port (open-output-string)
 (lambda (output)
   (let ([live (make-rich-live value (rich-console :output-port output) 100 #t)])
     (rich-live-start! live)
     (with-mutex lock
       (let wait () (unless entered? (condition-wait changed lock) (wait))))
     ;; A separate thread releases the blocked renderer. Main-thread stop must join it.
     (let ([releaser (fork-thread
                      (lambda ()
                        (milisleep 30)
                        (with-mutex lock
                          (set! released? #t) (condition-broadcast changed))))])
       (rich-live-stop! live)
       (let ([waited? released?])
         (thread-join releaser)
         (unless waited?
           (error 'external-stop "stop returned before the worker finished")))))))
'''

MANUAL_REFRESH_STOP = r'''
(import (cspp))
(let ([live #f] [stop? #f] [value (vector 'manual-stop)])
  (rich-register-renderer! (lambda (candidate) (eq? candidate value))
    (lambda (_)
      (when stop?
        ;; Manual refresh must defer both direct and renderer-child shutdown.
        (rich-live-stop! live)
        (thread-join (fork-thread (lambda () (rich-live-stop! live)))))
      "frame"))
  (call-with-port (open-output-string)
    (lambda (output)
      (set! live (make-rich-live value (rich-console :output-port output) 0 #t))
      (rich-live-start! live)
      (get-output-string output)
      (set! stop? #t)
      (rich-live-refresh! live)
      (unless (string=? (get-output-string output) "\r\033[2Kframe\r\033[2K")
        (error 'manual-refresh-stop "new frame remained visible after shutdown"))
      (rich-live-stop! live)
      (unless (string=? (get-output-string output) "")
        (error 'manual-refresh-stop "shutdown cleanup was left pending")))))
'''

STATUS_INITIAL_STOP = r'''
(import (cspp))
(for-each
 (lambda (child?)
   (let ([status #f] [calls 0])
     (rich-register-renderer! (lambda (candidate) (eq? candidate status))
       (lambda (_)
         (set! calls (+ calls 1))
         (if child?
             (thread-join (fork-thread (lambda () (rich-status-stop! status))))
             (rich-status-stop! status))
         "ok"))
     (call-with-port (open-output-string)
       (lambda (output)
         (set! status (make-rich-status "working" (rich-console :output-port output)
                                        '("*") 10000))
         (rich-status-start! status)
         (rich-status-start! status)
         (rich-status-stop! status)
         (unless (= calls 2)
           (error 'status-initial-stop "initial renderer could not stop and restart status"))))))
 '(#f #t))
'''

STATUS_INITIAL_SETTERS = r'''
(import (cspp))
(let ([status #f] [calls 0] [observed #f])
  (call-with-port (open-output-string)
    (lambda (first)
      (call-with-port (open-output-string)
        (lambda (second)
          (rich-register-renderer! (lambda (candidate) (eq? candidate status))
            (lambda (_)
              (set! calls (+ calls 1))
              (if (= calls 1)
                  (begin
                    (rich-status-console-set! status (rich-console :output-port second))
                    (rich-status-interval-set! status 0.125))
                  (rich-status-stop! status))
              "frame"))
          (set! status (make-rich-status "working" (rich-console :output-port first)
                                         '("*") 10000))
          (parameterize ([rich-sleep (lambda (seconds) (set! observed seconds))])
            (rich-status-start! status)
            (let wait ([remaining 1000])
              (when (and (< calls 2) (positive? remaining))
                (milisleep 1) (wait (- remaining 1))))
            (rich-status-stop! status))
          (unless (and (= calls 2) (real? observed) (= observed 0.125)
                       (string=? (get-output-string first) "frame")
                       (string=? (get-output-string second) "frame"))
            (error 'status-initial-setters "initial renderer setters did not reach live display")))))))
'''

EXTERNAL_MANUAL_STOP = r'''
(import (cspp))
(define lock (make-mutex))
(define changed (make-condition))
(define entered? #f)
(define released? #f)
(define calls 0)
(define value (vector 'manual-external-stop))
(rich-register-renderer! (lambda (candidate) (eq? value candidate))
  (lambda (_)
    (set! calls (+ calls 1))
    (when (= calls 2)
      (with-mutex lock
        (set! entered? #t)
        (condition-broadcast changed)
        (let wait ()
          (unless released? (condition-wait changed lock) (wait)))))
    "frame"))
(call-with-port (open-output-string)
  (lambda (output)
    (let ([live (make-rich-live value (rich-console :output-port output) 0 #t)])
      (rich-live-start! live)
      (get-output-string output)
      (let ([refresh (fork-thread (lambda () (rich-live-refresh! live)))])
        (with-mutex lock
          (let wait () (unless entered? (condition-wait changed lock) (wait))))
        (let ([release (fork-thread
                        (lambda ()
                          (milisleep 30)
                          (with-mutex lock
                            (set! released? #t) (condition-broadcast changed))))])
          (rich-live-stop! live)
          (let ([waited? released?])
          (thread-join release)
          (thread-join refresh)
          (unless waited?
            (error 'manual-external-stop "stop returned before manual refresh finished"))
          (unless (string=? (get-output-string output) "\r\033[2Kframe\r\033[2K")
            (error 'manual-external-stop "transient frame remained visible")))
          (rich-live-stop! live))))))
'''


# Initial rendering has no published worker either; shutdown must await that frame too.
EXTERNAL_INITIAL_STOP = EXTERNAL_MANUAL_STOP.replace(
    '(= calls 2)', '(= calls 1)').replace(
    '      (rich-live-start! live)\n      (get-output-string output)\n', '').replace(
    '(lambda () (rich-live-refresh! live))', '(lambda () (rich-live-start! live))').replace(
    r'"\r\033[2Kframe\r\033[2K"', r'"frame\r\033[2K"')

# An exceptional manual frame must also release shutdown waiters.
EXTERNAL_MANUAL_FAILURE = EXTERNAL_MANUAL_STOP.replace(
    '    "frame"))', '(if (= calls 2) (raise \'manual-frame-failure) "frame")))').replace(
    '(lambda () (rich-live-refresh! live))',
    "(lambda () (guard (condition [(eq? condition 'manual-frame-failure) (void)]) "
    "(rich-live-refresh! live)))").replace(
    r'"\r\033[2Kframe\r\033[2K"', r'"\r\033[2K"')

NESTED_STOP = r'''
(import (cspp))
(define outer #f)
(define inner #f)
(define calls 0)
(define completed? #f)
(define lock (make-mutex))
(define ready (make-condition))
(define stopping? #f)
(define first (vector 'outer))
(define second (vector 'inner))
(rich-register-renderer! (lambda (v) (eq? v first))
  (lambda (_)
    (set! calls (+ calls 1))
    (when (= calls 2)
      (rich-live-refresh! inner)
      (set! completed? #t))
    "outer"))
(rich-register-renderer! (lambda (v) (eq? v second))
  (lambda (_)
    (thread-join (fork-thread
      (lambda ()
        (with-mutex lock (set! stopping? #t) (condition-signal ready))
        (rich-live-stop! outer))))
    "inner"))
(call-with-port (open-output-string)
  (lambda (out)
    (call-with-port (open-output-string)
      (lambda (in)
        (set! outer (make-rich-live first (rich-console :output-port out) 100 #t))
        (set! inner (make-rich-live second (rich-console :output-port in) 0 #t))
        (rich-live-start! outer)
        (with-mutex lock
          (let wait () (unless stopping? (condition-wait ready lock) (wait))))
        (rich-live-stop! outer)
        (unless completed? (error 'nested-stop "nested refresh did not finish"))
        (unless (string=? (get-output-string out) "outer\r\033[2Kouter\r\033[2K")
          (error 'nested-stop "outer frame was not cleared"))
        (rich-live-stop! inner)))))
'''

NESTED_INITIAL_STOP = NESTED_STOP.replace(
    '(= calls 2)', '(= calls 1)').replace(
    ':output-port out) 100 #t', ':output-port out) 0 #t').replace(
    r'"outer\r\033[2Kouter\r\033[2K"', r'"outer\r\033[2K"')

FAILED_CLEANUP_CONTEXT = r'''
(import (cspp))
(define lock (make-mutex))
(define ready (make-condition))
(define released? #f)
(define child #f)
(define live #f)
(define writes 0)
(define calls 0)
(define value (vector 'failed-cleanup))
(rich-register-renderer! (lambda (candidate) (eq? candidate value))
  (lambda (_)
    (set! calls (+ calls 1))
    (when (= calls 2)
      (set! child
        (fork-thread
          (lambda ()
            (with-mutex lock
              (let wait () (unless released? (condition-wait ready lock) (wait))))
            (rich-live-stop! live))))
      (rich-live-stop! live))
    "ok"))
(call-with-port
  (make-custom-textual-output-port "failed-cleanup"
    (lambda (text start count)
      (with-mutex lock
        (set! writes (+ writes 1))
        (condition-broadcast ready))
      ;; The final transient erase fails inside the worker's deferred-stop cleanup.
      (when (= writes 3) (raise 'cleanup-failure))
      count)
    #f #f #f)
  (lambda (output)
    (set! live (make-rich-live value (rich-console :output-port output) 100 #t))
    (rich-live-start! live)
    (with-mutex lock
      (let wait () (unless (>= writes 3) (condition-wait ready lock) (wait))))
    (let ([failure (guard (condition [else condition]) (rich-live-stop! live) #f)])
      (unless (eq? failure 'cleanup-failure)
        (error 'failed-cleanup "worker cleanup failure was not reported")))
    (call-with-port (open-output-string)
      (lambda (repaired)
        (rich-live-console-set! live (rich-console :output-port repaired))
        (rich-live-refresh-rate-set! live 0)
        (rich-live-start! live)
        (with-mutex lock (set! released? #t) (condition-broadcast ready))
        (thread-join child)
        (unless (string=? (get-output-string repaired) "ok\r\033[2K")
          (error 'failed-cleanup "stale child deferred shutdown after failed cleanup"))
        (rich-live-stop! live)
        (unless (string=? (get-output-string repaired) "")
          (error 'failed-cleanup "cleanup or failure repeated"))))))
'''

def main():
    with tempfile.TemporaryDirectory(prefix="cspp-rich-live-") as directory:
        cases = (
            ("pause", PAUSE),
            ("recovery", RECOVERY),
            ("failure", FAILURE),
            ("false-failure", FAILURE.replace(
                '(error \'worker-recovery "later frame failed")', "(raise #f)")),
            ("stop", STOP),
            ("status", STATUS),
            ("self-stop", SELF_STOP),
            ("initial-stop", INITIAL_STOP),
            ("child-stop", CHILD_STOP),
            ("cleanup-child-stop", CLEANUP_CHILD_STOP),
            ("stale-child-stop", STALE_CHILD_STOP),
            ("failed-cleanup-context", FAILED_CLEANUP_CONTEXT),
            ("status-recovery", STATUS_RECOVERY),
            ("status-initial-stop", STATUS_INITIAL_STOP),
            ("status-initial-setters", STATUS_INITIAL_SETTERS),
            ("manual-refresh-stop", MANUAL_REFRESH_STOP),
            ("concurrent-capture", CONCURRENT_CAPTURE),
            ("concurrent-width", CONCURRENT_WIDTH),
            ("external-stop", EXTERNAL_STOP),
            ("initial-external-stop", EXTERNAL_INITIAL_STOP),
            ("manual-external-failure", EXTERNAL_MANUAL_FAILURE),
            ("manual-external-stop", EXTERNAL_MANUAL_STOP),
            ("nested-stop", NESTED_STOP),
            ("nested-initial-stop", NESTED_INITIAL_STOP),
            ("extreme-timing", EXTREME_TIMING),
        )
        for name, source in cases:
            probe = Path(directory) / (name + ".ss")
            probe.write_text(source)
            result = subprocess.run([str(ROOT / "cspp"), "--script=" + str(probe)],
                                    cwd=ROOT, capture_output=True, timeout=5)
            assert result.returncode == 0 and not result.stdout and not result.stderr, result


if __name__ == "__main__":
    main()
