(import (cspp))

(define previous (timer-interrupt-handler))
(define sentinel (list 'process-mutex-interrupt))
(define escape #f)
(define interrupt-kind 'exception)
(define interruptions 0)
(define child (process-spawn "/bin/true" '() '()))
(define expected (process-wait child))

(define check-status-on-another-thread
  (lambda (operation ticks)
    (let ([gate (make-mutex)] [ready (make-condition)] [done? #f] [failure #f])
      (fork-thread
        (lambda ()
          (guard (condition [else (set! failure condition)])
            (unless (eq? expected (process-status child))
              (error 'process-mutex "cached status changed")))
          (with-mutex gate (set! done? #t) (condition-signal ready))))
      (with-mutex gate
        (let loop ()
          (unless done?
            (unless (condition-wait ready gate (make-time 'time-duration 0 5))
              (unless done?
                (error 'process-mutex "another thread cannot read status"
                       interrupt-kind operation ticks)))
            (loop))))
      (when failure (raise failure)))))

(dynamic-wind #t
  (lambda ()
    (timer-interrupt-handler
      (lambda ()
        (set! interruptions (+ interruptions 1))
        (if (eq? interrupt-kind 'exception) (raise sentinel) (escape sentinel)))))
  (lambda ()
    (for-each
      (lambda (kind)
        (set! interrupt-kind kind)
        (for-each
          (lambda (operation)
            (do ([ticks 1 (+ ticks 1)]) ((> ticks 64))
              ;; Error case: escaping mutex entry or exit must leave other threads able to read.
              (guard (failure [(eq? failure sentinel) (void)])
                (call/1cc
                  (lambda (leave)
                    (set! escape leave)
                    (dynamic-wind #t
                      (lambda () (set-timer ticks))
                      (lambda () ((cdr operation) child))
                      (lambda () (set-timer 0))))))
              (check-status-on-another-thread (car operation) ticks)))
          (list (cons 'status process-status)
                (cons 'no-hang process-wait/no-hang)
                (cons 'kill (lambda (process) (process-kill process 15)))
                (cons 'wait process-wait)
                (cons 'wait-timeout (lambda (process) (process-wait/timeout process 100))))))
      '(exception continuation))
    (unless (> interruptions 0) (error 'process-mutex "interrupt sweep did not run a handler")))
  (lambda () (set-timer 0) (timer-interrupt-handler previous)))
