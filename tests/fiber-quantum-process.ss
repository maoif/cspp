(import (cspp)
        (cspp concurrency)
        (cspp concurrency fiber))

;; Run in a separate process: a dispatch livelock cannot run a fiber watchdog.
(define quantum (string->number (car (command-line-arguments))))

(define-syntax error?
  (syntax-rules ()
    [(_ expression)
     (guard (condition [else #t]) expression #f)]))

(if (< quantum 100)
    (let ([entered? #f])
      ;; Negative case: unsupported budgets must fail before entering the init thunk.
      (let ([rejected? (error?
                        (run-fibers (lambda () (set! entered? #t) 42) 1 quantum))])
        (unless (and rejected? (not entered?))
          (error 'fiber-quantum-process "unsupported quantum was not rejected"))
        (display "rejected\n")))
    (begin
      (unless
          (and
           (= 42 (run-fibers (lambda () 42) 1 quantum))
           (for-all
            (lambda (workers)
              (run-fibers
               (lambda ()
                 (let ([messages (make-channel)] [ready (abox #f)])
                   (spawn-fiber
                    (lambda ()
                      (abox-set! ready #t)
                      (do ([index 0 (add1 index)]) ((= index 32))
                        (fiber-yield)
                        (channel-put! messages index))))
                   ;; This loop needs automatic preemption on the single worker.
                   ;; Clamping by disabling the timer must not satisfy this probe.
                   (let wait ([remaining 1000000])
                     (cond
                      [(unabox ready)
                       (let ([sum (do ([index 0 (add1 index)]
                                       [sum 0 (+ sum (channel-get messages))])
                                      ((= index 32) sum))])
                         (fiber-milisleep 1)
                         (= sum 496))]
                      [(zero? remaining) #f]
                      [else (wait (sub1 remaining))]))))
               workers quantum))
            '(1 2)))
        (error 'fiber-quantum-process "supported quantum failed to make progress"))
      (display "completed\n")))
