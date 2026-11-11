(import (cspp)
        (cspp concurrency fiber)
        (cspp net operation))

;; RISC-V CI needs twice the x86 subprocess watchdog budget.
(define fiber-test-process-timeout
  (if (memq (machine-type) '(rv64le trv64le)) 10000 5000))

;; Bounded adaptations of the older spawn/yield/channel cases from `fiber.ss.bak`.
;; That legacy file labels `loko-tests` as adapted from Loko, and the sieve and
;; merge-sort examples as Guile Fibers benchmarks. These cases retain that origin
;; while using CSPP APIs and finite result assertions.
;; Loko: https://scheme.fail/ and https://gitlab.com/akkuscm/loko
;; Guile Fibers: https://github.com/wingo/fibers (benchmarks)
(mat fiber-spawn-nested-completes
     (let ([done (make-channel)] [values (make-vector 6 #f)])
       (run-fibers
        (lambda ()
          (for-each
           (lambda (index)
             (spawn-fiber
              (lambda ()
                (vector-set! values index index)
                (channel-put! done index))))
           (iota 6))
          (let loop ([remaining 6])
            (if (zero? remaining)
                #t
                (begin (channel-get done) (loop (sub1 remaining)))))))
       (equal? '#(0 1 2 3 4 5) values)))

(mat fiber-yield-round-trips
     (let ([done (make-channel)] [rounds 64])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda ()
             (let loop ([index 0])
               (if (= index rounds)
                   (channel-put! done #t)
                   (begin (fiber-yield) (loop (add1 index)))))))
          (channel-get done)))
       #t))

(mat fiber-operation-aliases-wrap-and-select
     (equal? '((wrapped . message) selected)
             (run-fibers
              (lambda ()
                (list
                 (perform-operation
                  (wrap-operation (event-always 'message)
                                  (lambda (message) (cons 'wrapped message))))
                 (perform-operation
                  (choice-operation
                   (wrap-operation (event-always 'selected)
                                   (lambda (value) value))
                   (event-never))))))))

(mat fiber-channel-fan-in-and-fan-out
     (let ([channel (make-channel)] [out (make-channel)] [count 32])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda ()
             (let loop ([total 0] [remaining count])
               (if (zero? remaining)
                   (channel-put! out (= total (* count (sub1 count) 1/2)))
                   (loop (+ total (channel-get channel)) (sub1 remaining))))))
          (for-each
           (lambda (value)
             (spawn-fiber (lambda () (channel-put! channel value))))
           (iota count))
          (channel-get out)))))

(mat fiber-channel-ping-pong
     (let ([request (make-channel)] [reply (make-channel)] [rounds 128])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda ()
             (let loop ([index 0])
               (unless (= index rounds)
                 (channel-put! reply (channel-get request))
                 (loop (add1 index))))))
          (let loop ([index 0])
            (if (= index rounds)
                #t
                (begin
                  (channel-put! request index)
                  (unless (= index (channel-get reply))
                    (error 'fiber-channel-ping-pong "wrong message"))
                  (loop (add1 index)))))))
       #t))

(mat loko-tests
     (let ([messages (make-channel)] [wrapped (make-channel)] [choice (make-channel)])
       (run-fibers
        (lambda ()
          ;; Nested senders may rendezvous in either order, as in the original Loko case.
          (spawn-fiber
           (lambda ()
             (spawn-fiber (lambda () (channel-put! messages 'first)))
             (channel-put! messages 'second)))
          (let ([received (list (channel-get messages) (channel-get messages))])
            (unless (or (equal? received '(first second))
                        (equal? received '(second first)))
              (error 'loko-tests "unexpected nested sender order")))
          (spawn-fiber (lambda () (channel-put! wrapped 'message)))
          (unless (equal? '(wrapped . message)
                         (event-sync
                          (event-wrap (channel-get-event wrapped)
                                      (lambda (message) (cons 'wrapped message)))))
            (error 'loko-tests "wrapped receive mismatch"))
          (spawn-fiber
           (lambda ()
             (fiber-milisleep 1)
             (channel-put! choice 'choice)))
          (equal? 'choice
                  (event-sync
                   (event-select (channel-get-event choice)
                                 (event-wrap (milisleep-event 20)
                                             (lambda (_) 'timeout)))))))))

(mat sieve-of-Eratosthenes
     (letrec ([sieve
               (lambda (prime input)
                 (let ([output (make-channel)])
                   (spawn-fiber
                    (lambda ()
                      (let loop ()
                        (let ([value (channel-get input)])
                          (unless (zero? (modulo value prime))
                            (channel-put! output value))
                          (loop)))))
                   output))]
              [integers-from
               (lambda (start)
                 (let ([output (make-channel)])
                   (spawn-fiber
                    (lambda ()
                      (let loop ([value start])
                        (channel-put! output value)
                        (loop (add1 value)))))
                   output))]
              [take-primes
               (lambda (count)
                 (let loop ([input (integers-from 2)] [remaining count] [result '()])
                   (if (zero? remaining)
                       (reverse result)
                       (let ([prime (channel-get input)])
                         (loop (sieve prime input)
                               (sub1 remaining)
                               (cons prime result))))))])
       (equal? '(2 3 5 7 11 13 17 19 23 29
                 31 37 41 43 47 53 59 61 67 71
                 73 79 83 89 97)
               (run-fibers (lambda () (take-primes 25))))))

(mat merge-sort
     (letrec ([split
               (lambda (input left right)
                 (let loop ([value (channel-get input)] [left left] [right right])
                   (if (eq? value 'done)
                       (begin (channel-put! left 'done) (channel-put! right 'done))
                       (begin
                         (channel-put! left value)
                         (loop (channel-get input) right left)))))]
              [copy-channel
               (lambda (source output)
                 (let loop ([value (channel-get source)])
                   (unless (eq? value 'done)
                     (channel-put! output value)
                     (loop (channel-get source)))))]
              [merge-values
               (lambda (left right output)
                 (let loop ([left-value (channel-get left)]
                            [right-value (channel-get right)])
                   (cond
                    [(eq? left-value 'done)
                     (if (eq? right-value 'done)
                         (channel-put! output 'done)
                         (begin
                           (channel-put! output right-value)
                           (copy-channel right output)))]
                    [(eq? right-value 'done)
                     (channel-put! output left-value)
                     (copy-channel left output)]
                    [(< left-value right-value)
                     (channel-put! output left-value)
                     (loop (channel-get left) right-value)]
                    [else
                     (channel-put! output right-value)
                     (loop left-value (channel-get right))])))]
              [sort-helper
               (lambda ()
                 (let ([input (make-channel)])
                   (spawn-fiber
                    (lambda ()
                      (let ([first (channel-get input)])
                        (if (eq? first 'done)
                            (channel-put! input 'done)
                            (let ([second (channel-get input)])
                              (if (eq? second 'done)
                                  (begin (channel-put! input first)
                                         (channel-put! input 'done))
                                  (let ([left (sort-helper)] [right (sort-helper)])
                                    (channel-put! left first)
                                    (channel-put! right second)
                                    (split input left right)
                                    (merge-values left right input))))))
                      (channel-put! input 'done)))
                   input))])
       ;; Negative case: the channel pipeline must return the sorted values, not lose a tail.
       (equal? (sort < (reverse (iota 64)))
               (run-fibers
                (lambda ()
                  (let ([input (sort-helper)] [values (reverse (iota 64))])
                    (for-each (lambda (value) (channel-put! input value)) values)
                    (channel-put! input 'done)
                    (let loop ([value (channel-get input)] [result '()])
                      (if (eq? value 'done)
                          (reverse result)
                          (loop (channel-get input) (cons value result))))))))))

;; These bounds use the monotonic clock independently of the fiber timer implementation.
(define fiber-test-deadline
  (lambda (milliseconds)
    (add-duration (current-time 'time-monotonic)
                  (make-time 'time-duration
                             (* (modulo milliseconds 1000) 1000000)
                             (quotient milliseconds 1000)))))

(define fiber-test-await
  (lambda (ready? deadline)
    (let loop ()
      (cond
       [(ready?) #t]
       [(time>=? (current-time 'time-monotonic) deadline) #f]
       [else (fiber-yield) (loop)]))))

(define fiber-test-watchdog
  (lambda (deadline)
    (spawn-fiber
     (lambda ()
       ;; Negative case: a stranded channel or barrier must fail within the deadline.
       (fiber-test-await (lambda () #f) deadline)
       (error 'fiber-test "monotonic deadline exceeded")))))

;;;;===----------------------------------------------------------------------===
;;;; Public API usage examples
;;;;===----------------------------------------------------------------------===

(mat fiber-api-channel-operation-aliases
     (run-fibers
      (lambda ()
        (fiber-test-watchdog (fiber-test-deadline 5000))
        (let ([request (make-channel)] [reply (make-channel)]
              [payload (vector 'request 42)])
          (spawn-fiber
           (lambda ()
             (let ([message (get-message request)])
               (put-message reply (list 'reply message)))))
          ;; put-operation constructs an event; perform-operation performs the send.
          (perform-operation (put-operation request payload))
          (let ([response (perform-operation (get-operation reply))])
            (and (eq? 'reply (car response))
                 (eq? payload (cadr response))))))))

(mat fiber-api-wrapped-send-event
     (run-fibers
      (lambda ()
        (fiber-test-watchdog (fiber-test-deadline 5000))
        (let ([messages (make-channel)] [received (make-channel)] [wrap-count 0])
          (spawn-fiber
           (lambda () (channel-put! received (channel-get messages))))
          (let ([sent
                 (event-sync
                  (event-wrap
                   (channel-put-event messages #f)
                   (lambda (ignored)
                     (set! wrap-count (add1 wrap-count))
                     'sent)))])
            (and (eq? 'sent sent)
                 (eq? #f (channel-get received))
                 (= 1 wrap-count)))))))

(mat fiber-api-named-mutex-and-cvar
     (run-fibers
      (lambda ()
        (fiber-test-watchdog (fiber-test-deadline 5000))
        (let ([mutex (make-fiber-mutex 'mailbox)]
              [changed (make-fiber-cvar 'mailbox-changed)]
              [ready (make-channel)] [done (make-channel)] [message #f])
          (spawn-fiber
           (lambda ()
             (let ([result
                    (with-fiber-mutex mutex
                      (channel-put! ready #t)
                      (let wait ()
                        (unless message
                          (fiber-cvar-wait changed mutex)
                          (wait)))
                      message)])
               (channel-put! done result))))
          (channel-get ready)
          ;; Acquiring the mutex proves that the child registered before releasing it.
          (fiber-mutex-acquire mutex)
          (set! message '(delivered 42))
          (fiber-cvar-signal changed)
          (fiber-mutex-release mutex)
          (equal? '(delivered 42) (channel-get done))))))

(mat fiber-api-timer-constructors-and-reuse
     (run-fibers
      (lambda ()
        (fiber-test-watchdog (fiber-test-deadline 5000))
        (let ([milliseconds (milisleep-event 0)] [seconds (sleep-event 0)])
          (fiber-milisleep 0)
          (fiber-sleep 0)
          ;; Event values can be constructed once and synchronized more than once.
          (equal? '(milliseconds seconds milliseconds seconds)
                  (map (lambda (event) (event-sync event))
                       (list (event-wrap milliseconds (lambda (ignored) 'milliseconds))
                             (event-wrap seconds (lambda (ignored) 'seconds))
                             (event-wrap milliseconds (lambda (ignored) 'milliseconds))
                             (event-wrap seconds (lambda (ignored) 'seconds)))))))))

(mat fiber-api-timer-off-preserves-values
     (run-fibers
      (lambda ()
        (let* ([results
                (call-with-values
                 (lambda () (with-timer-off (with-timer-off (values 'left 'right))))
                 list)]
               [empty (call-with-values (lambda () (with-timer-off (values))) list)]
               [previous-timer (set-timer 0)])
          (set-timer previous-timer)
          (and (equal? '(left right) results)
               (null? empty)
               (positive? previous-timer))))))

(mat fiber-api-network-result-and-failure
     (run-fibers
      (lambda ()
        (fiber-test-watchdog (fiber-test-deadline 5000))
        (let ([completed
               (make-net-operation 'usage-result
                 (lambda () (net-operation-completed '(response 42))) void)])
          (equal? '(response 42) (event-sync (net-operation-event completed))))))

     ;; Negative case: synchronizing a failed network operation raises its exact failure.
     (error?
      (run-fibers
       (lambda ()
         (event-sync
          (net-operation-event
           (make-net-operation 'usage-failure
             (lambda ()
               (net-operation-failed
                (condition (make-error) (make-message-condition "usage failure"))))
             void)))))))

(mat fiber-api-requires-run-context
     ;; Spawning requires an active run context.
     (error? (spawn-fiber void))

     ;; Yielding requires a running fiber.
     (error? (fiber-yield))

     ;; Quitting requires a running fiber.
     (error? (fiber-quit))

     ;; Synchronization requires a running fiber even for an immediately ready event.
     (error? (event-sync (event-always 'ready)))

     ;; The synchronization alias also requires a run context.
     (error? (perform-operation (event-never)))

     ;; Receiving requires a running fiber.
     (error? (channel-get (make-channel)))

     ;; Sending requires a running fiber.
     (error? (channel-put! (make-channel) 'message))

     ;; Mutex acquisition requires a fiber owner token.
     (error? (fiber-mutex-acquire (make-fiber-mutex)))

     ;; Mutex release requires a fiber owner token.
     (error? (fiber-mutex-release (make-fiber-mutex)))

     ;; Zero-duration millisecond sleep still requires a run context.
     (error? (fiber-milisleep 0))

     ;; Zero-duration second sleep still requires a run context.
     (error? (fiber-sleep 0)))

(mat fiber-api-rejects-invalid-arguments
     ;; The init argument must be a procedure.
     (error? (run-fibers 'not-a-thunk))

     ;; The wrapped value must be an event.
     (error? (event-wrap 'not-an-event values))

     ;; The wrapper must be a procedure.
     (error? (event-wrap (event-always 'ready) 'not-a-procedure))

     ;; Every selected value must be an event.
     (error? (event-select (event-always 'ready) 'not-an-event))

     ;; A receive-event constructor requires a channel.
     (error? (channel-get-event 'not-a-channel))

     ;; A send-event constructor requires a channel.
     (error? (channel-put-event 'not-a-channel 'message))

     ;; The receive-event alias requires a channel.
     (error? (get-operation 'not-a-channel))

     ;; The send-event alias requires a channel.
     (error? (put-operation 'not-a-channel 'message))

     ;; Acquiring requires a fiber mutex.
     (error? (fiber-mutex-acquire 'not-a-mutex))

     ;; Releasing requires a fiber mutex.
     (error? (fiber-mutex-release 'not-a-mutex))

     ;; Waiting requires a fiber condition variable.
     (error? (fiber-cvar-wait 'not-a-cvar (make-fiber-mutex)))

     ;; Waiting also requires a fiber mutex.
     (error? (fiber-cvar-wait (make-fiber-cvar) 'not-a-mutex))

     ;; Signaling requires a fiber condition variable.
     (error? (fiber-cvar-signal 'not-a-cvar))

     ;; Broadcasting requires a fiber condition variable.
     (error? (fiber-cvar-broadcast 'not-a-cvar))

     ;; A network event requires a network operation.
     (error? (net-operation-event 'not-an-operation))

     ;; Millisecond event durations cannot be negative.
     (error? (milisleep-event -1))

     ;; Second event durations must be integers.
     (error? (sleep-event 1/2))

     ;; Millisecond sleep durations cannot be negative.
     (error? (fiber-milisleep -1))

     ;; Second sleep durations must be integers.
     (error? (fiber-sleep 1/2)))

(mat fiber-event-never-blocks
     (let ([attempted (make-channel)] [done (abox #f)])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda ()
             (channel-put! attempted #t)
             (event-sync (event-never))
             (abox-set! done #t)))
          (channel-get attempted)
          ;; Give the child time to attempt synchronization before stopping the context.
          (let loop ([remaining 1000])
            (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))))
       ;; Negative case: event-never must not let the waiting child complete.
       (not (unabox done))))

(mat fiber-empty-select-is-never
     (let ([attempted (make-channel)] [done (abox #f)])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda ()
             (channel-put! attempted #t)
             (event-sync (event-select))
             (abox-set! done #t)))
          (channel-get attempted)
          (let loop ([remaining 1000])
            (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))))
       ;; Negative case: an empty selection must remain blocked.
       (not (unabox done))))

(mat fiber-wrap-does-not-mutate-source
     (let ([source (event-select (event-always 1))])
       ;; Negative case: constructing a wrapper must not modify its source event.
       (let ([wrapped (event-wrap source add1)])
         (equal? '(1 2)
                 (run-fibers
                  (lambda ()
                    (list (event-sync source) (event-sync wrapped))))))))

(mat fiber-nested-selection-and-repeated-wrapping
     (let* ([source (event-select (event-never) (event-always 3))]
            [inner (event-wrap source add1)]
            [outer (event-wrap (event-select (event-never) inner) (lambda (x) (* x 2)))]
            [again (event-wrap outer sub1)])
       ;; Negative case: wrapping a shared selection must not mutate any previous event.
       (equal? '(3 4 8 7 3 4 8)
               (run-fibers
                (lambda ()
                  (map event-sync (list source inner outer again source inner outer)))))))

(mat fiber-event-wrap-preserves-multiple-values
     (equal? '(4 5)
             (call-with-values
              (lambda ()
                (run-fibers
                 (lambda ()
                   (event-sync
                    (event-wrap
                     (event-wrap (event-always 3) (lambda (x) (values x (add1 x))))
                     (lambda (left right) (values (add1 left) (add1 right))))))))
              list)))

(mat fiber-quit-stops-context
     (begin (run-fibers (lambda () (fiber-quit))) #t))

(mat fiber-quit-stops-only-child
     (let ([events (make-channel)])
       (run-fibers
        (lambda ()
          (spawn-fiber (lambda () (channel-put! events 'child) (fiber-quit)))
          (spawn-fiber (lambda () (channel-put! events 'sibling)))
          (let ([first (channel-get events)] [second (channel-get events)])
            (and (memq 'child (list first second))
                 (memq 'sibling (list first second))
                 #t))))))

(mat fiber-shutdown-drops-runnable-children
     (let ([result (run-fibers
                    (lambda ()
                      (spawn-fiber
                       (lambda ()
                         (let loop () (fiber-yield) (loop))))
                      'complete))])
       (eq? result 'complete)))

(mat fiber-init-error-reaches-caller
     ;; Negative case: the original init condition must reach the caller.
     (let ([original (condition (make-error) (make-who-condition 'fiber-init)
                               (make-message-condition "boom")
                               (make-irritants-condition '(original)))])
       (let ([caught (guard (condition [else condition])
                       (run-fibers (lambda () (fiber-yield) (raise original)))
                       #f)])
         (eq? original caught))))

(mat fiber-child-error-reaches-caller
     ;; Negative case: a child failure must stop the init fiber and reach its caller.
     (let ([original (condition (make-error) (make-message-condition "child boom"))])
       (eq? original
            (guard (condition [else condition])
              (run-fibers
               (lambda ()
                 (spawn-fiber (lambda () (fiber-yield) (raise original)))
                 (let loop () (fiber-yield) (loop))))
              #f))))

(mat fiber-init-false-error-reaches-caller
     ;; Negative case: #f is a raised object, even though it is also the default error value.
     (guard (condition [else (eq? condition #f)])
       (run-fibers (lambda () (fiber-yield) (raise #f)))
       #f))

(mat fiber-child-false-error-reaches-caller
     ;; Negative case: a child's raised #f must stop the context and reach its caller.
     (guard (condition [else (eq? condition #f)])
       (run-fibers
        (lambda ()
          ;; A yield count is not a deadline: a faster init can finish its loop
          ;; before the child starts. Block until failure and bound it by the clock.
          (fiber-test-watchdog (fiber-test-deadline 5000))
          (spawn-fiber (lambda () (fiber-yield) (raise #f)))
          (event-sync (event-never))))
       #f))

(mat fiber-multiple-values-preserved
     (call-with-values (lambda () (run-fibers (lambda () (values 'left 'right))))
       (lambda (left right) (and (eq? left 'left) (eq? right 'right)))))

(mat fiber-nested-run-spawns-and-returns
     (let ([nested-ran #f] [nested-done (make-channel)] [outer-ready (make-channel)])
       (let ((result
              (run-fibers
               (lambda ()
                 (run-fibers
                  (lambda ()
                    ;; The nested thunk waits for work published after run-fibers returns.
                    (channel-get outer-ready)
                    (set! nested-ran #t)
                    (channel-put! nested-done #t)))
                 (channel-put! outer-ready #t)
                 (channel-get nested-done)
                 'outer))))
         (and (eq? result 'outer) nested-ran))))

(mat fiber-cvar-no-lost-wakeup
     (let ([mutex (make-fiber-mutex)] [cvar (make-fiber-cvar)]
           [ready (make-channel)] [finished (make-channel)]
           [iterations 10000] [count 0])
       (let ((result
              (run-fibers
               (lambda ()
                 (spawn-fiber
                  (lambda ()
                    (let loop ([i 0])
                      (unless (= i iterations)
                        (fiber-mutex-acquire mutex)
                        (channel-put! ready #t)
                        (fiber-cvar-wait cvar mutex)
                        (set! count (add1 count))
                        (fiber-mutex-release mutex)
                        (loop (add1 i))))
                    (channel-put! finished #t)))
                 (spawn-fiber
                  (lambda ()
                    (let loop ([i 0])
                      (unless (= i iterations)
                        (channel-get ready)
                        (fiber-mutex-acquire mutex)
                        (fiber-cvar-signal cvar)
                        (fiber-mutex-release mutex)
                        (loop (add1 i))))))
                 (spawn-fiber
                  (lambda ()
                    ;; Negative case: a lost wakeup must fail without depending on fiber timers.
                    ;; Count elapsed time: queue optimization changes yields per second.
                    (let ([deadline (add-duration (current-time 'time-monotonic)
                                                  (make-time 'time-duration 0 30))])
                      (let loop ()
                        (when (time<? (current-time 'time-monotonic) deadline)
                          (fiber-yield)
                          (loop))))
                    (channel-put! finished 'timeout)))
                 (channel-get finished)))))
         (and (eq? result #t) (= count iterations)))))

(mat fiber-mutex-rejects-foreign-release
     (let ([mutex (make-fiber-mutex)] [outcome (make-channel)])
       (run-fibers
        (lambda ()
          (fiber-mutex-acquire mutex)
          (spawn-fiber
           (lambda ()
             ;; Negative case: a foreign fiber must not release this mutex.
             (channel-put! outcome
               (guard (condition
                       [else (and (who-condition? condition)
                                  (eq? 'fiber-mutex-release (condition-who condition)))])
                 (fiber-mutex-release mutex)
                 #f))))
          ;; The owner keeps the mutex until the foreign release has actually been attempted.
          (let ([rejected (channel-get outcome)])
            (when rejected (fiber-mutex-release mutex))
            rejected)))))

(mat fiber-mutex-unwind-releases-to-peer
     (let ([mutex (make-fiber-mutex)] [done (make-channel)]
           [original (condition (make-error) (make-message-condition "mutex unwind"))])
       (run-fibers
        (lambda ()
          ;; Negative case: an exception after yielding must release exactly once.
          (let ([caught (guard (condition [else condition])
                          (with-fiber-mutex mutex (fiber-yield) (raise original))
                          #f)])
            (spawn-fiber
             (lambda ()
               (with-fiber-mutex mutex (void))
               (channel-put! done 'released)))
            (spawn-fiber
             (lambda ()
               (let loop ([remaining 100000])
                 (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
               (channel-put! done 'timeout)))
            (and (eq? caught original) (eq? 'released (channel-get done))))))))

(mat fiber-mutex-yield-preserves-ownership
     (let ([mutex (make-fiber-mutex)] [attempted (make-channel)] [done (make-channel)]
           [entered (abox #f)])
       (run-fibers
        (lambda ()
          (let ([retained
                 (with-fiber-mutex mutex
                   (spawn-fiber
                    (lambda ()
                      (channel-put! attempted #t)
                      (with-fiber-mutex mutex (abox-set! entered #t))
                      (channel-put! done #t)))
                   (channel-get attempted)
                   ;; Negative case: yielding inside the scope must not unlock its mutex.
                   (let loop ([remaining 1000])
                     (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
                   (not (unabox entered)))])
            (and retained (channel-get done) (unabox entered)))))))

(mat fiber-mutex-scope-preserves-multiple-values
     (let ([mutex (make-fiber-mutex)])
       (equal? '(left right)
               (run-fibers
                (lambda ()
                  (let ([result
                         (call-with-values
                          (lambda ()
                            (with-fiber-mutex mutex (fiber-yield) (values 'left 'right)))
                          list)])
                    ;; A normal scope exit must release before another acquisition.
                    (with-fiber-mutex mutex result)))))))

(mat fiber-cvar-wait-requires-mutex-owner
     (let ([mutex (make-fiber-mutex)] [cvar (make-fiber-cvar)])
       (run-fibers
        (lambda ()
          ;; Negative case: waiting without ownership must raise on the calling fiber.
          (guard (condition
                  [else (and (who-condition? condition)
                             (eq? 'fiber-cvar-wait (condition-who condition)))])
            (fiber-cvar-wait cvar mutex)
            #f)))))

(mat fiber-cvar-broadcast-wakes-predicate-waiters
     (let ([mutex (make-fiber-mutex)] [cvar (make-fiber-cvar)]
           [ready (make-channel)] [done (make-channel)] [open? #f])
       (run-fibers
        (lambda ()
          (do ([index 0 (add1 index)]) ((= index 4))
            (spawn-fiber
             (lambda ()
               (with-fiber-mutex mutex
                 (channel-put! ready #t)
                 (let wait ()
                   (unless open? (fiber-cvar-wait cvar mutex) (wait))))
               (channel-put! done #t))))
          (spawn-fiber
           (lambda ()
             ;; Negative case: a broadcast must not strand any of the registered waiters.
             (let loop ([remaining 100000])
               (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
             (channel-put! done #f)))
          (do ([index 0 (add1 index)]) ((= index 4)) (channel-get ready))
          ;; Acquiring proves the final child has registered its wait and released the mutex.
          (with-fiber-mutex mutex
            (set! open? #t)
            (fiber-cvar-broadcast cvar))
          (let loop ([remaining 4])
            (or (zero? remaining)
                (and (channel-get done) (loop (sub1 remaining)))))))))

(mat fiber-mutex-shutdown-discards-waiter
     (let ([mutex (make-fiber-mutex)] [held (make-channel)] [attempted (make-channel)]
           [release (make-channel)] [released (make-channel)] [done (make-channel)])
       (equal? '#(owner reused)
               (run-threads 0
                (lambda ()
                  (run-fibers
                   (lambda ()
                     (fiber-mutex-acquire mutex)
                     (channel-put! held #t)
                     (channel-get release)
                     (fiber-mutex-release mutex)
                     (channel-put! released #t)
                     'owner)))
                (lambda ()
                  (run-fibers
                   (lambda ()
                     (channel-get held)
                     (spawn-fiber
                      (lambda ()
                        (channel-put! attempted #t)
                        (with-fiber-mutex mutex (void))))
                     (channel-get attempted)
                     (let loop ([remaining 1000])
                       (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))))
                  (run-fibers
                   (lambda ()
                     (channel-put! release #t)
                     (channel-get released)
                     (spawn-fiber
                      (lambda ()
                        (with-fiber-mutex mutex (void))
                        (channel-put! done 'reused)))
                     (spawn-fiber
                      (lambda ()
                        ;; Negative case: a canceled waiter must never receive ownership.
                        (let loop ([remaining 100000])
                          (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
                        (channel-put! done 'timeout)))
                     (channel-get done))))))))

(mat fiber-cvar-shutdown-discards-waiter
     (let ([mutex (make-fiber-mutex)] [cvar (make-fiber-cvar)]
           [ready (make-channel)] [done (make-channel)])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda ()
             (with-fiber-mutex mutex
               (channel-put! ready #t)
               (fiber-cvar-wait cvar mutex))))
          (channel-get ready)
          ;; The child is registered once this acquisition succeeds.
          (with-fiber-mutex mutex (void))))
       (eq? 'reused
            (run-fibers
             (lambda ()
               (spawn-fiber
                (lambda ()
                  (with-fiber-mutex mutex
                    (channel-put! ready #t)
                    (fiber-cvar-wait cvar mutex))
                  (channel-put! done 'reused)))
               (spawn-fiber
                (lambda ()
                  ;; Negative case: a stale cvar waiter must not consume the fresh signal.
                  (let loop ([remaining 100000])
                    (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
                  (channel-put! done 'timeout)))
               (channel-get ready)
               (with-fiber-mutex mutex (fiber-cvar-signal cvar))
               (channel-get done))))))

(mat fiber-select-cleans-losing-registrations
     (let ([unused (make-channel)] [active (make-channel)] [done (make-channel)]
           [iterations 1000])
       (eq? 'complete
            (run-fibers
             (lambda ()
               (spawn-fiber
                (lambda ()
                  (let loop ([i 0])
                    (when (< i iterations)
                      (spawn-fiber (lambda () (fiber-yield) (channel-put! active i)))
                      (unless (= i (event-sync
                                    (event-select (channel-get-event unused)
                                                  (channel-get-event active))))
                        (error 'fiber-select "wrong channel value"))
                      (loop (add1 i))))
                  (channel-put! done 'complete)))
               (spawn-fiber
                (lambda ()
                  ;; Negative case: leaked/blocked choices must fail through public behavior.
                  (let loop ([remaining 100000])
                    (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
                  (channel-put! done 'timeout)))
               (channel-get done))))))

(mat fiber-select-cleans-losing-network-registrations
     (let ([active (make-channel)] [done (make-channel)]
           [operation (make-net-operation 'losing-choice
                        (lambda () (net-operation-pending '() #f)) void)])
       (eq? 'complete
            (run-fibers
             (lambda ()
               (spawn-fiber
                (lambda ()
                  (let loop ([remaining 400])
                    (unless (zero? remaining)
                      (spawn-fiber
                       (lambda ()
                         ;; Ensure the network branch registered before the channel wins.
                         (let wait ([attempts 10000])
                           (when (and (positive? attempts)
                                      (zero? (vector-ref
                                              (net-operation-event-pool-metrics) 1)))
                             (fiber-yield) (wait (sub1 attempts))))
                         (channel-put! active 'channel)))
                      (unless (eq? 'channel
                                   (event-sync
                                    (event-select (net-operation-event operation)
                                                  (channel-get-event active))))
                        (error 'fiber-select "wrong selection result"))
                      ;; Losing network waiters must be returned immediately to the pool.
                      (unless (zero? (vector-ref (net-operation-event-pool-metrics) 1))
                        (error 'fiber-select "network waiter retained"))
                      (loop (sub1 remaining))))
                  (channel-put! done 'complete)))
               (spawn-fiber
                (lambda ()
                  ;; Negative case: repeated losing registrations must not exhaust the pool.
                  (let loop ([remaining 200000])
                    (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
                  (channel-put! done 'timeout)))
               (channel-get done))))))

(mat fiber-select-duplicate-network-branches
     (let* ([operation (make-net-operation 'duplicate-choice
                         (lambda () (net-operation-completed 'done)) void)]
            [event (net-operation-event operation)])
       ;; Negative case: committing one branch must safely detach its pooled sibling waiters.
       (and (eq? 'done
                 (run-fibers (lambda () (event-sync (event-select event event event)))))
            (zero? (vector-ref (net-operation-event-pool-metrics) 1)))))

(mat fiber-channel-shutdown-discards-waiter
     (let ([channel (make-channel)] [attempted (make-channel)])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda () (channel-put! attempted #t) (channel-put! channel 'abandoned)))
          (channel-get attempted)
          (let loop ([remaining 1000])
            (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))))
       ;; Negative case: a stopped context's sender must not participate in a later rendezvous.
       (eq? 'fresh
            (run-fibers
             (lambda ()
               (spawn-fiber (lambda () (channel-put! channel 'fresh)))
               (channel-get channel))))))

(mat fiber-channel-selection-does-not-rendezvous-with-itself
     (let ([channel (make-channel)])
       (eq? 'peer
            (run-fibers
             (lambda ()
               (spawn-fiber (lambda () (fiber-yield) (channel-put! channel 'peer)))
               ;; Negative case: opposite branches of one transaction cannot match each other.
               (event-sync (event-select (channel-get-event channel)
                                         (channel-put-event channel 'self))))))))

(mat fiber-channel-rendezvous-across-contexts
     (let* ([channel (make-channel)]
            [results
             (run-threads 0
              (lambda () (run-fibers (lambda () (channel-get channel))))
              (lambda () (run-fibers (lambda () (channel-put! channel 'shared) 'sent))))])
       (and (eq? 'shared (vector-ref results 0)) (eq? 'sent (vector-ref results 1)))))

(mat fiber-sleep-event
     (let ([milliseconds #f] [seconds #f])
       (run-fibers
        (lambda ()
          (event-sync (milisleep-event 1))
          (set! milliseconds #t)
          (event-sync (sleep-event 1))
          (set! seconds #t)))
       (and milliseconds seconds)))

(mat fiber-sleep-event-wrapper-result
     (equal? '(wrapped selected)
             (run-fibers
              (lambda ()
                (list
                 (event-sync
                  (event-wrap (milisleep-event 1) (lambda (ignored) 'wrapped)))
                 (event-sync
                  (event-wrap (event-select (event-never) (milisleep-event 1))
                              (lambda (ignored) 'selected))))))))

(mat fiber-zero-sleep-is-immediate
     (let ([done #f])
       (run-fibers
        (lambda ()
          (fiber-milisleep 0)
          (set! done #t)))
       done))

(mat fiber-sleep-honors-minimum-delay
     (let ([started #f] [finished #f])
       (run-fibers
        (lambda ()
          (let ([before (current-time 'time-monotonic)])
            (set! started before)
            (fiber-milisleep 20)
            (set! finished (current-time 'time-monotonic)))))
       ;; Negative case: a timer must not resume before its requested deadline.
       (and started finished
            (let ([elapsed (time-difference finished started)])
              (or (> (time-second elapsed) 0)
                  (>= (time-nanosecond elapsed) 15000000))))))

(mat fiber-many-sleepers-share-workers
     (let ([done (make-channel)] [count 32])
       (run-fibers
        (lambda ()
          (do ([i 0 (add1 i)]) ((= i count))
            (spawn-fiber
             (lambda ()
               (fiber-milisleep 1)
               (channel-put! done #t))))
          (let loop ([remaining count])
            (if (zero? remaining)
                #t
                (and (channel-get done) (loop (sub1 remaining)))))))))

(mat fiber-sleep-shutdown-cleans-timers
     (let* ([registered (abox #f)] [wrappers (abox 0)] [resumes (abox 0)]
            [operation (make-net-operation 'timer-shutdown-registration
                         (lambda ()
                           (abox-set! registered #t)
                           (net-operation-pending '() #f))
                         void)])
       (dynamic-wind
         void
         (lambda ()
           (let ([stopped
                  (run-fibers
                   (lambda ()
                     (spawn-fiber
                      (lambda ()
                        ;; Poll order is random, but failed polls register in source order
                        ;; under the event mutex. The driver callback can run only after
                        ;; that mutex is released, proving the earlier timer registered.
                        (event-sync
                         (event-select
                          (event-wrap (milisleep-event 1000)
                                      (lambda (ignored) (abox-get&add1! wrappers)))
                          (net-operation-event operation)))
                        (abox-get&add1! resumes)))
                     (fiber-test-await (lambda () (unabox registered))
                                       (fiber-test-deadline 5000))))])
             (net-operation-cancel! operation)
             ;; Negative case: shutdown must prevent wrappers and resumes, including
             ;; after the abandoned timer's deadline while a later context is active.
             (let ([late (fiber-test-deadline 1100)])
               (and stopped
                    (eq? 'reused
                         (run-fibers
                          (lambda ()
                            (and (fiber-test-await
                                  (lambda () (time>=? (current-time 'time-monotonic) late))
                                  (fiber-test-deadline 5000))
                                 'reused))))
                    (zero? (unabox wrappers))
                    (zero? (unabox resumes))))))
         (lambda () (net-operation-cancel! operation)))))

(mat fiber-select-cleans-losing-timer-before-late-expiry
     (let* ([registered (abox #f)] [timer-wrappers (abox 0)]
            [channel-wrappers (abox 0)] [resumes (abox 0)] [active (make-channel)]
            [operation (make-net-operation 'losing-timer-registration
                         (lambda ()
                           (abox-set! registered #t)
                           (net-operation-pending '() #f))
                         void)])
       (dynamic-wind
         void
         (lambda ()
           (run-fibers
            (lambda ()
              (let ([deadline (fiber-test-deadline 5000)])
                (fiber-test-watchdog deadline)
                (spawn-fiber
                 (lambda ()
                   ;; As above, the final driver's callback acknowledges registration
                   ;; of both the earlier timer and channel branches under the mutex.
                   (let ([selected
                          (event-sync
                           (event-select
                            (event-wrap (milisleep-event 1000)
                                        (lambda (ignored)
                                          (abox-get&add1! timer-wrappers)
                                          'timer))
                            (event-wrap (channel-get-event active)
                                        (lambda (value)
                                          (abox-get&add1! channel-wrappers)
                                          value))
                            (net-operation-event operation)))])
                     (unless (eq? selected 'channel)
                       (error 'fiber-test "timer won before channel selection"))
                     (abox-get&add1! resumes))))
                (and (fiber-test-await (lambda () (unabox registered)) deadline)
                     (let ([late (fiber-test-deadline 1100)])
                       (channel-put! active 'channel)
                       (and (fiber-test-await (lambda () (= 1 (unabox resumes))) deadline)
                            ;; Negative case: expiry of the losing timer must neither
                            ;; invoke its wrapper nor repeat the winning wrapper/resume.
                            (fiber-test-await
                             (lambda () (time>=? (current-time 'time-monotonic) late))
                             deadline)
                            (zero? (unabox timer-wrappers))
                            (= 1 (unabox channel-wrappers))
                            (= 1 (unabox resumes)))))))))
         (lambda () (net-operation-cancel! operation)))))

(mat fiber-with-timer-off-restores-on-error
     (let ([caught #f] [restored #f])
       (run-fibers
        (lambda ()
          ;; Negative case: timer-off must restore preemption after an exception.
          (set-timer 10000000)
          (set! caught
                (guard (condition [else #t])
                  (with-timer-off (error 'timer "boom"))
                  #f))
          (set! restored (positive? (set-timer 0)))))
       (and caught restored)))

(mat fiber-independent-contexts
     (let ([left-entered (abox #f)] [right-entered (abox #f)]
           [left-published (abox #f)] [right-published (abox #f)]
           [left-finished (abox #f)] [right-finished (abox #f)])
       (define exercise-context
         (lambda (label entered peer-entered published peer-published finished peer-finished)
           (run-fibers
            (lambda ()
              (let ([deadline (fiber-test-deadline 10000)]
                    [request (make-channel)] [reply (make-channel)] [iterations 100])
                (fiber-test-watchdog deadline)
                (abox-set! entered #t)
                ;; Both init fibers must enter before either publishes child work.
                (unless (fiber-test-await (lambda () (unabox peer-entered)) deadline)
                  (error 'fiber-test "peer context did not enter"))
                (spawn-fiber
                 (lambda ()
                   (do ([index 0 (add1 index)]) ((= index iterations))
                     (let ([value (channel-get request)])
                       (fiber-yield)
                       (channel-put! reply (list label value))))))
                (abox-set! published #t)
                (unless (fiber-test-await (lambda () (unabox peer-published)) deadline)
                  (error 'fiber-test "peer context did not publish child"))
                (do ([index 0 (add1 index)]) ((= index iterations))
                  (channel-put! request index)
                  (unless (equal? (list label index) (channel-get reply))
                    (error 'fiber-test "wrong context or round-trip result"))
                  (fiber-yield))
                (abox-set! finished #t)
                ;; Keep both contexts active until all child block/resume cycles finish.
                (and (fiber-test-await (lambda () (unabox peer-finished)) deadline)
                     label))))))
       (let ([results
              (run-threads 0
               (lambda ()
                 (exercise-context 'left left-entered right-entered
                                   left-published right-published left-finished right-finished))
               (lambda ()
                 (exercise-context 'right right-entered left-entered
                                   right-published left-published right-finished left-finished)))])
         (and (eq? (vector-ref results 0) 'left)
              (eq? (vector-ref results 1) 'right)))))

(mat fiber-forked-thread-runs-independent-context
     (run-fibers
      (lambda ()
        (let ([result (abox #f)])
          ;; Chez thread parameters are inherited, but a new OS thread must create
          ;; its own context rather than enqueue its init thunk in the parent.
          (thread-join
           (fork-thread
            (lambda ()
              (abox-set! result
                (run-fibers
                 (lambda ()
                   (fiber-yield)
                   (values 'independent)))))))
          (eq? 'independent (unabox result))))))

(mat fiber-forked-thread-cannot-release-parent-mutex
     ;; Negative case: inherited parameters must not confer fiber ownership.
     (error?
      (let ([failure (abox #f)])
        (run-fibers
         (lambda ()
           (let ([mutex (make-fiber-mutex)])
             (fiber-mutex-acquire mutex)
             (thread-join
              (fork-thread
               (lambda ()
                 ;; Transport the exception to mat's thread without invoking a
                 ;; continuation captured by its exception handler on another thread.
                 (guard (condition [else (abox-set! failure condition)])
                   (fiber-mutex-release mutex)))))
             (when (unabox failure) (fiber-mutex-release mutex)))))
        (when (unabox failure) (raise (unabox failure))))))

(mat fiber-owner-checks-under-preemption
     (run-fibers
      (lambda ()
        (fiber-test-watchdog (fiber-test-deadline 10000))
        (let ([done (make-channel)])
          (do ([worker 0 (add1 worker)]) ((= worker 24))
            (spawn-fiber
             (lambda ()
               (let ([mutex (make-fiber-mutex)])
                 (do ([round 0 (add1 round)]) ((= round 256))
                   ;; Tiny, varied instruction budgets exercise migration near
                   ;; the scheduler/owner checks. Valid fibers must keep access.
                   (set-timer (add1 (modulo round 13)))
                   (fiber-mutex-acquire mutex)
                   (set-timer (add1 (modulo round 7)))
                   (fiber-mutex-release mutex))
                 (channel-put! done #t)))))
          (let collect ([remaining 24])
            (or (zero? remaining)
                (and (channel-get done) (collect (sub1 remaining)))))))))

;;;;===----------------------------------------------------------------------===
;;;; Scheduler configuration
;;;;===----------------------------------------------------------------------===

(mat fiber-configured-single-worker-preemption
     ;; With one worker, the init fiber cannot observe the child without preemption.
     (let ([ready (abox #f)])
       (run-fibers
        (lambda ()
          (spawn-fiber (lambda () (abox-set! ready #t)))
          (let wait ([remaining 1000000])
            (cond [(unabox ready) #t]
                  [(zero? remaining) #f]
                  [else (wait (sub1 remaining))])))
        1 100)))

(mat fiber-configured-workers-and-values
     ;; The configured pool must execute every child and preserve all init values.
     (equal? '(42 done)
             (call-with-values
              (lambda ()
                (run-fibers
                 (lambda ()
                   (let ([done (make-channel)])
                     (do ([index 0 (add1 index)]) ((= index 16))
                       (spawn-fiber (lambda () (channel-put! done 1))))
                     (do ([index 0 (add1 index)] [sum 0 (+ sum (channel-get done))])
                         ((= index 16) (values (+ sum 26) 'done)))))
                 2 30000))
              list)))

(mat fiber-configured-quantum-is-installed
     ;; Sampling the initial budget catches an ignored quantum without timing CPU work.
     ;; The lower bound allows entry overhead; the upper bound rejects a larger default.
     (for-all
      (lambda (quantum)
        (run-fibers
         (lambda ()
           (let ([remaining (set-timer 0)])
             (and (<= remaining quantum) (> remaining (quotient quantum 2)))))
         1 quantum))
      '(1000000 100000000)))

(mat fiber-configured-workers-execute-concurrently
     ;; Both tasks must enter on different OS threads before either can leave its
     ;; barrier. OS sleeps intentionally occupy the worker: fiber-yield would let
     ;; a one-worker pool satisfy the barrier sequentially and hide ignored settings.
     (let ([entered (abox 0)])
       (run-fibers
        (lambda ()
          (let ([done (make-channel)])
            (do ([index 0 (add1 index)]) ((= index 2))
              (spawn-fiber
               (lambda ()
                 (let ([result
                        (with-timer-off
                          (let ([thread-id (get-thread-id)]
                                [deadline (fiber-test-deadline 1000)])
                            (abox-get&add1! entered)
                            (let wait ()
                              (cond [(= 2 (unabox entered)) (cons thread-id #t)]
                                    [(time>=? (current-time 'time-monotonic) deadline)
                                     (cons thread-id #f)]
                                    [else (milisleep 1) (wait)]))))])
                   (channel-put! done result)))))
            (let ([first (channel-get done)] [second (channel-get done)])
              (and (cdr first) (cdr second) (not (eqv? (car first) (car second)))))))
        2 30000)))

(mat fiber-configuration-rejects-invalid-arguments
     ;; Negative case: the worker count cannot be zero.
     (error? (run-fibers (lambda () #t) 0))

     ;; Negative case: the worker count must be a fixnum, not a fraction.
     (error? (run-fibers (lambda () #t) 1/2))

     ;; Negative case: the quantum cannot disable preemption with zero ticks.
     (error? (run-fibers (lambda () #t) 1 0))

     ;; Negative case: the quantum cannot be negative.
     (error? (run-fibers (lambda () #t) 1 -1))

     ;; Negative case: a bignum is outside ChezScheme's instruction timer range.
     (error? (run-fibers (lambda () #t) 1 (expt 2 100))))

(mat fiber-idle-workers-wake-for-published-work
     ;; Repeated empty-queue transitions must not lose the producer's wakeup.
     (run-fibers
      (lambda ()
        (let ([results (make-channel)])
          (do ([round 0 (add1 round)]) ((= round 200) #t)
            (fiber-milisleep 1)
            (do ([index 0 (add1 index)]) ((= index 8))
              (spawn-fiber (lambda () (fiber-yield) (channel-put! results index))))
            (let ([sum (do ([index 0 (add1 index)] [sum 0 (+ sum (channel-get results))])
                           ((= index 8) sum))])
              (unless (= sum 28) (error 'fiber-test "lost or duplicated runnable task"))))))
      4 100))

(mat fiber-timers-progress-with-continuously-runnable-work
     ;; A local queue fast path must still service due timers when it never empties.
     (let ([stop (abox #f)])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda ()
             (let loop ()
               (unless (unabox stop) (fiber-yield) (loop)))))
          (fiber-milisleep 5)
          (abox-set! stop #t)
          #t)
        1 100)))

(mat fiber-ready-event-does-not-schedule-another-fiber
     ;; A ready operation must not capture/requeue its caller just to return a value.
     (run-fibers
      (lambda ()
        (let ([child-ran #f])
          (with-timer-off
            (spawn-fiber (lambda () (set! child-ran #t)))
            (let ([result (event-sync (event-always 42))])
              (and (= result 42) (not child-ran))))))
      1 300000))

(mat fiber-ready-wrapper-can-yield-and-synchronize
     ;; Ready wrappers are user code: they must run outside every protocol lock.
     (run-fibers
      (lambda ()
        (let ([channel (make-channel)])
          (spawn-fiber (lambda () (channel-put! channel 40)))
          (= 42 (event-sync
                 (event-wrap (event-always 2)
                             (lambda (value)
                               (fiber-yield)
                               (+ value (channel-get channel))))))))
      2 100))

(mat fiber-choice-joins-previously-blocked-resources
     ;; Joining lock domains must keep older channel/timer registrations cancellable.
     (run-fibers
      (lambda ()
        (fiber-test-watchdog (fiber-test-deadline 10000))
        (let ([left (make-channel)] [right (make-channel)] [done (make-channel)])
          (spawn-fiber
           (lambda ()
             (channel-put! done
               (event-sync (event-select (channel-get-event left)
                                         (milisleep-event 60000))))))
          (spawn-fiber
           (lambda ()
             (channel-put! done
               (event-sync (event-select (channel-get-event right)
                                         (milisleep-event 60000))))))
          (fiber-milisleep 2)
          ;; Connect domains while transactions already wait on each original root.
          (let ([choice (event-select (channel-put-event left 'delivered)
                                      (channel-put-event right 'delivered))])
            (event-sync choice)
            (event-sync choice))
          (and (eq? 'delivered (channel-get done))
               (eq? 'delivered (channel-get done)))))
      4 100))

(mat fiber-opposite-choice-order-does-not-deadlock
     ;; Concurrent choices list shared resources in opposite orders; both must finish.
     (run-fibers
      (lambda ()
        (fiber-test-watchdog (fiber-test-deadline 10000))
        (let ([left (make-channel)] [right (make-channel)] [done (make-channel)])
          (spawn-fiber
           (lambda ()
             (do ([round 0 (add1 round)]) ((= round 1000))
               (event-sync (event-select (channel-put-event left 1)
                                         (channel-put-event right 1))))
             (channel-put! done #t)))
          (spawn-fiber
           (lambda ()
             (do ([round 0 (add1 round)]
                  [sum 0 (+ sum (event-sync
                                 (event-select (channel-get-event right)
                                               (channel-get-event left))))])
                 ((= round 1000) (channel-put! done (= sum 1000))))))
          (and (channel-get done) (channel-get done))))
      4 100))

(mat fiber-repeated-overlapping-domain-joins
     ;; Each round creates three roots and joins overlapping pairs concurrently.
     ;; Reusing one pair would exercise a real merge only during the first round.
     (run-fibers
      (lambda ()
        (fiber-test-watchdog (fiber-test-deadline 20000))
        (do ([round 0 (add1 round)]) ((= round 100) #t)
          (let ([left (make-channel)] [middle (make-channel)] [right (make-channel)]
                [ready (make-channel)] [done (make-channel)] [start (abox #f)])
            (define launch
              (lambda (thunk)
                (spawn-fiber
                 (lambda ()
                   (channel-put! ready #t)
                   (let wait () (unless (unabox start) (fiber-yield) (wait)))
                   (channel-put! done (thunk))))))
            (launch
             (lambda ()
               (event-sync (event-select (channel-put-event left 42)
                                         (channel-put-event middle 42)))
               0))
            (launch
             (lambda ()
               (event-sync (event-select (channel-get-event right)
                                         (channel-get-event middle)))))
            (launch
             (lambda ()
               ;; This third edge connects the two other choices without consuming.
               (event-select (channel-get-event right) (channel-get-event left))
               0))
            (do ([index 0 (add1 index)]) ((= index 3)) (channel-get ready))
            (abox-set! start #t)
            (let ([sum (do ([index 0 (add1 index)] [sum 0 (+ sum (channel-get done))])
                           ((= index 3) sum))])
              (unless (= sum 42) (error 'fiber-test "incorrect joined rendezvous"))))))
      4 100))

(mat fiber-shutdown-races-independent-transaction-registries
     ;; Stop while many independent resources register/finish; then verify that
     ;; their abandoned senders and receivers cannot participate in a later run.
     (let rounds ([remaining 20])
       (or (zero? remaining)
           (let ([channels (map (lambda (_) (make-channel)) (iota 24))])
             (run-fibers
              (lambda ()
                (fiber-test-watchdog (fiber-test-deadline 10000))
                (let ([ready (make-channel)])
                  (for-each
                   (lambda (channel)
                     (spawn-fiber
                      (lambda ()
                        (channel-put! ready #t)
                        (let loop () (channel-put! channel 'abandoned) (loop))))
                     (spawn-fiber
                      (lambda ()
                        (channel-put! ready #t)
                        (let loop () (channel-get channel) (loop)))))
                   channels)
                  (do ([index 0 (add1 index)]) ((= index 48)) (channel-get ready))
                  (do ([index 0 (add1 index)]) ((= index 32)) (fiber-yield))))
              4 100)
             (and
              (run-fibers
               (lambda ()
                 (fiber-test-watchdog (fiber-test-deadline 10000))
                 (for-each
                  (lambda (channel)
                    (spawn-fiber (lambda () (channel-put! channel 'fresh))))
                  channels)
                 (andmap (lambda (channel) (eq? 'fresh (channel-get channel))) channels))
               4 100)
              (rounds (sub1 remaining)))))))

(mat fiber-timer-off-survives-suspension
     ;; Suspend inside nested scopes; resumption must not arm the instruction timer.
     (for-all
      (lambda (suspend)
        (run-fibers
         (lambda ()
           (let ([outside (set-timer 10000000)])
             (let ([disabled?
                    (with-timer-off
                      (with-timer-off
                        (suspend)
                        (zero? (set-timer 0))))])
               (let ([restored (set-timer outside)])
                 (and disabled? (positive? restored))))))
         2 100))
      (list fiber-yield (lambda () (fiber-milisleep 1))
            (lambda () (event-sync (event-always #t))))))

(mat fiber-timer-off-exception-after-suspension
     ;; Negative case: a raised object must restore the enclosing timer after a yield.
     (run-fibers
      (lambda ()
        (let ([original (list 'failure)] [caught #f])
          (set-timer 10000000)
          (guard (condition [else (set! caught condition)])
            (with-timer-off (fiber-yield) (raise original)))
          (let ([restored (set-timer 0)])
            (and (eq? caught original) (positive? restored)))))
      1 100))

(mat fiber-timer-off-does-not-disable-siblings
     ;; A disabled task can yield, but another task must retain its own preemption.
     (run-fibers
      (lambda ()
        (let ([done (make-channel)])
          (spawn-fiber
           (lambda ()
             (with-timer-off
               (fiber-yield)
               (channel-put! done (zero? (set-timer 0))))))
          (and (channel-get done) (positive? (set-timer 0)))))
      1 100))

(mat fiber-reinstalls-timer-handler-after-suspension
     ;; A fiber can replace the thread's handler. Its next slice must regain the
     ;; scheduler handler, including when a cached handler is reused between slices.
     (for-all
      (lambda (suspend)
        (let ([stale-handler-ran? #f])
          (and
           (run-fibers
            (lambda ()
              (with-timer-off
                (timer-interrupt-handler (lambda () (set! stale-handler-ran? #t)))
                (suspend))
              ;; Force an interrupt after resumption without depending on CPU speed.
              (set-timer 100)
              (do ([index 0 (add1 index)]) ((= index 100000) #t)))
            1 3000)
           (not stale-handler-ran?))))
      (list fiber-yield (lambda () (fiber-milisleep 1)))))

(define fiber-test-invalid-yield-timer #f)

(mat fiber-invalid-yield-preserves-caller-timer
     ;; Negative case: rejecting a call outside a worker must restore its timer.
     (error?
      (let ([previous (set-timer 10000000)])
        (guard (failure
                [else
                 (set! fiber-test-invalid-yield-timer (set-timer previous))
                 (raise failure)])
          (fiber-yield))))

     (and fiber-test-invalid-yield-timer (positive? fiber-test-invalid-yield-timer)))

(mat fiber-quantum-boundary-has-bounded-progress
     ;; An OS process deadline bounds dispatch livelock independently of fiber timers.
     (for-all
      (lambda (case)
        (guard (failure [else #f])
          (let ([result
                 (process-capture "../cspp" "--script=fiber-quantum-process.ss" "--"
                   (car case) :timeout (begin fiber-test-process-timeout)
                   :stdin null :stdout capture :stderr capture)])
            (and (process-exit-success? (process-result-status result))
                 (string=? (cdr case) (process-result-stdout result))
                 (string=? "" (process-result-stderr result))))))
      '(("1" . "rejected\n") ("2" . "rejected\n") ("99" . "rejected\n")
        ("100" . "completed\n"))))

(mat fiber-forked-thread-timer-keeps-original-handler
     ;; A foreign OS thread must never invoke a parent worker's scheduler escape.
     ;; Discard stderr here: a broken escape can generate an unbounded error stream.
     ;; Exact successful stdout and status still reject that subprocess failure.
     (for-all
      (lambda (mode)
        (guard (failure [else #f])
          (let ([result
                 (process-capture "../cspp" "--script=fiber-inherited-timer-process.ss" "--"
                   (begin mode) :timeout (begin fiber-test-process-timeout)
                   :stdin null :stdout capture :stderr null)])
            (and (process-exit-success? (process-result-status result))
                 (string=? "completed\n" (process-result-stdout result))))))
      '("active" "raising" "late" "nested")))
