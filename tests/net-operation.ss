(import (cspp)
        (cspp net)
        (cspp net operation))

(define capture-condition
  (lambda (thunk)
    (guard (failure [else failure])
      (thunk)
      #f)))

(define current-monotonic-ms
  (lambda ()
    (let ([time (current-time 'time-monotonic)])
      (+ (* (time-second time) 1000)
         (quotient (time-nanosecond time) 1000000)))))


(mat net-operation-lifecycle

     (let ([operation
            (make-net-operation 'immediate
                                (lambda () (net-operation-completed 'done))
                                void)])
       (and (eq? 'pending (net-operation-state operation))
            (eq? operation (net-operation-step! operation))
            (eq? 'completed (net-operation-state operation))
            (eq? 'done (net-operation-result operation))))

     (let* ([steps 0]
            [operation
             (make-net-operation
              'test
              (lambda ()
                (set! steps (fx+ steps 1))
                (if (fx= steps 1)
                    (net-operation-pending '() 5000)
                    (net-operation-completed 'done)))
              (lambda () (void)))])
       (and (eq? 'pending (net-operation-state operation))
            (begin (net-operation-step! operation) #t)
            (eq? 'pending (net-operation-state operation))
            (begin (net-operation-step! operation) #t)
            (eq? 'completed (net-operation-state operation))
            (eq? 'done (net-operation-result operation))))

     ;; The wait path blocks in poll until the operation's absolute deadline.
     (let* ([steps 0]
            [started-ms (current-monotonic-ms)]
            [deadline-ms (+ started-ms 30)]
            [operation
             (make-net-operation
              'timeout
              (lambda ()
                (set! steps (fx+ steps 1))
                (if (fx= steps 1)
                    (net-operation-pending '() deadline-ms)
                    (net-operation-completed 'timeout-wakeup)))
              void)])
       (and (eq? 'timeout-wakeup (net-operation-wait operation))
            (fx= steps 2)
            (let ([elapsed-ms (- (current-monotonic-ms) started-ms)])
              (and (>= elapsed-ms 20) (< elapsed-ms 2000)))))

     ;; A poll condition fails the operation and runs cleanup exactly once.
     (let* ([events (list 'read)]
            [target (make-poll-target 0 events)]
            [cleanup-count 0]
            [operation
             (make-net-operation
              'poll-failure
              (lambda () (net-operation-pending (list target) #f))
              void
              (lambda () (set! cleanup-count (fx+ cleanup-count 1))))])
       (set-car! events 'unknown)
       (let ([failure (capture-condition (lambda () (net-operation-wait operation)))])
         (and (condition? failure)
              (eq? 'failed (net-operation-state operation))
              (eq? failure (net-operation-condition operation))
              (fx= cleanup-count 1)
              (begin (net-operation-cancel! operation) #t)
              (fx= cleanup-count 1))))

     ;; A cancelled operation cannot be stepped or read as a successful result.
     (let ([operation
            (make-net-operation 'test
                                (lambda () (net-operation-pending '() #f))
                                (lambda () (void)))])
       (net-operation-cancel! operation)
       (let ([cancel-condition
              (guard (failure [else #f])
                (net-operation-condition operation))])
         (and (eq? 'cancelled (net-operation-state operation))
              (error? cancel-condition)
              (eq? cancel-condition
                   (capture-condition (lambda () (net-operation-wait operation))))
              (error? (capture-condition (lambda () (net-operation-step! operation))))
              (error? (capture-condition (lambda () (net-operation-result operation)))))))

     ;; Error case: cancellation during advancement must survive its returned update.
     (let ([cleanup-count 0] [listener-count 0] [operation #f])
       (set! operation
         (make-net-operation
          'cancel-during-advance
          (lambda ()
            (net-operation-cancel! operation)
            (net-operation-completed 'unexpected))
          void
          (lambda () (set! cleanup-count (fx1+ cleanup-count)))))
       (net-operation-register-listener!
        operation (lambda (operation) (set! listener-count (fx1+ listener-count))))
       (net-operation-step! operation)
       (and (eq? 'cancelled (net-operation-state operation))
            (= cleanup-count 1) (= listener-count 1)
            (error? (capture-condition (lambda () (net-operation-wait operation))))))

     ;; Repeated cancellation invokes cancellation and cleanup procedures only once.
     (let ([cancel-count 0]
           [cleanup-count 0])
       (let ([operation
              (make-net-operation
               'cleanup
               (lambda () (net-operation-pending '() #f))
               (lambda () (set! cancel-count (fx+ cancel-count 1)))
               (lambda () (set! cleanup-count (fx+ cleanup-count 1))))])
         (and (eq? operation (net-operation-cancel! operation))
              (eq? operation (net-operation-cancel! operation))
              (fx= cancel-count 1)
              (fx= cleanup-count 1))))

     ;; A cancel callback failure is stored after cleanup and cancellation still returns normally.
     (let* ([failure (condition (make-error) (make-message-condition "cancel failed"))]
            [cleanup-count 0]
            [operation
             (make-net-operation
              'cancel-failure
              (lambda () (net-operation-pending '() #f))
              (lambda () (raise failure))
              (lambda () (set! cleanup-count (fx+ cleanup-count 1))))])
       (and (eq? operation (guard (condition [else #f])
                             (net-operation-cancel! operation)))
            (eq? 'failed (net-operation-state operation))
            (eq? failure (net-operation-condition operation))
            (fx= cleanup-count 1)))

     ;; A cleanup callback failure is stored and cancellation still returns the same operation.
     (let* ([failure (condition (make-error) (make-message-condition "cleanup failed"))]
            [cancel-count 0]
            [operation
             (make-net-operation
              'cleanup-failure
              (lambda () (net-operation-pending '() #f))
              (lambda () (set! cancel-count (fx+ cancel-count 1)))
              (lambda () (raise failure)))])
       (and (eq? operation (guard (condition [else #f])
                             (net-operation-cancel! operation)))
            (eq? 'failed (net-operation-state operation))
            (eq? failure (net-operation-condition operation))
            (fx= cancel-count 1)))

     ;; When both callbacks fail, cleanup runs and the earlier cancellation condition wins.
     (let* ([cancel-failure
             (condition (make-error) (make-message-condition "cancel failed first"))]
            [cleanup-failure
             (condition (make-error) (make-message-condition "cleanup failed second"))]
            [cleanup-count 0]
            [operation
             (make-net-operation
              'both-failures
              (lambda () (net-operation-pending '() #f))
              (lambda () (raise cancel-failure))
              (lambda ()
                (set! cleanup-count (fx+ cleanup-count 1))
                (raise cleanup-failure)))])
       (and (eq? operation (guard (condition [else #f])
                             (net-operation-cancel! operation)))
            (eq? 'failed (net-operation-state operation))
            (eq? cancel-failure (net-operation-condition operation))
            (fx= cleanup-count 1)))

     ;; Failed operations expose their condition but not a successful result.
     (let* ([failure (condition (make-error) (make-message-condition "failed"))]
            [cleanup-count 0]
            [operation
             (make-net-operation
              'failure
              (lambda () (net-operation-failed failure))
              void
              (lambda () (set! cleanup-count (fx+ cleanup-count 1))))])
       (net-operation-step! operation)
       (and (eq? 'failed (net-operation-state operation))
            (eq? failure (net-operation-condition operation))
            (error? (capture-condition (lambda () (net-operation-result operation))))
            (fx= cleanup-count 1)))

     ;; Cleanup failure does not replace an explicit failed update's primary condition.
     (let* ([advance-failure
             (condition (make-error) (make-message-condition "advance failed"))]
            [cleanup-failure
             (condition (make-error) (make-message-condition "cleanup also failed"))]
            [cleanup-count 0]
            [operation
             (make-net-operation
              'failed-cleanup
              (lambda () (net-operation-failed advance-failure))
              void
              (lambda ()
                (set! cleanup-count (fx+ cleanup-count 1))
                (raise cleanup-failure)))])
       (and (eq? operation (net-operation-step! operation))
            (eq? 'failed (net-operation-state operation))
            (eq? advance-failure (net-operation-condition operation))
            (fx= cleanup-count 1)))

     ;; Conditions raised by advancement become failed operation results.
     (let ([operation
            (make-net-operation
             'raised
             (lambda () (error 'raised "failed"))
             void)])
       (net-operation-step! operation)
       (and (eq? 'failed (net-operation-state operation))
            (condition? (net-operation-condition operation))))

     ;; Cleanup failure does not replace a condition raised by advancement.
     (let* ([advance-failure
             (condition (make-error) (make-message-condition "advance raised"))]
            [cleanup-failure
             (condition (make-error) (make-message-condition "cleanup raised"))]
            [cleanup-count 0]
            [operation
             (make-net-operation
              'raised-cleanup
              (lambda () (raise advance-failure))
              void
              (lambda ()
                (set! cleanup-count (fx+ cleanup-count 1))
                (raise cleanup-failure)))])
       (and (eq? operation (net-operation-step! operation))
            (eq? 'failed (net-operation-state operation))
            (eq? advance-failure (net-operation-condition operation))
            (fx= cleanup-count 1)))

     ;; Cleanup failure after successful advancement becomes the terminal condition.
     (let* ([cleanup-failure
             (condition (make-error) (make-message-condition "completion cleanup failed"))]
            [cleanup-count 0]
            [operation
             (make-net-operation
              'completed-cleanup
              (lambda () (net-operation-completed 'done))
              void
              (lambda ()
                (set! cleanup-count (fx+ cleanup-count 1))
                (raise cleanup-failure)))])
       (and (eq? operation (net-operation-step! operation))
            (eq? 'failed (net-operation-state operation))
            (eq? cleanup-failure (net-operation-condition operation))
            (fx= cleanup-count 1)))

     ;; Pending and completed operations do not expose a failure condition.
     (let ([operation
            (make-net-operation 'pending
                                (lambda () (net-operation-completed 'done))
                                void)])
       (and (error? (capture-condition (lambda () (net-operation-result operation))))
            (error? (capture-condition (lambda () (net-operation-condition operation))))
            (begin (net-operation-step! operation) #t)
            (error? (capture-condition (lambda () (net-operation-condition operation))))))

     ;; An invalid advancement update is caught and stored as a failed condition.
     (let ([operation (make-net-operation 'invalid (lambda () 'invalid) void)])
       (net-operation-step! operation)
       (and (eq? 'failed (net-operation-state operation))
            (condition? (net-operation-condition operation))))
     )


(mat net-would-block-record

     (let ([would-block (make-net-would-block 'socket '(read write))])
       (and (net-would-block? would-block)
            (eq? 'socket (net-would-block-resource would-block))
            (equal? '(read write) (net-would-block-events would-block))))

     ;; Error case: would-block event lists must contain supported event symbols.
     (let ([failure
            (capture-condition
             (lambda () (make-net-would-block 'socket '(unknown))))])
       (error? failure))
     )


(mat net-operation-poll-resource

     (let* ([descriptor-target (make-poll-target 0 '(read))]
            [operation
             (make-net-operation
              'poll-resource
              (lambda () (net-operation-pending (list descriptor-target) #f))
              void)])
       (net-operation-step! operation)
       (let ([operation-target (make-poll-target operation '(read))])
         (and (eq? operation (poll-target-resource operation-target))
              (fx= 0 (poll-target-fd operation-target))
              (equal? '(read) (poll-target-events operation-target)))))
     )

(mat net-operation-step-cancel-race-is-serialized
     ;; Cancellation must not run its callback while the advance callback owns the operation.
     (let ([advancing? #f] [cancel-overlapped? #f] [cleanup-count 0])
       (let* ([operation
               (make-net-operation
                'race
                (lambda ()
                  (set! advancing? #t)
                  (milisleep 100)
                  (set! advancing? #f)
                  (net-operation-completed 'done))
                (lambda ()
                  (when advancing? (set! cancel-overlapped? #t)))
                (lambda () (set! cleanup-count (fx1+ cleanup-count))))]
              [thread (fork-thread (lambda () (net-operation-step! operation)))])
         (milisleep 20)
         (net-operation-cancel! operation)
         (thread-join thread)
         (and (not cancel-overlapped?)
              (eq? 'completed (net-operation-state operation))
              (= cleanup-count 1)))))
