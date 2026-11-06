(import (cspp chez) (cspp system signal) (cspp system process) (cspp system common))

(mat signal-parse

     (signal? (signal term))
     (signal? (signal "TERM"))
     (signal? (signal "SIGTERM"))
     (signal? (signal 15))
     (signal? (string->signal "sigterm"))
     (= (signal-number (signal term))
        (signal-number (signal "TERM")))
     (= (signal-number (signal term))
        (signal-number (signal 15)))
     (eq? 'term (signal-name (string->signal "TERM")))
     (string? (signal->string (signal term))))

(mat signal-list-basic

     (exists (lambda (s) (eq? 'term (signal-name s))) (signal-list))
     (exists (lambda (s) (eq? 'int (signal-name s))) (signal-list))

     (let ([signals (signal-list)])
       (set-cdr! signals '())
       (exists (lambda (s) (eq? 'int (signal-name s))) (signal-list))))

(mat signal-bound-identifiers

     (let ([term (signal int)])
       (eq? 'int (signal-name (signal term)))))

;; Error case: unknown signal names and numbers should be rejected.
(mat signal-parse-errors

     (guard (c [(error? c) #t] [else #f])
       (signal "NOT_A_SIGNAL")
       #f)

     (guard (c [(error? c) #t] [else #f])
       (signal 9999)
       #f))

;; Error case: send-signal should reject process-group/all-process pid values.
(mat signal-send-validation

     ;; Error case: group 1 would become kill(-1), broadcasting to unrelated processes.
     ;; An unknown signal prevents any native delivery if the group check regresses.
     (guard (condition [else (and (who-condition? condition)
                                 (eq? 'send-process-group-signal (condition-who condition)))])
       (send-process-group-signal 1 'definitely-not-a-signal)
       #f)

     (guard (c [(error? c) #t] [else #f])
       (send-signal 0 term)
       #f)

     (guard (c [(error? c) #t] [else #f])
       (send-signal -1 term)
       #f)

     (guard (c [(error? c) #t] [else #f])
       (send-process-group-signal 0 term)
       #f)

     (guard (c [(error? c) #t] [else #f])
       (send-process-group-signal -1 term)
       #f))

(mat signal-platform-values

     (let ([first (signal-number (signal 'rtmin))]
           [next (signal-number (signal "SIGRTMIN+1"))]
           [last (signal-number (signal 'rtmax))])
       (and (= (+ first 1) next) (< next last)))

     (for-all (lambda (name) (signal? (signal name)))
              '(abrt stop cont tstp ttin ttou urg xcpu xfsz vtalrm prof winch sys)))

(mat signal-exact-validation

     ;; Error case: integer-valued flonums must never cross the integer FFI boundary.
     (guard (c [else #t]) (signal 15.0) #f)

     ;; Error case: process IDs must be exact positive signed 32-bit integers.
     (guard (c [else #t]) (send-signal 1.0 term) #f)

     ;; Error case: oversized process group IDs must not wrap to another target.
     (guard (c [else #t]) (send-process-group-signal #x80000000 term) #f))

(mat signal-mask-roundtrip

     (let ([original (signal-mask)])
       (and (dynamic-wind
              (lambda () (signal-block! (list usr1 usr2)))
              (lambda ()
                (let ([blocked (map signal-name (signal-mask))])
                  (signal-unblock! (list usr2))
                  (and (memq 'usr1 blocked) (memq 'usr2 blocked)
                       (not (memq 'usr2 (map signal-name (signal-mask)))))))
              (lambda () (signal-mask-set! original)))
            (equal? (map signal-number original) (map signal-number (signal-mask))))))

(mat signal-mask-validation

     ;; Error case: SIGKILL and SIGSTOP cannot be masked, unmasked, or waited for.
     (for-all
       (lambda (name)
         (and (guard (c [else #t]) (signal-block! (list name)) #f)
              (guard (c [else #t]) (signal-unblock! (list name)) #f)
              (guard (c [else #t]) (signal-mask-set! (list name)) #f)
              (guard (c [else #t]) (wait-signal (list name)) #f)))
       '(kill stop))

     ;; Error case: synchronous crash signals remain available to Chez's native handlers.
     (for-all
       (lambda (name)
         (and (guard (c [else #t]) (signal-block! (list name)) #f)
              (guard (c [else #t]) (signal-mask-set! (list name)) #f)
              (guard (c [else #t]) (signal-unblock! (list name)) #f)
              (guard (c [else #t]) (wait-signal (list name)) #f)))
       '(segv bus ill fpe))

     ;; Error case: a wait requires a nonempty set that is already blocked in this thread.
     (and (guard (c [else #t]) (wait-signal '()) #f)
          (let ([original (signal-mask)])
            (dynamic-wind
              (lambda () (signal-unblock! (list usr2)))
              (lambda () (guard (c [else #t]) (wait-signal (list usr2)) #f))
              (lambda () (signal-mask-set! original))))))

(mat signal-thread-directed-wait

     (for-all
       (lambda (name)
         (let ([original (signal-mask)] [sig (signal name)]
               [self ((foreign-procedure "pthread_self" () uptr))]
               [send (foreign-procedure "pthread_kill" (uptr int) int)])
           (dynamic-wind
             (lambda () (signal-block! (list sig)))
             (lambda ()
               (and (= 0 (send self (signal-number sig)))
                    (eq? name (signal-name (wait-signal (list sig))))
                    (and (memq name (map signal-name (signal-mask))) #t)))
             (lambda () (signal-mask-set! original)))))
       '(usr1 rtmin)))

(mat signal-wait-allows-collection

     (let ([mutex (make-mutex)] [changed (make-condition)]
           [worker-id #f] [finished? #f] [received #f]
           [self (foreign-procedure "pthread_self" () uptr)]
           [send (foreign-procedure "pthread_kill" (uptr int) int)])
       (fork-thread
         (lambda ()
           (let ([original (signal-mask)])
             (dynamic-wind
               (lambda () (signal-block! (list usr1)))
               (lambda ()
                 (with-mutex mutex
                   (set! worker-id (self))
                   (condition-signal changed))
                 (set! received (signal-name (wait-signal (list usr1)))))
               (lambda () (signal-mask-set! original))))
           (with-mutex mutex
             (set! finished? #t)
             (condition-signal changed))))
       (with-mutex mutex
         (let loop () (unless worker-id (condition-wait changed mutex) (loop))))
       (let ([before (collections)] [blocks (make-vector 30)])
         ;; Error case: an active native sigwait would prevent these allocations from collecting.
         (do ([index 0 (+ index 1)]) ((= index 30))
           (vector-set! blocks index (make-bytevector 1000000)))
         (let ([code (send worker-id (signal-number (signal usr1)))])
           (with-mutex mutex
             (let loop () (unless finished? (condition-wait changed mutex) (loop))))
           (and (= code 0) (eq? received 'usr1) (> (collections) before)
                (= 1000000 (bytevector-length (vector-ref blocks 29))))))))

(mat signal-send-child

     (let ([p (process-spawn "sleep" '("5") '((stdout . null) (stderr . null)))])
       (send-process-signal p term)
       (let ([status (process-wait/timeout p 1000)])
         (eq? 'signal (process-exit-status-kind status))))

     (let ([p (process-spawn "sleep" '("5") '((stdout . null) (stderr . null)))])
       (send-signal (process-pid p) "TERM")
       (let ([status (process-wait/timeout p 1000)])
         (eq? 'signal (process-exit-status-kind status)))))
