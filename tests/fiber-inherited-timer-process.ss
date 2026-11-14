(import (cspp)
        (cspp concurrency)
        (cspp concurrency fiber))

;; An inherited handler can corrupt a foreign scheduler escape. Keep this probe
;; in a separate process so the suite can bound that failure independently.
(define mode (car (command-line-arguments)))
(define interrupts (abox 0))
(define finished (abox #f))
(define released (abox #f))
(define correct-thread? (abox #t))
(define child-thread-id (abox #f))
(define failure #f)
(define child #f)
(define original-failure (list 'original-timer-failure))

(define-syntax error?
  (syntax-rules ()
    [(_ expression)
     (guard (condition [else (eq? condition original-failure)]) expression #f)]))

(define original-handler
  (lambda ()
    (abox-set! correct-thread?
      (and (unabox correct-thread?) (eqv? (get-thread-id) (unabox child-thread-id))))
    (abox-get&add1! interrupts)
    (if (string=? mode "raising")
        (raise original-failure)
        ;; The fallback must become this thread's handler, too: rearming must
        ;; call it again without entering any ancestor scheduler.
        (when (< (unabox interrupts) 3) (set-timer 10)))))

(define run-child
  (lambda ()
    (abox-set! child-thread-id (get-thread-id))
    (let wait () (unless (unabox released) (milisleep 1) (wait)))
    (guard (condition [else (set! failure condition)])
      (when (string=? mode "nested")
        ;; A second scheduler captures the inherited handler as its original.
        ;; Its shutdown must still leave a safe fallback chain on this caller.
        (unless (= 42 (run-fibers (lambda () (fiber-yield) 42) 1 100))
          (error 'inherited-timer "independent context returned wrong result")))
      (if (string=? mode "raising")
          ;; Negative case: preserve the original handler's raised object.
          (unless (error?
                   (begin
                     (set-timer 10)
                     (do ([index 0 (add1 index)]) ((= index 100000)))))
            (error 'inherited-timer "original timer failure was not raised"))
          (begin
            (set-timer 10)
            (do ([index 0 (add1 index)]) ((= index 100000)))
            (unless (= 3 (unabox interrupts))
              (error 'inherited-timer "original handler did not run three times")))))
    (set-timer 0)
    (abox-set! finished #t)))

(parameterize ([timer-interrupt-handler original-handler])
  (unless
      (eq? 'parent-done
           (run-fibers
            (lambda ()
              (set! child (fork-thread run-child))
              (unless (string=? mode "late")
                (abox-set! released #t)
                ;; Joining here would park the parent and hide escape corruption.
                (let wait () (unless (unabox finished) (fiber-yield) (wait))))
              'parent-done)
            1 30000))
    (error 'inherited-timer "parent context was corrupted"))
  (abox-set! released #t)
  (thread-join child))

(when failure (raise failure))
(unless (and (unabox finished) (unabox correct-thread?)
             (= (if (string=? mode "raising") 1 3) (unabox interrupts)))
  (error 'inherited-timer "foreign timer handler did not preserve caller behavior"))
(display "completed\n")
