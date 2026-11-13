(import (cspp) (cspp file))

;; Allocation on the producer must be able to collect while the consumer waits in C.
(define root (car (command-line-arguments)))
(when (member "zero-timeout" (cdr (command-line-arguments)))
  (let ([watcher (make-fswatcher)])
    (dynamic-wind
      void
      (lambda ()
        (when (fswatcher-next! watcher 0)
          (error 'fswatcher-blocking-process "unexpected event")))
      (lambda () (close-fswatcher watcher))))
  (exit 0))

(when (member "finite-timeout" (cdr (command-line-arguments)))
  (let ([watcher (make-fswatcher)])
    (dynamic-wind
      void
      (lambda ()
        (let* ([start (current-time 'time-monotonic)]
               [event (fswatcher-next! watcher 40)]
               [elapsed (time-difference (current-time 'time-monotonic) start)]
               [elapsed-ns (+ (* (time-second elapsed) 1000000000)
                              (time-nanosecond elapsed))])
          (unless (and (not event) (<= 30000000 elapsed-ns 1000000000))
            (error 'fswatcher-blocking-process "incorrect finite timeout"))))
      (lambda () (close-fswatcher watcher))))
  (exit 0))

(define path (path-build root "top.txt"))
(define watcher (make-fswatcher))
(define received #f)
(define sink #f)
(fswatcher-add! watcher path (fswatcher-mask FSW_modify))
(define producer
  (fork-thread
    (lambda ()
      ;; Give the consumer time to enter the native wait before forcing collections.
      (milisleep 100)
      (do ([index 0 (fx1+ index)]) ((fx= index 20000))
        (set! sink (make-bytevector 8192 0)))
      (collect)
      (write-datum>> path 'wake))))
(dynamic-wind
  void
  (lambda ()
    (set! received (fswatcher-next! watcher))
    (thread-join producer))
  (lambda () (close-fswatcher watcher)))
(unless (and (vector? received)
             (equal? path (vector-ref received 0))
             (flag-all-set? (fswatcher-mask FSW_modify) (vector-ref received 1))
             (eq? #f (vector-ref received 2)))
  (error 'fswatcher-blocking-process "incorrect blocking event"))
