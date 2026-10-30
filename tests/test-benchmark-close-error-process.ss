(import (cspp))

;; The parent applies a zero file-size limit so buffered output fails during close.
(define (descriptor-count) (length (directory-list "/proc/self/fd")))
(define config (make-benchmark-config 0 1 0 1 #f #f #f #f #f))
(define results (benchmark-run (make-benchmark 'close-error (lambda (state) (void)) '()) config))
(define baseline-path "tmp-cspp-close-error-baseline.dat")

(define (check-no-leak thunk)
  (collect)
  (parameterize ([collect-trip-bytes (* 1024 1024 1024)])
    (let ([before (descriptor-count)])
      (unless (and (for-all (lambda (_) (thunk)) (iota 8))
                   (= before (descriptor-count)))
        (error 'close-error "failed close leaked descriptors or hid the write error")))))

(dynamic-wind
  (lambda () (void))
  (lambda ()
    ;; Error: baseline output buffered until close must still release its descriptor.
    (check-no-leak
      (lambda ()
        (guard (raised [else (i/o-write-error? raised)])
          (benchmark-save-baseline results baseline-path)
          #f)))

    ;; Error: compile input output must close its descriptor even when flushing raises.
    (check-no-leak
      (lambda ()
        (let ([result
               (car (test-run
                      (list (make-test-compile 'close-error '((+ 1 2)) '()))
                      (make-test-config #f #f #f #f 'never)))])
          (and (eq? (test-result-status result) 'errored)
               (i/o-write-error? (test-result-condition result)))))))
  (lambda () (when (file-exists? baseline-path) (delete-file baseline-path))))
