(import (chezscheme) (cspp crypto) (cspp crypto ffi) (cspp optional-library))

(define rejects-input?
  (lambda (proc)
    (guard (condition
             [else (and (who-condition? condition) (eq? (condition-who condition) 'pcheck))])
      (proc)
      #f)))

(let ([mode (car (command-line-arguments))])
  (cond
    [(string=? mode "random-lengths")
     ;; Error case: inexact lengths must not cross the integer FFI and become huge
     ;; allocation requests. Reject negative and unrepresentable exact lengths too.
     (for-each
       (lambda (length)
         (for-each
           (lambda (make)
             (unless (rejects-input? (lambda () (make length)))
               (error 'crypto-inputs "invalid random output length accepted" length)))
           (list random-bytes random-key-bytes random-nonce ffi-random-bytevector)))
       (list 1.0 0.0 8.0 +inf.0 +nan.0 -1 1/2 'one
             (+ (most-positive-fixnum) 1) (expt 2 64)))
     (when (optional-library-available? (optional-library-info 'openssl))
       (for-each
         (lambda (make)
           (for-each
             (lambda (length)
               (unless (= length (bytevector-length (make length)))
                 (error 'crypto-inputs "wrong random output length" length)))
             '(0 1 32)))
         (list random-bytes random-key-bytes random-nonce ffi-random-bytevector)))]
    [else (error 'crypto-inputs "unknown fixture mode" mode)]))
