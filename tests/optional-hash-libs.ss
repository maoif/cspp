(import (cspp)
        (cspp crypto digest))

(define fail
  (lambda (message)
    (display message (current-error-port))
    (newline (current-error-port))
    (exit 1)))

(define expect-loader-error
  (lambda (dependency expected-fragment thunk)
    (define error-message
      (lambda ()
        ;; The selected optional dependency is intentionally incompatible or incomplete.
        (guard (condition [else (condition-message condition)])
          (thunk)
          (fail (format "~a API unexpectedly succeeded" dependency)))))
    (let* ([result* (apply run-threads 8 (make-list 8 error-message))]
           [message (vector-ref result* 0)])
      (unless (and (string? message)
                   (string-contains? message dependency)
                   (string-contains? message expected-fragment)
                   (for-all (lambda (other)
                              (and (string? other)
                                   (string=? message other)))
                            (vector->list result*)))
        (fail (format "unexpected concurrent loader errors: ~s" result*))))))

(define expect-loader-success
  (lambda (dependency thunk result?)
    (guard (condition
            [else (fail (format "~a API failed: ~a" dependency
                                (condition-message condition)))])
      (unless (result? (thunk))
        (fail (format "~a API returned an invalid result" dependency))))))

(let ([arguments (command-line-arguments)])
  (unless (= (length arguments) 2)
    (fail "expected a dependency and diagnostic fragment"))
  (let ([dependency (car arguments)]
        [expected-fragment (cadr arguments)])
    (cond
     [(string=? dependency "xxhash")
      (if (string=? expected-fragment "success")
          (expect-loader-success dependency
                                 (lambda () (xxhash32-string "cspp"))
                                 integer?)
          (expect-loader-error dependency expected-fragment
                               (lambda () (xxhash32-string "cspp"))))]
     [(string=? dependency "blake3")
      (if (string=? expected-fragment "success")
          (expect-loader-success dependency
                                 (lambda ()
                                   (let* ([input (string->utf8 "abc")]
                                          [digester (make-digester 'blake3)]
                                          [one-shot (bytevector->hex
                                                     (blake3-bytevector input))])
                                     (digester-update-bytevector! digester input)
                                     (let ([before-reset
                                            (bytevector->hex
                                             (digester-get digester))])
                                       (digester-reset! digester)
                                       (digester-update-bytevector! digester input)
                                       (list one-shot
                                             before-reset
                                             (bytevector->hex
                                              (digester-finalize! digester))))))
                                 (lambda (result)
                                   (let ([expected
                                          (string-append
                                           "6437b3ac38465133ffb63b75273a8db5"
                                           "48c558465d79db03fd359c6cd5bd9d85")])
                                     (and (= (length result) 3)
                                          (for-all (lambda (digest)
                                                     (string=? digest expected))
                                                   result)))))
          (expect-loader-error dependency expected-fragment
                               (lambda () (blake3-string "cspp"))))]
     [else (fail (format "unknown dependency: ~a" dependency))])))
