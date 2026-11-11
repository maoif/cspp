;; Shared parser fixture helpers. Expected binary encodings remain independent of production.
(define parser-error-from
  (lambda (thunk)
    (guard (condition [(parser-error? condition) condition])
      (thunk)
      #f)))

(define parser-test-bytes
  (lambda bytevectors
    (let-values ([(port bytes) (open-bytevector-output-port)])
      (call-with-port port
        (lambda (port)
          (for-each (lambda (bytevector) (put-bytevector port bytevector)) bytevectors)
          (bytes))))))

(define with-parser-file
  (lambda (bytes procedure)
    (let ([path (format "parser-fixture-~a.tmp" (random 999999999))])
      (dynamic-wind
        (lambda () (write-u8vec path bytes))
        (lambda () (procedure path))
        (lambda () (when (file-exists? path) (delete-file path)))))))

(define encode-wasm-u32
  (lambda (value)
    (let loop ([value value] [bytes '()])
      (let ([payload (logand value #x7f)] [remaining (ash value -7)])
        (if (zero? remaining)
            (u8-list->bytevector (reverse (cons payload bytes)))
            (loop remaining (cons (logor payload #x80) bytes)))))))
