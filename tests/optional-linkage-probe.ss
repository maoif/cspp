(import (chezscheme))

(define probe-arguments (command-line-arguments))
(unless (= (length probe-arguments) 4)
  (error 'optional-linkage-probe "expected native library, Scheme library, mode, and tests path"))
(putenv "LIBCSPP" (car probe-arguments))
(load (cadr probe-arguments))

(import (cspp) (cspp optional-library) (cspp net))

(define probe-check
  (lambda (value message)
    (unless value
      (error 'optional-linkage-probe message))))

(define probe-disabled
  (lambda (name)
    (let ([info (optional-library-info name)])
      (probe-check
       (and (not (optional-library-available? info))
            (not (optional-library-version info))
            (null? (optional-library-capabilities info))
            (string? (optional-library-error info))
            (string-contains? (optional-library-error info) "disabled at build time"))
       (format "invalid disabled metadata for ~s" name)))))

(let ([mode (string->symbol (caddr probe-arguments))]
      [names '(cares curl grpc idn2 ssh websockets zlib openssl uuid xxhash blake3)])
  (case mode
    [(disabled)
     (for-each probe-disabled names)]

    [(idn2)
     (for-each probe-disabled (remq 'idn2 names))
     (let ([info (optional-library-info 'idn2)])
       (probe-check
        (and (optional-library-available? info)
             (equal? (optional-library-version info) "2.3.9")
             (null? (optional-library-capabilities info))
             (not (optional-library-error info)))
        "invalid linked IDN2 metadata"))
     (probe-check (equal? (idna->ascii "input.test") "fixture.example")
                  "IDNA ASCII conversion did not call the linked fixture")
     (probe-check (equal? (idna->unicode "input.test") "fixture-\xfc;.test")
                  "IDNA Unicode conversion did not call the linked fixture")
     ;; Error case: enabled conversion failures preserve the dependency diagnostic.
     (probe-check
      (guard (condition
              [(net-error? condition)
               (string-contains? (net-error-message condition) "fixture conversion error")]
              [else #f])
        (idna->ascii "error.test")
        #f)
      "enabled IDNA failure did not retain its API diagnostic")
     ;; A real enabled requirement must evaluate boolean and expected-condition clauses.
     (parameterize ([current-directory (cadddr probe-arguments)])
       (load "mat.sls")
       (load "mat-requires-fixture.ss"))]

    [else (error 'optional-linkage-probe "unknown mode" mode)]))
