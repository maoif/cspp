(import (cspp)
        (cspp optional-library))

(define mat-requires-test-ran? #f)

(define mat-requires-check
  (lambda (value message)
    (unless value
      (error 'mat-requires-fixture message))))

(define mat-requires-capture
  (lambda (expression)
    (let ([skipped #f])
      (let ([output
             (call-with-string-output-port
              (lambda (port)
                (parameterize ([mat-output port] [mat-skipped 0] [optimize-level 2])
                  (eval expression)
                  (set! skipped (mat-skipped)))))])
        (values output skipped)))))

(define mat-requires-bad-syntax?
  (lambda (expression)
    (guard (condition [(syntax-violation? condition) #t] [else #f])
      (eval expression)
      #f)))

(let* ([libraries '(openssl curl cares idn2 ssh websockets grpc zlib uuid xxhash blake3)]
       [available?
        (lambda (name) (optional-library-available? (optional-library-info name)))]
       [enabled (find available? libraries)]
       [disabled (filter (lambda (name) (not (available? name))) libraries)])
  (when enabled
    (set! mat-requires-test-ran? #f)
    (let-values ([(output skipped)
                  (mat-requires-capture
                   `(mat requirement-enabled
                      (mat-requires (,enabled)
                        (begin (set! mat-requires-test-ran? #t) #t))))])
      (mat-requires-check (and mat-requires-test-ran? (= skipped 0) (string=? output ""))
                          "enabled requirement did not evaluate its clause"))

    ;; Error cases: wrappers preserve each expected-condition clause's classification.
    (for-each
     (lambda (kind)
       (let-values ([(output skipped)
                     (mat-requires-capture
                      `(mat requirement-expected-condition
                         (mat-requires (,enabled)
                           (,kind ,(if (eq? kind 'warning?)
                                      '(warningf 'fixture "expected warning")
                                      '(error 'fixture "expected error"))))))])
         (mat-requires-check
          (and (= skipped 0)
               (string-contains? output "Expected")
               (not (string-contains? output "Bug")))
          "wrapped expected-condition clause lost its classification")))
     '(error? warning? sanitized-error?)))

  (unless (null? disabled)
    (set! mat-requires-test-ran? #f)
    (let-values ([(output skipped)
                  (mat-requires-capture
                   `(mat requirement-disabled
                      (mat-requires (,(car disabled))
                        (begin
                          (set! mat-requires-test-ran? #t)
                          (error 'fixture "disabled body must not run")))
                      #t))])
      (mat-requires-check
       (and (not mat-requires-test-ran?) (= skipped 1)
            (string-contains? output "Skipped mat requirement-disabled clause 1:")
            (string-contains? output (symbol->string (car disabled)))
            (not (string-contains? output "Bug"))
            (not (string-contains? output "Error")))
       "disabled requirement evaluated its body or reported a failure")))

  (set! mat-requires-test-ran? #f)
  (let-values ([(output skipped)
                (mat-requires-capture
                 '(mat requirement-multiple
                    (mat-requires (curl ssh)
                      (begin (set! mat-requires-test-ran? #t) #t))))])
    (let ([missing (filter (lambda (name) (not (available? name))) '(curl ssh))])
      (mat-requires-check
       (if (null? missing)
           (and mat-requires-test-ran? (= skipped 0) (string=? output ""))
           (and (not mat-requires-test-ran?) (= skipped 1)
                (for-all (lambda (name) (string-contains? output (symbol->string name)))
                         missing)))
       "multiple requirements did not check and report every missing library"))))

;; Error cases: a clause needs at least one literal library symbol.
(mat-requires-check (mat-requires-bad-syntax? '(mat invalid (mat-requires () #t)))
                    "empty requirements were accepted")

(mat-requires-check (mat-requires-bad-syntax? '(mat invalid (mat-requires ("curl") #t)))
                    "non-symbol requirement was accepted")

;; Error case: unsupported names must be rejected even alongside a disabled feature.
(mat-requires-check
 (guard (condition
         [(message-condition? condition)
          (string-contains? (condition-message condition) "supported optional libraries")]
         [else #f])
   (mat-requires-capture '(mat invalid (mat-requires (curl nghttp2) #t)))
   #f)
 "unsupported requirement was hidden by a disabled feature")
