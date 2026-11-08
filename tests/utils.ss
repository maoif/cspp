(import (cspp chez) (cspp utils))

(mat utility-validation-consistency

     ;; Error case: numeric mutation macros reject nonnumbers using their own public names.
     (for-all
       (lambda (name)
         (parameterize ([optimize-level 3])
           (eval `(let ([value #f])
                    (guard (failure
                             [else (and (eq? ',name (condition-who failure)) (not value))])
                      (,name value)
                      #f))
             (environment '(cspp chez) '(cspp utils)))))
       '(incr! decr! add1! sub1!))

     ;; Error case: the optional unreachable caller must be a symbol or #f.
     (guard (failure [else (eq? 'unreachable! (condition-who failure))])
       (unreachable! 42)
       #f))

(mat flag-duplicate-names

     ;; Error case: duplicate names cannot encode one mask while decoding several masks.
     (for-all
       (lambda (declarations)
         (guard (failure [(syntax-violation? failure) #t] [else #f])
           (eval `(let () (define-flags flags ,@declarations) (flags read))
             (environment '(cspp chez) '(cspp utils)))
           #f))
       '(((read 1) (read 2)) ((read 1) (write 2) (read (or read write))))))

(mat pcheck-explicit-caller

     ;; Error case: a failed argument check must retain the explicit API name.
     (guard (condition
              [else (and (who-condition? condition)
                         (eq? 'different-name (condition-who condition)))])
       (eval '(let ([example-api 'different-name] [value #f]) (pcheck-who example-api ([string? value]) #t))
             (environment '(cspp chez) '(cspp utils)))
       #f)

     ;; Predicate expressions run once per clause and bodies preserve internal definitions/values.
     (guard (condition [else #f])
       (eval '(let ([example-api 'different-name] [events '()] [first 1] [second 2])
                (let ([answer
                        (call-with-values
                          (lambda ()
                            (pcheck-who example-api
                              ([(begin (set! events (cons 'predicate events))
                                   (lambda (value) (set! events (cons value events)) 'accepted))
                                first second])
                              (define result (+ first second))
                              (values result 'done)))
                          list)])
                  (and (equal? '(3 done) answer)
                       (equal? '(2 1 predicate) events))))
             (environment '(cspp chez) '(cspp utils))))

     ;; Error case: later checks and the body must not run after a rejection.
     (guard (condition [else #f])
       (eval '(let ([example-api 'different-name] [value #f] [later? #f])
                (and (guard (condition
                              [else (and (who-condition? condition)
                                         (eq? 'different-name (condition-who condition)))])
                       (pcheck-who example-api
                         ([(begin (set! example-api 'changed-caller) string?) value]
                          [(begin (set! later? #t) string?) value])
                         (set! later? #t))
                       #f)
                     (not later?)))
             (environment '(cspp chez) '(cspp utils))))

     ;; Empty checks preserve zero return values with any symbol-valued caller variable.
     (guard (condition [else #f])
       (eval '(let ([example-api 'different-name])
                (null? (call-with-values
                              (lambda ()
                                (pcheck-who example-api () (values)))
                              list)))
             (environment '(cspp chez) '(cspp utils))))

     ;; Error case: define-who's bound name is used when the caller identifier is who.
     (guard (condition
              [else (and (who-condition? condition)
                         (eq? 'example-api (condition-who condition)))])
       (eval '(let ()
                (define-who example-api
                  (lambda (value) (pcheck-who who ([string? value]) #t)))
                (example-api #f))
             (environment '(cspp chez) '(cspp utils)))
       #f)

     ;; Error case: a dynamic who binding must hold a symbol, even when no checks are requested.
     (for-all
       (lambda (value)
         (guard (condition
                  [else (and (who-condition? condition)
                             (eq? 'pcheck-who (condition-who condition)))])
           (eval `(let ([caller ',value]) (pcheck-who caller () #t))
                 (environment '(cspp chez) '(cspp utils)))
           #f))
       '(#f "example-api" 17))

     ;; Error case: caller forms must be identifiers, not literals or compound expressions.
     (for-all
       (lambda (caller)
         (guard (condition [(syntax-violation? condition) #t] [else #f])
           (eval `(pcheck-who ,caller () #t) (environment '(cspp chez) '(cspp utils)))
           #f))
       '('example-api "example-api" #f (begin 'example-api))))

(mat utility-error-origins

     ;; Error case: invalid arguments identify the API, including shared bytevector workers.
     (for-all
       (lambda (entry)
         (guard (condition
                  [else (and (who-condition? condition)
                             (eq? (car entry) (condition-who condition)))])
           ((cdr entry)) #f))
       (list (cons 'hex (lambda () (hex #f)))
             (cons 'p/not (lambda () (p/not #f)))
             (cons 'procedure-arity (lambda () (procedure-arity #f)))
             (cons 'random-list (lambda () (random-list #f 0 1)))
             (cons 'random-symbol (lambda () (random-symbol #f)))
             (cons 'random-u8vec (lambda () (random-u8vec 1.0))))))

(mat pcase-fallback

     (let ([value 1]) (eq? 'fallback (pcase value [else 'fallback])))

     (equal? '(one two)
       (call-with-values
         (lambda () (let ([value #f]) (pcase value [else (values 'one 'two)]))) list))

     (let ([value "text"])
       (eq? 'string (pcase value [number? 'number] [string? 'string] [else 'fallback])))

     ;; Error case: a dispatch without a match or fallback still raises.
     (guard (condition [else #t])
       (let ([value #f]) (pcase value [number? 'number]))
       #f))

(mat pcase-alternative-evaluation

     ;; Error case: unused alternative expressions must not raise after an earlier match.
     (guard (failure [else #f])
       (let ([value 1])
         (eq? 'number
           (pcase value
             [(number? (error 'unused-predicate "must not be evaluated")) 'number]
             [else 'fallback]))))

     ;; Construct and call each reached predicate once, in order; preserve the body's values.
     (let ([value 1] [events '()])
       (let ([make-predicate
               (lambda (name result)
                 (set! events (cons (list 'build name) events))
                 (lambda (object)
                   (set! events (cons (list 'call name object) events))
                   result))])
         (and
           (equal? '(one two)
             (call-with-values
               (lambda ()
                 (pcase value
                   [((make-predicate 'first #f) (make-predicate 'second 'accepted)
                     (make-predicate 'unused #t))
                    (values 'one 'two)]
                   [else 'fallback]))
               list))
           (equal? '((build first) (call first 1) (build second) (call second 1))
                   (reverse events)))))

     (let ([value #f])
       (eq? 'fallback
         (pcase value [(number? string?) 'unexpected] [else 'fallback])))

     ;; Error case: an exception from a reached alternative expression must propagate.
     (guard (failure
              [else (and (who-condition? failure)
                         (eq? 'reached-predicate (condition-who failure)))])
       (let ([value #f])
         (pcase value
           [(number? (error 'reached-predicate "expected failure")) 'unexpected]
           [else 'fallback]))
       #f))

(mat mutation-macro-argument-safety

     ;; Error case: release arithmetic must reject nonfixnum operands before changing the target.
     (parameterize ([optimize-level 3])
       (eval '(let ([invalid #f])
                (and
                  (let ([value 0])
                    (and (guard (condition [else #t]) (fx+! value invalid) #f) (= value 0)))
                  (let ([value 0])
                    (and (guard (condition [else #t]) (fx-! value invalid) #f) (= value 0)))
                  (let ([value 0])
                    (and (guard (condition [else #t]) (fx*! value invalid) #f) (= value 0)))
                  (let ([value 0])
                    (and (guard (condition [else #t]) (fx/! value invalid) #f) (= value 0)))
                  (let ([value invalid])
                    (and (guard (condition [else #t]) (fx+! value 1) #f) (eq? value invalid)))))
             (environment '(cspp chez) '(cspp utils))))

     ;; Error case: cdr! must reject a nonpair and leave the target unchanged.
     (parameterize ([optimize-level 3])
       (eval '(let* ([original '#(1 2)] [value original])
                (and (guard (condition [else #t]) (cdr! value) #f) (eq? original value)))
             (environment '(cspp chez) '(cspp utils))))

     (let ([value 5] [calls 0])
       (fx+! value (begin (set! calls (+ calls 1)) 2) (begin (set! calls (+ calls 1)) 3))
       (fx-! value 1 2)
       (fx*! value 2)
       (fx/! value 7)
       (and (= value 2) (= calls 2)))

     (let ([value '(1 2 3)]) (cdr! value) (equal? value '(2 3))))

(mat mutation-macro-arithmetic-boundaries

     ;; Error case: overflow and zero divisors must raise without changing the target in release.
     (parameterize ([optimize-level 3])
       (eval '(and
                (let ([value (most-positive-fixnum)])
                  (and (guard (condition [else #t]) (fx+! value 1) #f)
                       (= value (most-positive-fixnum))))
                (let ([value (most-negative-fixnum)])
                  (and (guard (condition [else #t]) (fx-! value 1) #f)
                       (= value (most-negative-fixnum))))
                (let ([value (most-positive-fixnum)])
                  (and (guard (condition [else #t]) (fx*! value 2) #f)
                       (= value (most-positive-fixnum))))
                (let ([value 1])
                  (and (guard (condition [else #t]) (fx/! value 0) #f) (= value 1))))
             (environment '(cspp chez) '(cspp utils)))))

(mat port-check-argument-safety

     ;; Error case: release port checks reject nonports before inspecting their native layout.
     (for-all
       (lambda (name)
         (let ([check (parameterize ([optimize-level 3])
                        (eval `(lambda (value) (,name (value) #t))
                          (environment '(cspp chez) '(cspp utils))))])
           (for-all
             (lambda (value)
               (guard (condition
                        [else (and (who-condition? condition)
                                   (eq? 'pcheck (condition-who condition)))])
                 (check value) #f))
             (list 'invalid '#() "not a port" #f 17))))
       '(pcheck-binary-port pcheck-textual-port
         pcheck-open-binary-port pcheck-open-textual-port
         pcheck-input-binary-port pcheck-input-textual-port
         pcheck-output-binary-port pcheck-output-textual-port)))

(mat flag-name-syntax-safety

     ;; Error case: nonidentifier names must fail expansion before a decoder can return nonsymbols.
     (for-all
       (lambda (name)
         (guard (condition [(syntax-violation? condition) #t])
           (eval `(let ()
                    (define-flags invalid-flags (,name 1))
                    (invalid-flags->symbols 1))
                 (environment '(cspp chez) '(cspp utils)))
           #f))
       '("read" 17 (read) #f #\r #(read)))

     ;; With no declarations, the mask is zero and the decoder has no named flags.
     (eval '(let ()
              (define-flags empty-flags)
              (and (= 0 (empty-flags)) (null? (empty-flags->symbols 1))))
           (environment '(cspp chez) '(cspp utils))))

(mat flag-mask-syntax-safety

     ;; Error case: empty, nested-empty, and improper masks must raise safe expansion errors.
     (for-all
       (lambda (mask)
         (guard (condition
                  [else (and (who-condition? condition)
                             (eq? 'flag->mask (condition-who condition)))])
           (eval `(let () (define-flags invalid-flags (bad ,mask)) (invalid-flags bad))
                 (environment '(cspp chez) '(cspp utils)))
           #f))
       '(() (or 1 ()) (or . 1)))

     ;; Error case: malformed masks passed to a generated flag macro use the same validation.
     (guard (condition
              [else (and (who-condition? condition)
                         (eq? 'flag->mask (condition-who condition)))])
       (eval '(let () (define-flags checked-flags (first 1)) (checked-flags ()))
             (environment '(cspp chez) '(cspp utils)))
       #f)

     ;; An empty or expression remains a valid zero mask.
     (eval '(let () (define-flags zero-flags (none (or))) (= 0 (zero-flags none)))
           (environment '(cspp chez) '(cspp utils))))

(mat flag-check-argument-safety

     ;; Error case: generated decoders must check fixnum arguments even at optimize level 3.
     (parameterize ([optimize-level 3])
       (eval '(let ()
                (define-flags checked-flags (first 1))
                (guard (condition [else #t]) (checked-flags->symbols 'invalid) #f))
             (environment '(cspp chez) '(cspp utils))))

     ;; Error case: flag operations reject nonfixnum masks and values in release code.
     (parameterize ([optimize-level 3])
       (eval '(let ([invalid 'invalid])
                (and (guard (condition [else #t]) (flag-any-set? invalid 1) #f)
                     (guard (condition [else #t]) (flag-all-set? 1 invalid) #f)
                     (guard (condition [else #t]) (flag-set invalid 1) #f)
                     (guard (condition [else #t]) (flag-reset 1 invalid) #f)))
             (environment '(cspp chez) '(cspp utils))))

     (let ([calls 0])
       (and (= 3 (flag-set (begin (set! calls (+ calls 1)) 1)
                          (begin (set! calls (+ calls 1)) 2)))
            (= 2 calls))))

(mat natural-contract

     (and (natural? 0) (natural? (+ 1 (most-positive-fixnum)))
          (positive-natural? 1) (not (positive-natural? 0)))

     ;; Error case: integer-valued inexact numbers are not natural numbers.
     (and (not (natural? 1.0)) (not (positive-natural? 1.0)))

     (and (not (natural? -1)) (not (natural? 1/2)) (not (natural? 'one))))

(mat random-generator-contracts

     (string=? "aaa" (random-string (lambda () #\a) 3 4))

     (equal? '(item item) (random-list (lambda () 'item) 2 3))

     (eq? 'item (unbox (random-box (lambda () 'item))))

     ;; Error case: even a zero-length result must reject a nonprocedure generator.
     (guard (c [else #t]) (random-string #f 0 1) #f)

     ;; Error case: boxed values require a callable generator.
     (guard (c [else #t]) (random-box #f) #f)

     ;; Error case: the character callback must return a character before string mutation.
     (guard (c [else #t]) (random-string (lambda () "text") 1 2) #f)

     ;; Error case: lengths must fit the fixnum arithmetic used by the generators.
     (let ([too-large (+ 1 (most-positive-fixnum))])
       (and (guard (c [else #t]) (random-string too-large (+ too-large 1)) #f)
            (guard (c [else #t]) (random-list too-large (+ too-large 1)) #f)
            (guard (c [else #t]) (random-bytevector too-large (+ too-large 1)) #f)))

     ;; Error case: reject inexact bounds before unsafe fixnum operations.
     (guard (c [else #t]) (random-string 1.0 2.0) #f)

     ;; Error case: symbol bounds must be fixnums before the callback is invoked.
     (let ([called? #f] [bound (+ 1 (most-positive-fixnum))])
       (and (guard (c [else #t])
              (random-symbol (lambda () (set! called? #t) "name") bound (+ bound 1))
              #f)
            (not called?)))

     ;; Error case: lower bounds must be nonnegative and strictly below upper bounds.
     (and (guard (c [else #t]) (random-list -1 2) #f)
          (guard (c [else #t]) (random-bytevector 2 2) #f))

     (and (= 0 (bytevector-length (random-bytevector 0 1)))
          (= 2 (bytevector-length (random-u8vec 2 3)))))

(mat random-symbol-contract

     (let ([calls 0])
       (and (eq? 'name (random-symbol (lambda () (set! calls (+ calls 1)) "name") 1 10))
            (= calls 1)))

     ;; Error case: the symbol callback must return a string.
     (guard (c [else #t]) (random-symbol (lambda () #\a) 1 2) #f)

     ;; Error case: the exclusive upper bound is enforced without retrying the callback.
     (let ([calls 0])
       (and (guard (c [else #t])
              (random-symbol (lambda () (set! calls (+ calls 1)) "abc") 1 3)
              #f)
            (= calls 1))))

(mat random-unicode-scalars

     ;; Error case: a range crossing the surrogate gap must only yield scalar values.
     (guard (c [else #f])
       (let loop ([remaining 100])
         (or (= remaining 0)
             (let ([value (char->integer (random-char #\xD7FF #\xE001))])
               (and (memv value '(#xD7FF #xE000))
                    (loop (- remaining 1))))))))

(mat procedure-arity-contract

     ;; Error case: a procedure with no clauses accepts no arity and must terminate.
     ((make-engine (lambda () (null? (procedure-arity (case-lambda)))))
      100000 (lambda (ticks result) result) (lambda (unfinished) #f))

     (= 2 (procedure-arity (lambda (first second) first)))

     (equal? '(0 2) (procedure-arity (case-lambda [() #f] [(first second) first])))

     (equal? '(1 3 *) (procedure-arity (case-lambda [(one) one] [(a b c . rest) rest])))

     (equal? '(0 2 4 *)
             (procedure-arity
               (case-lambda [() #t] [(a b) #t] [(a b c d . rest) #t])))

     (equal? '(*) (procedure-arity (lambda arguments arguments)))

     (and (procedure-variable-arity? (lambda arguments arguments))
          (not (procedure-variable-arity? (case-lambda)))))

(mat predicate-combinators

     (let ([visited '()])
       (and (not ((p/and (lambda (value) (set! visited (cons 'first visited)) #f)
                         (lambda (value) (set! visited (cons 'second visited)) #t)) 'object))
            (equal? '(first) visited)))

     (eq? 'accepted ((p/or (lambda (value) #f) (lambda (value) 'accepted)) 0))

     (and ((p/and) 'object) (not ((p/or) 'object)) ((p/not number?) 'object))

     ;; Error case: combinators reject nonprocedure callbacks before returning a predicate.
     (and (guard (c [else #t]) (p/and number? #f) #f)
          (guard (c [else #t]) (p/or #f) #f)))

(define-flags review-flags (none 0) (first 1) (second 2) (both (or first second)) (alias 1))

(mat flag-overlap-contract

     (equal? '(first both alias) (review-flags->symbols 1))

     (null? (review-flags->symbols 0))

     (and (flag-any-set? 3 1) (not (flag-all-set? 3 1))
          (= 3 (flag-set 2 1)) (= 1 (flag-reset 2 3))))

(mat flag-wrapper-hygiene

     ;; Error case: a wrapper must expose the decoder using the caller-supplied name's context.
     (guard (failure [else #f])
       (eval
         '(let ()
            (define-syntax make-flags
              (syntax-rules ()
                [(_ name)
                 (define-flags name (read 1) (write 2) (both (or read write)))]))
            (make-flags permissions)
            (and (= 1 (permissions read))
                 (= 3 (permissions both))
                 (equal? '(read write both) (permissions->symbols 3))))
         (environment '(cspp chez) '(cspp utils))))

     ;; Error case: nested wrappers must bind a decoder that shadows the surrounding identifier.
     (guard (failure [else #f])
       (eval
         '(let ([permissions->symbols 'outer])
            (let ()
              (define-syntax make-flags
                (syntax-rules () [(_ name) (define-flags name (read 1) (write 2))]))
              (define-syntax make-permissions
                (syntax-rules () [(_ name) (make-flags name)]))
              (make-permissions permissions)
              (and (= 3 (permissions read write))
                   (equal? '(read write) (permissions->symbols 3)))))
         (environment '(cspp chez) '(cspp utils)))))
