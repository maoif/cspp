(import (cspp chez) (cspp utils))

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

     ;; Error case: nonports must fail the predicate check before inspecting port state.
     (let ([invalid 17])
       (andmap
         (lambda (thunk)
           (guard (condition [else (and (who-condition? condition)
                                       (eq? 'pcheck (condition-who condition)))])
             (thunk) #f))
         (list (lambda () (pcheck-input-binary-port (invalid) #t))
               (lambda () (pcheck-input-textual-port (invalid) #t))
               (lambda () (pcheck-output-binary-port (invalid) #t))
               (lambda () (pcheck-output-textual-port (invalid) #t))))))

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

     ;; Error case: even a zero-length list must validate its generator.
     (guard (c [else #t]) (random-list #f 0 1) #f)

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

     ;; Error case: out-of-range names must fail without repeatedly calling the callback.
     (let ([calls 0])
       (and (guard (c [else #t])
              (random-symbol (lambda () (set! calls (+ calls 1)) "long") 1 3)
              #f)
            (= calls 1)))

     ;; Error case: upper bounds are exclusive for generated symbol names.
     (guard (c [else #t]) (random-symbol (lambda () "abc") 1 3) #f))

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
          (guard (c [else #t]) (p/or #f) #f)
          (guard (c [else #t]) (p/not #f) #f)))

(define-flags review-flags (none 0) (first 1) (second 2) (both (or first second)) (alias 1))

(mat flag-overlap-contract

     (equal? '(first both alias) (review-flags->symbols 1))

     (null? (review-flags->symbols 0))

     (and (flag-any-set? 3 1) (not (flag-all-set? 3 1))
          (= 3 (flag-set 2 1)) (= 1 (flag-reset 2 3))))
