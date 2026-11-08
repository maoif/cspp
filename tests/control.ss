(import (cspp chez) (cspp control) (cspp utils) (cspp string) (cspp vector))

(mat control-error-origins

     ;; Error case: callback validation identifies the public composition or section form.
     (for-all
       (lambda (entry)
         (guard (condition
                  [else (and (who-condition? condition)
                             (eq? (car entry) (condition-who condition)))])
           ((cdr entry)) #f))
       (list (cons 'compose (lambda () (compose #f)))
             (cons 'compose (lambda () (compose values #f)))
             (cons 'compose1 (lambda () (compose1 #f)))
             (cons 'compose1 (lambda () (compose1 values #f)))
             (cons '>>> (lambda () (>>> 1 #f)))
             (cons '>>> (lambda () (>>> 1 values #f)))
             (cons '>>>1 (lambda () (>>>1 1 #f)))
             (cons '>>>1 (lambda () (>>>1 1 values #f)))
             (cons 'sect (lambda () (sect #f _)))
             (cons 'sect+ (lambda () (sect+ #f _ _))))))

(mat compose

     ;; Error case: a callback cannot consume the returned values.
     (error? ((compose sub1 (lambda () (values 1 2)))))

     (eq? cons (compose cons))

     (equal? ((compose vreverse list->vector string->list) "123456")
             '#(#\6 #\5 #\4 #\3 #\2 #\1))
     (equal? ((compose
               list
               (lambda (x y a b) (values b a y x))
               (lambda (x y) (values (* x x) (* y y) x y))
               (lambda (x y) (values (add1 x) (add1 y))))
              1 2)
             '(3 2 9 4)))

(mat compose1

     ;; Error case: both the direct and longer-chain paths require one intermediate value.
     (for-all
       (lambda (count)
         (guard (failure [else #t])
           ((apply compose1
              (append (make-list (- count 1) sub1) (list (lambda () (values 1 2))))))
           #f))
       '(2 3))

     (eq? cons (compose1 cons))

     (equal? ((compose1 vreverse list->vector string->list) "123456")
             '#(#\6 #\5 #\4 #\3 #\2 #\1)))

(mat compose1-chain-contract

     (for-all
       (lambda (count)
         (let ([events '()])
           (let ([composed
                   (apply compose1
                     (append
                       (list (lambda (value)
                               (set! events (cons 'left events))
                               (values value 'done)))
                       (make-list (- count 2)
                         (lambda (value)
                           (set! events (cons 'middle events))
                           (+ value 1)))
                       (list (lambda (first second)
                               (set! events (cons 'right events))
                               (+ first second)))))])
             (and (equal? (list (+ count 3) 'done)
                          (call-with-values (lambda () (composed 2 3)) list))
                  (equal? (append '(right) (make-list (- count 2) 'middle) '(left))
                          (reverse events))))))
       '(2 3 10))

     (for-all
       (lambda (count)
         (null? (call-with-values
                  (lambda ()
                    ((apply compose1
                       (append (list (lambda (value) (values)))
                               (make-list (- count 2) add1)
                               (list (lambda () 0))))))
                  list)))
       '(2 3 10)))

(mat compose1-continuation-replay

     ;; call/cc intentionally replays a callback continuation after the composition returned.
     (for-all
       (lambda (count)
         (let ([saved #f] [replayed? #f] [middle-calls 0])
           (let ([composed
                   (apply compose1
                     (append
                       (list (lambda (value) (values value middle-calls)))
                       (make-list (- count 2)
                         (lambda (value)
                           (set! middle-calls (+ middle-calls 1))
                           (+ value 1)))
                       (list (lambda (first second)
                               (call/cc
                                 (lambda (continuation)
                                   (set! saved continuation)
                                   (+ first second)))))))])
             (let-values ([(value calls) (composed 2 3)])
               (if replayed?
                   (and (= value (+ count 8)) (= calls (* 2 (- count 2))))
                   (begin (set! replayed? #t) (saved 10)))))))
       '(2 3 10)))

(mat compose1-deep-chain-space

     ;; Error case: deep compositions must not allocate a continuation stack per callback.
     (let ([composed (apply compose1 (make-list 100000 add1))])
       (collect)
       (let* ([before (statistics)]
              [result (composed 0)]
              [bytes (sstats-bytes (sstats-difference (statistics) before))])
         (and (= result 100000) (< bytes 262144)))))

(mat >>>

     ;; Error case: a pipeline requires at least one callback.
     (error? (>>> 1))

     ;; Error case: intermediate values must match the next callback's arity.
     (error? (>>> 10 (lambda (value) (values value value)) sub1))

     (fx= 2 (>>> 1 (lambda (x) (values x x)) +))
     (equal? (>>> "123456" string->list list->vector vreverse)
             '#(#\6 #\5 #\4 #\3 #\2 #\1)))

(mat >>>1

     ;; Error case: a pipeline requires at least one callback.
     (error? (>>>1 1))

     ;; Error case: intermediate values must match the next callback's arity.
     (error? (>>>1 1 (lambda (x) (values x x)) +))

     (equal? (>>>1 "123456" string->list list->vector vreverse)
             '#(#\6 #\5 #\4 #\3 #\2 #\1)))

(mat sect

     (eq? (let ([add2 (sect + 2 _)])
            (add2 2))
          4)

     (eq? (let ([add2 (sect + _ 2)])
            (add2 2))
          4)

     (eq? (let ([add2* (sect + _ 2 ...)])
            (add2* 2 2 2))
          8))

(mat sect+

     (eq? (let* ([raise (sect+ expt _ _)]
                 [^2 (raise 2)])
            (^2 2))
          4)

     (eq? (let ([f (sect+ + _ _ ...)])
            (((f 1) 1) 1 1 1))
          5))

(mat section-syntax-errors

     ;; Error case: ellipses must be last, with or without an earlier placeholder.
     (for-all
       (lambda (form)
         (guard (failure
                  [(syntax-violation? failure)
                   (string-contains? (condition-message failure) "... must appear last")]
                  [else #f])
           (expand form)
           #f))
       '((sect + ... _) (sect + _ ... _) (sect+ + ... _) (sect+ + _ ... _))))

(mat forever-and-continuations

     (= 4 (forever (index count) (when (= count 4) (break count))))

     (let ([count 0])
       (= 3 (forever (set! count (+ count 1)) (when (= count 3) (break count)))))

     (equal? '(one two)
             (call-with-values
               (lambda () (let/1cc escape (escape 'one 'two))) list))

     ;; call/cc intentionally captures an escape continuation for this control form.
     (= 7 (let/cc escape (+ 1 (escape 7)))))

(mat section-capture-and-zero-slots

     (null? ((sect list)))

     (null? (call-with-values (sect values) list))

     (let ([calls 0])
       (let ([result (sect+ (lambda () (set! calls (+ calls 1)) 'done))])
         (and (eq? 'done result) (= calls 1))))

     (let* ([value 2] [add (sect + value _)])
       (set! value 20)
       (= 5 (add 3)))

     (let ([calls 0])
       (let ([result (sect+ (lambda (value) (set! calls (+ calls 1)) value) 8)])
         (and (= result 8) (= calls 1))))

     (let ([captures 0])
       (let ([section (sect list _ (begin (set! captures (+ captures 1)) 'fixed) _ ...)])
         (and (equal? '(a fixed b c) (section 'a 'b 'c))
              (equal? '(x fixed y) (section 'x 'y))
              (= captures 1))))

     (let ([captures 0])
       (let ([section (sect+ list _ (begin (set! captures (+ captures 1)) 'fixed) _ ...)])
         (and (equal? '(a fixed b c) (((section 'a) 'b) 'c))
              (equal? '(x fixed y) (((section 'x) 'y)))
              (= captures 1))))

     (equal? '(fixed a b) ((sect+ list 'fixed ...) 'a 'b))

     (equal? '(left right) (call-with-values (lambda () ((sect values _ _) 'left 'right)) list)))
