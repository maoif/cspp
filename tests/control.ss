(import (cspp chez) (cspp control) (cspp utils) (cspp list) (cspp string) (cspp vector))

(mat control-error-origins

     ;; Error case: callback validation identifies the public composition or section form.
     (for-all
       (lambda (entry)
         (guard (condition
                  [else (and (who-condition? condition)
                             (eq? (car entry) (condition-who condition)))])
           ((cdr entry)) #f))
       (list (cons 'compose (lambda () (compose #f)))
             (cons 'compose1 (lambda () (compose1 values #f)))
             (cons '>>> (lambda () (>>> 1 #f)))
             (cons '>>>1 (lambda () (>>>1 1 values #f)))
             (cons 'sect (lambda () (sect #f _)))
             (cons 'sect+ (lambda () (sect+ #f _))))))

(mat section-procedure-contract

     ;; Error case: reject a nonprocedure while constructing a section, before it is called.
     (guard (condition [else #t]) (sect #f _) #f)

     ;; Error case: curried sections must check the captured callback before accepting slots.
     (guard (condition [else #t]) (sect+ #f _ _) #f))


(mat compose

     ;; not proc
     (error? (compose 1))
     (error? (compose 1 #\A))
     (error? (compose ""))
     (error? (compose zip list 'a))


     ;; wrong #ret val
     (error? ((compose sub1 (lambda (x) (values x x)) random) 100))
     (error? ((compose
               list
               (lambda (x y a) (values b a y))
               (lambda (x y) (values (* x x) (* y y) x y))
               (lambda (x y) (values (add1 x) (add1 y))))
              1 2))

     (eq? cons (compose cons))
     (eq? list (compose list))

     (equal? ((compose vreverse list->vector string->list) "123456")
             '#(#\6 #\5 #\4 #\3 #\2 #\1))
     (equal? ((compose
               list
               (lambda (x y a b) (values b a y x))
               (lambda (x y) (values (* x x) (* y y) x y))
               (lambda (x y) (values (add1 x) (add1 y))))
              1 2)
             '(3 2 9 4))
     )


(mat compose1

     ;; not proc
     (error? (compose1 1))
     (error? (compose1 1 #\A))
     (error? (compose1 ""))
     (error? (compose1 zip list 'a))

     ;; wrong #ret val
     (error? ((compose1 sub1 (lambda (x) (values x x)) random) 100))
     (error? ((compose1
               list
               (lambda (x y a) (values b a y))
               (lambda (x y) (values (* x x) (* y y) x y))
               (lambda (x y) (values (add1 x) (add1 y))))
              1 2))

     (eq? cons (compose1 cons))
     (eq? list (compose1 list))

     (equal? ((compose1 vreverse list->vector string->list) "123456")
             '#(#\6 #\5 #\4 #\3 #\2 #\1))

     (equal? '(5 6)
             (call-with-values
               (lambda () ((compose1 (lambda (value) (values value (+ value 1))) +) 2 3))
               list))
     )


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

     (error? (>>> 1))
     (error? (>>> 1 #\A))
     (error? (>>> "" 'a))
     (error? (>>> zip list 'a))

     ;; wrong arity
     (error? (>>> 10 random (lambda (x) (values x x)) sub1))

     (fx= 2 (>>> 1 (lambda (x) (values x x)) +))
     (equal? (>>> "123456" string->list list->vector vreverse)
             ((compose1 vreverse list->vector string->list) "123456"))
     )


(mat >>>1

     (error? (>>>1 1))
     (error? (>>>1 1 #\A))
     (error? (>>>1 "" 'a))
     (error? (>>>1 zip list 'a))

     ;; wrong arity
     (error? (>>>1 1 (lambda (x) (values x x)) +))

     (equal? (>>>1 "123456" string->list list->vector vreverse)
             ((compose1 vreverse list->vector string->list) "123456"))
     )


(mat sect

     (eq? (let ([add2 (sect + 2 _)])
            (add2 2))
          4)

     (eq? (let ([add2 (sect + _ 2)])
            (add2 2))
          4)

     (eq? (let ([add2* (sect + _ 2 ...)])
            (add2* 2 2 2))
          8)

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(sect + ... _))))
      "... must appear last")

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(sect + _ ... _))))
      "... must appear last")

     )


(mat sect+

     (eq? (let* ([raise (sect+ expt _ _)]
                 [^2 (raise 2)])
            (^2 2))
          4)

     (eq? (let ([f (sect+ + _ _ ...)])
            (((f 1) 1) 1 1 1))
          5)

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(sect+ + ... _))))
      "... must appear last")

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(sect+ + _ ... _))))
      "... must appear last")

     )

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
