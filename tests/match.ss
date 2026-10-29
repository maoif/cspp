(import (cspp) (cspp private match) (only (cspp irregex) irregex))

(define-match-expander tagged-expander
  (lambda (pp rho expr-id pat body fk)
    (syntax-case pat ()
      [(_ ignored sub)
       (pp rho expr-id #'sub body fk)])))

(mat custom-expander-captures-only-subpatterns
     ;; The ignored selector is an expression, so it must not become a repeated capture.
     (equal? '((10 20) 99)
             (let ([outer 99])
               (match '(10 20)
                 [(,(tagged ,outer ,x) ...) (list x outer)]))))

(mat reserved-extension-capture-dispatch
     ;; Built-in heads stay reserved during both capture discovery and matcher generation.
     (let ()
       (define-match-expander hash-expander (lambda (pp rho expr pat body fk) body))
       (let ([table (make-hashtable string-hash string=?)])
         (hashtable-set! table "key" 42)
         (equal? '(42)
                 (match (list table) [(,(hash ("key" ,x)) ...) x]))))

     (let ()
       (define-match-expander box-expander (lambda (pp rho expr pat body fk) body))
       (equal? '(10 20) (match '#(#&10 #&20) [#(,(box ,x) ...) x])))

     (let ()
       (define-match-expander regex-expander (lambda (pp rho expr pat body fk) body))
       (equal? '("a" "b") (match '("a" "b") [(,(regex "." (0 ,x)) ...) x])))

     (let ()
       (record hash (value))
       (let ([table (make-hashtable string-hash string=?)])
         (hashtable-set! table "key" 42)
         (equal? '(42) (match (list table) [(,(hash ("key" ,x)) ...) x])))))


(mat else-clause

     (match 1
       [else #t])

     )


;;;; simple list

(mat simple-list
     (match '()
       [() #t]
       [else #f])

     (match '()
       [() #t])

     (equal? 1
             (match '(1 . 2)
               [(,a . ,b) a]))
     (equal? 2
             (match '(1 . 2)
               [(,a . ,b) b]))
     (match '(1 . 2)
       [(,a . ,b) #t])

     (match '(#t . #f)
       [(,t . ,f) t])

     (not
      (match '(#t . #f)
        [(,t . ,f) f]))

     (equal? 2
             (match '(1 2 3)
               [(_ ,x _) x]))

     (equal? '(2 . 22)
             (match '(1 (2 . 22) 3)
               [(_ ,x  _) x]))

     (equal? 2
             (match '(1 2 3)
               [() #f]
               [(,a _) #f]
               [(_ ,x _) x]))

     (match '(1 2 3)
       [() #f]
       [(,a _) #f]
       [(_ ,x _ _) x]
       [else #t])

     ;; TODO guards

     )




;;;; list with ellipses
(mat list-with-ellipses-in-the-end
     (match '(1)
       [(,a ...) #t]
       [else #f])

     (equal? '(1 2 3)
             (match '(1 2 3)
               [(,a ...) a]
               [else #f]))

     (equal? '(2 3)
             (match '(1 2 3)
               [(,a ,b ...) b]
               [else #f]))

     (equal? '(1 2 3 4)
             (match '((1 11) (2 22) (3 33) (4 44))
               [((,a ,b) ...) a]))

     (equal? '(11 22 33 44)
             (match '((1 11) (2 22) (3 33) (4 44))
               [((,a ,b) ...) b]))

     (equal? '((1 11) (2 22) (3 33) (4 44))
             (match '((1 11) (2 22) (3 33) (4 44))
               [((,a ...) ...) a]))

     ;; before commit 19bb7a4 (when i == #e1e7):
     ;;  479 collections
     ;;  6.629166675s elapsed cpu time, including 5.552937423s collecting
     ;;  6.637919414s elapsed real time, including 5.561474874s collecting
     ;;  4067993680 bytes allocated, including 2090397616 bytes reclaimed
     ;; after:
     ;;  250 collections
     ;;  4.653256847s elapsed cpu time, including 3.708845426s collecting
     ;;  4.661777688s elapsed real time, including 3.716243300s collecting
     ;;  2147844608 bytes allocated, including 1000091024 bytes reclaimed
     (let ([lb (make-list-builder)])
       (for ([i #e1e5])
         (lb `(((,i) ,i ,i) (,i (,i) ,i) (,i (,i) ,i) . ((,i) (,i) (,i)))))
       (match (lb)
         [((((,a) ,b ,c) (,d (,e) ,f) (,g (,h) ,i) . ((,j) (,k) (,l)))
           ...)
          (and (equal? a d)
               (equal? g j)
               (equal? h k))]
         [else #f]))

     (match '(json (object
                    (items
                     (array
                      (object (name "pen")
                              (price 3.5))
                      (object (name "notebook")
                              (price 12.0))
                      (object (name "apple")
                              (price 1.0))
                      100
                      #f))))
       [(json (object
               (items
                (array
                 (object (name ,names)
                         (price ,prices))
                 ...
                 100
                 #f))))
        (equal? '(("pen" "notebook" "apple") (3.5 12.0 1.0))
                (list names prices))])

     )

(mat list-with-ellipses-in-the-middle

     (equal? '(4 44)
             (match '((1 11) (2 22) (3 33) (4 44))
               [((,a ...) ... ,b) b]))

     ;; full match after ...
     (equal? '()
             (match '((1 11) (2 22) (3 33) (4 44))
               [((,a ...) ... ,b ,c ,d ,e) a]))

     )

;;;; vector

(mat simple-vector

     (match '#()
       [#() #t]
       [else #f])

     (= 1 (match '#(1)
            [#(,a) a]
            [else #f]))

     (equal? (list 3 2 1)
             (match '#(1 2 3)
               [#(,a ,b ,c) (list c b a)]
               [else #f]))

     ;;nested

     (equal? (list "bad" "good" 3)
             (match '#(1 ("good" "bad") 3)
               [#() 'none]
               [#(,a _) #f]
               [#(_ (,a ,b) ,c) (list b a c)]
               [else #f]))

     (equal? '(11 22 33 44)
             (match '#(1 ((11 22) (33 44)) 3)
               [#(_ ((,a ,b) (,x ,y)) _) (list a b x y)]
               [else #f]))


     )


(mat vector-with-ellipses-in-the-end

     (equal? '()
             (match '#()
               [#(,x ...) x]))

     (equal? '((33 333) (22 222) (11 111))
             (match '#(#(1 2 3) #(11 22 33) #(111 222 333))
               [#(_ #(,a ,b ,c) ...) (list c b a)]))

     (equal? '((22 33) (222 333))
             (match '#(#(1 2 3) #(11 22 33) #(111 222 333))
               [#(_ #(,a ,b ...) ...) b]))

     (equal? '((1 4 #()) (2 5 #()) (3 6 #()))
             (match '(#(1 2 3) #(4 5 6) #(#() #() #()))
               [(#(,a 3 _)) #f]
               [(#(,a ,b ,c) ...)
                (list a b c)]))

     )


(mat vector-with-ellipses-in-the-middle

     (equal? '()
             (match '#(1 2)
               [#(,x ... 1 2) x]))

     (equal? '((22 33) (222 333))
             (match '#(#(1 2 3) #(11 22 33) #(111 222 333) #(1111 2222 3333 4444))
               [#(_ #(,a ,b ...) ... ,c) b]))

     (equal? '#(1111 2222 3333 4444)
             (match '#(#(1 2 3) #(11 22 33) #(111 222 333) #(1111 2222 3333 4444))
               [#(_ #(,a ,b ...) ... ,c) c]))

     (equal?
      '(((a a) (a a) (a a) (a a))
        (((1 2) (1 2)) ((1 2) (1 2)) ((1 2) (1 2)) ((1 2) (1 2)))
        ((3 3) (3 3) (3 3) (3 3)))
      (match '#((#(a (1 2 3) c) #(a (1 2 3) c) #(a (1 2 3) c)) (#(a (1 2 3) c) #(a (1 2 3) c) #(a (1 2 3) c))
                (#(a (1 2 3) c) #(a (1 2 3) c) #(a (1 2 3) c)) (#(a (1 2 3) c) #(a (1 2 3) c) #(a (1 2 3) c)))
        [#((#(,x (,y ... ,z) _) ... _) ...)
         (list x y z)]))

     )


(mat ellipsis-capture-order-and-fallback

     (equal? '((1 2 1) (11 22 11) (3))
             (match '#(#(1 11) #(2 22) #(1 11) end 3)
               [#(#(,x ,y) ... end ,z ...) (list x y z)]))

     (equal? '((1 2 1) (11 22 11) (3))
             (match '((1 11) (2 22) (1 11) end 3)
               [((,x ,y) ... end ,z ...) (list x y z)]))

     ;; A failed repeated vector subpattern must discard its partial captures.
     (equal? '(#(1 11) bad end)
             (match '#(#(1 11) bad end)
               [#(#(,x ,y) ... end) #f]
               [#(,all ...) all]))

     ;; A failed repeated list subpattern must discard its partial captures.
     (equal? '((1 11) bad end)
             (match '((1 11) bad end)
               [((,x ,y) ... end) #f]
               [(,all ...) all]))

     )


(mat simple-box

     (equal? 1
             (match (box 1)
               [,(box ,a) a]))

     (equal? (box 222)
             (match (box (box 222))
               [,(box ,a) a]))

     (equal? 222
             (match (box (box 222))
               [,(box ,(box ,a)) a]))


     )


(mat mlambda

     (begin
       (define fact
         (mlambda
          [0 1]
          [,n (* n (fact (sub1 n)))]))
       (define rev
         (mlambda
          [() '()]
          [(,a) `(,a)]
          [(,a ,b . ,ls) (append (rev ls) (list b a))]))
       #t)

     (= 1 (fact 0))
     (= 6 (fact 3))
     (= 5040 (fact 7))

     (equal? (reverse '()) (rev '()))
     (equal? (reverse '(1)) (rev '(1)))
     (equal? (reverse (iota 10)) (rev (iota 10)))

     )


(mat mlambda+

     (begin
       ;; (snoc 2 '(1)) => '(1 2)
       (define snoc
         (mlambda+
          [(,x ())
           `(,x)]
          [(,x (,a . ,b))
           (cons a (snoc x b))]))
       #t)

     (equal? '(0)     (snoc 0 '()))
     (equal? '(1 0)   (snoc 0 '(1)))
     (equal? '(1 2 0) (snoc 0 '(1 2)))


     )


(mat mlet

     (equal?
      '((1 2 3) (1 4 9) (name age) ("jack" 16) box)
      (mlet ([((,x . ,y) ...) '((1 . 1) (2 . 4) (3 . 9))]
             [((,head ,body) ...) '((name "jack") (age 16))]
             [,(box ,b) (box 'box)])
            (list x y head body b)))

     )


(mat mlet-independent-patterns
     ;; An ellipsis in a separate binding remains a literal symbol.
     (equal? 1 (mlet ([,x 1] [... '...]) x))

     ;; A mismatched literal must not become a repetition of the preceding binding.
     (error? (mlet ([,x 1] [... 'wrong]) x))

     ;; Adjacent patterns must not form an extension at expansion time.
     (guard (condition [else #f])
       (= 42 (eval '(mlet ([unquote 'unquote] [,x 42]) x) (environment '(cspp)))))

     ;; Literal patterns must reject unrelated values rather than form a capture.
     (error? (mlet ([unquote 1] [x 2]) 'accepted))

     ;; A literal else pattern must not become a generated fallback clause.
     (guard (condition [else #f])
       (= 42 (eval '(mlet ([else 'else]) 42) (environment '(cspp)))))

     ;; A mismatched literal else must fail at runtime, rather than fail to expand.
     (guard (condition [(syntax-violation? condition) #f] [else #t])
       (eval '(mlet ([else 'wrong]) 'accepted) (environment '(cspp)))
       #f)

     ;; A literal quasisyntax list must not turn into a whole-value capture.
     (let ([x 'outer])
       (equal? '(outer 10 20)
               (mlet ([#`x '(quasisyntax x)] [(,a ,b) '(10 20)]) (list x a b))))

     ;; Literal pattern data must not become a catamorphism that invokes project.
     (let ([calls 0])
       (let ([project (lambda (input) (set! calls (+ calls 1)) 'unexpected)])
         (and (equal? 42
                      (mlet ([unquote-splicing 'unquote-splicing]
                             [(project -> ,x) '(project -> 42)]) x))
              (= calls 0)))))

(mat mlet-parallel-evaluation
     ;; Every right-hand side sees outer bindings and runs exactly once.
     (let ([x 40] [calls 0])
       (and (equal? '(1 42)
                    (mlet ([,x (begin (set! calls (+ calls 1)) 1)]
                           [,y (begin (set! calls (+ calls 1)) (+ x 2))])
                      (list x y)))
            (= calls 2)))

     ;; All right-hand sides run before a failed pattern can leave the binding form.
     (let ([calls 0])
       (guard (condition [else (= calls 2)])
         (mlet ([expected (begin (set! calls (+ calls 1)) 'wrong)]
                [,x (begin (set! calls (+ calls 1)) 42)]) x)
         #f))

     ;; Later patterns can still refer to earlier captures.
     (equal? '(1 2) (mlet ([,x '(1 2)] [`x '(1 2)]) x))

     ;; Captures may shadow syntax names without hiding generated macro bindings.
     (equal? '(1 2 3 4)
             (mlet ([,match 1] [,let 2] [,if 3] [,value 4]) (list match let if value))))

(mat mlet-pattern-binding-environment
     ;; A capture must not hide the record declaration selected by another pattern.
     (guard (condition [else #f])
       (= 42 (eval '(let ()
                      (record Point (x))
                      (mlet ([,Point 99] [,(record Point :values ,x) (Point 42)]) x))
                    (environment '(cspp)))))

     ;; Custom-expander lookup also uses the original pattern environment.
     (guard (condition [else #f])
       (= 42 (eval '(let ()
                      (define-match-expander identity-expander
                        (lambda (pp rho input pattern body failure)
                          (syntax-case pattern ()
                            [(_ subpattern) (pp rho input #'subpattern body failure)])))
                      (mlet ([,identity-expander 99] [,(identity ,x) 42]) x))
                    (environment '(cspp) '(cspp private match)))))

     ;; Datatype variant metadata must survive a capture with the variant's name.
     (guard (condition [else #f])
       (= 42 (eval '(let ()
                      (datatype Tree [Leaf value])
                      (mlet ([,Leaf 99] [,(datatype Tree Leaf :values ,x) (Leaf 42)]) x))
                    (environment '(cspp))))))

(mat mlet*

     (equal?
      (mlet* ([(,a ,b) '(1 2)]
              [(,x ,y ...) `(22 33 ,a ,b)])
             (list a b x y))
      '(1 2 22 (33 1 2)))

     )


(mat regex

     (equal? '("ababab" "ab" "ab" "ab")
             (match "ababababab"
               [,(regex "(ab)(ab)(ab)" (0 ,all) (1 ,x1) (2 ,x2) (3 ,x3))
                (list all x1 x2 x3)]))

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(match "ababababab"
                    [,(regex "(ab)(ab)(ab)" (0 ,all) (1 ,x1)
                             (-2 ,x2) ;; error
                             (3 ,x3) (4 ,x4))
                     (list all x1 x2 x3 x4)]))))
      "invalid regex pattern:")

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(match "ababababab"
                    [,(regex "(ab)(ab)(ab)"
                             (0 (,a ,b)) ;; error
                             (1 ,x1) (2 ,x2) (3 ,x3) (4 ,x4))
                     (list all x1 x2 x3 x4)]))))
      "only one pattern variable is allowed in regex pattern:")

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (match "ababababab"
           [,(regex "(ab)(ab)(ab)" (0 ,all) (1 ,x1) (2 ,x2) (3 ,x3) (4 ,x4))
            (list all x1 x2 x3 x4)])))
      "invalid index number or name for regex")

     (equal? '("jack" "male" "16" "jack-16-male")
             (match "jack-16-male"
               [,(regex '(: (=> name (+ any)) "-" (=> age (+ any)) "-" (=> sex (+ any)))
                        (0 ,all)
                        (name ,n) (sex ,s) (age ,a))
                (list n s a all)]))

     )


(mat non-linearity

     (match '(a b c c b a)
       [(,x ,y ,z `z `y `x) #t]
       [else #f])


     (let ([x 42])
       (match (list 42 '(a b) x '(a b))
         [(`x ,y `x `y) #t]
         [else #f]))


     )


(mat named-ref

     (match '(1 ((a b) (c d) (e f)) 2)
       [(1 [#`blob (_ (,x ,y) _)] 2)
        (equal? (list blob x y)
                '(((a b) (c d) (e f))
                  c d))])

     (match '((1 2) (1 2) (1 2) (1 2))
       [([#`p (,x ,y)] ...)
        (and (equal? p '((1 2) (1 2) (1 2) (1 2)))
             (equal? x '(1 1 1 1))
             (equal? y '(2 2 2 2)))])

     (match `(1 . 2)
       [[#`p (,x . ,y)]
        (equal? p (cons x y))])

     ;; ellipsis

     (match '((a b) (c d) (e f))
       [((#`l (,x ,y)) ...)
        (equal? l
                (zip x y))])

     (let ([ls '(((a) (a) (a) (a) (a))
                 ((b) (b) (b) (b) (b))
                 ((c) (c) (c) (c) (c)))])
       (match ls
         [(((#`ref (,x)) ...) ...)
          (and (equal? ref ls)
               (equal? x '((a a a a a) (b b b b b) (c c c c c))))]))

     )


;;;; record



;;;; datatype




;;;; catamorphism

(mat catamorphism

     (begin (define (cata-list-map f ls)
              (match ls
                [()
                 '()]
                [(,head . ,@tail)
                 (cons (f head) tail)]))
            (define (cata-list-snoc! ls x)
              (match ls
                [()
                 (list x)]
                [(,head . ,@[,tail])
                 (cons head tail)]))
            #t)

     (equal? '()
             (cata-list-map add1 '()))
     (equal? (map add1 (iota 10))
             (cata-list-map add1 (iota 10)))
     (equal? (map add1 (iota 100))
             (cata-list-map add1 (iota 100)))

     (equal? '(1)
             (cata-list-snoc! '() 1))
     (equal? (snoc! (iota 10) #t)
             (cata-list-snoc! (iota 10) #t))
     (equal? (snoc! (iota 100) #t)
             (cata-list-snoc! (iota 100) #t))


     (begin (define (tree-count T)
              (match T
                [()
                 0]
                [(,@L ,@R ,v)
                 (+ L R 1)]))
            ;; effect only
            (define tree-flatten
              (lambda (T)
                (let ([lb (make-list-builder)])
                  (match T
                    [()
                     (void)]
                    [(,@[] ,@[] ,v)
                     (lb v)])
                  (lb))))
            #t)

     (equal? 0
             (tree-count '()))
     (equal? 1
             (tree-count '(() () 1)))
     (equal? 3
             (tree-count '((() () 1)
                           (() () 2)
                           3)))
     (equal? 5
             (tree-count '(((() () 1)
                            () 2)
                           (()
                            (() () 3) 4)
                           5)))
     (equal? 8
             (tree-count '(((() (() () 1) 2)
                            () 3)
                           (()
                            ((() (() () 4)
                              5)
                             ()
                             6)
                            7)
                           8)))

     (equal? 8
             (tree-count '(((() (() () 1) 2)
                            () 3)
                           (()
                            ((() (() () 4)
                              5)
                             ()
                             6)
                            7)
                           8)))

     (equal? '()
             (tree-flatten '()))
     (equal? '(1)
             (tree-flatten '(() () 1)))
     (equal? '(2 1)
             (tree-flatten '((() () 2) () 1)))
     (equal? '(2 1)
             (tree-flatten '(() (() () 2) 1)))
     (equal? '(1 2 3)
             (tree-flatten '((() () 1)
                             (() () 2)
                             3)))
     (equal? '(1 2 3 4 5)
             (tree-flatten '(((() () 1)
                              () 2)
                             (()
                              (() () 3) 4)
                             5)))
     (equal? '(1 2 3 4 5 6 7 8)
             (tree-flatten '(((() (() () 1) 2)
                              () 3)
                             (()
                              ((() (() () 4)
                                5)
                               ()
                               6)
                              7)
                             8)))


     ;; in ellipsis
     (begin (define (sum-list ls)
              (match ls
                [()
                 0]
                ;; order matters
                [(,@x* ...)
                 (apply + x*)]
                [,x
                 x]))
            #t)

     (= 0
        (sum-list '()))
     (= 1
        (sum-list '(1)))
     (= 2
        (sum-list '(1 1)))
     (= 4
        (sum-list '(1 (1 1) 1)))
     (= 12
        (sum-list '(1 (1 (1 (1 1) 1) 1) 1 (1 (1 1) 1))))
     (= 24
        (sum-list '(1 (1 1) (1 1) (1 (1 1) (1 (1 1) (1 1) 1) 1) 1 (1 1) (1 (1 1) (1 1) 1))))


     ;; nested ellipses, custom proc
     (begin (define proc (lambda (x) (collapse-list x)))
            (define (collapse-list data)
              (match data
                [#f
                 #f]
                [%
                 '%]
                [(((1 ,@[proc -> ,x] 3) ...) ...)
                 x]))
            #t)

     (equal? #f
             (collapse-list #f))
     (equal? '%
             (collapse-list '%))
     (equal? '((#f))
             (collapse-list '(((1 #f 3)))))
     (equal? '((%))
             (collapse-list '(((1 % 3)))))
     (equal? '((% %) (#f #f))
             (collapse-list '(((1 % 3) (1 % 3) #|...|#)
                              ((1 #f 3) (1 #f 3) #|...|#)
                              ;; ...
                              )))
     (equal? '((#f #f #f #f #f)
               (% %
                  ((#f #f #f #f #f) (% % % % %) (#f #f #f #f #f) (% % % % %))
                  %
                  %)
               (#f #f #f #f #f)
               (% % %
                  ((#f #f #f #f #f) (% % % % %) (#f #f #f #f #f) (% % % % %))
                  %))
             (collapse-list '(((1 #f 3)
                               (1 #f 3)
                               (1 #f 3)
                               (1 #f 3)
                               (1 #f 3))
                              ((1 % 3)
                               (1 % 3)
                               (1 (((1 #f 3)
                                    (1 #f 3)
                                    (1 #f 3)
                                    (1 #f 3)
                                    (1 #f 3))
                                   ((1 % 3)
                                    (1 % 3)
                                    (1 % 3)
                                    (1 % 3)
                                    (1 % 3))
                                   ((1 #f 3)
                                    (1 #f 3)
                                    (1 #f 3)
                                    (1 #f 3)
                                    (1 #f 3))
                                   ((1 % 3)
                                    (1 % 3)
                                    (1 % 3)
                                    (1 % 3)
                                    (1 % 3)))
                                  3)
                               (1 % 3)
                               (1 % 3))
                              ((1 #f 3)
                               (1 #f 3)
                               (1 #f 3)
                               (1 #f 3)
                               (1 #f 3))
                              ((1 % 3)
                               (1 % 3)
                               (1 % 3)
                               (1 (((1 #f 3)
                                    (1 #f 3)
                                    (1 #f 3)
                                    (1 #f 3)
                                    (1 #f 3))
                                   ((1 % 3)
                                    (1 % 3)
                                    (1 % 3)
                                    (1 % 3)
                                    (1 % 3))
                                   ((1 #f 3)
                                    (1 #f 3)
                                    (1 #f 3)
                                    (1 #f 3)
                                    (1 #f 3))
                                   ((1 % 3)
                                    (1 % 3)
                                    (1 % 3)
                                    (1 % 3)
                                    (1 % 3)))
                                  3)
                               (1 % 3)))))


     ;; multiple retval
     (begin (define (sum/multiply-list ls)
              (match ls
                [()
                 (values 0 1)]
                ;; order matters
                [(,@[,s* ,p*] ...)
                 (values (apply + s*)
                         (apply * p*))]
                [,x
                 (values x x)]))
            #t)

     (let-values ([(s p) (sum/multiply-list '())])
       (and (= s 0)
            (= p 1)))
     (let-values ([(s p) (sum/multiply-list '(1))])
       (and (= s 1)
            (= p 1)))
     (let-values ([(s p) (sum/multiply-list '(1 2))])
       (and (= s 3)
            (= p 2)))
     (let-values ([(s p) (sum/multiply-list '(1 (2 3) 1))])
       (and (= s 7)
            (= p 6)))
     (let-values ([(s p) (sum/multiply-list '(1 (2 (1 (3 1) 4) 1) 5 (1 (6 1) 7)))])
       (and (= s 33)
            (= p (* 2 3 4 5 6 7))))
     (let-values ([(s p) (sum/multiply-list '(1 (2 3) (1 1 6 7) (1 (1 1) (1 (2 3) (1 1) 1) 1) 5 (1 1) (1 (7 8) (1 1) 1)))])
       (and (= s 60)
            (= p (* 2 3 6 7 2 3 5 7 8))))

     ;; The generated match-loop binding stays hygienic when a capture has the same name.
     (let ([match-loop 'outer])
       (equal? '(1 2)
               (match '(1 2)
                 [() '()]
                 [(,head . ,@match-loop) (cons head match-loop)])))
     )

;;;; in-place update





;;;; error tests

(mat native-record-patterns
     (let ()
       (define-record-type ReviewPoint (fields x y))
       (equal? '(10 20)
               (match (make-ReviewPoint 10 20)
                 [,(record ReviewPoint :values ,x ,y) (list x y)]
                 [else #f])))

     (let ()
       (define-record-type ReviewPoint (fields x y))
       (equal? 20
               (match (make-ReviewPoint 10 20)
                 [,(record ReviewPoint :fields (y ,y)) y]
                 [else #f])))

     ;; A native record pattern must fall through for other value types.
     (let ()
       (define-record-type ReviewPoint (fields x))
       (match 10
         [,(record ReviewPoint :values ,x) #f]
         [else #t])))

(mat hash-pattern-grammar
     ;; With no required keys, the pattern checks only that the input is a hashtable.
     (match (make-eq-hashtable) [,(hash) #t] [else #f])

     (match 42 [,(hash) #f] [else #t])

     (let ([key 'wrong] [table (make-eq-hashtable)])
       (hashtable-set! table 'key 42)
       (= 42 (match table [,(hash (key ,value)) value])))

     (let ([table (make-eq-hashtable)])
       (hashtable-set! table 'key #f)
       (equal? '(#f #f)
               (match (list table table) [(,(hash (key ,value)) ...) value])))

     (match (make-eq-hashtable) [,(hash (missing ,value)) #f] [else #t])

     ;; Keys are literal data; compound expressions are not key expressions.
     (error? (eval '(match (make-eq-hashtable)
                     [,(hash ((string-append "k" "ey") ,value)) value]))))

(mat extension-fallbacks
     ;; An absent key must fail, while a stored #f is a valid value.
     (match (make-hashtable string-hash string=?)
       [,(hash ("missing" ,value)) #f]
       [else #t])

     (let ([table (make-hashtable string-hash string=?)])
       (hashtable-set! table "key" #f)
       (match table
         [,(hash ("key" ,value)) (eq? value #f)]
         [else #f]))

     ;; Incompatible extension inputs must try the next clause.
     (match 123 [,(hash ("key" ,value)) #f] [else #t])

     (match 123 [,(regex "a") #f] [else #t])

     (match 123 [,(regex "a" (0 ,value)) #f] [else #t])

     ;; An unknown named group must report the group, not reference a variable.
     (let ([result (guard (e [#t e])
                     (match "a" [,(regex "a" (missing ,value)) value]))])
       (and (condition? result)
            (string-contains? (condition-message result)
                              "invalid index number or name for regex")
            (equal? (condition-irritants result) '(missing)))))

(mat matching-helper-boundaries
     (= 42 ((mlambda [0 0] [else 42]) 1))

     (= 42 ((mlambda+ [(0) 0] [else 42]) 1))

     (let ([value #f])
       (and (string=? ""
                      (with-output-to-string
                        (lambda ()
                          (parameterize ([trace-output-port (current-output-port)])
                            (set! value (eval '(mlet* ([,x 1]) x)
                                              (environment '(cspp))))))))
            (= value 1)))

     (equal? '() (match '() [(,x ...) x] [else #f]))

     (equal? '(() ()) (match '() [((,x ,y) ...) (list x y)] [else #f]))

     (equal? '(() (1 2))
             (match '(() (1 2)) [((,x ...) ...) x] [else #f]))

     ;; A repeated list pattern must reject a non-list input.
     (match 123 [(,x ...) #f] [else #t]))

(mat native-record-descriptor-boundaries
     (let ()
       (define-record-type StaticPoint (nongenerative review-static-point) (fields x))
       (= 10 (match (make-StaticPoint 10) [,(record StaticPoint :values ,x) x])))

     (let ()
       (define-record-type StaticEmpty (nongenerative review-static-empty) (fields))
       (match (make-StaticEmpty) [,(record StaticEmpty :values) #t] [else #f]))

     (let ()
       (define-record-type DynamicEmpty (fields))
       (match (make-DynamicEmpty) [,(record DynamicEmpty :values) #t] [else #f]))

     ;; A zero-field pattern is invalid for a record with a field.
     (let ()
       (define-record-type Nonempty (fields x))
       (guard (e [else (string-contains? (condition-message e) "invalid record pattern")])
         (match (make-Nonempty 10) [,(record Nonempty :values) #f] [else #f]))))

(mat native-record-syntax-named-field
     ;; A field name may also be a pattern keyword; its named pattern remains valid.
     (let ()
       (define-record-type SyntaxField (fields unquote))
       (= 10 (match (make-SyntaxField 10)
               [,(record SyntaxField :fields (unquote ,x)) x]
               [else #f]))))

(mat ellipsis-suffix-backreferences
     (equal? '(1 2)
             (match '(1 2 stop (1 2)) [(,x ... stop `x) x] [else #f]))

     (equal? '(1 2)
             (match '#(1 2 stop (1 2)) [#(,x ... stop `x) x] [else #f]))

     ;; A suffix reference must use the capture, not an unrelated outer binding.
     (let ([x '(wrong)])
       (match '(1 2 stop (wrong)) [(,x ... stop `x) #f] [else #t]))

     (let ([x '(wrong)])
       (match '#(1 2 stop (wrong)) [#(,x ... stop `x) #f] [else #t]))

     (equal? '() (match '(stop ()) [(,x ... stop `x) x] [else #f]))

     (equal? '() (match '#(stop ()) [#(,x ... stop `x) x] [else #f]))

     ;; A failed suffix must allow the repeated prefix to consume another item.
     (equal? '(stop nope)
             (match '(stop nope stop (stop nope))
               [(,x ... stop `x) x] [else #f]))

     (equal? '(stop nope)
             (match '#(stop nope stop (stop nope))
               [#(,x ... stop `x) x] [else #f])))

(mat extension-capture-boundaries
     (eq? 'hit
          (match '("a" "a")
            [(,(regex (let ([piece "a"]) `(: ,piece))) ...) 'hit]
            [else 'miss]))

     (eq? 'hit
          (match '#("a" "a")
            [#(,(regex (let ([piece "a"]) `(: ,piece))) ...) 'hit]
            [else 'miss]))

     (equal? '("a" "b")
             (match '("a" "b")
               [(,(regex (let ([piece 'any]) `(: ,piece)) (0 ,text)) ...) text]
               [else #f]))

     ;; A named field is syntax, and its literal value is not a capture variable.
     (let ()
       (define-record-type SyntaxLiteral (fields unquote))
       (match (list (make-SyntaxLiteral 'marker) (make-SyntaxLiteral 'marker))
         [(,(record SyntaxLiteral :fields (unquote marker)) ...) #t]
         [else #f]))

     (let ()
       (define-record-type SyntaxLiteral (fields unquote))
       (match (vector (make-SyntaxLiteral 'marker))
         [#(,(record SyntaxLiteral :fields (unquote marker)) ...) #t]
         [else #f]))

     (let ()
       (define-record-type SyntaxLiteral (nongenerative round4-syntax-literal) (fields unquote))
       (match (list (make-SyntaxLiteral 'marker))
         [(,(record SyntaxLiteral :fields (unquote marker)) ...) #t]
         [else #f]))

     ;; Positional mode declares its captures regardless of field names.
     (let ()
       (define-record-type Positional (fields value))
       (equal? '(10 20)
               (match (list (make-Positional 10) (make-Positional 20))
                 [(,(record Positional :values ,item) ...) item]
                 [else #f]))))

(mat extension-capture-composition
     (equal? '(10 20)
             (match (list (box 10) (box 20) 'end)
               [(,(box ,x) ... end) x] [else #f]))

     (let ()
       (record CaptureRecord (value))
       (equal? '(10 20)
               (match (list (CaptureRecord 10) (CaptureRecord 20))
                 [(,(record CaptureRecord :fields (value ,x)) ...) x] [else #f])))

     (let ()
       (datatype CaptureType [Item value])
       (equal? '(10 20)
               (match (vector (Item 10) (Item 20))
                 [#(,(datatype CaptureType Item :fields (value ,x)) ...) x] [else #f])))

     (let ()
       (define-record-type DynamicCapture (fields unquote))
       (equal? '(10 20)
               (match (list (make-DynamicCapture 10) (make-DynamicCapture 20))
                 [(,(record DynamicCapture :fields (unquote ,x)) ...) x] [else #f])))

     ;; Named and positional modes may coexist in one repeated pattern.
     (let ()
       (define-record-type DynamicLiteral (fields unquote))
       (define-record-type DynamicValue (fields value))
       (equal? '(10 20)
               (match (list (list (make-DynamicLiteral 'marker) (make-DynamicValue 10))
                            (list (make-DynamicLiteral 'marker) (make-DynamicValue 20)))
                 [((,(record DynamicLiteral :fields (unquote marker)) ,(record DynamicValue :values ,x)) ...) x]
                 [else #f]))))

(mat middle-ellipsis-materialization
     ;; A fixed literal suffix must not repeatedly materialize all prefix captures.
     (let* ([source (append (iota 2000) '(stop))]
            [before (statistics)]
            [result (match source [(,x ... stop) (length x)] [else #f])]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= result 2000) (< allocated 1000000)))

     (let* ([source (list->vector (append (iota 2000) '(stop)))]
            [before (statistics)]
            [result (match source [#(,x ... stop) (length x)] [else #f])]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= result 2000) (< allocated 1000000)))

     ;; Embedded suffix expressions can read prefix captures before body selection.
     (equal? '(1 2)
             (match '(1 2 #&2)
               [(,x ... ,(box `x)) 'wrong]
               [(,x ... ,(box ,y)) (= y (length x)) x]
               [else #f]))

     (equal? '(1 2)
             (match '(1 2 (1 2)) [(,x ... `x) x] [else #f]))

     ;; Selected captures retain ordinary mutable variable semantics.
     (equal? '(changed)
             (match '(1 2 stop) [(,x ... stop) (begin (set! x '(changed)) x)] [else #f])))

(mat extension-suffix-materialization
     ;; Built-in suffixes without prefix references must allocate linearly, not reverse per retry.
     (let ()
       (define-record-type SuffixRecord (fields value))
       (datatype SuffixType [SuffixItem value])
       (define (check suffix matcher)
         (let* ([input (append (iota 2000) (list suffix))]
                [before (statistics)]
                [result (matcher input)]
                [allocated (sstats-bytes (sstats-difference (statistics) before))])
           (and (= result 2000) (< allocated 1000000))))
       (let ([table (make-eq-hashtable)])
         (hashtable-set! table 'key 'stop)
         (and (check (box 'stop)
                     (lambda (input) (match input [(,x ... ,(box stop)) (length x)])))
              (check (box 'stop)
                     (lambda (input)
                       (match (list->vector input) [#(,x ... ,(box stop)) (length x)])))
              (check table
                     (lambda (input) (match input [(,x ... ,(hash (key stop))) (length x)])))
              (check (make-SuffixRecord 'stop)
                     (lambda (input)
                       (match input [(,x ... ,(record SuffixRecord :values stop)) (length x)])))
              (check (SuffixItem 'stop)
                     (lambda (input)
                       (match input
                         [(,x ... ,(datatype SuffixType SuffixItem :fields (value stop)))
                          (length x)]))))))

     ;; Known extensions containing prefix backreferences still materialize before matching.
     (let ()
       (datatype SuffixReference [ReferenceItem value])
       (let ([table (make-eq-hashtable)])
         (hashtable-set! table 'key '(1 2))
         (and (equal? '(1 2)
                      (match (list 1 2 table) [(,x ... ,(hash (key `x))) x]))
              (equal? '(1 2)
                      (match (vector 1 2 (ReferenceItem '(1 2)))
                        [#(,x ... ,(datatype SuffixReference ReferenceItem :values `x)) x]))))))

(mat record-pattern-expansion-growth
     ;; Four sibling local records must not duplicate the remaining matcher exponentially.
     (let* ([before (statistics)]
            [expanded
             (expand '(lambda ()
                        (define-record-type P (fields value))
                        (match '()
                          [((,(record P :values ,a) ,(record P :values ,b) ,(record P :values ,c) ,(record P :values ,d)) ...) #t]
                          [else #f])))]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (pair? expanded) (< allocated 10000000))))

(mat explicit-record-capture-scope
     (let ([marker 'outer])
       (define-record-type LiteralCapture (fields unquote))
       (and (eq? 'outer
                 (match (list (make-LiteralCapture 'marker))
                   [(,(record LiteralCapture :fields (unquote marker)) ...) marker]))
            (eq? 'changed
                 (match (list (make-LiteralCapture 'marker))
                   [(,(record LiteralCapture :fields (unquote marker)) ...)
                    (begin (set! marker 'changed) marker)]))
            (eq? marker 'changed)))

     ;; Literal field patterns keep outer bindings visible to later backreferences.
     (let ([marker 'outer])
       (define-record-type LiteralCapture (fields unquote))
       (match (list (list (make-LiteralCapture 'marker) 'outer))
         [((,(record LiteralCapture :fields (unquote marker)) `marker) ...) #t]
         [else #f]))

     (let ([x 99])
       (define-record-type ValueCapture (fields value))
       (and (equal? '(10 20)
                    (match (list (list (make-ValueCapture 10) 10)
                                 (list (make-ValueCapture 20) 20))
                      [((,(record ValueCapture :values ,x) `x) ...) x] [else #f]))
            (equal? '(changed)
                    (match (list (make-ValueCapture 10))
                      [(,(record ValueCapture :values ,x) ...) (begin (set! x '(changed)) x)]))
            (= x 99)))

     (let ()
       (define-record-type ValueCapture (fields value))
       (equal? '((10 20) (30))
               (match (list (list (make-ValueCapture 10) (make-ValueCapture 20))
                            (list (make-ValueCapture 30)))
                 [((,(record ValueCapture :values ,x) ...) ...) x] [else #f]))))

(mat sibling-record-pattern-expansion
     ;; A seven-record tuple should produce a modest expansion, including capture consumers.
     (letrec ([size (lambda (tree)
                      (cond [(pair? tree) (+ 1 (size (car tree)) (size (cdr tree)))]
                            [(vector? tree) (fold-left (lambda (n x) (+ n (size x))) 1
                                                       (vector->list tree))]
                            [else 1]))])
       (< (size (expand '(lambda ()
                          (define-record-type P (fields value))
                          (match '()
                            [((,(record P :values ,a) ,(record P :values ,b) ,(record P :values ,c) ,(record P :values ,d)
                               ,(record P :values ,e) ,(record P :values ,f) ,(record P :values ,g)) ...)
                             (list a b c d e f g)]
                            [else #f]))))
          20000)))

(mat record-capture-location
     ;; Closures created in later pattern expressions share capture assignment with the body.
     (let ([saved #f])
       (define-record-type LocationRecord (fields a b))
       (equal? '(20 20)
               (match (make-LocationRecord 10 "a")
                 [,(record LocationRecord :fields (a ,x)
                                   (b ,(regex (begin (set! saved (lambda () x)) "a"))))
                  (begin (set! x 20)
                         (list x (saved)))]
                 [else #f]))))

(mat macro-introduced-suffix-reference
     ;; A suffix macro can introduce a reference absent from its surface syntax.
     (let ([saved #f] [x 'outer])
       (define-syntax capture-regex
         (lambda (stx)
           (syntax-case stx ()
             [(keyword)
              (with-syntax ([x (datum->syntax #'keyword 'x)])
                #'(begin (set! saved x) "a"))])))
       (and (equal? '(1 2)
                    (match '(1 2 "a") [(,x ... ,(regex (capture-regex))) x] [else #f]))
            (equal? '(1 2) saved))))

(mat library-record-captures
     ;; Captures may shadow imports and identifiers with no surrounding binding.
     (begin
       (eval '(library (match-tests local-record-capture)
                (export run)
                (import (cspp))
                (define run
                  (lambda ()
                    (define-record-type P (fields value))
                    (list (match (make-P 10) [,(record P :values ,map) map])
                          (match (list (make-P 20)) [(,(record P :values ,x) ...) x])))))
             (copy-environment (environment '(cspp)) #t))
       (equal? '(10 (20))
               ((eval 'run (environment '(match-tests local-record-capture)))))))

(mat named-and-positional-record-fields
     (let ()
       (define-record-type NamedRecord (fields a))
       (= 10 (match (make-NamedRecord 10) [,(record NamedRecord :fields (a ,x)) x] [else #f])))

     ;; Explicit positional mode treats a two-element list as the field value.
     (let ()
       (define-record-type PositionalRecord (fields value))
       (= 10 (match (make-PositionalRecord '(a 10))
               [,(record PositionalRecord :values (a ,x)) x] [else #f])))

     (let ()
       (define-record-type PositionalRecord (fields value))
       (match (make-PositionalRecord '(b 10))
         [,(record PositionalRecord :values (a ,x)) #f] [else #t]))

     ;; A wildcard is an ordinary part of a positional list pattern.
     (let ()
       (define-record-type PositionalRecord (fields value))
       (= 10 (match (make-PositionalRecord '(b 10))
               [,(record PositionalRecord :values (_ ,x)) x] [else #f]))))

(mat nested-record-pattern-expansion-growth
     ;; Each nested named field generates a single inner matcher.
     (let* ([pattern (let loop ([depth 6])
                       (if (= depth 0) '(unquote x)
                           (list 'unquote (list 'record 'P ':fields (list 'a (loop (- depth 1)))))))]
            [before (statistics)]
            [expanded (expand `(lambda ()
                                 (define-record-type P (fields a))
                                 (match #f [,pattern x] [else #f])))]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (pair? expanded) (< allocated 10000000))))

(mat local-record-capture-allocation
     ;; A read-only repetition must not allocate a box for each capture.
     (let ()
       (define-record-type AllocationRecord (fields value))
       (let* ([source (map make-AllocationRecord (iota 1000))]
              [before (statistics)]
              [result (match source [(,(record AllocationRecord :values ,x) ...) (length x)] [else #f])]
              [allocated (sstats-bytes (sstats-difference (statistics) before))])
         (and (= result 1000) (< allocated 60000)))))

(mat macro-introduced-backreference
     ;; Identifier macros in backreferences must retain repeated prefix capture scope.
     (let ([x 'outer])
       (define-syntax prefix-value (lambda (stx) (datum->syntax stx 'x)))
       (equal? '(1 2)
               (match '(1 2 (1 2)) [(,x ... `prefix-value) x] [else #f])))

     (let ([x 'outer])
       (define-syntax prefix-value (lambda (stx) (datum->syntax stx 'x)))
       (match '(1 2 outer) [(,x ... `prefix-value) #f] [else #t]))

     (let ([x 'outer])
       (define-syntax prefix-value (lambda (stx) (datum->syntax stx 'x)))
       (equal? '(1 2)
               (match '#(1 2 (1 2)) [#(,x ... `prefix-value) x] [else #f])))

     (let ([x 'outer])
       (define-syntax prefix-value (lambda (stx) (datum->syntax stx 'x)))
       (match '#(1 2 outer) [#(,x ... `prefix-value) #f] [else #t])))

(mat native-record-capture-bindings
     ;; Active captures must shadow missing, imported, and syntax bindings in libraries.
     (begin
       (eval '(library (match-tests native-capture-bindings)
                (export run)
                (import (cspp))
                (define run
                  (lambda ()
                    (define-record-type P (fields value))
                    (list (match (make-P 10) [,(record P :values ,x) x])
                          (match (make-P 10) [,(record P :values ,map) (begin (set! map 20) map)])
                          (match (list (make-P 10))
                            [(,(record P :values ,if) ...) (begin (set! if '(30)) if)])
                          (match (vector (make-P 10)) [#(,(record P :values ,P) ...) P])))))
             (copy-environment (environment '(cspp)) #t))
       (equal? '(10 20 (30) (10))
               ((eval 'run (environment '(match-tests native-capture-bindings))))))

     ;; Literal fields leave readable imports and writable lexical aliases in scope.
     (let ([outer 0])
       (define-record-type SyntaxField (fields unquote))
       (let-syntax ([name (identifier-syntax
                          (reference outer)
                          ((set! reference value) (set! outer value)))])
         (and (eq? map (match (list (make-SyntaxField 'map))
                        [(,(record SyntaxField :fields (unquote map)) ...) map]))
              (= 20 (match (list (make-SyntaxField 'name))
                      [(,(record SyntaxField :fields (unquote name)) ...)
                       (begin (set! name 20) name)]))
              (= outer 20))))

     ;; Short and substantial bodies must accept the same record capture bindings.
     (let ()
       (define-record-type P (fields value))
       (= 40 (match (make-P 10)
               [,(record P :values ,map)
                (begin 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                       (set! map 40) map)]))))

(mat literal-record-field-syntax-bindings
     ;; Literal field names do not hide surrounding operator syntax.
     (let ()
       (define-record-type SyntaxField (fields unquote))
       (eq? 'ok (match (make-SyntaxField 'if)
                  [,(record SyntaxField :fields (unquote if)) (if #t 'ok 'bad)])))

     (let ()
       (define-record-type SyntaxField (fields unquote))
       (eq? 'ok (match (list (make-SyntaxField 'if))
                  [(,(record SyntaxField :fields (unquote if)) ...) (if #t 'ok 'bad)])))

     ;; Collecting pattern variables must not speculatively expand identifier macros.
     (let ()
       (define-record-type SyntaxField (fields unquote))
       (define-syntax counter
         (let ([count 0])
           (lambda (stx)
             (set! count (+ count 1))
             (datum->syntax stx count))))
       (= 1 (match (make-SyntaxField 'counter) [,(record SyntaxField :fields (unquote counter)) counter])))

     (let ()
       (define-record-type SyntaxField (fields unquote))
       (define-syntax counter
         (let ([count 0])
           (lambda (stx)
             (set! count (+ count 1))
             (datum->syntax stx count))))
       (= 1 (match (list (make-SyntaxField 'counter))
              [(,(record SyntaxField :fields (unquote counter)) ...) counter])))

     ;; Forwarded macros preserve definition-site bindings and temporary-variable hygiene.
     (let ([outer 10])
       (define-record-type SyntaxField (fields unquote))
       (define-syntax value
         (lambda (stx) #'(let ([temporary 1]) (+ outer temporary))))
       (let ([outer 100] [temporary 100])
         (= 11 (match (make-SyntaxField 'value) [,(record SyntaxField :fields (unquote value)) value]))))

     ;; Literal field patterns preserve macro lookup callbacks and compile-time properties.
     (let ()
       (define-record-type SyntaxField (fields unquote))
       (define-syntax property-key (syntax-rules () [(_) #f]))
       (define-syntax value (lambda (stx) (lambda (lookup) #'42)))
       (define-property value property-key #t)
       (= 42 (match (make-SyntaxField 'value) [,(record SyntaxField :fields (unquote value)) value]))))

(mat record-operator-bindings
     ;; A captured procedure may shadow core syntax even if its arity differs.
     (let ()
       (define-record-type P (fields value))
       (= 43 (match (make-P (lambda (value) (+ value 1)))
               [,(record P :values ,if)
                (begin 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                       (if 42))])))

     (let ()
       (define-record-type P (fields value))
       (eq? 'zero (match (make-P (lambda () 'zero))
                    [,(record P :values ,if)
                     (begin 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                            0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                            (if))])))

     ;; A captured operator must not cause its argument macros to expand twice.
     (let ()
       (define-record-type P (fields value))
       (define-syntax counter
         (let ([count 0])
           (lambda (stx)
             (set! count (+ count 1))
             (datum->syntax stx count))))
       (= 1 (match (make-P (lambda (value) value))
              [,(record P :values ,if)
               (begin 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      (if counter))])))

     ;; Old automatic native-record syntax is rejected at expansion time.
     (guard (condition [(syntax-violation? condition) #t] [else #f])
       (expand '(let ()
                  (define-record-type OldRecord (fields value))
                  (match (make-OldRecord 10) [,(OldRecord ,x) x])))
       #f))

(mat explicit-record-modes
     ;; The explicit wrapper must select positional matching without inspecting field names.
     (equal? '(10 20)
             (eval '(let ()
                      (define-record-type ExplicitPoint (fields x y))
                      (match (make-ExplicitPoint 10 20)
                        [,(record ExplicitPoint :values ,x ,y) (list x y)]))
                   (environment '(cspp))))

     (equal? '(20 10)
             (eval '(let ()
                      (define-record-type ExplicitPoint (fields x y))
                      (match (make-ExplicitPoint 10 20)
                        [,(record ExplicitPoint :fields (y ,y) (x ,x)) (list y x)]))
                   (environment '(cspp)))))

(mat explicit-record-disambiguation
     (let ()
       (define-record-type BothModes (fields unquote))
       (and (= 10 (match (make-BothModes 10) [,(record BothModes :values ,x) x]))
            (= 20 (match (make-BothModes 20)
                    [,(record BothModes :fields (unquote ,x)) x]))
            (= 30 (match (make-BothModes '(tag 30))
                    [,(record BothModes :values (tag ,x)) x]))))

     ;; A positional list pattern stays positional even when its head names a field.
     (let ()
       (define-record-type PairField (fields tag))
       (= 30 (match (make-PairField '(tag 30))
               [,(record PairField :values (tag ,x)) x])))

     ;; A named field pattern never declares a capture for a literal symbol.
     (let ([marker 'outer])
       (define-record-type LiteralField (fields unquote))
       (eq? 'outer (match (list (make-LiteralField 'marker))
                     [(,(record LiteralField :fields (unquote marker)) ...) marker])))

     (let ()
       (record WrappedRecord (value))
       (and (= 42 (match (WrappedRecord 42)
                    [,(record WrappedRecord :values ,x) x]))
            (= 42 (match (WrappedRecord 42)
                    [,(record WrappedRecord :fields (value ,x)) x]))))

     ;; The reserved wrapper and modes use their spelling even with lexical bindings.
     (let ([record #f] [:fields #f] [:values #f])
       (define-record-type KeywordBindings (fields value))
       (= 42 (match (make-KeywordBindings 42)
               [,(record KeywordBindings :values ,x) x])))

     ;; A record named hash is reached explicitly, leaving the hash extension usable.
     (let ()
       (define-record-type hash (fields value))
       (let ([table (make-hashtable string-hash string=?)])
         (hashtable-set! table "key" 20)
         (and (= 10 (match (make-hash 10) [,(record hash :values ,x) x]))
              (= 20 (match table [,(hash ("key" ,x)) x])))))

     (let ()
       (define-record-type NonemptySelection (fields value))
       (match (make-NonemptySelection 10) [,(record NonemptySelection :fields) #t]))

     (let ()
       (define-record-type EmptySelection (fields))
       (and (match (make-EmptySelection) [,(record EmptySelection :fields) #t])
            (match (make-EmptySelection) [,(record EmptySelection :values) #t])))

     ;; A list whose reader form resembles a hash extension is data, matched with a guard.
     (= 42 (match '(unquote (hash ("key" 42)))
             [(,head (hash ("key" ,value))) (eq? head 'unquote) value]
             [else #f]))

     ;; The guard must reject the same list with a different head symbol.
     (match '(other (hash ("key" 42)))
       [(,head (hash ("key" ,value))) (eq? head 'unquote) #f]
       [else #t]))

(mat explicit-record-errors
     ;; Malformed selectors, missing modes, duplicate fields, and old forms are syntax errors.
     (for-all
      (lambda (pattern)
        (guard (condition [(syntax-violation? condition) #t] [else #f])
          (expand `(let ()
                     (define-record-type InvalidRecord
                       (nongenerative explicit-invalid-record) (fields value))
                     (match #f [,(list 'unquote pattern) #t] [else #f])))
          #f))
      '((record InvalidRecord)
        (record InvalidRecord :unknown ,x)
        (record InvalidRecord :fields ,x)
        (record InvalidRecord :fields (value ,x extra))
        (record InvalidRecord :fields (42 ,x))
        (record InvalidRecord :fields (value ,x) (value ,y))
        (record InvalidRecord :fields (missing ,x))
        (record InvalidRecord :values)
        (record InvalidRecord :values ,x ,y)
        (record InvalidRecord :values . tail)
        (record 42 :values ,x)
        (record MissingRecord :values ,x)
        (InvalidRecord ,x)))

     ;; CSPP records also require the explicit wrapper.
     (guard (condition [(syntax-violation? condition) #t] [else #f])
       (expand '(let ()
                  (record OldWrapped (value))
                  (match (OldWrapped 10) [,(OldWrapped ,x) x])))
       #f)

     ;; Unknown local fields are validated when their generative descriptor is available.
     (let ()
       (define-record-type DynamicInvalid (fields value))
       (guard (condition [(error? condition) #t] [else #f])
         (match (make-DynamicInvalid 10) [,(record DynamicInvalid :fields (missing ,x)) x])
         #f)))

(mat explicit-record-composition
     (let ()
       (define-record-type CompositionPoint (fields x y))
       (and (= 30 ((mlambda [,(record CompositionPoint :values ,x ,y) (+ x y)])
                  (make-CompositionPoint 10 20)))
            (= 20 (mlet ([,(record CompositionPoint :fields (y ,value))
                          (make-CompositionPoint 10 20)])
                    value))
            (= 30 (mlet* ([,(record CompositionPoint :values ,x ,y)
                           (make-CompositionPoint 10 20)]
                          [,sum (+ x y)])
                    sum))))

     ;; Empty list and vector repetitions bind every explicit field capture to an empty list.
     (let ()
       (define-record-type RepeatedPoint (fields x y))
       (and (equal? '(() ())
                    (match '() [(,(record RepeatedPoint :fields (y ,y) (x ,x)) ...)
                                (list x y)]))
            (equal? '(() ())
                    (match '#() [#(,(record RepeatedPoint :values ,x ,y) ...) (list x y)]))))

     ;; A record suffix may refer to captures from a middle ellipsis.
     (let ()
       (define-record-type ListSuffix (fields value))
       (and (equal? '(1 2)
                    (match (list 1 2 (make-ListSuffix '(1 2)))
                      [(,x ... ,(record ListSuffix :fields (value `x))) x]))
            (equal? '(1 2)
                    (match (vector 1 2 (make-ListSuffix '(1 2)))
                      [#(,x ... ,(record ListSuffix :values `x)) x])))))

(mat explicit-record-inheritance
     ;; The selected descriptor supplies its own fields and also matches descendant records.
     (let ()
       (define-record-type ParentPoint (fields parent-value))
       (define-record-type ChildPoint (parent ParentPoint) (fields child-value))
       (let ([source (make-ChildPoint 10 20)])
         (and (= 10 (match source [,(record ParentPoint :values ,value) value]))
              (= 20 (match source [,(record ChildPoint :values ,value) value]))
              (= 20 (match source [,(record ChildPoint :fields (child-value ,value)) value])))))

     ;; A nonmatching value falls through before checking a local descriptor's field count.
     (let ()
       (define-record-type MismatchedPoint (fields value))
       (match #f [,(record MismatchedPoint :values) #f] [else #t]))

     ;; Wrong positional arity on a matching local record is an error, not a partial match.
     (let ()
       (define-record-type MismatchedPoint (fields value))
       (guard (condition [(error? condition) #t] [else #f])
         (match (make-MismatchedPoint 10)
           [,(record MismatchedPoint :values ,x ,y) #f] [else #f]))))

(mat explicit-record-type-capture-hygiene
     ;; Capturing the type's public name must not hide its descriptor in a later pattern.
     (equal? '(1 2)
             (eval '(let ()
                      (define-record-type CaptureTypeName (fields value))
                      (match (list (make-CaptureTypeName 1) (make-CaptureTypeName 2))
                        [(,(record CaptureTypeName :values ,CaptureTypeName)
                          ,(record CaptureTypeName :values ,x))
                         (list CaptureTypeName x)]))
                   (environment '(cspp)))))

(mat explicit-record-binding-hygiene
     ;; Pattern captures shadow outer names, while generated record bindings stay hygienic.
     (begin
       (define-record-type BindingNames (fields value))
       (let ([rtd 'outer-rtd] [fields 'outer-fields] [indices 'outer-indices])
       (and (equal? '(10 outer-fields outer-indices)
                    (match (make-BindingNames 10)
                      [,(record BindingNames :values ,rtd)
                       (list rtd fields indices)]))
            (= 10 (match (make-BindingNames 10)
                    [,(record BindingNames :fields (value ,fields)) fields]))))))

(mat explicit-cspp-record-extension-names
     ;; Removing automatic record heads also keeps a CSPP record from hiding a built-in head.
     (let ()
       (record hash (value))
       (let ([table (make-hashtable string-hash string=?)])
         (hashtable-set! table "key" 20)
         (and (= 10 (match (hash 10) [,(record hash :values ,x) x]))
              (= 20 (match table [,(hash ("key" ,x)) x]))))))

(mat imported-adt-pattern-metadata
     (let ([definition-environment (copy-environment (environment '(cspp)) #t)])
       (eval '(library (match-tests public-adt-types)
                (export Point Tree Empty Leaf Other OtherLeaf)
                (import (cspp))
                (record Point (x y))
                (datatype Tree [Empty] [Leaf value])
                (datatype Other [OtherLeaf value]))
             definition-environment)
       (eval '(library (match-tests reexported-adt-types)
                (export Point Tree Empty Leaf)
                (import (match-tests public-adt-types)))
             definition-environment)
       (and
        (guard (condition [#t #f])
          (equal? '((10 20) 42 empty)
                  (eval '(list
                          (match (Point 10 20)
                            [,(record Point :values ,x ,y) (list x y)])
                          (match (Leaf 42)
                            [,(datatype Tree Leaf :fields (value ,x)) x])
                          (match Empty [,(datatype Tree Empty) 'empty]))
                        (environment '(cspp) '(match-tests public-adt-types)))))
        (guard (condition [#t #f])
          (equal? '((10 20) 42 empty)
                  (eval '(list
                          (match (p:Point 10 20)
                            [,(record p:Point :values ,x ,y) (list x y)])
                          (match (p:Leaf 42)
                            [,(datatype p:Tree p:Leaf :values ,x) x])
                          (match p:Empty [,(datatype p:Tree p:Empty) 'empty]))
                        (environment '(cspp)
                                     '(prefix (match-tests reexported-adt-types) p:)))))
        (guard (condition [#t #f])
          (equal? '(10 42)
                  (eval '(list
                          (match (P 10 20)
                            [,(record P :fields (x ,P)) P])
                          (match (L 42)
                            [,(datatype T L :values ,T) T]))
                        (environment '(cspp)
                                     '(rename (match-tests public-adt-types)
                                              (Point P) (Tree T) (Leaf L))))))
        ;; A constructor from another datatype is not a variant of Tree.
        (guard (condition [#t #t])
          (eval '(match (OtherLeaf 1)
                   [,(datatype Tree OtherLeaf :values ,x) x])
                (environment '(cspp) '(match-tests public-adt-types)))
          #f))))

(mat repeated-custom-expander-fresh-captures
     (guard (condition [#t #f])
       (equal?
        '((10 20) (10 20) (10 20) (10 20) ((10 20) (30)) (10 20))
        (eval
         '(let ()
            (define-match-expander fresh-tail-expander
              (lambda (pp rho input pattern body failure)
                (syntax-case pattern ()
                  [(_ subpattern)
                   (with-syntax ([(hidden) (generate-temporaries '(hidden))])
                     (pp rho input #`(,hidden . subpattern) body failure))])))
            (define-match-expander fresh-alias-expander
              (lambda (pp rho input pattern body failure)
                (syntax-case pattern ()
                  [(_ subpattern)
                   (with-syntax ([(hidden) (generate-temporaries '(hidden))])
                     (pp rho input #`(#`hidden subpattern) body failure))])))
            (list
             (match '((ignored . 10) (ignored . 20))
               [(,(fresh-tail ,x) ...) x])
             (match '#((ignored . 10) (ignored . 20))
               [#(,(fresh-tail ,x) ...) x])
             (match '((ignored . 10) (ignored . 20) stop)
               [(,(fresh-tail ,x) ... stop) x])
             (match '#((ignored . 10) (ignored . 20) stop)
               [#(,(fresh-tail ,x) ... stop) x])
             (match '(((ignored . 10) (ignored . 20)) ((ignored . 30)))
               [((,(fresh-tail ,x) ...) ...) x])
             (match '(10 20) [(,(fresh-alias ,x) ...) x])))
         (environment '(cspp) '(cspp private match))))))

(mat custom-expander-compiled-once
     (guard (condition [#t #f])
       (equal?
        '(10 20)
        (eval
         '(let ()
            (define-match-expander once-expander
              (let ([calls 0])
                (lambda (pp rho input pattern body failure)
                  (set! calls (+ calls 1))
                  (unless (= calls 1)
                    (syntax-error pattern "custom expander invoked more than once"))
                  (syntax-case pattern ()
                    [(_ subpattern) (pp rho input #'subpattern body failure)]))))
            (match '(10 20) [(,(once ,x) ...) x]))
         (environment '(cspp) '(cspp private match))))))

(mat invalid-match-compile-time-metadata
     ;; Non-pair compile-time values are not record metadata.
     (guard (condition [(syntax-violation? condition) #t] [else #f])
       (eval '(let-syntax ([P (make-compile-time-value 10)])
                (match 10 [,(record P :values) #t] [else #f]))
             (environment '(cspp)))
       #f)

     ;; Non-pair compile-time values are not custom expander metadata.
     (guard (condition [(syntax-violation? condition) #t] [else #f])
       (eval '(let-syntax ([bad-expander (make-compile-time-value 10)])
                (match 10 [,(bad ,x) x] [else #f]))
             (environment '(cspp)))
       #f)

     ;; A tagged expander must carry a procedure, not merely the right tag.
     (guard (condition [(syntax-violation? condition) #t] [else #f])
       (eval '(let-syntax ([bad-expander
                           (make-compile-time-value (cons '*MATCH-EXPANDER* 10))])
                (match 10 [,(bad ,x) x] [else #f]))
             (environment '(cspp)))
       #f))

(mat strict-regex-group-index
     ;; An inexact integer group selector must fail during expansion, without normalization.
     (guard (condition [(syntax-violation? condition) #t] [else #f])
       (eval '(match "a" [,(regex "(.)" (1.0 ,x)) x]) (environment '(cspp)))
       #f)

     (equal? "a" (match "a" [,(regex "(.)" (1 ,x)) x])))

(mat custom-expander-branch-private-captures
     (guard (condition [#t #f])
       (equal?
        '((10 20) (10 20) (10 20) ((10) (20)) () 99)
        (eval
         '(let ()
            (define-match-expander branch-expander
              (lambda (pp rho input pattern body failure)
                (syntax-case pattern ()
                  [(_ subpattern)
                   (with-syntax ([(hidden) (generate-temporaries '(hidden))])
                     #`(if (pair? #,input)
                           #,(pp rho input #`(,hidden . subpattern) body failure)
                           #,(pp rho input #'subpattern body failure)))])))
            (let ([hidden 99])
              (list
               (match '(10 (ignored . 20)) [(,(branch ,x) ...) x])
               (match '#(10 (ignored . 20)) [#(,(branch ,x) ...) x])
               (match '(10 (ignored . 20) stop) [(,(branch ,x) ... stop) x])
               (match '((10) ((ignored . 20))) [((,(branch ,x) ...) ...) x])
               (match '() [(,(branch ,x) ...) x])
               (match '(10 (ignored . 20)) [(,(branch ,x) ...) hidden]))))
         (environment '(cspp) '(cspp private match))))))

(mat independent-regex-suffix-allocation
     ;; A regex expression with no prefix dependency must not copy every candidate prefix.
     (let* ([compiled (irregex "^stop$")]
            [input (append (iota 4000) '("stop"))]
            [before (statistics)]
            [result (match input [(,x ... ,(regex compiled)) (length x)])]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= result 4000) (< allocated 1000000)))

     (let* ([compiled (irregex "^stop$")]
            [input (list->vector (append (iota 4000) '("stop")))]
            [before (statistics)]
            [result (match input [#(,x ... ,(regex compiled)) (length x)])]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= result 4000) (< allocated 1000000))))

(mat delayed-suffix-capture-locations
     ;; Closures made by failed suffix attempts retain their own prefix and mutable location.
     (let ([saved '()])
       (let ([result
              (match '(1 2 3 "stop")
                [(,x ... ,(regex (begin (set! saved (cons (lambda () x) saved)) "^stop$")))
                 (begin (set! x '(changed)) x)])])
         (equal? '((changed) ((changed) (1 2) (1) ()))
                 (list result (map (lambda (read) (read)) saved)))))

     ;; Assigning #f must not be confused with an unmaterialized capture.
     (let ([read #f])
       (and (not (match '(1 "stop")
                   [(,x ... ,(regex (begin (set! read (lambda () x)) "^stop$")))
                    (begin (set! x #f) x)]))
            (not (read))))

     ;; A dependency in the regex expression still observes the current candidate prefix.
     (equal? '(1 2)
             (match '(1 2 "stop")
               [(,x ... ,(regex (if (= (length x) 2) "^stop$" "never"))) x])))

(mat constant-regex-repetition-allocation
     ;; A literal regex compiles once per match invocation, rather than once per item.
     (let* ([input (make-list 1000 "stop")]
            [before (statistics)]
            [result (match input [(,(regex "^stop$" (0 ,x)) ...) (length x)])]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= result 1000) (< allocated 500000)))

     (let* ([input (make-vector 1000 "stop")]
            [before (statistics)]
            [result (match input [#(,(regex '(: bos "stop" eos) (0 ,x)) ...) (length x)])]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (= result 1000) (< allocated 500000))))

(mat regex-cache-evaluation-and-hygiene
     ;; Invalid constants in unvisited clauses remain unevaluated.
     (= 42 (match 42 [,x x] [,(regex "(") #f]))

     (eq? 'empty (match '() [(,(regex "(") ...) 'empty]))

     ;; Regex expressions remain ordinary expressions, evaluated for every visited element.
     (let ([calls 0])
       (and (equal? '("a" "b")
                    (match '("a" "b")
                      [(,(regex (begin (set! calls (+ calls 1)) ".") (0 ,x)) ...) x]))
            (= calls 2)))

     (let ([compiled 99] [regex-cache 100])
       (and (equal? '("a" "b")
                    (match '("a" "b") [(,(regex "." (0 ,compiled)) ...) compiled]))
            (= compiled 99) (= regex-cache 100)))

     ;; Each invocation has its own cache and independent capture locations.
     (let ([matcher (mlambda [(,(regex "." (0 ,x)) ...) x])])
       (and (equal? '("a") (matcher '("a")))
            (equal? '("b") (matcher '("b"))))))

(mat regex-cache-capture-shadowing
     ;; An earlier capture turns apparent quotation into a call on every candidate.
     (let ([calls 0] [argument 'ignored])
       (let ([result
              (match (list (lambda (value)
                             (set! calls (+ calls 1))
                             (if (= calls 1) "^a$" "^b$"))
                           '("a" "b"))
                [(,quote (,(regex (quote argument) (0 ,text)) ...)) text]
                [else 'failed])])
         (and (equal? '("a" "b") result) (= calls 2))))

     ;; Vector repetition must resolve the same generated capture binding.
     (let ([calls 0] [argument 'ignored])
       (let ([result
              (match (vector (lambda (value)
                               (set! calls (+ calls 1))
                               (if (= calls 1) "^a$" "^b$"))
                             '#("a" "b"))
                [#(,quote #(,(regex (quote argument) (0 ,text)) ...)) text]
                [else 'failed])])
         (and (equal? '("a" "b") result) (= calls 2))))

     ;; The captured procedure can also change between repeated element matches.
     (let ([argument 'ignored])
       (equal? '("a" "b")
               (match (list (list (lambda (value) "^a$") "a")
                            (list (lambda (value) "^b$") "b"))
                 [((,quote ,(regex (quote argument) (0 ,text))) ...) text]
                 [else 'failed]))))

(mat strict-regex-capture-tags
     ;; A quoted identifier is not a regex capture declaration.
     (guard (condition [(syntax-violation? condition) #t] [else #f])
       (eval '(match "abc" [,(regex "(.)" (1 'x)) x]) (environment '(cspp)))
       #f)

     ;; An arbitrary two-element list is not a regex capture declaration.
     (guard (condition [(syntax-violation? condition) #t] [else #f])
       (eval '(match "abc" [,(regex "(.)" (1 (foo x))) x]) (environment '(cspp)))
       #f)

     (equal? "a" (match "abc" [,(regex "(.)" (1 ,x)) x])))

(mat matching-let-definition-context
     (= 3 (mlet ([,x 1]) (define y 2) (+ x y)))

     (= 3 (mlet* ([,x 1]) (define y 2) (+ x y)))

     (= 2 (mlet () (define y 2) y))

     (= 2 (mlet* () (define y 2) y))

     (equal? '(1 2 3)
             (let ([events '()])
               (mlet* ([,x (begin (set! events '(1)) 10)]
                        [,y (begin (set! events (append events '(2))) (+ x 20))])
                 (define total (+ x y))
                 (set! events (append events '(3)))
                 (and (= total 40) events))))

     (equal? '(1 2)
             (call-with-values
               (lambda () (mlet ([,x 1]) (define y 2) (values x y)))
               list)))

(mat boolean-clause-guards
     (eq? 'positive (match 3 [,x (positive? x) 'positive] [else 'negative]))

     (eq? 'negative (match -3 [,x (positive? x) 'positive] [else 'negative]))

     ;; A single result expression returning false still selects the clause.
     (not (match 3 [,x #f] [else 'unexpected]))

     ;; Failed structural patterns do not evaluate their conditions.
     (eq? 'fallback
          (match 3 [(a ,x) (error 'guard "unexpected condition") x] [else 'fallback]))

     (equal? '(condition fallback)
             (let ([events '()])
               (match 3
                 [,x (begin (set! events '(condition)) #f)
                     (error 'guard "unexpected result")]
                 [else (set! events (append events '(fallback)))])
               events))

     ;; Exception-handling guard is now an ordinary condition expression.
     (= 4 (match 3
            [,x (guard (condition [else #t]) (error 'guard "caught")) (+ x 1)]))

     (= 42 (match 3
             [,x (guard (condition [else 42]) (error 'guard "caught"))]))

     (equal? '(3 4)
             (call-with-values
               (lambda () (match 3 [,x #t (values x (+ x 1))]))
               list))

     (equal? '(changed)
             (match '(1 2 stop)
               [(,x ... stop) (pair? x) (begin (set! x '(changed)) x)]))

     (= 4 ((mlambda [,x (positive? x) (+ x 1)] [else 0]) 3))

     (= 0 ((mlambda [,x (positive? x) (+ x 1)] [else 0]) -3))

     (= 3 ((mlambda+ [(,x ,y) (< x y) (+ x y)] [else 0]) 1 2))

     (= 0 ((mlambda+ [(,x ,y) (< x y) (+ x y)] [else 0]) 2 1)))

(mat match-clause-expression-count
     ;; An ordinary clause requires a result expression.
     (error? (eval '(match 1 [,x]) (environment '(cspp))))

     ;; More than one condition and one result must be grouped explicitly.
     (error? (eval '(match 1 [,x #t x x]) (environment '(cspp))))

     ;; Else requires exactly one result expression.
     (error? (eval '(match 1 [else 1 2]) (environment '(cspp))))

     ;; Else cannot omit its result expression.
     (error? (eval '(match 1 [else]) (environment '(cspp))))

     ;; Else must be the final clause.
     (error? (eval '(match 1 [else 1] [,x x]) (environment '(cspp))))

     ;; Matching procedure clauses enforce the same expression count.
     (error? (eval '(mlambda [,x #t x x]) (environment '(cspp))))

     ;; Variadic matching procedure fallbacks also have a single result expression.
     (error? (eval '(mlambda+ [else 1 2]) (environment '(cspp)))))

(mat vector-shape-before-elements
     (let ([calls 0])
       (and (not (match '#("a")
                   [#(,(regex (begin (set! calls (+ calls 1)) ".")) _) #t]
                   [else #f]))
            (= calls 0)))

     (let ([calls 0])
       (and (equal? '(1 2)
                    (match '#(1 2 "stop")
                      [#(,prefix ... ,(regex (begin (set! calls (+ calls 1)) "^stop$"))) prefix]))
            (= calls 1))))

(mat hash-single-lookup
     (let ([calls 0])
       (let ([table (make-hashtable (lambda (key) (set! calls (+ calls 1)) 0) eq?)])
         (hashtable-set! table 'key #f)
         (set! calls 0)
         (and (not (match table [,(hash (key ,value)) value])) (= calls 1)))))

(mat repeated-list-continuation-state
     (let ([resume #f] [resumed? #f])
       (let ([result
              (match '("a" "b")
                [(,(regex (begin
                            (when (and (not resumed?) (not resume))
                              ;; Resume before accumulating this repeated capture.
                              (call/cc (lambda (continuation) (set! resume continuation))))
                            ".") (0 ,text)) ...) text])])
         (if resumed?
             (equal? '("a" "b") result)
             (begin (set! resumed? #t) (resume 'continue))))))

(mat fixed-vector-expansion-size
     (let* ([pattern (make-vector 100 '_)]
            [expanded (expand `(lambda (input) (match input [,pattern #t] [else #f]))
                              (environment '(cspp)))])
       (define (nodes expression)
         (if (pair? expression)
             (+ 1 (nodes (car expression)) (nodes (cdr expression)))
             1))
       (< (nodes expanded) 30000)))
