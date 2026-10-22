(import (cspp))


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
       (time (match (lb)
               [((((,a) ,b ,c) (,d (,e) ,f) (,g (,h) ,i) . ((,j) (,k) (,l)))
                 ...)
                (equal? a d)
                (equal? g j)
                (equal? h k)]
               [else #f])))

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
        (displayln p)
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
     (begin (define proc (lambda (x) (displayln x) (collapse-list x)))
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
     )

;;;; in-place update





;;;; error tests
