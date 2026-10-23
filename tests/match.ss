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

(mat native-record-patterns
     (let ()
       (define-record-type ReviewPoint (fields x y))
       (equal? '(10 20)
               (match (make-ReviewPoint 10 20)
                 [,(ReviewPoint ,x ,y) (list x y)]
                 [else #f])))

     (let ()
       (define-record-type ReviewPoint (fields x y))
       (equal? 20
               (match (make-ReviewPoint 10 20)
                 [,(ReviewPoint (y ,y)) y]
                 [else #f])))

     ;; A native record pattern must fall through for other value types.
     (let ()
       (define-record-type ReviewPoint (fields x))
       (match 10
         [,(ReviewPoint ,x) #f]
         [else #t])))

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
       (= 10 (match (make-StaticPoint 10) [,(StaticPoint ,x) x])))

     (let ()
       (define-record-type StaticEmpty (nongenerative review-static-empty) (fields))
       (match (make-StaticEmpty) [,(StaticEmpty) #t] [else #f]))

     (let ()
       (define-record-type DynamicEmpty (fields))
       (match (make-DynamicEmpty) [,(DynamicEmpty) #t] [else #f]))

     ;; A zero-field pattern is invalid for a record with a field.
     (let ()
       (define-record-type Nonempty (fields x))
       (guard (e [else (string-contains? (condition-message e) "invalid record pattern")])
         (match (make-Nonempty 10) [,(Nonempty) #f] [else #f]))))

(mat native-record-syntax-named-field
     ;; A field name may also be a pattern keyword; its named pattern remains valid.
     (let ()
       (define-record-type SyntaxField (fields unquote))
       (= 10 (match (make-SyntaxField 10)
               [,(SyntaxField (unquote ,x)) x]
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
         [(,(SyntaxLiteral (unquote marker)) ...) #t]
         [else #f]))

     (let ()
       (define-record-type SyntaxLiteral (fields unquote))
       (match (vector (make-SyntaxLiteral 'marker))
         [#(,(SyntaxLiteral (unquote marker)) ...) #t]
         [else #f]))

     (let ()
       (define-record-type SyntaxLiteral (nongenerative round4-syntax-literal) (fields unquote))
       (match (list (make-SyntaxLiteral 'marker))
         [(,(SyntaxLiteral (unquote marker)) ...) #t]
         [else #f]))

     ;; The same surface syntax is a positional capture for a differently named field.
     (let ()
       (define-record-type Positional (fields value))
       (equal? '(10 20)
               (match (list (make-Positional 10) (make-Positional 20))
                 [(,(Positional ,item) ...) item]
                 [else #f]))))

(mat extension-capture-composition
     (equal? '(10 20)
             (match (list (box 10) (box 20) 'end)
               [(,(box ,x) ... end) x] [else #f]))

     (let ()
       (record CaptureRecord (value))
       (equal? '(10 20)
               (match (list (CaptureRecord 10) (CaptureRecord 20))
                 [(,(CaptureRecord (value ,x)) ...) x] [else #f])))

     (let ()
       (datatype CaptureType [Item value])
       (equal? '(10 20)
               (match (vector (Item 10) (Item 20))
                 [#(,(CaptureType Item (value ,x)) ...) x] [else #f])))

     (let ()
       (define-record-type DynamicCapture (fields unquote))
       (equal? '(10 20)
               (match (list (make-DynamicCapture 10) (make-DynamicCapture 20))
                 [(,(DynamicCapture (unquote ,x)) ...) x] [else #f])))

     ;; Two descriptors with different named/positional interpretations may coexist.
     (let ()
       (define-record-type DynamicLiteral (fields unquote))
       (define-record-type DynamicValue (fields value))
       (equal? '(10 20)
               (match (list (list (make-DynamicLiteral 'marker) (make-DynamicValue 10))
                            (list (make-DynamicLiteral 'marker) (make-DynamicValue 20)))
                 [((,(DynamicLiteral (unquote marker)) ,(DynamicValue ,x)) ...) x]
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
               [(,x ... ,(box ,y)) (guard (= y (length x))) x]
               [else #f]))

     (equal? '(1 2)
             (match '(1 2 (1 2)) [(,x ... `x) x] [else #f]))

     ;; Selected captures retain ordinary mutable variable semantics.
     (equal? '(changed)
             (match '(1 2 stop) [(,x ... stop) (set! x '(changed)) x] [else #f])))

(mat record-pattern-expansion-growth
     ;; Four sibling local records must not duplicate the remaining matcher exponentially.
     (let* ([before (statistics)]
            [expanded
             (expand '(lambda ()
                        (define-record-type P (fields value))
                        (match '()
                          [((,(P ,a) ,(P ,b) ,(P ,c) ,(P ,d)) ...) #t]
                          [else #f])))]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (pair? expanded) (< allocated 10000000))))

(mat conditional-record-capture-scope
     (let ([marker 'outer])
       (define-record-type LiteralCapture (fields unquote))
       (and (eq? 'outer
                 (match (list (make-LiteralCapture 'marker))
                   [(,(LiteralCapture (unquote marker)) ...) marker]))
            (eq? 'changed
                 (match (list (make-LiteralCapture 'marker))
                   [(,(LiteralCapture (unquote marker)) ...) (set! marker 'changed) marker]))
            (eq? marker 'changed)))

     ;; An inactive capture keeps its outer binding visible to later backreferences.
     (let ([marker 'outer])
       (define-record-type LiteralCapture (fields unquote))
       (match (list (list (make-LiteralCapture 'marker) 'outer))
         [((,(LiteralCapture (unquote marker)) `marker) ...) #t]
         [else #f]))

     (let ([x 99])
       (define-record-type ValueCapture (fields value))
       (and (equal? '(10 20)
                    (match (list (list (make-ValueCapture 10) 10)
                                 (list (make-ValueCapture 20) 20))
                      [((,(ValueCapture ,x) `x) ...) x] [else #f]))
            (equal? '(changed)
                    (match (list (make-ValueCapture 10))
                      [(,(ValueCapture ,x) ...) (set! x '(changed)) x]))
            (= x 99)))

     (let ()
       (define-record-type ValueCapture (fields value))
       (equal? '((10 20) (30))
               (match (list (list (make-ValueCapture 10) (make-ValueCapture 20))
                            (list (make-ValueCapture 30)))
                 [((,(ValueCapture ,x) ...) ...) x] [else #f]))))

(mat record-pattern-shared-continuations
     ;; A seven-record tuple should produce a modest expansion, including capture consumers.
     (letrec ([size (lambda (tree)
                      (cond [(pair? tree) (+ 1 (size (car tree)) (size (cdr tree)))]
                            [(vector? tree) (fold-left (lambda (n x) (+ n (size x))) 1
                                                       (vector->list tree))]
                            [else 1]))])
       (< (size (expand '(lambda ()
                          (define-record-type P (fields value))
                          (match '()
                            [((,(P ,a) ,(P ,b) ,(P ,c) ,(P ,d)
                               ,(P ,e) ,(P ,f) ,(P ,g)) ...)
                             (list a b c d e f g)]
                            [else #f]))))
          20000)))

(mat record-capture-location
     ;; Closures created in later pattern expressions share capture assignment with the body.
     (let ([saved #f])
       (define-record-type LocationRecord (fields a b))
       (equal? '(20 20)
               (match (make-LocationRecord 10 "a")
                 [,(LocationRecord (a ,x)
                                   (b ,(regex (begin (set! saved (lambda () x)) "a"))))
                  (set! x 20)
                  (list x (saved))]
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
     ;; Library expansion rejects unbound fallbacks and assignments to immutable imports.
     (begin
       (eval '(library (match-tests local-record-capture)
                (export run)
                (import (cspp))
                (define run
                  (lambda ()
                    (define-record-type P (fields value))
                    (list (match (make-P 10) [,(P ,map) map])
                          (match (list (make-P 20)) [(,(P ,x) ...) x])))))
             (copy-environment (environment '(cspp)) #t))
       (equal? '(10 (20))
               ((eval 'run (environment '(match-tests local-record-capture)))))))

(mat shared-named-record-fields
     (let ()
       (define-record-type NamedRecord (fields a))
       (= 10 (match (make-NamedRecord 10) [,(NamedRecord (a ,x)) x] [else #f])))

     ;; Without a matching field name the original two-element list pattern is positional.
     (let ()
       (define-record-type PositionalRecord (fields value))
       (= 10 (match (make-PositionalRecord '(a 10))
               [,(PositionalRecord (a ,x)) x] [else #f])))

     (let ()
       (define-record-type PositionalRecord (fields value))
       (match (make-PositionalRecord '(b 10))
         [,(PositionalRecord (a ,x)) #f] [else #t]))

     ;; The wildcard head retains its positional meaning when it is not a field name.
     (let ()
       (define-record-type PositionalRecord (fields value))
       (= 10 (match (make-PositionalRecord '(b 10))
               [,(PositionalRecord (_ ,x)) x] [else #f]))))

(mat nested-record-pattern-expansion-growth
     ;; Nested named fields share the same inner matcher for named and positional inputs.
     (let* ([pattern (let loop ([depth 6])
                       (if (= depth 0) '(unquote x)
                           (list 'unquote (list 'P (list 'a (loop (- depth 1)))))))]
            [before (statistics)]
            [expanded (expand `(lambda ()
                                 (define-record-type P (fields a))
                                 (match #f [,pattern x] [else #f])))]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (pair? expanded) (< allocated 10000000))))

(mat local-record-capture-allocation
     ;; A short read-only repetition must not box each capture for an unused setter.
     (let ()
       (define-record-type AllocationRecord (fields value))
       (let* ([source (map make-AllocationRecord (iota 1000))]
              [before (statistics)]
              [result (match source [(,(AllocationRecord ,x) ...) (length x)] [else #f])]
              [allocated (sstats-bytes (sstats-difference (statistics) before))])
         (and (= result 1000) (< allocated 60000)))))
