(import (cspp))


;;;; When the macro does not exist, the error is
;;;; Exception: variable x is not bound
;;;; This should be improved.
#;
(fold/fold ([x :iota 20]
            [:guard (odd? x)])
           (displayln x))



(mat for/fold

;;;; simple literal

     (let ([lb (make-list-builder)])
       (for/fold ([x 0])
           (lb x))
       (equal? (lb)
               '()))
     (let ([lb (make-list-builder)])
       (for/fold ([x 10])
           (lb x))
       (equal? (lb)
               (iota 10)))

     (let ([lb (make-list-builder)])
       (for/fold ([x ""])
           (lb x))
       (equal? (lb)
               '()))
     (let ([lb (make-list-builder)])
       (for/fold ([x "abcd"])
           (lb x))
       (equal? (lb)
               '(#\a #\b #\c #\d)))

     (let ([lb (make-list-builder)])
       (for/fold ([x '#()])
           (lb x))
       (equal? (lb)
               '()))
     (let ([lb (make-list-builder)])
       (for/fold ([x '#(1 2 3)])
           (lb x))
       (equal? (lb)
               '(1 2 3)))


;;;; single var

     (let ([lb (make-list-builder)])
       (for/fold ([x :iota 10])
           (lb x))
       (equal? (lb)
               (iota 10)))

     (let ([lb (make-list-builder)])
       (for/fold ([x :nums 10])
           (lb x))
       (equal? (lb)
               (nums 10)))

     (let ([lb (make-list-builder)])
       (for/fold ([x :nums 10 20])
           (lb x))
       (equal? (lb)
               (nums 10 20)))

     (let ([lb (make-list-builder)])
       (for/fold ([x :nums -33 33 2.5])
           (lb x))
       (equal? (lb)
               (nums -33 33 2.5)))

     (let ([lb (make-list-builder)])
       (for/fold ([x :nums 33 -33 -2.5])
           (lb x))
       (equal? (lb)
               (nums 33 -33 -2.5)))

     (let ([lb (make-list-builder)]
           [ls (random-list 10 20)])
       (for/fold ([x :list ls])
           (lb x))
       (equal? (lb)
               ls))

     (let ([lb (make-list-builder)]
           [vec (random-vector 10 20)])
       (for/fold ([x :vector vec])
           (lb x))
       (equal? (lb)
               (vector->list vec)))

     ;; multiple clauses

     (let ([ns (iota 5)]
           [str "abcde"]
           [vec '#(Q W E R T)]
           [lb (make-list-builder)])
       (for/fold ([x 5]
                  [y "abcde"]
                  [z '#(Q W E R T)])
           (lb (list x y z)))
       (equal? (lb)
               (zip ns (string->list str) (vector->list vec))))


     (let ([ns (iota 10)]
           [ls (random-list 10 11)]
           [vec (random-vector 10 11)]
           [lb (make-list-builder)])
       (for/fold ([x :iota 10]
                  [y :list ls]
                  [z :vector vec])
           (lb (list x y z)))
       (equal? (lb)
               (zip ns ls (vector->list vec))))

     ;; different lengths

     (let ([ns (iota 5)]
           [str "abcdefg"]
           [vec '#(Q W E R T Y U I O P)]
           [lb (make-list-builder)])
       (for/fold ([x 5]
                  [y "abcdefg"]
                  [z '#(Q W E R T Y U I O P)])
           (lb (list x y z)))
       (equal? (lb)
               (zip ns (string->list "abcde") (vector->list '#(Q W E R T)))))


     (let ([ns (iota 10)]
           [ls (random-list 10 20)]
           [vec (random-vector 10 20)]
           [lb (make-list-builder)])
       (for/fold ([x :iota 10]
                  [y :list ls]
                  [z :vector vec])
           (lb (list x y z)))
       (equal? (lb)
               (zip ns (slice ls 10) (slice (vector->list vec) 10))))


;;;; index

     (= (for/fold ([:index i]
                   [:finish i]
                   [x #e1e9])
            (void))
        #e1e9)

     (= (for/fold ([:index i]
                   [:finish i]
                   [x :iota #e1e9])
            (void))
        #e1e9)


;;;; init

     (= (for/fold ([:init i 0]
                   [:finish i]
                   [x #e1e9])
            (+ i 1))
        #e1e9)

     (let ([v (random-vector #e1e8 1000)])
       (= (vsum v)
          (for/fold ([:init acc 0]
                     [:finish acc]
                     [x :vector v])
              (fx+ x acc))))

;;;; break

     (let ([ls '()])
       (for/fold ([x :iota 20]
                  [:break (= x 5)])
           (set! ls (cons x ls)))
       (equal? ls
               (reverse (iota 5))))

     (let ([ls '()])
       (and (equal? '(res . 5)
                    (for/fold ([x :iota 20]
                               [:break (= x 5)
                                       (begin (displayln "breaking~")
                                              (cons 'res x))])
                        (set! ls (cons x ls))))
            (equal? ls
                    (reverse (iota 5)))))

;;;; guard

     (andmap odd?
             (for/fold ([:init odds '()]
                        [:finish odds]
                        [x 100]
                        [:guard (odd? x)])
                 (cons x odds)))
     )


(mat for-string

     (error?
      (for/fold ([x :string #f])
          (displayln x)))

     (error?
      (for/fold ([x :string #f :from 10 :to 21])
          (displayln x)))

     ;; CJK Common Range: U+4E00 – U+9FFF (+ Ext A U+3400–U+4DBF)
     ;; Emoji Common Range: U+1F300 – U+1FAFF (with some in U+2600–U+27BF)

     (begin (define gen-char (lambda ()
                               (if (odd? (random 100))
                                   (random-char #\x4e00 #\x9fff)
                                   (random-char #\x1f300 #\x1faf8))))
            (define str (random-string gen-char 29 30))
            #t)

     (equal?
      (substring str 10 21)
      (for/fold ([:init acc '()]
                 [:finish (apply string (reverse acc))]
                 [x :string str :from 10 :to 21])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice (string->list str) 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :string str :from 10 :to 21 :step 2])
          (displayln x)
        (cons x acc)))

     ;; :rev

     (equal?
      (slice (string->list str) 10 21)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :string str :from 10 :to 21 :rev])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice (string->list str) 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :string str :from 10 :to 21 :step 2 :rev])
          (displayln x)
        (cons x acc)))

     ;; reorder ops

     (equal?
      (substring str 10 21)
      (for/fold ([:init acc '()]
                 [:finish (apply string (reverse acc))]
                 [x :string str :to 21 :from 10])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice (string->list str) 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :string str :step 2 :to 21 :from 10])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice (string->list str) 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :string str :to 21 :from 10 :rev :step 2])
          (displayln x)
        (cons x acc)))

     )


(mat for-vector

     (error?
      (for/fold ([x :vector #f])
          (displayln x)))

     (error?
      (for/fold ([x :vector #f :from 10 :to 21])
          (displayln x)))

     (begin (define v (random-vector 30))
            (define ls (vector->list v))
            #t)

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :vector v :from 10 :to 21])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :vector v :from 10 :to 21 :step 2])
          (displayln x)
        (cons x acc)))


     ;; :rev

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :vector v :from 10 :to 21 :rev])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :vector v :from 10 :to 21 :step 2 :rev])
          (displayln x)
        (cons x acc)))


     ;; reorder ops

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :vector v :to 21 :from 10])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :vector v :step 2 :to 21 :from 10])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :vector v :to 21 :from 10 :rev :step 2])
          (displayln x)
        (cons x acc)))

     )


(mat for-fxvector

     (error?
      (for/fold ([x :fxvector #f])
          (displayln x)))

     (error?
      (for/fold ([x :fxvector #f :from 10 :to 21])
          (displayln x)))

     (begin (define v (random-fxvector 30))
            (define ls (fxvector->list v))
            #t)

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :fxvector v :from 10 :to 21])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :fxvector v :from 10 :to 21 :step 2])
          (displayln x)
        (cons x acc)))


     ;; :rev

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :fxvector v :from 10 :to 21 :rev])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :fxvector v :from 10 :to 21 :step 2 :rev])
          (displayln x)
        (cons x acc)))


     ;; reorder ops

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :fxvector v :to 21 :from 10])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :fxvector v :step 2 :to 21 :from 10])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :fxvector v :to 21 :from 10 :rev :step 2])
          (displayln x)
        (cons x acc)))

     )


(mat for-flvector

     (error?
      (for/fold ([x :flvector #f])
          (displayln x)))

     (error?
      (for/fold ([x :flvector #f :from 10 :to 21])
          (displayln x)))

     (begin (define v (random-flvector 30))
            (define ls (flvector->list v))
            #t)

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :flvector v :from 10 :to 21])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :flvector v :from 10 :to 21 :step 2])
          (displayln x)
        (cons x acc)))


     ;; :rev

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :flvector v :from 10 :to 21 :rev])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :flvector v :from 10 :to 21 :step 2 :rev])
          (displayln x)
        (cons x acc)))


     ;; reorder ops

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :flvector v :to 21 :from 10])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :flvector v :step 2 :to 21 :from 10])
          (displayln x)
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :flvector v :to 21 :from 10 :rev :step 2])
          (displayln x)
        (cons x acc)))

     )


(mat for-bytevector-ops

     (error?
      (for/fold ([x :bytevector #f])
          (displayln x)))

     (error?
      (for/fold ([x :bytevector #f :from 10 :to 21])
          (displayln x)))

     (begin (define v (bytevector #xA  #xB  #xC  #xD
                                  #xE  #xAA #xBB #xCC
                                  #xDD #xEE #xFF #xAB
                                  #xCD #xEF))
            #t)

     (equal?
      '(#xA #xB #xC #xD #xE #xAA #xBB #xCC #xDD #xEE #xFF #xAB #xCD #xEF)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :bytevector v])
          (println "#x~x" x)
        (cons x acc)))


     (begin (define-syntax test-type
              (syntax-rules ()
                [(_ ty res)
                 (equal?
                  res
                  (for/fold ([:init acc '()]
                             [:finish (reverse acc)]
                             [x :bytevector v :type ty])
                      (println "#x~x" x)
                    (cons x acc)))]))
            #t)

     (test-type u8  '(#xA #xB #xC #xD #xE #xAA #xBB #xCC #xDD #xEE #xFF #xAB #xCD #xEF))
     (test-type u16 '(#xB0A
                      #xD0C
                      #xAA0E
                      #xCCBB
                      #xEEDD
                      #xABFF
                      #xEFCD))
     (test-type u24 '(#xC0B0A
                      #xAA0E0D
                      #xDDCCBB
                      #xABFFEE))
     (test-type u32 '(#xD0C0B0A
                      #xCCBBAA0E
                      #xABFFEEDD))
     (test-type u40 '(#xE0D0C0B0A
                      #xEEDDCCBBAA))
     (test-type u48 '(#xAA0E0D0C0B0A
                      #xABFFEEDDCCBB))
     (test-type u56 '(#xBBAA0E0D0C0B0A
                      #xEFCDABFFEEDDCC))
     (test-type u64 '(#xCCBBAA0E0D0C0B0A))

     (test-type s8  '(#xA
                      #xB
                      #xC
                      #xD
                      #xE
                      #x-56
                      #x-45
                      #x-34
                      #x-23
                      #x-12
                      #x-1
                      #x-55
                      #x-33
                      #x-11))
     (test-type s16 '(#xB0A
                      #xD0C
                      #x-55F2
                      #x-3345
                      #x-1123
                      #x-5401
                      #x-1033))
     (test-type s24 '(#xC0B0A
                      #x-55F1F3
                      #x-223345
                      #x-540012))
     (test-type s32 '(#xD0C0B0A
                      #x-334455F2
                      #x-54001123))
     (test-type s40 '(#xE0D0C0B0A
                      #x-1122334456))
     (test-type s48 '(#x-55F1F2F3F4F6
                      #x-540011223345))
     (test-type s56 '(#x-4455F1F2F3F4F6
                      #x-10325400112234))
     (test-type s64 '(#x-334455F1F2F3F4F6))

     (test-type single '(#x0.00000000000000000000000008C0B0A
                         #x-5DD5070.0
                         #x-0.0000000001FFDDBA))
     (test-type double '(-4.445477901866812e61))


     (begin (define-syntax test-endianness
              (syntax-rules ()
                [(_ end ty res)
                 (equal?
                  res
                  (for/fold ([:init acc '()]
                             [:finish (reverse acc)]
                             [x :bytevector v :type ty end])
                      (println "#x~x" x)
                    (cons x acc)))]))
            #t)

     (test-endianness :little u16 '(#xB0A
                                    #xD0C
                                    #xAA0E
                                    #xCCBB
                                    #xEEDD
                                    #xABFF
                                    #xEFCD))
     (test-endianness :little u24 '(#xC0B0A
                                    #xAA0E0D
                                    #xDDCCBB
                                    #xABFFEE))
     (test-endianness :little u32 '(#xD0C0B0A
                                    #xCCBBAA0E
                                    #xABFFEEDD))
     (test-endianness :little u40 '(#xE0D0C0B0A
                                    #xEEDDCCBBAA))
     (test-endianness :little u48 '(#xAA0E0D0C0B0A
                                    #xABFFEEDDCCBB))
     (test-endianness :little u56 '(#xBBAA0E0D0C0B0A
                                    #xEFCDABFFEEDDCC))
     (test-endianness :little u64 '(#xCCBBAA0E0D0C0B0A))

     (test-endianness :little s16 '(#xB0A
                                    #xD0C
                                    #x-55F2
                                    #x-3345
                                    #x-1123
                                    #x-5401
                                    #x-1033))
     (test-endianness :little s24 '(#xC0B0A
                                    #x-55F1F3
                                    #x-223345
                                    #x-540012))
     (test-endianness :little s32 '(#xD0C0B0A
                                    #x-334455F2
                                    #x-54001123))
     (test-endianness :little s40 '(#xE0D0C0B0A
                                    #x-1122334456))
     (test-endianness :little s48 '(#x-55F1F2F3F4F6
                                    #x-540011223345))
     (test-endianness :little s56 '(#x-4455F1F2F3F4F6
                                    #x-10325400112234))
     (test-endianness :little s64 '(#x-334455F1F2F3F4F6))

     (test-endianness :little single '(#x0.00000000000000000000000008C0B0A
                                       #x-5DD5070.0
                                       #x-0.0000000001FFDDBA))
     (test-endianness :little double '(#x-1BAA0E0D0C0B0A00000000000000000000000000000000000000.0))


     (test-endianness :big u16 '(#xA0B
                                 #xC0D
                                 #x0EAA
                                 #xBBCC
                                 #xDDEE
                                 #xFFAB
                                 #xCDEF))
     (test-endianness :big u24 '(#xA0B0C
                                 #xD0EAA
                                 #xBBCCDD
                                 #xEEFFAB))
     (test-endianness :big u32 '(#xA0B0C0D
                                 #xEAABBCC
                                 #xDDEEFFAB))
     (test-endianness :big u40 '(#xA0B0C0D0E
                                 #xAABBCCDDEE))
     (test-endianness :big u48 '(#xA0B0C0D0EAA
                                 #xBBCCDDEEFFAB))
     (test-endianness :big u56 '(#xA0B0C0D0EAABB
                                 #xCCDDEEFFABCDEF))
     (test-endianness :big u64 '(#xA0B0C0D0EAABBCC))

     (test-endianness :big s16 '(#xA0B
                                 #xC0D
                                 #xEAA
                                 #x-4434
                                 #x-2212
                                 #x-55
                                 #x-3211))
     (test-endianness :big s24 '(#xA0B0C
                                 #xD0EAA
                                 #x-443323
                                 #x-110055))
     (test-endianness :big s32 '(#xA0B0C0D
                                 #xEAABBCC
                                 #x-22110055))
     (test-endianness :big s40 '(#xA0B0C0D0E
                                 #x-5544332212))
     (test-endianness :big s48 '(#xA0B0C0D0EAA
                                 #x-443322110055))
     (test-endianness :big s56 '(#xA0B0C0D0EAABB
                                 #x-33221100543211))
     (test-endianness :big s64 '(#xA0B0C0D0EAABBCC))

     (test-endianness :big single '(#x0.0000000000000000000000000022C3034
                                    #x0.000000000000000000000000555DE6
                                    #x-1DDFF56000000000.0))
     (test-endianness :big double '(2.748615887074701e-260))


     (begin (define-syntax test-rev
              (syntax-rules ()
                [(_ end ty res)
                 (equal?
                  res
                  (for/fold ([:init acc '()]
                             [:finish (reverse acc)]
                             [x :bytevector v end :type ty :rev])
                      (println "#x~x" x)
                    (cons x acc)))]))
            #t)

     (test-rev :little u8 (reverse '(#xA #xB #xC #xD #xE #xAA #xBB #xCC #xDD #xEE #xFF #xAB #xCD #xEF)))
     (test-rev :little u16 (reverse '(#xB0A
                                      #xD0C
                                      #xAA0E
                                      #xCCBB
                                      #xEEDD
                                      #xABFF
                                      #xEFCD)))
     (test-rev :little u56 (reverse '(#xBBAA0E0D0C0B0A
                                      #xEFCDABFFEEDDCC)))

     (test-rev :big u8 (reverse '(#xA #xB #xC #xD #xE #xAA #xBB #xCC #xDD #xEE #xFF #xAB #xCD #xEF)))
     (test-rev :big u16 (reverse '(#xA0B
                                   #xC0D
                                   #x0EAA
                                   #xBBCC
                                   #xDDEE
                                   #xFFAB
                                   #xCDEF)))
     (test-rev :big u56 (reverse '(#xA0B0C0D0EAABB
                                   #xCCDDEEFFABCDEF)))
     )


(mat for-hashtable

     (error?
      (for/fold ([:index i]
                 [(k v) :hashtable '()])
          (void)))

     (begin (define ht (make-eqv-hashtable))
            (for/fold ([i 10])
                (hashtable-set! ht i (* i i)))
            #t)

     (let ([vec (make-vector 10)])
       (for/fold ([:index i]
                  [(k v) :hashtable ht])
           (println "~a -> ~a" k v)
           (vector-set! vec i (cons k v)))
       (equal? vec
               (hashtable-cells ht)))

     )


(mat for-hashtable-keys

     (error?
      (for/fold ([:index i]
                 [x :hashtable-keys '()])
          (void)))

     (begin (define ht (make-eqv-hashtable))
            (for/fold ([i 10])
                (hashtable-set! ht i (* i i)))
            #t)

     (let ([vec (make-vector 10)])
       (for/fold ([:index i]
                  [x :hashtable-keys ht])
           (vector-set! vec i x))
       (equal? vec
               (hashtable-keys ht)))

     )


(mat for-hashtable-values

     (error?
      (for/fold ([x :hashtable-values '()])
          (void)))

     (begin (define ht (make-eqv-hashtable))
            (for/fold ([i 10])
                (hashtable-set! ht i (* i i)))
            #t)

     (let ([vec (make-vector 10)])
       (for/fold ([:index i]
                  [x :hashtable-values ht])
           (vector-set! vec i x))
       (equal? vec
               (hashtable-values ht)))

     )


(mat for-iter

     (error?
      (for/fold ([x :iter #t])
          (void)))
     (error?
      (for/fold ([x :iter (vector)])
          (void)))
     (error?
      (for/fold ([x :iter (list)])
          (void)))

     ;; list
     (let ([ls (random-list 100)])
       (equal?
        (reverse ls)
        (for/fold ([:init acc '()]
                   [:finish acc]
                   [x :iter (list->iter ls)])
            (cons x acc))))


     ;; vector
     (let ([vec (random-vector 100)])
       (equal?
        (vreverse! vec)
        (for/fold ([:init acc '()]
                   [:finish (list->vector acc)]
                   [x :iter (vector->iter vec)])
            (cons x acc))))

     )



(mat for

;;;; simple literal

     (let ([lb (make-list-builder)])
       (for ([x 10])
         (lb x))
       (equal? (lb)
               (iota 10)))

     (let ([lb (make-list-builder)])
       (for ([x "abcd"])
         (lb x))
       (equal? (lb)
               '(#\a #\b #\c #\d )))

     (let ([lb (make-list-builder)])
       (for ([x '#(1 2 3)])
         (lb x))
       (equal? (lb)
               '(1 2 3)))


;;;; single var

     (let ([lb (make-list-builder)])
       (for ([x :iota 10])
         (lb x))
       (equal? (lb)
               (iota 10)))

     (let ([lb (make-list-builder)])
       (for ([x :nums 10])
         (lb x))
       (equal? (lb)
               (nums 10)))

     (let ([lb (make-list-builder)])
       (for ([x :nums 10 20])
         (lb x))
       (equal? (lb)
               (nums 10 20)))

     (let ([lb (make-list-builder)])
       (for ([x :nums -33 33 2.5])
         (lb x))
       (equal? (lb)
               (nums -33 33 2.5)))

     (let ([lb (make-list-builder)])
       (for ([x :nums 33 -33 -2.5])
         (lb x))
       (equal? (lb)
               (nums 33 -33 -2.5)))

     (let ([lb (make-list-builder)]
           [ls (random-list 10 20)])
       (for ([x :list ls])
         (lb x))
       (equal? (lb)
               ls))

     (let ([lb (make-list-builder)]
           [vec (random-vector 10 20)])
       (for ([x :vector vec])
         (lb x))
       (equal? (lb)
               (vector->list vec)))

     ;; multiple clauses

     (let ([ns (iota 5)]
           [str "abcde"]
           [vec '#(Q W E R T)]
           [lb (make-list-builder)])
       (for ([x 5]
             [y "abcde"]
             [z '#(Q W E R T)])
         (lb (list x y z)))
       (equal? (lb)
               (zip ns (string->list str) (vector->list vec))))


     (let ([ns (iota 10)]
           [ls (random-list 10 11)]
           [vec (random-vector 10 11)]
           [lb (make-list-builder)])
       (for ([x :iota 10]
             [y :list ls]
             [z :vector vec])
         (lb (list x y z)))
       (equal? (lb)
               (zip ns ls (vector->list vec))))

     ;; differet lengths

     (let ([ns (iota 5)]
           [str "abcdefg"]
           [vec '#(Q W E R T Y U I O P)]
           [lb (make-list-builder)])
       (for ([x 5]
             [y "abcdefg"]
             [z '#(Q W E R T Y U I O P)])
         (lb (list x y z)))
       (equal? (lb)
               (zip ns (string->list "abcde") (vector->list '#(Q W E R T)))))


     (let ([ns (iota 10)]
           [ls (random-list 10 20)]
           [vec (random-vector 10 20)]
           [lb (make-list-builder)])
       (for ([x :iota 10]
             [y :list ls]
             [z :vector vec])
         (lb (list x y z)))
       (equal? (lb)
               (zip ns (slice ls 10) (slice (vector->list vec) 10))))

     )


(mat for/list

     (equal? (iota 10)
             (for/list ([x 10])
               x))

     (equal? (iota 10)
             (for/list ([x :iota 10])
               x))

     (equal? (iota 10)
             (for/list ([x :fixnums 10])
               x))

     (equal? (nums -10 10)
             (for/list ([x :nums -10 10])
               x))

     (equal? (nums -10 10 2.2)
             (for/list ([x :nums -10 10 2.2])
               x))

     )


(mat for/vector

     (equal? (viota 10)
             (for/vector ([i 10])
               i))

     (equal? '#(0 1 2 3 4 5 6 7 8 9 #f #f #f #f #f)
             (for/vector ([:length 15]
                          [i 10])
               i))

     (equal? (viota 10)
             (for/vector ([:fill 42]
                          [i 10])
               i))

     (equal? '#(0 1 2 3 4 5 6 7 8 9 42 42 42 42 42)
             (for/vector ([:length 15]
                          [:fill 42]
                          [i 10])
               i))

;;;; with index
     (equal? '#(0 2 4 6 8 10 12 14 16 18 #f #f #f #f #f)
             (for/vector ([:index j]
                          [:length 15]
                          [i 10])
               (+ i j)))

     (equal? '#(0 2 4 6 8 10 12 14 16 18)
             (for/vector ([:index j]
                          [:fill 42]
                          [i 10])
               (+ i j)))

     (equal? '#(0 2 4 6 8 10 12 14 16 18 42 42 42 42 42)
             (for/vector ([:index j]
                          [:length 15]
                          [:fill 42]
                          [i 10])
               (+ i j)))

;;;; multiple clauses
     (equal? '#((0 #\a Q) (1 #\b W) (2 #\c E) 42 42)
             (for/vector ([:length 5]
                          [:fill 42]
                          [x 3]
                          [y "abc"]
                          [z '#(Q W E)])
               (list x y z)))

     (equal? '#((0 #\a Q) (1 #\b W) (2 #\c E) 42 42)
             (for/vector ([:length 5]
                          [:fill 42]
                          [x 3]
                          [y "abc"]
                          [z '#(Q W E)])
               (list x y z)))

     (equal? '#((0 #\a Q) (1 #\b W) (2 #\c E) 42 42)
             (for/vector ([:length 5]
                          [:fill 42]
                          [x 3]
                          [y "abc"]
                          [z '#(Q W E)])
               (list x y z)))

     )



(mat for*/fold

     (= (for*/fold ([:index i]
                    [:finish i]
                    [x 1000]
                    [y 1000]
                    [z 1000])
            (void))
        #e1e9)

     ;; index counts the number of times the body is actually executed
     (= (for*/fold ([:init acc 0]
                    [:index i]
                    [:finish i]
                    [x 1000]
                    [:guard (< x 500)]
                    [y 1000]
                    [:guard (< y 500)]
                    [z 1000]
                    [:guard (< z 500)])
            (fx1+ acc))
        (* 500 500 500))

     (= (for*/fold ([:init acc 0]
                    [:index i]
                    [:finish i]
                    [x 1000]
                    [:guard (< x 500)]
                    [y 1000]
                    [:guard (< y 400)]
                    [z 1000]
                    [:guard (< z 300)])
            (fx1+ acc))
        (* 500 400 300))


     ;; TODO clauses are init'ed once

     (let ([lb (make-list-builder)])
       (and (= 42
               (for*/fold ([x 10]
                           [:break (= x 5)]
                           [y 10]
                           [:break (= y 4)]
                           [z 10]
                           [:stop (= z 3)
                                  (begin (displayln "early quit...")
                                         42)])
                   (lb (list x y z))))
            (equal? (lb)
                    '((0 0 0)
                      (0 0 1)
                      (0 0 2)))))

     (let ([lb (make-list-builder)])
       (and (= 42
               (for*/fold ([x 10]
                           [:break (= x 5)]
                           [y 10]
                           [:stop (= y 2)
                                  (begin (displayln "early quit...")
                                         42)]
                           [z 3])
                   (lb (list x y z))))
            (equal? (lb)
                    '((0 0 0)
                      (0 0 1)
                      (0 0 2)
                      (0 1 0)
                      (0 1 1)
                      (0 1 2)))))

     (let ([lb (make-list-builder)])
       (and (= 42
               (for*/fold ([x 10]
                           [:stop (= x 2)
                                  (begin (displayln "early quit...")
                                         42)]
                           [y 2]
                           [:break (= y 4)]
                           [z 3])
                   (lb (list x y z))))
            (equal? (lb)
                    '((0 0 0) (0 0 1) (0 0 2) (0 1 0) (0 1 1) (0 1 2)
                      (1 0 0) (1 0 1) (1 0 2) (1 1 0) (1 1 1) (1 1 2)))))

     )

(mat for*

     (let ([s 0])
       (for* ([x 1000]
              [y 100]
              [z 100])
         (set! s (fx1+ s)))
       (= s #e1e7))

     (let ([s 0])
       (for* ([:index i]
              [x 1000]
              [y 100]
              [z 100])
         (set! s i))
       (= s (fx1- #e1e7)))

     ;; index counts the number of times the body is actually executed
     (let ([s 0])
       (for* ([x 1000]
              [:guard (< x 500)]
              [y 1000]
              [:guard (< y 500)]
              [z 1000]
              [:guard (< z 500)])
         (set! s (fx1+ s)))
       (= s (* 500 500 500)))

     (let ([s 0])
       (for* ([:index i]
              [x 1000]
              [:guard (< x 500)]
              [y 1000]
              [:guard (< y 400)]
              [z 1000]
              [:guard (< z 300)])
         (set! s i))
       (= s (fx1- (* 500 400 300))))
     )


(mat for*/list

     (null?
      (for*/list ([x 0])
        x))

     (null?
      (for*/list ([x 0] [y :list '()] [z :vector (vector)])
        (list x y z)))

     (equal?
      '((0 #\a Q) (0 #\a W) (0 #\a E) (0 #\b Q) (0 #\b W) (0 #\b E) (0 #\c Q) (0 #\c W) (0 #\c E)
        (1 #\a Q) (1 #\a W) (1 #\a E) (1 #\b Q) (1 #\b W) (1 #\b E) (1 #\c Q) (1 #\c W) (1 #\c E)
        (2 #\a Q) (2 #\a W) (2 #\a E) (2 #\b Q) (2 #\b W) (2 #\b E) (2 #\c Q) (2 #\c W) (2 #\c E))
      (for*/list ([x 3]
                  [y "abc"]
                  [z '#(Q W E)])
        (list x y z)))

     (equal?
      '((0 #\a Q) (1 #\a W) (2 #\a E) (3 #\b Q) (4 #\b W) (5 #\b E) (6 #\c Q) (7 #\c W) (8 #\c E))
      (for*/list ([:index i]
                  [y "abc"]
                  [z '#(Q W E)])
        (list i y z)))

     )


(mat for*/vector

     (equal? (vector)
             (for*/vector ([:length 0]
                           [x 4])
               x))

     (equal? (vector 0)
             (for*/vector ([:length 1]
                           [x 4])
               x))

     ;; default fill is #f
     (equal? (vector 0 1 2 3 #f)
             (for*/vector ([:length 5]
                           [x 4])
               x))

     (equal? (vector 0 1 2 3 'bla 'bla 'bla 'bla)
             (for*/vector ([:length 8]
                           [:fill 'bla]
                           [x 4])
               x))

     ;; TODO multiple clauses

     (equal?
      (for*/vector ([:length 30]
                    [:fill 42]
                    [x 3]
                    [y "abc"]
                    [z '#(Q W E)])
        (list x y z))
      (for*/vector ([:length 30]
                    [:fill 42]
                    [x 5]
                    [:guard (< x 3)]
                    [y "abcde"]
                    [:guard (char<? y #\d)]
                    [z '#(Q W E R T)]
                    [:guard (memq z '(Q W E))])
        (list x y z)))

     )

(mat numeric-range-boundaries
     ;; Fractional bounds must stop after crossing the exclusive upper bound.
     (equal? '(0 1 2)
             (for/list ([x :nums 5/2] [:break (> x 5)]) x))

     (equal? (list (- (most-positive-fixnum) 1))
             (for/list ([x :fixnums (- (most-positive-fixnum) 1)
                                   (most-positive-fixnum) 2]) x))

     (equal? (list (+ (most-negative-fixnum) 1))
             (for/list ([x :fixnums (+ (most-negative-fixnum) 1)
                                   (most-negative-fixnum) -2]) x))

     ;; :iota accepts natural bounds beyond the fixnum range.
     (equal? '(0 1 2)
             (for/list ([x :iota (+ 1 (most-positive-fixnum))]
                        [:break (= x 3)]) x)))

(mat bytevector-stride-and-hygiene
     (equal? '(2571 1543 515)
             (for/list ([x :bytevector #vu8(0 1 2 3 4 5 6 7 8 9 10 11)
                           :type u16 :big :rev :step 2]) x))

     (equal? '(2312 1284 256)
             (for/list ([x :bytevector #vu8(0 1 2 3 4 5 6 7 8 9 10 11)
                           :type u16 :little :from 0 :to 10 :rev :step 2]) x))

     ;; Caller bindings must not replace the generated primitive accessor.
     (let ([bytevector-u16-ref (lambda args 42)]
           [endianness (lambda args 'little)])
       (equal? '(1 515)
               (for/list ([x :bytevector #vu8(0 1 2 3) :type u16 :big]) x))))

(mat vector-configuration-and-capacity
     (equal? '#(1 3) (for/vector ([x 5] [:guard (odd? x)]) x))

     (equal? '#(0 1) (for/vector ([x 5] [:break (= x 2)]) x))

     (equal? '#(1 3 #f)
             (for/vector ([:length 3] [x 5] [:guard (odd? x)]) x))

     (equal? '#((1 1) (3 3))
             (for/vector ([:index i] [:length 2] [x 5] [:guard (odd? x)])
               (list i x)))

     (equal? '#(1 2 3)
             (for/vector ([x 3] [:let y (+ x 1)]) y))

     ;; Filling a nested vector must stop all levels immediately.
     (let ([count 0])
       (and (equal? '#((0 0))
                    (for*/vector ([:length 1] [x 5]
                                  [:let ignored (begin (set! count (+ count 1)) #f)]
                                  [y 2])
                      (list x y)))
            (= count 1)))

     ;; A bounded witness makes failure terminate even with an unbounded source.
     (let ([count 0])
       (equal? '#((0 0))
               (for*/vector ([:length 1] [x :nums +inf.0]
                             [:let ignored (begin
                                             (set! count (+ count 1))
                                             (when (> count 2)
                                               (error 'vector-capacity "failed to stop")))]
                             [y 2])
                 (list x y))))

     ;; A zero-length comprehension must not evaluate its iterator source.
     (equal? '#()
             (for*/vector ([:length 0] [x :iota (error 'vector-capacity "source evaluated")])
               x)))

(mat nested-iterator-initialization
     (equal? '((1 0) (2 0) (2 1))
             (for*/list ([x 3] [y :iota x]) (list x y)))

     (equal? '((0 0) (1 0) (1 1))
             (for*/list ([x 2] [:let bound (+ x 1)] [y :iota bound]) (list x y)))

     ;; The inner source must use the loop binding rather than its outer namesake.
     (let ([x 5])
       (equal? '((1 0) (2 0) (2 1))
               (for*/list ([x 3] [y :iota x]) (list x y))))

     (let ([count 0])
       (and (equal? '((0 0) (1 0) (2 0))
                    (for*/list ([x 3] [y :iota (begin (set! count (+ count 1)) 1)])
                      (list x y)))
            (= count 3)))

     (equal? '((0 a) (0 b) (1 a) (1 b) (2 a) (2 b))
             (for*/list ([x 3] [y :iter (list->iter '(a b))]) (list x y)))

     ;; Inner setup must not run when an outer guard rejects the iteration.
     (equal? '()
             (for*/list ([x 3] [:guard #f]
                          [y :iota (error 'nested-setup "source evaluated")]) y))

     (= 5 (for*/fold ([:init sum 0] [:finish sum] [x 3] [y :iota x])
            (+ sum x))))

(mat documented-binding-clauses
     (equal? '(1 2 3) (for/list ([x 3] [:let* y (+ x 1)]) y))

     (equal? '(1 1 2)
             (for/list ([x 3]
                        [:letrec fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))])
               (fact x)))

     (equal? '(1 1 2)
             (for/list ([x 3]
                        [:letrec* fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))])
               (fact x)))

     (equal? '((0 1) (1 2) (2 3))
             (for/list ([x 3] [:let*-values (y z) (values x (+ x 1))]) (list y z)))

     (equal? '((0 1) (1 1) (1 2))
             (for*/list ([x 2] [:let* n (+ x 1)] [y :iota n]
                          [:letrec fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))]
                          [:letrec* f (lambda (n) (if (zero? n) (fact x) (f (- n 1))))]
                          [:let*-values (z) (values (+ (f y) y))])
               (list x z))))

(mat guard-local-indices
     (equal? '((1 0) (3 1) (5 2))
             (for/list ([x 6] [:guard (odd? x) :index j]) (list x j)))

     ;; Later guards do not undo a pass through an earlier indexed guard.
     (equal? '((3 1) (5 2))
             (for/list ([x 6] [:guard (odd? x) :index j] [:guard (> x 1)]) (list x j)))

     (= 3 (for/fold ([:init sum 0] [:finish sum]
                     [x 6] [:guard (odd? x) :index j]) (+ sum j)))

     ;; Each inner loop has its own index starting from zero.
     (equal? '((1 0 1 0) (1 0 2 1) (3 1 1 0) (3 1 2 1))
             (for*/list ([x 4] [:guard (odd? x) :index i]
                          [y 3] [:guard (> y 0) :index j])
               (list x i y j)))

     (= 2 (for*/fold ([:init sum 0] [:finish sum] [x 2]
                      [y 5] [:guard (odd? y) :index j]) (+ sum j)))

     (equal? '#(0 1 #f)
             (for/vector ([:length 3] [x 4] [:guard (odd? x) :index j]) j)))

(mat one-expression-stop
     (let ([seen (make-list-builder)])
       (and (eq? (void)
                 (for* ([x 3] [y 4] [:stop (= y 2)]) (seen (list x y))))
            (equal? '((0 0) (0 1)) (seen))))

     (equal? '((0 0) (0 1))
             (for*/list ([x 3] [y 4] [:stop (= y 2)]) (list x y))))

(mat hygienic-loop-state
     ;; Later bindings must not replace the numeric induction state.
     (equal? '(100 100 100)
             (for/list ([x 3] [:break (> x 5)] [:let x 100]) x))

     (equal? '(100 100 100)
             (for/list ([x :iota 3] [:break (> x 5)] [:let x 100]) x))

     (equal? '(100 100 100)
             (for/list ([x :nums 3] [:break (> x 5)] [:let x 100]) x))

     (equal? '(100 100 100)
             (for/list ([x :fixnums 3] [:break (> x 5)] [:let x 100]) x))

     (equal? '(0 1 0 1 0 1) (for*/list ([x 3] [x 2]) x))

     ;; A rejected guard preserves the accumulator even when its public name is shadowed.
     (= 0 (for/fold ([:init sum 0] [:finish sum] [x 3]
                     [:let sum 100] [:guard #f]) (+ sum x)))

     (= 0 (for*/fold ([:init sum 0] [:finish sum] [x 3]
                      [:let sum 100] [:guard #f] [y 2]) (+ sum y)))

     (= 3 (for/fold ([:index i] [:finish i] [x 3] [:let i 100]) i))

     (= 6 (for*/fold ([:index i] [:finish i] [x 3] [y 2] [:let i 100]) i))

     ;; Mutating the exposed item does not mutate the private induction variable.
     (equal? '(0 1 2)
             (for/list ([x 3] [:break (> x 5)])
               (let ([original x]) (set! x 100) original)))

     ;; Global state can be shadowed by the innermost iterator without losing progress.
     (= 6 (for*/fold ([:index i] [:finish i] [x 3] [i 2]) (void))))

(mat nested-state-binding-scope
     (equal? '(100 100 100 100)
             (for*/list ([:index i] [x 2] [:let i 100] [y 2]) i))

     (= 100 (for*/fold ([:init sum 0] [:finish sum] [x 2]
                        [:let sum 100] [y 2]) (+ sum 0)))

     ;; Outer binding clauses keep their lexical scope inside inner iterators.
     (equal? '(100 100 100 100)
             (for*/list ([:index i] [x 2] [:let-values (i) (values 100)] [y 2]) i)))
