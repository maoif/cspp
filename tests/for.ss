(import (cspp) (cspp private for))

(mat keyword-predicate-consistency
     (and (kw-for-config-clause? #':letrec*)
          (kw-for-config-clause? #':let*-values)
          (kw-for*-config-clause? #':letrec*)
          (kw-for*-config-clause? #':let*-values)
          (not (kw-for-config-clause? #':index))
          (kw-for*-config-clause? #':index)
          (not (kw-for-config-clause? #':stop))
          (kw-for*-config-clause? #':stop)))


;;;; When the macro does not exist, the error is
;;;; Exception: variable x is not bound
;;;; This should be improved.
#;
(fold/fold ([x :iota 20]
            [:guard (odd? x)])
           (void))



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
                                       (cons 'res x)])
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
          (void)))

     (error?
      (for/fold ([x :string #f :from 10 :to 21])
          (void)))

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
        (cons x acc)))

     (equal?
      (slice (string->list str) 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :string str :from 10 :to 21 :step 2])
        (cons x acc)))

     ;; :rev

     (equal?
      (slice (string->list str) 10 21)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :string str :from 10 :to 21 :rev])
        (cons x acc)))

     (equal?
      (slice (string->list str) 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :string str :from 10 :to 21 :step 2 :rev])
        (cons x acc)))

     ;; reorder ops

     (equal?
      (substring str 10 21)
      (for/fold ([:init acc '()]
                 [:finish (apply string (reverse acc))]
                 [x :string str :to 21 :from 10])
        (cons x acc)))

     (equal?
      (slice (string->list str) 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :string str :step 2 :to 21 :from 10])
        (cons x acc)))

     (equal?
      (slice (string->list str) 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :string str :to 21 :from 10 :rev :step 2])
        (cons x acc)))

     )


(mat for-vector

     (error?
      (for/fold ([x :vector #f])
          (void)))

     (error?
      (for/fold ([x :vector #f :from 10 :to 21])
          (void)))

     (begin (define v (random-vector 30))
            (define ls (vector->list v))
            #t)

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :vector v :from 10 :to 21])
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :vector v :from 10 :to 21 :step 2])
        (cons x acc)))


     ;; :rev

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :vector v :from 10 :to 21 :rev])
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :vector v :from 10 :to 21 :step 2 :rev])
        (cons x acc)))


     ;; reorder ops

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :vector v :to 21 :from 10])
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :vector v :step 2 :to 21 :from 10])
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :vector v :to 21 :from 10 :rev :step 2])
        (cons x acc)))

     )


(mat for-fxvector

     (error?
      (for/fold ([x :fxvector #f])
          (void)))

     (error?
      (for/fold ([x :fxvector #f :from 10 :to 21])
          (void)))

     (begin (define v (random-fxvector 30))
            (define ls (fxvector->list v))
            #t)

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :fxvector v :from 10 :to 21])
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :fxvector v :from 10 :to 21 :step 2])
        (cons x acc)))


     ;; :rev

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :fxvector v :from 10 :to 21 :rev])
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :fxvector v :from 10 :to 21 :step 2 :rev])
        (cons x acc)))


     ;; reorder ops

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :fxvector v :to 21 :from 10])
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :fxvector v :step 2 :to 21 :from 10])
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :fxvector v :to 21 :from 10 :rev :step 2])
        (cons x acc)))

     )


(mat for-flvector

     (error?
      (for/fold ([x :flvector #f])
          (void)))

     (error?
      (for/fold ([x :flvector #f :from 10 :to 21])
          (void)))

     (begin (define v (random-flvector 30))
            (define ls (flvector->list v))
            #t)

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :flvector v :from 10 :to 21])
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :flvector v :from 10 :to 21 :step 2])
        (cons x acc)))


     ;; :rev

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :flvector v :from 10 :to 21 :rev])
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :flvector v :from 10 :to 21 :step 2 :rev])
        (cons x acc)))


     ;; reorder ops

     (equal?
      (slice ls 10 21)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :flvector v :to 21 :from 10])
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish (reverse acc)]
                 [x :flvector v :step 2 :to 21 :from 10])
        (cons x acc)))

     (equal?
      (slice ls 10 21 2)
      (for/fold ([:init acc '()]
                 [:finish acc]
                 [x :flvector v :to 21 :from 10 :rev :step 2])
        (cons x acc)))

     )


(mat for-bytevector-ops

     (error?
      (for/fold ([x :bytevector #f])
          (void)))

     (error?
      (for/fold ([x :bytevector #f :from 10 :to 21])
          (void)))

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
        (cons x acc)))


     (begin (define-syntax test-type
              (syntax-rules ()
                [(_ ty res)
                 (equal?
                  res
                  (for/fold ([:init acc '()]
                             [:finish (reverse acc)]
                             [x :bytevector v :type ty])
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

     ;; A fill value requires an explicit length.
     (error? (eval '(for/vector ([:fill 42] [i 10]) i)))

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

     ;; A leading global index does not make a fill-only form valid.
     (error? (eval '(for/vector ([:index j] [:fill 42] [i 10]) (+ i j))))

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
                                  42])
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
                                  42]
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
                                  42]
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

(mat fxiota
     (equal? '(0 1 2 3) (for/list ([x :fxiota 4]) x))

     (equal? '() (for/list ([x :fxiota 0]) x))

     (equal? '#(0 1 2) (for/vector ([x :fxiota 3]) x))

     (= 6 (for/fold ([:init sum 0] [:finish sum] [x :fxiota 4]) (+ sum x)))

     (let ([sum 0])
       (for ([x :fxiota 4]) (set! sum (+ sum x)))
       (= sum 6))

     (equal? '((1 0) (2 0) (2 1))
             (for*/list ([x :fxiota 3] [y :fxiota x]) (list x y)))

     (equal? '#((1 0) (2 0) (2 1))
             (for*/vector ([x :fxiota 3] [y :fxiota x]) (list x y)))

     (= 1 (for*/fold ([:init sum 0] [:finish sum]
                       [x :fxiota 3] [y :fxiota x]) (+ sum y)))

     (let ([sum 0])
       (for* ([x :fxiota 3] [y :fxiota x]) (set! sum (+ sum y)))
       (= sum 1))

     (equal? '((1 1 a) (3 3 b))
             (for/list ([x :fxiota 5 :index j :guard (odd? x)]
                        [y :list '(a b c)]) (list x j y)))

     (let ([calls 0])
       (and (equal? '(0 1 2)
                    (for/list ([x :fxiota (begin (set! calls (+ calls 1)) 3)]) x))
            (= calls 1)))

     ;; The largest fixnum is a valid exclusive bound; break keeps the test short.
     (equal? '(0 1 2)
             (for/list ([x :fxiota (most-positive-fixnum)] [:break (= x 3)]) x))

     ;; Public bindings must not capture private induction state.
     (equal? '(100 100 100)
             (for/list ([x :fxiota 3] [:let x 100]) x))

     (equal? '(0 1 0 1 0 1)
             (for*/list ([x :fxiota 3] [x :fxiota 2]) x))

     ;; Negative, inexact, nonnumeric, and bignum bounds must be rejected.
     (error? (for/list ([x :fxiota -1]) x))

     (error? (for/list ([x :fxiota 3.0]) x))

     (error? (for/list ([x :fxiota 'three]) x))

     (error? (for/list ([x :fxiota (+ 1 (most-positive-fixnum))]) x))

     ;; Exactly one bound and one item binding are required; slicing is unsupported.
     (error? (eval '(for/list ([x :fxiota]) x)))

     (error? (eval '(for/list ([x :fxiota 1 2]) x)))

     (error? (eval '(for/list ([(x y) :fxiota 3]) x)))

     (error? (eval '(for/list ([x :fxiota 3 :step 2]) x))))

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

     (equal? '#((0 1) (1 3))
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

(mat global-index-after-guards
     (equal? '((1 0) (3 1))
             (for/list ([:index i] [x 5] [:guard (odd? x)]) (list x i)))

     ;; Rejecting a tuple preserves the body count and accumulator for both guard forms.
     (= 0 (for/fold ([:index i] [:finish i] [x 5] [:guard #f]) (void)))

     (= 0 (for/fold ([:index i] [:finish i] [x 5] [:guard #f :index passed]) (void)))

     (equal? '(4 2)
             (for/fold ([:init sum 0] [:index i] [:finish (list sum i)]
                        [x 6] [:guard (odd? x) :index passed] [:guard (> x 1)])
               (+ sum passed i)))

     (equal? '#((1 0) (3 1) (5 2))
             (for/vector ([:index i] [:length 3] [x 6] [:guard (odd? x)]) (list x i)))

     ;; Parallel sources share the accepted-body count, including across iterator-local filters.
     (equal? '((1 b 0) (3 d 1))
             (for/list ([:index i] [x :fxiota 5] [y :list '(a b c d e)]
                        [:guard (odd? x)]) (list x y i))))

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

(mat nested-local-index
     (equal? '((0 0 0) (0 0 1) (1 1 0) (1 1 1))
             (for*/list ([x 2] [:index i] [y 2]) (list x i y)))

     ;; Each counter resets when its loop level is entered, even after an empty inner source.
     (equal? '((1 1 0 0) (2 2 0 0) (2 2 1 1))
             (for*/list ([x 3] [:index i] [y :fxiota x] [:index j]) (list x i y j)))

     ;; Counters at different clause positions count only visits reaching that position.
     (equal? '((3 3 1 0) (5 5 2 1))
             (for*/list ([x 6] [:index before] [:guard (odd? x)] [:index odd]
                          [:guard (> x 1)] [:index accepted])
               (list x before odd accepted)))

     ;; An iterator-local guard runs before the standalone counter; source positions stay intact.
     (equal? '((d 3 0) (b 1 1))
             (for*/list ([x :vector '#(a b c d) :rev :index source :guard (odd? source)]
                          [:index local])
               (list x source local)))

     ;; The global index counts bodies, while an outer local index counts outer visits.
     (equal? '((0 0 0 0) (1 0 0 1) (2 1 1 0) (3 1 1 1))
             (for*/list ([:index global] [x 2] [:index i] [y 2])
               (list global x i y)))

     ;; A same-named local index shadows the global alias without replacing finish state.
     (= 6 (for*/fold ([:index i] [:finish i] [x 2] [:index i] [y 3]) (void)))

     ;; Mutation and later shadowing of the public alias must not change its private counter.
     (equal? '(0 1 2)
             (for*/list ([x 3] [:index i])
               (let ([original i]) (set! i 100) original)))

     (equal? '(100 100 100)
             (for*/list ([x 3] [:index i] [:let i 100] [y 1]) i))

     ;; A local alias remains visible in inner source setup and may shadow the accumulator.
     (equal? '((1 0) (2 0) (2 1))
             (for*/list ([x 3] [:index i] [y :fxiota i]) (list i y)))

     (= 3 (for*/fold ([:init sum 0] [:finish sum] [x 3] [:index sum] [y 2])
            (+ sum 1)))

     (let ([sum 0])
       (for* ([x 2] [:index i] [y 3] [:index j])
         (set! sum (+ sum i j)))
       (= sum 9))

     (equal? '#(0 1 0 1)
             (for*/vector ([x 2] [y 2] [:index i]) i))

     ;; Capacity limits and control clauses still exit the appropriate loop levels.
     (equal? '#(0 1 0)
             (for*/vector ([:length 3] [x 2] [y 2] [:index i]) i))

     (equal? '((0 0) (0 1) (1 0) (1 1))
             (for*/list ([x 2] [y 4] [:index i] [:break (= i 2)]) (list x i)))

     (equal? '((0 0) (0 1))
             (for*/list ([x 2] [y 4] [:index i] [:stop (= i 2)]) (list x i)))

     ;; Standalone local indexes require an identifier and remain specific to nested loops.
     (error? (eval '(for*/list ([x 2] [:index 0]) x)))

     (error? (eval '(for*/list ([x 2] [:index]) x)))

     (error? (eval '(for*/list ([x 2] [:index i extra]) x)))

     (error? (eval '(for/list ([x 2] [:index i]) x))))

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

(mat large-literal-range
     (equal? '(0 1 2)
             (for/list ([x 1152921504606846976] [:break (= x 3)]) x))

     (equal? '(0 1 2)
             (for*/list ([x 1152921504606846976] [:break (= x 3)]) x)))

(mat large-sequence-strides
     (equal? '(2)
             (for/list ([x :vector '#(1 2 3) :from 1 :step (most-positive-fixnum)]) x))

     (equal? '(1)
             (for/list ([x :vector '#(1 2 3) :step (+ 1 (most-positive-fixnum))]) x))

     (equal? '(#\b)
             (for/list ([x :string "abc" :from 1 :step (most-positive-fixnum)]) x))

     (equal? '(3)
             (for/list ([x :fxvector (fxvector 1 2 3) :rev :step (most-positive-fixnum)]) x))

     (equal? '(3.0)
             (for/list ([x :flvector (flvector 1.0 2.0 3.0)
                           :rev :step (+ 1 (most-positive-fixnum))]) x))

     (equal? '(256)
             (for/list ([x :bytevector #vu8(0 1) :type u16
                           :step (most-positive-fixnum)]) x))

     (equal? '(515)
             (for/list ([x :bytevector #vu8(0 1 2 3) :type u16 :big :rev
                           :step (+ 1 (most-positive-fixnum))]) x))

     ;; Ordinary non-dividing strides still stop at the exclusive endpoint.
     (equal? '(1 3 5) (for/list ([x :vector '#(1 2 3 4 5) :step 2]) x))

     (equal? '(5 3 1) (for/list ([x :vector '#(1 2 3 4 5) :rev :step 2]) x)))

(mat iterator-local-options
     (equal? '((1 a) (3 b))
             (for/list ([x :vector '#(1 2 3) :guard (odd? x)]
                        [y :list '(a b c)]) (list x y)))

     (equal? '((1 0) (3 2))
             (for/list ([x :list '(1 2 3) :index j :guard (odd? x)]) (list x j)))

     (equal? '((5 4) (3 2) (1 0))
             (for/list ([x :vector '#(1 2 3 4 5) :rev :step 2 :index j]) (list x j)))

     (equal? '((5 4) (3 2) (1 0))
             (for/list ([x :list '(1 2 3 4 5) :rev :step 2 :index j]) (list x j)))

     (equal? '(2 4)
             (for/list ([x :list '(0 1 2 3 4 5) :from 2 :to 5 :step 2]) x))

     (equal? '((#\d 3) (#\b 1))
             (for/list ([x :string "abcde" :rev :index j :guard (odd? j)]) (list x j)))

     (equal? '((30 2) (20 1))
             (for/list ([x :fxvector (fxvector 10 20 30) :rev :index j :guard (> x 10)])
               (list x j)))

     (equal? '((3.0 2) (1.0 0))
             (for/list ([x :flvector (flvector 1.0 2.0 3.0) :rev :step 2 :index j])
               (list x j)))

     (equal? '((515 2) (1 0))
             (for/list ([x :bytevector #vu8(0 1 2 3) :type u16 :big :rev :index j])
               (list x j)))

     (equal? '((1 1) (3 3))
             (for/list ([x :iota 5 :index j :guard (odd? x)]) (list x j)))

     (equal? '((10 0) (14 2))
             (for/list ([x :nums 10 16 2 :index j :guard (even? j)]) (list x j)))

     (equal? '((a 0) (c 2))
             (for/list ([x :iter (list->iter '(a b c)) :index j :guard (even? j)])
               (list x j)))

     (equal? '((0 1 1) (0 3 3) (1 1 1) (1 3 3))
             (for*/list ([outer 2] [x :list '(0 1 2 3) :index j :guard (odd? x)])
               (list outer x j)))

     ;; An entirely filtered source terminates without evaluating the body.
     (equal? '() (for/list ([x :list '(1 2 3) :guard #f]) (error 'guard "body evaluated")))

     ;; Unsupported and duplicate trailing options must raise an expansion error.
     (error? (eval '(for/list ([x :list '(1 2 3) :unknown 2]) x)))

     (error? (eval '(for/list ([x :iter (list->iter '(1 2)) :step 2]) x)))

     (error? (eval '(for/list ([x :vector '#(1 2) :guard #t :guard #f]) x))))

(mat iterator-option-evaluation
     (let ([calls 0])
       (and (equal? '((1 a) (3 b))
                    (for/list ([x :list '(1 2 3 4)
                                  :guard (begin (set! calls (+ calls 1)) (odd? x))]
                               [y :list '(a b)])
                      (list x y)))
            ;; Known companion exhaustion prevents another predicate evaluation.
            (= calls 3)))

     ;; A successful predicate must preserve the item it inspected.
     (let ([source (vector 10)])
       (equal? '(10)
               (for/list ([x :vector source
                             :guard (begin (vector-set! source 0 20) #t)]) x)))

     ;; Iterator-local indexes may shadow the global index without changing its state.
     (= 4 (for*/fold ([:index i] [:finish i] [x 2]
                      [y :list '(a b) :index i]) (void)))

     (let ([table (make-eq-hashtable)])
       (hashtable-set! table 'one 1)
       (equal? '((one 1 0))
               (for/list ([(key value) :hashtable table :index i :guard (= value 1)])
                 (list key value i)))))

(mat duplicate-source-options
     ;; Duplicate options must be rejected rather than discard an earlier expression.
     (error? (eval '(for/list ([x :vector '#(0 1 2) :step 1 :step 2]) x)))

     (error? (eval '(for/list ([x :string "abc" :from 0 :from 1]) x)))

     (error? (eval '(for/list ([x :fxvector (fxvector 1 2) :to 1 :to 2]) x)))

     (error? (eval '(for/list ([x :flvector (flvector 1.0 2.0) :rev :rev]) x)))

     (error? (eval '(for/list ([x :bytevector #vu8(0 1) :type u8 :type s8]) x)))

     (error? (eval '(for/list ([x :list '(0 1 2) :step 1 :step 2]) x)))

     ;; Invalid earlier arity must not disappear behind a later valid occurrence.
     (error? (eval '(for/list ([x :vector '#(0 1 2) :from 0 1 :from 1]) x))))

(mat numeric-range-progress
     ;; Rounding must raise an error instead of repeating a finite range's first value.
     ;; The break clauses bound these witnesses when progress detection is missing.
     (error? (let ([visits 0])
               (for ([x :nums 1e16 (+ 1e16 2.0) 1.0] [:break (= visits 3)])
                 (set! visits (+ visits 1)))))

     (error? (let ([visits 0])
               (for ([x :nums 1e16 (+ 1e16 2.0)] [:break (= visits 3)])
                 (set! visits (+ visits 1)))))

     (error? (let ([visits 0])
               (for* ([x :nums -1e16 (- -1e16 2.0) -1.0] [:break (= visits 3)])
                 (set! visits (+ visits 1)))))

     (equal? '(0.0 0.5 1.0) (for/list ([x :nums 0.0 1.5 0.5]) x))

     (equal? '(1.0 0.5 0.0) (for/list ([x :nums 1.0 -0.5 -0.5]) x))

     ;; Crossing a bound in one step remains valid, including an infinite step.
     (equal? '(0) (for/list ([x :nums 0 1 +inf.0]) x)))

(mat parallel-filter-exhaustion
     ;; Known empty sources must stop before running any companion filter.
     (equal? '()
             (for/list ([x :fxiota 0]
                        [y :nums +inf.0 :guard (error 'exhaustion "filter evaluated")])
               (list x y)))

     (equal? '()
             (for/list ([y :nums +inf.0 :guard (error 'exhaustion "filter evaluated")]
                        [x :list '()])
               (list x y)))

     ;; After the shortest source ends, do not seek another accepted companion item.
     (equal? '((0 0))
             (for/list ([x :fxiota 1]
                        [y :nums +inf.0 :guard
                           (if (= y 0) #t (error 'exhaustion "filter past end"))])
               (list x y)))

     (equal? '#((0 0))
             (for/vector ([y :nums +inf.0 :guard
                             (if (= y 0) #t (error 'exhaustion "filter past end"))]
                          [x :fxiota 1])
               (list x y)))

     ;; An exhausted filtered source prevents later sources from seeking as well.
     (equal? '()
             (for/list ([x :fxiota 3 :guard #f]
                        [y :nums +inf.0 :guard (error 'exhaustion "later filter")])
               (list x y))))

(mat nested-break-result
     (eq? 'done (for*/fold ([x :fxiota 2] [:break (= x 1) 'done]) x))

     ;; An inner break replaces the accumulator and resumes the next outer item.
     (= 20 (for*/fold ([:init sum 0] [:finish sum]
                       [x :fxiota 2] [y :fxiota 3]
                       [:break (= y 1) (+ sum 10)]) (+ sum y)))

     ;; A result expression runs exactly once per break, including non-fold loops.
     (let ([calls 0] [seen (make-list-builder)])
       (and (eq? (void)
                 (for* ([x :fxiota 2] [y :fxiota 3]
                        [:break (= y 1) (begin (set! calls (+ calls 1)) 'ignored)])
                   (seen (list x y))))
            (= calls 2)
            (equal? '((0 0) (1 0)) (seen))))

     ;; False break conditions must not evaluate their result expression.
     (= 3 (for*/fold ([:init sum 0] [:finish sum]
                       [x :fxiota 3] [:break #f (error 'break "result evaluated")])
            (+ sum x)))

     ;; The break result sees bindings at the clause's original source position.
     (= 101 (for*/fold ([:init sum 0] [:finish sum]
                        [x :fxiota 2] [y :fxiota 2] [:let sum 100]
                        [:break (= y 1) (+ sum x)]) 1)))

(mat index-only-sources
     (equal? '((0 0) (1 1) (2 2))
             (for/list ([x :fxiota 3 :index i]) (list x i)))

     (equal? '((10 0) (12 1) (14 2))
             (for/list ([x :nums 10 16 2 :index i]) (list x i)))

     (equal? '((a 0) (b 1))
             (for/list ([x :list '(a b) :index i]) (list x i)))

     (equal? '((a 0) (b 1))
             (for/list ([x :iter (list->iter '(a b)) :index i]) (list x i)))

     ;; Mutation and shadowing of public indexes must not change the source position.
     (equal? '(0 1 2)
             (for/list ([x :iota 3 :index i])
               (let ([original i]) (set! i 100) original)))

     (equal? '(0 1 2)
             (for/list ([x :fxiota 3 :index i] [:let i 100]) x))

     (equal? '((0 0) (0 1) (1 0) (1 1))
             (for*/list ([x :fxiota 2] [y :fxiota 2 :index i]) (list x i))))

(mat numeric-unordered-progress
     ;; Opposing infinities must raise an error rather than produce NaN forever.
     ;; The visit limits keep the witnesses finite without progress validation.
     (error? (let ([visits 0])
               (for ([x :nums -inf.0 0 +inf.0] [:break (= visits 3)])
                 (set! visits (+ visits 1)))))

     (error? (let ([visits 0])
               (for* ([x :nums +inf.0 0 -inf.0] [:break (= visits 3)])
                 (set! visits (+ visits 1)))))

     ;; Infinite bounds and infinite steps that cross their bound remain supported.
     (equal? '(0 1 2)
             (for/list ([x :nums +inf.0] [:break (= x 3)]) x))

     (equal? '(0) (for/list ([x :nums 0 1 +inf.0]) x))

     (equal? '(0) (for/list ([x :nums 0 -1 -inf.0]) x)))

(mat rejected-inexact-sequence-strides
     ;; Integer-only iterator options reject inexact values rather than convert them.
     (error? (for/list ([x :vector '#(0 1 2 3) :step 2.0]) x))

     (error? (for/list ([x :string "abcd" :rev :step 2.0]) x))

     (error? (for/list ([x :fxvector (fxvector 0 1 2 3) :step 2.0]) x))

     (error? (for*/list ([x :flvector (flvector 0.0 1.0 2.0 3.0) :rev :step 2.0]) x))

     (error? (for/list ([x :list '(0 1 2 3) :step 2.0]) x))

     (error? (for/list ([x :bytevector #vu8(0 1 2 3 4 5) :type u16 :big :step 2.0]) x))

     (let ([calls 0])
       (guard (condition [(error? condition) (= calls 1)] [else #f])
         (for/list ([x :vector '#(0 1 2 3)
                      :step (begin (set! calls (+ calls 1)) 2.0)]) x)
         #f))

     ;; Non-integer, zero, negative, NaN, and infinite strides remain invalid.
     (error? (for/list ([x :vector '#(0 1) :step 1.5]) x))

     (error? (for/list ([x :vector '#(0 1) :step 0.0]) x))

     (error? (for/list ([x :vector '#(0 1) :step -1.0]) x))

     (error? (for/list ([x :vector '#(0 1) :step +nan.0]) x))

     (error? (for/list ([x :vector '#(0 1) :step +inf.0]) x)))

(mat deferred-parallel-iterators
     (let ([source (list->iter '(a b c))])
       (and (equal? '() (for/list ([x :fxiota 0] [y :iter source]) y))
            (eq? 'a (iter-next! source))))

     (let ([source (list->iter '(a b c))])
       (and (equal? '(a) (for/list ([y :iter source] [x :fxiota 1]) y))
            (eq? 'b (iter-next! source))))

     ;; A known empty companion must stop before a rejecting pipeline can seek.
     (let* ([calls 0]
            [source (iter-filter
                     (lambda (value)
                       (set! calls (+ calls 1))
                       (when (> calls 3) (error 'exhaustion "unexpected seek"))
                       #f)
                     (range 0 +inf.0))])
       (and (equal? '() (for/list ([y :iter source] [x :list '()]) y))
            (= calls 0)))

     (let ([source (list->iter '(a b c d e))])
       (and (equal? '(b) (for/list ([x :fxiota 1]
                                    [y :iter source :index i :guard (odd? i)]) y))
            (eq? 'c (iter-next! source))))

     (equal? '((0 a 0) (0 c 2) (1 a 0) (1 c 2))
             (for*/list ([x :fxiota 2]
                          [y :iter (list->iter '(a b c)) :index i :guard (even? i)])
               (list x y i)))

     ;; With unknown source lengths one pending value may be needed to discover exhaustion.
     (equal? '((a 1) (b 2))
             (for/list ([x :iter (list->iter '(a b))]
                        [y :iter (list->iter '(1 2 3))]) (list x y))))

(mat direct-filter-binding-locations
     (let ([read-item #f])
       (equal? '(10 11)
               (for/list ([x :fxiota 2
                             :guard (begin (set! read-item (lambda () x)) #t)])
                 (set! x (+ x 10))
                 (read-item))))

     (let ([read-index #f])
       (equal? '((100 100) (100 100))
               (for/list ([x :vector '#(a b) :index i
                             :guard (begin (set! i 100)
                                           (set! read-index (lambda () i)) #t)])
                 (list i (read-index)))))

     (let ([read-item #f])
       (equal? '(changed changed changed changed)
               (for*/list ([outer :fxiota 2]
                            [x :list '(a b)
                               :guard (begin (set! read-item (lambda () x)) #t)])
                 (set! x 'changed)
                 (read-item))))

     ;; Source filtering must not advance the global index or inspect its local binding.
     (let ([i 100])
       (equal? '((1 0) (3 1))
               (for/list ([:index i]
                          [x :fxiota 5 :guard (and (= i 100) (odd? x))]) (list x i))))

     (let ([sum 100])
       (= 6 (for/fold ([:init sum 0] [:finish sum]
                       [x :fxiota 4 :guard (= sum 100)]) (+ sum x))))

     ;; Each rejected item keeps its original source position.
     (equal? '((1 1) (3 3))
             (for/list ([x :fxiota 5 :index i :guard (odd? i)]) (list x i))))

(mat parallel-filter-binding-locations
     (let ([read-item #f])
       (equal? '((10 a 10) (11 b 11))
               (for/list ([x :fxiota 2
                             :guard (begin (set! read-item (lambda () x)) #t)]
                          [y :list '(a b)])
                 (set! x (+ x 10))
                 (list x y (read-item)))))

     (let ([read-index #f])
       (equal? '((100 100) (100 100))
               (for/list ([y :fxiota 2]
                          [x :vector '#(a b) :index i
                             :guard (begin (set! i 100)
                                           (set! read-index (lambda () i)) #t)])
                 (list i (read-index)))))

     ;; Closures escaping the body retain their own iteration's assignment location.
     (let* ([saved (make-list-builder)]
            [readers
             (for/list ([x :fxiota 2
                           :guard (begin (saved (lambda () x)) #t)] [y :fxiota 2])
               (set! x (+ x 10))
               (lambda () x))])
       (and (equal? '(10 11) (map (lambda (read) (read)) readers))
            (equal? '(10 11) (map (lambda (read) (read)) (saved)))))

     ;; Local guards see their own source and lexical context, not companion aliases.
     (let ([x 100] [i 100])
       (equal? '((0 1) (1 3))
               (for/list ([x :fxiota 2 :index i]
                          [y :fxiota 5 :guard (and (= x 100) (= i 100) (odd? y))])
                 (list x y))))

     ;; Mutating both hashtable bindings in a guard and body shares their locations.
     (let ([table (make-eq-hashtable)] [read-pair #f])
       (hashtable-set! table 'one 1)
       (equal? '((changed 10))
               (for/list ([(key value) :hashtable table
                            :guard (begin (set! read-pair (lambda () (list key value))) #t)]
                          [y :fxiota 1])
                 (set! key 'changed)
                 (set! value 10)
                 (read-pair)))))

(mat list-result-length-limit
     (guard (condition [#t #f])
       (equal? '(0 1)
               (eval '(for/list ([:length 2] [x 4]) x) (environment '(cspp)))))

     (guard (condition [#t #f])
       (equal? '((0 1) (1 3) (2 5))
               (eval '(let ([limit 3])
                        (for/list ([:index i] [:length (+ limit 0)] [x 10]
                                   [:guard (odd? x)])
                          (list i x)))
                     (environment '(cspp)))))

     (guard (condition [#t #f])
       (equal? '((0 0) (0 1) (1 0))
               (eval '(for*/list ([:length 3] [x 3] [y 2]) (list x y))
                     (environment '(cspp)))))

     (guard (condition [#t #f])
       (equal? '(10 11)
               (eval '(for/list ([:length 5] [x :list '(10 11)]) x)
                     (environment '(cspp)))))

     ;; Reserved option tags must not accidentally become iterator variables.
     (error? (eval '(for ([:length 2] [x 3]) x)))

     (error? (eval '(for/list ([x 3] [:length 2]) x)))

     (error? (eval '(for*/list ([x 3] [:length 2]) x))))

(mat collection-option-order
     ;; Global indexes precede length options, consistently with vector comprehensions.
     (error? (eval '(for/list ([:length 2] [:index i] [x 4]) (list i x))))

     (error? (eval '(for*/list ([:length 2] [:index i] [x 4]) (list i x)))))

(mat list-result-limit-evaluation
     (guard (condition [#t #f])
       (equal? '((1 3) 1 2)
               (eval '(let ([length-calls 0] [body-calls 0])
                        (let ([result
                               (for/list ([:length (begin (set! length-calls
                                                                (+ length-calls 1)) 2)]
                                          [x :nums +inf.0] [:guard (odd? x)])
                                 (set! body-calls (+ body-calls 1))
                                 x)])
                          (list result length-calls body-calls)))
                     (environment '(cspp)))))

     (guard (condition [#t #f])
       (equal? '(() ())
               (eval '(list
                       (for/list ([:length 0]
                                  [x :iota (error 'list-limit "source evaluated")])
                         (error 'list-limit "body evaluated"))
                       (for*/list ([:length 0]
                                   [x :iota (error 'list-limit "source evaluated")])
                         (error 'list-limit "body evaluated")))
                     (environment '(cspp)))))

     (guard (condition [#t #f])
       (equal? '((0 1 10) 2 3)
               (eval '(let ([sources 0] [bodies 0])
                        (let ([result
                               (for*/list ([:index i] [:length 3] [x 4]
                                           [y :iota (begin (set! sources (+ sources 1)) 2)])
                                 (set! bodies (+ bodies 1))
                                 (+ (* 10 x) y))])
                          (list result sources bodies)))
                     (environment '(cspp)))))

     (guard (condition [#t #f])
       (equal? '((0 10 20) (0 1))
               (eval '(list
                       (for*/list ([:length 3] [x 4] [y 4] [:break (= y 1)])
                         (+ (* 10 x) y))
                       (for*/list ([:length 5] [x 4] [y 4] [:stop (= y 2)]) y))
                     (environment '(cspp)))))

     ;; Length expressions must produce nonnegative fixnums, even for empty sources.
     (error? (eval '(for/list ([:length -1] [x 0]) x)))

     (error? (eval '(for*/list ([:length 1.5] [x 0]) x)))

     (error? (eval '(for/list ([:length (+ (most-positive-fixnum) 1)] [x 0]) x))))

(mat vector-fill-requires-length
     ;; Reject fill-only forms during expansion, before their expressions run.
     (guard (condition [(syntax-violation? condition) #t] [else #f])
       (eval '(for/vector ([:fill (error 'fill-only "evaluated")]
                           [x 2]) x)
             (environment '(cspp)))
       #f)

     (guard (condition [(syntax-violation? condition) #t] [else #f])
       (eval '(for*/vector ([:fill (error 'fill-only "evaluated")]
                            [x 2] [y 2]) (list x y))
             (environment '(cspp)))
       #f)

     (guard (condition [(syntax-violation? condition) #t] [else #f])
       (eval '(for*/vector ([:index i] [:fill #f] [x 2]) i)
             (environment '(cspp)))
       #f)

     (let ([calls 0])
       (let ([result (for/vector ([:length 3]
                                 [:fill (begin (set! calls (+ calls 1)) 'unused)] [x 2]) x)])
         (and (equal? '#(0 1 unused) result) (= calls 1))))

     (let ([calls 0])
       (let ([result (for*/vector ([:length 3]
                                  [:fill (begin (set! calls (+ calls 1)) 'unused)]
                                  [x 1] [y 2]) y)])
         (and (equal? '#(0 1 unused) result) (= calls 1))))

     ;; A zero-length vector evaluates fill once while skipping the sources and body.
     (let ([calls 0])
       (let ([result (for/vector ([:length 0]
                                 [:fill (begin (set! calls (+ calls 1)) 'unused)]
                                 [x :iota (error 'fill-length "source evaluated")])
                       (error 'fill-length "body evaluated"))])
         (and (equal? '#() result) (= calls 1))))

     (let ([calls 0])
       (let ([result (for*/vector ([:length 0]
                                  [:fill (begin (set! calls (+ calls 1)) 'unused)]
                                  [x :iota (error 'fill-length "source evaluated")])
                       (error 'fill-length "body evaluated"))])
         (and (equal? '#() result) (= calls 1)))))

(mat parallel-item-index-shadowing
     (guard (condition [#t #f])
       (equal? '(0 1 2)
               (eval '(for/list ([x :nums 10 13 :index x :guard #t] [y 3]) x)
                     (environment '(cspp)))))

     (guard (condition [#t #f])
       (equal? '(1)
               (eval '(for/list ([x :vector '#(10 11 12) :index x :guard (odd? x)] [y 3]) x)
                     (environment '(cspp)))))

     (guard (condition [#t #f])
       (equal? '(10 11)
               (eval '(let ([read-index #f])
                        (for/list ([x :fxiota 2 :index x
                                     :guard (begin (set! read-index (lambda () x)) #t)]
                                   [y 2])
                          (set! x (+ x 10))
                          (read-index)))
                     (environment '(cspp))))))

(mat inner-break-discarded-values
     (guard (condition [#t #f])
       (equal? '()
               (for*/list ([x 2] [y 2] [:break #t (values 1 2)]) (list x y))))

     (guard (condition [#t #f])
       (equal? '()
               (for*/list ([x 2] [y 2] [:break #t (values)]) (list x y))))

     (guard (condition [#t #f])
       (let ([calls 0])
         (for* ([x 3] [y 2]
                [:break #t (begin (set! calls (+ calls 1)) (values 1 2))])
           (error 'break-values "body evaluated"))
         (= calls 3))))

(mat strict-sequence-integer-options
     ;; Strides must be exact positive integers, with no inexact-to-exact normalization.
     (error? (for/list ([x :vector '#(0 1 2) :step 2.0]) x))

     (error? (for/list ([x :string "abc" :step 2.0]) x))

     (error? (for/list ([x :fxvector (fxvector 0 1 2) :step 2.0]) x))

     (error? (for/list ([x :flvector (flvector 0.0 1.0 2.0) :step 2.0]) x))

     (error? (for/list ([x :list '(0 1 2) :step 2.0]) x))

     (error? (for/list ([x :bytevector #vu8(0 1 2) :step 2.0]) x))

     ;; Source positions must be fixnums, even for an empty slice.
     (error? (for/list ([x :vector '#(0 1 2) :from 1.0 :to 1.0]) x))

     (error? (for/list ([x :string "abc" :to 2.0]) x))

     (error? (for/list ([x :bytevector #vu8(0 1 2) :from 1.0]) x))

     ;; Empty slices still validate positions against the source bounds.
     (error? (for/list ([x :vector '#(0 1) :from 10 :to 10]) x))

     (error? (for/list ([x :bytevector #vu8(0 1) :from 10 :to 11 :type u16]) x))

     ;; Iota counts must be exact nonnegative integers.
     (error? (for/list ([x :iota 2.0]) x))

     (equal? '(1) (for/list ([x :vector '#(0 1 2) :from 1
                              :step (+ (most-positive-fixnum) 1)]) x))

     (equal? '(2 1 0) (for/list ([x :vector '#(0 1 2) :from 2 :to -1]) x)))

(mat typed-bytevector-reverse-chunks
     (equal? '((2 515) (0 1))
             (for/list ([x :bytevector #vu8(0 1 2 3 4) :type u16 :big :rev :index i])
               (list i x)))

     (equal? '((1 66051))
             (for/list ([x :bytevector #vu8(0 1 2 3 4 5 6)
                           :from 1 :to 6 :type u24 :big :rev :index i])
               (list i x)))

     (equal? '((6 395016) (3 197637) (0 258))
             (for/list ([x :bytevector #vu8(0 1 2 3 4 5 6 7 8 9 10)
                           :type u24 :big :rev :index i])
               (list i x)))

     (equal? '((6 395016) (0 258))
             (for/list ([x :bytevector #vu8(0 1 2 3 4 5 6 7 8 9 10)
                           :type u24 :big :rev :step 2 :index i])
               (list i x)))

     (equal? '((4 263430))
             (for/list ([x :bytevector #vu8(0 1 2 3 4 5 6)
                           :from 6 :to 1 :type u24 :big :rev :index i])
               (list i x)))

     (equal? '()
             (for/list ([x :bytevector #vu8(0 1) :type u24 :rev]) x)))

(mat empty-descending-numeric-ranges
     (null? (for/list ([x :nums 2 2 -1]) x))

     (null? (for/list ([x :fixnums 2 2 -1]) x))

     (null? (for*/list ([x 2] [y :nums x x -1]) y))

     (null? (for*/list ([x 2] [y :fixnums x x -1]) y))

     (let ([calls 0])
       (and (null? (for/list ([x :nums 2 2 (begin (set! calls (+ calls 1)) -1)]) x))
            (= calls 1)))

     ;; Zero steps remain invalid for an empty numeric range.
     (error? (for/list ([x :nums 2 2 0]) x))

     ;; Zero fixnum steps remain invalid for an empty range.
     (error? (for/list ([x :fixnums 2 2 0]) x)))

(mat forward-list-slice-storage
     ;; A forward slice must not allocate storage proportional to the entire source.
     (let* ([source (iota 100000)]
            [before (statistics)]
            [result (for/list ([:length 1] [x :list source :from 0]) x)]
            [bytes (sstats-bytes (sstats-difference (statistics) before))])
       (and (equal? '(0) result) (< bytes 200000)))

     ;; Empty forward slices also avoid copying the source.
     (let* ([source (iota 100000)]
            [before (statistics)]
            [result (for/list ([x :list source :from 0 :to 0]) x)]
            [bytes (sstats-bytes (sstats-difference (statistics) before))])
       (and (null? result) (< bytes 200000))))

(mat list-slice-traversal
     (equal? '((1 1) (3 3) (5 5))
             (for/list ([x :list '(0 1 2 3 4 5 6) :from 1 :to 6 :step 2 :index i])
               (list x i)))

     (equal? '((5 5) (3 3) (1 1))
             (for/list ([x :list '(0 1 2 3 4 5 6) :from 5 :to 0 :step 2 :index i])
               (list x i)))

     (equal? '((1 1) (3 3) (5 5))
             (for/list ([x :list '(0 1 2 3 4 5 6) :from 5 :to 0 :rev :step 2 :index i])
               (list x i)))

     (equal? '((1 a) (3 b) (5 c))
             (for/list ([x :list '(0 1 2 3 4 5 6) :from 1 :to 6 :guard (odd? x)]
                        [y :list '(a b c)])
               (list x y)))

     (equal? '((0) (1 2) (2 3 4))
             (for/list ([outer :fxiota 3])
               (for/list ([x :list '(0 1 2 3 4 5 6) :from outer :to (+ (* outer 2) 1)])
                 x)))

     (equal? '(2) (for/list ([x :list '(0 1 2 3) :from 2
                               :step (+ 1 (most-positive-fixnum))]) x))

     (null? (for/list ([x :list '() :rev]) x))

     (let ([events '()])
       (let ([result
              (for/list ([x :list (begin (set! events (cons 'source events)) '(0 1 2 3))
                             :from (begin (set! events (append events '(from))) 1)
                             :to (begin (set! events (append events '(to))) 4)
                             :step (begin (set! events (append events '(step))) 2)]) x)])
         (and (equal? '(1 3) result)
              (equal? '("from" "source" "step" "to")
                      (sort string<? (map symbol->string events))))))

     ;; List slices retain strict source types.
     (error? (for/list ([x :list '#(0 1 2) :from 0]) x))

     ;; Forward list bounds require fixnums.
     (error? (for/list ([x :list '(0 1 2) :from 1.0]) x))

     ;; Even an empty slice must lie inside the source.
     (error? (for/list ([x :list '(0 1 2) :from 4 :to 4]) x)))

(mat list-slice-continuation-state
     (let ()
       (define (check traverse expected)
         (let ([resume #f] [resumed? #f])
           (let ([result
                  (traverse
                   (lambda (value)
                     (when (and (not resumed?) (not resume))
                       ;; Re-enter a completed immutable fold to verify restored source state.
                       (call/cc (lambda (continuation) (set! resume continuation))))
                     value))])
             (if resumed?
                 (equal? expected result)
                 (begin (set! resumed? #t) (resume 'continue))))))
       (and
        (check (lambda (visit)
                 (for/fold ([:init result '()] [:finish (reverse result)]
                             [x :list (list 'a 'b 'c) :from 0])
                   (cons (visit x) result)))
               '(a b c))
        (check (lambda (visit)
                 (for/fold ([:init result '()] [:finish (reverse result)]
                             [x :list (list 'a 'b 'c 'd 'e) :step 2])
                   (cons (visit x) result)))
               '(a c e))
        (check (lambda (visit)
                 (for/fold ([:init result '()] [:finish (reverse result)]
                             [x :list (list 'a 'b 'c 'd) :from 0 :index i :guard (odd? i)])
                   (cons (visit x) result)))
               '(b d))
        (check (lambda (visit)
                 (for/fold ([:init result '()] [:finish (reverse result)]
                             [x :list (list 'a 'b 'c 'd) :from 0 :index i :guard (odd? i)]
                             [other :fxiota 2])
                   (cons (visit x) result)))
               '(b d))
        (check (lambda (visit)
                 (for*/fold ([:init result '()] [:finish (reverse result)]
                              [outer :fxiota 2] [x :list (list 'a 'b) :from 0])
                   (cons (visit (list outer x)) result)))
               '((0 a) (0 b) (1 a) (1 b))))))

(mat parallel-list-slice-continuation-state
     (let ([resume #f] [resumed? #f])
       (let ([result
              (for/fold
                ([:init result '()] [:finish (reverse result)]
                 [x :fxiota 2 :guard
                    (begin
                      (when (and (not resumed?) (not resume))
                        ;; Capture before a companion getter has bound its list item.
                        (call/cc (lambda (continuation) (set! resume continuation))))
                      #t)]
                 [y :list (list 'a 'b) :from 0])
                (cons (list x y) result))])
         (if resumed?
             (equal? '((0 a) (1 b)) result)
             (begin (set! resumed? #t) (resume 'continue))))))

(mat list-slice-extra-state-transitions
     (equal? '((1 b) (3 d))
             (for/list ([x :list '(0 1 2 3 4) :from 0 :guard (odd? x)]
                        [y :list '(a b c d e) :from 1 :step 2])
               (list x y)))

     (equal? '((a 0) (b 0))
             (for*/list ([x :list '(a b) :from 0] [y 3] [:break (= y 1)])
               (list x y)))

     (= 3 (for*/fold ([:init sum 0] [:finish sum]
                       [x :list '(1 2) :from 0] [y 3] [:break (= y 1)])
            (+ sum x)))

     (= 23 (for*/fold ([:init sum 0] [:finish sum]
                        [x :list '(1 2) :from 0] [y 3] [:break (= y 1) (+ sum 10)])
             (+ sum x)))

     (equal? '(1 3)
             (for/list ([x :list '(0 1 2 3 4) :from 0] [:guard (odd? x)]) x)))

(mat parallel-source-evaluation
     (let ([events '()])
       (for ([x :iota (begin (set! events (append events '(x))) 1)]
             [y :iota (begin (set! events (append events '(y))) 1)]) (void))
       (equal? '("x" "y") (sort string<? (map symbol->string events))))

     (let ([events '()])
       (for/vector ([x :list (begin (set! events (append events '(x))) '(1))]
                    [y :vector (begin (set! events (append events '(y))) '#(2))]) (+ x y))
       (equal? '("x" "y") (sort string<? (map symbol->string events)))))

(mat option-argument-positions
     (let ([:end 2] [:stride 2])
       (equal? '(a c)
               (for/list ([x :vector '#(a b c d) :to (+ :end 1) :step :stride]) x)))

     (let ([:end 2])
       (equal? '(a b) (for/list ([x :vector '#(a b c) :to :end]) x)))

     (let ([:index 2] [:guard 1])
       (equal? '(b) (for/list ([x :list '(a b c) :from :guard :to :index]) x)))

     (let ([:guard '(a b)])
       (equal? '(a b) (for/list ([x :list :guard :guard #t]) x)))

     (let ([:index 3])
       (equal? '(0 1 2) (for/list ([x :iota :index]) x)))

     ;; A missing option value must be rejected.
     (error? (eval '(for/list ([x :vector '#(a b) :to]) x)))

     ;; Unknown option tags must still be rejected.
     (error? (eval '(for/list ([x :vector '#(a b) :unknown 1]) x))))

(mat many-valid-loop-clauses
     (guard (condition [#t #f])
       (expand `(lambda () (for/fold ([x 1] ,@(make-list 5100 '[:guard #t])) #t))
               (environment '(cspp)))
       #t))

(mat counter-continuation-state
     (let ()
       (define (check traverse)
         (let ([resume #f] [resumed? #f])
           (let ([result
                  (traverse
                   (lambda ()
                     (when (and (not resumed?) (not resume))
                       ;; Restore the source and its private counters together.
                       (call/cc (lambda (continuation) (set! resume continuation))))))])
             (if resumed?
                 (equal? '((0 0) (1 1)) result)
                 (begin (set! resumed? #t) (resume 'continue))))))
       (and
        (check (lambda (visit)
                 (for/fold ([:init result '()] [:finish (reverse result)]
                             [x :nums 0 2 :index i])
                   (visit) (cons (list x i) result))))
        (check (lambda (visit)
                 (for/fold ([:init result '()] [:finish (reverse result)]
                             [x 2] [:guard #t :index i])
                   (visit) (cons (list x i) result))))
        (check (lambda (visit)
                 (for*/fold ([:init result '()] [:finish (reverse result)]
                              [outer 1] [x 2] [:index i])
                   (visit) (cons (list x i) result))))
        (check (lambda (visit)
                 (for/fold ([:init result '()] [:finish (reverse result)]
                             [x :nums 0 2 :index i :guard #t] [other 2])
                   (visit) (cons (list x i) result)))))))

(mat backward-list-slice-storage
     (let* ([source (iota 100000)]
            [before (statistics)]
            [result (for/list ([x :list source :from 1 :to 0]) x)]
            [bytes (sstats-bytes (sstats-difference (statistics) before))])
       (and (equal? '(1) result) (< bytes 200000)))

     (let* ([source (iota 100000)]
            [before (statistics)]
            [result (for/list ([x :list source :from 99999 :to -1 :step 100000]) x)]
            [bytes (sstats-bytes (sstats-difference (statistics) before))])
       (and (equal? '(99999) result) (< bytes 200000))))

(mat vector-collector-storage
     (let* ([before (statistics)]
            [result (for/vector ([x :fxiota 100000]) x)]
            [bytes (sstats-bytes (sstats-difference (statistics) before))])
       (and (= (vector-length result) 100000)
            (= (vector-ref result 99999) 99999)
            (< bytes 2100000)))

     (equal? (list->vector (iota 2049)) (for/vector ([x :fxiota 2049]) x))

     (equal? '#() (for/vector ([x 3] [:guard #f]) x))

     (equal? '#((0 0) (0 1) (1 0) (1 1))
             (for*/vector ([x 2] [y 2]) (list x y)))

     (equal? '#(0 1 2) (for/vector ([x 10] [:break (= x 3)]) x)))

(mat vector-collector-filtered-storage
     (let* ([before (statistics)]
            [result (for/vector ([x :fxiota 100000 :guard (even? x)]) x)]
            [bytes (sstats-bytes (sstats-difference (statistics) before))])
       (and (= (vector-length result) 50000)
            (= (vector-ref result 49999) 99998)
            (< bytes 1100000)))

     (let ([calls 0])
       (and (equal? '#(0 1 2)
                    (for/vector ([x :fxiota (begin (set! calls (+ calls 1)) 3)]) x))
            (= calls 1)))

     ;; Inferred lengths retain strict source-bound types, including an empty range.
     (error? (for/vector ([x :fxiota 0.0]) x)))

(mat iterator-prefetch-continuation-state
     (let ([resume #f] [phase 0])
       (let ([result
              (for/fold
                ([:init result '()] [:finish (reverse result)]
                 [item :iter (list->iter '(a b c d e f))]
                 [position :fxiota 2
                           :guard (begin
                                    (when (and (= position 0) (not resume))
                                      ;; Replay after the current item has already been fetched.
                                      (call/cc (lambda (continuation) (set! resume continuation))))
                                    #t)])
                (cons (list item position) result))])
         (let ([expected (case phase
                           [(0) '((a 0) (b 1))]
                           [(1) '((a 0) (c 1))]
                           [else '((a 0) (d 1))])])
           (and (equal? expected result)
                (if (= phase 2)
                    #t
                    (begin (set! phase (+ phase 1)) (resume #t))))))))

(mat collector-loop-parameter-replay
     (let ()
       (define (check kind size target)
         (let ([resume #f] [phase 0])
           (define (visit item)
             (if (= item target)
                 ;; Restore collection positions, while allowing shared results to change.
                 (call/cc (lambda (continuation) (set! resume continuation) item))
                 item))
           (let* ([result
                   (case kind
                     [(list) (for/list ([x :fxiota size]) (visit x))]
                     [(nested-list) (for*/list ([outer 1] [x :fxiota size]) (visit x))]
                     [(bounded-list) (for/list ([:length size] [x :fxiota (+ size 2)]) (visit x))]
                     [(nested-bounded-list)
                      (for*/list ([:length size] [outer 2] [x :fxiota (+ size 2)]) (visit x))]
                     [(vector) (for/vector ([x :fxiota size :guard #t]) (visit x))]
                     [(nested-vector) (for*/vector ([outer 1] [x :fxiota size]) (visit x))]
                     [(bounded-vector)
                      (for/vector ([:length size] [x :fxiota (+ size 2)]) (visit x))]
                     [(nested-bounded-vector)
                      (for*/vector ([:length size] [outer 2] [x :fxiota (+ size 2)]) (visit x))]
                     [(inferred-vector) (for/vector ([x :fxiota size]) (visit x))])]
                  [expected (iota size)]
                  [expected (if (= phase 0) expected
                                (append (list-head expected target) (list (- phase))
                                        (list-tail expected (+ target 1))))])
             (and (equal? expected (if (vector? result) (vector->list result) result))
                  (if (= phase 2)
                      #t
                      (begin (set! phase (+ phase 1)) (resume (- phase))))))))
       (andmap
        (lambda (kind)
          (andmap (lambda (size)
                    (andmap (lambda (target) (check kind size target))
                            (list 0 (quotient size 2) (- size 1))))
                  '(3 17 49 1041 2049)))
        '(list nested-list bounded-list nested-bounded-list
               vector nested-vector bounded-vector nested-bounded-vector inferred-vector))))

(mat collection-replay-shared-storage
     (let ([resume #f] [replayed? #f] [first #f])
       (let ([result
              (for/vector ([:length 3] [x 5])
                (if (= x 1)
                    ;; The saved position restores, but the preallocated vector is reused.
                    (call/cc (lambda (continuation) (set! resume continuation) x))
                    x))])
         (if replayed?
             (and (eq? first result) (equal? '#(0 9 2) result))
             (begin (set! first result) (set! replayed? #t) (resume 9)))))

     (let ([resume #f] [replayed? #f] [first #f])
       (let ([result
              (for/list ([x 3])
                (if (= x 1)
                    ;; Replaying replaces the shared prefix link instead of copying history.
                    (call/cc (lambda (continuation) (set! resume continuation) x))
                    x))])
         (if replayed?
             (and (eq? first result) (equal? '(0 9 2) result))
             (begin (set! first result) (set! replayed? #t) (resume 9))))))

(mat collection-private-position-and-exits
     ;; Public index mutation and shadowing must not change the private result count.
     (equal? '#((0 0) (0 1) (0 2))
             (for*/vector ([:index i] [:length 3] [x 2] [y 4] [:let i 100])
               (set! i 200)
               (list x y)))

     ;; Collection exit results are evaluated and discarded, even with multiple values.
     (equal? '(0)
             (for/list ([x 3] [:break (= x 1) (values 'ignored 'also-ignored)]) x))

     (equal? '#((0 0) (1 0))
             (for*/vector ([x 2] [y 3] [:break (= y 1) (values 'ignored 'also-ignored)])
               (list x y)))

     (equal? '#((0 0))
             (for*/vector ([x 2] [y 3] [:stop (= y 1) (values)]) (list x y))))

(mat iterator-clause-let-semantics
     ;; All source and bound expressions run before their source type is checked.
     (andmap
      (lambda (kind)
        (andmap
         (lambda (source-type)
           (eval
            `(let ([calls 0])
               (guard (condition [else (= calls 4)])
                 (,kind ([value ,source-type (begin (set! calls (+ calls 1)) #f)
                                :from (begin (set! calls (+ calls 1)) 0)
                                :step (begin (set! calls (+ calls 1)) 1)
                                :to (begin (set! calls (+ calls 1)) 2)])
                   value)
                 #f))
            (environment '(cspp))))
         '(:list :vector :string :fxvector :flvector :bytevector)))
      '(for for/fold for/list for/vector for* for*/fold for*/list for*/vector))

     ;; An invalid bound must not prevent another peer expression from running.
     (let ([step-calls 0])
       (guard (condition [else (= step-calls 1)])
         (for/list ([value :vector '#(a b) :from #f
                          :step (begin (set! step-calls (+ step-calls 1)) 1)])
           value)
         #f))

     ;; Peer expressions see the surrounding binding, including the source itself.
     (let ([value '#(10 20)] [position 1])
       (equal? '((20 1))
               (for/list ([value :vector value :from position
                                 :to (vector-length value) :index position])
                 (list value position))))

     ;; Nested sources see earlier iterator bindings, but their own options see outer ones.
     (let ([value '#(10 20)])
       (equal? '((1 20) (2 10) (2 20))
               (for*/list ([outer 3]
                           [value :vector value :from (- 2 outer)
                                  :to (vector-length value)])
                 (list outer value)))))

(mat parallel-source-let-semantics
     ;; Parallel source expressions all run before validating any one source.
     (andmap
      (lambda (kind)
        (eval
         `(let ([calls 0])
            (guard (condition [else (= calls 2)])
              (,kind ([x :vector (begin (set! calls (+ calls 1)) #f)]
                      [y :list (begin (set! calls (+ calls 1)) '(a))])
                (list x y))
              #f))
         (environment '(cspp))))
      '(for for/fold for/list for/vector))

     ;; A parallel companion source sees the outer binding rather than the current item.
     (let ([x '(a b)])
       (equal? '((0 a) (1 b)) (for/list ([x 2] [y :list x]) (list x y))))

     ;; An empty outer nested source never evaluates its inner source.
     (let ([calls 0])
       (and (null? (for*/list ([x 0] [y :list (begin (set! calls (+ calls 1)) '(a))]) y))
            (= calls 0))))
