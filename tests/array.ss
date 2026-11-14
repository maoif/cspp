(import (cspp)
        (cspp io)
        (cspp array)
        (cspp test))

(define collect-array-iter
  (lambda (iter)
    (let loop ([values '()])
      (let ([value (iter-next! iter)])
        (if (iter-end? value)
            (reverse values)
            (loop (cons value values)))))))

(define bytearray-width-roundtrip?
  (lambda (width lower-set! lower-ref upper-set! upper-ref value)
    (let ([lower (make-bytearray width 0)] [upper (make-bytearray width 0)])
      (lower-set! lower 0 value)
      (upper-set! upper 0 value)
      (and (equal? value (lower-ref lower 0))
           (equal? value (upper-ref upper 0))))))


(mat array

     ;; invalid array: size observers validate through pcheck
     (guard (condition
             [(and (who-condition? condition)
                   (eq? 'pcheck (condition-who condition))) #t]
             [else #f])
       (array-size 42)
       #f)

     (let ([arr (apply array (iota 10))])
       (fx= (array-size arr) 10))

     (and (array? (array 1))
          (not (array? (fxarray 1)))
          (not (array? (flarray 1.0)))
          (not (array? (bytearray 1))))

     (let ([arr (apply fxarray (iota 10))])
       (fx= (fxarray-size arr) 10))

     (let ([arr (apply bytearray (iota 10))])
       (fx= (bytearray-size arr) 10))



     )

(mat bytearray-widths
     (bytearray-width-roundtrip? 1 bytearray-u8-set! bytearray-u8-ref
                                  bytearray-U8-set! bytearray-U8-ref 200)
     (bytearray-width-roundtrip? 1 bytearray-s8-set! bytearray-s8-ref
                                  bytearray-S8-set! bytearray-S8-ref -100)
     (let ([a (make-bytearray 2 0)])
       (bytearray-u16-set! a 0 #x1234)
       (and (= (bytearray-u16-ref a 0) #x1234)
            (= (bytearray-U16-ref a 0) #x3412)))

     (let ([a (make-bytearray 4 0)])
       (bytearray-fp32-set! a 0 1.5)
       (= (bytearray-fp32-ref a 0) 1.5))

     (bytearray-width-roundtrip? 2 bytearray-u16-set! bytearray-u16-ref
                                  bytearray-U16-set! bytearray-U16-ref 50000)
     (bytearray-width-roundtrip? 2 bytearray-s16-set! bytearray-s16-ref
                                  bytearray-S16-set! bytearray-S16-ref -20000)
     (bytearray-width-roundtrip? 3 bytearray-u24-set! bytearray-u24-ref
                                  bytearray-U24-set! bytearray-U24-ref 1000000)
     (bytearray-width-roundtrip? 3 bytearray-s24-set! bytearray-s24-ref
                                  bytearray-S24-set! bytearray-S24-ref -1000000)
     (bytearray-width-roundtrip? 4 bytearray-u32-set! bytearray-u32-ref
                                  bytearray-U32-set! bytearray-U32-ref 3000000000)
     (bytearray-width-roundtrip? 4 bytearray-s32-set! bytearray-s32-ref
                                  bytearray-S32-set! bytearray-S32-ref -1000000000)
     (bytearray-width-roundtrip? 5 bytearray-u40-set! bytearray-u40-ref
                                  bytearray-U40-set! bytearray-U40-ref 100000000000)
     (bytearray-width-roundtrip? 5 bytearray-s40-set! bytearray-s40-ref
                                  bytearray-S40-set! bytearray-S40-ref -100000000000)
     (bytearray-width-roundtrip? 6 bytearray-u48-set! bytearray-u48-ref
                                  bytearray-U48-set! bytearray-U48-ref 200000000000000)
     (bytearray-width-roundtrip? 6 bytearray-s48-set! bytearray-s48-ref
                                  bytearray-S48-set! bytearray-S48-ref -100000000000000)
     (bytearray-width-roundtrip? 7 bytearray-u56-set! bytearray-u56-ref
                                  bytearray-U56-set! bytearray-U56-ref 50000000000000000)
     (bytearray-width-roundtrip? 7 bytearray-s56-set! bytearray-s56-ref
                                  bytearray-S56-set! bytearray-S56-ref -30000000000000000)
     (bytearray-width-roundtrip? 8 bytearray-u64-set! bytearray-u64-ref
                                  bytearray-U64-set! bytearray-U64-ref 10000000000000000000)
     (bytearray-width-roundtrip? 8 bytearray-s64-set! bytearray-s64-ref
                                  bytearray-S64-set! bytearray-S64-ref -5000000000000000000)
     (bytearray-width-roundtrip? 4 bytearray-fp32-set! bytearray-fp32-ref
                                  bytearray-FP32-set! bytearray-FP32-ref 1.5)
     (bytearray-width-roundtrip? 8 bytearray-fp64-set! bytearray-fp64-ref
                                  bytearray-FP64-set! bytearray-FP64-ref 1.5)

     ;; Width-qualified access rejects storage with trailing partial bytes.
     (error? (bytearray-u16-ref (make-bytearray 1 0) 0))

     ;; Width-qualified access uses logical element indexes.
     (let ([a (make-bytearray 4 0)])
       (bytearray-u16-set! a 1 99)
       (= (bytearray-u16-ref a 1) 99))

     ;; Width-qualified APIs do not accept a runtime endianness argument.
     (error? (bytearray-u16-ref (make-bytearray 2 0) 0 (endianness little)))

     (let ([a (bytearray-u16-iota 4)])
       (bytearray-u16-add*! a 2 8 9)
       (equal? '(0 1 8 9 2 3) (bytearray-u16->list a)))

     (equal? '(0 2 4)
             (bytearray-u16->list
              (bytearray-u16-filter even? (bytearray-u16-iota 5))))

     (equal? '(1 2 3)
             (bytearray-u16->list
              (bytearray-u16-sort < (bytearray-u16-nums 3 0 -1))))

     (let ([a (bytearray-u16-iota 5)])
       (bytearray-u16-copy! a 0 a 1 4)
       (equal? '(0 0 1 2 3) (bytearray-u16->list a)))

     ;; Typed bulk insertion validates all values before changing storage.
     (let ([a (bytearray-u16-iota 2)])
       (and (guard (condition [else #t])
              (bytearray-u16-add*! a 1 3 'bad)
              #f)
            (equal? '(0 1) (bytearray-u16->list a))))

     (let ([a (bytearray-u16-iota 2)])
       (bytearray-u16-add*! a)
       (equal? '(0 1) (bytearray-u16->list a)))

     (equal? '(2 3)
             (bytearray-u16->list
              (bytearray-u16-slice (bytearray-u16-iota 4) -2 99)))

     ;; Numeric constructors reject zero and direction-inconsistent steps.
     (error? (bytearray-u16-nums 0 3 0))
     (error? (bytearray-u16-nums 0 3 -1))
     (equal? '(1.0 2.0)
             (bytearray-fp32->list (bytearray-fp32-nums 1.0 3.0)))

     (let ([seen '()])
       (bytearray-u16-for-each
        (lambda (value) (set! seen (cons value seen)))
        (bytearray-u16-iota 3))
       (equal? '(2 1 0) seen))
     )

(mat bytearray-direct-large
     (let* ([left (bytearray-u16-iota 1000)]
            [right (bytearray-u16-nums 1000 2000)]
            [mapped (bytearray-u16-map + left right)]
            [indexed (bytearray-u16-map/i (lambda (i value) (+ i value)) left)]
            [seen 0])
       (bytearray-u16-for-each (lambda (a b) (set! seen (+ seen a b))) left right)
       (and (= (bytearray-size mapped) 2000)
            (equal? (bytearray-u16->list mapped) (map + (iota 1000) (nums 1000 2000)))
            (equal? (bytearray-u16->list indexed) (map (lambda (i) (* 2 i)) (iota 1000)))
            (= seen (apply + (map + (iota 1000) (nums 1000 2000))))))

     (let-values ([(even odd)
                   (bytearray-u16-partition even? (bytearray-u16-iota 1000))])
       (let ([filtered (bytearray-u16-filter even? (bytearray-u16-iota 1000))])
         (and (= (bytearray-size even) 1000)
            (= (bytearray-size odd) 1000)
            (= (bytearray-size filtered) 1000)
            (equal? (bytearray-u16->list even) (filter even? (iota 1000)))
            (equal? (bytearray-u16->list odd) (filter odd? (iota 1000)))
            (equal? (bytearray-u16->list filtered) (filter even? (iota 1000))))))

     (let ([values (bytearray-u8-iota 100)])
       (and (bytearray-u8-andmap (lambda (value) (< value 100)) values)
            (bytearray-u8-ormap (lambda (value) (= value 99)) values)
            (= (bytearray-u8-fold-left + 0 values) (apply + (iota 100)))
            (= (bytearray-u8-fold-right - 0 values)
               (fold-right - 0 (iota 100)))
            (equal? (bytearray-u8->list
                     (bytearray-u8-map-rev (lambda (value) value) values))
                    (reverse (iota 100)))))

     ;; Reverse traversal accepts multiple bytearrays and preserves source order.
     (let* ([left (bytearray 1 2 3)]
            [right (bytearray 10 20 30)]
            [third (bytearray 100 100 100)]
            [seen '()])
       (bytearray-for-each/i-rev
        (lambda (index x y) (set! seen (cons (list index x y) seen)))
        left right)
       (and (equal? '(133 122 111)
                    (bytearray->list (bytearray-map-rev + left right third)))
            (equal? '((0 1 10) (1 2 20) (2 3 30)) seen)))

     ;; Folds accept multiple bytearrays with the documented accumulator positions.
     (let ([left (bytearray 1 2 3)]
           [right (bytearray 10 20 30)]
           [third (bytearray 100 200 255)])
       (and (= 66 (bytearray-fold-left
                   (lambda (acc x y) (+ acc x y)) 0 left right))
            (= 624 (bytearray-fold-left/i
                    (lambda (index acc x y z) (+ index acc x y z))
                    0 left right third))
            (equal? '((1 10) (2 20) (3 30))
                    (bytearray-fold-right
                     (lambda (x y acc) (cons (list x y) acc))
                     '() left right))))

     ;; Multi-input traversal rejects unequal logical lengths.
     (error? (bytearray-map-rev + (bytearray 1) (bytearray 2 3)))
     (error? (bytearray-fold-left + 0 (bytearray 1) (bytearray 2 3)))

     ;; A callback error leaves the prefix already written by direct bytearray-map!.
     (let ([values (bytearray 1 2 3)])
       (and (guard (condition [else #t])
              (bytearray-map!
               (lambda (value)
                 (if (= value 3) (error 'bytearray-map! "stop") (add1 value)))
               values)
              #f)
            (equal? '(2 3 3) (bytearray->list values))))

     ;; A callback error leaves the prefix already written by direct bytearray-map/i!.
     (let ([values (bytearray 1 2 3)])
       (and (guard (condition [else #t])
              (bytearray-map/i!
               (lambda (index value other third)
                 (if (= index 2)
                     (error 'bytearray-map/i! "stop")
                     (+ index value other third)))
               values (bytearray 10 20 30) (bytearray 100 100 100))
              #f)
            (equal? '(111 123 3) (bytearray->list values))))

     (let* ([values (bytearray-fp32-iota 100)]
            [reversed (bytearray-fp32-map-rev (lambda (value) value) values)]
            [joined (bytearray-fp32-append values reversed)])
       (and (= (bytearray-size values) 400)
            (= (bytearray-size joined) 800)
            (equal? (bytearray-fp32->list reversed)
                    (reverse (map inexact (iota 100))))))

     (let* ([descending (bytearray-u16-nums 1000 0 -1)]
            [sorted (bytearray-u16-sort < descending)])
       (and (= (bytearray-size sorted) 2000)
            (equal? (bytearray-u16->list sorted) (nums 1 1001))))

     (let ([values (bytearray-u16-nums 1000 0 -1)])
       (and (eq? values (bytearray-u16-sort! < values))
            (= (bytearray-size values) 2000)
            (equal? (bytearray-u16->list values) (nums 1 1001))))
     )

(mat flarray
     (let ([a (flarray 1.0 2.0 3.0)])
       (and (= (flarray-size a) 3)
            (not (flarray-empty? a))
            (= (flarray-ref a 1) 2.0)))
     (let ([a (make-flarray)])
       (and (flarray-empty? a)
            (flarray-set! (flarray 1.0) 0 2.0)
            #t))
     ;; Flarray mutation rejects non-flonum values.
     (error? (flarray-set! (flarray 1.0) 0 1))
     (let ([a (make-flarray)])
       (flarray-add! a 4.0)
       (equal? '(4.0) (flarray->list a)))
     (let ([a (flarray 1.0 3.0)])
       (flarray-add! a 1 2.0)
       (equal? '(1.0 2.0 3.0) (flarray->list a)))
     (let ([a (flarray 1.0 2.0)])
       (and (begin (flarray-add*! a) #t)
            (begin (flarray-add*! a 3.0) #t)
            (begin (flarray-add*! a 4.0 5.0) #t)
            (equal? '(1.0 2.0 3.0 4.0 5.0) (flarray->list a))))
     ;; Bulk insertion validates every value before changing the array.
     (let ([a (flarray 1.0 2.0)])
       (and (guard (condition [else #t])
              (flarray-add*! a 1 3.0 'bad)
              #f)
            (equal? '(1.0 2.0) (flarray->list a))))
     (let ([a (flarray 1.0 2.0 3.0)])
       (flarray-delete! a 1)
       (equal? '(1.0 3.0) (flarray->list a)))
     (let ([a (flarray 1.0 2.0)])
       (flarray-clear! a)
       (flarray-empty? a))

     (equal? '(2.0 4.0 6.0)
             (flarray->list (flarray-map (lambda (x) (fl* x 2.0))
                                         (flarray 1.0 2.0 3.0))))

     (= 6.0 (flarray-fold-left fl+ 0.0 (flarray 1.0 2.0 3.0)))

     (equal? '(1.0 2.0 3.0)
             (flarray->list (flarray-sort fl< (flarray 3.0 1.0 2.0))))

     (let* ([a (flarray 1.0 2.0)] [iter (flarray->iter a)])
       (and (equal? '(1.0 2.0) (iter->list iter))
            (let ([again (flarray->iter a)])
              (iter-reset! again)
              (equal? '(1.0 2.0) (iter->list again)))))

     (equal? '(0.0 1.0 2.0) (flarray->list (flarray-iota 3)))
     (equal? '(1.0 1.5 2.0 2.5)
             (flarray->list (flarray-nums 1.0 3.0 0.5)))

     (let ([a (flarray 1.0 4.0)])
       (flarray-add*! a 1 2.0 3.0)
       (equal? '(1.0 2.0 3.0 4.0) (flarray->list a)))

     (equal? (flarray 1.0 2.0) (flarray 1.0 2.0))

     (let ([a (make-flarray 0)])
       (flarray-push-back! a 1.0)
       (equal? '(1.0) (flarray->list a)))

     (equal? '(3.0 2.0 1.0)
             (flarray->list (flarray-reverse (flarray 1.0 2.0 3.0))))
     )

(mat array-add*!
     (let ([a (array 1 4)])
       (array-add*! a 1 2 3)
       (equal? '(1 2 3 4) (array->list a)))

     (let ([a (fxarray 1 4)])
       (fxarray-add*! a 1 2 3)
       (equal? '(1 2 3 4) (fxarray->list a)))

     ;; A bad value is rejected before any mutation occurs.
     (let ([a (fxarray 1 2)])
       (and (guard (condition [else #t])
              (fxarray-add*! a 3 4 'bad 5)
              #f)
            (equal? '(1 2) (fxarray->list a))))

     (let ([a (bytearray 1 4)])
       (bytearray-add*! a 1 2 3)
       (equal? '(1 2 3 4) (bytearray->list a)))
     )


(mat array-add!

     (error? (array-add! (array) 1 1))
     (error? (array-add! (array 1) 2 1))
     (error? (array-add! (array) -1 1))

     (error? (fxarray-add! (fxarray) 'c))
     (error? (fxarray-add! (fxarray) 0.0))
     (error? (bytearray-add! (bytearray) 'c))
     (error? (bytearray-add! (bytearray) 0.0))

     ;; `array` uses `mincap`, hence `make-array`
     (let ([arr (make-array 0 0)])
       (displayln arr)
       (array-add! arr 0)
       (array-add! arr 1)
       (array-add! arr 2)
       (array-add! arr 0 -1)
       (array-add! arr 0 -2)
       (array-add! arr 1 100)
       (array-add! arr 1 200)
       (array-add! arr 3 300)
       (array-add! arr 4 400)
       (displayln arr)
       (and (= 9 (array-size arr))
            (equal? '(-2 200 100 300 400 -1 0 1 2) (array->list arr))))

     (let ([arr (array)] [n 9999])
       (let loop ([i 0])
         (if (fx= i n)
             (and (fx= n (array-size arr))
                  (equal? (iota n) (array->list arr)))
             (begin (array-add! arr i)
                    (loop (fx1+ i))))))

     (let ([arr (make-array 0 0)] [n 9999])
       (let loop ([i 0])
         (if (fx= i n)
             (and (fx= n (array-size arr))
                  (equal? (iota n) (array->list arr)))
             (begin (array-add! arr i)
                    (loop (fx1+ i))))))

     )


(mat array-set!

     (error? (array-set! (array) 1 1))
     (error? (array-set! (array 1) 2 1))
     (error? (array-set! (array) -1 1))

     (let* ([v (random-vector 100 200)]
            [arr (make-array (vector-length v))])
       (vfor-each/i (lambda (i x) (array-set! arr i x)) v)
       (let loop ([i 0])
         (if (fx= i (array-size arr))
             #t
             (and (equal? (vector-ref v i) (array-ref arr i))
                  (loop (fx1+ i))))))

     )


(mat array-clear!

     (error? (array-clear! 42))

     (let ([arr (array 1)])
       (array-clear! arr)
       (array-empty? arr))

     (let ([arr (apply array (iota 10))])
       (array-clear! arr)
       (array-empty? arr))
     )


(mat array-delete!

     (error? (array-delete! 42 42))
     (error? (array-delete! (array) 0))
     (error? (array-delete! (array) 1))
     ;; index is equal to the size
     (error? (array-delete! (array 1) 1))

     ;; index exceeds the size
     (error? (array-delete! (array 1) 3))

     ;; delete first
     (let ([arr (array 1)])
       (array-delete! arr 0)
       (array-empty? arr))

     (let ([arr (array 0 1)])
       (array-delete! arr 0)
       (and (= 1 (array-size arr))
            (= 1 (array-ref arr 0))))

     (let ([arr (array 0 1 2)])
       (array-delete! arr 0)
       (and (= 2 (array-size arr))
            (= 1 (array-ref arr 0))))

     ;; delete last
     (let ([arr (array 0 1)])
       (array-delete! arr 1)
       (and (= 1 (array-size arr))
            (= 0 (array-ref arr 0))))

     (let ([arr (array 0 1 2)])
       (array-delete! arr 2)
       (and (= 2 (array-size arr))
            (= 0 (array-ref arr 0))))

     (let* ([n* (iota 100)]
            [arr (apply array n*)])
       (for-each (lambda (x)
                   (array-delete! arr x))
                 (reverse (nums 0 100 2)))
       (equal? (apply array (nums 1 100 2))
               arr))

     (let* ([n* (iota 100)]
            [arr (apply array n*)])
       (andmap (lambda (x)
                 (array-delete! arr x)
                 (set! n* (remv x n*))
                 (equal? n*
                         (array->list arr)))
               (reverse (nums 0 100 3))))

     )


(mat array-contains?


     (error? (array-contains? 42 42))
     (error? (array-contains? (array)))

     (not (array-contains? (array 10 20) 30))
     (eq? #t (array-contains? (array 10 20) 10))
     (= 1 (array-index-of (array 10 20) 20))
     (= 1 (array-find-index (array 10 20)
                            (lambda (x) (= x 20))))

     (not (fxarray-contains? (fxarray 10 20) 30))
     (eq? #t (fxarray-contains? (fxarray 10 20) 10))
     (= 1 (fxarray-index-of (fxarray 10 20) 20))
     (= 1 (fxarray-find-index (fxarray 10 20)
                              (lambda (x) (= x 20))))

     (not (bytearray-contains? (bytearray 10 20) 30))
     (eq? #t (bytearray-contains? (bytearray 10 20) 10))
     (= 1 (bytearray-index-of (bytearray 10 20) 20))
     (= 1 (bytearray-find-index (bytearray 10 20)
                              (lambda (x) (= x 20))))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (bool (andmap (lambda (x) (array-contains? arr x)) ls)))

     (let* ([ls (random-list 20 30)]
            [arr (apply array ls)])
       (bool (andmap (lambda (x) (array-contains? arr x)) ls)))

     )


(mat array-contains/p?

     (error? (array-contains/p? = 42))
     (error? (array-contains/p? odd? (array)))

     (eq? #t (array-contains/p? (array 1 2) odd?))
     (not (array-contains/p? (array 1 3) even?))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (bool (and (array-contains/p? arr odd?)
                  (array-contains/p? arr even?))))

     )


(mat array-search

     (error? (array-search 42 42))
     (error? (array-search odd? (array)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (and (= 0 (array-search arr even?))
            (= 1 (array-search arr odd?))))

     (let ([arr (array "1" "11" "111" "1111")])
       (and (string=? "1"   (array-search arr (lambda (x) (= 1 (string-length x)))))
            (string=? "111" (array-search arr (lambda (x) (= 3 (string-length x)))))
            (string=? "11"  (array-search arr (lambda (x) (<= 2 (string-length x)))))))

     )

(mat array-search-default
     (let ([absent (vector 'absent)])
       (and (eq? absent (array-search (array 1 2) (lambda (x) (> x 2)) absent))
            (= 1 (array-search (array 1 2) odd? absent))
            (not (array-search (array 1 2) (lambda (x) (> x 2)))))))


(mat array-search*

     (error? (array-search* 42 42))
     (error? (array-search* odd? (array)))


     (let ([arr (array "1" "11" "111" "1111")])
       (and (equal? '("1")
                    (array-search* arr (lambda (x) (= 1 (string-length x)))))
            (equal? '("111")
                    (array-search* arr (lambda (x) (= 3 (string-length x)))))
            (equal? '("11" "111" "1111")
                    (array-search* arr (lambda (x) (<= 2 (string-length x)))))))

     ;; custom collector
     (let ([arr (array "1" "11" "111" "1111")])
       (and (equal? (array "1")
                    (let ([col (array)])
                      (array-search* arr (lambda (x) (= 1 (string-length x))) (lambda (x) (array-add! col x)))
                      col))
            (equal? (array "111")
                    (let ([col (array)])
                      (array-search* arr (lambda (x) (= 3 (string-length x))) (lambda (x) (array-add! col x)))
                      col))
            (equal? (array "11" "111" "1111")
                    (let ([col (array)])
                      (array-search* arr (lambda (x) (<= 2 (string-length x))) (lambda (x) (array-add! col x)))
                      col))))

     )


(mat array-slice

     (equal? '(1) (array->list (array-slice (array 1) 1)))
     (equal? '(1) (array->list (array-slice (array 1) 2)))
     (equal? '(1) (array->list (array-slice (array 1) 3)))
     (equal? '(1) (array->list (array-slice (array 1) 2 -4 -4)))
     (equal? '()  (array->list (array-slice (array 1) 0 1 -1)))

     (equal? '() (array->list (array-slice (array 1) 2 -4)))
     (equal? '() (array->list (array-slice (array 1) 1 5)))
     (equal? '() (array->list (array-slice (array 1) 3 0 3)))

     (begin (define ls (iota 10))
            (define arr (apply array ls))
            #t)


     ;; positive index, forward
     (equal? '(0 1 2 3 4)
             (array->list (array-slice arr 5)))
     (equal? '(0 2 4 6 8)
             (array->list (array-slice arr 0 9 2)))
     (equal? '(2 5 8)
             (array->list (array-slice arr 2 9 3)))


     ;; positive index, backward
     (equal? '(3 2)
             (array->list (array-slice arr 3 1 -1)))
     (equal? '(8 6 4)
             (array->list (array-slice arr 8 2 -2)))
     (equal? '(9 6 3)
             (array->list (array-slice arr 9 1 -3)))
     (equal? '(9 5)
             (array->list (array-slice arr 9 1 -4)))
     (equal? '(9 5 1)
             (array->list (array-slice arr 9 0 -4)))


     ;; negative index, forward
     (equal? '(0 1 2 3 4)
             (array->list (array-slice arr -5)))
     (equal? (iota 9)
             (array->list (array-slice arr -1)))
     (equal? '(5 6 7 8)
             (array->list (array-slice arr -5 -1)))
     (equal? '(9)
             (array->list (array-slice arr -1 -2 -1)))
     (equal? '(1 2 3 4 5 6 7 8)
             (array->list (array-slice arr -9 -1)))
     (equal? '(1 4 7)
             (array->list (array-slice arr -9 -1 3)))


     ;; negative index, backward
     (equal? '(9 8 7 6)
             (array->list (array-slice arr -1 -5 -1)))
     (equal? '(9 7 5 3)
             (array->list (array-slice arr -1 -9 -2)))
     (equal? '(8 4)
             (array->list (array-slice arr -2 -9 -4)))
     (equal? '(1)
             (array->list (array-slice (array 1) -1 -2 -1)))
     )


(mat array-slice!

     (let ([arr (array 1)])
       (array-slice! arr 1)
       (equal? '(1) (array->list arr)))
     (let ([arr (array 1)])
       (array-slice! arr 2)
       (equal? '(1) (array->list arr)))
     (let ([arr (array 1)])
       (array-slice! arr 3)
       (equal? '(1) (array->list arr)))
     (let ([arr (array 1)])
       (array-slice! arr 2 -4 -4)
       (equal? '(1) (array->list arr)))

     ;; bad indices, so no effect
     (let ([arr (array 1)])
       (array-slice! arr 0 1 -1)
       (displayln arr)
       (equal? '(1) (array->list arr)))
     (let ([arr (array 1)])
       (array-slice! arr 2 -4)
       (displayln arr)
       (equal? '(1) (array->list arr)))
     (let ([arr (array 1)])
       (array-slice! arr 1 5)
       (displayln arr)
       (equal? '(1) (array->list arr)))
     (let ([arr (array 1)])
       (array-slice! arr 3 0 3)
       (displayln arr)
       (equal? '(1) (array->list arr)))


     (let ([arr (array 1)])
       (array-slice! arr 1)
       (equal? '(1) (array->list arr)))
     (let ([arr (array 1)])
       (array-slice! arr 2)
       (equal? '(1) (array->list arr)))
     (let ([arr (array 1)])
       (array-slice! arr 3)
       (equal? '(1) (array->list arr)))

     ;; positive index, forward
     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr 5)
       (equal? '(0 1 2 3 4)
               (array->list arr)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr 0 9 2)
       (equal? '(0 2 4 6 8)
               (array->list arr)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr 2 9 3)
       (equal? '(2 5 8)
               (array->list arr)))

     ;; positive index, backward
     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr 3 1 -1)
       (equal? '(3 2)
               (array->list arr)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr 8 2 -2)
       (equal? '(8 6 4)
               (array->list arr)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr 9 1 -3)
       (equal? '(9 6 3)
               (array->list arr)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr 9 1 -4)
       (equal? '(9 5)
               (array->list arr)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr 9 0 -4)
       (equal? '(9 5 1)
               (array->list arr)))


     ;; negative index, forward
     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr -5)
       (equal? '(0 1 2 3 4)
               (array->list arr)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr -1)
       (equal? (iota 9)
               (array->list arr)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr -5 -1)
       (equal? '(5 6 7 8)
               (array->list arr)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr -1 -2 -1)
       (equal? '(9)
               (array->list arr)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr -9 -1)
       (equal? '(1 2 3 4 5 6 7 8)
               (array->list arr)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr -9 -1 3)
       (equal? '(1 4 7)
               (array->list arr)))


     ;; negative index, backward
     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr -1 -5 -1)
       (equal? '(9 8 7 6)
               (array->list arr)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr -1 -9 -2)
       (equal? '(9 7 5 3)
               (array->list arr)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (array-slice! arr -2 -9 -4)
       (equal? '(8 4)
               (array->list arr)))
     (let* ([ls '(1)] [arr (apply array ls)])
       (array-slice! arr -1 -2 -1)
       (equal? ls
               (array->list arr)))

     )


(mat array-copy

     (error? (array-copy))
     (error? (array-copy #f))

     (error? (fxarray-copy))
     (error? (fxarray-copy #f))

     (error? (bytearray-copy))
     (error? (bytearray-copy #f))

     (let* ([arr (apply array (iota 10))]
            [newarr (array-copy arr)])
       (and (equal? arr newarr)
            (not (eq? arr newarr))))

     (let* ([arr (apply fxarray (iota 10))]
            [newarr (fxarray-copy arr)])
       (and (equal? arr newarr)
            (not (eq? arr newarr))))

     (let* ([arr (apply bytearray (iota 10))]
            [newarr (bytearray-copy arr)])
       (and (equal? arr newarr)
            (not (eq? arr newarr))))

     )


(mat array-stack-ops

     (error? (array-pop! (array)))
     (error? (array-pop-back! (array)))

     (begin (define ls (iota 10000))
            #t)

     (let* ([arr (array)])
       (for-each (lambda (x) (array-push! arr x)) ls)
       (equal? (array->list arr) (reverse ls)))

     (let* ([arr (array)])
       (for-each (lambda (x) (array-push! arr x)) ls)
       (equal? (let loop ([res '()])
                 (if (array-empty? arr)
                     res
                     (loop (cons (array-pop! arr) res))))
               ls))


     (let* ([arr (array)])
       (for-each (lambda (x) (array-push-back! arr x)) ls)
       (equal? (array->list arr) ls))

     (let* ([arr (array)])
       (for-each (lambda (x) (array-push-back! arr x)) ls)
       (equal? (let loop ([res '()])
                 (if (array-empty? arr)
                     res
                     (loop (cons (array-pop-back! arr) res))))
               ls))


     )


(mat array-copy!


;;;; same array

     ;; disjoint, left to right
     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 3 3)
       (equal? (array 0 1 2 0 1 2 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 5 3)
       (equal? (array 0 1 2 3 4 0 1 2 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 5 5)
       (equal? (array 0 1 2 3 4 0 1 2 3 4) arr))

     ;; disjoint, right to left
     (let ([arr (apply array (iota 10))])
       (array-copy! arr 5 arr 0 3)
       (equal? (array 5 6 7 3 4 5 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 3 arr 0 3)
       (equal? (array 3 4 5 3 4 5 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 5 arr 0 5)
       (equal? (array 5 6 7 8 9 5 6 7 8 9) arr))

     ;; overlapping, left to right
     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 2 3)
       (equal? (array 0 1 0 1 2 5 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 0 5)
       (equal? (array 0 1 2 3 4 5 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 1 5)
       (equal? (array 0 0 1 2 3 4 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 3 5)
       (equal? (array 0 1 2 0 1 2 3 4 8 9) arr))

     ;; overlapping , right to left
     (let ([arr (apply array (iota 10))])
       (array-copy! arr 2 arr 0 3)
       (equal? (array 2 3 4 3 4 5 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 2 arr 0 5)
       (equal? (array 2 3 4 5 6 5 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 5 arr 3 5)
       (equal? (array 0 1 2 5 6 7 8 9 8 9) arr))



;;;; different array

     (let ([arr (apply array (iota 10))]
           [arr1 (make-array 10 #f)])
       (array-copy! arr 0 arr1 0 3)
       (equal? (array 0 1 2 #f #f #f #f #f #f #f) arr1))

     (let ([arr (apply array (iota 10))]
           [arr1 (make-array 10 #f)])
       (array-copy! arr 0 arr1 0 5)
       (equal? (array 0 1 2 3 4 #f #f #f #f #f) arr1))

     (let ([arr (apply array (iota 10))]
           [arr1 (make-array 10 #f)])
       (array-copy! arr 0 arr1 3 5)
       (equal? (array #f #f #f 0 1 2 3 4 #f #f) arr1))

     (let ([arr (apply array (iota 10))]
           [arr1 (make-array 10 #f)])
       (array-copy! arr 2 arr1 0 5)
       (equal? (array 2 3 4 5 6 #f #f #f #f #f) arr1))


     )


(mat fxarray-copy!


;;;; same fxarray

     ;; disjoint, left to right
     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 3 3)
       (equal? (fxarray 0 1 2 0 1 2 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 5 3)
       (equal? (fxarray 0 1 2 3 4 0 1 2 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 5 5)
       (equal? (fxarray 0 1 2 3 4 0 1 2 3 4) arr))

     ;; disjoint, right to left
     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 5 arr 0 3)
       (equal? (fxarray 5 6 7 3 4 5 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 3 arr 0 3)
       (equal? (fxarray 3 4 5 3 4 5 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 5 arr 0 5)
       (equal? (fxarray 5 6 7 8 9 5 6 7 8 9) arr))

     ;; overlapping, left to right
     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 2 3)
       (equal? (fxarray 0 1 0 1 2 5 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 0 5)
       (equal? (fxarray 0 1 2 3 4 5 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 1 5)
       (equal? (fxarray 0 0 1 2 3 4 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 3 5)
       (equal? (fxarray 0 1 2 0 1 2 3 4 8 9) arr))

     ;; overlapping , right to left
     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 2 arr 0 3)
       (equal? (fxarray 2 3 4 3 4 5 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 2 arr 0 5)
       (equal? (fxarray 2 3 4 5 6 5 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 5 arr 3 5)
       (equal? (fxarray 0 1 2 5 6 7 8 9 8 9) arr))



;;;; different fxarray

     (let ([arr (apply fxarray (iota 10))]
           [arr1 (make-fxarray 10 -1)])
       (fxarray-copy! arr 0 arr1 0 3)
       (equal? (fxarray 0 1 2 -1 -1 -1 -1 -1 -1 -1) arr1))

     (let ([arr (apply fxarray (iota 10))]
           [arr1 (make-fxarray 10 -1)])
       (fxarray-copy! arr 0 arr1 0 5)
       (equal? (fxarray 0 1 2 3 4 -1 -1 -1 -1 -1) arr1))

     (let ([arr (apply fxarray (iota 10))]
           [arr1 (make-fxarray 10 -1)])
       (fxarray-copy! arr 0 arr1 3 5)
       (equal? (fxarray -1 -1 -1 0 1 2 3 4 -1 -1) arr1))

     (let ([arr (apply fxarray (iota 10))]
           [arr1 (make-fxarray 10 -1)])
       (fxarray-copy! arr 2 arr1 0 5)
       (equal? (fxarray 2 3 4 5 6 -1 -1 -1 -1 -1) arr1))


     )


(mat *array-sorted?

     (array-sorted? < (array))
     (fxarray-sorted? < (fxarray))
     (bytearray-sorted? < (bytearray))

     (array-sorted? < (array 42))
     (fxarray-sorted? < (fxarray 42))
     (bytearray-sorted? < (bytearray 42))

     (error? (array-sorted? 1 (array)))
     (error? (fxarray-sorted? 1 (fxarray)))
     (error? (bytearray-sorted? 1 (bytearray)))
     (error? (array-sorted? 1 (array 42)))
     (error? (fxarray-sorted? 1 (fxarray 42)))
     (error? (bytearray-sorted? 1 (bytearray 42)))

     (error? (array-sorted? < '()))
     (error? (fxarray-sorted? < '()))
     (error? (bytearray-sorted? < '()))
     (error? (array-sorted? < '()))
     (error? (fxarray-sorted? < '()))
     (error? (bytearray-sorted? < '()))

     (error? (array-sorted? < (array 1 2 3 4) -1))
     (error? (array-sorted? < (array 1 2 3 4) 0 5))
     (error? (array-sorted? < (array 1 2 3 4) 4 2))

     (error? (fxarray-sorted? < (fxarray 1 2 3 4) -1))
     (error? (fxarray-sorted? < (fxarray 1 2 3 4) 0 5))
     (error? (fxarray-sorted? < (fxarray 1 2 3 4) 4 2))

     (error? (bytearray-sorted? < (bytearray 1 2 3 4) -1))
     (error? (bytearray-sorted? < (bytearray 1 2 3 4) 0 5))
     (error? (bytearray-sorted? < (bytearray 1 2 3 4) 4 2))

     (let ([arr (array 9 8 7 1 2 3 4 0)])
       (and (not (array-sorted? < arr))
            (array-sorted? < arr 3 7)))

     (begin (define (test n)
              (let ([arr (apply array (iota n))])
                (and (array-sorted? < arr)
                     (not (array-sorted? > arr)))))
            (define (fxtest n)
              (let ([arr (apply fxarray (iota n))])
                (and (fxarray-sorted? < arr)
                     (not (fxarray-sorted? > arr)))))
            (define (u8test n)
              (let ([arr (apply bytearray (iota n))])
                (and (bytearray-sorted? < arr)
                     (not (bytearray-sorted? > arr)))))
            #t)

     (test 10)
     (test 100)
     (test 1000)

     (fxtest 10)
     (fxtest 100)
     (fxtest 1000)

     (u8test 10)
     (u8test 100)
     (u8test 255)


     )


(mat array-iota

     (error? (array-iota -1))
     (error? (array-iota -#f))

     ;; out of range
     (error? (bytearray-iota 500))

     (equal? (apply array (iota 10))
             (array-iota 10))
     (equal? (array)
             (array-iota 0))

     (equal? (apply fxarray (iota 10))
             (fxarray-iota 10))
     (equal? (fxarray)
             (fxarray-iota 0))

     (equal? (apply bytearray (iota 10))
             (bytearray-iota 10))
     (equal? (bytearray)
             (bytearray-iota 0))

     )


(mat array-nums

     (error? (array-nums 'x 'x 'x))
     (error? (array-nums 0 10  -1))
     (error? (array-nums 0 -10 1))

     (equal? (array-nums 0 10)
             (array-iota 10))
     (equal? (array-nums 5 10)
             (array 5 6 7 8 9))
     (equal? (array-nums 5 15 3)
             (array 5 8 11 14))
     (equal? (array-nums 0 -10 -1)
             (array 0 -1 -2 -3 -4 -5 -6 -7 -8 -9))
     (equal? (array-nums 0 -10 -2)
             (array 0 -2 -4 -6 -8))

     (equal? (nums -10 10.5 2.6)
             (array->list (array-nums -10 10.5 2.6)))
     (equal? (nums 10 -10.5 -2.6)
             (array->list (array-nums 10 -10.5 -2.6)))

     (equal? (nums
              (most-positive-fixnum)  (+ 50  (most-positive-fixnum)) 3.14)
             (array->list (array-nums
                           (most-positive-fixnum) (+ 50  (most-positive-fixnum)) 3.14)))
     (equal? (nums
              (most-positive-fixnum) (+ 5000 (most-positive-fixnum)) 333.14)
             (array->list (array-nums
                           (most-positive-fixnum) (+ 5000 (most-positive-fixnum)) 333.14)))

     (equal? (fxarray-nums 0 10)
             (fxarray-iota 10))
     (equal? (fxarray-nums 5 10)
             (fxarray 5 6 7 8 9))
     (equal? (fxarray-nums 5 15 3)
             (fxarray 5 8 11 14))
     (equal? (fxarray-nums 0 -10 -1)
             (fxarray 0 -1 -2 -3 -4 -5 -6 -7 -8 -9))
     (equal? (fxarray-nums 0 -10 -2)
             (fxarray 0 -2 -4 -6 -8))

     (equal? (bytearray-nums 0 10)
             (bytearray-iota 10))
     (equal? (bytearray-nums 5 10)
             (bytearray 5 6 7 8 9))
     (equal? (bytearray-nums 5 15 3)
             (bytearray 5 8 11 14))

     )


(mat *array-sort

     ;; bad <?
     (error? (array-sort!   1 (array)))
     (error? (fxarray-sort! 1 (fxarray)))
     ;; not arrays
     (error? (array-sort!   <= '(1 2 3)))
     (error? (fxarray-sort! <= '(1 2 3)))
     ;; bad range
     (error? (array-sort!   <= (array) 3))
     (error? (fxarray-sort! <= (fxarray) 3))
     (error? (array-sort!   <= (array   2 2 2 2 2) 6 3))
     (error? (fxarray-sort! <= (fxarray 2 2 2 2 2) 6 3))

     ;; bad <?
     (error? (array-sort   1 (array)))
     (error? (fxarray-sort 1 (fxarray)))
     ;; not arrays
     (error? (array-sort   <= '(1 2 3)))
     (error? (fxarray-sort <= '(1 2 3)))
     ;; bad range
     (error? (array-sort   <= (array) 3))
     (error? (fxarray-sort <= (fxarray) 3))
     (error? (array-sort   <= (array   2 2 2 2 2) 6 3))
     (error? (fxarray-sort <= (fxarray 2 2 2 2 2) 6 3))


     ;; full range, in place
     (begin (define (test1 rand <? sort! sorted? v->a)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let* ([v (rand #e1e6 bd)] [arr (v->a v)])
                                    (sort! <? arr)
                                    (sorted? <? arr)))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test1 random-vector   fx<= array-sort!   array-sorted?   vector->array)
     (test1 random-fxvector fx<= fxarray-sort! fxarray-sorted? fxvector->fxarray)


     ;; full range, return new
     (begin (define (test2 rand <? sort sorted? v->a)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let* ([v (rand #e1e6 bd)] [arr (v->a v)])
                                    (sorted? <? (sort <? arr))))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test2 random-vector   fx<= array-sort   array-sorted?   vector->array)
     (test2 random-fxvector fx<= fxarray-sort fxarray-sorted? fxvector->fxarray)



     ;; ranged, in place
     (begin (define (test3 rand <? sort! sorted? v->a)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let* ([v (rand #e1e6 bd)]
                                         [mid (fx/ #e1e6 2)]
                                         [arr (v->a v)])
                                    (sort! <? arr mid)
                                    (sort! <? arr mid #e1e6)
                                    (and (sorted? <? arr 0 mid)
                                         (sorted? <? arr mid #e1e6))))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test3 random-vector   fx<= array-sort!   array-sorted?   vector->array)
     (test3 random-fxvector fx<= fxarray-sort! fxarray-sorted? fxvector->fxarray)


     ;; ranged, return new
     (begin (define (test4 rand <? sort sorted? v->a)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let* ([v (rand #e1e6 bd)]
                                         [mid (fx/ #e1e6 2)]
                                         [arr (v->a v)])
                                    (and (sorted? <? (sort <? arr mid))
                                         (sorted? <? (sort <? arr mid #e1e6)))))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test4 random-vector   fx<= array-sort   array-sorted?   vector->array)
     (test4 random-fxvector fx<= fxarray-sort fxarray-sorted? fxvector->fxarray)

     )


(mat array<->vector

     (error? (array->vector))
     (error? (fxarray->fxvector))
     (error? (bytearray->bytevector))

     (error? (array->vector '()))
     (error? (fxarray->fxvector '()))
     (error? (bytearray->bytevector '()))

     (error? (array->vector '#()))
     (error? (fxarray->fxvector '#()))
     (error? (bytearray->bytevector '#()))

     (let ([v (random-vector 100)])
       (equal? v (array->vector (vector->array v))))

     (let ([v (random-fxvector 100)])
       (equal? v (fxarray->fxvector (fxvector->fxarray v))))

     (let ([v (random-u8vec 100 200)])
       (equal? v (bytearray->bytevector (bytevector->bytearray v))))


     (let ([v (random-vector 10000)])
       (equal? v (array->vector (vector->array v))))

     (let ([v (random-fxvector 10000)])
       (equal? v (fxarray->fxvector (fxvector->fxarray v))))

     (let ([v (random-u8vec 100 20000)])
       (equal? v (bytearray->bytevector (bytevector->bytearray v))))

     )


(mat array-iter-directional

     (equal? '(0 1 2) (iter->list (array->iter (array 0 1 2))))
     (equal? '(8 6 4) (iter->list (array->iter (array 0 1 2 3 4 5 6 7 8 9)
                                               8 2 -2)))
     (equal? '(8 6 4) (iter->list (fxarray->iter (fxarray 0 1 2 3 4 5 6 7 8 9)
                                                 8 2 -2)))
     (equal? '(8 6 4) (iter->list (bytearray->iter (bytearray 0 1 2 3 4 5 6 7 8 9)
                                                 8 2 -2)))
     (equal? '() (iter->list (fxarray->iter (fxarray 1 2) 0 2 -1)))

     ;; A zero step cannot advance an array iterator.
     (error? (array->iter (array 1 2) 0 2 0))

     (let ([iter (array->iter (array 0 1 2 3 4 5 6 7 8 9) 8 2 -2)])
       (and (equal? '(8 6 4) (collect-array-iter iter))
            (begin
              (iter-reset! iter)
              (equal? '(8 6 4) (collect-array-iter iter)))))
     (let ([iter (fxarray->iter (fxarray 0 1 2 3 4 5 6 7 8 9) 8 2 -2)])
       (and (equal? '(8 6 4) (collect-array-iter iter))
            (begin
              (iter-reset! iter)
              (equal? '(8 6 4) (collect-array-iter iter)))))
     (let ([iter (bytearray->iter (bytearray 0 1 2 3 4 5 6 7 8 9) 8 2 -2)])
       (and (equal? '(8 6 4) (collect-array-iter iter))
            (begin
              (iter-reset! iter)
              (equal? '(8 6 4) (collect-array-iter iter)))))

     (let* ([arr (array 0 1 2)]
            [iter (array->iter arr)])
       (array-set! arr 1 9)
       (equal? '(0 9 2) (iter->list iter)))
     (let* ([arr (fxarray 0 1 2)]
            [iter (fxarray->iter arr)])
       (fxarray-set! arr 1 9)
       (equal? '(0 9 2) (iter->list iter)))
     (let* ([arr (bytearray 0 1 2)]
            [iter (bytearray->iter arr)])
       (bytearray-set! arr 1 9)
       (equal? '(0 9 2) (iter->list iter)))

     (let* ([arr (array 0 1)]
            [iter (array->iter arr)])
       (and (equal? '(0 1) (collect-array-iter iter))
            (begin
              (array-push-back! arr 2)
              (iter-reset! iter)
              (equal? '(0 1 2) (collect-array-iter iter)))))
     (let* ([arr (fxarray 0 1)]
            [iter (fxarray->iter arr)])
       (and (equal? '(0 1) (collect-array-iter iter))
            (begin
              (fxarray-push-back! arr 2)
              (iter-reset! iter)
              (equal? '(0 1 2) (collect-array-iter iter)))))
     (let* ([arr (bytearray 0 1)]
            [iter (bytearray->iter arr)])
       (and (equal? '(0 1) (collect-array-iter iter))
            (begin
              (bytearray-push-back! arr 2)
              (iter-reset! iter)
              (equal? '(0 1 2) (collect-array-iter iter)))))

     )


(mat array-map

     ;; type error
     (error? (array-map #f (array)))
     (error? (array-map odd? (fxarray 1)))
     (error? (fxarray-map odd? (array 1)))

     (array-empty? (array-map   + (array)))
     (fxarray-empty? (fxarray-map + (fxarray)))
     (bytearray-empty? (bytearray-map + (bytearray)))

     (array-empty? (array-map   + (array) (array)))
     (fxarray-empty? (fxarray-map + (fxarray) (fxarray)))
     (bytearray-empty? (bytearray-map + (bytearray) (bytearray)))

     (array-empty? (array-map   + (array) (array) (array) (array) (array)))
     (fxarray-empty? (fxarray-map + (fxarray) (fxarray) (fxarray) (fxarray) (fxarray)))
     (bytearray-empty? (bytearray-map + (bytearray) (bytearray) (bytearray) (bytearray) (bytearray)))

     ;; length not equal
     (error? (array-map   + (array) (array 1)))
     (error? (fxarray-map + (fxarray) (fxarray 1)))
     (error? (bytearray-map + (bytearray) (bytearray 1)))

     (error? (array-map   + (array) (array 1) (array) (array 1 1) (array)))
     (error? (fxarray-map + (fxarray) (fxarray 1) (fxarray) (fxarray 1 1) (fxarray)))
     (error? (bytearray-map + (bytearray) (bytearray 1) (bytearray) (bytearray 1 1) (bytearray)))


     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (map add1 ls0))
               (array-map add1 arr0)))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (map + ls0 ls0))
               (array-map + arr0 arr0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (apply array (map + ls0 ls0))
               (array-map + arr0 arr1)))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (apply array (map + ls0 ls0 ls0 ls0 ls0))
               (array-map + arr0 arr1 arr2 arr3 arr4)))
     )


(mat array-map/i

     ;; type error
     (error? (array-map/i #f (array)))
     (error? (array-map/i odd? (fxarray 1)))
     (error? (fxarray-map/i odd? (array 1)))

     ;; arity error
     (error? (fxarray-map/i odd? (fxarray 1)))

     (array-empty? (array-map/i   + (array)))
     (fxarray-empty? (fxarray-map/i + (fxarray)))
     (bytearray-empty? (bytearray-map/i + (bytearray)))

     (array-empty? (array-map/i   + (array) (array)))
     (fxarray-empty? (fxarray-map/i + (fxarray) (fxarray)))
     (bytearray-empty? (bytearray-map/i + (bytearray) (bytearray)))

     (array-empty? (array-map/i   + (array) (array) (array) (array) (array)))
     (fxarray-empty? (fxarray-map/i + (fxarray) (fxarray) (fxarray) (fxarray) (fxarray)))
     (bytearray-empty? (bytearray-map/i + (bytearray) (bytearray) (bytearray) (bytearray) (bytearray)))

     ;; length not equal
     (error? (array-map/i   + (array) (array 1)))
     (error? (fxarray-map/i + (fxarray) (fxarray 1)))
     (error? (bytearray-map/i + (bytearray) (bytearray 1)))

     (error? (array-map/i   + (array) (array 1) (array) (array 1 1) (array)))
     (error? (fxarray-map/i + (fxarray) (fxarray 1) (fxarray) (fxarray 1 1) (fxarray)))
     (error? (bytearray-map/i + (bytearray) (bytearray 1) (bytearray) (bytearray 1 1) (bytearray)))


     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-map/i (lambda (i x) (list i x)) arr0)
               (apply array (zip ls0 ls0))))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-map/i (lambda (i x y) (list i x y)) arr0 arr1)
               (apply array (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-map/i (lambda (i x y) (list i x y)) arr0 arr0)
               (apply array (zip ls0 ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-map/i (lambda (i x0 x1 x2 x3 x4) (list i x0 x1 x2 x3 x4)) arr0 arr1 arr2 arr3 arr4)
               (apply array (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-map/i (lambda (i x0 x1 x2 x3 x4) (list i x0 x1 x2 x3 x4)) arr0 arr0 arr0 arr0 arr0)
               (apply array (zip ls0 ls0 ls0 ls0 ls0 ls0))))


     )


(mat array-map!

     ;; type error
     (error? (array-map! #f (array)))
     (error? (array-map! odd? (fxarray 1)))
     (error? (fxarray-map! odd? (array 1)))

     (array-empty? (array-map!   + (array)))
     (fxarray-empty? (fxarray-map! + (fxarray)))
     (bytearray-empty? (bytearray-map! + (bytearray)))

     (array-empty? (array-map!   + (array) (array)))
     (fxarray-empty? (fxarray-map! + (fxarray) (fxarray)))
     (bytearray-empty? (bytearray-map! + (bytearray) (bytearray)))

     (array-empty? (array-map!   + (array) (array) (array) (array) (array)))
     (fxarray-empty? (fxarray-map! + (fxarray) (fxarray) (fxarray) (fxarray) (fxarray)))
     (bytearray-empty? (bytearray-map! + (bytearray) (bytearray) (bytearray) (bytearray) (bytearray)))

     ;; length not equal
     (error? (array-map!   + (array) (array 1)))
     (error? (fxarray-map! + (fxarray) (fxarray 1)))
     (error? (bytearray-map! + (bytearray) (bytearray 1)))

     (error? (array-map!   + (array) (array 1) (array) (array 1 1) (array)))
     (error? (fxarray-map! + (fxarray) (fxarray 1) (fxarray) (fxarray 1 1) (fxarray)))
     (error? (bytearray-map! + (bytearray) (bytearray 1) (bytearray) (bytearray 1 1) (bytearray)))


     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (map add1 ls0))
               (array-map! add1 arr0)))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (apply array (map + ls0 ls0))
               (array-map! + arr0 arr1)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (array-map! + arr0 arr0)
       (equal? (apply array (map + ls0 ls0))
               arr0))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (apply array (map + ls0 ls0 ls0 ls0 ls0))
               (array-map! + arr0 arr1 arr2 arr3 arr4)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (map + ls0 ls0 ls0 ls0 ls0))
               (array-map! + arr0 arr0 arr0 arr0 arr0)))

     )


(mat array-map/i!

     ;; type error
     (error? (array-map/i! #f (array)))
     (error? (array-map/i! odd? (fxarray 1)))
     (error? (fxarray-map/i! odd? (array 1)))

     ;; arity error
     (error? (fxarray-map/i! odd? (fxarray 1)))

     (array-empty? (array-map/i!   + (array)))
     (fxarray-empty? (fxarray-map/i! + (fxarray)))
     (bytearray-empty? (bytearray-map/i! + (bytearray)))

     (array-empty? (array-map/i!   + (array) (array)))
     (fxarray-empty? (fxarray-map/i! + (fxarray) (fxarray)))
     (bytearray-empty? (bytearray-map/i! + (bytearray) (bytearray)))

     (array-empty? (array-map/i!   + (array) (array) (array) (array) (array)))
     (fxarray-empty? (fxarray-map/i! + (fxarray) (fxarray) (fxarray) (fxarray) (fxarray)))
     (bytearray-empty? (bytearray-map/i! + (bytearray) (bytearray) (bytearray) (bytearray) (bytearray)))

     ;; length not equal
     (error? (array-map/i!   + (array) (array 1)))
     (error? (fxarray-map/i! + (fxarray) (fxarray 1)))
     (error? (bytearray-map/i! + (bytearray) (bytearray 1)))

     (error? (array-map/i!   + (array) (array 1) (array) (array 1 1) (array)))
     (error? (fxarray-map/i! + (fxarray) (fxarray 1) (fxarray) (fxarray 1 1) (fxarray)))
     (error? (bytearray-map/i! + (bytearray) (bytearray 1) (bytearray) (bytearray 1 1) (bytearray)))

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-map/i! (lambda (i x) (list i x)) arr0)
               (apply array (zip ls0 ls0))))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-map/i! (lambda (i x y) (list i x y)) arr0 arr1)
               (apply array (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-map/i! (lambda (i x y) (list i x y)) arr0 arr0)
               (apply array (zip ls0 ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-map/i! (lambda (i x0 x1 x2 x3 x4) (list i x0 x1 x2 x3 x4)) arr0 arr1 arr2 arr3 arr4)
               (apply array (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-map/i! (lambda (i x0 x1 x2 x3 x4) (list i x0 x1 x2 x3 x4)) arr0 arr0 arr0 arr0 arr0)
               (apply array (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     )


(mat array-for-each

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each (lambda (x) (set! ls (cons x ls)))
                       arr0)
       (equal? ls (reverse ls0)))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [ls '()])
       (array-for-each (lambda (x y) (set! ls (cons (list x y) ls)))
                       arr0 arr1)
       (equal? ls (reverse (zip ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each (lambda (x y) (set! ls (cons (list x y) ls)))
                       arr0 arr0)
       (equal? ls (reverse (zip ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)]
            [ls '()])
       (array-for-each (lambda (x0 x1 x2 x3 x4) (set! ls (cons (list x0 x1 x2 x3 x4) ls)))
                       arr0 arr1 arr2 arr3 arr4)
       (equal? ls (reverse (zip ls0 ls0 ls0 ls0 ls0))))

     )


(mat array-for-each/i

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each/i (lambda (i x) (set! ls (cons (list i x) ls)))
                         arr0)
       (equal? ls (reverse (zip ls0 ls0))))

     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [ls '()])
       (array-for-each/i (lambda (i x y) (set! ls (cons (list i x y) ls)))
                         arr0 arr1)
       (equal? ls (reverse (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each/i (lambda (i x y) (set! ls (cons (list i x y) ls)))
                         arr0 arr0)
       (equal? ls (reverse (zip ls0 ls0 ls0))))

     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)]
            [ls '()])
       (array-for-each/i (lambda (i x0 x1 x2 x3 x4) (set! ls (cons (list i x0 x1 x2 x3 x4) ls)))
                         arr0 arr1 arr2 arr3 arr4)
       (equal? ls (reverse (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     )


(mat array-map-rev

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (reverse (map add1 ls0)))
               (array-map-rev add1 arr0)))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0)))
               (array-map-rev + arr0 arr0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0)))
               (array-map-rev + arr0 arr1)))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0 ls0 ls0 ls0)))
               (array-map-rev + arr0 arr1 arr2 arr3 arr4)))
     )


(mat array-map/i-rev

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0)))
               (array-map/i-rev + arr0)))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0 ls0)))
               (array-map/i-rev + arr0 arr0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0 ls0)))
               (array-map/i-rev + arr0 arr1)))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0 ls0 ls0 ls0 ls0)))
               (array-map/i-rev + arr0 arr1 arr2 arr3 arr4)))

     )


(mat array-for-each-rev

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each-rev (lambda (x) (set! ls (cons x ls)))
                           arr0)
       (equal? ls ls0))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [ls '()])
       (array-for-each-rev (lambda (x y) (set! ls (cons (list x y) ls)))
                           arr0 arr1)
       (equal? ls (zip ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each-rev (lambda (x y) (set! ls (cons (list x y) ls)))
                           arr0 arr0)
       (equal? ls (zip ls0 ls0)))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)]
            [ls '()])
       (array-for-each-rev (lambda (x0 x1 x2 x3 x4) (set! ls (cons (list x0 x1 x2 x3 x4) ls)))
                           arr0 arr1 arr2 arr3 arr4)
       (equal? ls (zip ls0 ls0 ls0 ls0 ls0)))

     )


(mat array-for-each/i-rev

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each/i-rev (lambda (i x) (set! ls (cons (list i x) ls)))
                             arr0)
       (equal? ls (zip ls0 ls0)))

     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [ls '()])
       (array-for-each/i-rev (lambda (i x y) (set! ls (cons (list i x y) ls)))
                             arr0 arr1)
       (equal? ls (zip ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each/i-rev (lambda (i x y) (set! ls (cons (list i x y) ls)))
                             arr0 arr0)
       (equal? ls (zip ls0 ls0 ls0)))

     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)]
            [ls '()])
       (array-for-each/i-rev (lambda (i x0 x1 x2 x3 x4) (set! ls (cons (list i x0 x1 x2 x3 x4) ls)))
                             arr0 arr1 arr2 arr3 arr4)
       (equal? ls (zip ls0 ls0 ls0 ls0 ls0 ls0)))

     )


(mat array-andmap

     (error? (array-andmap #f (array)))
     (error? (array-andmap odd? (fxarray 1)))
     (error? (fxarray-andmap odd? (array 1)))

     (array-andmap   odd? (array))
     (fxarray-andmap odd? (fxarray))
     (bytearray-andmap odd? (bytearray))

     (array-andmap   odd? (array) (array))
     (fxarray-andmap odd? (fxarray) (fxarray))
     (bytearray-andmap odd? (bytearray) (bytearray))

     (array-andmap   odd? (array) (array) (array) (array) (array))
     (fxarray-andmap odd? (fxarray) (fxarray) (fxarray) (fxarray) (fxarray))
     (bytearray-andmap odd? (bytearray) (bytearray) (bytearray) (bytearray) (bytearray))

     (error? (array-andmap   odd? (array) (array 1)))
     (error? (fxarray-andmap odd? (fxarray) (fxarray 1)))
     (error? (bytearray-andmap odd? (bytearray) (bytearray 1)))

     (error? (array-andmap   odd? (array) (array 1) (array) (array 1 1) (array)))
     (error? (fxarray-andmap odd? (fxarray) (fxarray 1) (fxarray) (fxarray 1 1) (fxarray)))
     (error? (bytearray-andmap odd? (bytearray) (bytearray 1) (bytearray) (bytearray 1 1) (bytearray)))

     ;; 1 arr
     (begin (define (test1 arr-proc andmap-proc)
              (let* ([n* (nums 1 100 2)]
                     [arr0 (apply arr-proc n*)])
                (andmap-proc odd? arr0)))
            #t)

     (test1 array   array-andmap)
     (test1 fxarray fxarray-andmap)
     (test1 bytearray bytearray-andmap)

     ;; 2 arrs
     (begin (define (test2 arr-proc andmap-proc)
              (let* ([n* (nums 1 100 2)]
                     [arr0 (apply arr-proc n*)]
                     [arr1 (apply arr-proc n*)])
                (andmap-proc = arr0 arr1)))
            #t)
     (test2 array   array-andmap)
     (test2 fxarray fxarray-andmap)
     (test2 bytearray bytearray-andmap)

     ;; more arrs
     (begin (define (test* arr-proc andmap-proc map-proc)
              (let* ([n* (nums 1 50 2)]
                     [arr0 (apply arr-proc n*)]
                     [arr1 (apply arr-proc n*)]
                     [arr2 (apply arr-proc n*)]
                     [arr3 (map-proc + arr0 arr1 arr2)])
                (andmap-proc (lambda (a b c d) (= d (+ a b c)))
                             arr0 arr1 arr2 arr3)))
            #t)
     (test* array   array-andmap   array-map)
     (test* fxarray fxarray-andmap fxarray-map)
     (test* bytearray bytearray-andmap bytearray-map)

     )


(mat array-ormap

     (error? (array-ormap #f (array)))
     (error? (array-ormap odd? (fxarray 1)))
     (error? (fxarray-ormap odd? (array 1)))

     (not (array-ormap   odd? (array)))
     (not (fxarray-ormap odd? (fxarray)))
     (not (bytearray-ormap odd? (bytearray)))

     (not (array-ormap   odd? (array) (array)))
     (not (fxarray-ormap odd? (fxarray) (fxarray)))
     (not (bytearray-ormap odd? (bytearray) (bytearray)))

     (not (array-ormap   odd? (array) (array) (array) (array) (array)))
     (not (fxarray-ormap odd? (fxarray) (fxarray) (fxarray) (fxarray) (fxarray)))
     (not (bytearray-ormap odd? (bytearray) (bytearray) (bytearray) (bytearray) (bytearray)))

     (error? (array-ormap   odd? (array) (array 1)))
     (error? (fxarray-ormap odd? (fxarray) (fxarray 1)))
     (error? (bytearray-ormap odd? (bytearray) (bytearray 1)))

     (error? (array-ormap   odd? (array) (array 1) (array) (array 1 1) (array)))
     (error? (fxarray-ormap odd? (fxarray) (fxarray 1) (fxarray) (fxarray 1 1) (fxarray)))
     (error? (bytearray-ormap odd? (bytearray) (bytearray 1) (bytearray) (bytearray 1 1) (bytearray)))


     ;; 1 arr
     (begin (define (test1 arr-proc ormap-proc)
              (let* ([n* (snoc! (nums 1 100 2) 2)]
                     [arr0 (apply arr-proc n*)])
                (ormap-proc even? arr0)))
            #t)

     (test1 array   array-ormap)
     (test1 fxarray fxarray-ormap)
     (test1 bytearray bytearray-ormap)

     ;; 2 arrs
     (begin (define (test2 arr-proc ormap-proc)
              (let* ([n* (nums 1 100 2)]
                     [arr0 (apply arr-proc n*)]
                     [arr1 (apply arr-proc n*)])
                (ormap-proc (lambda (a b) (= (+ 49 49) (+ a b)))
                            arr0 arr1)))
            #t)
     (test2 array   array-ormap)
     (test2 fxarray fxarray-ormap)
     (test2 bytearray bytearray-ormap)

     ;; more arrs
     (begin (define (test* arr-proc ormap-proc map-proc)
              (let* ([n* (nums 1 50 2)]
                     [arr0 (apply arr-proc n*)]
                     [arr1 (apply arr-proc n*)]
                     [arr2 (apply arr-proc n*)]
                     [arr3 (apply arr-proc n*)])
                (ormap-proc (lambda (a b c d) (= (+ 33 33 33 33) (+ a b c d)))
                             arr0 arr1 arr2 arr3)))
            #t)
     (test* array   array-ormap   array-map)
     (test* fxarray fxarray-ormap fxarray-map)
     (test* bytearray bytearray-ormap bytearray-map)

     )


(mat array-fold-left

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left (lambda (acc x) (cons x acc))
                                '() arr0)
               (reverse ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left (lambda (acc x) (fx+ acc x))
                                0 arr0)
               (apply fx+ ls0)))

     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-left (lambda (acc x y) (cons (list x y) acc))
                                '() arr0 arr1)
               (reverse (zip ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left (lambda (acc x y) (cons (list x y) acc))
                                '() arr0 arr0)
               (reverse (zip ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-left (lambda (acc x y) (fx+ acc x y))
                                0 arr0 arr1)
               (apply fx+ (map fx+ ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left (lambda (acc x y) (fx+ acc x y))
                                0 arr0 arr0)
               (apply fx+ (map fx+ ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)]
            [ls '()])
       (equal? (array-fold-left (lambda (acc x0 x1 x2 x3 x4) (cons (list x0 x1 x2 x3 x4) acc))
                                '() arr0 arr1 arr2 arr3 arr4)
               (reverse (zip ls0 ls0 ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)]
            [ls '()])
       (equal? (array-fold-left (lambda (acc x0 x1 x2 x3 x4) (fx+ acc x0 x1 x2 x3 x4))
                                0 arr0 arr1 arr2 arr3 arr4)
               (apply fx+ (map fx+ ls0 ls0 ls0 ls0 ls0))))

     )


(mat array-fold-left/i

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x) (cons (list i x) acc))
                                  '() arr0)
               (reverse (zip ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x) (fx+ i acc x))
                                  0 arr0)
               (apply fx+ (map fx+ ls0 ls0))))

     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x y) (cons (list i x y) acc))
                                  '() arr0 arr1)
               (reverse (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x y) (cons (list i x y) acc))
                                  '() arr0 arr0)
               (reverse (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x y) (fx+ i acc x y))
                                  0 arr0 arr1)
               (apply fx+ (map fx+ ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x y) (fx+ i acc x y))
                                  0 arr0 arr0)
               (apply fx+ (map fx+ ls0 ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x0 x1 x2 x3 x4) (cons (list i x0 x1 x2 x3 x4) acc))
                                  '() arr0 arr1 arr2 arr3 arr4)
               (reverse (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x0 x1 x2 x3 x4) (fx+ i acc x0 x1 x2 x3 x4))
                                  0 arr0 arr1 arr2 arr3 arr4)
               (apply fx+ (map fx+ ls0 ls0 ls0 ls0 ls0 ls0))))

     )


(mat array-fold-right

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right (lambda (x acc) (cons x acc))
                                 '() arr0)
               ls0))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right (lambda (x acc) (fx+ acc x))
                                 0 arr0)
               (apply fx+ ls0)))

     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-right (lambda (x y acc) (cons (list x y) acc))
                                 '() arr0 arr1)
               (zip ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right (lambda (x y acc) (cons (list x y) acc))
                                 '() arr0 arr0)
               (zip ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-right (lambda (x y acc) (fx+ acc x y))
                                 0 arr0 arr1)
               (apply fx+ (map fx+ ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right (lambda (x y acc) (fx+ acc x y))
                                 0 arr0 arr0)
               (apply fx+ (map fx+ ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-fold-right (lambda (x0 x1 x2 x3 x4 acc) (cons (list x0 x1 x2 x3 x4) acc))
                                 '() arr0 arr1 arr2 arr3 arr4)
               (zip ls0 ls0 ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-fold-right (lambda (x0 x1 x2 x3 x4 acc) (fx+ acc x0 x1 x2 x3 x4))
                                 0 arr0 arr1 arr2 arr3 arr4)
               (apply fx+ (map fx+ ls0 ls0 ls0 ls0 ls0))))

     )


(mat array-fold-right/i

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x acc) (cons (list i x) acc))
                                   '() arr0)
               (zip ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x acc) (fx+ i acc x))
                                   0 arr0)
               (apply fx+ (map fx+ ls0 ls0))))

     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x y acc) (cons (list i x y) acc))
                                   '() arr0 arr1)
               (zip ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x y acc) (cons (list i x y) acc))
                                   '() arr0 arr0)
               (zip ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x y acc) (fx+ i acc x y))
                                   0 arr0 arr1)
               (apply fx+ (map fx+ ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x y acc) (fx+ i acc x y))
                                   0 arr0 arr0)
               (apply fx+ (map fx+ ls0 ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x0 x1 x2 x3 x4 acc) (cons (list i x0 x1 x2 x3 x4) acc))
                                   '() arr0 arr1 arr2 arr3 arr4)
               (zip ls0 ls0 ls0 ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x0 x1 x2 x3 x4 acc) (fx+ i acc x0 x1 x2 x3 x4))
                                   0 arr0 arr1 arr2 arr3 arr4)
               (apply fx+ (map fx+ ls0 ls0 ls0 ls0 ls0 ls0))))

     )
