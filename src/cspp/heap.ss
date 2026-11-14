(library (cspp heap)
  (export heap make-heap make-bounded-heap heap? heap-size
          heap-push! heap-pop! heap-pop-all! heap-peek
          heap-contains? heap-contains/p?
          heap-clear! heap-empty? heap-bounded?
          heap-copy heap->list)
  (import (cspp chez)
          (only (cspp utils) bool define-who natural? pcheck)
          (cspp list)
          (cspp vector)
          (only (cspp iter) iter-register-source! make-iter iter-end))


  #|record:$heap
  Mutable priority-heap record with an optional capacity bound.
  |#
  (define-record-type ($heap mk-heap heap?)
    (fields (immutable <? heap-<?)
            (immutable bound heap-bound)
            (mutable data heap-data heap-data-set!)
            (mutable size heap-size heap-size-set!)))

  ;; get indices of parent, left child, and right child
  (define P (lambda (i) (fxsrl (fx1- i)  1)))
  (define L (lambda (i) (fx+ (fxsll i 1) 1)))
  (define R (lambda (i) (fx+ (fxsll i 1) 2)))

  (define *default-cap* 32)

  #;
  (define (heap-dump hp)
    (println (heap-data hp)))


  #|proc:make-heap
  `<?` must be a procedure that can be used to compare items;
  `cap` must be a natural number specifying initial approximate capacity of the heap,
  and can be used to reduce heap resize overhead.

  `make-heap` constructs a unbounded heap.
  |#
  (define-who make-heap
    (case-lambda
      [(<?) (make-heap <? *default-cap*)]
      [(<? cap)
       (pcheck ([procedure? <?] [natural? cap])
               (mk-heap <? #f (make-vector (max *default-cap* cap)) 0))]))


  #|proc:make-bounded-heap
  `<?` must be a procedure that can be used to compare items;
  `bound` must be a natural number specifying the max number of items
  the heap can store.

  `make-bounded-heap` constructs a bounded heap.
  When adding/pushing an item `v` into the bounded heap, if (<? top v) is #t,
  where `top` is the heap's top item, and the heap size has reached `bound`,
  the top item will be popped off and `v` will pushed into the heap.
  Bounded heap is useful when one wants to filter out the N largest or
  smallest items in a huge set of data.
  |#
  (define-who make-bounded-heap
    (lambda (<? bound)
      (pcheck ([procedure? <?] [natural? bound])
              (mk-heap <? bound (make-vector (fx1+ bound)) 0))))


  #|proc:heap
  `<?` must be a procedure that can be used to compare items.

  `heap` constructs a unbounded heap using the comparison procedure `<?`
  and the list of items in `args` by succesively pushing them into the heap.
  |#
  (define-who heap
    (lambda (<? . args)
      (pcheck ([procedure? <?])
              (let* ([len (length args)]
                     [s (max len *default-cap*)]
                     [data (make-vector s 0)]
                     [hp (mk-heap <? #f data len)])
                (for-each/i (lambda (i x)
                              (vector-set! data i x))
                            args)
                ;; heapify from the last non-leaf node backwards
                (when (fx> len 0)
                  (let loop ([i (fx1- (fxsrl len 1))])
                    (unless (fx= i -1)
                      (bubble-down! data <? i len)
                      (loop (fx1- i)))))
                hp))))


  (define swap!
    (lambda (vec i j)
      (let ([t (vector-ref vec i)])
        (vector-set! vec i (vector-ref vec j))
        (vector-set! vec j t))))

  (define grow-data!
    (case-lambda
      [(hp)
       (grow-data! hp (fx* (heap-size hp) 2))]
      [(hp newsize)
       (let* ([data (heap-data hp)] [newdata (make-vector newsize 0)])
         (vcopy! data 0 newdata 0 (heap-size hp))
         (heap-data-set! hp newdata))]))

  ;; maintain the heap property from `i` downwards
  (define bubble-down!
    (lambda (vec <? i size)
      (let loop ([i i])
        (let* ([Li (L i)] [Ri (R i)]
               [Si (let ([Si (if (and (fx< Li size)
                                      (<? (vector-ref vec Li) (vector-ref vec i)))
                                 Li
                                 i)])
                     (if (and (fx< Ri size)
                              (<? (vector-ref vec Ri) (vector-ref vec Si)))
                         Ri
                         Si))])
          (unless (fx= Si i)
            (swap! vec i Si)
            (loop Si))))))

  ;; maintain the heap property from `i` upwards
  (define bubble-up!
    (lambda (vec <? i)
      (let loop ([i i])
        (unless (fx= i 0)
          (let* ([x (vector-ref vec i)] [pi (P i)]
                 [p (vector-ref vec pi)])
            (when (not (<? p x))
              (vector-set! vec i  p)
              (vector-set! vec pi x)
              (loop pi)))))))


  #|proc:heap-push!
  Push one or more items into the heap `hp`.
  |#
  (define-who heap-push!
    (case-lambda
      [(hp v)
       (pcheck ([heap? hp])
               (let ([bd (heap-bound hp)] [<? (heap-<? hp)])
                 (if bd
                     (let ([data (heap-data hp)] [size (heap-size hp)])
                       (if (fx< size bd)
                           ;; just push v
                           (begin (vector-set! data size v)
                                  (bubble-up! data <? size)
                                  (heap-size-set! hp (fx1+ size)))
                           (when (and (fx> size 0) (<? (vector-ref data 0) v))
                             ;; pop and push v, size not changed
                             (vector-set! data 0 (vector-ref data (fx1- size)))
                             (bubble-down! data <? 0 (fx1- size))
                             (vector-set! data (fx1- size) v)
                             (bubble-up! data <? (fx1- size)))))
                     (begin
                       (when (fx= (heap-size hp) (vector-length (heap-data hp)))
                         (grow-data! hp))
                       (let ([data (heap-data hp)] [size (heap-size hp)])
                         (vector-set! data size v)
                         (bubble-up! data <? size)
                         (heap-size-set! hp (fx1+ size)))))))]
      [(hp . v*)
       (pcheck ([heap? hp])
               (let ([bd (heap-bound hp)] [<? (heap-<? hp)] [len (length v*)])
                 (if bd
                     (for-each (lambda (v) ;; TODO optimize this
                                 (let ([data (heap-data hp)] [size (heap-size hp)])
                                   (if (fx< size bd)
                                       ;; just push v
                                       (begin (vector-set! data size v)
                                              (bubble-up! data <? size)
                                              (heap-size-set! hp (fx1+ size)))
                                       (when (and (fx> size 0) (<? (vector-ref data 0) v))
                                         ;; pop and push v, size not changed
                                         (vector-set! data 0 (vector-ref data (fx1- size)))
                                         (bubble-down! data <? 0 (fx1- size))
                                         (vector-set! data (fx1- size) v)
                                         (bubble-up! data <? (fx1- size))))))
                               v*)
                     (begin (when (fx>= (fx+ (heap-size hp) len) (vector-length (heap-data hp)))
                              (grow-data! hp (fx+ (vector-length (heap-data hp)) len)))
                            (let ([data (heap-data hp)] [size (heap-size hp)])
                              (for-each/i (lambda (i v)
                                            (vector-set! data (fx+ i size) v)
                                            (bubble-up! data <? (fx+ i size)))
                                          v*)
                              (heap-size-set! hp (fx+ size len)))))))]))


  ;; size must > 0
  (define pop!
    (lambda (hp)
      (let* ([data (heap-data hp)] [size (heap-size hp)]
             [<? (heap-<? hp)])
        (let ([v (vector-ref data 0)])
          (vector-set! data 0 (vector-ref data (fx1- size)))
          (bubble-down! data <? 0 (fx1- size))
          (heap-size-set! hp (fx1- size))
          v))))


  #|proc:heap-pop!
  Pop the top item from the heap `hp`.
  It is an error if the heap is empty.
  |#
  (define-who heap-pop!
    (lambda (hp)
      (pcheck ([heap? hp])
              (if (fx= (heap-size hp) 0)
                  (errorf who "heap is empty")
                  (pop! hp)))))


  #|proc:heap-pop-all!
  Pop all items from the heap `hp` into a list.
  The first item in the list corresponds to the top item in the heap.
  If an additional procedure `proc` is given, it is apply to
  each item popped from the heap for effect.
  |#
  (define-who heap-pop-all!
    (case-lambda
      [(hp)
       (pcheck ([heap? hp])
               (let ([size (heap-size hp)])
                 (if (fx= size 0)
                     '()
                     (let ([lb (make-list-builder)])
                       (let loop ([size size])
                         (if (fx= 0 size)
                             (lb)
                             (begin (lb (pop! hp))
                                    (loop (fx1- size)))))))))]
      [(hp proc)
       (pcheck ([heap? hp] [procedure? proc])
               (let ([size (heap-size hp)])
                 (let loop ([size size])
                   (when (fx> size 0)
                     (proc (pop! hp))
                     (loop (fx1- size))))))]))


  #|proc:heap-peek
  Get the top item in the heap without removing it.
  It is an error if the heap is empty.
  |#
  (define-who heap-peek
    (lambda (hp)
      (pcheck ([heap? hp])
              (let* ([data (heap-data hp)] [size (heap-size hp)])
                (if (fx= size 0)
                    (errorf who "heap is empty")
                    (vector-ref data 0))))))


  #|proc:heap-clear!
  Remove all items in the heap.
  |#
  (define-who heap-clear!
    (lambda (hp)
      (pcheck ([heap? hp])
              (let* ([data (heap-data hp)] [size (heap-size hp)])
                (unless (fx= size 0)
                  (vector-fill! data 0)
                  (heap-size-set! hp 0))))))


  #|proc:heap-empty?
  Check whether the heap is empty.
  |#
  (define-who heap-empty?
    (lambda (hp)
      (pcheck ([heap? hp])
              (fx= (heap-size hp) 0))))


  #|proc:heap-bounded?
  Check whether the heap `hp` is bounded.
  |#
  (define-who heap-bounded?
    (lambda (hp)
      (pcheck ([heap? hp])
              (bool (heap-bound hp)))))


  #|proc:heap-contains?
  Return whether the heap `hp` contains the given item `v`.
  If it does, the procedure returns #t; otheriwse it returns #f.
  Items are compared using `equal?`.
  |#
  (define-who heap-contains?
    (lambda (hp v)
      (pcheck ([heap? hp])
              (let* ([data (heap-data hp)] [size (heap-size hp)])
                (let loop ([i 0])
                  (if (fx= i size)
                      #f
                      (if (equal? v (vector-ref data i))
                          #t
                          (loop (fx1+ i)))))))))


  #|proc:heap-contains/p?
  Return whether the heap `hp` contains an item that satisfies `pred`.
  |#
  (define-who heap-contains/p?
    (lambda (hp pred)
      (pcheck ([heap? hp] [procedure? pred])
              (let* ([data (heap-data hp)] [size (heap-size hp)])
                (let loop ([i 0])
                  (if (fx= i size)
                      #f
                      (if (pred (vector-ref data i))
                          #t
                          (loop (fx1+ i)))))))))


  #|proc:heap-copy
  Make a copy of the heap `hp`.
  |#
  (define-who heap-copy
    (lambda (hp)
      (pcheck ([heap? hp])
              (mk-heap (heap-<? hp)
                       (heap-bound hp)
                       (vector-copy (heap-data hp))
                       (heap-size hp)))))


  #|proc:heap->list
  Convert the heap into a list, without popping the heap's items.
  |#
  (define-who heap->list
    (lambda (hp)
      (pcheck ([heap? hp])
              (heap-pop-all! (heap-copy hp)))))


;;;;===----------------------------------------------------------------------===
;;;; Iterator extension registration
;;;;===----------------------------------------------------------------------===

  (iter-register-source!
   heap?
   (lambda (hp)
     (let ([frontier
            (make-heap
             (lambda (left-index right-index)
               ((heap-<? hp)
                (vector-ref (heap-data hp) left-index)
                (vector-ref (heap-data hp) right-index))))]
           [size 0])
       (define reset-frontier!
         (lambda ()
           (set! size (heap-size hp))
           (heap-clear! frontier)
           (unless (fx= size 0) (heap-push! frontier 0))))
       (reset-frontier!)
       (make-iter
        (lambda ()
          (if (heap-empty? frontier)
              iter-end
              (let* ([index (heap-pop! frontier)]
                     [left (L index)]
                     [right (R index)])
                (when (fx< left size)
                  (heap-push! frontier left))
                (when (fx< right size)
                  (heap-push! frontier right))
                (vector-ref (heap-data hp) index))))
        reset-frontier!))))

  (record-writer
   (type-descriptor $heap)
   (lambda (r p wr)
     (if (heap-bound r)
         (begin (display "#[bounded-heap " p)
                (display (format "(~a) " (heap-bound r))))
         (display "#[heap " p))
     (display (heap-size r) p)
     (display "]" p)))


  ;; TODO how?
  #;
  (record-type-equal-procedure
   (type-descriptor $heap)
   (lambda (hp1 hp2 =?)
     (and (= (heap-size hp1) (heap-size hp2))
          (todo))))

  )
