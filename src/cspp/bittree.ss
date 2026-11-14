(library (cspp bittree)
  (export bittree make-bittree bittree? bittree-size bittree-empty?
          bittree-set! bittree-unset! bittree-flip!
          bittree-set? bittree-clear! bittree-copy
          bittree-merge bittree-or bittree-and bittree-xor

          bittree-andmap bittree-ormap
          bittree-for-each bittree-fold-left bittree-fold-right

          bittree->list)
  (import (cspp chez)
          (cspp utils)
          (cspp private rbtree)
          (only (cspp iter) iter-register-source! make-iter iter-end)
          (only (cspp navigator) nav-register-set!))

  ;; Bittree: huge sparse bit set.
  ;; Use rbtree to represent the bittree.
  ;; Key is index mod 60 (index->key) in the node, value is a fixnum containing bits.
  ;; After unsetting, if the value becomes 0, delete the node.

  ;; TODO ranged ops


  #|record:$bittree
  Sparse mutable bit-set record backed by an ordered tree.
  |#
  (define-record-type ($bittree mk-bittree bittree?)
    (fields (immutable rbt bittree-rbt)))

  (define all-naturals?
    (lambda (args) (andmap natural? args)))
  (define all-bittrees?
    (lambda (args) (andmap bittree? args)))

  (define *num-bits* (integer-length (most-positive-fixnum)))

  (define index->key
    (lambda (x) (quotient x *num-bits*)))
  (define index->offset
    (lambda (x) (modulo x *num-bits*)))
  (define key->index
    (lambda (x) (* x *num-bits*)))


  #|proc:make-bittree
  Construct a bittree object.
  |#
  (define-who make-bittree
    (lambda ()
      (mk-bittree (make-rbtree who = <))))


  ;; args must be a list of naturals
  #|proc:bittree
  Return a sparse bit set containing the natural-number indexes in `args`.
  |#
  (define-who bittree
    (lambda args
      (pcheck ([all-naturals? args])
              (let ([bt (mk-bittree (make-rbtree who = <))])
                (for-each (lambda (x) (bittree-set! bt x)) args)
                bt))))


  #|proc:bittree-set!
  Set the i'th bit in the bittree `bt`, regardless of whether
  the i'th bit is already set or not.
  |#
  (define-who bittree-set!
    (lambda (bt i)
      (pcheck ([bittree? bt] [natural? i])
              (let* ([rbt (bittree-rbt bt)]
                     [k (index->key i)]
                     [off (index->offset i)]
                     [v (rbtree-ref who rbt k #f)])
                (if v
                    (unless (logbit? off v)
                      (rbtree-set! who rbt (rbtree-fixnum? rbt) k (fxlogbit1 off v)))
                    (rbtree-set! who rbt (rbtree-fixnum? rbt) k (fxlogbit1 off 0)))))))


  #|proc:bittree-unset!
  Unset the i'th bit in the bittree `bt`, regardless of whether
  the i'th bit is already set or not.
  |#
  (define-who bittree-unset!
    (lambda (bt i)
      (pcheck ([bittree? bt] [natural? i])
              (let* ([rbt (bittree-rbt bt)]
                     [k (index->key i)]
                     [off (index->offset i)]
                     [v (rbtree-ref who rbt k #f)])
                (when (and v (fxlogbit? off v))
                  (let ([v (fxlogbit0 off v)])
                    (if (fx= v 0)
                        (rbtree-delete! who rbt (rbtree-fixnum? rbt) k)
                        (rbtree-set! who rbt (rbtree-fixnum? rbt) k v))))))))


  #|proc:bittree-flip!
  Flip the i'th bit in the bittree `bt`.
  |#
  (define-who bittree-flip!
    (lambda (bt i)
      (pcheck ([bittree? bt] [natural? i])
              (let* ([rbt (bittree-rbt bt)]
                     [k (index->key i)]
                     [off (index->offset i)]
                     [v (rbtree-ref who rbt k #f)])
                (if v
                    (if (fxlogbit? off v)
                        (let ([v (fxlogbit0 off v)])
                          (if (fx= v 0)
                              (rbtree-delete! who rbt (rbtree-fixnum? rbt) k)
                              (rbtree-set! who rbt (rbtree-fixnum? rbt) k v)))
                        (rbtree-set! who rbt (rbtree-fixnum? rbt) k (fxlogbit1 off v)))
                    (rbtree-set! who rbt (rbtree-fixnum? rbt) k (fxlogbit1 off 0)))))))


  #|proc:bittree-set?
  Check whether the bittree `bt` has the i'th bit set.
  |#
  (define-who bittree-set?
    (lambda (bt i)
      (pcheck ([bittree? bt] [natural? i])
              (let* ([rbt (bittree-rbt bt)]
                     [k (index->key i)]
                     [off (index->offset i)]
                     [v (rbtree-ref who rbt k #f)])
                (and v (fxlogbit? off v))))))


  #|proc:bittree-clear!
  Unset all bits in the bittree `bt`.
  |#
  (define-who bittree-clear!
    (lambda (bt)
      (pcheck ([bittree? bt])
              (rbtree-clear! who (bittree-rbt bt)))))


  #|proc:bittree-empty?
  Check whether the bittree `bt` is empty, i.e., has no bits set.
  |#
  (define-who bittree-empty?
    (lambda (bt)
      (pcheck ([bittree? bt])
              (let ([rbt (bittree-rbt bt)])
                (fx= 0 (rbtree-size rbt))))))


  #|proc:bittree-size
  Return the number of set bits in the bittree `bt`.
  |#
  (define-who bittree-size
    (lambda (bt)
      (pcheck ([bittree? bt])
              (rbtree-fold-left who
                                (lambda (acc k v) (+ acc (fxpopcount v)))
                                0 (bittree-rbt bt)))))


  #|proc:bittree-copy
  Make a copy of the bittree `bt`.
  |#
  (define-who bittree-copy
    (lambda (bt)
      (pcheck ([bittree? bt])
              (let* ([rbt (bittree-rbt bt)]
                     [newbt (make-bittree)]
                     [newrbt (bittree-rbt newbt)])
                (rbtree-for-each who
                                 (lambda (k v)
                                   (rbtree-set! who newrbt (rbtree-fixnum? newrbt) k v))
                                 rbt)
                newbt))))


  #|proc:bittree-merge
  Merge bittrees, i.e., union all set bits together, and return a new bittree.
  If only one bittree is given, it is returned directly.
  |#
  (define-who bittree-merge
    (case-lambda
      [(bt0)
       (pcheck ([bittree? bt0]) bt0)]
      [(bt0 . bt*)
       (pcheck ([bittree? bt0] [all-bittrees? bt*])
               (let* ([rbt* (map bittree-rbt (cons bt0 bt*))]
                      [newbt (make-bittree)]
                      [newrbt (bittree-rbt newbt)])
                 (for-each
                  (lambda (rbt)
                    (rbtree-for-each
                     who
                     (lambda (k v)
                       (rbtree-set! who newrbt (rbtree-fixnum? newrbt) k
                                    (fxlogior v (rbtree-ref who newrbt k v))))
                     rbt))
                  rbt*)
                 newbt))]))

  #|proc:bittree-or
  Return a new bittree containing the union of `left` and `right`.
  Inputs are not mutated, and results traverse in ascending bit-index order.
  |#
  (define-who bittree-or
    (lambda (left right)
      (pcheck ([bittree? left right])
              (bittree-merge left right))))

  #|proc:bittree-and
  Return a new bittree containing bits present in both `left` and `right`.
  Inputs are not mutated, and results traverse in ascending bit-index order.
  |#
  (define-who bittree-and
    (lambda (left right)
      (pcheck ([bittree? left right])
              (let ([result (make-bittree)])
                (bittree-for-each
                 (lambda (index)
                   (when (bittree-set? right index)
                     (bittree-set! result index)))
                 left)
                result))))

  #|proc:bittree-xor
  Return a new bittree containing bits present in exactly one input.
  Inputs are not mutated, and results traverse in ascending bit-index order.
  |#
  (define-who bittree-xor
    (lambda (left right)
      (pcheck ([bittree? left right])
              (let ([result (bittree-copy left)])
                (bittree-for-each
                 (lambda (index) (bittree-flip! result index))
                 right)
                result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; count the number of leading zeros in non-negative fixnum `n`
  ;; Note that the starting position is 59 (leftmost bit of (most-positive-fixnum)).
  (define fxclz+
    (lambda (n)
      ;; TODO optimize
      (let loop ([i 0] [n n])
        (if (fx= n 0)
            (fx- *num-bits* i)
            (loop (fx1+ i) (fxsrl n 1))))))


  ;; iterate indices of set bits inside a fixnum
  (define bits-for-each
    (lambda (proc n)
      (let loop ([i 0] [n n])
        (unless (fx= n 0)
          (when (fxlogbit? 0 n)
            (proc i))
          (loop (fx1+ i) (fxsrl n 1))))))
  (define bits-andmap
    (lambda (proc n)
      (let loop ([i 0] [n n])
        (if (fx= n 0)
            #t
            (if (fxlogbit? 0 n)
                (and (proc i)
                     (loop (fx1+ i) (fxsrl n 1)))
                (loop (fx1+ i) (fxsrl n 1)))))))
  (define bits-ormap
    (lambda (proc n)
      (let loop ([i 0] [n n])
        (if (fx= n 0)
            #f
            (if (fxlogbit? 0 n)
                (or (proc i)
                    (loop (fx1+ i) (fxsrl n 1)))
                (loop (fx1+ i) (fxsrl n 1)))))))
  (define bits-fold-left
    (lambda (proc acc n)
      (let loop ([acc acc] [i 0] [n n])
        (if (fx= n 0)
            acc
            (if (fxlogbit? 0 n)
                (loop (proc acc i) (fx1+ i) (fxsrl n 1))
                (loop acc (fx1+ i) (fxsrl n 1)))))))
  (define bits-fold-right
    (lambda (proc acc n)
      (let loop ([acc acc] [i (fx- *num-bits* (fxclz+ n) 1)])
        (if (fx< i 0)
            acc
            (if (fxlogbit? i n)
                (loop (proc i acc) (fx1- i))
                (loop acc (fx1- i)))))))


  #|proc:bittree-andmap
  Apply the unary procedure `proc` to the indices of set bits in bittree `bt`.
  The result is #t if `proc` returns #t for all indices;
  if `proc` returns #f on any set of indices,
  `bittree-andmap` returns #f after the first such application of `proc`.
  |#
  (define-who bittree-andmap
    (lambda (proc bt)
      (pcheck ([procedure? proc] [bittree? bt])
              (let ([rbt (bittree-rbt bt)])
                (rbtree-andmap who
                               (lambda (k v)
                                 (let ([k (key->index k)])
                                   (bits-andmap (lambda (i)
                                                  (proc (+ i k)))
                                                v)))
                               rbt)))))


  #|proc:bittree-ormap
  Apply the unary procedure `proc` to the indices of set bits in bittree `bt`.
  The result is #t if `proc` returns #t for at least one index;
  if `proc` returns #f on all indices, `bittree-ormap` returns #f.
  |#
  (define-who bittree-ormap
    (lambda (proc bt)
      (pcheck ([procedure? proc] [bittree? bt])
              (let ([rbt (bittree-rbt bt)])
                (rbtree-ormap who
                              (lambda (k v)
                                (let ([k (key->index k)])
                                  (bits-ormap (lambda (i)
                                                (proc (+ i k)))
                                              v)))
                              rbt)))))



  #|proc:bittree-for-each
  Apply the unary procedure `proc` to the indices of set bits in
  bittree `bt` for side effects, sequentially in ascending order.
  |#
  (define-who bittree-for-each
    (lambda (proc bt)
      (pcheck ([procedure? proc] [bittree? bt])
              (let ([rbt (bittree-rbt bt)])
                (rbtree-for-each who
                                 (lambda (k v)
                                   (let ([k (key->index k)])
                                     (bits-for-each (lambda (i)
                                                      (proc (+ i k)))
                                                    v)))
                                 rbt)))))


  #|proc:bittree-fold-left
  Fold set bit indexes in ascending order with `proc`, returning the accumulator.
  |#
  (define-who bittree-fold-left
    (lambda (proc acc bt)
      (pcheck ([procedure? proc] [bittree? bt])
              (let ([rbt (bittree-rbt bt)])
                (rbtree-fold-left who
                                  (lambda (acc k v)
                                    (let ([k (key->index k)])
                                      (bits-fold-left (lambda (acc i)
                                                        (proc acc (+ i k)))
                                                      acc v)))
                                  acc rbt)))))


  #|proc:bittree-fold-right
  Fold set bit indexes in descending order with `proc`, returning the accumulator.
  |#
  (define-who bittree-fold-right
    (lambda (proc acc bt)
      (pcheck ([procedure? proc] [bittree? bt])
              (let ([rbt (bittree-rbt bt)])
                (rbtree-fold-right who
                                   (lambda (k v acc)
                                     (let ([k (key->index k)])
                                       (bits-fold-right (lambda (i acc)
                                                          (proc (+ i k) acc))
                                                        acc v)))
                                   acc rbt)))))


  #|proc:bittree->list
  Convert the bittree `bt` to a list, that is,
  put all indices of set bits into a list, in ascending order.
  |#
  (define-who bittree->list
    (lambda (bt)
      (pcheck ([bittree? bt])
              (rbtree-fold-right who
                                 (lambda (k v acc)
                                   (let ([k (key->index k)])
                                     (bits-fold-right (lambda (i acc)
                                                        (cons (+ i k) acc))
                                                      acc v)))
                                 '() (bittree-rbt bt)))))


;;;;===----------------------------------------------------------------------===
;;;; Iterator extension registration
;;;;===----------------------------------------------------------------------===

  (iter-register-source!
   bittree?
   (lambda (bt)
     (let ([cursor (rbtree-inorder-cursor (bittree-rbt bt))]
           [base 0]
           [bits 0]
           [offset *num-bits*])
       (make-iter
        (lambda ()
          (let loop ()
            (cond [(fx< offset *num-bits*)
                   (let ([current offset])
                     (set! offset (fx1+ offset))
                     (if (fxlogbit? current bits)
                         (fx+ base current)
                         (loop)))]
                  [else
                   (call-with-values cursor
                     (lambda (key value)
                       (if (eq? key *dummy-v*)
                           iter-end
                           (begin
                             (set! base (key->index key))
                             (set! bits value)
                             (set! offset 0)
                             (loop)))))])))
        (lambda ()
          (set! cursor (rbtree-inorder-cursor (bittree-rbt bt)))
          (set! offset *num-bits*))))))

;;;;===----------------------------------------------------------------------===
;;;; Navigator extension registration
;;;;===----------------------------------------------------------------------===

  (nav-register-set!
   bittree? bittree->list
   (lambda (bt members)
     (let ([copy (bittree-copy bt)])
       (bittree-clear! copy)
       (for-each (lambda (member) (bittree-set! copy member)) members)
       copy))
   (lambda (bt members)
     (bittree-clear! bt)
     (for-each (lambda (member) (bittree-set! bt member)) members)
     bt)
   (lambda (bt member)
     (let ([copy (bittree-copy bt)])
       (bittree-unset! copy member)
       copy))
   (lambda (bt member)
     (bittree-unset! bt member)
     bt))

  (record-writer
   (type-descriptor $bittree)
   (lambda (r p wr)
     (display "#[bittree " p)
     (wr (bittree->list r) p)
     (display "]" p)))


  (record-type-equal-procedure
   (type-descriptor $bittree)
   (lambda (bt1 bt2 =?)
     (let ([rbt1 (bittree-rbt bt1)] [rbt2 (bittree-rbt bt2)])
       (=? rbt1 rbt2))))



  )
