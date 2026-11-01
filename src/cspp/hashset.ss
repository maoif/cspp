(library (cspp hashset)
  (export hashset hashset? hashset-empty? hashset-clear!
          make-hashset make-eq-hashset make-eqv-hashset make-symbol-hashset
          hashset-add! hashset-delete! hashset-size hashset-contains? hashset-contains/p?
          hashset-filter hashset-filter! hashset-partition
          hashset-search hashset-search*

          hashset-map hashset-for-each

          hashset+ hashset- hashset& hashset^

          hashset->list hashset->vector
          list->eq-hashset vector->eq-hashset
          list->eqv-hashset vector->eqv-hashset)
  (import (cspp chez)
          (cspp internal)
          (cspp utils)
          (cspp list)
          (only (cspp iter) iter-register-source! make-iter iter-end)
          (only (cspp navigator) nav-register-set!))


  ;; dummy value for all keys
  (define V #f)

  #|record:$hashset
  Mutable hash-set record preserving its equality and hashing strategy.
  |#
  (define-record-type ($hashset mk-hashset hashset?)
    (nongenerative) (opaque #t)
    (fields (immutable ht hashset-ht)
            ;; 'eq, 'eqv, 'others
            (immutable type hashset-type)))


  #|proc:make-hashset
  Return a newly allocated mutable hashset using `hash` as the hash function
  and `=?` as the equivalence function used to compare values.
  If a third argument is given, the initial capacity of the hashset is
  set to approximately `k` elements.

  `hash` and `=?` must be procedures.
  `hash` should accept a single value as an argument and
  should return a non-negative exact integer object (the hash).
  `=?` should accept two values as arguments and return a single value.
  Neither procedure should mutate the hashset returned by this procedure.

  If neither `hash` nor `=?` is given, an eqv-hashset is returned.
  |#
  (define-who make-hashset
    (case-lambda
      [()          (mk-hashset (make-eqv-hashtable)       'eqv)]
      [(hash =?)   (mk-hashset (make-hashtable hash =?)   'others)]
      [(hash =? k) (mk-hashset (make-hashtable hash =? k) 'others)]))


  #|proc:make-eq-hashset
  Make a mutable eq-hashset that accepts arbirary objects as values,
  and compares those values using `eq?`.

  If an argument is given, the initial capacity of the
  eq-hashset is set to approximately `k` elements.
  |#
  (define make-eq-hashset
    (case-lambda
      [()  (mk-hashset (make-eq-hashtable)   'eq)]
      [(k) (mk-hashset (make-eq-hashtable k) 'eq)]))


  #|proc:make-eqv-hashset
  Make a mutable eqv-hashset that accepts arbirary objects as values,
  and compares those values using `eqv?`.

  If an argument is given, the initial capacity of the
  eqv-hashset is set to approximately `k` elements.
  |#
  (define make-eqv-hashset
    (case-lambda
      [()  (mk-hashset (make-eqv-hashtable)   'eqv)]
      [(k) (mk-hashset (make-eqv-hashtable k) 'eqv)]))


  #|proc:make-symbol-hashset
  Make a mutable symbol-hashset that accepts symbols as values.

  If an argument is given, the initial capacity of the
  symbol-hashset is set to approximately `k` elements.
  |#
  (define make-symbol-hashset
    (case-lambda
      [()  (mk-hashset (make-hashtable symbol-hash symbol=?)   'others)]
      [(k) (mk-hashset (make-hashtable symbol-hash symbol=? k) 'others)]))


  #|proc:hashset
  Return a mutable eqv-hashset containing all `args`.
  |#
  (define-who hashset
    (lambda args
      (let ([hs (make-eqv-hashset (length args))])
        (for-each (lambda (x) (hashset-add! hs x)) args)
        hs)))


  #|proc:hashset-empty?
  Return whether the hashset is empty.
  |#
  (define-who hashset-empty?
    (lambda (hs)
      (pcheck ([hashset? hs])
              (= 0 (hashtable-size (hashset-ht hs))))))


  #|proc:hashset-add!
  Add a new item `v` to the hashset `hs` if `v` does not exist yet.
  |#
  (define-who hashset-add!
    (lambda (hs v)
      (pcheck ([hashset? hs])
              (hashtable-set! (hashset-ht hs) v V))))

  #|proc:hashset-delete!
  Remove the value `v` from the hashset `hs`.
  If `v` is absent, the hashset is unchanged.
  |#
  (define-who hashset-delete!
    (lambda (hs v)
      (pcheck ([hashset? hs])
              (hashtable-delete! (hashset-ht hs) v))))


  #|proc:hashset-clear!
  Remove all items from the hashset `hs`.
  |#
  (define-who hashset-clear!
    (lambda (hs)
      (pcheck ([hashset? hs])
              (hashtable-clear! (hashset-ht hs)))))


  #|proc:hashset-size
  Return the number of items in the hashset.
  |#
  (define-who hashset-size
    (lambda (hs)
      (pcheck ([hashset? hs])
              (hashtable-size (hashset-ht hs)))))

  (define $newhs
    (lambda (hs)
      (let* ([ht (hashset-ht hs)] [k (hashtable-size ht)]
             [newht (case (hashset-type hs)
                      [eq  (make-eq-hashtable  k)]
                      [eqv (make-eqv-hashtable k)]
                      [others (make-hashtable
                               (hashtable-hash-function ht)
                               (hashtable-equivalence-function ht)
                               k)]
                      [else (assert-unreachable)])])
        (mk-hashset newht (hashset-type hs)))))


  #|proc:hashset-filter
  Return a new hashset `h` such that for each item `x` in `h`,
  (pred x) returns #t.
  |#
  (define-who hashset-filter
    (lambda (pred hs)
      (pcheck ([hashset? hs] [procedure? pred])
              (let* ([ht (hashset-ht hs)] [k (hashtable-size ht)]
                     [newhs ($newhs hs)] [newht (hashset-ht newhs)]
                     [v* (hashtable-keys ht)])
                (vector-for-each (lambda (v) (when (pred v) (hashtable-set! newht v V))) v*)
                newhs))))


  #|proc:hashset-filter!
  Remove all items in `hs` that do not satisfy the predicate `pred`.
  |#
  (define-who hashset-filter!
    (lambda (pred hs)
      (pcheck ([hashset? hs] [procedure? pred])
              (let* ([ht (hashset-ht hs)] [v* (hashtable-keys ht)])
                (vector-for-each (lambda (v) (unless (pred v) (hashtable-delete! ht v))) v*)
                hs))))


  #|proc:hashset-partition
  Apply `pred` to every item in hashset `hs` and return two values,
  the first one a hashset of the items of `hs` for which `(pred v)` returns #t,
  the second one a hashset of the items of `hs` for which `(pred v)` returns #f.
  |#
  (define-who hashset-partition
    (lambda (pred hs)
      (pcheck ([hashset? hs] [procedure? pred])
              (let* ([ht (hashset-ht hs)] [k (hashtable-size ht)]
                     [T ($newhs hs)] [ht-T (hashset-ht T)]
                     [F ($newhs hs)] [ht-F (hashset-ht F)]
                     [v* (hashtable-keys ht)])
                (vector-for-each (lambda (v) (if (pred v)
                                                 (hashtable-set! ht-T v V)
                                                 (hashtable-set! ht-F v V)))
                                 v*)
                (values T F)))))


  #|proc:hashset-contains?
  Return whether the hashset `hs` already contains the value `v`.
  |#
  (define-who hashset-contains?
    (lambda (hs v)
      (pcheck ([hashset? hs])
              (hashtable-contains? (hashset-ht hs) v))))


  #|proc:hashset-contains/p?
  Return whether the hashset `hs` contains the item `v`
  such that `(pred v)` returns #t.
  |#
  (define-who hashset-contains/p?
    (lambda (hs pred)
      (pcheck ([hashset? hs] [procedure? pred])
              (let* ([ht (hashset-ht hs)] [v* (hashtable-keys ht)])
                (let loop ([i 0])
                  (if (fx= i (vector-length v*))
                      #f
                      (if (pred (vector-ref v* i))
                          #t
                          (loop (fx1+ i)))))))))


  #|proc:hashset-search
  Return the 1st item in the hashset `hs` that satisfies the predicate `pred`.
  If no such item exists, #f is returned.
  |#
  (define-who hashset-search
    (lambda (hs pred)
      (pcheck ([hashset? hs] [procedure? pred])
              (let* ([ht (hashset-ht hs)] [v* (hashtable-keys ht)])
                (let loop ([i 0])
                  (if (fx= i (vector-length v*))
                      #f
                      (if (pred (vector-ref v* i))
                          (vector-ref v* i)
                          (loop (fx1+ i)))))))))


  #|proc:hashset-search*
  Return the the list of items in the hashset `hs` that satify the predicate `pred`.

  By default the items satisfying `pred` are returned in a list.

  If `collect` is given, it is applied to every item that satisfies `pred`
  in the hashset. This is useful when collecting the desired items in custom
  data structures.
  |#
  (define-who hashset-search*
    (case-lambda
      [(hs pred)
       (pcheck ([hashset? hs] [procedure? pred])
               (let ([lb (make-list-builder)])
                 (let* ([ht (hashset-ht hs)] [v* (hashtable-keys ht)])
                   (let loop ([i 0])
                     (if (fx= i (vector-length v*))
                         (lb)
                         (begin (when (pred (vector-ref v* i))
                                  (lb (vector-ref v* i)))
                                (loop (fx1+ i))))))))]
      [(hs pred collect)
       (pcheck ([hashset? hs] [procedure? pred collect])
               (let* ([ht (hashset-ht hs)] [v* (hashtable-keys ht)])
                 (let loop ([i 0])
                   (unless (fx= i (vector-length v*))
                     (begin (when (pred (vector-ref v* i))
                              (collect (vector-ref v* i)))
                            (loop (fx1+ i)))))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   set operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; TODO containment relations


  #|proc:hashset+
  Compute the union of the hashsets, i.e., the hashset that contains all items
  in all the given hashsets.
  If only one hashset is given, it is returned immediately.

  The returned hashset has the same type (i.e., the same `hash` and `=?` procedures)
  as that of the first input hashset.
  |#
  (define-who hashset+
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (pcheck ([all-hashsets? hs*])
                          (let* ([newhs ($newhs hs)]
                                 [newht (hashset-ht newhs)])
                            (for-each (lambda (hs)
                                        (let ([ht (hashset-ht hs)])
                                          (vector-for-each (lambda (v) (hashtable-set! newht v V))
                                                           (hashtable-keys ht))))
                                      (cons hs hs*))
                            newhs))))))


  #|proc:hashset-
  Compute the difference of the hashsets, i.e., the hashset that contains those items
  that are in the first hashset, but are not in the rest of the hashsets.
  If only one hashset is given, it is returned immediately.

  The returned hashset has the same type (i.e., the same `hash` and `=?` procedures)
  as that of the first input hashset.
  |#
  (define-who hashset-
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (pcheck ([all-hashsets? hs*])
                          (let* ([newhs ($newhs hs)]
                                 [newht (hashset-ht newhs)])
                            (vector-for-each (lambda (v) (hashtable-set! newht v V))
                                             (hashtable-keys (hashset-ht hs)))
                            (for-each (lambda (hs)
                                        (let ([ht (hashset-ht hs)])
                                          (vector-for-each (lambda (v)
                                                             (when (hashtable-contains? newht v)
                                                               (hashtable-delete! newht v)))
                                                           (hashtable-keys ht))))
                                      hs*)
                            newhs))))))


  #|proc:hashset&
  Compute the intersection of the hashsets, i.e., the hashset whose items are contained
  in all given hashsets.
  If only one hashset is given, it is returned immediately.

  The returned hashset has the same type (i.e., the same `hash` and `=?` procedures)
  as that of the first input hashset.
  |#
  (define-who hashset&
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (pcheck ([all-hashsets? hs*])
                          (let* ([newhs (apply hashset+ hs hs*)]
                                 [newht (hashset-ht newhs)]
                                 [v* (hashtable-keys newht)])
                            (vector-for-each (lambda (v)
                                               (unless (andmap (lambda (hs)
                                                                 (hashtable-contains? (hashset-ht hs) v))
                                                               (cons hs hs*))
                                                 (hashtable-delete! newht v)))
                                             v*)
                            newhs))))))


  #|proc:hashset^
  Compute the symmetric difference of the hashsets, i.e., the difference of the union
  and the intersection of the hashsets.
  If only one hashset is given, it is returned immediately.

  The returned hashset has the same type (i.e., the same `hash` and `=?` procedures)
  as that of the first input hashset.
  |#
  (define-who hashset^
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (pcheck ([all-hashsets? hs*])
                          (let* ([newhs (apply hashset+ hs hs*)]
                                 [newht (hashset-ht newhs)]
                                 [v* (hashtable-keys newht)])
                            (vector-for-each (lambda (v)
                                               (when (andmap (lambda (hs)
                                                               (hashtable-contains? (hashset-ht hs) v))
                                                             (cons hs hs*))
                                                 (hashtable-delete! newht v)))
                                             v*)
                            newhs))))))


;;;; imperative versions


  #|proc:hashset+!
  Union.
  |#
  (define-who hashset+!
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (todo)))))


  #|proc:hashset-!
  Difference.
  |#
  (define-who hashset-!
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (todo)))))


  #|proc:hashset&!
  Intersection.
  |#
  (define-who hashset&!
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (todo)))))


  #|proc:hashset^!
  symmetric difference
  |#
  (define-who hashset^!
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (todo)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define all-hashsets? (lambda (x*) (andmap hashset? x*)))
  (define check-size
    (case-lambda
      [(who x0 x1)
       (unless (fx= (hashset-size x0) (hashset-size x1))
         (errorf who "hashsets are not of the same size"))]
      [(who x0 . x*)
       (unless (null? x*)
         (unless (apply fx= (hashset-size x0) (map hashset-size x*))
           (errorf who "hashsets are not of the same size")))]))


  #|proc:hashset-map
  Return a hashset containing `proc` results for corresponding input members.
  |#
  (define-who hashset-map
    (case-lambda
      [(proc hs0)
       (pcheck ([procedure? proc] [hashset? hs0])
               (let* ([k* (hashtable-keys (hashset-ht hs0))]
                      [newhs ($newhs hs0)])
                 (vector-for-each (lambda (x) (hashset-add! newhs (proc x))) k*)
                 newhs))]
      [(proc hs0 hs1)
       (pcheck ([procedure? proc] [hashset? hs0] [hashset? hs1])
               (check-size who hs0 hs1)
               (let* ([k0* (hashtable-keys (hashset-ht hs0))]
                      [k1* (hashtable-keys (hashset-ht hs1))]
                      [newhs ($newhs hs0)])
                 (vector-for-each (lambda (x0 x1) (hashset-add! newhs (proc x0 x1))) k0* k1*)
                 newhs))]
      [(proc hs0 . hs*)
       (pcheck ([procedure? proc] [hashset? hs0] [all-hashsets? hs*])
               (apply check-size who hs0 hs*)
               (let* ([k0* (hashtable-keys (hashset-ht hs0))] [len (vector-length k0*)]
                      [k** (map hashtable-keys (map hashset-ht hs*))]
                      [newhs ($newhs hs0)])
                 (let loop ([i 0])
                   (if (fx= i len)
                       newhs
                       (begin (hashset-add! newhs (apply proc (vector-ref k0* i)
                                                         (map (lambda (v) (vector-ref v i)) k**)))
                              (loop (fx1+ i)))))))]))


  #|proc:hashset-for-each
  Call `proc` on corresponding members and return the result of `(void)`.
  |#
  (define-who hashset-for-each
    (case-lambda
      [(proc hs0)
       (pcheck ([procedure? proc] [hashset? hs0])
               (let ([k* (hashtable-keys (hashset-ht hs0))])
                 (vector-for-each proc k*)))]
      [(proc hs0 hs1)
       (pcheck ([procedure? proc] [hashset? hs0] [hashset? hs1])
               (check-size who hs0 hs1)
               (let ([k0* (hashtable-keys (hashset-ht hs0))]
                     [k1* (hashtable-keys (hashset-ht hs1))])
                 (vector-for-each proc k0* k1*)))]
      [(proc hs0 . hs*)
       (pcheck ([procedure? proc] [hashset? hs0] [all-hashsets? hs*])
               (apply check-size who hs0 hs*)
               (let* ([k0* (hashtable-keys (hashset-ht hs0))] [len (vector-length k0*)]
                      [k** (map hashtable-keys (map hashset-ht hs*))])
                 (let loop ([i 0])
                   (unless (fx= i len)
                     (apply proc (vector-ref k0* i)
                            (map (lambda (v) (vector-ref v i)) k**))
                     (loop (fx1+ i))))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   conversions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|proc:hashset->list
  Convert a hashset `hs` into a list.
  |#
  (define-who hashset->list
    (lambda (hs)
      (pcheck ([hashset? hs])
              (vector->list (hashtable-keys (hashset-ht hs))))))


  #|proc:hashset->vector
  Convert a hashset `hs` into a vector.
  |#
  (define-who hashset->vector
    (lambda (hs)
      (pcheck ([hashset? hs])
              (vector-copy (hashtable-keys (hashset-ht hs))))))


  #|proc:list->eq-hashset
  Convert a list `ls` into an eq-hashset.
  |#
  (define-who list->eq-hashset
    (lambda (ls)
      (pcheck ([list? ls])
              (let* ([len (length ls)]
                     [hs (make-eq-hashset len)])
                (for-each (lambda (x) (hashset-add! hs x)) ls)
                hs))))


  #|proc:vector->eq-hashset
  Convert a vector `vec` into an eq-hashset.
  |#
  (define-who vector->eq-hashset
    (lambda (vec)
      (pcheck ([vector? vec])
              (let* ([len (vector-length vec)]
                     [hs (make-eq-hashset len)])
                (vector-for-each (lambda (x) (hashset-add! hs x)) vec)
                hs))))


  #|proc:list->eqv-hashset
  Convert a list `ls` into an eqv-hashset.
  |#
  (define-who list->eqv-hashset
    (lambda (ls)
      (pcheck ([list? ls])
              (let* ([len (length ls)]
                     [hs (make-eqv-hashset len)])
                (for-each (lambda (x) (hashset-add! hs x)) ls)
                hs))))


  #|proc:vector->eqv-hashset
  Convert a vector `vec` into an eqv-hashset.
  |#
  (define-who vector->eqv-hashset
    (lambda (vec)
      (pcheck ([vector? vec])
              (let* ([len (vector-length vec)]
                     [hs (make-eqv-hashset len)])
                (vector-for-each (lambda (x) (hashset-add! hs x)) vec)
                hs))))


;;;;===----------------------------------------------------------------------===
;;;; Iterator extension registration
;;;;===----------------------------------------------------------------------===

  (iter-register-source!
   hashset?
   (lambda (hs)
     ;; ChezScheme exposes no lazy hashtable cursor, so snapshot only the keys.
     (let ([keys (hashtable-keys (hashset-ht hs))] [index 0])
       (make-iter
        (lambda ()
          (if (fx= index (vector-length keys))
              iter-end
              (let ([key (vector-ref keys index)])
                (set! index (fx1+ index))
                key)))
        (lambda ()
          (set! keys (hashtable-keys (hashset-ht hs)))
          (set! index 0))))))

;;;;===----------------------------------------------------------------------===
;;;; Navigator extension registration
;;;;===----------------------------------------------------------------------===

  (nav-register-set!
   hashset? hashset->list
   (lambda (hs members)
     (let ([copy ($newhs hs)])
       (for-each (lambda (member) (hashset-add! copy member)) members)
       copy))
   (lambda (hs members)
     (hashset-clear! hs)
     (for-each (lambda (member) (hashset-add! hs member)) members)
     hs)
   (lambda (hs member)
     (let ([copy (hashset-map (lambda (item) item) hs)])
       (hashset-delete! copy member)
       copy))
   (lambda (hs member)
     (hashset-delete! hs member)
     hs))

  (record-writer (type-descriptor $hashset)
                 (lambda (r p wr)
                   (display "#[hashset " p)
                   (display (hashset->list r) p)
                   (display "]" p)))


  (record-type-equal-procedure
   (type-descriptor $hashset)
   (lambda (hs1 hs2 =?)
     (let* ([t1 (hashset-type hs1)] [t2 (hashset-type hs2)]
            [ht1 (hashset-ht hs1)]  [ht2 (hashset-ht hs2)])
       (and (eq? t1 t2)
            (fx= (hashtable-size ht1) (hashtable-size ht2))
            (let ([v1 (hashtable-keys ht1)])
              (let loop ([i 0])
                (if (fx= i (vector-length v1))
                    #t
                    (if (hashtable-contains? ht2 (vector-ref v1 i))
                        (loop (fx1+ i))
                        #f))))))))

  )
