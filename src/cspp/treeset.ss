(library (cspp treeset)
  (export make-treeset make-fxtreeset fxtreeset fxtreeset? treeset treeset? treeset-empty? treeset-size
          fxtreeset-empty? fxtreeset-size
          treeset-add! treeset-delete! treeset-clear!
          fxtreeset-add! fxtreeset-delete! fxtreeset-clear!

          treeset-contains? treeset-contains/p?
          fxtreeset-contains? fxtreeset-contains/p?
          treeset-filter treeset-filter! treeset-partition
          fxtreeset-filter fxtreeset-filter! fxtreeset-partition
          treeset-search treeset-search*
          fxtreeset-search fxtreeset-search*

          treeset-successor treeset-predecessor
          treeset-min treeset-max
          fxtreeset-successor fxtreeset-predecessor
          fxtreeset-min fxtreeset-max

          treeset-andmap treeset-ormap
          fxtreeset-andmap fxtreeset-ormap
          treeset-map treeset-map/i
          fxtreeset-map fxtreeset-map/i
          treeset-for-each treeset-for-each/i
          fxtreeset-for-each fxtreeset-for-each/i
          treeset-fold-left treeset-fold-left/i
          fxtreeset-fold-left fxtreeset-fold-left/i
          treeset-fold-right treeset-fold-right/i
          fxtreeset-fold-right fxtreeset-fold-right/i

          treeset+ treeset- treeset& treeset^
          fxtreeset+ fxtreeset- fxtreeset& fxtreeset^
          treeset+! treeset-! treeset&! treeset^!
          fxtreeset+! fxtreeset-! fxtreeset&! fxtreeset^!

          treeset->list list->treeset
          treeset->vector vector->treeset
          fxtreeset->list fxtreeset->vector
          list->fxtreeset vector->fxtreeset)
  (import (cspp chez)
          (cspp list)
          (cspp internal)
          (cspp utils)
          (cspp private rbtree)
          (only (cspp iter) iter-register-source! make-iter iter-end)
          (only (cspp navigator) nav-register-set!))

  #|macro:define-treeset-procedure
  `(define-treeset-procedure (name fxname) implementation)` defines the generic `name` and fixnum
  `fxname` procedures from one lambda or case-lambda `implementation`. `tree?`, `tree-like`, `fx-`
  `mode`, and `checked` supply the fixed family predicate, constructor, storage mode, and item
  validation. `tree-who` names the procedure and `tree-self` refers to it. `all-family?` and
  `check-family-size` check multiple inputs; arity-specific map checkers validate callback results
  before specialized writes. `tree-size`, `tree-add!`, `tree-clear!`, `tree-list`, and the four
  algebra helpers name same-family APIs.
  |#
  (define-syntax define-treeset-procedure
    (lambda (stx)
      (syntax-case stx ()
        [(k (name fxname) implementation)
         (with-implicit (k tree? tree-like fx-mode checked tree-who tree-self
                           all-family? check-family-size checked-map-proc1 checked-map-proc2
                           checked-map-proc* tree-size
                           tree-add! tree-clear! tree-list tree-union tree-difference
                           tree-intersection tree-symmetric-difference)
           #'(begin
               (define name
                 (let ([tree-who 'name])
                   (let-syntax
                     ([tree? (identifier-syntax treeset?)]
                      [tree-like (identifier-syntax make-treeset-like)]
                      [fx-mode (identifier-syntax #f)]
                      [checked (identifier-syntax (lambda (item) item))]
                      [tree-self (identifier-syntax name)]
                      [all-family? (identifier-syntax all-treesets?)]
                      [check-family-size (identifier-syntax check-size)]
                      [checked-map-proc1 (identifier-syntax (lambda (who proc) proc))]
                      [checked-map-proc2 (identifier-syntax (lambda (who proc) proc))]
                      [checked-map-proc* (identifier-syntax (lambda (who proc) proc))]
                      [tree-size (identifier-syntax treeset-size)]
                      [tree-add! (identifier-syntax treeset-add!)]
                      [tree-clear! (identifier-syntax treeset-clear!)]
                      [tree-list (identifier-syntax treeset->list)]
                      [tree-union (identifier-syntax treeset+)]
                      [tree-difference (identifier-syntax treeset-)]
                      [tree-intersection (identifier-syntax treeset&)]
                      [tree-symmetric-difference (identifier-syntax treeset^)])
                     implementation)))
               (define fxname
                 (let ([tree-who 'fxname])
                   (let-syntax
                     ([tree? (identifier-syntax fxtreeset?)]
                      [tree-like (identifier-syntax make-fxtreeset-like)]
                      [fx-mode (identifier-syntax #t)]
                      [checked (identifier-syntax
                                 (lambda (item) (pcheck ([fixnum? item]) item)))]
                      [tree-self (identifier-syntax fxname)]
                      [all-family? (identifier-syntax all-fxtreesets?)]
                      [check-family-size (identifier-syntax fx-check-size)]
                      [checked-map-proc1 (identifier-syntax check-fxtreeset-map-proc1)]
                      [checked-map-proc2 (identifier-syntax check-fxtreeset-map-proc2)]
                      [checked-map-proc* (identifier-syntax check-fxtreeset-map-proc*)]
                      [tree-size (identifier-syntax fxtreeset-size)]
                      [tree-add! (identifier-syntax fxtreeset-add!)]
                      [tree-clear! (identifier-syntax fxtreeset-clear!)]
                      [tree-list (identifier-syntax fxtreeset->list)]
                      [tree-union (identifier-syntax fxtreeset+)]
                      [tree-difference (identifier-syntax fxtreeset-)]
                      [tree-intersection (identifier-syntax fxtreeset&)]
                      [tree-symmetric-difference (identifier-syntax fxtreeset^)])
                     implementation)))))])))

  (define all-treesets? (lambda (sets) (andmap treeset? sets)))
  (define all-fxtreesets? (lambda (sets) (andmap fxtreeset? sets)))

  (define check-size
    (lambda (who first . rest)
      (unless (null? rest)
        (unless (apply fx= (treeset-size first) (map treeset-size rest))
          (errorf who "treesets are not of the same size")))))

  (define fx-check-size
    (lambda (who first . rest)
      (unless (null? rest)
        (unless (apply fx= (fxtreeset-size first) (map fxtreeset-size rest))
          (errorf who "treesets are not of the same size")))))

  (define-record-type ($treeset mk-treeset treeset-record?)
    (parent rbtree) (nongenerative) (opaque #t)
    (protocol (lambda (pnew)
                (lambda (=? <? size)
                  ((pnew =? <? size #f))))))

  #|proc:fxtreeset?
  Return whether `object` is a fixnum treeset. Any object may be tested.
  |#
  #|record:$fxtreeset
  Ordered set record restricted to fixnum members.
  |#
  (define-record-type ($fxtreeset mk-fxtreeset fxtreeset?)
    (parent rbtree) (nongenerative) (opaque #t)
    (protocol (lambda (pnew) (lambda (=? <? size) ((pnew =? <? size #t))))))
  #|proc:treeset?
  Return whether `object` is a generic treeset. Any object may be tested.
  |#
  (define treeset? treeset-record?)

  #|proc:make-fxtreeset
  Construct a treeset whose items are exact fixnums. `=?` compares items for equality and `<?`
  orders items. Returns an empty treeset.
  |#
  (define make-fxtreeset
    (lambda (=? <?)
      (pcheck ([procedure? =? <?]) (mk-fxtreeset =? <? 0))))

  (define make-treeset-like
    (lambda (source)
      (make-treeset (rbtree-=? source) (rbtree-<? source))))

  (define check-fxtreeset-map-proc1
    (lambda (who proc)
      (lambda (item)
        (let ([new-item (proc item)])
          (pcheck ([fixnum? new-item]) new-item)))))

  (define check-fxtreeset-map-proc2
    (lambda (who proc)
      (lambda (item0 item1)
        (let ([new-item (proc item0 item1)])
          (pcheck ([fixnum? new-item]) new-item)))))

  (define check-fxtreeset-map-proc*
    (lambda (who proc)
      (lambda args
        (let ([new-item (apply proc args)])
          (pcheck ([fixnum? new-item]) new-item)))))


  #|proc:make-treeset
  Return an empty generic treeset. `=?` compares two items for equality and `<?` orders them.
  |#
  (define make-treeset
    (lambda (=? <?)
      (pcheck ([procedure? =? <?])
              (mk-treeset =? <? 0))))


  #|proc:treeset
  Return a new generic treeset containing the items in `args`, storing duplicates once. `=?`
  compares two items for equality and `<?` orders them.
  |#
  (define-who treeset
    (lambda (=? <? . args)
      (pcheck ([procedure? =? <?])
              (let ([ts (make-treeset =? <?)])
                (for-each (lambda (item) (treeset-add! ts item)) args)
                ts))))

  #|proc:fxtreeset
  Create a fixnum-key treeset initialized with the supplied fixnums. `=?` compares items and `<?`
  orders items. Returns the populated treeset.
  |#
  (define-who fxtreeset
    (lambda (=? <? . args)
      (pcheck ([procedure? =? <?])
              (let ([ts (make-fxtreeset =? <?)])
                (for-each (lambda (x)
                            (unless (fixnum? x)
                              (errorf who "not a fixnum treeset item: ~a" x))
                            (fxtreeset-add! ts x))
                          args)
                ts))))


  #|proc:treeset-contains/p?
  Return whether set `ts` contains an item for which `(pred item)` is true. `pred` takes one item
  and returns a truth value; `ts` must belong to the named family.
  |#
  #|proc:fxtreeset-contains/p?
  Return whether set `ts` contains an item for which `(pred item)` is true. `pred` takes one item
  and returns a truth value; `ts` must belong to the named family.
  |#
  (define-treeset-procedure (treeset-contains/p? fxtreeset-contains/p?)
    (lambda (ts pred)
      (pcheck ([tree? ts] [procedure? pred])
              (rbtree-contains/p? tree-who ts (lambda (k v) (pred k))))))


  #|proc:treeset-filter
  Return a new set containing items in `ts` for which `(pred item)` is true. `pred` takes one item
  and returns a truth value. Preserve `ts`'s comparators and family.
  |#
  #|proc:fxtreeset-filter
  Return a new set containing items in `ts` for which `(pred item)` is true. `pred` takes one item
  and returns a truth value. Preserve `ts`'s comparators and family.
  |#
  (define-treeset-procedure (treeset-filter fxtreeset-filter)
    (lambda (pred ts)
      (pcheck ([procedure? pred] [tree? ts])
              (let ([newts (tree-like ts)])
                (rbtree-visit tree-who (lambda (k v) (when (pred k) (rbtree-set! tree-who newts fx-mode k *dummy-v*))) ts)
                newts))))


  #|proc:treeset-filter!
  Retain items in set `ts` for which `(pred item)` is true, and return `ts`. `pred` takes one item
  and returns a truth value; `ts` must belong to the named family.
  |#
  #|proc:fxtreeset-filter!
  Retain items in set `ts` for which `(pred item)` is true, and return `ts`. `pred` takes one item
  and returns a truth value; `ts` must belong to the named family.
  |#
  (define-treeset-procedure (treeset-filter! fxtreeset-filter!)
    (lambda (pred ts)
      (pcheck ([procedure? pred] [tree? ts])
              (let ([lb (make-list-builder)])
                (rbtree-visit tree-who (lambda (k v) (lb k)) ts)
                (for-each (lambda (v)
                            (unless (pred v)
                              (rbtree-delete! tree-who ts fx-mode v)))
                          (lb))
                ts))))


  #|proc:treeset-partition
  Apply `pred` to every item in treeset `ts` and return two values, the first one a treeset of
  items for which `(pred item)` is true, the second one a treeset of the remaining items. Both
  belong to the input family. `pred` takes one item and returns a truth value. Both results use
  `ts`'s comparators.
  |#
  #|proc:fxtreeset-partition
  Apply `pred` to every item in treeset `ts` and return two values, the first one a treeset of
  items for which `(pred item)` is true, the second one a treeset of the remaining items. Both
  belong to the input family. `pred` takes one item and returns a truth value. Both results use
  `ts`'s comparators.
  |#
  (define-treeset-procedure (treeset-partition fxtreeset-partition)
    (lambda (pred ts)
      (pcheck ([procedure? pred] [tree? ts])
              (let ([T (tree-like ts)]
                    [F (tree-like ts)])
                (rbtree-visit tree-who (lambda (k v) (if (pred k)
                                                    (rbtree-set! tree-who T fx-mode k *dummy-v*)
                                                    (rbtree-set! tree-who F fx-mode k *dummy-v*)))
                              ts)
                (values T F)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   set operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|proc:treeset+
  Compute the union of first set `ts` and additional sets `ts*`, i.e., the treeset that contains
  all items in all the given treesets. If only one treeset is given, it is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  #|proc:fxtreeset+
  Compute the union of first set `ts` and additional sets `ts*`, i.e., the treeset that contains
  all items in all the given treesets. If only one treeset is given, it is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  (define-treeset-procedure (treeset+ fxtreeset+)
    (lambda (ts . ts*)
      (pcheck ([tree? ts])
              (if (null? ts*)
                  ts
                  (pcheck ([all-family? ts*])
                          (let ([newts (tree-like ts)])
                            (for-each (lambda (ts)
                                        (rbtree-visit tree-who
                                                      (lambda (k v)
                                                        (rbtree-set! tree-who newts fx-mode k *dummy-v*))
                                                      ts))
                                      (cons ts ts*))
                            newts))))))


  #|proc:treeset-
  Compute the difference of first set `ts` and additional sets `ts*`, i.e., the treeset that
  contains those items that are in the first treeset, but are not in the rest of the treesets. If
  only one treeset is given, it is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  #|proc:fxtreeset-
  Compute the difference of first set `ts` and additional sets `ts*`, i.e., the treeset that
  contains those items that are in the first treeset, but are not in the rest of the treesets. If
  only one treeset is given, it is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  (define-treeset-procedure (treeset- fxtreeset-)
    (lambda (ts . ts*)
      (pcheck ([tree? ts])
              (if (null? ts*)
                  ts
                  (pcheck ([all-family? ts*])
                          (let ([newts (tree-like ts)])
                            (rbtree-visit tree-who (lambda (k v) (rbtree-set! tree-who newts fx-mode k *dummy-v*)) ts)
                            (for-each (lambda (ts)
                                        (rbtree-visit tree-who
                                                      (lambda (k v)
                                                        (when (rbtree-contains? tree-who newts k)
                                                          (rbtree-delete! tree-who newts fx-mode k)))
                                                      ts))
                                      ts*)
                            newts))))))


  #|proc:treeset&
  Compute the intersection of first set `ts` and additional sets `ts*`, i.e., the treeset whose
  items are contained in all given treesets. If only one treeset is given, it is returned
  immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  #|proc:fxtreeset&
  Compute the intersection of first set `ts` and additional sets `ts*`, i.e., the treeset whose
  items are contained in all given treesets. If only one treeset is given, it is returned
  immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  (define-treeset-procedure (treeset& fxtreeset&)
    (lambda (ts . ts*)
      (pcheck ([tree? ts])
              (if (null? ts*)
                  ts
                  (pcheck ([all-family? ts*])
                          (let ([newts (apply tree-union ts ts*)] [lb (make-list-builder)])
                            (rbtree-visit tree-who
                                          (lambda (k v)
                                            (unless (andmap (lambda (ts) (rbtree-contains? tree-who ts k))
                                                            (cons ts ts*))
                                              (lb k)))
                                          newts)
                            (for-each (lambda (k) (rbtree-delete! tree-who newts fx-mode k)) (lb))
                            newts))))))


  #|proc:treeset^
  Compute the symmetric difference of first set `ts` and additional sets `ts*`, i.e., the
  difference of the union and the intersection of the treesets. If only one treeset is given, it
  is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  #|proc:fxtreeset^
  Compute the symmetric difference of first set `ts` and additional sets `ts*`, i.e., the
  difference of the union and the intersection of the treesets. If only one treeset is given, it
  is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  (define-treeset-procedure (treeset^ fxtreeset^)
    (lambda (ts . ts*)
      (pcheck ([tree? ts] [all-family? ts*])
              (if (null? ts*)
                  ts
                  (let ([newts (tree-like ts)]
                        [lb (make-list-builder)])
                    ;; union
                    (for-each (lambda (ts)
                                (rbtree-visit tree-who
                                              (lambda (k v)
                                                (rbtree-set! tree-who newts fx-mode k *dummy-v*))
                                              ts))
                              (cons ts ts*))
                    ;; intersect
                    (rbtree-visit tree-who
                                  (lambda (k v)
                                    (when (andmap (lambda (ts) (rbtree-contains? tree-who ts k))
                                                  (cons ts ts*))
                                      (lb k)))
                                  newts)
                    ;; diff
                    (for-each (lambda (k) (rbtree-delete! tree-who newts fx-mode k)) (lb))
                    newts)))))


;;;; imperative versions

  #|proc:treeset+!
  Replace treeset `ts` with the union of `ts` and the additional treesets `ts*`. Return `ts`;
  without additional sets it is unchanged. Aliased operands are supported. Compute using the first
  set's comparators and preserve its family.
  |#
  #|proc:fxtreeset+!
  Replace treeset `ts` with the union of `ts` and the additional treesets `ts*`. Return `ts`;
  without additional sets it is unchanged. Aliased operands are supported. Compute using the first
  set's comparators and preserve its family.
  |#
  (define-treeset-procedure (treeset+! fxtreeset+!)
    (lambda (ts . ts*)
      (pcheck ([tree? ts] [all-family? ts*])
              (unless (null? ts*)
                (let ([result (apply tree-union ts ts*)])
                  (tree-clear! ts)
                  (for-each (lambda (item) (tree-add! ts item)) (tree-list result))))
              ts)))


  #|proc:treeset-!
  Replace treeset `ts` with the difference of `ts` and the additional treesets `ts*`. Return `ts`;
  without additional sets it is unchanged. Aliased operands are supported. Compute using the first
  set's comparators and preserve its family.
  |#
  #|proc:fxtreeset-!
  Replace treeset `ts` with the difference of `ts` and the additional treesets `ts*`. Return `ts`;
  without additional sets it is unchanged. Aliased operands are supported. Compute using the first
  set's comparators and preserve its family.
  |#
  (define-treeset-procedure (treeset-! fxtreeset-!)
    (lambda (ts . ts*)
      (pcheck ([tree? ts] [all-family? ts*])
              (unless (null? ts*)
                (let ([result (apply tree-difference ts ts*)])
                  (tree-clear! ts)
                  (for-each (lambda (item) (tree-add! ts item)) (tree-list result))))
              ts)))


  #|proc:treeset&!
  Replace treeset `ts` with the intersection of `ts` and the additional treesets `ts*`. Return
  `ts`; without additional sets it is unchanged. Aliased operands are supported. Compute using the
  first set's comparators and preserve its family.
  |#
  #|proc:fxtreeset&!
  Replace treeset `ts` with the intersection of `ts` and the additional treesets `ts*`. Return
  `ts`; without additional sets it is unchanged. Aliased operands are supported. Compute using the
  first set's comparators and preserve its family.
  |#
  (define-treeset-procedure (treeset&! fxtreeset&!)
    (lambda (ts . ts*)
      (pcheck ([tree? ts] [all-family? ts*])
              (unless (null? ts*)
                (let ([result (apply tree-intersection ts ts*)])
                  (tree-clear! ts)
                  (for-each (lambda (item) (tree-add! ts item)) (tree-list result))))
              ts)))


  #|proc:treeset^!
  Replace treeset `ts` with the union minus intersection of `ts` and the additional treesets
  `ts*`. Return `ts`; without additional sets it is unchanged. Aliased operands are supported.
  Compute using the first set's comparators and preserve its family.
  |#
  #|proc:fxtreeset^!
  Replace treeset `ts` with the union minus intersection of `ts` and the additional treesets
  `ts*`. Return `ts`; without additional sets it is unchanged. Aliased operands are supported.
  Compute using the first set's comparators and preserve its family.
  |#
  (define-treeset-procedure (treeset^! fxtreeset^!)
    (lambda (ts . ts*)
      (pcheck ([tree? ts] [all-family? ts*])
              (unless (null? ts*)
                (let ([result (apply tree-symmetric-difference ts ts*)])
                  (tree-clear! ts)
                  (for-each (lambda (item) (tree-add! ts item)) (tree-list result))))
              ts)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; no in-place maps since we can't modify the tree structure


  #|proc:treeset-andmap
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. Return #f at the first false callback result; otherwise return
  #t.
  |#
  #|proc:fxtreeset-andmap
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. Return #f at the first false callback result; otherwise return
  #t.
  |#
  (define-treeset-procedure (treeset-andmap fxtreeset-andmap)
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [tree? ts0])
               (rbtree-andmap1 tree-who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [tree? ts0 ts1])
               (check-family-size tree-who ts0 ts1)
               (rbtree-andmap1 tree-who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [tree? ts0] [all-family? ts*])
               (apply check-family-size tree-who ts0 ts*)
               (apply rbtree-andmap1 tree-who proc ts0 ts*))]))


  #|proc:treeset-ormap
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. Return the first true callback result, or #f if no result is
  true.
  |#
  #|proc:fxtreeset-ormap
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. Return the first true callback result, or #f if no result is
  true.
  |#
  (define-treeset-procedure (treeset-ormap fxtreeset-ormap)
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [tree? ts0])
               (rbtree-ormap1 tree-who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [tree? ts0 ts1])
               (check-family-size tree-who ts0 ts1)
               (rbtree-ormap1 tree-who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [tree? ts0] [all-family? ts*])
               (apply check-family-size tree-who ts0 ts*)
               (apply rbtree-ormap1 tree-who proc ts0 ts*))]))


  #|proc:treeset-map
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. Return a new treeset using the first input's comparators and
  family. The callback returns the new item, which must be a fixnum for the fixnum family.
  |#
  #|proc:fxtreeset-map
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. Return a new treeset using the first input's comparators and
  family. The callback returns the new item, which must be a fixnum for the fixnum family.
  |#
  (define-treeset-procedure (treeset-map fxtreeset-map)
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [tree? ts0])
               (rbtree-map1 tree-who (checked-map-proc1 tree-who proc) (tree-like ts0) ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [tree? ts0 ts1])
               (check-family-size tree-who ts0 ts1)
               (rbtree-map1 tree-who (checked-map-proc2 tree-who proc) (tree-like ts0) ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [tree? ts0] [all-family? ts*])
               (apply check-family-size tree-who ts0 ts*)
               (apply rbtree-map1 tree-who (checked-map-proc* tree-who proc) (tree-like ts0) ts0 ts*))]))


  #|proc:treeset-map/i
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (index item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. Indices are zero-based inorder positions. Return a new treeset
  using the first input's comparators and family. The callback returns the new item, which must be
  a fixnum for the fixnum family.
  |#
  #|proc:fxtreeset-map/i
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (index item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. Indices are zero-based inorder positions. Return a new treeset
  using the first input's comparators and family. The callback returns the new item, which must be
  a fixnum for the fixnum family.
  |#
  (define-treeset-procedure (treeset-map/i fxtreeset-map/i)
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [tree? ts0])
               (rbtree-map/i1 tree-who (checked-map-proc1 tree-who proc) (tree-like ts0) ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [tree? ts0 ts1])
               (check-family-size tree-who ts0 ts1)
               (rbtree-map/i1 tree-who (checked-map-proc2 tree-who proc) (tree-like ts0) ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [tree? ts0] [all-family? ts*])
               (apply check-family-size tree-who ts0 ts*)
               (apply rbtree-map/i1 tree-who (checked-map-proc* tree-who proc) (tree-like ts0) ts0 ts*))]))


  #|proc:treeset-for-each
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. Callback results are ignored. Return an unspecified value.
  |#
  #|proc:fxtreeset-for-each
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. Callback results are ignored. Return an unspecified value.
  |#
  (define-treeset-procedure (treeset-for-each fxtreeset-for-each)
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [tree? ts0])
               (rbtree-for-each1 tree-who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [tree? ts0 ts1])
               (check-family-size tree-who ts0 ts1)
               (rbtree-for-each1 tree-who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [tree? ts0] [all-family? ts*])
               (apply check-family-size tree-who ts0 ts*)
               (apply rbtree-for-each1 tree-who proc ts0 ts*))]))


  #|proc:treeset-for-each/i
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (index item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. Indices are zero-based inorder positions. Callback results are
  ignored. Return an unspecified value.
  |#
  #|proc:fxtreeset-for-each/i
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (index item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. Indices are zero-based inorder positions. Callback results are
  ignored. Return an unspecified value.
  |#
  (define-treeset-procedure (treeset-for-each/i fxtreeset-for-each/i)
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [tree? ts0])
               (rbtree-for-each/i1 tree-who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [tree? ts0 ts1])
               (check-family-size tree-who ts0 ts1)
               (rbtree-for-each/i1 tree-who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [tree? ts0] [all-family? ts*])
               (apply check-family-size tree-who ts0 ts*)
               (apply rbtree-for-each/i1 tree-who proc ts0 ts*))]))


;;;; folds


  #|proc:treeset-fold-left
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (acc item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. The callback returns the next accumulator. Return the
  accumulated value; `acc` is its initial value.
  |#
  #|proc:fxtreeset-fold-left
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (acc item0 item1 ...); collections supply ordered positions. Input
  collections must have equal size. The callback returns the next accumulator. Return the
  accumulated value; `acc` is its initial value.
  |#
  (define-treeset-procedure (treeset-fold-left fxtreeset-fold-left)
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [tree? ts0])
               (rbtree-fold-left1 tree-who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [tree? ts0 ts1])
               (check-family-size tree-who ts0 ts1)
               (rbtree-fold-left1 tree-who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [tree? ts0] [all-family? ts*])
               (apply check-family-size tree-who ts0 ts*)
               (apply rbtree-fold-left1 tree-who proc acc ts0 ts*))]))


  #|proc:treeset-fold-left/i
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (index acc item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions. The callback
  returns the next accumulator. Return the accumulated value; `acc` is its initial value.
  |#
  #|proc:fxtreeset-fold-left/i
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in ascending comparator
  order. `proc` has signature (index acc item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions. The callback
  returns the next accumulator. Return the accumulated value; `acc` is its initial value.
  |#
  (define-treeset-procedure (treeset-fold-left/i fxtreeset-fold-left/i)
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [tree? ts0])
               (rbtree-fold-left/i1 tree-who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [tree? ts0 ts1])
               (check-family-size tree-who ts0 ts1)
               (rbtree-fold-left/i1 tree-who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [tree? ts0] [all-family? ts*])
               (apply check-family-size tree-who ts0 ts*)
               (apply rbtree-fold-left/i1 tree-who proc acc ts0 ts*))]))


  #|proc:treeset-fold-right
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in descending comparator
  order. `proc` has signature (item0 item1 ... acc); collections supply ordered positions. Input
  collections must have equal size. The callback returns the next accumulator. Return the
  accumulated value; `acc` is its initial value.
  |#
  #|proc:fxtreeset-fold-right
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in descending comparator
  order. `proc` has signature (item0 item1 ... acc); collections supply ordered positions. Input
  collections must have equal size. The callback returns the next accumulator. Return the
  accumulated value; `acc` is its initial value.
  |#
  (define-treeset-procedure (treeset-fold-right fxtreeset-fold-right)
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [tree? ts0])
               (rbtree-fold-right1 tree-who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [tree? ts0 ts1])
               (check-family-size tree-who ts0 ts1)
               (rbtree-fold-right1 tree-who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [tree? ts0] [all-family? ts*])
               (apply check-family-size tree-who ts0 ts*)
               (apply rbtree-fold-right1 tree-who proc acc ts0 ts*))]))


  #|proc:treeset-fold-right/i
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in descending comparator
  order. `proc` has signature (index item0 item1 ... acc); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions. The callback
  returns the next accumulator. Return the accumulated value; `acc` is its initial value.
  |#
  #|proc:fxtreeset-fold-right/i
  Traverse input treesets `ts0`, `ts1`, and any additional sets `ts*` in descending comparator
  order. `proc` has signature (index item0 item1 ... acc); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions. The callback
  returns the next accumulator. Return the accumulated value; `acc` is its initial value.
  |#
  (define-treeset-procedure (treeset-fold-right/i fxtreeset-fold-right/i)
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [tree? ts0])
               (rbtree-fold-right/i1 tree-who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [tree? ts0 ts1])
               (check-family-size tree-who ts0 ts1)
               (rbtree-fold-right/i1 tree-who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [tree? ts0] [all-family? ts*])
               (apply check-family-size tree-who ts0 ts*)
               (apply rbtree-fold-right/i1 tree-who proc acc ts0 ts*))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   conversions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  #|proc:treeset->list
  Return a list of the items in treeset `ts`. By default, the treeset is converted in order.

  `order` can be 'in, 'pre or 'post, so the items are collected in in-order, pre- and post-order,
  respectively.
  |#
  #|proc:fxtreeset->list
  Return a list of the items in treeset `ts`. By default, the treeset is converted in order.

  `order` can be 'in, 'pre or 'post, so the items are collected in in-order, pre- and post-order,
  respectively.
  |#
  (define-treeset-procedure (treeset->list fxtreeset->list)
    (case-lambda
      [(ts)
       (tree-self ts 'in)]
      [(ts order)
       (pcheck ([tree? ts])
               (let ([lb (make-list-builder)])
                 (case order
                   [in   (rbtree-visit-inorder   tree-who (lambda (k v) (lb k)) ts)]
                   [pre  (rbtree-visit-preorder  tree-who (lambda (k v) (lb k)) ts)]
                   [post (rbtree-visit-postorder tree-who (lambda (k v) (lb k)) ts)]
                   [else (errorf tree-who "invalid traversal order: ~a, should be one of 'in, 'pre and 'post" order)])
                 (lb)))]))


  #|proc:treeset->vector
  Return a vector of the items in treeset `ts`. By default, the treeset is converted in order.

  `order` can be 'in, 'pre or 'post, so the items are collected in in-order, pre- and post-order,
  respectively.
  |#
  #|proc:fxtreeset->vector
  Return a vector of the items in treeset `ts`. By default, the treeset is converted in order.

  `order` can be 'in, 'pre or 'post, so the items are collected in in-order, pre- and post-order,
  respectively.
  |#
  (define-treeset-procedure (treeset->vector fxtreeset->vector)
    (case-lambda
      [(ts)
       (tree-self ts 'in)]
      [(ts order)
       (pcheck ([tree? ts])
               (let* ([vec (make-vector (tree-size ts) #f)] [i 0]
                      [add! (lambda (k v) (vector-set! vec i k) (set! i (fx1+ i)))])
                 (case order
                   [in   (rbtree-visit-inorder   tree-who add! ts)]
                   [pre  (rbtree-visit-preorder  tree-who add! ts)]
                   [post (rbtree-visit-postorder tree-who add! ts)]
                   [else (errorf tree-who "invalid traversal order: ~a, should be one of 'in, 'pre and 'post" order)])
                 vec))]))


  #|proc:list->treeset
  Return a generic treeset containing the items in list `ls`. `=?` compares two items for equality
  and `<?` orders them.
  |#
  (define-who list->treeset
    (lambda (=? <? ls)
      (pcheck ([procedure? =? <?] [list? ls])
              (apply treeset =? <? ls))))


  #|proc:vector->treeset
  Return a generic treeset containing the items in vector `vec`. `=?` compares two items for
  equality and `<?` orders them.
  |#
  (define-who vector->treeset
    (lambda (=? <? vec)
      (pcheck ([procedure? =? <?] [vector? vec])
              (let ([ts (make-treeset =? <?)])
                (vector-for-each (lambda (x) (treeset-add! ts x)) vec)
                ts))))


  #|proc:list->fxtreeset
  Return a new fixnum treeset containing the fixnums in list `items`. Equality predicate `equal?`
  and ordering predicate `less?` each take two fixnum items. Duplicate items are stored once.
  |#
  (define list->fxtreeset
    (lambda (equal? less? items)
      (pcheck ([procedure? equal? less?] [list? items])
              (let ([result (make-fxtreeset equal? less?)])
                (for-each (lambda (item) (fxtreeset-add! result item)) items)
                result))))

  #|proc:vector->fxtreeset
  Return a new fixnum treeset containing the fixnums in vector `items`. Equality predicate
  `equal?` and ordering predicate `less?` each take two fixnum items. Duplicate items are stored
  once.
  |#
  (define vector->fxtreeset
    (lambda (equal? less? items)
      (pcheck ([procedure? equal? less?] [vector? items])
              (let ([result (make-fxtreeset equal? less?)])
                (vector-for-each (lambda (item) (fxtreeset-add! result item)) items)
                result))))


  (define make-fxtreeset-like
    (lambda (source)
      (make-fxtreeset (rbtree-=? source) (rbtree-<? source))))


  (define write-treeset
                 (lambda (r p wr)
                   (display "#[treeset (" p)
                   (if (fx= 0 (rbtree-size r))
                       (display ")]" p)
                       (begin
                         (let ([n (rbtree-size r)] [i 0])
                           (rbtree-visit 'treeset-writer
                                         (lambda (k v)
                                           (if (fx= i (fx1- n))
                                               (wr k p)
                                               (begin
                                                 (wr k p)
                                                 (display " " p)))
                                           (set! i (fx1+ i)))
                                         r)
                           (display ")]" p))))))

  #|proc:treeset-empty?
  Return whether the set `ts` is empty. The input must belong to the named family.
  |#
  #|proc:fxtreeset-empty?
  Return whether the set `ts` is empty. The input must belong to the named family.
  |#
  (define-treeset-procedure (treeset-empty? fxtreeset-empty?)
    (lambda (ts)
      (pcheck ([tree? ts]) (fx= 0 (rbtree-size ts)))))

  #|proc:treeset-size
  Return the number of members in `ts`. The input must belong to the named family.
  |#
  #|proc:fxtreeset-size
  Return the number of members in `ts`. The input must belong to the named family.
  |#
  (define-treeset-procedure (treeset-size fxtreeset-size)
    (lambda (ts)
      (pcheck ([tree? ts]) (rbtree-size ts))))

  #|proc:treeset-add!
  Add `item` to `ts`; fixnum sets require a fixnum item. Return the mutation result.
  |#
  #|proc:fxtreeset-add!
  Add `item` to `ts`; fixnum sets require a fixnum item. Return the mutation result.
  |#
  (define-treeset-procedure (treeset-add! fxtreeset-add!)
    (lambda (ts item)
      (pcheck ([tree? ts])
        (checked item)
        (rbtree-set! tree-who ts fx-mode item *dummy-v*))))

  #|proc:treeset-delete!
  Delete `item` from `ts` if present; fixnum sets require a fixnum item. Leave `ts` unchanged when
  the item is absent. Return an unspecified value.
  |#
  #|proc:fxtreeset-delete!
  Delete `item` from `ts` if present; fixnum sets require a fixnum item. Leave `ts` unchanged when
  the item is absent. Return an unspecified value.
  |#
  (define-treeset-procedure (treeset-delete! fxtreeset-delete!)
    (lambda (ts item)
      (pcheck ([tree? ts])
        (checked item)
        (when (rbtree-contains? tree-who ts item)
          (rbtree-delete! tree-who ts fx-mode item)))))

  #|proc:treeset-clear!
  Remove every item from `ts`; the input must belong to the named family. Return an unspecified
  value.
  |#
  #|proc:fxtreeset-clear!
  Remove every item from `ts`; the input must belong to the named family. Return an unspecified
  value.
  |#
  (define-treeset-procedure (treeset-clear! fxtreeset-clear!)
    (lambda (ts)
      (pcheck ([tree? ts]) (rbtree-clear! tree-who ts))))

  #|proc:treeset-contains?
  Return whether `ts` contains `item`; fixnum sets require a fixnum item.
  |#
  #|proc:fxtreeset-contains?
  Return whether `ts` contains `item`; fixnum sets require a fixnum item.
  |#
  (define-treeset-procedure (treeset-contains? fxtreeset-contains?)
    (lambda (ts item)
      (pcheck ([tree? ts])
        (checked item)
        (rbtree-contains? tree-who ts item))))

  #|proc:treeset-search
  Return the first item in set `ts` satisfying `(pred item)`, or `default` (#f when omitted).
  `pred` takes one item and returns a truth value. Supply a unique default to distinguish #f.
  |#
  #|proc:fxtreeset-search
  Return the first item in set `ts` satisfying `(pred item)`, or `default` (#f when omitted).
  `pred` takes one item and returns a truth value. Supply a unique default to distinguish #f.
  |#
  (define-treeset-procedure (treeset-search fxtreeset-search)
    (case-lambda
      [(ts pred) (tree-self ts pred #f)]
      [(ts pred default)
       (pcheck ([tree? ts] [procedure? pred])
         (call-with-values
           (lambda () (rbtree-search tree-who ts (lambda (k v) (pred k))))
           (lambda (key value) (if (eq? key *dummy-v*) default key))))]))

  #|proc:treeset-search*
  Test each item in set `ts` with `(pred item)`; `pred` returns a truth value. Return a list of
  matching items, empty when none match. With `(collect item)` supplied, call it for each match
  and return an unspecified value; its result is ignored.
  |#
  #|proc:fxtreeset-search*
  Test each item in set `ts` with `(pred item)`; `pred` returns a truth value. Return a list of
  matching items, empty when none match. With `(collect item)` supplied, call it for each match
  and return an unspecified value; its result is ignored.
  |#
  (define-treeset-procedure (treeset-search* fxtreeset-search*)
    (case-lambda
      [(ts pred)
       (pcheck ([tree? ts] [procedure? pred])
         (let ([lb (make-list-builder)])
           (rbtree-visit tree-who (lambda (key value) (when (pred key) (lb key))) ts)
           (lb)))]
      [(ts pred collect)
       (pcheck ([tree? ts] [procedure? pred collect])
         (rbtree-visit tree-who
           (lambda (key value) (when (pred key) (collect key))) ts))]))

  #|proc:treeset-successor
  Return the successor of `item` in set `ts`, or `default` (#f when omitted) at the end. An absent
  requested item raises an error; fixnum sets require a fixnum item.
  |#
  #|proc:fxtreeset-successor
  Return the successor of `item` in set `ts`, or `default` (#f when omitted) at the end. An absent
  requested item raises an error; fixnum sets require a fixnum item.
  |#
  (define-treeset-procedure (treeset-successor fxtreeset-successor)
    (case-lambda
      [(ts item) (tree-self ts item #f)]
      [(ts item default)
       (pcheck ([tree? ts])
         (checked item)
         (call-with-values (lambda () (rbtree-successor tree-who ts item))
           (lambda (key value) (if (eq? key *dummy-v*) default key))))]))

  #|proc:treeset-predecessor
  Return the predecessor of `item` in set `ts`, or `default` (#f when omitted) at the start. An
  absent requested item raises an error; fixnum sets require a fixnum item.
  |#
  #|proc:fxtreeset-predecessor
  Return the predecessor of `item` in set `ts`, or `default` (#f when omitted) at the start. An
  absent requested item raises an error; fixnum sets require a fixnum item.
  |#
  (define-treeset-procedure (treeset-predecessor fxtreeset-predecessor)
    (case-lambda
      [(ts item) (tree-self ts item #f)]
      [(ts item default)
       (pcheck ([tree? ts])
         (checked item)
         (call-with-values (lambda () (rbtree-predecessor tree-who ts item))
           (lambda (key value) (if (eq? key *dummy-v*) default key))))]))

  #|proc:treeset-min
  Return the minimum item of set `ts`, or `default` (#f when omitted) for an empty set.
  |#
  #|proc:fxtreeset-min
  Return the minimum item of set `ts`, or `default` (#f when omitted) for an empty set.
  |#
  (define-treeset-procedure (treeset-min fxtreeset-min)
    (case-lambda
      [(ts) (tree-self ts #f)]
      [(ts default)
       (pcheck ([tree? ts])
         (call-with-values (lambda () (rbtree-min tree-who ts))
           (lambda (key value) (if (eq? key *dummy-v*) default key))))]))

  #|proc:treeset-max
  Return the maximum item of set `ts`, or `default` (#f when omitted) for an empty set.
  |#
  #|proc:fxtreeset-max
  Return the maximum item of set `ts`, or `default` (#f when omitted) for an empty set.
  |#
  (define-treeset-procedure (treeset-max fxtreeset-max)
    (case-lambda
      [(ts) (tree-self ts #f)]
      [(ts default)
       (pcheck ([tree? ts])
         (call-with-values (lambda () (rbtree-max tree-who ts))
           (lambda (key value) (if (eq? key *dummy-v*) default key))))]))

;;;; Iterator extension registration
;;;;===----------------------------------------------------------------------===

  (iter-register-source!
   treeset?
   (lambda (ts)
     (let ([cursor (rbtree-inorder-cursor ts)])
       (make-iter
        (lambda ()
          (call-with-values cursor
            (lambda (key value)
              (if (eq? key *dummy-v*) iter-end key))))
        (lambda () (set! cursor (rbtree-inorder-cursor ts)))))))

;;;;===----------------------------------------------------------------------===
;;;; Navigator extension registration
;;;;===----------------------------------------------------------------------===

  (nav-register-set!
   treeset? treeset->list
   (lambda (ts members)
     (let ([copy (make-treeset-like ts)])
       (for-each (lambda (member) (treeset-add! copy member)) members)
       copy))
   (lambda (ts members)
     (treeset-clear! ts)
     (for-each (lambda (member) (treeset-add! ts member)) members)
     ts)
   (lambda (ts member)
     (let ([copy (treeset-map (lambda (item) item) ts)])
       (treeset-delete! copy member)
       copy))
   (lambda (ts member)
     (treeset-delete! ts member)
     ts))

  (record-writer (type-descriptor $treeset) write-treeset)

  (iter-register-source!
   fxtreeset?
   (lambda (ts)
     (let ([cursor (rbtree-inorder-cursor ts)])
       (make-iter
        (lambda ()
          (call-with-values cursor
            (lambda (key value) (if (eq? key *dummy-v*) iter-end key))))
        (lambda () (set! cursor (rbtree-inorder-cursor ts)))))))

  (nav-register-set!
   fxtreeset? fxtreeset->list
   (lambda (ts members)
     (let ([copy (fxtreeset-map (lambda (item) item) ts)])
       (fxtreeset-clear! copy)
       (for-each (lambda (member) (fxtreeset-add! copy member)) members) copy))
   (lambda (ts members)
     (fxtreeset-clear! ts)
     (for-each (lambda (member) (fxtreeset-add! ts member)) members) ts)
   (lambda (ts member)
     (let ([copy (fxtreeset-map (lambda (item) item) ts)])
       (fxtreeset-delete! copy member) copy))
   (lambda (ts member) (fxtreeset-delete! ts member) ts))
  (record-writer (type-descriptor $fxtreeset) write-treeset)

  )
