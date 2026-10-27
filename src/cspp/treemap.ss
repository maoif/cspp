(library (cspp treemap)
  (export make-treemap make-fxtreemap fxtreemap fxtreemap? treemap treemap? treemap-empty?
          treemap-set! treemap-ref treemap-size
          treemap-delete! treemap-clear!

          fxtreemap-empty? fxtreemap-set! fxtreemap-ref fxtreemap-size
          fxtreemap-delete! fxtreemap-clear!

          treemap-keys treemap-values treemap-cells
          treemap-search treemap-search*
          treemap-contains? treemap-contains/p?
          treemap-filter treemap-filter! treemap-partition

          fxtreemap-keys fxtreemap-values fxtreemap-cells
          fxtreemap-search fxtreemap-search*
          fxtreemap-contains? fxtreemap-contains/p?
          fxtreemap-filter fxtreemap-filter! fxtreemap-partition

          treemap-successor treemap-predecessor
          treemap-min treemap-max

          fxtreemap-successor fxtreemap-predecessor
          fxtreemap-min fxtreemap-max

          treemap-andmap treemap-ormap
          treemap-map treemap-map/i treemap-map! treemap-map/i!
          treemap-for-each treemap-for-each/i
          treemap-fold-left treemap-fold-left/i
          treemap-fold-right treemap-fold-right/i

          fxtreemap-andmap fxtreemap-ormap
          fxtreemap-map fxtreemap-map/i fxtreemap-map! fxtreemap-map/i!
          fxtreemap-for-each fxtreemap-for-each/i
          fxtreemap-fold-left fxtreemap-fold-left/i
          fxtreemap-fold-right fxtreemap-fold-right/i

          treemap->list fxtreemap->list hashtable->treemap hashtable->fxtreemap

          $rbtree-verify)
  (import (cspp chez)
          (cspp internal)
          (cspp utils)
          (cspp list)
          (cspp private rbtree)
          (only (cspp iter) iter-register-source! make-iter iter-end)
          (only (cspp navigator) nav-register-keyed!))


  (define-record-type ($treemap mk-treemap treemap-record?)
    (parent rbtree) (nongenerative) (opaque #t)
    (protocol (lambda (pnew)
                (lambda (=? <? size)
                  ((pnew =? <? size))))))

  #|proc:treemap?
  Return whether `object` is a generic treemap. Any object may be tested.
  |#
  (define treemap? treemap-record?)

  #|proc:fxtreemap?
  Return whether `object` is a treemap restricted to fixnum keys and values.
  Any object may be tested.
  |#
  #|record:$fxtreemap
  Ordered map record restricted to fixnum keys and values.
  |#
  (define-record-type ($fxtreemap mk-fxtreemap fxtreemap?)
    (parent rbtree) (nongenerative) (opaque #t)
    (protocol (lambda (pnew)
                (lambda (=? <? size) ((pnew =? <? size #t))))))

  #|proc:make-fxtreemap
  Construct a treemap whose keys and values must be exact fixnums.
  `=?` compares keys for equality and `<?` orders keys. Returns an empty treemap.
  |#
  (define make-fxtreemap
    (lambda (=? <?)
      (pcheck ([procedure? =? <?]) (mk-fxtreemap =? <? 0))))


  (define make-treemap-like
    (lambda (source)
      (make-treemap (rbtree-=? source) (rbtree-<? source))))

  (define make-fxtreemap-like
    (lambda (source)
      (make-fxtreemap (rbtree-=? source) (rbtree-<? source))))

  (define check-fxtreemap-map-proc1
    (lambda (who proc)
      (lambda (key value)
        (call-with-values (lambda () (proc key value))
          (lambda (new-key new-value)
            (pcheck ([fixnum? new-key new-value])
                    (values new-key new-value)))))))

  (define check-fxtreemap-map-proc2
    (lambda (who proc)
      (lambda (key0 value0 key1 value1)
        (call-with-values (lambda () (proc key0 value0 key1 value1))
          (lambda (new-key new-value)
            (pcheck ([fixnum? new-key new-value])
                    (values new-key new-value)))))))

  (define check-fxtreemap-map-proc*
    (lambda (who proc)
      (lambda args
        (call-with-values (lambda () (apply proc args))
          (lambda (new-key new-value)
            (pcheck ([fixnum? new-key new-value])
                    (values new-key new-value)))))))

  (define check-fxtreemap-value-proc1
    (lambda (who proc)
      (lambda (key value)
        (let ([new-value (proc key value)])
          (pcheck ([fixnum? new-value]) new-value)))))

  (define check-fxtreemap-value-proc2
    (lambda (who proc)
      (lambda (key0 value0 key1 value1)
        (let ([new-value (proc key0 value0 key1 value1)])
          (pcheck ([fixnum? new-value]) new-value)))))

  (define check-fxtreemap-value-proc*
    (lambda (who proc)
      (lambda args
        (let ([new-value (apply proc args)])
          (pcheck ([fixnum? new-value]) new-value)))))

  #|macro:define-treemap-procedure
  `(define-treemap-procedure suffix implementation)` defines `treemap-suffix` and
  `fxtreemap-suffix` from one lambda or case-lambda expression, `implementation`.
  The body uses `tm?`, `tm-like`, `fx-mode`, `family-key?`, and `family-value?` for
  fixed family checks, construction, storage mode, and input validation. `all-family?`
  and `check-family-size` validate multiple maps; arity-specific callback
  checkers validate results before specialized writes.
  `who` names the generated procedure and `thisproc` refers to that same procedure.
  |#
  (define-syntax define-treemap-procedure
    (lambda (stx)
      (syntax-case stx ()
        [(k suffix implementation)
         (identifier? #'suffix)
         (with-implicit (k who tm? tm-like fx-mode thisproc family-key?
                           family-value? all-family? check-family-size
                           checked-map-proc1 checked-map-proc2 checked-map-proc*
                           checked-value-proc1 checked-value-proc2 checked-value-proc*)
           (with-syntax ([generic-name ($construct-name #'suffix "treemap-" #'suffix)]
                         [fixnum-name ($construct-name #'suffix "fxtreemap-" #'suffix)])
             #'(begin
                 (module (generic-name)
                   (define tm? treemap?)
                   (define tm-like make-treemap-like)
                   (define fx-mode #f)
                   (define family-key? (lambda (x) #t))
                   (define family-value? (lambda (x) #t))
                   (define all-family? (lambda (x*) (andmap treemap? x*)))
                   (define check-family-size
                     (lambda (who x0 . x*)
                       (unless (null? x*)
                         (unless (apply fx= (treemap-size x0) (map treemap-size x*))
                           (errorf who "treemaps are not of the same size")))))
                   (define checked-map-proc1 (lambda (who proc) proc))
                   (define checked-map-proc2 (lambda (who proc) proc))
                   (define checked-map-proc* (lambda (who proc) proc))
                   (define checked-value-proc1 (lambda (who proc) proc))
                   (define checked-value-proc2 (lambda (who proc) proc))
                   (define checked-value-proc* (lambda (who proc) proc))
                   (define who 'generic-name)
                   (define generic-name implementation)
                   (define thisproc generic-name))
                 (module (fixnum-name)
                   (define tm? fxtreemap?)
                   (define tm-like make-fxtreemap-like)
                   (define fx-mode #t)
                   (define family-key? fixnum?)
                   (define family-value? fixnum?)
                   (define all-family? (lambda (x*) (andmap fxtreemap? x*)))
                   (define check-family-size
                     (lambda (who x0 . x*)
                       (unless (null? x*)
                         (unless (apply fx= (fxtreemap-size x0) (map fxtreemap-size x*))
                           (errorf who "treemaps are not of the same size")))))
                   (define checked-map-proc1 check-fxtreemap-map-proc1)
                   (define checked-map-proc2 check-fxtreemap-map-proc2)
                   (define checked-map-proc* check-fxtreemap-map-proc*)
                   (define checked-value-proc1 check-fxtreemap-value-proc1)
                   (define checked-value-proc2 check-fxtreemap-value-proc2)
                   (define checked-value-proc* check-fxtreemap-value-proc*)
                   (define who 'fixnum-name)
                   (define fixnum-name implementation)
                   (define thisproc fixnum-name)))))])))

  #|proc:make-treemap
  Return an empty generic treemap.
  `=?` is used by the treemap internally to do equality comparison of keys;
  `<?` is used by the treemap internally to do order comparison.
  |#
  (define-who make-treemap
    (lambda (=? <?)
      (pcheck ([procedure? =? <?])
              (mk-treemap =? <? 0))))


  #|proc:treemap
  Return a new generic treemap populated from the supplied entries.

  `args` must be a list of pairs in which each pair's car field will be the key,
  and each pair's cdr field will be the value.

  `=?` is used by the treemap internally to do equality comparison of keys;
  `<?` is used by the treemap internally to do order comparison.
  |#
  (define-who treemap
    (lambda (=? <? . args)
      (pcheck ([procedure? =? <?])
              (let ([tm (make-treemap =? <?)])
                (for-each (lambda (x) (unless (pair? x) (errorf who "not a pair: ~a" x))) args)
                (for-each (lambda (x) (treemap-set! tm (car x) (cdr x))) args)
                tm))))

  #|proc:fxtreemap
  Create a fixnum treemap and initialize it from fixnum key/value pairs.
  `=?` and `<?` compare and order fixnum keys; each argument is a pair of fixnums.
  Returns the populated treemap.
  |#
  (define-who fxtreemap
    (lambda (=? <? . args)
      (pcheck ([procedure? =? <?])
              (let ([tm (make-fxtreemap =? <?)])
                (for-each (lambda (x)
                            (unless (and (pair? x) (fixnum? (car x)) (fixnum? (cdr x)))
                              (errorf who "not a fixnum key/value pair: ~a" x))
                            (fxtreemap-set! tm (car x) (cdr x)))
                          args)
                tm))))


  #|proc:treemap-empty?
  Return whether the generic treemap `tm` is empty.
  |#
  #|proc:fxtreemap-empty?
  Return whether the fixnum treemap `tm` is empty.
  |#
  (define-treemap-procedure empty?
    (lambda (tm)
      (pcheck ([tm? tm])
              (fx= 0 (rbtree-size tm)))))


  #|proc:treemap-set!
  Associate key `k` with value `v` in the treemap `tm`.
  If `k` already exists, its original value is replaced by `v`. Return an unspecified value.
  |#
  #|proc:fxtreemap-set!
  Associate key `k` with value `v` in the treemap `tm`.
  If `k` already exists, its original value is replaced by `v`. Return an unspecified value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure set!
    (lambda (tm k v)
      (pcheck ([tm? tm] [family-key? k] [family-value? v])
              (rbtree-set! who tm fx-mode k v))))


  #|proc:treemap-ref
  Return the value keyed by `k` in the treemap `tm`.

  If `default` is given and `k` does not exist in the treemap, `default` is returned.
  If `default` is not given and `k` does not exist, an error is raised.
  |#
  #|proc:fxtreemap-ref
  Return the value keyed by `k` in the treemap `tm`.

  If `default` is given and `k` does not exist in the treemap, `default` is returned.
  If `default` is not given and `k` does not exist, an error is raised.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure ref
    (case-lambda
      [(tm k)
       (pcheck ([tm? tm] [family-key? k])
               (rbtree-ref who tm k))]
      [(tm k default)
       (pcheck ([tm? tm] [family-key? k])
               (rbtree-ref who tm k default))]))


  #|proc:treemap-delete!
  Remove the key `k` along with its value from the treemap `tm`.
  If `k` is absent, the treemap is unchanged. Return an unspecified value.
  |#
  #|proc:fxtreemap-delete!
  Remove the key `k` along with its value from the treemap `tm`.
  If `k` is absent, the treemap is unchanged. Return an unspecified value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure delete!
    (lambda (tm k)
      (pcheck ([tm? tm] [family-key? k])
              (when (rbtree-contains? who tm k)
                (rbtree-delete! who tm fx-mode k)))))


  #|proc:treemap-clear!
  Remove all keys and values from the treemap `tm`. Return an unspecified value.
  |#
  #|proc:fxtreemap-clear!
  Remove all keys and values from the treemap `tm`. Return an unspecified value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure clear!
    (lambda (tm)
      (pcheck ([tm? tm])
              (rbtree-clear! who tm))))


  #|proc:treemap-size
  Return the number of keys in the treemap `tm`.
  |#
  #|proc:fxtreemap-size
  Return the number of keys in the treemap `tm`.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure size
    (lambda (tm)
      (pcheck ([tm? tm])
              (rbtree-size tm))))


  #|proc:treemap-contains?
  Return whether the treemap `tm` contains the key `k`.
  Comparison uses the equality predicate supplied when constructing `tm`.
  |#
  #|proc:fxtreemap-contains?
  Return whether the treemap `tm` contains the key `k`.
  Comparison uses the equality predicate supplied when constructing `tm`.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure contains?
    (lambda (tm k)
      (pcheck ([tm? tm] [family-key? k])
              (rbtree-contains? who tm k))))


  #|proc:treemap-contains/p?
  Return whether treemap `tm` contains at least one key/value pair such that
  (pred key value) returns true.
  |#
  #|proc:fxtreemap-contains/p?
  Return whether treemap `tm` contains at least one key/value pair such that
  (pred key value) returns true.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure contains/p?
    (lambda (tm pred)
      (pcheck ([tm? tm] [procedure? pred])
              (rbtree-contains/p? who tm pred))))


  #|proc:treemap-search
  Return a pair consisting of the first key and value in `tm` such that `(pred key value)`
  returns #t. If no pair matches, return `default`, which defaults to #f.
  |#
  #|proc:fxtreemap-search
  Return a pair consisting of the first key and value in `tm` such that `(pred key value)`
  returns #t. If no pair matches, return `default`, which defaults to #f.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure search
    (case-lambda
      [(tm pred) (thisproc tm pred #f)]
      [(tm pred default)
       (pcheck ([tm? tm] [procedure? pred])
               (call-with-values (lambda () (rbtree-search who tm pred))
                 (lambda (k v) (if (eq? k *dummy-v*) default (cons k v)))))]))


  #|proc:treemap-search*
  Return the list of all key/value pairs in `tm` such that
  for each pair of key and value, (pred key value) returns #t.

  By default the items satisfying `pred` are returned in a list.

  If `collect` is given, it is applied to every key and value pair that satisfies `pred`
  in the treemap. This is useful when collecting the desired key and value pairs in custom
  data structures.
  |#
  #|proc:fxtreemap-search*
  Return the list of all key/value pairs in `tm` such that
  for each pair of key and value, (pred key value) returns #t.

  By default the items satisfying `pred` are returned in a list.

  If `collect` is given, it is applied to every key and value pair that satisfies `pred`
  in the treemap. This is useful when collecting the desired key and value pairs in custom
  data structures.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure search*
    (case-lambda
      [(tm pred)
       (pcheck ([tm? tm] [procedure? pred])
               (let ([lb (make-list-builder)])
                 (rbtree-visit who (lambda (k v) (when (pred k v) (lb (cons k v)))) tm)
                 (lb)))]
      [(tm pred collect)
       (pcheck ([tm? tm] [procedure? pred collect])
               (rbtree-visit who (lambda (k v) (when (pred k v) (collect k v))) tm))]))


  #|proc:treemap-keys
  Return all keys in `tm` in an ascending-order vector, or call `(collect key)`
  for each key and return an unspecified value when `collect` is supplied.
  |#
  #|proc:fxtreemap-keys
  Return all keys in `tm` in an ascending-order vector, or call `(collect key)`
  for each key and return an unspecified value when `collect` is supplied.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure keys
    (case-lambda
      [(tm)
       (pcheck ([tm? tm])
               (let ([lb (make-list-builder)])
                 (rbtree-visit who (lambda (k v) (lb k)) tm)
                 (list->vector (lb))))]
      [(tm collect)
       (pcheck ([tm? tm] [procedure? collect])
               (rbtree-visit who (lambda (k v) (collect k)) tm))]))

  #|proc:treemap-values
  Return all values in `tm` in a vector ordered by their keys, or call
  `(collect value)` for each value and return an unspecified value.
  |#
  #|proc:fxtreemap-values
  Return all values in `tm` in a vector ordered by their keys, or call
  `(collect value)` for each value and return an unspecified value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure values
    (case-lambda
      [(tm)
       (pcheck ([tm? tm])
               (let ([lb (make-list-builder)])
                 (rbtree-visit who (lambda (k v) (lb v)) tm)
                 (list->vector (lb))))]
      [(tm collect)
       (pcheck ([tm? tm] [procedure? collect])
               (rbtree-visit who (lambda (k v) (collect v)) tm))]))


  #|proc:treemap-cells
  Return all key-value pairs in `tm` in a vector ordered by key, or call
  `(collect key value)` for each entry and return an unspecified value.

  Mutating the returned key-value pairs has no effect on the treemap.
  |#
  #|proc:fxtreemap-cells
  Return all key-value pairs in `tm` in a vector ordered by key, or call
  `(collect key value)` for each entry and return an unspecified value.

  Mutating the returned key-value pairs has no effect on the treemap.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure cells
    (case-lambda
      [(tm)
       (pcheck ([tm? tm])
               (let ([lb (make-list-builder)])
                 (rbtree-visit who (lambda (k v) (lb (cons k v))) tm)
                 (list->vector (lb))))]
      [(tm collect)
       (pcheck ([tm? tm] [procedure? collect])
               (rbtree-visit who collect tm))]))


  #|proc:treemap-successor
  Return a pair consisting of a key and its value,
  where the key is the successor of `k` in the treemap `tm`.

  If the successor of `k` does not exist, `default` is returned; it defaults to #f.
  |#
  #|proc:fxtreemap-successor
  Return a pair consisting of a key and its value,
  where the key is the successor of `k` in the treemap `tm`.

  If the successor of `k` does not exist, `default` is returned; it defaults to #f.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure successor
    (case-lambda
      [(tm k) (thisproc tm k #f)]
      [(tm k default)
       (pcheck ([tm? tm] [family-key? k])
               (call-with-values (lambda () (rbtree-successor who tm k))
                 (lambda (key value) (if (eq? key *dummy-v*) default (cons key value)))))]))


  #|proc:treemap-predecessor
  Return a pair consisting of a key and its value,
  where the key is the predecessor of `k` in the treemap `tm`.

  If the predecessor of `k` does not exist, `default` is returned; it defaults to #f.
  |#
  #|proc:fxtreemap-predecessor
  Return a pair consisting of a key and its value,
  where the key is the predecessor of `k` in the treemap `tm`.

  If the predecessor of `k` does not exist, `default` is returned; it defaults to #f.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure predecessor
    (case-lambda
      [(tm k) (thisproc tm k #f)]
      [(tm k default)
       (pcheck ([tm? tm] [family-key? k])
               (call-with-values (lambda () (rbtree-predecessor who tm k))
                 (lambda (key value) (if (eq? key *dummy-v*) default (cons key value)))))]))


  #|proc:treemap-min
  Return a pair consisting of the minimum (leftmost) key and its value in the treemap `tm`.

  If the treemap is empty, `default` is returned; it defaults to #f.
  |#
  #|proc:fxtreemap-min
  Return a pair consisting of the minimum (leftmost) key and its value in the treemap `tm`.

  If the treemap is empty, `default` is returned; it defaults to #f.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure min
    (case-lambda
      [(tm) (thisproc tm #f)]
      [(tm default)
       (pcheck ([tm? tm])
               (call-with-values (lambda () (rbtree-min who tm))
                 (lambda (key value) (if (eq? key *dummy-v*) default (cons key value)))))]))


  #|proc:treemap-max
  Return a pair consisting of the maximum (rightmost) key and its value in the treemap `tm`.

  If the treemap is empty, `default` is returned; it defaults to #f.
  |#
  #|proc:fxtreemap-max
  Return a pair consisting of the maximum (rightmost) key and its value in the treemap `tm`.

  If the treemap is empty, `default` is returned; it defaults to #f.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure max
    (case-lambda
      [(tm) (thisproc tm #f)]
      [(tm default)
       (pcheck ([tm? tm])
               (call-with-values (lambda () (rbtree-max who tm))
                 (lambda (key value) (if (eq? key *dummy-v*) default (cons key value)))))]))


  #|proc:treemap-filter
  Apply `pred` to each pair of keys and values in the treemap `tm`,
  if `(pred key value)` is true, the respective key and value are added to a new
  treemap. Then the new treemap is returned.
  |#
  #|proc:fxtreemap-filter
  Apply `pred` to each pair of keys and values in the treemap `tm`,
  if `(pred key value)` is true, the respective key and value are added to a new
  treemap. Then the new treemap is returned.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure filter
    (lambda (pred tm)
      (pcheck ([procedure? pred] [tm? tm])
              (let ([newtm (tm-like tm)])
                (rbtree-visit who (lambda (k v) (when (pred k v) (rbtree-set! who newtm fx-mode k v))) tm)
                newtm))))


  #|proc:treemap-filter!
  Apply `pred` to each pair of keys and values in the treemap `tm`,
  if `(pred key value)` is false, remove that entry. Return the mutated map `tm`.
  |#
  #|proc:fxtreemap-filter!
  Apply `pred` to each pair of keys and values in the treemap `tm`,
  if `(pred key value)` is false, remove that entry. Return the mutated map `tm`.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure filter!
    (lambda (pred tm)
      (pcheck ([procedure? pred] [tm? tm])
              (let ([lb (make-list-builder)])
                (rbtree-visit who (lambda (k v) (lb (cons k v))) tm)
                (for-each (lambda (kv)
                            (let ([k (car kv)])
                              (unless (pred k (cdr kv))
                                (rbtree-delete! who tm fx-mode k))))
                          (lb))
                tm))))


  #|proc:treemap-partition
  Apply `pred` to every pair of keys and values in `tm` and return two values,
  the first one a treemap of the keys/values of `tm` for which `(pred k v)` returns #t,
  the second one a treemap of the keys/values of `tm` for which `(pred k v)` returns #f.
  |#
  #|proc:fxtreemap-partition
  Apply `pred` to every pair of keys and values in `tm` and return two values,
  the first one a treemap of the keys/values of `tm` for which `(pred k v)` returns #t,
  the second one a treemap of the keys/values of `tm` for which `(pred k v)` returns #f.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure partition
    (lambda (pred tm)
      (pcheck ([procedure? pred] [tm? tm])
              (let ([T (tm-like tm)]
                    [F (tm-like tm)])
                (rbtree-visit who (lambda (k v) (if (pred k v)
                                                    (rbtree-set! who T fx-mode k v)
                                                    (rbtree-set! who F fx-mode k v)))
                              tm)
                (values T F)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; maps return new treemaps.
  ;; =? and <? of the new treemap are taken from the first treemap argument.
  ;; Procs in maps return two values.
  ;; procs should take twice as many args (k + v) as #trees.
  ;; All do inorder traversal.


  #|proc:treemap-andmap
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return #f at the first false callback result; otherwise return #t.
  |#
  #|proc:fxtreemap-andmap
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return #f at the first false callback result; otherwise return #t.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure andmap
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [tm? tm0])
               (rbtree-andmap who proc tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [tm? tm0 tm1])
               (check-family-size who tm0 tm1)
               (rbtree-andmap who proc tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [tm? tm0] [all-family? tm*])
               (apply check-family-size who tm0 tm*)
               (apply rbtree-andmap who proc tm0 tm*))]))


  #|proc:treemap-ormap
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return the first true callback result, or #f if no result is true.
  |#
  #|proc:fxtreemap-ormap
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return the first true callback result, or #f if no result is true.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure ormap
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [tm? tm0])
               (rbtree-ormap who proc tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [tm? tm0 tm1])
               (check-family-size who tm0 tm1)
               (rbtree-ormap who proc tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [tm? tm0] [all-family? tm*])
               (apply check-family-size who tm0 tm*)
               (apply rbtree-ormap who proc tm0 tm*))]))


  #|proc:treemap-map
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return a new treemap using the first input's comparators and backend.
  The callback returns two values: the new key and value.
  |#
  #|proc:fxtreemap-map
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return a new treemap using the first input's comparators and backend.
  The callback returns two values: the new key and value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure map
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [tm? tm0])
               (rbtree-map who (checked-map-proc1 who proc) (tm-like tm0) tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [tm? tm0 tm1])
               (check-family-size who tm0 tm1)
               (rbtree-map who (checked-map-proc2 who proc) (tm-like tm0) tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [tm? tm0] [all-family? tm*])
               (apply check-family-size who tm0 tm*)
               (apply rbtree-map who (checked-map-proc* who proc) (tm-like tm0) tm0 tm*))]))


  #|proc:treemap-map/i
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (index key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return a new treemap using the first input's comparators and backend.
  The callback returns two values: the new key and value.
  |#
  #|proc:fxtreemap-map/i
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (index key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return a new treemap using the first input's comparators and backend.
  The callback returns two values: the new key and value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure map/i
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [tm? tm0])
               (rbtree-map/i who (checked-map-proc1 who proc) (tm-like tm0) tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [tm? tm0 tm1])
               (check-family-size who tm0 tm1)
               (rbtree-map/i who (checked-map-proc2 who proc) (tm-like tm0) tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [tm? tm0] [all-family? tm*])
               (apply check-family-size who tm0 tm*)
               (apply rbtree-map/i who (checked-map-proc* who proc) (tm-like tm0) tm0 tm*))]))


  ;; `proc` in in-place maps should return only one value
  #|proc:treemap-map!
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Update values only; `proc` returns one replacement value. Return an unspecified value.
  |#
  #|proc:fxtreemap-map!
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Update values only; `proc` returns one replacement value. Return an unspecified value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure map!
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [tm? tm0])
               (rbtree-map! who (checked-value-proc1 who proc) fx-mode tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [tm? tm0 tm1])
               (check-family-size who tm0 tm1)
               (rbtree-map! who (checked-value-proc2 who proc) fx-mode tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [tm? tm0] [all-family? tm*])
               (apply check-family-size who tm0 tm*)
               (apply rbtree-map! who (checked-value-proc* who proc) fx-mode tm0 tm*))]))


  #|proc:treemap-map/i!
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (index key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Update values only; `proc` returns one replacement value. Return an unspecified value.
  |#
  #|proc:fxtreemap-map/i!
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (index key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Update values only; `proc` returns one replacement value. Return an unspecified value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure map/i!
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [tm? tm0])
               (rbtree-map/i! who (checked-value-proc1 who proc) fx-mode tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [tm? tm0 tm1])
               (check-family-size who tm0 tm1)
               (rbtree-map/i! who (checked-value-proc2 who proc) fx-mode tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [tm? tm0] [all-family? tm*])
               (apply check-family-size who tm0 tm*)
               (apply rbtree-map/i! who (checked-value-proc* who proc) fx-mode tm0 tm*))]))


  #|proc:treemap-for-each
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return an unspecified value.
  |#
  #|proc:fxtreemap-for-each
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return an unspecified value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure for-each
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [tm? tm0])
               (rbtree-for-each who proc tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [tm? tm0 tm1])
               (check-family-size who tm0 tm1)
               (rbtree-for-each who proc tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [tm? tm0] [all-family? tm*])
               (apply check-family-size who tm0 tm*)
               (apply rbtree-for-each who proc tm0 tm*))]))


  #|proc:treemap-for-each/i
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (index key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return an unspecified value.
  |#
  #|proc:fxtreemap-for-each/i
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (index key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return an unspecified value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure for-each/i
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [tm? tm0])
               (rbtree-for-each/i who proc tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [tm? tm0 tm1])
               (check-family-size who tm0 tm1)
               (rbtree-for-each/i who proc tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [tm? tm0] [all-family? tm*])
               (apply check-family-size who tm0 tm*)
               (apply rbtree-for-each/i who proc tm0 tm*))]))


;;;; folds

  ;; The treemaps' <? procedure defines an ordering of the keys.
  ;; fold-left folds from the leftmost key-value as defined by <?,
  ;; fold-right folds from the rightmost one.

  #|proc:treemap-fold-left
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (acc key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return the accumulated value; `acc` is its initial value.
  |#
  #|proc:fxtreemap-fold-left
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (acc key0 value0 key1 value1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return the accumulated value; `acc` is its initial value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure fold-left
    (case-lambda
      [(proc acc tm0)
       (pcheck ([procedure? proc] [tm? tm0])
               (rbtree-fold-left who proc acc tm0))]
      [(proc acc tm0 tm1)
       (pcheck ([procedure? proc] [tm? tm0 tm1])
               (check-family-size who tm0 tm1)
               (rbtree-fold-left who proc acc tm0 tm1))]
      [(proc acc tm0 . tm*)
       (pcheck ([procedure? proc] [tm? tm0] [all-family? tm*])
               (apply check-family-size who tm0 tm*)
               (apply rbtree-fold-left who proc acc tm0 tm*))]))


  #|proc:treemap-fold-left/i
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (index acc key0 value0 key1 value1 ...); collections supply ordered
  positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return the accumulated value; `acc` is its initial value.
  |#
  #|proc:fxtreemap-fold-left/i
  Traverse the input treemaps in ascending comparator order.
  `proc` has signature (index acc key0 value0 key1 value1 ...); collections supply ordered
  positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return the accumulated value; `acc` is its initial value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure fold-left/i
    (case-lambda
      [(proc acc tm0)
       (pcheck ([procedure? proc] [tm? tm0])
               (rbtree-fold-left/i who proc acc tm0))]
      [(proc acc tm0 tm1)
       (pcheck ([procedure? proc] [tm? tm0 tm1])
               (check-family-size who tm0 tm1)
               (rbtree-fold-left/i who proc acc tm0 tm1))]
      [(proc acc tm0 . tm*)
       (pcheck ([procedure? proc] [tm? tm0] [all-family? tm*])
               (apply check-family-size who tm0 tm*)
               (apply rbtree-fold-left/i who proc acc tm0 tm*))]))


  #|proc:treemap-fold-right
  Traverse the input treemaps in descending comparator order.
  `proc` has signature (key0 value0 key1 value1 ... acc); collections supply ordered positions.
  Input collections must have equal size.
  Return the accumulated value; `acc` is its initial value.
  |#
  #|proc:fxtreemap-fold-right
  Traverse the input treemaps in descending comparator order.
  `proc` has signature (key0 value0 key1 value1 ... acc); collections supply ordered positions.
  Input collections must have equal size.
  Return the accumulated value; `acc` is its initial value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure fold-right
    (case-lambda
      [(proc acc tm0)
       (pcheck ([procedure? proc] [tm? tm0])
               (rbtree-fold-right who proc acc tm0))]
      [(proc acc tm0 tm1)
       (pcheck ([procedure? proc] [tm? tm0 tm1])
               (check-family-size who tm0 tm1)
               (rbtree-fold-right who proc acc tm0 tm1))]
      [(proc acc tm0 . tm*)
       (pcheck ([procedure? proc] [tm? tm0] [all-family? tm*])
               (apply check-family-size who tm0 tm*)
               (apply rbtree-fold-right who proc acc tm0 tm*))]))


  #|proc:treemap-fold-right/i
  Traverse the input treemaps in descending comparator order.
  `proc` has signature (index key0 value0 key1 value1 ... acc); collections supply ordered
  positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return the accumulated value; `acc` is its initial value.
  |#
  #|proc:fxtreemap-fold-right/i
  Traverse the input treemaps in descending comparator order.
  `proc` has signature (index key0 value0 key1 value1 ... acc); collections supply ordered
  positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return the accumulated value; `acc` is its initial value.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure fold-right/i
    (case-lambda
      [(proc acc tm0)
       (pcheck ([procedure? proc] [tm? tm0])
               (rbtree-fold-right/i who proc acc tm0))]
      [(proc acc tm0 tm1)
       (pcheck ([procedure? proc] [tm? tm0 tm1])
               (check-family-size who tm0 tm1)
               (rbtree-fold-right/i who proc acc tm0 tm1))]
      [(proc acc tm0 . tm*)
       (pcheck ([procedure? proc] [tm? tm0] [all-family? tm*])
               (apply check-family-size who tm0 tm*)
               (apply rbtree-fold-right/i who proc acc tm0 tm*))]))

;;;;===----------------------------------------------------------------------===
;;;; Conversions
;;;;===----------------------------------------------------------------------===

  #|proc:treemap->list
  Convert `tm` to an association list of fresh key/value pairs, in inorder by default.

  `order` can be 'in, 'pre or 'post, so the items are collected in
  in-order, pre- and post-order, respectively.
  |#
  #|proc:fxtreemap->list
  Convert `tm` to an association list of fresh key/value pairs, in inorder by default.

  `order` can be 'in, 'pre or 'post, so the items are collected in
  in-order, pre- and post-order, respectively.
  Accept only fixnum treemaps; keys, stored values, and mapped results must be fixnums.
  |#
  (define-treemap-procedure >list
    (case-lambda
      [(tm)
       (thisproc tm 'in)]
      [(tm order)
       (pcheck ([tm? tm])
               (let ([lb (make-list-builder)])
                 (case order
                   [in   (rbtree-visit-inorder   who (lambda (k v) (lb (cons k v))) tm)]
                   [pre  (rbtree-visit-preorder  who (lambda (k v) (lb (cons k v))) tm)]
                   [post (rbtree-visit-postorder who (lambda (k v) (lb (cons k v))) tm)]
                   [else (errorf who "invalid traversal order: ~a, should be one of 'in, 'pre and 'post" order)])
                 (lb)))]))


  #|proc:hashtable->treemap
  Return a new generic treemap containing the entries of hashtable `ht`.
  `=?` compares keys for equality and `<?` orders keys, as in `treemap`.
  |#
  (define-who hashtable->treemap
    (lambda (=? <? ht)
      (pcheck ([hashtable? ht] [procedure? =? <?])
              (let ([tm (make-treemap =? <?)])
                (vector-for-each (lambda (kv) (treemap-set! tm (car kv) (cdr kv)))
                                 (hashtable-cells ht))
                tm))))


  #|proc:hashtable->fxtreemap
  Return a new fixnum treemap containing the entries of hashtable `table`.
  Equality predicate `equal?` and ordering predicate `less?` each take two fixnum keys.
  All keys and values in `table` must be fixnums.
  |#
  (define hashtable->fxtreemap
    (lambda (equal? less? table)
      (pcheck ([procedure? equal? less?] [hashtable? table])
              (let ([result (make-fxtreemap equal? less?)])
                (vector-for-each
                  (lambda (cell) (fxtreemap-set! result (car cell) (cdr cell)))
                  (hashtable-cells table))
                result))))

  (define write-treemap
                 (lambda (r p wr)
                   (display "#[treemap (" p)
                   (if (fx= 0 (rbtree-size r))
                       (display ")]" p)
                       (begin
                         (let ([n (rbtree-size r)] [i 0])
                           (rbtree-visit 'treemap-writer
                                         (lambda (k v)
                                           (if (fx= i (fx1- n))
                                               (wr (cons k v) p)
                                               (begin
                                                 (wr (cons k v) p)
                                                 (display " " p)))
                                           (set! i (fx1+ i)))
                                         r)
                           (display ")]" p))))))
;;;;===----------------------------------------------------------------------===
;;;; Iterator extension registration
;;;;===----------------------------------------------------------------------===

  (iter-register-source!
   treemap?
   (lambda (tm)
     (let ([cursor (rbtree-inorder-cursor tm)])
       (make-iter
        (lambda ()
          (call-with-values cursor
            (lambda (key value)
              (if (eq? key *dummy-v*) iter-end (cons key value)))))
        (lambda () (set! cursor (rbtree-inorder-cursor tm)))))))

;;;;===----------------------------------------------------------------------===
;;;; Navigator extension registration
;;;;===----------------------------------------------------------------------===

  ;; Final adapter callbacks provide a fresh map with the same comparison policy,
  ;; canonical stored-key lookup, and complete mutable membership replacement.
  ;; Clearing before reinsertion avoids searching an old tree whose key ordering
  ;; may already have changed through nested mutation; entry order decides collisions.
  (nav-register-keyed!
   treemap?
   (lambda (tm key default)
     (if (treemap-contains? tm key) (treemap-ref tm key) default))
   (lambda (tm key value)
     (let ([copy (treemap-map (lambda (old-key old-value)
                                (values old-key old-value))
                              tm)])
       (treemap-set! copy key value)
       copy))
   (lambda (tm key value)
     (treemap-set! tm key value)
     tm)
   (lambda (tm key)
     (let ([copy (treemap-map (lambda (old-key old-value)
                                (values old-key old-value))
                              tm)])
       (treemap-delete! copy key)
       copy))
   (lambda (tm key)
     (treemap-delete! tm key)
     tm)
   treemap->list
   (lambda (tm) (make-treemap-like tm))
   rbtree-ref-key
   (lambda (tm entries)
     (treemap-clear! tm)
     (for-each (lambda (entry) (treemap-set! tm (car entry) (cdr entry))) entries)))

  (iter-register-source!
   fxtreemap?
   (lambda (tm)
     (let ([cursor (rbtree-inorder-cursor tm)])
       (make-iter
        (lambda ()
          (call-with-values cursor
            (lambda (key value)
              (if (eq? key *dummy-v*) iter-end (cons key value)))))
        (lambda () (set! cursor (rbtree-inorder-cursor tm)))))))

  (nav-register-keyed!
   fxtreemap?
   (lambda (tm key default)
     (if (fxtreemap-contains? tm key) (fxtreemap-ref tm key) default))
   (lambda (tm key value)
     (let ([copy (fxtreemap-map (lambda (old-key old-value)
                                  (values old-key old-value))
                                tm)])
       (fxtreemap-set! copy key value)
       copy))
   (lambda (tm key value)
     (fxtreemap-set! tm key value)
     tm)
   (lambda (tm key)
     (let ([copy (fxtreemap-map (lambda (old-key old-value)
                                  (values old-key old-value))
                                tm)])
       (fxtreemap-delete! copy key)
       copy))
   (lambda (tm key)
     (fxtreemap-delete! tm key)
     tm)
   fxtreemap->list
   (lambda (tm) (make-fxtreemap-like tm))
   rbtree-ref-key
   (lambda (tm entries)
     (fxtreemap-clear! tm)
     (for-each (lambda (entry) (fxtreemap-set! tm (car entry) (cdr entry))) entries)))

  (record-writer (type-descriptor $treemap) write-treemap)
  (record-writer (type-descriptor $fxtreemap) write-treemap)

  )
