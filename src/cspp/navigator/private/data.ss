(library (cspp navigator private data)
  (export byte? indexed-length indexed-ref/missing indexed-set indexed-set!
          indexed-supported? indexed-walk indexed-transform indexed-transform!
          indexed-clear-values indexed-clear-values! index-list
          keyed-update-values keyed-update-values! keyed-rebuild-entries
          keyed-canonical-key
          keyed-transform-entries!
          child-container? emit-children
          keyed-supported? keyed-ref/missing keyed-set keyed-set!
          keyed-delete keyed-delete! keyed-entries keyed-keys keyed-values
          set-supported? set-values set-replace set-replace!
          set-delete set-delete!
          nav-register-indexed! nav-register-keyed! nav-register-set!
          alist? alist-ref/missing)
  (import (chezscheme)
          (cspp utils)
          (cspp navigator private core))

;;;;===----------------------------------------------------------------------===
;;;; Container policies and mutation boundaries
;;;;===----------------------------------------------------------------------===

  ;; A key policy is a map's equivalence, hashing, and/or ordering relation; a set's
  ;; membership policy has the same meaning for members. An index is the bucket or
  ;; ordered-tree position derived from that policy. Dependencies include object
  ;; contents, aliases, and mutable state captured by hash/comparison callbacks.
  ;; These dependencies must remain stable while indexed, except during explicitly
  ;; supported key/member operations that rebuild the traversed parent below.
  ;; Identity hashing/equality can remain stable while object contents change.
  ;;
  ;; Reindexing reinserts retained keys/members under their current policy. It can
  ;; merge newly equivalent items and does not undo mutations. Key/entry and set
  ;; replacement operations repair only the parent they explicitly traverse.
  ;; They do not discover other containers sharing a key, repair a key mutated
  ;; through an unrelated value alias, or repair read-only ancestors. Such changes
  ;; must preserve those other indexes or be managed by the caller. Exceptions and
  ;; continuation resumption provide no transaction rollback of heap updates.

  (define-record-type ($indexed-protocol make-$indexed-protocol $indexed-protocol?)
    (fields (immutable predicate)
            (immutable length-proc)
            (immutable ref-proc)
            (immutable set-proc)
            (immutable set!-proc)
            (immutable walk-proc)
            (immutable transform-proc)
            (immutable transform!-proc))
    (sealed #t))

  (define-record-type ($keyed-protocol make-$keyed-protocol $keyed-protocol?)
    (fields (immutable predicate)
            (immutable ref-proc)
            (immutable set-proc)
            (immutable set!-proc)
            (immutable delete-proc)
            (immutable delete!-proc)
            (immutable entries-proc)
            (immutable empty-proc)
            (immutable canonical-proc)
            (immutable replace!-proc))
    (sealed #t))

  (define-record-type ($set-protocol make-$set-protocol $set-protocol?)
    (fields (immutable predicate)
            (immutable values-proc)
            (immutable replace-proc)
            (immutable replace!-proc)
            (immutable delete-proc)
            (immutable delete!-proc))
    (sealed #t))

  ;; A registry snapshot is an immutable newest-first protocol list. Readers load
  ;; once, acquire its publication, then invoke user predicates without locks.
  ;; Publishers construct an immutable entry before a release/CAS retry loop; a
  ;; competing publication cannot lose an accepted registration. CAS retries may
  ;; allocate list cells but never rerun user callbacks.
  (define indexed-protocols (box '()))
  (define keyed-protocols (box '()))
  (define set-protocols (box '()))

  (define registry-snapshot
    (lambda (registry)
      (let ([snapshot (unbox registry)])
        (memory-order-acquire)
        snapshot)))

  (define register-protocol!
    (lambda (registry protocol)
      (let retry ()
        (let* ([snapshot (registry-snapshot registry)]
               [replacement (cons protocol snapshot)])
          (memory-order-release)
          (unless (box-cas! registry snapshot replacement)
            (retry))))))

  (define byte?
    (lambda (x)
      (and (fixnum? x) (fx<= 0 x) (fx<= x 255))))

  (define fixnum-value?
    (lambda (x)
      (fixnum? x)))

  (define flonum-value?
    (lambda (x)
      (flonum? x)))

  #|proc:nav-register-indexed!
  The `nav-register-indexed!` procedure registers an indexed container type for
  built-in indexed navigators. The `predicate` procedure receives a value and
  returns true for supported containers. The `length-proc` procedure receives a
  container and returns a non-negative integer length no larger than
  `(most-positive-fixnum)`. Integer-valued inexact lengths are normalized to exact integers.
  The `ref-proc` procedure receives a container and zero-based index and returns the indexed value.
  The `set-proc` procedure receives a container, index, and new value, and returns a rebuilt
  container. The `set!-proc` procedure receives a container, index, and new
  value, mutates the container, and returns a value.
  Optional `walk-proc` has signature `(container start stop step visit) -> unspecified`.
  It visits the half-open index range by positive stride, calling `(visit index value)`
  synchronously in increasing order. Range bounds are clipped to the container length;
  an empty range performs no callbacks. Selection need not allocate a list of all indexes.
  Optional `transform-proc` / `transform!-proc` have signature `(container indexes update)`;
  `update` accepts one value and returns its replacement. The pure hook returns a rebuilt
  container without modifying the original; the mutating hook updates the original and its
  return value is ignored. Hooks visit each selected index once, in order, and may suspend.
  Update `indexes` are increasing valid exact indexes; walk bounds/stride are exact integers.
  Bulk hooks preserve shape, element restrictions, callback order and shallow sharing of
  unchanged children. Changing indexed length or structure inside callbacks is unspecified.
  Allocate traversal state per invocation; never cache a shared mutable cursor in a hook.
  Missing hooks (`#f`) use indexed access/pure setters as before. Bulk pure updates through
  legacy setters may copy once per element. Existing five-argument registrations remain valid.
  Registration returns an unspecified value.
  Newer registrations are tried first. Concurrent publications are retained atomically;
  one lookup uses one stable registry snapshot and runs callbacks outside synchronization.
  Overlapping predicates are allowed; callbacks must obey the caller's data-sharing rules.
  Kind: Adapter registration.
  |#
  (define nav-register-indexed!
    (case-lambda
      [(predicate length-proc ref-proc set-proc set!-proc)
       (nav-register-indexed! predicate length-proc ref-proc set-proc set!-proc #f #f #f)]
      [(predicate length-proc ref-proc set-proc set!-proc walk-proc)
       (nav-register-indexed! predicate length-proc ref-proc set-proc set!-proc walk-proc #f #f)]
      [(predicate length-proc ref-proc set-proc set!-proc walk-proc transform-proc)
       (nav-register-indexed! predicate length-proc ref-proc set-proc set!-proc
                              walk-proc transform-proc #f)]
      [(predicate length-proc ref-proc set-proc set!-proc walk-proc transform-proc transform!-proc)
       (pcheck ([procedure? predicate length-proc ref-proc set-proc set!-proc])
               (for-each (lambda (hook)
                           (unless (or (not hook) (procedure? hook))
                             (nav-error 'nav-register-indexed! "expected bulk hook or #f: ~s" hook)))
                         (list walk-proc transform-proc transform!-proc))
               (register-protocol! indexed-protocols
                                   (make-$indexed-protocol
                                    (nav-protect-callback predicate)
                                    (nav-protect-callback length-proc)
                                    (nav-protect-callback ref-proc)
                                    (nav-protect-callback set-proc)
                                    (nav-protect-callback set!-proc)
                                    (and walk-proc (nav-protect-callback walk-proc))
                                    (and transform-proc (nav-protect-callback transform-proc))
                                    (and transform!-proc (nav-protect-callback transform!-proc))))
               (void))]))

  #|proc:nav-register-keyed!
  The `nav-register-keyed!` procedure registers a key-value map type for keyed
  navigators. The `predicate` procedure receives a value and returns true for
  supported maps. The `ref-proc` procedure receives a map, key, and default
  value, and returns either the associated value or the default. The `set-proc`
  procedure receives a map, key, and new value, and returns a rebuilt map. The
  `set!-proc` procedure receives a map, key, and new value, mutates the map, and
  returns a value. The `delete-proc` procedure receives a map and key and
  returns a rebuilt map without that key. The `delete!-proc` procedure receives
  a map and key, mutates the map, and returns a value. The `entries-proc`
  procedure receives a map and returns an association list of `(key . value)`
  entries in traversal order. Optional `empty-proc` receives a map and returns a fresh,
  mutable empty map with the same key policy. It enables efficient bulk rebuilding and
  submaps; without it, rebuilding uses the pure delete and set callbacks.
  Optional `canonical-proc` has signature `(map key default) -> stored-key-or-default`.
  It returns the stored key equivalent to `key`, preserving its identity, or `default`
  when absent. Without it, canonical lookup uses pure deletion and an entry scan.
  Optional `replace!-proc` has signature `(map entries) -> unspecified`; it replaces
  all membership from the supplied entry list and rebuilds indexes even if the old
  keys have mutated. This supports clearing inside mutable keys of the writable traversed
  parent without detaching them first. Read-only parents and other aliases are not repaired;
  their keys/members must retain stable hash/equivalence/order dependencies.
  The key policy is the container's equivalence, hashing, and/or ordering rules. A canonical
  key is the stored object matching a lookup key under that policy. The replacement entry
  list is complete membership, not a list of edits; process it in order so later entries
  win equivalent-key collisions. Existing seven- and eight-argument registrations remain valid.
  Registration returns an unspecified value.
  Newer registrations are tried first. Concurrent publications are retained atomically;
  one lookup uses one stable registry snapshot and runs callbacks outside synchronization.
  Overlapping predicates are allowed; callbacks must obey the caller's data-sharing rules.
  Kind: Adapter registration.
  |#
  (define nav-register-keyed!
    (case-lambda
      [(predicate ref-proc set-proc set!-proc delete-proc delete!-proc entries-proc)
       (nav-register-keyed! predicate ref-proc set-proc set!-proc delete-proc delete!-proc
                            entries-proc #f #f)]
      [(predicate ref-proc set-proc set!-proc delete-proc delete!-proc entries-proc empty-proc)
       (nav-register-keyed! predicate ref-proc set-proc set!-proc delete-proc delete!-proc
                            entries-proc empty-proc #f)]
      [(predicate ref-proc set-proc set!-proc delete-proc delete!-proc entries-proc empty-proc
                  canonical-proc)
       (nav-register-keyed! predicate ref-proc set-proc set!-proc delete-proc delete!-proc
                            entries-proc empty-proc canonical-proc #f)]
      [(predicate ref-proc set-proc set!-proc delete-proc delete!-proc entries-proc empty-proc
                  canonical-proc replace!-proc)
       (pcheck ([procedure? predicate ref-proc set-proc set!-proc
                            delete-proc delete!-proc entries-proc])
               (unless (or (not empty-proc) (procedure? empty-proc))
                 (nav-error 'nav-register-keyed! "expected empty-map procedure: ~s" empty-proc))
               (unless (or (not canonical-proc) (procedure? canonical-proc))
                 (nav-error 'nav-register-keyed!
                            "expected canonical-key procedure: ~s" canonical-proc))
               (unless (or (not replace!-proc) (procedure? replace!-proc))
                 (nav-error 'nav-register-keyed!
                            "expected replacement procedure: ~s" replace!-proc))
               (register-protocol! keyed-protocols
                                   (make-$keyed-protocol
                                    (nav-protect-callback predicate)
                                    (nav-protect-callback ref-proc)
                                    (nav-protect-callback set-proc)
                                    (nav-protect-callback set!-proc)
                                    (nav-protect-callback delete-proc)
                                    (nav-protect-callback delete!-proc)
                                    (nav-protect-callback entries-proc)
                                    (and empty-proc (nav-protect-callback empty-proc))
                                    (and canonical-proc (nav-protect-callback canonical-proc))
                                    (and replace!-proc (nav-protect-callback replace!-proc))))
               (void))]))

  #|proc:nav-register-set!
  Register callbacks for a set-like container. `predicate` recognizes the container.
  `values-proc` returns its members in traversal order. The replace callbacks receive
  a set and a list containing its complete replacement membership. The delete callbacks
  receive a set and one member.
  Pure callbacks return rebuilt sets. Mutating callbacks mutate their set and may return
  any value; navigator mutation still returns the original set object.
  Signatures are `(value) -> boolean`, `(set) -> member-list`,
  `(set member-list) -> rebuilt-set` / unspecified, and `(set member) -> rebuilt-set` /
  unspecified, respectively. Registration itself returns an unspecified value.
  Newer registrations are tried first. Concurrent publications are retained atomically;
  one lookup uses one stable registry snapshot and runs callbacks outside synchronization.
  Overlapping predicates are allowed; callbacks must obey the caller's data-sharing rules.
  Kind: Adapter registration.
  |#
  (define nav-register-set!
    (lambda (predicate values-proc replace-proc replace!-proc delete-proc delete!-proc)
      (pcheck ([procedure? predicate values-proc replace-proc replace!-proc
                           delete-proc delete!-proc])
              (register-protocol! set-protocols
                                  (make-$set-protocol
                                   (nav-protect-callback predicate)
                                   (nav-protect-callback values-proc)
                                   (nav-protect-callback replace-proc)
                                   (nav-protect-callback replace!-proc)
                                   (nav-protect-callback delete-proc)
                                   (nav-protect-callback delete!-proc)))
              (void))))

  (define find-indexed-protocol
    (lambda (value)
      (let loop ([protocols (registry-snapshot indexed-protocols)])
        (cond [(null? protocols) #f]
              [(($indexed-protocol-predicate (car protocols)) value) (car protocols)]
              [else (loop (cdr protocols))]))))

  ;; Native representations always keep precedence over registered adapters.
  (define find-indexed-extension
    (lambda (value)
      (and (not (or (list? value) (vector? value) (fxvector? value) (flvector? value)
                    (bytevector? value) (string? value)))
           (find-indexed-protocol value))))

  (define find-keyed-protocol
    (lambda (value)
      (let loop ([protocols (registry-snapshot keyed-protocols)])
        (cond [(null? protocols) #f]
              [(($keyed-protocol-predicate (car protocols)) value) (car protocols)]
              [else (loop (cdr protocols))]))))

  (define find-set-protocol
    (lambda (value)
      (let loop ([protocols (registry-snapshot set-protocols)])
        (cond [(null? protocols) #f]
              [(($set-protocol-predicate (car protocols)) value) (car protocols)]
              [else (loop (cdr protocols))]))))

  (define protocol-indexed-length
    (lambda (protocol value)
      (let ([length (($indexed-protocol-length-proc protocol) value)])
        (unless (integer? length)
          (nav-error 'nav-register-indexed! "invalid indexed length: ~s" length))
        (let ([length (inexact->exact length)])
          (unless (and (fixnum? length) (fx>= length 0))
            (nav-error 'nav-register-indexed! "invalid indexed length: ~s" length))
          length))))

  (define indexed-length
    (lambda (value)
      (cond [(list? value) (length value)]
            [(vector? value) (vector-length value)]
            [(fxvector? value) (fxvector-length value)]
            [(flvector? value) (flvector-length value)]
            [(string? value) (string-length value)]
            [(bytevector? value) (bytevector-length value)]
            [(find-indexed-protocol value)
             => (lambda (protocol)
                  (protocol-indexed-length protocol value))]
            [else #f])))

  (define indexed-supported?
    (lambda (value)
      (and (indexed-length value) #t)))

  ;; Recursive traversal and terminal selection share the same container dispatch.
  (define child-container?
    (lambda (value)
      (cond [(list? value) (pair? value)]
            [(pair? value) #f]
            [else (or (indexed-supported? value) (keyed-supported? value))])))

  (define emit-children
    (lambda (value emit)
      (cond [(list? value) (for-each emit value)]
            [(pair? value) (void)]
            [(indexed-supported? value)
             (indexed-walk value 0 (indexed-length value) 1
                           (lambda (index child) (emit child)))]
            [(keyed-values value)
             => (lambda (values) (for-each emit values))]
            [else (void)])))

  (define indexed-ref/missing
    (lambda (value index)
      (cond [(list? value)
             (if (< index (length value)) (list-ref value index) nav-missing)]
            [(vector? value)
             (if (< index (vector-length value)) (vector-ref value index) nav-missing)]
            [(fxvector? value)
             (if (< index (fxvector-length value)) (fxvector-ref value index) nav-missing)]
            [(flvector? value)
             (if (< index (flvector-length value)) (flvector-ref value index) nav-missing)]
            [(string? value)
             (if (< index (string-length value)) (string-ref value index) nav-missing)]
            [(bytevector? value)
             (if (< index (bytevector-length value)) (bytevector-u8-ref value index) nav-missing)]
            [(find-indexed-protocol value)
             => (lambda (protocol)
                  (if (< index (protocol-indexed-length protocol value))
                      (($indexed-protocol-ref-proc protocol) value index)
                      nav-missing))]
            [else (nav-focus-error 'nav/nth "unsupported indexed value: ~s" value)])))

  (define index-list
    (lambda (len)
      (let loop ([i 0] [indexes '()])
        (if (fx= i len)
            (reverse indexes)
            (loop (fx+ i 1) (cons i indexes))))))

  ;; Bulk selections use ranges so an early-stopping consumer need not allocate
  ;; every index first. Lists/sequential adapters walk their spine once; callbacks
  ;; retain invocation-local progress across ordinary suspension.
  (define indexed-walk
    (lambda (value start stop step visit)
      (unless (>= start stop)
        (if (list? value)
            (let loop ([items value] [index 0] [next start])
              (when (< index stop)
                (if (= index next)
                    (begin (visit index (car items))
                           (loop (cdr items) (fx1+ index) (+ next step)))
                    (loop (cdr items) (fx1+ index) next))))
            (let* ([protocol (find-indexed-extension value)]
                   [walk (and protocol ($indexed-protocol-walk-proc protocol))])
              (if walk
                  (walk value start stop step visit)
                  (let loop ([index start])
                    (when (< index stop)
                      (visit index (indexed-ref/missing value index))
                      (loop (+ index step))))))))))

  ;; A registered bulk hook avoids repeatedly cloning or seeking through the
  ;; same container. Native storage continues to use its specialized fallback.
  (define indexed-transform
    (lambda (value indexes update)
      (if (null? indexes)
          value
          (let* ([protocol (find-indexed-extension value)]
                 [transform (and protocol ($indexed-protocol-transform-proc protocol))])
            (if transform (transform value indexes update)
                (indexed-transform/fallback value indexes update))))))

  (define indexed-transform!
    (lambda (value indexes update!)
      (unless (null? indexes)
        (let* ([protocol (find-indexed-extension value)]
               [transform! (and protocol ($indexed-protocol-transform!-proc protocol))])
          (if transform! (transform! value indexes update!)
              (indexed-transform!/fallback value indexes update!))))
      value))

  ;; Index lists are increasing. Walk a list spine once instead of seeking each index.
  (define indexed-transform!/fallback
    (lambda (value indexes update!)
      (if (list? value)
          (let loop ([items value] [indexes indexes] [index 0])
            (cond [(null? indexes) value]
                  [(null? items)
                   (nav-focus-error 'nav-transform! "index ~s out of range for: ~s"
                                    (car indexes) value)]
                  [(= index (car indexes))
                   (set-car! items (update! (car items)))
                   (loop (cdr items) (cdr indexes) (fx+ index 1))]
                  [else (loop (cdr items) indexes (fx+ index 1))]))
          (begin
            (for-each
             (lambda (index)
               (let ([old (indexed-ref/missing value index)])
                 (when (nav-missing? old)
                   (nav-focus-error 'nav-transform! "index ~s out of range for: ~s" index value))
                 (indexed-set! value index (update! old))))
             indexes)
            value))))

  (define require-retained-child
    (lambda (who value child)
      (when (nav-missing? child)
        (nav-error who "cannot remove indexed child from: ~s" value))
      child))

  (define indexed-clear-values
    (lambda (value indexes clear)
      (if (list? value)
          (let loop ([items value] [indexes indexes] [index 0])
            (cond [(null? indexes) items]
                  [(null? items)
                   (nav-focus-error 'nav-clearval "index ~s out of range for: ~s"
                                    (car indexes) value)]
                  [(= index (car indexes))
                   (let ([child (clear (car items))])
                     (if (nav-missing? child)
                         (loop (cdr items) (cdr indexes) (fx+ index 1))
                         (cons child (loop (cdr items) (cdr indexes) (fx+ index 1)))))]
                  [else (cons (car items) (loop (cdr items) indexes (fx+ index 1)))]))
          (indexed-transform value indexes
                             (lambda (old)
                               (require-retained-child 'nav-clearval value (clear old)))))))

  (define indexed-clear-values!
    (lambda (value indexes clear!)
      (indexed-transform! value indexes
                          (lambda (old)
                            (require-retained-child 'nav-clearval! value (clear! old))))))

  ;; Built-in containers need one copy per pure update. Registered protocols retain
  ;; their pure setter contract because a generic container has no cloning operation.
  (define indexed-transform/fallback
    (lambda (value indexes update)
      (cond [(null? indexes) value]
            [(list? value)
             (let loop ([items value] [indexes indexes] [index 0])
               (cond [(null? indexes) items]
                     [(null? items)
                      (nav-focus-error 'nav-transform "index ~s out of range for: ~s"
                                       (car indexes) value)]
                     [(= index (car indexes))
                      (let ([child (update (car items))])
                        (cons child (loop (cdr items) (cdr indexes) (fx+ index 1))))]
                     [else (cons (car items) (loop (cdr items) indexes (fx+ index 1)))]))]
            [else
             (let ([copy (cond [(vector? value) (vector-copy value)]
                               [(fxvector? value) (fxvector-copy value)]
                               [(flvector? value) (flvector-copy value)]
                               [(bytevector? value) (bytevector-copy value)]
                               [(string? value) (string-copy value)]
                               [else #f])])
               (let loop ([indexes indexes] [result (or copy value)])
                 (if (null? indexes)
                     result
                     (let* ([index (car indexes)]
                            [old (indexed-ref/missing result index)])
                       (when (nav-missing? old)
                         (nav-focus-error 'nav-transform "index ~s out of range for: ~s"
                                          index value))
                       (let ([child (update old)])
                         (if copy
                             (begin (indexed-set! copy index child)
                                    (loop (cdr indexes) copy))
                             (loop (cdr indexes) (indexed-set result index child))))))))])))

  (define indexed-set
    (lambda (value index new-value)
      (cond [(list? value)
             (let loop ([xs value] [i index])
               (cond [(null? xs)
                      (nav-focus-error 'nav-transform "index ~s out of range for: ~s" index value)]
                     [(= i 0) (cons new-value (cdr xs))]
                     [else (cons (car xs) (loop (cdr xs) (- i 1)))]))]
            [(vector? value)
             (let ([copy (vector-copy value)])
               (vector-set! copy index new-value)
               copy)]
            [(fxvector? value)
             (if (fixnum-value? new-value)
                 (let ([copy (fxvector-copy value)])
                   (fxvector-set! copy index new-value)
                   copy)
                 (nav-error 'nav-transform
                            "fxvector element is not a fixnum: ~s"
                            new-value))]
            [(flvector? value)
             (if (flonum-value? new-value)
                 (let ([copy (flvector-copy value)])
                   (flvector-set! copy index new-value)
                   copy)
                 (nav-error 'nav-transform
                            "flvector element is not a flonum: ~s"
                            new-value))]
            [(string? value)
             (if (char? new-value)
                 (let ([copy (string-copy value)])
                   (string-set! copy index new-value)
                   copy)
                 (nav-error 'nav-transform
                            "string element is not a char: ~s"
                            new-value))]
            [(bytevector? value)
             (if (byte? new-value)
                 (let ([copy (bytevector-copy value)])
                   (bytevector-u8-set! copy index new-value)
                   copy)
                 (nav-error 'nav-transform
                            "bytevector element is not a byte: ~s"
                            new-value))]
            [(find-indexed-protocol value)
             => (lambda (protocol)
                  (($indexed-protocol-set-proc protocol) value index new-value))]
            [else (nav-focus-error 'nav-transform "unsupported indexed value: ~s" value)])))

  (define indexed-set!
    (lambda (value index new-value)
      (cond [(list? value)
             (let loop ([xs value] [i index])
               (cond [(null? xs)
                      (nav-focus-error 'nav-transform! "index ~s out of range for: ~s" index value)]
                     [(= i 0) (set-car! xs new-value)]
                     [else (loop (cdr xs) (- i 1))]))]
            [(vector? value)
             (unless (eq? (vector-ref value index) new-value)
               (unless (mutable-vector? value)
                 (nav-error 'nav-transform! "vector is immutable: ~s" value))
               (vector-set! value index new-value))]
            [(fxvector? value)
             (if (fixnum-value? new-value)
                 (fxvector-set! value index new-value)
                 (nav-error 'nav-transform!
                            "fxvector element is not a fixnum: ~s"
                            new-value))]
            [(flvector? value)
             (if (flonum-value? new-value)
                 (flvector-set! value index new-value)
                 (nav-error 'nav-transform!
                            "flvector element is not a flonum: ~s"
                            new-value))]
            [(string? value)
             (if (char? new-value)
                 (unless (char=? (string-ref value index) new-value)
                   (unless (mutable-string? value)
                     (nav-error 'nav-transform! "string is immutable: ~s" value))
                   (string-set! value index new-value))
                 (nav-error 'nav-transform!
                            "string element is not a char: ~s"
                            new-value))]
            [(bytevector? value)
             (if (byte? new-value)
                 (unless (fx= (bytevector-u8-ref value index) new-value)
                   (unless (mutable-bytevector? value)
                     (nav-error 'nav-transform! "bytevector is immutable: ~s" value))
                   (bytevector-u8-set! value index new-value))
                 (nav-error 'nav-transform!
                            "bytevector element is not a byte: ~s"
                            new-value))]
            [(find-indexed-protocol value)
             => (lambda (protocol)
                  (($indexed-protocol-set!-proc protocol) value index new-value))]
            [else (nav-focus-error 'nav-transform! "unsupported indexed value: ~s" value)])))

  (define alist?
    (lambda (value)
      (and (list? value) (andmap pair? value))))

  (define alist-ref/missing
    (lambda (alist key)
      (let ([entry (assq key alist)])
        (if entry (cdr entry) nav-missing))))

  (define hashtable->alist
    (lambda (value)
      (let-values ([(keys vals) (hashtable-entries value)])
        (let ([len (vector-length keys)])
          (let loop ([i 0] [entries '()])
            (if (= i len)
                (reverse entries)
                (loop (+ i 1)
                      (cons (cons (vector-ref keys i) (vector-ref vals i))
                            entries))))))))

  (define keyed-supported?
    (lambda (value)
      (or (hashtable? value) (alist? value) (and (find-keyed-protocol value) #t))))

  (define keyed-ref/missing
    (lambda (value key)
      (cond [(hashtable? value) (nav-call-user hashtable-ref value key nav-missing)]
            [(alist? value) (alist-ref/missing value key)]
            [(find-keyed-protocol value)
             => (lambda (protocol)
                  (($keyed-protocol-ref-proc protocol) value key nav-missing))]
            [else (nav-focus-error 'nav/key "unsupported keyed value: ~s" value)])))

  ;; Return the stored representative, not merely the query key. For legacy
  ;; adapters there is no exported equivalence predicate: pure-delete the query
  ;; from a copy, then find which original key is absent there. This derives the
  ;; policy from existing callbacks; the optional canonical callback avoids that cost.
  (define keyed-canonical-key
    (lambda (value key)
      (cond [(hashtable? value)
             (if (nav-call-user hashtable-contains? value key)
                 (if (hashtable-mutable? value)
                     (car (nav-call-user hashtable-cell value key nav-missing))
                     (let ([same? (hashtable-equivalence-function value)])
                       (let loop ([keys (vector->list (hashtable-keys value))])
                         (cond [(null? keys) nav-missing]
                               [(nav-call-user same? key (car keys)) (car keys)]
                               [else (loop (cdr keys))]))))
                 nav-missing)]
            [(alist? value)
             (let ([entry (assq key value)])
               (if entry (car entry) nav-missing))]
            [(find-keyed-protocol value)
             => (lambda (protocol)
                  (let ([proc ($keyed-protocol-canonical-proc protocol)])
                    (if proc
                        (proc value key nav-missing)
                        (if (nav-missing? (keyed-ref/missing value key))
                            nav-missing
                            (let* ([without (keyed-delete value key)]
                                   [entry (find (lambda (entry)
                                                  (nav-missing?
                                                   (keyed-ref/missing without (car entry))))
                                                (keyed-entries value))])
                              (if entry (car entry) nav-missing))))))]
            [else nav-missing])))

  (define keyed-set
    (lambda (value key new-value)
      (cond [(hashtable? value)
             (let ([copy (nav-call-user hashtable-copy value #t)])
               (nav-call-user hashtable-set! copy key new-value)
               copy)]
            [(alist? value)
             (let loop ([xs value])
               (cond [(null? xs) (list (cons key new-value))]
                     [(eq? (caar xs) key) (cons (cons key new-value) (cdr xs))]
                     [else (cons (car xs) (loop (cdr xs)))]))]
            [(find-keyed-protocol value)
             => (lambda (protocol)
                  (($keyed-protocol-set-proc protocol) value key new-value))]
            [else (nav-focus-error 'nav-transform "unsupported keyed value: ~s" value)])))

  (define keyed-absent-token (list 'keyed-absent))

  (define keyed-set!
    (lambda (value key new-value)
      (cond [(hashtable? value)
             (unless (eq? (nav-call-user hashtable-ref value key keyed-absent-token) new-value)
               (unless (hashtable-mutable? value)
                 (nav-error 'nav-transform! "hashtable is immutable: ~s" value))
               (nav-call-user hashtable-set! value key new-value))
             value]
            [(alist? value)
             (let loop ([xs value])
               (cond [(null? xs)
                      (nav-focus-error 'nav-transform! "missing key ~s in: ~s" key value)]
                     [(eq? (caar xs) key)
                      (set-cdr! (car xs) new-value)
                      value]
                     [else (loop (cdr xs))]))]
            [(find-keyed-protocol value)
             => (lambda (protocol)
                  (($keyed-protocol-set!-proc protocol) value key new-value)
                  value)]
            [else (nav-focus-error 'nav-transform! "unsupported keyed value: ~s" value)])))

  (define alist-remove-key
    (lambda (key value)
      (let loop ([xs value])
        (cond [(null? xs) '()]
              [(eq? (caar xs) key) (cdr xs)]
              [else (cons (car xs) (loop (cdr xs)))]))))

  (define keyed-delete
    (lambda (value key)
      (cond [(hashtable? value)
             (let ([copy (nav-call-user hashtable-copy value #t)])
               (nav-call-user hashtable-delete! copy key)
               copy)]
            [(alist? value) (alist-remove-key key value)]
            [(find-keyed-protocol value)
             => (lambda (protocol)
                  (($keyed-protocol-delete-proc protocol) value key))]
            [else (nav-focus-error 'nav-clearval "unsupported keyed value: ~s" value)])))

  (define keyed-delete!
    (lambda (value key)
      (cond [(hashtable? value)
             (nav-call-user hashtable-delete! value key)
             value]
            [(alist? value)
             (nav-error 'nav-clearval! "association list key cannot be removed in place: ~s" key)]
            [(find-keyed-protocol value)
             => (lambda (protocol)
                  (($keyed-protocol-delete!-proc protocol) value key)
                  value)]
            [else (nav-focus-error 'nav-clearval! "unsupported keyed value: ~s" value)])))

  (define keyed-entries
    (lambda (value)
      (cond [(hashtable? value) (hashtable->alist value)]
            [(alist? value) value]
            [(find-keyed-protocol value)
             => (lambda (protocol)
                  (($keyed-protocol-entries-proc protocol) value))]
            [else #f])))

  ;; Rebuilding belongs to the container protocol, so submaps and entry updates
  ;; preserve key equivalence without depending on the concrete map implementation.
  ;; `entries` is complete replacement membership in collision-resolution order.
  ;; An empty builder plus mutable inserts avoids repeated whole-container copies;
  ;; legacy adapters fall back to their existing pure delete/set callbacks.
  (define keyed-rebuild-entries
    (lambda (value entries)
      (cond [(hashtable? value)
             (let* ([equivalent? (hashtable-equivalence-function value)]
                    [result (cond [(eq? equivalent? eq?) (make-eq-hashtable)]
                                  [(eq? equivalent? eqv?) (make-eqv-hashtable)]
                                  [else (make-hashtable (hashtable-hash-function value)
                                                        equivalent?)])])
               (for-each (lambda (entry) (keyed-set! result (car entry) (cdr entry))) entries)
               result)]
            [(alist? value) entries]
            [(find-keyed-protocol value)
             => (lambda (protocol)
                  (let ([empty-proc ($keyed-protocol-empty-proc protocol)])
                    (if empty-proc
                        (let ([result (empty-proc value)])
                          (for-each (lambda (entry)
                                      (($keyed-protocol-set!-proc protocol)
                                       result (car entry) (cdr entry))) entries)
                          result)
                        (let ([empty (fold-left
                                      (lambda (result entry)
                                        (keyed-delete result (car entry)))
                                      value (keyed-entries value))])
                          (fold-left (lambda (result entry)
                                       (keyed-set result (car entry) (cdr entry)))
                                     empty entries)))))]
            [else (nav-focus-error 'nav-transform "unsupported keyed value: ~s" value)])))

  ;; Native maps copy once. Registered maps can opt into one fresh mutable builder;
  ;; the original pure-setter protocol remains the fallback for existing adapters.
  (define keyed-update-values
    (lambda (value update)
      (let* ([entries (keyed-entries value)]
             [protocol (and (not (or (hashtable? value) (alist? value)))
                            (find-keyed-protocol value))]
             [empty-proc (and protocol ($keyed-protocol-empty-proc protocol))])
        (cond [(or (hashtable? value) empty-proc)
               (let ([result (if empty-proc (empty-proc value)
                                 (nav-call-user hashtable-copy value #t))])
                 (for-each
                  (lambda (entry)
                    (let ([child (update (cdr entry))])
                      (if (nav-missing? child)
                          (unless empty-proc (keyed-delete! result (car entry)))
                          (keyed-set! result (car entry) child))))
                  entries)
                 result)]
              [else
               (fold-left (lambda (result entry)
                            (let ([child (update (cdr entry))])
                              (if (nav-missing? child)
                                  (keyed-delete result (car entry))
                                  (keyed-set result (car entry) child))))
                          value entries)]))))

  (define keyed-update-values!
    (lambda (value update!)
      (for-each
       (lambda (entry)
         (let ([child (update! (cdr entry))])
           (if (nav-missing? child)
               (keyed-delete! value (car entry))
               (keyed-set! value (car entry) child))))
       (keyed-entries value))
      value))

  ;; Entry/key suffixes may mutate hash or ordering fields in a retained key.
  ;; Work from a snapshot and rebuild membership after completion or a suffix exception.
  ;; A nonlocal exit leaves state undefined. Ordinary suspension is not completion:
  ;; never rebuild in a dynamic-wind after-thunk, which runs during fiber suspension
  ;; and can itself invoke a yielding hash/comparison callback while unwinding.
  ;; Legacy adapters detach first because their delete callback needs the old index.
  ;; Reindexing means inserting retained keys using their current hash/order; eq?
  ;; identity alone cannot prove an old bucket or tree position is still correct.
  ;; Snapshot pairs are copied, but keys/values are shared so nested mutation works.
  ;; Rebuilding repairs membership, not a transaction rollback: earlier changes
  ;; remain, and current key equivalence can collapse formerly distinct entries.
  ;; Read-only hashtable ancestors bypass rebuilding; keyed-set! permits retained
  ;; identical values and rejects a replacement write. Alist entries are restored
  ;; into their original pairs. Basic nav/entries handles alists directly to expose
  ;; actual stored entry identity to suffix predicates.
  (define keyed-transform-entries!
    (lambda (value update!)
      (let* ([entries (map (lambda (entry) (cons (car entry) (cdr entry)))
                           (keyed-entries value))]
             [protocol (and (not (or (hashtable? value) (alist? value)))
                            (find-keyed-protocol value))]
             [replace! (and protocol ($keyed-protocol-replace!-proc protocol))]
             [readonly? (and (hashtable? value) (not (hashtable-mutable? value)))])
        (unless (or (hashtable? value) (alist? value) replace!)
          (for-each (lambda (entry) (keyed-delete! value (car entry))) entries))
        (let ([reraise
               ;; Return from the exception handler before rebuilding: repair callbacks
               ;; may suspend too. The thunk preserves even non-condition raised values.
               (guard (condition [else (lambda () (raise condition))])
                 (for-each
                  (lambda (old)
                    (let* ([key (car old)] [entry (update! (cons key (cdr old)))])
                      (unless (and (not (nav-missing? entry)) (pair? entry)
                                   (eq? key (car entry)))
                        (nav-error 'nav-clearval! "mutable clearing must retain entry key"))
                      (when readonly? (keyed-set! value key (cdr entry)))
                      (set-cdr! old (cdr entry))))
                  entries)
                 #f)])
          (cond [readonly? (void)]
                [(alist? value)
                 (for-each (lambda (old entry)
                             (set-car! old (car entry)) (set-cdr! old (cdr entry)))
                           value entries)]
                [replace! (replace! value entries)]
                [else
                 (when (hashtable? value) (hashtable-clear! value))
                 (for-each (lambda (entry)
                             (keyed-set! value (car entry) (cdr entry))) entries)])
          (when reraise (reraise)))
        value)))

  (define keyed-keys
    (lambda (value)
      (let ([entries (keyed-entries value)])
        (and entries (map car entries)))))

  (define keyed-values
    (lambda (value)
      (let ([entries (keyed-entries value)])
        (and entries (map cdr entries)))))

  (define set-supported?
    (lambda (value)
      (and (find-set-protocol value) #t)))

  (define set-values
    (lambda (value)
      (let ([protocol (find-set-protocol value)])
        (and protocol (($set-protocol-values-proc protocol) value)))))

  (define set-replace
    (lambda (value members)
      (let ([protocol (find-set-protocol value)])
        (if protocol
            (($set-protocol-replace-proc protocol) value members)
            (nav-error 'nav-transform "unsupported set value: ~s" value)))))

  (define set-replace!
    (lambda (value members)
      (let ([protocol (find-set-protocol value)])
        (if protocol
            (begin
              (($set-protocol-replace!-proc protocol) value members)
              value)
            (nav-error 'nav-transform! "unsupported set value: ~s" value)))))

  (define set-delete
    (lambda (value member)
      (let ([protocol (find-set-protocol value)])
        (if protocol
            (($set-protocol-delete-proc protocol) value member)
            (nav-error 'nav-clearval "unsupported set value: ~s" value)))))

  (define set-delete!
    (lambda (value member)
      (let ([protocol (find-set-protocol value)])
        (if protocol
            (begin
              (($set-protocol-delete!-proc protocol) value member)
              value)
            (nav-error 'nav-clearval! "unsupported set value: ~s" value)))))

  )
