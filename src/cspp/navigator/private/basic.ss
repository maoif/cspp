(library (cspp navigator private basic)
  (export nav/stay nav/none nav/all nav/values nav/keys nav/entries
          nav/nth nav/nth/default nav/first nav/second nav/last nav/slice
          nav/key nav/key/default nav/key-values nav/submap nav/car nav/cdr
          nav-register-indexed! nav-register-keyed! nav-register-set!)
  (import (chezscheme)
          (cspp utils)
          (cspp list)
          (cspp navigator private core)
          (cspp navigator private data))

  ;; Scheme map does not prescribe callback order. Navigation callbacks always
  ;; follow selection order, including indexed updates and stateful suffixes.
  ;; Preserve this order even when the result list itself would be identical:
  ;; nav-transform/i numbers visits, and callbacks may observe preceding updates.
  (define map-in-order
    (lambda (proc items)
      (let ([builder (make-list-builder)])
        (for-each (lambda (item) (builder (proc item))) items)
        (builder))))

  (define unsupported-transform!
    (lambda (who nav value)
      (nav-unsupported-error who nav value 'transform!)))

  (define make-basic-nav
    (lambda (name metadata select-proc transform-proc transform!-proc clear-proc clear!-proc)
      (letrec ([nav (make-$navigator name 'basic metadata
                                     select-proc
                                     transform-proc
                                     transform!-proc
                                     clear-proc
                                     clear!-proc)])
        nav)))

  (define unsupported-clear
    (lambda (who value)
      (nav-error who "focused value cannot be removed from: ~s" value)))

  ;; A suffix that selects no values returns without changing the operation's
  ;; update counter. Only a terminal invocation authorizes materializing a default.
  (define update-default-focus
    (lambda (default update absent present)
      (let* ([counter (current-update-count)]
             [before (if counter (car counter) 0)]
             [child (update default)])
        (if (= before (if counter (car counter) 0)) (absent) (present child)))))

  (define indexed-transform/default
    (lambda (value index default update)
      (let ([selected (indexed-ref/missing value index)])
        (cond [(not (nav-missing? selected))
               (indexed-transform value (list index) update)]
              [else
               (update-default-focus
                default update (lambda () value)
                (lambda (child)
                  (cond [(list? value)
                         (let loop ([xs value] [i 0])
                           (cond [(pair? xs) (cons (car xs) (loop (cdr xs) (+ i 1)))]
                                 [(= i index) (list child)]
                                 [else (cons default (loop '() (+ i 1)))]))]
                        [(vector? value)
                         (let ([copy (make-vector (+ index 1) default)]
                               [len (vector-length value)])
                           (vector-copy! value 0 copy 0 len)
                           (vector-set! copy index child)
                           copy)]
                        [else
                         (nav-error 'nav-transform "cannot update missing index ~s in: ~s"
                                    index value)])))]))))

  (define emit-indexed-values
    (lambda (value emit)
      (cond [(list? value) (for-each emit value)]
            [(indexed-supported? value)
             (let ([len (indexed-length value)])
               (let loop ([i 0])
                 (when (< i len)
                   (emit (indexed-ref/missing value i))
                   (loop (+ i 1)))))]
            [(keyed-values value)
             => (lambda (values) (for-each emit values))]
            [(set-values value)
             => (lambda (values) (for-each emit values))]
            [else (nav-focus-error 'nav/all "unsupported collection: ~s" value)])))

  (define transform-all-values
    (lambda (value update)
      (cond [(indexed-supported? value)
             (indexed-transform value (index-list (indexed-length value)) update)]
            [(keyed-supported? value) (keyed-update-values value update)]
            [(set-values value)
             => (lambda (values) (set-replace value (map-in-order update values)))]
            [else (nav-focus-error 'nav-transform "unsupported collection: ~s" value)])))

  (define transform-all-values!
    (lambda (value update!)
      (cond [(indexed-supported? value)
             (indexed-transform! value (index-list (indexed-length value)) update!)]
            [(keyed-supported? value) (keyed-update-values! value update!)]
            [(set-values value)
             => (lambda (values) (set-replace! value (map-in-order update! values)))]
            [else (nav-focus-error 'nav-transform! "unsupported collection: ~s" value)])))

  ;; A set's membership policy belongs to its replacement callback. Always rebuild
  ;; once: retained members may have mutated their hash or ordering in the suffix.
  (define clear-set-values
    (lambda (value members clear replace)
      (let loop ([remaining members] [retained '()])
        (if (null? remaining)
            (replace value (reverse retained))
            (let ([child (clear (car remaining))])
              (loop (cdr remaining)
                    (if (nav-missing? child) retained (cons child retained))))))))

  ;; Unsupported input is absence, handled by nav/maybe or nav/choice. Removing
  ;; a present fixed-size child is a different error, enforced by container helpers.
  (define clear-all-values
    (lambda (value clear)
      (cond [(indexed-supported? value)
             (indexed-clear-values value (index-list (indexed-length value)) clear)]
            [(keyed-supported? value) (keyed-update-values value clear)]
            [(set-values value)
             => (lambda (members) (clear-set-values value members clear set-replace))]
            [else (nav-focus-error 'nav-clearval "unsupported collection: ~s" value)])))

  (define clear-all-values!
    (lambda (value clear!)
      (cond [(indexed-supported? value)
             (indexed-clear-values! value (index-list (indexed-length value)) clear!)]
            [(keyed-supported? value) (keyed-update-values! value clear!)]
            [(set-values value)
             => (lambda (members) (clear-set-values value members clear! set-replace!))]
            [else (nav-focus-error 'nav-clearval! "unsupported collection: ~s" value)])))

  (define emit-alist-key-values
    (lambda (alist key emit)
      (let loop ([xs alist] [selected? #f])
        (cond [(null? xs)
               (unless selected?
                 (nav-focus-error 'nav/key "missing key ~s in: ~s" key alist))]
              [(eq? (caar xs) key)
               (emit (cdar xs))
               (loop (cdr xs) #t)]
              [else (loop (cdr xs) selected?)]))))

  (define key-ref/missing
    (lambda (value key)
      (keyed-ref/missing value key)))

  (define alist-update-values
    (lambda (value key update absent)
      (let ([builder (make-list-builder)] [found? #f])
        (for-each
         (lambda (entry)
           (if (eq? (car entry) key)
               (let ([child (update (cdr entry))])
                 (set! found? #t)
                 (unless (nav-missing? child) (builder (cons key child))))
               (builder entry)))
         value)
        (if found? (builder) (absent)))))

  (define key-transform
    (lambda (key default insert? value update)
      (if (alist? value)
          (alist-update-values
           value key update
           (lambda ()
             (if insert?
                 (update-default-focus default update (lambda () value)
                                       (lambda (child) (append value (list (cons key child)))))
                 (nav-focus-error 'nav-transform "missing key ~s in: ~s" key value))))
          (let ([old (keyed-ref/missing value key)])
            (cond [(nav-missing? old)
                   (if insert?
                       (update-default-focus default update (lambda () value)
                                             (lambda (child) (keyed-set value key child)))
                       (nav-focus-error 'nav-transform "missing key ~s in: ~s" key value))]
                  [else (keyed-set value key (update old))])))))

  (define key-transform!
    (lambda (key default insert? value update!)
      (let ([old (keyed-ref/missing value key)])
        (cond [(nav-missing? old)
               (if insert?
                   (update-default-focus default update! (lambda () value)
                                         (lambda (child) (keyed-set! value key child)))
                   (nav-focus-error 'nav-transform! "missing key ~s in: ~s" key value))]
              [(alist? value)
               (for-each (lambda (entry)
                           (when (eq? key (car entry))
                             (set-cdr! entry (update! (cdr entry)))))
                         value)
               value]
              [else (keyed-set! value key (update! old))]))))

  ;; A default is a virtual focus: traverse its suffix, but clearing never inserts it.
  (define clear-default-focus
    (lambda (selected default value clear present)
      (if (nav-missing? selected)
          (begin (clear default) value)
          (present))))

  (define indexed-clear
    (lambda (index value clear)
      (indexed-clear-values value (list index) clear)))

  (define indexed-clear!
    (lambda (index value clear!)
      (indexed-clear-values! value (list index) clear!)))

  (define clear-pair-field
    (lambda (who value ref replace clear)
      (unless (pair? value)
        (nav-focus-error who "not a pair: ~s" value))
      (let ([child (clear (ref value))])
        (when (nav-missing? child)
          (unsupported-clear who value))
        (replace value child))))

  (define slice-indexes
    (lambda (start stop step length)
      (let ([limit (min stop length)])
        (let loop ([index start] [indexes '()])
          (if (< index limit)
              (loop (+ index step) (cons index indexes))
              (reverse indexes))))))

  (define key-clear
    (lambda (key value clear)
      (if (alist? value)
          (alist-update-values
           value key clear
           (lambda () (nav-focus-error 'nav-clearval "missing key ~s in: ~s" key value)))
          (let ([old (keyed-ref/missing value key)])
            (if (nav-missing? old)
                (nav-focus-error 'nav-clearval "missing key ~s in: ~s" key value)
                (let ([child (clear old)])
                  (if (nav-missing? child)
                      (keyed-delete value key)
                      (keyed-set value key child))))))))

  (define key-clear!
    (lambda (key value clear!)
      (let ([old (keyed-ref/missing value key)])
        (cond [(nav-missing? old)
               (nav-focus-error 'nav-clearval! "missing key ~s in: ~s" key value)]
              [(alist? value)
               (for-each
                (lambda (entry)
                  (when (eq? key (car entry))
                    (let ([child (clear! (cdr entry))])
                      (when (nav-missing? child) (keyed-delete! value key))
                      (set-cdr! entry child))))
                value)]
              [else
               (let ([child (clear! old)])
                 (if (nav-missing? child)
                     (keyed-delete! value key)
                     (keyed-set! value key child)))])
        value)))

  (define alist-selected-location
    (lambda (who value key)
      (let loop ([entries value])
        (cond [(null? entries)
               (unless (current-optional-focus?)
                 (nav-focus-error who "missing key ~s in: ~s" key value))
               #f]
              [(eq? (caar entries) key) entries]
              [else (loop (cdr entries))]))))

  (define alist-selected-entry
    (lambda (who value key)
      (let ([location (alist-selected-location who value key)])
        (and location (car location)))))

  ;; Canonical means the actual stored key object equivalent to the query under
  ;; the map's key policy. Keeping it preserves key metadata and identity even
  ;; when a distinct query object compares equal. Read-only hashtables are copied
  ;; once because Chez's efficient cell lookup requires a mutable table.
  (define selected-submap
    (lambda (keys value)
      (let ([lookup (if (and (hashtable? value) (not (hashtable-mutable? value)))
                        (nav-call-user hashtable-copy value #t)
                        value)])
        (keyed-rebuild-entries
       value
       (map-in-order (lambda (key)
              (let ([canonical (keyed-canonical-key lookup key)])
                (let ([selected (if (nav-missing? canonical)
                                    nav-missing
                                    (keyed-ref/missing value canonical))])
                  (when (nav-missing? selected)
                    (nav-focus-error 'nav/submap "missing key ~s in: ~s" key value))
                  (cons canonical selected))))
            keys)))))

  (define updated-submap-entries
    (lambda (updated)
      (if (nav-missing? updated)
          '()
          (let ([entries (keyed-entries updated)])
            (unless (and entries (alist? entries))
              (nav-error 'nav-transform "expected replacement submap: ~s" updated))
            entries))))

  (define submap-transform
    (lambda (keys value update)
      (cond [(alist? value)
             (let ([replacements
                    (fold-left
                     (lambda (replacements key)
                       (let ([location (alist-selected-location 'nav-transform value key)])
                         (if location
                             (let ([entry (update (car location))])
                               (unless (pair? entry)
                                 (nav-error 'nav-transform "expected entry pair: ~s" entry))
                               (cons (cons location entry) replacements))
                             replacements)))
                     '() keys)])
               (let loop ([entries value])
                 (if (null? entries)
                     '()
                     (let ([replacement (assq entries replacements)])
                       (cons (if replacement (cdr replacement) (car entries))
                             (loop (cdr entries)))))))]
            [(keyed-supported? value)
             ;; Snapshot original membership before the suffix can modify submap.
             ;; Selected keys are canonical objects, so eq? suffices to identify
             ;; which original entries to omit without querying by mutated keys.
             (let* ([submap (selected-submap keys value)]
                    [selected-keys (make-eq-hashtable)]
                    [original (keyed-entries value)])
               (for-each (lambda (entry) (hashtable-set! selected-keys (car entry) #t))
                         (keyed-entries submap))
               (let ([updated (updated-submap-entries (update submap))]
                     [builder (make-list-builder)])
                 (for-each (lambda (entry)
                             (unless (hashtable-contains? selected-keys (car entry))
                               (builder entry)))
                           original)
                 ;; Unselected originals precede all replacement entries, so a
                 ;; replacement wins an equivalent-key collision. Rebuild once:
                 ;; pure per-key setters can copy the parent on every write.
                 (for-each builder updated)
                 (keyed-rebuild-entries value (builder))))]
            [else (nav-focus-error 'nav-transform "unsupported keyed value: ~s" value)])))

  (define submap-clear
    (lambda (keys value clear)
      (if (alist? value)
          (let ([replacements
                 (fold-left
                  (lambda (replacements key)
                    (let ([location (alist-selected-location 'nav-clearval value key)])
                      (if location
                          (cons (cons location (clear (car location))) replacements)
                          replacements)))
                  '() keys)])
            ;; Resolve repeated keys against the original locations before removing entries.
            (let loop ([entries value])
              (if (null? entries)
                  '()
                  (let ([replacement (assq entries replacements)])
                    (if (and replacement (nav-missing? (cdr replacement)))
                        (loop (cdr entries))
                        (cons (if replacement (cdr replacement) (car entries))
                              (loop (cdr entries))))))))
          (submap-transform keys value clear))))

  (define submap-clear!
    (lambda (keys value clear!)
      (if (alist? value)
          (begin
            (for-each
             (lambda (key)
               (let ([entry (alist-selected-entry 'nav-clearval! value key)])
                 (when entry
                   (let ([updated (clear! entry)])
                     (if (nav-missing? updated)
                         (keyed-delete! value key)
                         (keyed-set! value key (cdr updated)))))))
             keys)
            value)
          (let ([result (submap-clear keys value clear!)])
            (for-each
             (lambda (key)
               (let ([child (keyed-ref/missing result key)])
                 (if (nav-missing? child)
                     (keyed-delete! value key)
                     (keyed-set! value key child))))
             keys)
            value))))

  #|proc:nav/stay
  The `nav/stay` navigator focuses the current value unchanged.
  Kind: Reusable step.
  |#
  (define nav/stay
    (make-basic-nav 'nav/stay 'nav/stay
                    (lambda (value emit) (emit value))
                    (lambda (value update) (update value))
                    (lambda (value update!) (update! value))
                    (lambda (value clear) (clear value))
                    (lambda (value clear!) (clear! value))))

  #|proc:nav/none
  The `nav/none` navigator selects no values from the current value.
  Kind: Reusable step.
  |#
  (define nav/none
    (make-basic-nav 'nav/none 'nav/none
                    (lambda (value emit) (void))
                    (lambda (value update) value)
                    (lambda (value update!) value)
                    (lambda (value clear) value)
                    (lambda (value clear!) value)))

  #|proc:nav/all
  The `nav/all` navigator focuses every value in a supported collection. Pure clearing
  removes selected list elements or map entries; fixed-size and mutable list elements
  must remain present, but may be rebuilt by later path steps.
  Kind: Reusable step.
  |#
  (define nav/all
    (make-basic-nav 'nav/all 'nav/all
                    emit-indexed-values
                    transform-all-values
                    transform-all-values!
                    clear-all-values
                    clear-all-values!))

  #|proc:nav/values
  The `nav/values` navigator focuses collection values.
  Kind: Reusable step.
  |#
  (define nav/values
    (make-basic-nav 'nav/values 'nav/values
                    emit-indexed-values
                    transform-all-values
                    transform-all-values!
                    clear-all-values
                    clear-all-values!))

  ;; Clearing still runs the suffix. Retention means its result is a key value,
  ;; not the removal token; nested pure clears can rebuild that key. Mutable
  ;; clearing additionally requires the original key object (checked in data.ss).
  (define retained-key
    (lambda (value key clear)
      (let ([child (clear key)])
        (when (nav-missing? child) (unsupported-clear 'nav/keys value))
        child)))

  ;; An indexed key is a position, not replaceable storage. A nonmatching suffix
  ;; is a no-op, but an actual removal or changed index remains an error.
  (define clear-index-keys
    (lambda (value clear)
      (for-each (lambda (index)
                  (unless (eqv? index (retained-key value index clear))
                    (nav-error 'nav/keys "sequence index must remain unchanged: ~s" index)))
                (index-list (indexed-length value)))
      value))

  (define clear-keys
    (lambda (value clear)
      (if (and (not (alist? value)) (indexed-supported? value))
          (clear-index-keys value clear)
          (let ([entries (keyed-entries value)])
            (cond [entries
                   (keyed-rebuild-entries
                    value
                    (map-in-order (lambda (entry)
                                    (cons (retained-key value (car entry) clear) (cdr entry)))
                                  entries))]
                  [else (nav-focus-error 'nav-clearval "unsupported collection: ~s" value)])))))

  (define clear-keys!
    (lambda (value clear!)
      (cond [(and (not (alist? value)) (indexed-supported? value))
             (clear-index-keys value clear!)]
            [(keyed-supported? value)
             (keyed-transform-entries!
              value (lambda (entry)
                      (cons (retained-key value (car entry) clear!) (cdr entry))))]
            [else (nav-focus-error 'nav-clearval! "unsupported collection: ~s" value)])))

  #|proc:nav/keys
  The `nav/keys` navigator focuses map keys for alists and keyed containers, or
  sequence indexes for other indexed containers. Pure map transforms replace old keys.
  When transformed keys collide, the last entry in traversal order wins in maps with unique
  keys; alists retain duplicates. Sequence indexes are read-only: pure transforms return
  the sequence unchanged without invoking the callback, and mutation raises an error.
  Clearing traverses later path steps while retaining each key. Mutable clearing rebuilds
  writable map indexes when retained key objects change. Read-only maps cannot be reindexed:
  their keys must retain stable hash/equivalence dependencies, including changes through
  aliases. Shared keys must also stay valid in every other parent; no alias repair is provided.
  Mutable replacement and key removal are unsupported.
  Kind: Reusable step.
  |#
  (define nav/keys
    (make-basic-nav
     'nav/keys 'nav/keys
     (lambda (value emit)
       (cond [(alist? value) (for-each (lambda (entry) (emit (car entry))) value)]
             [(indexed-supported? value)
              (let ([len (indexed-length value)])
                (let loop ([i 0])
                  (when (< i len)
                    (emit i)
                    (loop (+ i 1)))))]
             [(keyed-keys value)
              => (lambda (keys) (for-each emit keys))]
             [else (nav-focus-error 'nav/keys "unsupported collection: ~s" value)]))
     (lambda (value update)
       (cond [(and (not (alist? value)) (indexed-supported? value)) value]
             [(keyed-entries value)
              => (lambda (entries)
                   (keyed-rebuild-entries
                    value (map-in-order (lambda (entry) (cons (update (car entry)) (cdr entry)))
                               entries)))]
             [else (nav-focus-error 'nav-transform "unsupported collection: ~s" value)]))
     (lambda (value update!)
       (nav-error 'nav-transform! "navigator nav/keys does not support mutation for: ~s" value))
     clear-keys
     clear-keys!))

  (define checked-entry
    (lambda (update entry)
      (let ([child (update entry)])
        (unless (and (not (nav-missing? child)) (pair? child))
          (nav-error 'nav/entries "expected retained entry pair: ~s" child))
        child)))

  (define transform-entries
    (lambda (value update)
       (cond [(alist? value)
              (map-in-order (lambda (entry) (checked-entry update entry)) value)]
             [(indexed-supported? value)
              (let ([index 0])
                (indexed-transform
                 value (index-list (indexed-length value))
                 (lambda (old)
                   (let ([entry (update (cons index old))])
                     (unless (and (pair? entry) (eqv? (car entry) index))
                       (nav-error 'nav-transform "entry must retain index ~s: ~s" index entry))
                     (set! index (fx+ index 1))
                     (cdr entry)))))]
             [(keyed-entries value)
              => (lambda (entries)
                   (keyed-rebuild-entries
                    value (map-in-order (lambda (old)
                                 (let ([entry (update old)])
                                   (unless (pair? entry)
                                     (nav-error 'nav-transform "expected entry pair: ~s" entry))
                                   entry))
                               entries)))]
             [else (nav-focus-error 'nav-transform "unsupported collection: ~s" value)])))

  (define clear-entries!
    (lambda (value clear!)
      (cond [(and (not (alist? value)) (indexed-supported? value))
             (let ([index 0])
               (indexed-transform!
                value (index-list (indexed-length value))
                (lambda (old)
                  (let ([entry (checked-entry clear! (cons index old))])
                    (unless (eqv? (car entry) index)
                      (nav-error 'nav/entries "entry must retain index ~s: ~s" index entry))
                    (set! index (fx+ index 1))
                    (cdr entry)))))]
            [(alist? value)
             (for-each
              (lambda (old)
                (let ([entry (checked-entry clear! old)])
                  (set-car! old (car entry))
                  (set-cdr! old (cdr entry)))) value)
             value]
            [(keyed-supported? value)
             (keyed-transform-entries! value (lambda (entry) (checked-entry clear! entry)))]
            [else (nav-focus-error 'nav/entries "unsupported collection: ~s" value)])))

  #|proc:nav/entries
  The `nav/entries` navigator focuses entries as `(key . value)` pairs. Pure transforms
  must return pairs. Map keys can change; the last entry in traversal order wins a key
  collision in maps with unique keys. Alists retain duplicate entries and their order.
  An indexed collection's entry must retain its index, and its value replaces that element.
  For containers supporting both protocols, indexed entries take precedence, except alists.
  Clearing visits later path steps and retains each entry pair; removing an entry is unsupported.
  Mutable clearing must retain map keys and indexes. Writable map parents rebuild their
  indexes after key clearing; read-only parents and other aliased parents are not repaired.
  Keys in those parents must keep stable hash/equivalence/order dependencies. Mutating only
  an entry value is supported when it does not change any aliased key's policy dependencies.
  Kind: Reusable step.
  |#
  (define nav/entries
    (make-basic-nav
     'nav/entries 'nav/entries
     (lambda (value emit)
       (cond [(alist? value) (for-each emit value)]
             [(list? value)
              (let loop ([items value] [index 0])
                (unless (null? items)
                  (emit (cons index (car items)))
                  (loop (cdr items) (fx+ index 1))))]
             [(indexed-supported? value)
              (let ([len (indexed-length value)])
                (let loop ([i 0])
                  (when (< i len)
                    (emit (cons i (indexed-ref/missing value i)))
                    (loop (+ i 1)))))]
             [(keyed-entries value)
              => (lambda (entries) (for-each emit entries))]
             [else (nav-focus-error 'nav/entries "unsupported collection: ~s" value)]))
     transform-entries
     (lambda (value update!)
       (nav-error 'nav-transform! "navigator nav/entries does not support mutation for: ~s" value))
     (lambda (value clear)
       (transform-entries value (lambda (entry) (checked-entry clear entry))))
     clear-entries!))

  #|proc:nav/nth
  The `nav/nth` procedure returns a navigator that focuses zero-based `index`.
  Kind: Reusable step constructor.
  |#
  (define nav/nth
    (lambda (index)
      (pcheck ([integer? index])
              (set! index (inexact->exact index))
              (when (< index 0)
                (nav-error 'nav/nth "index must be non-negative: ~s" index))
              (make-basic-nav
               'nav/nth `(nav/nth ,index)
               (lambda (value emit)
                 (let ([selected (indexed-ref/missing value index)])
                   (if (nav-missing? selected)
                       (nav-focus-error 'nav/nth "index ~s out of range for: ~s" index value)
                       (emit selected))))
               (lambda (value update)
                 (let ([selected (indexed-ref/missing value index)])
                   (if (nav-missing? selected)
                       (nav-focus-error 'nav-transform "index ~s out of range for: ~s" index value)
                       (indexed-transform value (list index) update))))
               (lambda (value update!)
                 (let ([selected (indexed-ref/missing value index)])
                   (if (nav-missing? selected)
                       (nav-focus-error 'nav-transform! "index ~s out of range for: ~s" index value)
                       (begin
                         (indexed-set! value index (update! selected))
                         value))))
               (lambda (value clear)
                 (indexed-clear index value clear))
               (lambda (value clear!)
                 (indexed-clear! index value clear!))))))

  #|proc:nav/nth/default
  The `nav/nth/default` procedure returns a navigator that focuses zero-based
  `index`, or `default` when the index is out of range. Clearing visits the default
  through later path steps but never inserts a missing location. Transforming inserts a
  missing location only if the full path reaches the update callback. The result is a navigator.
  Kind: Reusable step constructor.
  |#
  (define nav/nth/default
    (lambda (index default)
      (pcheck ([integer? index])
              (set! index (inexact->exact index))
              (when (< index 0)
                (nav-error 'nav/nth/default "index must be non-negative: ~s" index))
              (make-basic-nav
               'nav/nth/default `(nav/nth/default ,index ,default)
               (lambda (value emit)
                 (let ([selected (indexed-ref/missing value index)])
                   (emit (if (nav-missing? selected) default selected))))
               (lambda (value update)
                 (indexed-transform/default value index default update))
               (lambda (value update!)
                 (let ([selected (indexed-ref/missing value index)])
                   (if (nav-missing? selected)
                       (update-default-focus
                        default update! (lambda () value)
                        (lambda (child)
                          (nav-error 'nav-transform! "cannot update missing index ~s in: ~s"
                                     index value)))
                       (begin
                         (indexed-set! value index (update! selected))
                         value))))
               (lambda (value clear)
                 (clear-default-focus (indexed-ref/missing value index) default value clear
                                      (lambda () (indexed-clear index value clear))))
               (lambda (value clear!)
                 (clear-default-focus (indexed-ref/missing value index) default value clear!
                                      (lambda () (indexed-clear! index value clear!))))))))

  #|proc:nav/first
  The `nav/first` navigator focuses the first element of a supported sequence.
  Kind: Reusable step.
  |#
  (define nav/first (nav/nth 0))

  #|proc:nav/second
  The `nav/second` navigator focuses the second element of a supported sequence.
  Kind: Reusable step.
  |#
  (define nav/second (nav/nth 1))

  #|proc:nav/last
  The `nav/last` navigator focuses the last element of a non-empty sequence.
  Kind: Reusable step.
  |#
  (define nav/last
    (make-basic-nav
     'nav/last 'nav/last
     (lambda (value emit)
       (let ([len (indexed-length value)])
         (if (and len (> len 0))
             (emit (indexed-ref/missing value (- len 1)))
             (nav-focus-error 'nav/last "unsupported or empty sequence: ~s" value))))
     (lambda (value update)
       (let ([len (indexed-length value)])
         (if (and len (> len 0))
             (indexed-transform value (list (- len 1)) update)
             (nav-focus-error 'nav-transform "unsupported or empty sequence: ~s" value))))
     (lambda (value update!)
       (let ([len (indexed-length value)])
         (if (and len (> len 0))
             (begin
               (indexed-set! value (- len 1)
                             (update! (indexed-ref/missing value (- len 1))))
               value)
             (nav-focus-error 'nav-transform! "unsupported or empty sequence: ~s" value))))
     (lambda (value clear)
       (let ([len (indexed-length value)])
         (if (and len (> len 0))
             (indexed-clear (- len 1) value clear)
             (nav-focus-error 'nav-clearval "unsupported or empty sequence: ~s" value))))
     (lambda (value clear!)
       (let ([len (indexed-length value)])
         (if (and len (> len 0))
             (indexed-clear! (- len 1) value clear!)
             (nav-focus-error 'nav-clearval! "unsupported or empty sequence: ~s" value))))))

  #|proc:nav/slice
  The `nav/slice` procedure returns a navigator that focuses indexes in
  `[start, stop)`, optionally stepping by positive `step`.
  Kind: Reusable step constructor.
  |#
  (define nav/slice
    (case-lambda
      [(start stop) (nav/slice start stop 1)]
      [(start stop step)
       (pcheck ([integer? start stop step])
               (set! start (inexact->exact start))
               (set! stop (inexact->exact stop))
               (set! step (inexact->exact step))
               (when (or (< start 0) (< stop start) (<= step 0))
                 (nav-error 'nav/slice "invalid slice: ~s ~s ~s" start stop step))
               (make-basic-nav
                'nav/slice `(nav/slice ,start ,stop ,step)
                (lambda (value emit)
                  (let ([len (indexed-length value)])
                    (unless len
                      (nav-focus-error 'nav/slice "unsupported collection: ~s" value))
                    (let ([limit (min stop len)])
                      (if (list? value)
                          (when (< start limit)
                            (let loop ([items value] [index 0] [next start])
                              (when (< index limit)
                                (if (= index next)
                                    (begin
                                      (emit (car items))
                                      (loop (cdr items) (fx+ index 1) (+ next step)))
                                    (loop (cdr items) (fx+ index 1) next)))))
                          (let loop ([i start])
                            (when (< i limit)
                              (emit (indexed-ref/missing value i))
                              (loop (+ i step))))))))
                (lambda (value update)
                  (let ([len (indexed-length value)])
                    (unless len
                      (nav-focus-error 'nav-transform "unsupported collection: ~s" value))
                    (indexed-transform value (slice-indexes start stop step len) update)))
                (lambda (value update!)
                  (let ([len (indexed-length value)])
                    (unless len
                      (nav-focus-error 'nav-transform! "unsupported collection: ~s" value))
                    (indexed-transform! value (slice-indexes start stop step len) update!)))
                (lambda (value clear)
                  (let ([len (indexed-length value)])
                    (unless len
                      (nav-focus-error 'nav-clearval "unsupported collection: ~s" value))
                    (indexed-clear-values value (slice-indexes start stop step len) clear)))
                (lambda (value clear!)
                  (let ([len (indexed-length value)])
                    (unless len
                      (nav-focus-error 'nav-clearval! "unsupported collection: ~s" value))
                    (indexed-clear-values! value (slice-indexes start stop step len) clear!)))))]))

  #|proc:nav/key
  The `nav/key` procedure returns a navigator that focuses the value associated
  with key `key` in a hashtable or association list. For association lists,
  selection focuses each matching key in order.
  Kind: Reusable step constructor.
  |#
  (define nav/key
    (lambda (key)
      (make-basic-nav
       'nav/key `(nav/key ,key)
               (lambda (value emit)
                 (cond [(alist? value) (emit-alist-key-values value key emit)]
                       [else
                        (let ([selected (key-ref/missing value key)])
                          (if (nav-missing? selected)
                              (nav-focus-error 'nav/key "missing key ~s in: ~s" key value)
                              (emit selected)))]))
       (lambda (value update)
         (key-transform key nav-missing #f value update))
       (lambda (value update!)
         (key-transform! key nav-missing #f value update!))
       (lambda (value clear) (key-clear key value clear))
       (lambda (value clear!) (key-clear! key value clear!)))))

  #|proc:nav/key/default
  The `nav/key/default` procedure returns a navigator that focuses the value for
  `key`, or focuses `default` (which defaults to `#f`) when `key` is missing.
  For alists it focuses every matching
  entry in order, just like `nav/key`. Clearing visits a missing key's default through
  later path steps but never inserts that key. Transforming inserts it only if the full path
  reaches the update callback. The result is a navigator.
  Kind: Reusable step constructor.
  |#
  (define nav/key/default
    (case-lambda
      [(key) (nav/key/default key #f)]
      [(key default)
       (make-basic-nav
        'nav/key/default `(nav/key/default ,key ,default)
        (lambda (value emit)
          (if (and (alist? value) (assq key value))
              (emit-alist-key-values value key emit)
              (let ([selected (key-ref/missing value key)])
                (emit (if (nav-missing? selected) default selected)))))
        (lambda (value update)
          (key-transform key default #t value update))
        (lambda (value update!)
          (key-transform! key default #t value update!))
        (lambda (value clear)
          (clear-default-focus (keyed-ref/missing value key) default value clear
                               (lambda () (key-clear key value clear))))
        (lambda (value clear!)
          (clear-default-focus (keyed-ref/missing value key) default value clear!
                               (lambda () (key-clear! key value clear!)))))]))

  #|proc:nav/key-values
  The `nav/key-values` procedure returns a navigator that focuses the values for
  each key in `keys`, preserving key order. Updates and clears apply paths sequentially
  to the result of preceding keys; repeated keys may revisit changed values or be absent
  after an earlier removal. The result is a navigator.
  Kind: Reusable step constructor.
  |#
  (define nav/key-values
    (lambda keys
      (make-basic-nav
       'nav/key-values `(nav/key-values ,@keys)
       (lambda (value emit)
         (for-each
          (lambda (key)
            (nav-call-step ($navigator-select-proc (nav/key key)) value emit (void)))
          keys))
       (lambda (value update)
         (let loop ([keys keys] [value value])
           (if (null? keys)
               value
               (loop (cdr keys)
                     (nav-call-step ($navigator-transform-proc (nav/key (car keys)))
                                    value update value)))))
       (lambda (value update!)
         (for-each (lambda (key)
                     (nav-call-step ($navigator-transform!-proc (nav/key key)) value update! value))
                   keys)
         value)
       (lambda (value clear)
         (let loop ([keys keys] [value value])
           (if (null? keys)
               value
               (loop (cdr keys)
                     (nav-call-step (lambda (value clear) (key-clear (car keys) value clear))
                                    value clear value)))))
       (lambda (value clear!)
         (for-each (lambda (key)
                     (nav-call-step (lambda (value clear!) (key-clear! key value clear!))
                                    value clear! value))
                   keys)
         value))))

  #|proc:nav/submap
  The `nav/submap` procedure returns a navigator that focuses the subset of
  association-list entries or map entries named by `keys`. Map submaps preserve the source's
  container type and key comparison policy. Association lists focus matching entry pairs.
  For alists, each key selects its first matching entry; a pure transform replaces only that
  entry and may rename its key. Other keyed containers focus one submap containing the requested
  keys. Pure transforms remove the selected membership and merge every entry
  of the replacement submap, including new keys. Replacement entries win collisions with
  unselected parent keys. The result is a navigator.
  Kind: Reusable step constructor.
  |#
  (define nav/submap
    (lambda keys
      (make-basic-nav
       'nav/submap `(nav/submap ,@keys)
       (lambda (value emit)
         (cond [(alist? value)
                (for-each
                 (lambda (key)
                   (let ([entry (alist-selected-entry 'nav/submap value key)])
                     (when entry (emit entry))))
                 keys)]
               [(keyed-supported? value) (emit (selected-submap keys value))]
               [else (nav-focus-error 'nav/submap "unsupported keyed value: ~s" value)]))
       (lambda (value update)
         (submap-transform keys value update))
       (lambda (value update!)
         (nav-error 'nav-transform! "navigator nav/submap does not support mutation for: ~s"
                    value))
       (lambda (value clear)
         (submap-clear keys value clear))
       (lambda (value clear!)
         (submap-clear! keys value clear!)))))

  #|proc:nav/car
  The `nav/car` navigator focuses the `car` field of a pair.
  Kind: Reusable step.
  |#
  (define nav/car
    (make-basic-nav
     'nav/car 'nav/car
     (lambda (value emit)
       (if (pair? value)
           (emit (car value))
           (nav-focus-error 'nav/car "not a pair: ~s" value)))
     (lambda (value update)
       (if (pair? value)
           (cons (update (car value)) (cdr value))
           (nav-focus-error 'nav-transform "not a pair: ~s" value)))
     (lambda (value update!)
       (if (pair? value)
           (begin
             (set-car! value (update! (car value)))
             value)
           (nav-focus-error 'nav-transform! "not a pair: ~s" value)))
     (lambda (value clear)
       (clear-pair-field 'nav-clearval value car
                         (lambda (parent child) (cons child (cdr parent))) clear))
     (lambda (value clear!)
       (clear-pair-field 'nav-clearval! value car
                         (lambda (parent child) (set-car! parent child) parent) clear!))))

  #|proc:nav/cdr
  The `nav/cdr` navigator focuses the `cdr` field of a pair.
  Kind: Reusable step.
  |#
  (define nav/cdr
    (make-basic-nav
     'nav/cdr 'nav/cdr
     (lambda (value emit)
       (if (pair? value)
           (emit (cdr value))
           (nav-focus-error 'nav/cdr "not a pair: ~s" value)))
     (lambda (value update)
       (if (pair? value)
           (cons (car value) (update (cdr value)))
           (nav-focus-error 'nav-transform "not a pair: ~s" value)))
     (lambda (value update!)
       (if (pair? value)
           (begin
             (set-cdr! value (update! (cdr value)))
             value)
           (nav-focus-error 'nav-transform! "not a pair: ~s" value)))
     (lambda (value clear)
       (clear-pair-field 'nav-clearval value cdr
                         (lambda (parent child) (cons (car parent) child)) clear))
     (lambda (value clear!)
       (clear-pair-field 'nav-clearval! value cdr
                         (lambda (parent child) (set-cdr! parent child) parent) clear!))))

  (install-nav-key-proc! nav/key)
  (install-nav-nth-proc! nav/nth))
