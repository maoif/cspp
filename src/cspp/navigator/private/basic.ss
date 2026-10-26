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

  (define index-list
    (lambda (len)
      (let loop ([i 0] [acc '()])
        (if (= i len)
            (reverse acc)
            (loop (+ i 1) (cons i acc))))))

  (define indexed-transform!
    (lambda (value indexes update!)
      (for-each
       (lambda (index)
         (let ([old (indexed-ref/missing value index)])
           (if (nav-missing? old)
               (nav-focus-error 'nav-transform! "index ~s out of range for: ~s" index value)
               (indexed-set! value index (update! old)))))
       indexes)
      value))

  (define indexed-transform/default
    (lambda (value index default update)
      (let ([selected (indexed-ref/missing value index)])
        (cond [(not (nav-missing? selected))
               (indexed-transform value (list index) update)]
              [(list? value)
               (let loop ([xs value] [i 0])
                 (cond [(pair? xs)
                        (cons (car xs) (loop (cdr xs) (+ i 1)))]
                       [(= i index)
                        (list (update default))]
                       [else
                        (cons default (loop '() (+ i 1)))]))]
              [(vector? value)
               (let ([copy (make-vector (+ index 1) default)]
                     [len (vector-length value)])
                 (let loop ([i 0])
                   (when (< i len)
                     (vector-set! copy i (vector-ref value i))
                     (loop (+ i 1))))
                 (vector-set! copy index (update default))
                 copy)]
              [else
               (nav-error 'nav-transform "cannot update missing index ~s in: ~s"
                          index value)]))))

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
            [(keyed-entries value)
             => (lambda (entries)
                  (let loop ([entries entries] [value value])
                    (if (null? entries)
                        value
                        (loop (cdr entries)
                              (keyed-set value (caar entries)
                                         (update (cdar entries)))))))]
            [(set-values value)
             => (lambda (values)
                  (set-replace value (map update values)))]
            [else (nav-focus-error 'nav-transform "unsupported collection: ~s" value)])))

  (define transform-all-values!
    (lambda (value update!)
      (cond [(indexed-supported? value)
             (indexed-transform! value (index-list (indexed-length value)) update!)]
            [(keyed-entries value)
             => (lambda (entries)
                  (for-each
                   (lambda (entry)
                     (keyed-set! value (car entry) (update! (cdr entry))))
                   entries)
                  value)]
            [(set-values value)
             => (lambda (values)
                  (set-replace! value (map update! values)))]
            [else (nav-focus-error 'nav-transform! "unsupported collection: ~s" value)])))

  (define clear-all-values
    (lambda (value clear)
      (let ([values (set-values value)])
        (if values
            (let loop ([remaining values]
                       [members '()]
                       [result value]
                       [changed? #f])
              (if (null? remaining)
                  (if changed?
                      (set-replace result (reverse members))
                      result)
                  (let* ([old (car remaining)] [member (clear old)])
                    (if (nav-missing? member)
                        (loop (cdr remaining) members
                              (set-delete result old) changed?)
                        (loop (cdr remaining) (cons member members) result
                              (or changed? (not (equal? old member))))))))
            (unsupported-clear 'nav-clearval value)))))

  (define clear-all-values!
    (lambda (value clear!)
      (let ([values (set-values value)])
        (if values
            (let loop ([remaining values] [members '()] [changed? #f])
              (if (null? remaining)
                  (if changed?
                      (set-replace! value (reverse members))
                      value)
                  (let* ([old (car remaining)] [member (clear! old)])
                    (if (nav-missing? member)
                        (begin
                          (set-delete! value old)
                          (loop (cdr remaining) members changed?))
                        (loop (cdr remaining) (cons member members)
                              (or changed? (not (equal? old member))))))))
            (unsupported-clear 'nav-clearval! value)))))

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
                 (append value (list (cons key (update default))))
                 (nav-focus-error 'nav-transform "missing key ~s in: ~s" key value))))
          (let ([old (keyed-ref/missing value key)])
            (cond [(nav-missing? old)
                   (if insert?
                       (keyed-set value key (update default))
                       (nav-focus-error 'nav-transform "missing key ~s in: ~s" key value))]
                  [else (keyed-set value key (update old))])))))

  (define key-transform!
    (lambda (key default insert? value update!)
      (let ([old (keyed-ref/missing value key)])
        (cond [(nav-missing? old)
               (if insert?
                   (keyed-set! value key (update! default))
                   (nav-focus-error 'nav-transform! "missing key ~s in: ~s" key value))]
              [(alist? value)
               (for-each (lambda (entry)
                           (when (eq? key (car entry))
                             (set-cdr! entry (update! (cdr entry)))))
                         value)
               value]
              [else (keyed-set! value key (update! old))]))))

  (define list-remove-index
    (lambda (value index)
      (let loop ([xs value] [i index])
        (cond [(null? xs)
               (nav-focus-error 'nav-clearval "index ~s out of range for: ~s" index value)]
              [(= i 0) (cdr xs)]
              [else (cons (car xs) (loop (cdr xs) (- i 1)))]))))

  (define indexed-clear
    (lambda (index value clear)
      (let ([old (indexed-ref/missing value index)])
        (if (nav-missing? old)
            (nav-focus-error 'nav-clearval "index ~s out of range for: ~s" index value)
            (let ([new-value (clear old)])
              (if (nav-missing? new-value)
                  (if (list? value)
                      (list-remove-index value index)
                      (unsupported-clear 'nav-clearval value))
                  (indexed-set value index new-value)))))))

  (define indexed-clear!
    (lambda (index value clear!)
      (let ([old (indexed-ref/missing value index)])
        (if (nav-missing? old)
            (nav-focus-error 'nav-clearval! "index ~s out of range for: ~s" index value)
            (let ([new-value (clear! old)])
              (if (nav-missing? new-value)
                  (unsupported-clear 'nav-clearval! value)
                  (indexed-set! value index new-value))
              value)))))

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

  (define empty-subtable
    (lambda (table)
      (let ([equivalent? (hashtable-equivalence-function table)])
        (cond [(eq? equivalent? eq?) (make-eq-hashtable)]
              [(eq? equivalent? eqv?) (make-eqv-hashtable)]
              [else (make-hashtable (hashtable-hash-function table) equivalent?)]))))

  (define rebuild-keyed-entries
    (lambda (value entries)
      (cond [(hashtable? value)
             (let ([result (empty-subtable value)])
               (for-each (lambda (entry) (nav-call-user hashtable-set! result (car entry) (cdr entry)))
                         entries)
               result)]
            [(alist? value) entries]
            [else
             (let ([empty (fold-left (lambda (result entry)
                                       (keyed-delete result (car entry)))
                                     value (keyed-entries value))])
               (fold-left (lambda (result entry)
                            (keyed-set result (car entry) (cdr entry)))
                          empty entries))])))

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
      (cond [(hashtable? value)
             (let ([subtable (empty-subtable value)])
               (for-each
                (lambda (key)
                  (let ([selected (keyed-ref/missing value key)])
                    (if (nav-missing? selected)
                        (nav-focus-error 'nav-transform "missing key ~s in: ~s" key value)
                        (nav-call-user hashtable-set! subtable key selected))))
                keys)
               (let ([entries (updated-submap-entries (update subtable))]
                     [result (nav-call-user hashtable-copy value #t)])
                 (for-each (lambda (key) (nav-call-user hashtable-delete! result key)) keys)
                 (for-each
                  (lambda (entry)
                    (nav-call-user hashtable-set! result (car entry) (cdr entry)))
                  entries)
                 result))]
            [(alist? value)
             (let ([replacements
                    (fold-left
                     (lambda (replacements key)
                       (let find ([entries value])
                         (cond [(null? entries)
                                (nav-focus-error 'nav-transform "missing key ~s in: ~s"
                                                 key value)]
                               [(eq? (caar entries) key)
                                (let ([entry (update (car entries))])
                                  (unless (pair? entry)
                                    (nav-error 'nav-transform "expected entry pair: ~s" entry))
                                  (cons (cons entries entry) replacements))]
                               [else (find (cdr entries))])))
                     '() keys)])
               (let loop ([entries value])
                 (if (null? entries)
                     '()
                     (let ([replacement (assq entries replacements)])
                       (cons (if replacement (cdr replacement) (car entries))
                             (loop (cdr entries)))))))]
            [(keyed-supported? value)
             (let ([updated (update (let loop ([keys keys] [entries '()])
                                      (if (null? keys)
                                          (reverse entries)
                                          (let ([selected (keyed-ref/missing value (car keys))])
                                            (if (nav-missing? selected)
                                                (nav-focus-error 'nav-transform "missing key ~s in: ~s"
                                                           (car keys) value)
                                                (loop (cdr keys)
                                                      (cons (cons (car keys) selected)
                                                            entries)))))))])
               (let ([entries (updated-submap-entries updated)]
                     [remaining (fold-left (lambda (result key) (keyed-delete result key))
                                           value keys)])
                 (fold-left (lambda (result entry)
                              (keyed-set result (car entry) (cdr entry)))
                            remaining entries)))]
            [else (nav-focus-error 'nav-transform "unsupported keyed value: ~s" value)])))

  (define submap-clear
    (lambda (keys value clear)
      (if (alist? value)
          (let loop ([keys keys] [value value])
            (if (null? keys)
                value
                (loop
                 (cdr keys)
                 (let first-match ([entries value])
                   (cond [(null? entries)
                          (nav-focus-error 'nav-clearval "missing key ~s in: ~s"
                                           (car keys) value)]
                         [(eq? (caar entries) (car keys))
                          (let ([entry (clear (car entries))])
                            (if (nav-missing? entry)
                                (cdr entries)
                                (cons entry (cdr entries))))]
                         [else (cons (car entries) (first-match (cdr entries)))])))))
          (submap-transform keys value clear))))

  (define submap-clear!
    (lambda (keys value clear!)
      (if (alist? value)
          (begin
            (for-each
             (lambda (key)
               (let ([entry (assq key value)])
                 (unless entry
                   (nav-focus-error 'nav-clearval! "missing key ~s in: ~s" key value))
                 (let ([updated (clear! entry)])
                   (if (nav-missing? updated)
                       (keyed-delete! value key)
                       (keyed-set! value key (cdr updated))))))
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
  |#
  (define nav/none
    (make-basic-nav 'nav/none 'nav/none
                    (lambda (value emit) (void))
                    (lambda (value update) value)
                    (lambda (value update!) value)
                    (lambda (value clear) value)
                    (lambda (value clear!) value)))

  #|proc:nav/all
  The `nav/all` navigator focuses every value in a supported collection.
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
  |#
  (define nav/values
    (make-basic-nav 'nav/values 'nav/values
                    emit-indexed-values
                    transform-all-values
                    transform-all-values!
                    clear-all-values
                    clear-all-values!))

  #|proc:nav/keys
  The `nav/keys` navigator focuses map keys for alists and keyed containers, or
  sequence indexes for other indexed containers. Pure map transforms replace old keys.
  When transformed keys collide, the last entry in traversal order wins in maps with unique
  keys; alists retain duplicates. Sequence indexes are read-only.
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
                   (rebuild-keyed-entries
                    value (map (lambda (entry) (cons (update (car entry)) (cdr entry)))
                               entries)))]
             [else (nav-focus-error 'nav-transform "unsupported collection: ~s" value)]))
     (lambda (value update!)
       (nav-error 'nav-transform! "navigator nav/keys does not support mutation for: ~s" value))
     (lambda (value clear) (unsupported-clear 'nav-clearval value))
     (lambda (value clear!) (unsupported-clear 'nav-clearval! value))))

  #|proc:nav/entries
  The `nav/entries` navigator focuses entries as `(key . value)` pairs. Pure transforms
  must return pairs. Map keys can change; the last entry in traversal order wins a key
  collision in maps with unique keys. Alists retain duplicate entries and their order.
  An indexed collection's entry must retain its index, and its value replaces that element.
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
     (lambda (value update)
       (cond [(keyed-entries value)
              => (lambda (entries)
                   (rebuild-keyed-entries
                    value (map (lambda (old)
                                 (let ([entry (update old)])
                                   (unless (pair? entry)
                                     (nav-error 'nav-transform "expected entry pair: ~s" entry))
                                   entry))
                               entries)))]
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
             [else (nav-focus-error 'nav-transform "unsupported collection: ~s" value)]))
     (lambda (value update!)
       (nav-error 'nav-transform! "navigator nav/entries does not support mutation for: ~s" value))
     (lambda (value clear) (unsupported-clear 'nav-clearval value))
     (lambda (value clear!) (unsupported-clear 'nav-clearval! value))))

  #|proc:nav/nth
  The `nav/nth` procedure returns a navigator that focuses zero-based `index`.
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
  `index`, or `default` when the index is out of range.
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
                       (nav-error 'nav-transform! "cannot update missing index ~s in: ~s"
                                  index value)
                       (begin
                         (indexed-set! value index (update! selected))
                         value))))
               (lambda (value clear)
                 (unsupported-clear 'nav-clearval value))
               (lambda (value clear!)
                 (unsupported-clear 'nav-clearval! value))))))

  #|proc:nav/first
  The `nav/first` navigator focuses the first element of a supported sequence.
  |#
  (define nav/first (nav/nth 0))

  #|proc:nav/second
  The `nav/second` navigator focuses the second element of a supported sequence.
  |#
  (define nav/second (nav/nth 1))

  #|proc:nav/last
  The `nav/last` navigator focuses the last element of a non-empty sequence.
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
       (unsupported-clear 'nav-clearval value))
     (lambda (value clear!)
       (unsupported-clear 'nav-clearval! value))))

  #|proc:nav/slice
  The `nav/slice` procedure returns a navigator that focuses indexes in
  `[start, stop)`, optionally stepping by positive `step`.
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
                    (let ([limit (min stop len)])
                      (let loop ([i start] [indexes '()])
                        (if (< i limit)
                            (loop (+ i step) (cons i indexes))
                            (indexed-transform value (reverse indexes) update))))))
                (lambda (value update!)
                  (let ([len (indexed-length value)])
                    (unless len
                      (nav-focus-error 'nav-transform! "unsupported collection: ~s" value))
                    (let ([limit (min stop len)])
                      (let loop ([i start])
                        (when (< i limit)
                          (let ([selected (indexed-ref/missing value i)])
                            (unless (nav-missing? selected)
                              (indexed-set! value i (update! selected))))
                          (loop (+ i step)))))
                    value))
                (lambda (value clear)
                  (unsupported-clear 'nav-clearval value))
                (lambda (value clear!)
                  (unsupported-clear 'nav-clearval! value))))]))

  #|proc:nav/key
  The `nav/key` procedure returns a navigator that focuses the value associated
  with key `key` in a hashtable or association list. For association lists,
  selection focuses each matching key in order.
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
  `key`, or focuses `default` when `key` is missing. For alists it focuses every matching
  entry in order, just like `nav/key`.
  |#
  (define nav/key/default
    (case-lambda
      [(key) (nav/key/default key nav-missing)]
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
        (lambda (value clear) (key-clear key value clear))
        (lambda (value clear!) (key-clear! key value clear!)))]))

  #|proc:nav/key-values
  The `nav/key-values` procedure returns a navigator that focuses the values for
  each key in `keys`, preserving key order.
  |#
  (define nav/key-values
    (lambda keys
      (make-basic-nav
       'nav/key-values `(nav/key-values ,@keys)
       (lambda (value emit)
         (for-each
          (lambda (key)
            (($navigator-select-proc (nav/key key)) value emit))
          keys))
       (lambda (value update)
         (let loop ([keys keys] [value value])
           (if (null? keys)
               value
               (loop (cdr keys)
                     (($navigator-transform-proc (nav/key (car keys)))
                      value update)))))
       (lambda (value update!)
         (for-each (lambda (key)
                     (($navigator-transform!-proc (nav/key key)) value update!))
                   keys)
         value)
       (lambda (value clear)
         (let loop ([keys keys] [value value])
           (if (null? keys)
               value
               (loop (cdr keys)
                     (key-clear (car keys) value clear)))))
       (lambda (value clear!)
         (for-each (lambda (key)
                     (key-clear! key value clear!))
                   keys)
         value))))

  #|proc:nav/submap
  The `nav/submap` procedure returns a navigator that focuses the subset of
  association-list entries or hashtable entries named by `keys`. For alists, each key
  selects its first matching entry; a pure transform replaces only that entry and may rename
  its key. Other keyed containers focus one submap containing
  the requested keys. Pure transforms remove the selected membership and merge every entry
  of the replacement submap, including new keys. Replacement entries win collisions with
  unselected parent keys. The result is a navigator.
  |#
  (define nav/submap
    (lambda keys
      (make-basic-nav
       'nav/submap `(nav/submap ,@keys)
       (lambda (value emit)
         (cond [(hashtable? value)
                (let ([table (empty-subtable value)])
                  (for-each
                   (lambda (key)
                     (let ([selected (keyed-ref/missing value key)])
                       (if (nav-missing? selected)
                           (nav-focus-error 'nav/submap "missing key ~s in: ~s" key value)
                           (nav-call-user hashtable-set! table key selected))))
                   keys)
                  (emit table))]
               [(alist? value)
                (for-each
                 (lambda (key)
                   (let ([entry (assq key value)])
                     (if entry
                         (emit entry)
                         (nav-focus-error 'nav/submap "missing key ~s in: ~s" key value))))
                 keys)]
               [(keyed-supported? value)
                (let loop ([keys keys] [entries '()])
                  (if (null? keys)
                      (emit (reverse entries))
                      (let ([selected (keyed-ref/missing value (car keys))])
                        (if (nav-missing? selected)
                            (nav-focus-error 'nav/submap "missing key ~s in: ~s" (car keys) value)
                            (loop (cdr keys)
                                  (cons (cons (car keys) selected) entries))))))]
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
     (lambda (value clear) (unsupported-clear 'nav-clearval value))
     (lambda (value clear!) (unsupported-clear 'nav-clearval! value))))

  #|proc:nav/cdr
  The `nav/cdr` navigator focuses the `cdr` field of a pair.
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
     (lambda (value clear) (unsupported-clear 'nav-clearval value))
     (lambda (value clear!) (unsupported-clear 'nav-clearval! value))))

  (install-nav-key-proc! nav/key)
  (install-nav-nth-proc! nav/nth))
