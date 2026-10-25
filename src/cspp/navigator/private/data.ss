(library (cspp navigator private data)
  (export byte? indexed-length indexed-ref/missing indexed-set indexed-set!
          indexed-supported?
          keyed-supported? keyed-ref/missing keyed-set keyed-set!
          keyed-delete keyed-delete! keyed-entries keyed-keys keyed-values
          set-supported? set-values set-replace set-replace!
          set-delete set-delete!
          nav-register-indexed! nav-register-keyed! nav-register-set!
          alist? alist-ref/missing)
  (import (chezscheme)
          (cspp utils)
          (cspp navigator private core))

  (define-record-type ($indexed-protocol make-$indexed-protocol $indexed-protocol?)
    (fields (immutable predicate)
            (immutable length-proc)
            (immutable ref-proc)
            (immutable set-proc)
            (immutable set!-proc))
    (sealed #t))

  (define-record-type ($keyed-protocol make-$keyed-protocol $keyed-protocol?)
    (fields (immutable predicate)
            (immutable ref-proc)
            (immutable set-proc)
            (immutable set!-proc)
            (immutable delete-proc)
            (immutable delete!-proc)
            (immutable entries-proc))
    (sealed #t))

  (define-record-type ($set-protocol make-$set-protocol $set-protocol?)
    (fields (immutable predicate)
            (immutable values-proc)
            (immutable replace-proc)
            (immutable replace!-proc)
            (immutable delete-proc)
            (immutable delete!-proc))
    (sealed #t))

  (define indexed-protocols '())
  (define keyed-protocols '())
  (define set-protocols '())

  (define byte?
    (lambda (x)
      (and (integer? x) (<= 0 x) (<= x 255))))

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
  container and returns its length. The `ref-proc` procedure receives a
  container and zero-based index and returns the indexed value. The `set-proc`
  procedure receives a container, index, and new value, and returns a rebuilt
  container. The `set!-proc` procedure receives a container, index, and new
  value, mutates the container, and returns a value.
  |#
  (define nav-register-indexed!
    (lambda (predicate length-proc ref-proc set-proc set!-proc)
      (pcheck ([procedure? predicate length-proc ref-proc set-proc set!-proc])
              (set! indexed-protocols
                    (cons (make-$indexed-protocol predicate length-proc ref-proc
                                                  set-proc set!-proc)
                          indexed-protocols))
              (void))))

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
  entries in traversal order.
  |#
  (define nav-register-keyed!
    (lambda (predicate ref-proc set-proc set!-proc delete-proc delete!-proc entries-proc)
      (pcheck ([procedure? predicate ref-proc set-proc set!-proc
                           delete-proc delete!-proc entries-proc])
              (set! keyed-protocols
                    (cons (make-$keyed-protocol predicate ref-proc set-proc
                                                set!-proc delete-proc
                                                delete!-proc entries-proc)
                          keyed-protocols))
              (void))))

  #|proc:nav-register-set!
  Register callbacks for a set-like container. `predicate` recognizes the container.
  `values-proc` returns its members in traversal order. The replace callbacks receive
  a set and a list containing its complete replacement membership. The delete callbacks
  receive a set and one member.
  Pure callbacks return rebuilt sets. Mutating callbacks mutate their set and may return
  any value; navigator mutation still returns the original set object.
  |#
  (define nav-register-set!
    (lambda (predicate values-proc replace-proc replace!-proc delete-proc delete!-proc)
      (pcheck ([procedure? predicate values-proc replace-proc replace!-proc
                           delete-proc delete!-proc])
              (set! set-protocols
                    (cons (make-$set-protocol predicate values-proc replace-proc
                                              replace!-proc delete-proc delete!-proc)
                          set-protocols))
              (void))))

  (define find-indexed-protocol
    (lambda (value)
      (let loop ([protocols indexed-protocols])
        (cond [(null? protocols) #f]
              [(($indexed-protocol-predicate (car protocols)) value) (car protocols)]
              [else (loop (cdr protocols))]))))

  (define find-keyed-protocol
    (lambda (value)
      (let loop ([protocols keyed-protocols])
        (cond [(null? protocols) #f]
              [(($keyed-protocol-predicate (car protocols)) value) (car protocols)]
              [else (loop (cdr protocols))]))))

  (define find-set-protocol
    (lambda (value)
      (let loop ([protocols set-protocols])
        (cond [(null? protocols) #f]
              [(($set-protocol-predicate (car protocols)) value) (car protocols)]
              [else (loop (cdr protocols))]))))

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
                  (($indexed-protocol-length-proc protocol) value))]
            [else #f])))

  (define indexed-supported?
    (lambda (value)
      (and (indexed-length value) #t)))

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
                  (if (< index (($indexed-protocol-length-proc protocol) value))
                      (($indexed-protocol-ref-proc protocol) value index)
                      nav-missing))]
            [else (nav-focus-error 'nav/nth "unsupported indexed value: ~s" value)])))

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
            [(vector? value) (vector-set! value index new-value)]
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
                 (string-set! value index new-value)
                 (nav-error 'nav-transform!
                            "string element is not a char: ~s"
                            new-value))]
            [(bytevector? value)
             (if (byte? new-value)
                 (bytevector-u8-set! value index new-value)
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
      (cond [(hashtable? value) (hashtable-ref value key nav-missing)]
            [(alist? value) (alist-ref/missing value key)]
            [(find-keyed-protocol value)
             => (lambda (protocol)
                  (($keyed-protocol-ref-proc protocol) value key nav-missing))]
            [else (nav-focus-error 'nav/key "unsupported keyed value: ~s" value)])))

  (define keyed-set
    (lambda (value key new-value)
      (cond [(hashtable? value)
             (let ([copy (hashtable-copy value #t)])
               (hashtable-set! copy key new-value)
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

  (define keyed-set!
    (lambda (value key new-value)
      (cond [(hashtable? value)
             (hashtable-set! value key new-value)
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
             (let ([copy (hashtable-copy value #t)])
               (hashtable-delete! copy key)
               copy)]
            [(alist? value) (alist-remove-key key value)]
            [(find-keyed-protocol value)
             => (lambda (protocol)
                  (($keyed-protocol-delete-proc protocol) value key))]
            [else (nav-focus-error 'nav-clearval "unsupported keyed value: ~s" value)])))

  (define keyed-delete!
    (lambda (value key)
      (cond [(hashtable? value)
             (hashtable-delete! value key)
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
