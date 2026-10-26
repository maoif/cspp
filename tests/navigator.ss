(import (cspp))

(define list-copy*
  (lambda (x)
    (if (pair? x)
        (cons (list-copy* (car x)) (list-copy* (cdr x)))
        x)))

(define ht
  (lambda args
    (let ([table (make-eq-hashtable)])
      (let loop ([args args])
        (if (null? args)
            table
            (begin
              (hashtable-set! table (car args) (cadr args))
              (loop (cddr args))))))))

(define ht-ref
  (lambda (table key)
    (hashtable-ref table key #f)))

(define capture-written
  (lambda (proc value)
    (call-with-string-output-port
      (lambda (port)
        (proc value port)))))

(define has-same-keys?
  (lambda (table keys)
    (let ([ks (vector->list (hashtable-keys table))])
      (and (= (length ks) (length keys))
           (andmap (lambda (key) (memq key ks)) keys)))))

(define any?
  (lambda (predicate items)
    (and (ormap predicate items) #t)))

(define member-equal?
  (lambda (value items)
    (and (member value items) #t)))

(define same-items?
  (lambda (left right)
    (and (= (length left) (length right))
         (andmap (lambda (item) (member-equal? item right)) left))))

(define fxvector->list*
  (lambda (vec)
    (let ([len (fxvector-length vec)])
      (let loop ([i 0] [items '()])
        (if (= i len)
            (reverse items)
            (loop (+ i 1) (cons (fxvector-ref vec i) items)))))))

(define flvector->list*
  (lambda (vec)
    (let ([len (flvector-length vec)])
      (let loop ([i 0] [items '()])
        (if (= i len)
            (reverse items)
            (loop (+ i 1) (cons (flvector-ref vec i) items)))))))

(define deep-mixed-data
  (lambda ()
    (ht 'root
        (vector
         (list 'branch
               (ht 'bytes (bytevector 10 20 30)
                   'fxs (fxvector 1 2 3)
                   'fls (flvector 1.5 2.5 3.5)
                   'leaf 'done))
         'atom
         (list (ht 'items (vector (bytevector 7 8)
                                  (fxvector 4 5)
                                  (flvector 4.5 5.5))))))))

(define deep-count-data
  (lambda ()
    (ht 'root
        (vector
         (list (ht 'items (vector
                           (list (ht 'n 1))
                           (list (ht 'n 2))
                           (list (ht 'n 3)))))))))

(define deep-drop-data
  (lambda ()
    (ht 'root
        (list
         (list 'atom
               (ht 'keep 1
                   'drop 2
                   'nested (list (ht 'drop 3 'keep 4))))))))

(define deep-mutable-drop-data
  (lambda ()
    (ht 'root
        (ht 'branch
            (ht 'nested
                (ht 'drop 3 'keep 4))))))

(define deep-byte-path
  (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1)
            (nav/key 'bytes) (nav/nth 1)))

(define deep-fx-path
  (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1)
            (nav/key 'fxs) (nav/nth 2)))

(define deep-fl-path
  (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1)
            (nav/key 'fls) (nav/nth 0)))

(define deep-numbers-path
  (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 0) (nav/key 'items)
            nav/all (nav/nth 0) (nav/key 'n)))

(define deep-drop-path
  (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1) (nav/key 'drop)))

(define deep-nested-drop-path
  (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1) (nav/key 'nested)
            (nav/nth 0) (nav/key 'drop)))

(define deep-mutable-drop-path
  (nav/path (nav/key 'root) (nav/key 'branch) (nav/key 'nested) (nav/key 'drop)))

(define deep-pair-data
  (lambda ()
    (ht 'root
        (vector
         (list 'outer
               (cons (ht 'left (bytevector 1 2 3))
                     (ht 'right (fxvector 8 9))))))))

(define deep-choice-data
  (lambda (has-primary?)
    (ht 'root
        (vector
         (list
          (if has-primary?
              (ht 'primary (list (flvector 6.5 7.5))
                  'fallback (list (fxvector 4 5)))
              (ht 'fallback (list (fxvector 4 5)))))))))

(define deep-custom-record-data
  (lambda ()
    (vector
     (list
      (ht 'node (make-ixbox (vector (ht 'value 41))))))))

(define deep-pair-path
  (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1)))

(define deep-choice-primary-path
  (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 0)
            (nav/key 'primary) (nav/nth 0) (nav/nth 1)))

(define deep-choice-fallback-path
  (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 0)
            (nav/key 'fallback) (nav/nth 0) (nav/nth 1)))

(define deep-custom-record-path
  (nav/path (nav/nth 0) (nav/nth 0) (nav/key 'node)))

(define-record-type (ixbox make-ixbox ixbox?)
  (fields (mutable items)))

(define ixbox-copy-items
  (lambda (box)
    (vector-copy (ixbox-items box))))

(define ixbox-length
  (lambda (box)
    (vector-length (ixbox-items box))))

(define ixbox-ref
  (lambda (box index)
    (vector-ref (ixbox-items box) index)))

(define ixbox-set
  (lambda (box index value)
    (let ([items (ixbox-copy-items box)])
      (vector-set! items index value)
      (make-ixbox items))))

(define ixbox-set!
  (lambda (box index value)
    (vector-set! (ixbox-items box) index value)
    box))

(define-record-type (kvbox make-kvbox kvbox?)
  (fields (mutable entries)))

(define kvbox-ref
  (lambda (box key default)
    (let ([entry (assq key (kvbox-entries box))])
      (if entry (cdr entry) default))))

(define kvbox-set
  (lambda (box key value)
    (let loop ([entries (kvbox-entries box)])
      (cond [(null? entries) (make-kvbox (list (cons key value)))]
            [(eq? (caar entries) key)
             (make-kvbox (cons (cons key value) (cdr entries)))]
            [else
             (let ([tail (loop (cdr entries))])
               (make-kvbox (cons (car entries) (kvbox-entries tail))))]))))

(define kvbox-set!
  (lambda (box key value)
    (let loop ([entries (kvbox-entries box)])
      (cond [(null? entries)
             (kvbox-entries-set! box (cons (cons key value) (kvbox-entries box)))]
            [(eq? (caar entries) key)
             (set-cdr! (car entries) value)]
            [else (loop (cdr entries))]))
    box))

(define kvbox-delete
  (lambda (box key)
    (let loop ([entries (kvbox-entries box)])
      (cond [(null? entries) (make-kvbox '())]
            [(eq? (caar entries) key) (make-kvbox (cdr entries))]
            [else
             (let ([tail (loop (cdr entries))])
               (make-kvbox (cons (car entries) (kvbox-entries tail))))]))))

(define kvbox-delete!
  (lambda (box key)
    (kvbox-entries-set! box (kvbox-entries (kvbox-delete box key)))
    box))

(define kvbox->entries
  (lambda (box)
    (kvbox-entries box)))

(define-record-type (setbox make-setbox setbox?)
  (fields (mutable items)))

(nav-register-set!
 setbox?
 setbox-items
 (lambda (box items) (errorf 'setbox "replace callback must not clear members"))
 (lambda (box items) (errorf 'setbox "replace! callback must not clear members"))
 (lambda (box member)
   (make-setbox (remp (lambda (item) (equal? item member)) (setbox-items box))))
 (lambda (box member)
   (setbox-items-set! box
                      (remp (lambda (item) (equal? item member))
                            (setbox-items box)))
   box))

(mat navigator-core
     (nav? nav/stay)
     (nav-path? nav/empty-path)
     (nav-empty-path? nav/empty-path)
     (equal? '(nav/path) (nav->datum nav/empty-path))
     (equal? '(nav/path (nav/key user) (nav/nth 0))
             (nav->datum (nav/path 'user 0)))
     (equal? '(nav/path (nav/key user) nav/all (nav/key name))
             (nav->datum (nav/path (nav/path (nav/key 'user) nav/all)
                                   (nav/key 'name))))
     (eq? nav/stay (nav-coerce nav/stay))
     (nav-path? (nav-coerce (nav/path nav/stay)))
     (equal? '(nav/key user) (nav->datum (nav-coerce 'user)))
     (equal? '(nav/nth 3) (nav->datum (nav-coerce 3)))
     ;; negative: negative indexes are not valid path coercions.
     (error? (nav-coerce -1))
     ;; negative: arbitrary data cannot be coerced into a path step.
     (error? (nav-coerce '#(bad))))

(mat navigator-path-append
     (nav-empty-path? (nav/path-append))

     (equal? '(root) (nav-select (nav/path-append) 'root))

     (equal? '(Ada)
             (nav-select (nav/path-append (nav/path 'user 'name))
                         '((user (name . Ada)))))

     (equal? '(Ada)
             (nav-select (nav/path-append (nav/path 'user) nav/empty-path
                                         (nav/path 'name))
                         '((user (name . Ada)))))

     (equal? '(nav/path (nav/key user))
             (nav->datum (nav/path-append '() nav/empty-path (nav/path 'user)
                                         '() nav/empty-path)))

     (equal? '(nav/path (nav/key users) nav/all (nav/key profile) (nav/key name))
             (nav->datum (nav/path-append '(users) nav/all
                                         (nav/path (nav/path 'profile)) 'name)))

     (equal? '(Ada Grace)
             (nav-select (nav/path-append '(users) nav/all
                                         (nav/path 'profile) '(name))
                         (ht 'users (vector (ht 'profile (ht 'name 'Ada))
                                            (ht 'profile (ht 'name 'Grace))))))

     (equal? '(b)
             (nav-select (nav/path-append 0 '(1)) '#(#(a b) #(c d))))

     (equal? '((users ((score . 2)) ((score . 3))))
             (nav-transform (nav/path-append '(users) nav/all 'score)
                            add1 '((users ((score . 1)) ((score . 2))))))

     ;; negative: arbitrary values cannot be appended as paths.
     (error? (nav/path-append #t))

     ;; negative: a list of path steps cannot contain an invalid value.
     (error? (nav/path-append (nav/path 'user) '(name #t)))

     ;; negative: appended indexes must be non-negative.
     (error? (nav/path-append -1)))

(mat navigator-debug-core
     (string? (capture-written nav-write (nav/path nav/stay nav/none)))
     (equal? "(nav/path nav/stay nav/none)"
             (capture-written nav-write (nav/path nav/stay nav/none)))
     (let ([s (capture-written nav-display (nav/path nav/stay nav/none))])
       (and (string? s) (> (string-length s) 0)))
     (eq? 'nav/stay (nav-debug-name nav/stay))
     (equal? '(nav-explain (steps (nav/stay nav/none)))
             (nav-explain (nav/path nav/stay nav/none))))

(mat navigator-selection-basic
     (equal? '(root) (nav-select nav/empty-path 'root))
     (equal? '(root) (nav-select nav/stay 'root))
     (equal? '() (nav-select nav/none 'root))
     (equal? '(a b c) (nav-select nav/all '(a b c)))
     (equal? '(a b c) (nav-select nav/all '#(a b c)))
     (equal? '(#\a #\b) (nav-select nav/all "ab"))
     (equal? '(65 66) (nav-select nav/all #vu8(65 66)))
     (let ([table (ht 'a 1 'b 2)])
       (= 2 (length (nav-select nav/all table))))
     (equal? '(b) (nav-select (nav/nth 1) '(a b c)))
     (equal? '(b) (nav-select (nav/nth 1) '#(a b c)))
     (equal? '(#\b) (nav-select (nav/nth 1) "abc"))
     (equal? '(66) (nav-select (nav/nth 1) #vu8(65 66 67)))
     (equal? '(a) (nav-select nav/first '(a b)))
     (equal? '(b) (nav-select nav/second '(a b)))
     (equal? '(c) (nav-select nav/last '(a b c)))
     (equal? '(x) (nav-select (nav/key 'name) '((name . x) (age . 1))))
     (equal? '(7) (nav-select (nav/key 'count) (ht 'count 7)))
     (equal? '(fallback) (nav-select (nav/key/default 'missing 'fallback) '()))
     (equal? '(fallback) (nav-select (nav/nth/default 9 'fallback) '(a b)))
     (equal? '(1 3) (nav-select (nav/key-values 'a 'c) '((a . 1) (b . 2) (c . 3))))
     (equal? '((a . 1) (c . 3))
             (nav-select (nav/submap 'a 'c) '((a . 1) (b . 2) (c . 3))))
     (equal? '(a) (nav-select nav/car '(a . b)))
     (equal? '(b) (nav-select nav/cdr '(a . b)))
     ;; negative: strict index selection rejects out-of-range indexes.
     (error? (nav-select (nav/nth 9) '(a b)))
     ;; negative: strict key selection rejects missing association keys.
     (error? (nav-select (nav/key 'missing) '((name . x)))))

(mat navigator-data-structures
     (equal? '(1 2 3) (nav-select nav/all (array 1 2 3)))
     (equal? '(1 2 3) (nav-select nav/all (dlist 1 2 3)))
     (equal? '(1 2 3) (nav-select nav/all (treeset = < 3 1 2)))
     (equal? '(a b)
             (nav-select nav/values (treemap = < '(2 . b) '(1 . a))))
     (equal? '(10 20)
             (nav-select nav/values (fxtreemap fx= fx< '(2 . 20) '(1 . 10))))
     (equal? '(1 2 3) (nav-select nav/all (bitvec 3 1 2)))
     (equal? '(1 2 3) (nav-select nav/all (bittree 3 1 2)))
     (equal? '(1 2 3)
             (sort < (nav-select nav/all (hashset 3 1 2))))

     ;; set-like structures do not expose members as keys or indexes
     (error? (nav-select (nav/key 'x) (treeset = < 1)))
     (error? (nav-select (nav/nth 0) (hashset 1)))
     (error? (nav-select nav/keys (bitvec 1)))
     (error? (nav-select nav/entries (bittree 1))))

(mat navigator-data-structure-transforms
     (equal? '(2 3 4)
             (array->list (nav-transform nav/all add1 (array 1 2 3))))
     (equal? '(2 3 4)
             (dlist->list (nav-transform nav/all add1 (dlist 1 2 3))))
     (equal? '((1 . b) (2 . c))
             (treemap->list
              (nav-transform nav/values
                             (lambda (value) (if (eq? value 'a) 'b 'c))
                             (treemap = < '(1 . a) '(2 . b)))))
     (equal? '(2 3 4)
             (treeset->list (nav-transform nav/all add1 (treeset = < 1 2 3))))

     (let ([arr (array 1 2 3)])
       (and (eq? arr (nav-transform! nav/all add1 arr))
            (equal? '(2 3 4) (array->list arr))))
     (let ([dl (dlist 1 2 3)])
       (and (eq? dl (nav-transform! nav/all add1 dl))
            (equal? '(2 3 4) (dlist->list dl))))
     (let ([tm (treemap = < '(1 . 10) '(2 . 20))])
       (and (eq? tm (nav-transform! nav/values add1 tm))
            (equal? '((1 . 11) (2 . 21)) (treemap->list tm))))
     (let ([tm (fxtreemap fx= fx< '(1 . 10) '(2 . 20))])
       (and (eq? tm (nav-transform! nav/values fx1+ tm))
            (equal? '((1 . 11) (2 . 21)) (fxtreemap->list tm))))
     (let ([ts (treeset = < 1 2 3)])
       (and (eq? ts (nav-transform! nav/all add1 ts))
            (equal? '(2 3 4) (treeset->list ts))))

     (treeset-empty? (nav-clearval nav/all (treeset = < 1 2 3)))
     (let ([bv (bitvec 1 2 3)])
       (and (eq? bv (nav-clearval! nav/all bv))
            (bitvec-empty? bv)))
     (null? (setbox-items (nav-clearval nav/all (make-setbox '(1 2 3)))))
     (let ([box (make-setbox '(1 2 3))])
       (and (eq? box (nav-clearval! nav/all box))
            (null? (setbox-items box)))))

(mat navigator-selection-actions
     (equal? '(Ada)
             (nav-select (nav/path (nav/key 'user) (nav/key 'name))
                         '((user . ((name . Ada) (active . #t))))))
     (eq? #t
          (nav-select-one (nav/path (nav/key 'user) (nav/key 'active))
                          '((user . ((active . #t))))))
     (eq? 'fallback (nav-select-first (nav/key 'missing)
                                      '((name . Ada))
                                      'fallback))
     (eq? #f (nav-select-first nav/none '(a b c)))
     (let ([head-then-error
            (make-nav 'head-then-error
                      (lambda (value emit)
                        (emit (car value))
                        (errorf 'head-then-error "traversed too far"))
                      (lambda (value update) value)
                      (lambda (value update!) value))])
       (eq? 'a (nav-select-first head-then-error '(a b c))))
     (= 3 (nav-select-count nav/all '(a b c)))
     (nav-selected? (nav/key 'name) '((name . Ada)))
     (let ([head-then-error
            (make-nav 'head-then-error
                      (lambda (value emit)
                        (emit (car value))
                        (errorf 'head-then-error "traversed too far"))
                      (lambda (value update) value)
                      (lambda (value update!) value))])
       (nav-selected? head-then-error '(a b c)))
     (not (nav-selected? nav/none '(a b c)))
     (equal? '(0:a 1:b 2:c)
             (let ([lb (make-list-builder)])
               (nav-traverse/i nav/all '(a b c)
                               (lambda (i value)
                                 (lb (string->symbol
                                      (string-append (number->string i)
                                                     ":"
                                                     (symbol->string value))))))
               (lb)))
     ;; negative: select-one rejects zero selected values.
     (error? (nav-select-one nav/none '(a b)))
     ;; negative: select-one rejects multiple selected values.
     (error? (nav-select-one nav/all '(a b))))

(mat navigator-transform-pure
     (equal? '(2 3 4) (nav-transform nav/all add1 '(1 2 3)))
     (equal? '#(2 3 4) (nav-transform nav/all add1 '#(1 2 3)))
     (equal? "ABC" (nav-transform nav/all char-upcase "abc"))
     (equal? #vu8(66 67) (nav-transform nav/all add1 #vu8(65 66)))
     (equal? '((name . Ada) (age . 38))
             (nav-setval (nav/key 'age) 38 '((name . Ada) (age . 37))))
     (equal? '((name . Ada) (age . 1))
             (nav-setval (nav/key/default 'age) 1 '((name . Ada))))
     (equal? '(a x x z)
             (nav-setval (nav/nth/default 3 'x) 'z '(a)))
     (equal? '#(a x x z)
             (nav-setval (nav/nth/default 3 'x) 'z '#(a)))
     (equal? '(a X c) (nav-setval (nav/nth 1) 'X '(a b c)))
     (equal? '#(a X c) (nav-setval (nav/nth 1) 'X '#(a b c)))
     (equal? '(a (B C))
             (nav-transform (nav/path (nav/nth 1) nav/all)
                            (lambda (x) (string->symbol (string-upcase (symbol->string x))))
                            '(a (b c))))
     (equal? '((a . 11) (b . 2) (c . 13))
             (nav-transform (nav/submap 'a 'c)
                            (lambda (entry) (cons (car entry) (+ (cdr entry) 10)))
                            '((a . 1) (b . 2) (c . 3))))
     (let* ([table (ht 'a 1 'b 2 'c 3)]
            [new (nav-transform (nav/submap 'a 'c)
                                (lambda (subtable)
                                  (hashtable-set! subtable 'a 11)
                                  (hashtable-set! subtable 'c 13)
                                  subtable)
                                table)])
       (and (= 11 (ht-ref new 'a))
            (= 2 (ht-ref new 'b))
            (= 13 (ht-ref new 'c))
            (= 1 (ht-ref table 'a))))
     (equal? '(0:a 1:b 2:c)
             (nav-transform/i nav/all
                              (lambda (i value)
                                (string->symbol
                                 (string-append (number->string i) ":"
                                                (symbol->string value))))
                              '(a b c)))
     (let* ([old '#(1 2 3)]
            [new (nav-transform nav/all add1 old)])
       (and (equal? old '#(1 2 3))
            (equal? new '#(2 3 4))
            (not (eq? old new))))
     ;; negative: strict missing key cannot be transformed.
     (error? (nav-setval (nav/key 'missing) 1 '((name . Ada))))
     ;; negative: pure string transform must return characters.
     (error? (nav-transform nav/all (lambda (ch) 65) "ab"))
     ;; negative: pure bytevector transform must return bytes.
     (error? (nav-transform nav/all (lambda (b) #\A) #vu8(65))))

(mat navigator-transform-mutating
     (let ([xs (list 'a 'b 'c)])
       (and (eq? xs (nav-setval! (nav/nth 1) 'X xs))
            (equal? xs '(a X c))))
     (let ([v (vector 'a 'b 'c)])
       (and (eq? v (nav-setval! (nav/nth 1) 'X v))
            (equal? v '#(a X c))))
     (let ([s (string-copy "abc")])
       (and (eq? s (nav-transform! nav/all char-upcase s))
            (string=? s "ABC")))
     (let ([bv (bytevector-copy #vu8(65 66))])
       (and (eq? bv (nav-transform! nav/all add1 bv))
            (equal? bv #vu8(66 67))))
     (let ([table (ht 'count 1)])
       (and (eq? table (nav-setval! (nav/key 'count) 2 table))
            (= 2 (ht-ref table 'count))))
     (let ([root (vector (ht 'count 1))])
       (and (eq? root (nav-transform! (nav/path (nav/nth 0) (nav/key 'count)) add1 root))
            (= 2 (ht-ref (vector-ref root 0) 'count))))
     (let ([v (vector 'a 'b 'c)])
       (and (eq? v (nav-transform!/i nav/all
                                     (lambda (i value)
                                       (string->symbol
                                        (string-append (number->string i) ":"
                                                       (symbol->string value))))
                                     v))
            (equal? v '#(0:a 1:b 2:c))))
     ;; negative: mutating an out-of-range index is invalid.
     (error? (nav-setval! (nav/nth 3) 'x (vector 'a)))
     ;; negative: mutating an unsupported scalar does not fall back to pure rebuild.
     (error? (nav-transform! nav/all add1 1))
     ;; negative: string mutation must receive characters.
     (error? (nav-transform! nav/all (lambda (ch) 65) (string-copy "ab"))))

(mat navigator-numeric-vector-fastpaths
     (equal? '(1 2 3) (nav-select nav/all (fxvector 1 2 3)))
     (equal? '(1.5 2.5 3.5) (nav-select nav/all (flvector 1.5 2.5 3.5)))
     (equal? '(2) (nav-select (nav/nth 1) (fxvector 1 2 3)))
     (equal? '(2.5) (nav-select (nav/nth 1) (flvector 1.5 2.5 3.5)))
     (equal? '(3) (nav-select nav/last (fxvector 1 2 3)))
     (equal? '(3.5) (nav-select nav/last (flvector 1.5 2.5 3.5)))
     (equal? '(2 4) (nav-select (nav/slice 1 5 2) (fxvector 1 2 3 4 5)))
     (equal? '(2.5 4.5) (nav-select (nav/slice 1 5 2) (flvector 1.5 2.5 3.5 4.5 5.5)))
     (equal? '(0 1 2) (nav-select nav/keys (fxvector 1 2 3)))
     (equal? '(0 1 2) (nav-select nav/keys (flvector 1.5 2.5 3.5)))
     (equal? '((0 . 1) (1 . 2) (2 . 3))
             (nav-select nav/entries (fxvector 1 2 3)))
     (equal? '((0 . 1.5) (1 . 2.5) (2 . 3.5))
             (nav-select nav/entries (flvector 1.5 2.5 3.5)))
     (let* ([old (fxvector 1 2 3)]
            [new (nav-transform nav/all add1 old)])
       (and (not (eq? old new))
            (equal? '(1 2 3) (fxvector->list* old))
            (equal? '(2 3 4) (fxvector->list* new))))
     (let* ([old (flvector 1.5 2.5 3.5)]
            [new (nav-transform nav/all (lambda (x) (+ x 1.0)) old)])
       (and (not (eq? old new))
            (equal? '(1.5 2.5 3.5) (flvector->list* old))
            (equal? '(2.5 3.5 4.5) (flvector->list* new))))
     (equal? '(1 9 3)
             (fxvector->list* (nav-setval (nav/nth 1) 9 (fxvector 1 2 3))))
     (equal? '(1.5 9.5 3.5)
             (flvector->list* (nav-setval (nav/nth 1) 9.5 (flvector 1.5 2.5 3.5))))
     (let ([v (fxvector 1 2 3)])
       (and (eq? v (nav-transform! nav/all add1 v))
            (equal? '(2 3 4) (fxvector->list* v))))
     (let ([v (flvector 1.5 2.5 3.5)])
       (and (eq? v (nav-transform! nav/all (lambda (x) (+ x 1.0)) v))
            (equal? '(2.5 3.5 4.5) (flvector->list* v))))
     (let ([v (fxvector 1 2 3)])
       (and (eq? v (nav-setval! (nav/nth 1) 9 v))
            (equal? '(1 9 3) (fxvector->list* v))))
     (let ([v (flvector 1.5 2.5 3.5)])
       (and (eq? v (nav-setval! (nav/nth 1) 9.5 v))
            (equal? '(1.5 9.5 3.5) (flvector->list* v))))
     (let ([selected (nav-select nav/children (list (fxvector 1 2) (flvector 1.5 2.5)))])
       (and (= 6 (length selected))
            (fxvector? (car selected))
            (flvector? (cadr selected))
            (equal? '(1 2) (fxvector->list* (car selected)))
            (equal? '(1.5 2.5) (flvector->list* (cadr selected)))
            (equal? '(1 2 1.5 2.5) (cddr selected))))
     (equal? '(1 2 1.5 2.5)
             (nav-select nav/leaves (list (fxvector 1 2) (flvector 1.5 2.5))))
     ;; negative: pure fxvector transform must return fixnums.
     (error? (nav-transform nav/all (lambda (n) (+ n 0.5)) (fxvector 1)))
     ;; negative: pure flvector transform must return flonums.
     (error? (nav-transform nav/all (lambda (x) 1) (flvector 1.5)))
     ;; negative: mutating fxvector transform must return fixnums.
     (error? (nav-transform! nav/all (lambda (n) (+ n 0.5)) (fxvector 1)))
     ;; negative: mutating flvector transform must return flonums.
     (error? (nav-transform! nav/all (lambda (x) 1) (flvector 1.5))))

(mat navigator-conditional
     (equal? '(1 2 3) (nav-select (nav/path nav/all (nav/pred number?)) '(a 1 b 2 3 c)))
     (equal? '(a b c) (nav-select (nav/path nav/all (nav/not-pred number?)) '(a 1 b 2 3 c)))
     (equal? '() (nav-select (nav/path (nav/maybe (nav/key 'missing))) '((name . Ada))))
     (equal? '(Ada)
             (nav-select (nav/if (nav/key 'name)
                                 (nav/key 'name)
                                 (nav/key 'fallback))
                         '((name . Ada) (fallback . No))))
     (equal? '(No)
             (nav-select (nav/if (nav/key 'missing)
                                 (nav/key 'name)
                                 (nav/key 'fallback))
                         '((name . Ada) (fallback . No))))
     (equal? '(Ada 37)
             (nav-select (nav/multi-path (nav/key 'name) (nav/key 'age))
                         '((name . Ada) (age . 37))))
     (equal? '(Ada)
             (nav-select (nav/choice (nav/key 'missing) (nav/key 'name))
                         '((name . Ada))))
     (equal? '((name . ADA) (age . 37))
             (nav-transform (nav/when (nav/key 'name) (nav/key 'name))
                            (lambda (x) 'ADA)
                            '((name . Ada) (age . 37))))
     (equal? '((name . Ada) (age . 37))
             (nav-transform (nav/unless (nav/key 'name) (nav/key 'age))
                            add1
                            '((name . Ada) (age . 37))))
     ;; negative: must turns missing selection into an error.
     (error? (nav-select (nav/must (nav/key 'missing)) '((name . Ada)))))

(mat navigator-custom
     (let ([point-x (nav/getter-setter 'point-x car (lambda (p x) (cons x (cdr p))))])
       (equal? '(2 . 2) (nav-transform point-x add1 '(1 . 2))))
     (let ([cell (cons 1 2)]
           [point-x (nav/getter-setter! 'point-x car
                                        (lambda (p x) (cons x (cdr p)))
                                        (lambda (p x) (set-car! p x)))])
       (and (eq? cell (nav-setval! point-x 9 cell))
            (equal? cell '(9 . 2))))
     (equal? '(1) (nav-select (nav/getter 'first car) '(1 . 2)))
     ;; negative: read-only getter cannot be transformed.
     (error? (nav-transform (nav/getter 'first car) add1 '(1 . 2)))
     ;; negative: getter-setter without mutator cannot be used with transform!.
     (error? (nav-transform! (nav/getter-setter 'point-x car
                                                (lambda (p x) (cons x (cdr p))))
                            add1
                            (cons 1 2))))

(mat navigator-clear
     (equal? '(a c) (nav-clearval (nav/nth 1) '(a b c)))
     (equal? '((name . Ada)) (nav-clearval (nav/key 'age) '((name . Ada) (age . 37))))
     (let ([table (ht 'name 'Ada 'age 37)])
       (let ([new (nav-clearval (nav/key 'age) table)])
         (and (hashtable? new)
              (eq? 'Ada (ht-ref new 'name))
              (not (hashtable-contains? new 'age))
              (hashtable-contains? table 'age))))
     (let ([table (ht 'name 'Ada 'age 37)])
       (and (eq? table (nav-clearval! (nav/key 'age) table))
            (not (hashtable-contains? table 'age))))
     ;; negative: vector slots cannot be removed.
     (error? (nav-clearval (nav/nth 1) '#(a b c)))
     ;; negative: string slots cannot be removed.
     (error? (nav-clearval (nav/nth 1) "abc"))
     ;; negative: association list mutation cannot remove the head cell safely.
     (error? (nav-clearval! (nav/key 'name) '((name . Ada)))))

(mat navigator-custom-containers
     (begin
       (nav-register-indexed!
        ixbox?
        ixbox-length
        ixbox-ref
        ixbox-set
        ixbox-set!)
       #t)
     (equal? '(a b c) (nav-select nav/all (make-ixbox '#(a b c))))
     (equal? '(b) (nav-select (nav/nth 1) (make-ixbox '#(a b c))))
     (equal? '(0 1 2) (nav-select nav/keys (make-ixbox '#(a b c))))
     (equal? '((0 . a) (1 . b) (2 . c))
             (nav-select nav/entries (make-ixbox '#(a b c))))
     (let ([box (nav-transform nav/all
                               (lambda (value)
                                 (string->symbol
                                  (string-upcase (symbol->string value))))
                               (make-ixbox '#(a b)))])
       (equal? '#(A B) (ixbox-items box)))
     (let ([box (make-ixbox '#(a b c))])
       (and (eq? box (nav-setval! (nav/nth 1) 'B box))
            (equal? '#(a B c) (ixbox-items box)))))

(mat navigator-custom-maps
     (begin
       (nav-register-keyed!
        kvbox?
        kvbox-ref
        kvbox-set
        kvbox-set!
        kvbox-delete
        kvbox-delete!
        kvbox->entries)
       #t)
     (equal? '(Ada) (nav-select (nav/key 'name) (make-kvbox '((name . Ada) (age . 37)))))
     (equal? '(name age) (nav-select nav/keys (make-kvbox '((name . Ada) (age . 37)))))
     (equal? '(Ada 37) (nav-select nav/all (make-kvbox '((name . Ada) (age . 37)))))
     (equal? '((name . Ada) (age . 37))
             (nav-select nav/entries (make-kvbox '((name . Ada) (age . 37)))))
     (let ([box (nav-setval (nav/key 'age) 38 (make-kvbox '((name . Ada) (age . 37))))])
       (equal? '((name . Ada) (age . 38)) (kvbox-entries box)))
     (let ([box (nav-setval (nav/key/default 'active #f) #t (make-kvbox '((name . Ada))))])
       (equal? '((name . Ada) (active . #t)) (kvbox-entries box)))
     (let ([box (make-kvbox '((name . Ada) (age . 37)))])
       (and (eq? box (nav-setval! (nav/key 'age) 38 box))
            (equal? '((name . Ada) (age . 38)) (kvbox-entries box))))
     (let ([box (nav-clearval (nav/key 'age) (make-kvbox '((name . Ada) (age . 37))))])
       (equal? '((name . Ada)) (kvbox-entries box)))
     (let ([box (make-kvbox '((name . Ada) (age . 37)))])
       (and (eq? box (nav-clearval! (nav/key 'age) box))
            (equal? '((name . Ada)) (kvbox-entries box)))))

(mat navigator-recursive
     (equal? '((1 b) #(2 (c 3)) 1 b 2 (c 3) c 3)
             (nav-select (nav/path nav/children)
                         '(a (1 b) #(2 (c 3)))))
     (equal? '(a b c)
             (nav-select nav/leaves '(a (b) #(c))))
     (equal? '(1 2 3)
             (nav-select (nav/walker number?) '(a (1 b) #(2 (c 3)))))
     (let ([numbers-anywhere
            (nav/rec numbers-anywhere
              (nav/choice
               (nav/path (nav/pred number?))
               (nav/path nav/children numbers-anywhere)))])
       (equal? '(1 2 3) (nav-select numbers-anywhere '(a (1 b) #(2 (c 3))))))
     (let ([expr-values
            (nav/letrec
             ([expr (nav/choice
                     (nav/path (nav/key 'literal))
                     (nav/path (nav/key 'call) call)
                     (nav/path (nav/key 'block) stmt))]
              [stmt (nav/choice
                     (nav/path (nav/key 'expr) expr)
                     (nav/path (nav/key 'body) nav/all stmt))]
              [call (nav/path (nav/key 'args) nav/all expr)])
             expr)])
       (equal? '(1 2 3)
               (nav-select expr-values
                           '((block . ((body . (((expr . ((literal . 1)))
                                                 (expr . ((call . ((args . (((literal . 2)
                                                                              (literal . 3)))))))))))))))))
     (equal? '(root 1 2)
             (nav-select (nav/before (nav/path nav/children (nav/walker number?)))
                         '(root (1) (2))))
     (equal? '(1 2 root)
             (nav-select (nav/after (nav/path nav/children (nav/walker number?)))
                         '(root (1) (2))))
     (let ([numbers-anywhere
            (nav/rec numbers-anywhere
              (nav/choice
               (nav/path (nav/pred number?))
               (nav/path nav/children numbers-anywhere)))])
       (equal? '(a (2 b) #(3 (c 4)))
               (nav-transform numbers-anywhere add1 '(a (1 b) #(2 (c 3))))))
     (let ([root (vector 'a (list 1 'b) (vector 2 (list 'c 3)))]
           [numbers-anywhere
            (nav/rec numbers-anywhere
              (nav/choice
               (nav/path (nav/pred number?))
               (nav/path nav/children numbers-anywhere)))])
       (and (eq? root (nav-transform! numbers-anywhere add1 root))
            (equal? root '#(a (2 b) #(3 (c 4))))))
     (let ([drop-flags
            (nav/rec drop-flags
              (nav/choice
               (nav/path (nav/key 'drop))
               (nav/path nav/children drop-flags)))])
       (equal? '((keep . 1) ((drop . 4) (keep . 3)))
               (nav-clearval drop-flags '((keep . 1) (drop . 2) ((drop . 4) (keep . 3))))))
     (let ([root (vector (ht 'keep 1 'drop 2) (list (ht 'drop 4 'keep 3)))]
           [drop-flags
            (nav/rec drop-flags
              (nav/multi-path
               (nav/path (nav/maybe (nav/key 'drop)))
               (nav/path nav/children drop-flags)))])
       (and (eq? root (nav-clearval! drop-flags root))
            (not (hashtable-contains? (vector-ref root 0) 'drop))
            (not (hashtable-contains? (car (vector-ref root 1)) 'drop))
            (= 1 (ht-ref (vector-ref root 0) 'keep))
            (= 3 (ht-ref (car (vector-ref root 1)) 'keep))))
     ;; negative: unresolved recursive references are rejected.
     (error? (nav-select (make-nav-ref 'missing) '(a b))))

(mat navigator-interop
     (equal? '(1 2 3) (nav-selected->list nav/all '(1 2 3)))
     (equal? '#(1 2 3) (nav-selected->vector nav/all '(1 2 3)))
     (equal? '(1 2 3)
             (let ([iter (nav-traverse->iter nav/all '(1 2 3))])
               (iter->list iter)))
     (let* ([count 0]
            [lazy-nav (make-nav 'lazy
                                (lambda (value emit)
                                  (for-each
                                   (lambda (item)
                                     (set! count (+ count 1))
                                     (when (> count 1)
                                       (errorf 'lazy-nav "traversed too far"))
                                     (emit item))
                                   value))
                                (lambda (value update) value)
                                (lambda (value update!) value))])
       (let ([iter (nav-traverse->iter lazy-nav '(a b c))])
         (and (= count 0)
              (eq? 'a (iter-next! iter))
              (= count 1))))
     (= 6
        (nav-selected-transduce nav/all
                                (tidentity)
                                (rfsum)
                                '(1 2 3)))
     (= 6
        (nav-selected-transduce nav/all
                                (tidentity)
                                (rfsum)
                                0
                                '(1 2 3))))

(mat navigator-deep-actions
     (equal? '(20) (nav-select deep-byte-path (deep-mixed-data)))
     (= 1.5 (nav-select-one deep-fl-path (deep-mixed-data)))
     (= 3 (nav-select-first deep-fx-path (deep-mixed-data) 'missing))
     (= 3 (nav-select-count deep-numbers-path (deep-count-data)))
     (nav-selected? deep-byte-path (deep-mixed-data))
     (equal? '(1 2 3)
             (let ([lb (make-list-builder)])
               (nav-traverse deep-numbers-path (deep-count-data) lb)
               (lb)))
     (equal? '((0 . 1) (1 . 2) (2 . 3))
             (let ([lb (make-list-builder)])
               (nav-traverse/i deep-numbers-path
                               (deep-count-data)
                               (lambda (i value)
                                 (lb (cons i value))))
               (lb)))
     (equal? '(1 2 3)
             (let ([iter (nav-traverse->iter deep-numbers-path
                                             (deep-count-data))])
               (iter->list iter)))
     (equal? '(1 2 3)
             (nav-selected->list deep-numbers-path (deep-count-data)))
     (equal? '#(1 2 3)
             (nav-selected->vector deep-numbers-path (deep-count-data)))
     (= 6
        (nav-selected-transduce deep-numbers-path
                                (tidentity)
                                (rfsum)
                                (deep-count-data)))
     (let ([updated (nav-transform deep-fx-path add1 (deep-mixed-data))])
       (equal? '(1 2 4)
               (fxvector->list*
                (nav-select-one
                 (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1)
                           (nav/key 'fxs))
                 updated))))
     (equal? '(1 3 5)
             (nav-select deep-numbers-path
                         (nav-transform/i deep-numbers-path
                                          (lambda (i value) (+ value i))
                                          (deep-count-data))))
     (let ([updated (nav-setval deep-byte-path 99 (deep-mixed-data))])
       (= 99 (nav-select-one deep-byte-path updated)))
     (let* ([old (deep-drop-data)]
            [old-table (nav-select-one
                        (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1))
                        old)]
            [updated (nav-clearval deep-drop-path old)]
            [new-table (nav-select-one
                        (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1))
                        updated)])
       (and (hashtable-contains? old-table 'drop)
            (not (hashtable-contains? new-table 'drop))
            (= 1 (ht-ref new-table 'keep))))
     (let ([data (deep-mixed-data)])
       (and (eq? data (nav-transform! deep-byte-path add1 data))
            (= 21 (nav-select-one deep-byte-path data))))
     (let ([data (deep-count-data)])
       (and (eq? data (nav-transform!/i deep-numbers-path
                                        (lambda (i value) (+ value i))
                                        data))
            (equal? '(1 3 5) (nav-select deep-numbers-path data))))
     (let ([data (deep-mixed-data)])
       (and (eq? data (nav-setval! deep-fl-path 9.5 data))
            (= 9.5 (nav-select-one deep-fl-path data))))
     (let ([data (deep-mutable-drop-data)])
       (and (eq? data (nav-clearval! deep-mutable-drop-path data))
            (not (hashtable-contains?
                  (nav-select-one
                   (nav/path (nav/key 'root) (nav/key 'branch) (nav/key 'nested))
                   data)
                  'drop)))))

(mat navigator-deep-basic-navigators
     (let ([data (deep-mixed-data)])
       (eq? (ht-ref data 'root)
            (nav-select-one (nav/path nav/stay (nav/key 'root)) data)))
     (equal? '()
             (nav-select (nav/path (nav/key 'root) (nav/nth 0) nav/none
                                   (nav/key 'bytes))
                         (deep-mixed-data)))
     (= 3 (nav-select-count (nav/path (nav/key 'root) (nav/nth 2) (nav/nth 0)
                                      (nav/key 'items) nav/values)
                            (deep-mixed-data)))
     (same-items? '(bytes fxs fls leaf)
             (let ([keys (nav-select (nav/path (nav/key 'root) (nav/nth 0)
                                               (nav/nth 1) nav/keys)
                                     (deep-mixed-data))])
               (filter (lambda (key) (memq key '(bytes fxs fls leaf))) keys)))
     (let ([entries (nav-select (nav/path (nav/key 'root) (nav/nth 0)
                                          (nav/nth 1) nav/entries)
                                (deep-mixed-data))])
       (and (= 4 (length entries))
            (and (assq 'bytes entries) #t)
            (and (assq 'fxs entries) #t)
            (and (assq 'fls entries) #t)
            (and (assq 'leaf entries) #t)))
     (equal? '(#vu8(7 8))
             (nav-select (nav/path (nav/key 'root) (nav/nth 2) (nav/nth 0)
                                   (nav/key 'items) nav/first)
                         (deep-mixed-data)))
     (equal? '(#vfx(4 5))
             (nav-select (nav/path (nav/key 'root) (nav/nth 2) (nav/nth 0)
                                   (nav/key 'items) nav/second)
                         (deep-mixed-data)))
     (equal? '(#vfl(4.5 5.5))
             (nav-select (nav/path (nav/key 'root) (nav/nth 2) (nav/nth 0)
                                   (nav/key 'items) nav/last)
                         (deep-mixed-data)))
     (equal? '(#vfx(4 5) #vfl(4.5 5.5))
             (nav-select (nav/path (nav/key 'root) (nav/nth 2) (nav/nth 0)
                                   (nav/key 'items) (nav/slice 1 3))
                         (deep-mixed-data)))
     (equal? '(fallback)
             (nav-select (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1)
                                   (nav/key/default 'missing 'fallback))
                         (deep-mixed-data)))
     (equal? '(#vfx(1 2 3) done)
             (nav-select (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1)
                                   (nav/key-values 'fxs 'leaf))
                         (deep-mixed-data)))
     (let ([selected (nav-select (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1)
                                           (nav/submap 'bytes 'leaf))
                                 (deep-mixed-data))])
       (and (= 1 (length selected))
            (hashtable? (car selected))
            (hashtable-contains? (car selected) 'bytes)
            (hashtable-contains? (car selected) 'leaf)
            (equal? '(10 20 30)
                    (bytevector->u8-list (ht-ref (car selected) 'bytes)))))
     (let ([pair (nav-select-one deep-pair-path (deep-pair-data))])
       (let ([left (nav-select-one (nav/path deep-pair-path nav/car)
                                   (deep-pair-data))]
             [right (nav-select-one (nav/path deep-pair-path nav/cdr)
                                    (deep-pair-data))])
         (and (hashtable? left)
              (hashtable? right)
              (equal? '(1 2 3) (bytevector->u8-list (ht-ref left 'left)))
              (equal? '(8 9) (fxvector->list* (ht-ref right 'right))))))
     (equal? '(pad pad target)
             (nav-select-one
              (nav/key 'root)
              (nav-transform (nav/path (nav/key 'root) (nav/nth/default 2 'pad))
                             (lambda (value) 'target)
                             (ht 'root '(pad))))))

(mat navigator-deep-conditional-navigators
     (equal? '(10 20 30)
             (nav-select (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1)
                                   (nav/key 'bytes) nav/all (nav/pred even?))
                         (deep-mixed-data)))
     (equal? '(1 3)
             (nav-select (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1)
                                   (nav/key 'fxs) nav/all
                                   (nav/not-pred (lambda (n) (= n 2))))
                         (deep-mixed-data)))
     (= 20
        (nav-select-one (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1)
                                  (nav/key 'bytes) (nav/must (nav/nth 1)))
                        (deep-mixed-data)))
     (equal? '()
             (nav-select (nav/path (nav/key 'root) (nav/nth 0) (nav/nth 1)
                                   (nav/maybe (nav/key 'missing)))
                         (deep-mixed-data)))
     (= 7.5
        (nav-select-one (nav/path (nav/if deep-choice-primary-path
                                          deep-choice-primary-path
                                          deep-choice-fallback-path))
                        (deep-choice-data #t)))
     (= 5
        (nav-select-one (nav/path (nav/if deep-choice-primary-path
                                          deep-choice-primary-path
                                          deep-choice-fallback-path))
                        (deep-choice-data #f)))
     (equal? '(7.5)
             (nav-select (nav/when deep-choice-primary-path deep-choice-primary-path)
                         (deep-choice-data #t)))
     (equal? '(5)
             (nav-select (nav/unless deep-choice-primary-path deep-choice-fallback-path)
                         (deep-choice-data #f)))
     (equal? '(3 1.5)
             (nav-select (nav/multi-path deep-fx-path deep-fl-path)
                         (deep-mixed-data)))
     (= 5
        (nav-select-one (nav/choice deep-choice-primary-path deep-choice-fallback-path)
                        (deep-choice-data #f))))

(mat navigator-deep-custom-navigators
     (let ([box-items (nav/getter 'box-items ixbox-items)])
       (let ([selected (nav-select-one (nav/path deep-custom-record-path box-items)
                                       (deep-custom-record-data))])
         (and (vector? selected)
              (= 1 (vector-length selected))
              (= 41 (ht-ref (vector-ref selected 0) 'value)))))
     (let ([box-first (nav/getter-setter
                       'box-first
                       (lambda (box) (vector-ref (ixbox-items box) 0))
                       (lambda (box value)
                         (make-ixbox (vector value))))])
       (let ([updated (nav-transform (nav/path deep-custom-record-path box-first
                                               (nav/key 'value))
                                      add1
                                      (deep-custom-record-data))])
         (= 42
            (nav-select-one (nav/path deep-custom-record-path box-first
                                      (nav/key 'value))
                            updated))))
     (let ([box-first (nav/getter-setter!
                       'box-first!
                       (lambda (box) (vector-ref (ixbox-items box) 0))
                       (lambda (box value)
                         (make-ixbox (vector value)))
                       (lambda (box value)
                         (vector-set! (ixbox-items box) 0 value)))])
       (let ([data (deep-custom-record-data)])
         (and (eq? data (nav-transform! (nav/path deep-custom-record-path box-first
                                                  (nav/key 'value))
                                         add1
                                         data))
              (= 42
                 (nav-select-one (nav/path deep-custom-record-path box-first
                                           (nav/key 'value))
                                 data))))))

(mat navigator-deep-recursive-navigators
     (same-items? '(10 20 30 1 2 3 1.5 2.5 3.5 7 8 4 5 4.5 5.5)
             (nav-select (nav/walker number?) (deep-mixed-data)))
     (let ([selected (nav-select (nav/path (nav/key 'root) nav/children)
                                 (deep-mixed-data))])
       (and (pair? selected)
            (any? vector? selected)
            (any? list? selected)
            (any? hashtable? selected)))
     (same-items? '(branch 10 20 30 1 2 3 1.5 2.5 3.5 done atom 7 8 4 5 4.5 5.5)
             (nav-select nav/leaves (deep-mixed-data)))
     (let ([numbers-anywhere
            (nav/rec numbers-anywhere
              (nav/choice
               (nav/path (nav/pred number?))
               (nav/path nav/children numbers-anywhere)))])
       (same-items? '(10 20 30 1 2 3 1.5 2.5 3.5 7 8 4 5 4.5 5.5)
               (nav-select numbers-anywhere (deep-mixed-data))))
     (let ([numbers-anywhere
            (nav/letrec
             ([number-node (nav/choice
                            (nav/path (nav/pred number?))
                            (nav/path nav/children number-node))])
             number-node)])
       (same-items? '(10 20 30 1 2 3 1.5 2.5 3.5 7 8 4 5 4.5 5.5)
               (nav-select numbers-anywhere (deep-mixed-data))))
     (let ([selected (nav-select (nav/before deep-byte-path)
                                 (deep-mixed-data))])
       (and (hashtable? (car selected))
            (= 20 (cadr selected))))
     (let ([selected (nav-select (nav/after deep-byte-path)
                                 (deep-mixed-data))])
       (and (= 20 (car selected))
            (hashtable? (cadr selected)))))

(mat navigator-continuations
     (let ([visited 0])
       (let ([lazy-nav (make-nav 'lazy-first
                                 (lambda (value emit)
                                   (for-each
                                    (lambda (item)
                                      (set! visited (+ visited 1))
                                      (emit item))
                                    value))
                                 (lambda (value update) value)
                                 (lambda (value update!) value))])
         (and (eq? 'a (nav-select-first lazy-nav '(a b c)))
              (= visited 1))))
     (let ([visited 0])
       (let ([lazy-nav (make-nav 'lazy-selected?
                                 (lambda (value emit)
                                   (for-each
                                    (lambda (item)
                                      (set! visited (+ visited 1))
                                      (emit item))
                                    value))
                                 (lambda (value update) value)
                                 (lambda (value update!) value))])
         (and (nav-selected? lazy-nav '(a b c))
              (= visited 1))))
     (equal? '(a b c)
             (iter->list (nav-traverse->iter nav/all '(a b c)))))

(mat navigator-facade
     (equal? '(Ada)
             (nav-select (nav/path (nav/key 'user) (nav/key 'name))
                         '((user . ((name . Ada)))))))

(mat navigator-boundaries
     ;; negative: custom navigator name must be a symbol.
     (error? (make-nav "bad" values values values))
     ;; negative: custom navigator procedures must be procedures.
     (error? (make-nav 'bad 1 values values))
     ;; negative: nav-name expects a navigator.
     (error? (nav-name 'not-a-nav))
     ;; negative: nav/nth index must be natural.
     (error? (nav/nth -1))
     ;; negative: nav/slice step must be positive.
     (error? (nav/slice 0 3 0))
     ;; negative: nav/pred expects a procedure.
     (error? (nav/pred 'not-a-procedure))
     ;; negative: nav-traverse expects a procedure.
     (error? (nav-traverse nav/all '(a b) 'not-a-procedure))
     ;; negative: nav-transform expects a procedure.
     (error? (nav-transform nav/all 'not-a-procedure '(a b)))
     ;; negative: set protocol registration requires procedure callbacks.
     (error? (nav-register-set! 'not-a-procedure values values values values values))
     (error? (nav-register-set! pair? values values values values 'not-a-procedure)))

(mat navigator-review-iterator-and-transduction
  (let ([iterator (nav-traverse->iter nav/all '(a b c))]
        [events '()] [attempts 0])
    (equal? '((first a) (second b))
            ;; Bound reentry into the first caller if a generator continuation is wrong.
            (call/1cc
             (lambda (stop)
               (set! events (cons (list 'first (iter-next! iterator)) events))
               (set! attempts (+ attempts 1))
               (when (= attempts 4) (stop (reverse events)))
               (set! events (cons (list 'second (iter-next! iterator)) events))
               (reverse events)))))

  (let ([iterator (nav-traverse->iter nav/all '(a b))])
    (and (eq? 'a (iter-next! iterator))
         (begin (iter-reset! iterator) #t)
         (eq? 'a (iter-next! iterator))
         (eq? 'b (iter-next! iterator))
         (iter-end? (iter-next! iterator))
         (iter-end? (iter-next! iterator))
         (begin (iter-reset! iterator) #t)
         (eq? 'a (iter-next! iterator))))

  (equal? '(#(1 2) #(3))
          (nav-selected-transduce nav/all (tpartition-all 2) (rflist) '(1 2 3)))

  (= 19 (nav-selected-transduce nav/all (tmap add1) (rfsum) 10 '(1 2 3)))

  (let ([visited 0])
    (and (equal? '(1 2)
                 (nav-selected-transduce
                  (nav/path nav/all (nav/getter 'observe
                                               (lambda (value)
                                                 (set! visited (+ visited 1)) value)))
                  (ttake 2) (rflist) '(1 2 3 4)))
         (= visited 2)))

  ;; The transducer boundary rejects obsolete procedure arguments before traversal.
  (error? (nav-selected-transduce nav/all (lambda (reducer) reducer) (rfsum) '(1)))

  ;; The reducer boundary rejects obsolete procedure arguments before traversal.
  (error? (nav-selected-transduce nav/all (tidentity) + '(1))))

(define nested-group-path
  (nav/letrec
   ([outer (nav/path (nav/letrec ([inner (nav/key 'a)]) inner) leaf)]
    [leaf (nav/key 'x)])
   outer))

(mat navigator-review-recursive-environments
  (equal? '(1) (nav-select nested-group-path '((a . ((x . 1) (y . 2))))))

  (equal? '((a . ((x . 2) (y . 2))))
          (nav-transform nested-group-path add1 '((a . ((x . 1) (y . 2))))))

  (let ([data (ht 'a (ht 'x 1 'y 2))])
    (and (eq? data (nav-transform! nested-group-path add1 data))
         (equal? '(2) (nav-select nested-group-path data))))

  (equal? '((a . ((y . 2))))
          (nav-clearval nested-group-path '((a . ((x . 1) (y . 2))))))

  (let ([data (ht 'a (ht 'x 1 'y 2))])
    (and (eq? data (nav-clearval! nested-group-path data))
         (not (hashtable-contains? (ht-ref data 'a) 'x))
         (= 2 (ht-ref (ht-ref data 'a) 'y))))

  (equal? '(1) (iter->list (nav-traverse->iter nested-group-path
                                            '((a . ((x . 1) (y . 2))))))))

(define conditional-a-paths
  (list (nav/maybe (nav/key 'a))
        (nav/must (nav/key 'a))
        (nav/if nav/stay (nav/key 'a) nav/none)
        (nav/multi-path (nav/key 'a))
        (nav/choice (nav/key 'a))))

(mat navigator-review-conditional-clearing
  (andmap
   (lambda (wrapper)
     (equal? '((a . ((y . 2))) (b . 3))
             (nav-clearval (nav/path wrapper (nav/key 'x))
                           '((a . ((x . 1) (y . 2))) (b . 3)))))
   conditional-a-paths)

  (andmap
   (lambda (wrapper)
     (let ([data (ht 'a (ht 'x 1 'y 2) 'b 3)])
       (and (eq? data (nav-clearval! (nav/path wrapper (nav/key 'x)) data))
            (= 2 (ht-ref (ht-ref data 'a) 'y))
            (not (hashtable-contains? (ht-ref data 'a) 'x))
            (= 3 (ht-ref data 'b)))))
   conditional-a-paths)

  (andmap
   (lambda (wrapper)
     (equal? '((a . 1))
             (nav-clearval (nav/path wrapper nav/none) '((a . 1)))))
   conditional-a-paths)

  (equal? '((b . 3)) (nav-clearval (nav/maybe (nav/key 'a)) '((a . 1) (b . 3))))

  (equal? '((b . 3)) (nav-clearval (nav/maybe (nav/key 'a)) '((b . 3))))

  ;; Optional navigation must not hide failures raised by the user's updater.
  (error? (nav-transform (nav/maybe nav/stay)
                         (lambda (value) (error 'callback "failure")) 1))

  ;; Missing focuses in a suffix outside nav/maybe must still raise.
  (error? (nav-select (nav/path (nav/maybe nav/stay) (nav/key 'missing)) '()))

  ;; Optional selection must not hide a failing user predicate.
  (error? (nav-select (nav/maybe (nav/pred (lambda (value) (error 'pred "failure")))) 1))

  ;; Convenience selection must not convert failing callbacks into absent focuses.
  (error? (nav-select-first (nav/pred (lambda (value) (error 'pred "failure"))) 1))

  ;; Selection predicates must not suppress application failures.
  (error? (nav-selected? (nav/pred (lambda (value) (error 'pred "failure"))) 1))

  ;; Removing the root remains an error even through optional identity navigation.
  (error? (nav-clearval (nav/maybe nav/stay) 1))

  ;; Mutation cannot remove the root through a conditional predicate.
  (error? (nav-clearval! (nav/pred number?) 1)))

(mat navigator-review-clear-markers
  (equal? '((b . 2))
          (nav-clearval (nav/path (nav/key 'a) (nav/multi-path nav/stay))
                        '((a . 1) (b . 2))))

  (let ([data (ht 'a 1 'b 2)])
    (and (eq? data (nav-clearval! (nav/path (nav/key 'a) (nav/multi-path nav/stay)) data))
         (not (hashtable-contains? data 'a))
         (= 2 (ht-ref data 'b)))))

(mat navigator-review-container-clearing
  (equal? '((a . ((y . 2))) (b . ((y . 4))))
          (nav-clearval (nav/path (nav/key-values 'a 'b) (nav/key 'x))
                        '((a . ((x . 1) (y . 2))) (b . ((x . 3) (y . 4))))))

  (equal? '((a . 1) (b . 2))
          (nav-clearval (nav/path (nav/key-values 'a 'b) nav/none) '((a . 1) (b . 2))))

  (let ([data (ht 'a (ht 'x 1 'y 2) 'b (ht 'x 3 'y 4))])
    (nav-clearval! (nav/path (nav/key-values 'a 'b) (nav/key 'x)) data)
    (and (= 2 (ht-ref (ht-ref data 'a) 'y))
         (= 4 (ht-ref (ht-ref data 'b) 'y))
         (not (hashtable-contains? (ht-ref data 'a) 'x))))

  (equal? '((a . 1) (b . 2))
          (nav-clearval (nav/path (nav/submap 'a) nav/none) '((a . 1) (b . 2))))

  (let* ([data (ht 'a 1 'b 2 'c 3)]
         [result (nav-clearval (nav/path (nav/submap 'a 'b) (nav/key 'a)) data)])
    (and (not (hashtable-contains? result 'a))
         (= 2 (ht-ref result 'b))
         (= 3 (ht-ref result 'c))
         (= 1 (ht-ref data 'a))))

  (let ([data (ht 'a 1 'b 2 'c 3)])
    (and (eq? data (nav-clearval! (nav/path (nav/submap 'a 'b) (nav/key 'a)) data))
         (not (hashtable-contains? data 'a))
         (= 2 (ht-ref data 'b))
         (= 3 (ht-ref data 'c))))

  (equal? '#(((y . 2)))
          (nav-clearval (nav/path nav/children (nav/key 'x)) '#(((x . 1) (y . 2)))))

  (let* ([data (ht 'a '((x . 1) (y . 2)))]
         [result (nav-clearval (nav/path nav/children (nav/key 'x)) data)])
    (and (equal? '((y . 2)) (ht-ref result 'a))
         (equal? '((x . 1) (y . 2)) (ht-ref data 'a))))

  ;; Fixed-size indexed collections cannot remove a child directly.
  (error? (nav-clearval (nav/path nav/children nav/stay) '#(1 2)))

  (let ([data (make-hashtable string-hash string=?)])
    (hashtable-set! data (string-copy "a") 1)
    (= 1 (hashtable-ref (nav-select-one (nav/submap (string-copy "a")) data)
                        (string-copy "a") #f)))

  (let ([data (make-hashtable string-hash string=?)])
    (hashtable-set! data (string-copy "a") 1)
    (let ([result
           (nav-transform (nav/submap (string-copy "a"))
                          (lambda (table)
                            (hashtable-set! table (string-copy "a") 2)
                            table) data)])
      (and (= 2 (hashtable-ref result (string-copy "a") #f))
           (= 1 (hashtable-ref data (string-copy "a") #f))))))

(mat navigator-review-entry-transforms
  (equal? '#(11 12)
          (nav-transform nav/entries
                         (lambda (entry) (cons (car entry) (+ 10 (cdr entry)))) '#(1 2)))

  (equal? '((a . 2) (a . 3))
          (nav-transform (nav/key 'a) add1 '((a . 1) (a . 2))))

  (let ([data (list (cons 'a 1) (cons 'a 2))])
    (and (eq? data (nav-transform!/i (nav/key 'a) (lambda (index value) (+ index value)) data))
         (equal? '((a . 1) (a . 3)) data)))

  (equal? '((b . 3)) (nav-clearval (nav/key 'a) '((a . 1) (a . 2) (b . 3))))

  (let* ([data (ht 'a 1)]
         [result (nav-transform nav/keys (lambda (key) 'b) data)])
    (and (not (hashtable-contains? result 'a))
         (= 1 (ht-ref result 'b))
         (= 1 (hashtable-size result))
         (= 1 (ht-ref data 'a))))

  (let* ([data (ht 'a 1 'b 2)]
         [result (nav-transform nav/keys (lambda (key) (if (eq? key 'a) 'b 'a)) data)])
    (and (= 2 (ht-ref result 'a)) (= 1 (ht-ref result 'b))))

  (let* ([data (ht 'a 1)]
         [result (nav-transform nav/entries (lambda (entry) '(b . 10)) data)])
    (and (not (hashtable-contains? result 'a))
         (= 10 (ht-ref result 'b)) (= 1 (hashtable-size result))))

  (let ([result (nav-transform nav/entries (lambda (entry) '(b . 10))
                                (make-kvbox '((a . 1))))])
    (equal? '((b . 10)) (kvbox-entries result)))

  (equal? '((b . 1) (b . 2))
          (nav-transform nav/entries (lambda (entry) (cons 'b (cdr entry)))
                         '((a . 1) (a . 2))))

  ;; Indexed entries retain their positional key during updates.
  (error? (nav-transform nav/entries (lambda (entry) (cons 9 (cdr entry))) '#(1 2)))

  ;; Entry transforms must return pairs, including when the source is indexed.
  (error? (nav-transform nav/entries (lambda (entry) 12) '#(1 2))))

(mat navigator-review-indexed-copying
  (let ([source (vector 1 2 3 4)] [seen '()])
    (let ([result (nav-transform (nav/slice 1 4 2)
                                 (lambda (value) (set! seen (cons value seen)) (+ 10 value))
                                 source)])
      (and (equal? source '#(1 2 3 4))
           (equal? result '#(1 12 3 14))
           (equal? seen '(4 2))
           (not (eq? source result)))))

  (let* ([source (list 1 2 3 4)]
         [result (nav-transform (nav/slice 0 3 2) add1 source)])
    (and (equal? source '(1 2 3 4))
         (equal? result '(2 2 4 4))
         (eq? (cdddr source) (cdddr result))))

  (let ([source (vector 1 2)])
    (eq? source (nav-transform (nav/slice 2 2) add1 source)))

  (equal? '#vfx(2 3) (nav-transform nav/all add1 '#vfx(1 2)))

  (equal? '#vfl(2.0 3.0) (nav-transform nav/all (lambda (value) (fl+ value 1.0))
                                      '#vfl(1.0 2.0)))

  (equal? #vu8(2 3) (nav-transform nav/all add1 #vu8(1 2)))

  (string=? "ABC" (nav-transform nav/all char-upcase (string-copy "abc")))

  (let ([source (make-ixbox '#(1 2 3))])
    (and (equal? '#(1 2 3) (ixbox-items source))
         (equal? '#(2 3 4) (ixbox-items (nav-transform nav/all add1 source))))))

(mat navigator-review-alist-map-focuses
  (equal? '(a b) (nav-select nav/keys '((a . 1) (b . 2))))

  (equal? '((a . 1) (b . 2)) (nav-select nav/entries '((a . 1) (b . 2))))

  (equal? '((b . 1) (a . 2))
          (nav-transform nav/keys (lambda (key) (if (eq? key 'a) 'b 'a))
                         '((a . 1) (b . 2))))

  (equal? '(a a) (nav-select nav/keys '((a . 1) (a . 2))))

  (equal? '((a . 0) (a . 1))
          (nav-transform/i nav/entries (lambda (index entry) (cons (car entry) index))
                           '((a . 1) (a . 2))))

  (equal? '(1 2) (nav-select (nav/key/default 'a 0) '((a . 1) (a . 2))))

  (equal? '((a . 0) (a . 1))
          (nav-transform/i (nav/key/default 'a 0) (lambda (index value) index)
                           '((a . 1) (a . 2))))

  (equal? '(0) (nav-select (nav/key/default 'a 0) '((b . 2))))

  (equal? '(0 1) (nav-select nav/keys '(a b)))

  (equal? '((0 . a) (1 . b)) (nav-select nav/entries '(a b))))

(define callback-missing-focus
  (lambda (value) (nav-select-one (nav/key 'missing) '())))

(mat navigator-review-callback-focus-errors
  ;; A callback that itself performs a failing navigation must propagate that failure.
  (error? (nav-select (nav/maybe (nav/pred callback-missing-focus)) 1))

  ;; Getters are callbacks too, even when their exception describes an absent focus.
  (error? (nav-select (nav/maybe (nav/getter 'lookup callback-missing-focus)) 1))

  ;; Convenience predicates must distinguish callback failures from absent selections.
  (error? (nav-selected? (nav/pred callback-missing-focus) 1))

  ;; Choosing the first focus must not suppress a callback-origin focus exception.
  (error? (nav-select-first (nav/getter 'lookup callback-missing-focus) 1))

  ;; A failing callback inside the test path must not choose the fallback branch.
  (error? (nav-select (nav/if (nav/pred callback-missing-focus) nav/stay nav/none) 1))

  ;; User-defined navigator implementations must not hide navigation failures.
  (error? (nav-select
           (nav/maybe (make-nav 'lookup
                                (lambda (value emit) (emit (callback-missing-focus value)))
                                (lambda (value update) value)
                                (lambda (value update!) value))) 1)))

(define passthrough-nav
  (make-nav 'passthrough
            (lambda (value emit) (emit value))
            (lambda (value update) (update value))
            (lambda (value update!) (update! value))))

(mat navigator-review-custom-continuation-errors
  (null? (nav-select (nav/maybe (nav/path passthrough-nav (nav/key 'missing))) '()))

  (equal? '((a . 1))
          (nav-transform (nav/maybe (nav/path passthrough-nav (nav/key 'missing)))
                         add1 '((a . 1))))

  (let ([data (ht 'a 1)])
    (and (eq? data (nav-transform!
                   (nav/maybe (nav/path passthrough-nav (nav/key 'missing))) add1 data))
         (= 1 (ht-ref data 'a))))

  (null? (nav-select (nav/maybe (nav/path passthrough-nav passthrough-nav
                                          (nav/key 'missing))) '()))

  ;; A missing suffix outside the optional path must propagate through custom navigation.
  (error? (nav-select (nav/path (nav/maybe passthrough-nav) (nav/key 'missing)) '())))

(mat navigator-review-submap-duplicate-focuses
  (let ([path (nav/path (nav/submap 'a)
                        (nav/pred (lambda (entry) (= (cdr entry) 2))))]
        [data '((a . 1) (a . 2) (b . 3))])
    (and (null? (nav-select path data))
         (equal? data (nav-clearval path data))))

  (equal? '((a . 2) (b . 3))
          (nav-clearval (nav/submap 'a) '((a . 1) (a . 2) (b . 3))))

  (let ([calls 0] [data (list (cons 'a 1) (cons 'a 2))])
    (nav-clearval! (nav/path (nav/submap 'a)
                            (nav/pred (lambda (entry) (set! calls (+ calls 1)) #f))) data)
    (and (= calls 1) (equal? data '((a . 1) (a . 2)))))

  ;; Duplicate alist keys do not make direct entry removal possible in place.
  (error? (nav-clearval! (nav/submap 'a) (list (cons 'a 1) (cons 'a 2)))))

(mat navigator-review-submap-transform-locations
  (equal? '((a . 11) (a . 2) (b . 3))
          (nav-transform (nav/submap 'a)
                         (lambda (entry) (cons (car entry) (+ 10 (cdr entry))))
                         '((a . 1) (a . 2) (b . 3))))

  (equal? '((z . 11) (a . 2) (b . 3))
          (nav-transform (nav/submap 'a)
                         (lambda (entry) (cons 'z (+ 10 (cdr entry))))
                         '((a . 1) (a . 2) (b . 3))))

  (let ([seen '()])
    (let ([result (nav-transform/i
                   (nav/submap 'b 'a)
                   (lambda (index entry)
                     (set! seen (cons (cons index entry) seen))
                     (cons (if (eq? (car entry) 'a) 'b 'a) (+ 10 (cdr entry))))
                   '((a . 1) (a . 2) (b . 3)))])
      (and (equal? '((0 b . 3) (1 a . 1)) (reverse seen))
           (equal? '((b . 11) (a . 2) (a . 13)) result))))

  ;; An alist submap transformation must return an entry pair.
  (error? (nav-transform (nav/submap 'a) (lambda (entry) 42) '((a . 1)))))

(mat navigator-review-iterator-error-context
  (let ([iterator (nav-traverse->iter (nav/path nav/all nav/car)
                                     (list (cons 'a 1) 7))]
        [first-visits 0])
    (and
     (equal?
      '(a second-error)
      ;; Bound reentry if a later failure returns to the first caller's handler.
      (call/1cc
       (lambda (stop)
         (let ([first (guard (exn [else 'first-error]) (iter-next! iterator))])
           (set! first-visits (+ first-visits 1))
           (when (> first-visits 2) (stop 'reentered-first-caller))
           (let ([second (guard (exn [else 'second-error]) (iter-next! iterator))])
             (list first second))))))
     (= first-visits 1)
     (iter-end? (iter-next! iterator))
     (begin (iter-reset! iterator) #t)
     (eq? 'a (iter-next! iterator))
     ;; Reset recreates traversal; its invalid second focus must still raise.
     (guard (exn [else #t]) (iter-next! iterator) #f)
     (iter-end? (iter-next! iterator))
     (begin (iter-finalize! iterator) #t))))

(mat navigator-review-mutating-composition
  (let ([data (vector 1 2)])
    (and (eq? data (nav-transform! (nav/path nav/stay (nav/nth 0)) add1 data))
         (equal? '#(2 2) data)))

  (let ([data (vector 1)])
    (and (eq? data (nav-transform! (nav/path (nav/nth 0) nav/stay) add1 data))
         (equal? '#(2) data)))

  (let ([data (vector 1)])
    (and (eq? data
              (nav-transform!
               (nav/path (nav/nth 0) (nav/multi-path nav/empty-path nav/empty-path))
               add1 data))
         (equal? '#(3) data)))

  (let ([data (vector 1)])
    (and (eq? data (nav-transform! (nav/multi-path nav/empty-path) values data))
         (equal? '#(1) data)))

  ;; Mutating traversal cannot replace the caller's root reference.
  (error? (nav-transform! nav/empty-path add1 1))

  ;; Transparent stay still rejects replacement of the root itself.
  (error? (nav-transform! nav/stay add1 1)))

(mat navigator-review-children-boundaries
  ;; Fixed-size indexed children cannot disappear during mutation.
  (error? (nav-clearval! (nav/path nav/children nav/stay) (vector 1 2)))

  ;; List children cannot be removed in place through a returned root reference.
  (error? (nav-clearval! (nav/path nav/children nav/stay) (list 1 2)))

  (let ([data (vector (ht 'x 1 'y 2))])
    (and (eq? data (nav-clearval! (nav/path nav/children (nav/key 'x)) data))
         (not (hashtable-contains? (vector-ref data 0) 'x))
         (= 2 (ht-ref (vector-ref data 0) 'y))))

  (equal? '(#(2 3) 2 3)
          (nav-select nav/children (vector (cons 'a 1) (vector 2 3))))

  (let ([custom (make-nav 'nav/children
                         (lambda (value emit) (emit value))
                         (lambda (value update) (update value))
                         (lambda (value update!) (update! value)))])
    (equal? '(root) (nav-select custom 'root))))

(mat navigator-review-inexact-integer-indexes
  (equal? '(b) (nav-select (nav/nth 1.0) '#(a b c)))

  (equal? '#(a B c) (nav-setval (nav/nth 1.0) 'B '#(a b c)))

  (equal? '(a B c) (nav-setval (nav/nth 1.0) 'B '(a b c)))

  (equal? '#(a x x z) (nav-setval (nav/nth/default 3.0 'x) 'z '#(a)))

  (equal? '(a x x z) (nav-setval (nav/nth/default 3.0 'x) 'z '(a)))

  (equal? '(b d) (nav-select (nav/slice 1.0 5.0 2.0) '#(a b c d e)))

  (equal? '#(a B c D e)
          (nav-transform (nav/slice 1.0 5.0 2.0)
                         (lambda (x) (if (eq? x 'b) 'B 'D)) '#(a b c d e)))

  (equal? '(b) (nav-select (nav/path 1.0) '(a b c)))

  ;; Inexact integers are not valid bytevector replacement values.
  (error? (nav-transform nav/all (lambda (x) 1.0) (bytevector 0 0)))

  ;; Mutating bytevector updates must also reject inexact byte values.
  (error? (nav-transform! nav/all (lambda (x) 1.0) (bytevector 0 0))))

(mat navigator-review-dotted-diagnostics
  (equal? '(nav/key (a . b))
          (call-with-port (open-string-input-port
                           (capture-written nav-display (nav/key '(a . b)))) read))

  (equal? '(nav/nth/default 0 ((a . 1) (b . 2)))
          (call-with-port
           (open-string-input-port
            (capture-written nav-display (nav/nth/default 0 '((a . 1) (b . 2))))) read)))

(define-record-type (review-counted-indexed make-review-counted-indexed review-counted-indexed?)
  (fields items (mutable length-calls)))

(nav-register-indexed!
 review-counted-indexed?
 (lambda (box)
   (review-counted-indexed-length-calls-set!
    box (+ 1 (review-counted-indexed-length-calls box)))
   ;; Guard against unbounded work after the container's known end.
   (when (> (review-counted-indexed-length-calls box) 10)
     (error 'review "slice continued beyond the known container length"))
   (vector-length (review-counted-indexed-items box)))
 (lambda (box index) (vector-ref (review-counted-indexed-items box) index))
 (lambda (box index value)
   (let ([copy (vector-copy (review-counted-indexed-items box))])
     (vector-set! copy index value)
     (make-review-counted-indexed copy 0)))
 (lambda (box index value)
   (vector-set! (review-counted-indexed-items box) index value)))

(mat navigator-review-bounded-slice-selection
  (let ([box (make-review-counted-indexed '#(1 2 3) 0)])
    (equal? '(1 2 3) (nav-select (nav/slice 0 1000000) box)))

  (let ([box (make-review-counted-indexed '#(1 2 3) 0)])
    (null? (nav-select (nav/slice 10 (+ 1 (most-positive-fixnum))) box)))

  (equal? '(1 3) (nav-select (nav/slice 0 1000 2) '(1 2 3)))

  (equal? '(0 1 2) (nav-select nav/all '(0 1 2)))

  (equal? '((0 . 4) (1 . 5) (2 . 6)) (nav-select nav/entries '(4 5 6))))

(define review-lexical-path
  (lambda ()
    (nav/letrec ([leaf (nav/key 'a)]
                 [outer leaf])
      (nav/letrec ([leaf (nav/key 'b)]
                   [inner outer])
        inner))))

(mat navigator-review-enclosing-recursive-bindings
  (equal? '(1)
          (nav-select (nav/letrec ([outer (nav/key 'a)])
                        (nav/letrec ([inner outer]) inner)) '((a . 1))))

  (equal? '(1) (nav-select (review-lexical-path) '((a . 1) (b . 2))))

  (equal? '((a . 11) (b . 2))
          (nav-transform (review-lexical-path) (lambda (x) (+ 10 x)) '((a . 1) (b . 2))))

  (let ([data (ht 'a 1 'b 2)])
    (and (eq? data (nav-transform! (review-lexical-path) (lambda (x) (+ 10 x)) data))
         (= 11 (ht-ref data 'a))
         (= 2 (ht-ref data 'b))))

  (equal? '((b . 2)) (nav-clearval (review-lexical-path) '((a . 1) (b . 2))))

  (let ([data (ht 'a 1 'b 2)])
    (and (eq? data (nav-clearval! (review-lexical-path) data))
         (not (hashtable-contains? data 'a))
         (= 2 (ht-ref data 'b)))))

(mat navigator-review-terminal-recursive-children
  (equal? '(#(b) b) (nav-select (nav/rec visit nav/children) '#(a #(b))))

  (equal? '(#(b) b)
          (nav-select (nav/letrec ([visit nav/children]) visit) '#(a #(b))))

  (null? (nav-select (nav/path (nav/rec visit nav/children) nav/none) '#(a #(b)))))

(mat navigator-review-hashtable-callback-errors
  ;; Hash callback navigation errors are application failures, not optional absence.
  (let ([broken? #f])
    (let ([table (make-hashtable
                  (lambda (key)
                    (if broken? (nav-select-one (nav/key 'missing) '()) 0)) equal?)])
      (hashtable-set! table 'a 1)
      (set! broken? #t)
      (guard (exn [else #t])
        (nav-select (nav/maybe (nav/key 'a)) table)
        #f)))

  ;; Equivalence callback navigation errors must also survive optional selection.
  (let ([broken? #f])
    (let ([table (make-hashtable
                  (lambda (key) 0)
                  (lambda (left right)
                    (if broken? (nav-select-one (nav/key 'missing) '())
                        (string=? left right))))])
      (hashtable-set! table (string-copy "a") 1)
      (set! broken? #t)
      (guard (exn [else #t])
        (nav-select (nav/maybe (nav/key (string-copy "a"))) table)
        #f)))

  ;; Setter hash errors must not turn a failed transformation into a silent no-op.
  (let ([broken? #f])
    (let ([table (make-hashtable
                  (lambda (key)
                    (if broken? (nav-select-one (nav/key 'missing) '()) 0)) equal?)])
      (hashtable-set! table 'a 1)
      (guard (exn [else #t])
        (nav-transform (nav/maybe (nav/key 'a))
                       (lambda (x) (set! broken? #t) (add1 x)) table)
        #f)))

  ;; A hash failure during keyed removal is not optional absence either.
  (let ([broken? #f])
    (let ([table (make-hashtable
                  (lambda (key)
                    (if broken? (nav-select-one (nav/key 'missing) '()) 0)) equal?)])
      (hashtable-set! table 'a 1)
      (guard (exn [else #t])
        (nav-clearval
         (nav/maybe (nav/path (nav/key 'a)
                             (nav/pred (lambda (x) (set! broken? #t) #t)))) table)
        #f))))

(mat navigator-review-submap-replacement-membership
  (let* ([source (ht 'a 1 'b 2)]
         [result (nav-transform (nav/path (nav/submap 'a) nav/keys)
                                (lambda (key) 'z) source)])
    (and (= 2 (hashtable-size result))
         (not (hashtable-contains? result 'a))
         (= 1 (ht-ref result 'z))
         (= 2 (ht-ref result 'b))
         (= 1 (ht-ref source 'a))))

  (let* ([source (ht 'a 1 'b 2)]
         [result (nav-transform (nav/submap 'a)
                                (lambda (selected)
                                  (hashtable-set! selected 'b 20)
                                  (hashtable-set! selected 'z 30)
                                  selected) source)])
    (and (= 3 (hashtable-size result))
         (= 1 (ht-ref result 'a))
         (= 20 (ht-ref result 'b))
         (= 30 (ht-ref result 'z))
         (= 2 (ht-ref source 'b))))

  (let* ([source (ht 'a 1 'b 2)]
         [result (nav-transform (nav/path (nav/submap 'a) nav/keys)
                                (lambda (key) 'b) source)])
    (and (= 1 (hashtable-size result))
         (= 1 (ht-ref result 'b))
         (= 2 (ht-ref source 'b))))

  (let* ([source (make-kvbox '((a . 1) (b . 2)))]
         [result (nav-transform (nav/submap 'a)
                                (lambda (selected) '((z . 10) (b . 20))) source)])
    (and (= 10 (kvbox-ref result 'z #f))
         (= 20 (kvbox-ref result 'b #f))
         (not (kvbox-ref result 'a #f))
         (equal? '((a . 1) (b . 2)) (kvbox-entries source))))

  ;; Replacing a submap requires another keyed map, rather than an arbitrary scalar.
  (error? (nav-transform (nav/submap 'a) (lambda (selected) 42) (ht 'a 1))))
