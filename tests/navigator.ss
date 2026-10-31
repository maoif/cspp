(import (cspp) (cspp concurrency fiber))

;; A type/range error must name its procedure rather than report a memory fault.
(define navigator-type-error?
  (lambda (run)
    (guard (condition [(who-condition? condition) #t] [else #f])
      (run)
      #f)))

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
 (lambda (box items) (make-setbox items))
 (lambda (box items) (setbox-items-set! box items) box)
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
       (and (= 2 (length selected))
            (fxvector? (car selected))
            (flvector? (cadr selected))
            (equal? '(1 2) (fxvector->list* (car selected)))
            (equal? '(1.5 2.5) (flvector->list* (cadr selected)))))
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
     (equal? '(a (1 b) #(2 (c 3)))
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
       (and (= 3 (length selected))
            (list? (car selected))
            (eq? 'atom (cadr selected))
            (list? (caddr selected))))
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

  (equal? '((a . 1) #(2 3))
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
  (equal? '(a #(b)) (nav-select (nav/rec visit nav/children) '#(a #(b))))

  (equal? '(a #(b))
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

(mat navigator-review-submap-repeated-clear-keys
  (let* ([source '((a . 1) (a . 2) (b . 3))]
         [result (nav-clearval (nav/submap 'a 'a) source)])
    (and (equal? '((a . 2) (b . 3)) result)
         (equal? '((a . 1) (a . 2) (b . 3)) source)))

  (equal? '((b . 3))
          (nav-clearval (nav/submap 'a 'a) '((a . 1) (b . 3))))

  (let ([seen '()])
    (let ([result
           (nav-clearval
            (nav/path (nav/submap 'b 'a 'b 'c)
                      (nav/pred (lambda (entry)
                                  (set! seen (cons entry seen))
                                  #t)))
            '((a . 1) (b . 2) (b . 3) (c . 4) (d . 5)))])
      (and (equal? '((b . 3) (d . 5)) result)
           (equal? '((c . 4) (b . 2) (a . 1) (b . 2)) seen))))

  (let ([calls 0])
    (let ([result
           (nav-clearval
            (nav/path (nav/submap 'a 'a)
                      (nav/pred (lambda (entry)
                                  (set! calls (fx1+ calls))
                                  (fx= calls 1))))
            '((a . 1) (a . 2) (b . 3)))])
      (and (fx= calls 2)
           (equal? '((a . 1) (a . 2) (b . 3)) result))))

  (let ([calls 0])
    (let ([result
           (nav-clearval
            (nav/path (nav/submap 'a 'a)
                      (nav/pred (lambda (entry)
                                  (set! calls (fx1+ calls))
                                  (fx= calls 2))))
            '((a . 1) (a . 2) (b . 3)))])
      (and (fx= calls 2)
           (equal? '((a . 2) (b . 3)) result))))

  (equal? '((a . 1) (a . 2) (b . 3))
          (nav-clearval (nav/path (nav/submap 'a 'a) nav/none)
                        '((a . 1) (a . 2) (b . 3)))))

(mat navigator-review-wrapper-selection-context
  (andmap
   (lambda (wrap)
     (equal? '(1)
             (nav-select (nav/path (wrap nav/children) (nav/pred number?)) '#(1 #(2)))))
   (list nav/maybe nav/must
         (lambda (path) (nav/choice path))
         (lambda (path) (nav/multi-path path))
         (lambda (path) (nav/if nav/stay path nav/none))))

  (equal? '(1 #(2))
          (nav-select (nav/path (nav/maybe nav/stay) nav/children) '#(1 #(2))))

  (equal? '(1)
          (iter->list (nav-traverse->iter
                       (nav/path (nav/maybe nav/children) (nav/pred number?)) '#(1 #(2)))))

  (let ([inner '()])
    (and (equal? '(1)
                 (nav-select
                  (nav/path (nav/maybe (nav/pred
                                       (lambda (value)
                                         (set! inner (nav-select nav/children '#(#(3))))
                                         #t)))
                            nav/children (nav/pred number?))
                  '#(1 #(2))))
         (equal? '(#(3)) inner)))

  (equal? '(#(1))
          (nav-select nav/children (make-ixbox '#(#(1)))))

  (equal? '(#(1))
          (nav-select nav/children (make-kvbox '((a . #(1)))))))

(mat navigator-review-required-operation-single-pass
  (let ([calls 0])
    (let ([result (nav-transform
                   (nav/must (nav/pred (lambda (value)
                                         (set! calls (fx1+ calls))
                                         (fx= calls 1)))) add1 7)])
      (and (= 8 result) (fx= calls 1))))

  (let ([calls 0] [source (vector 7)])
    (and (eq? source
              (nav-transform!
               (nav/path (nav/nth 0)
                         (nav/must (nav/pred (lambda (value)
                                              (set! calls (fx1+ calls))
                                              (fx= calls 1))))) add1 source))
         (equal? '#(8) source)
         (fx= calls 1)))

  (let ([calls 0])
    (let ([result (nav-clearval
                   (nav/path (nav/key 'a)
                             (nav/must (nav/pred (lambda (value)
                                                  (set! calls (fx1+ calls))
                                                  (fx= calls 1)))))
                   '((a . 1) (b . 2)))])
      (and (equal? '((b . 2)) result) (fx= calls 1))))

  (let ([calls 0] [source (ht 'a 1 'b 2)])
    (and (eq? source
              (nav-clearval!
               (nav/path (nav/key 'a)
                         (nav/must (nav/pred (lambda (value)
                                              (set! calls (fx1+ calls))
                                              (fx= calls 1))))) source))
         (not (hashtable-contains? source 'a))
         (fx= calls 1)))

  ;; The actual transformation must reject a required path with no focuses.
  (error? (nav-transform (nav/must nav/none) add1 7))

  ;; The actual clearing pass must reject an empty required path.
  (error? (nav-clearval (nav/must nav/none) '(1))))

(mat navigator-review-clear-suffix-continuations
  (andmap
   (lambda (step)
     (equal? '#(((y . 2)))
             (nav-clearval (nav/path step (nav/key 'x)) '#(((x . 1) (y . 2))))))
   (list nav/all nav/values nav/last (nav/nth/default 0 #f) (nav/slice 0 1)))

  (andmap
   (lambda (step)
     (equal? '#(1) (nav-clearval (nav/path step nav/none) '#(1))))
   (list nav/all nav/values nav/last (nav/nth/default 0 #f) (nav/slice 0 1)))

  (equal? '(((y . 2)) . tail)
          (nav-clearval (nav/path nav/car (nav/key 'x)) '(((x . 1) (y . 2)) . tail)))

  (equal? '(head (y . 2))
          (nav-clearval (nav/path nav/cdr (nav/key 'x)) '(head (x . 1) (y . 2))))

  (equal? '(1 3) (nav-clearval (nav/path nav/all (nav/pred even?)) '(1 2 3 4)))

  (equal? '(0 2 4) (nav-clearval (nav/slice 1 5 2) '(0 1 2 3 4)))

  (equal? '(1 2) (nav-clearval nav/last '(1 2 3)))

  (let* ([source (ht 'a '((x . 1) (y . 2)))]
         [result (nav-clearval (nav/path nav/values (nav/key 'x)) source)])
    (and (equal? '((y . 2)) (ht-ref result 'a))
         (equal? '((x . 1) (y . 2)) (ht-ref source 'a))))

  (andmap
   (lambda (step)
     (let* ([child (ht 'x 1 'y 2)] [source (vector child)])
       (and (eq? source (nav-clearval! (nav/path step (nav/key 'x)) source))
            (not (hashtable-contains? child 'x))
            (= 2 (ht-ref child 'y)))))
   (list nav/all nav/values nav/last (nav/nth/default 0 #f) (nav/slice 0 1)))

  (let* ([child (ht 'x 1 'y 2)] [source (cons child 'tail)])
    (and (eq? source (nav-clearval! (nav/path nav/car (nav/key 'x)) source))
         (not (hashtable-contains? child 'x))
         (eq? child (car source))))

  (let* ([child (ht 'x 1 'y 2)] [source (cons 'head child)])
    (and (eq? source (nav-clearval! (nav/path nav/cdr (nav/key 'x)) source))
         (not (hashtable-contains? child 'x))
         (eq? child (cdr source))))

  (equal? '() (nav-clearval nav/all '()))

  ;; A fixed-size sequence still cannot lose a directly selected element.
  (error? (nav-clearval nav/all '#(1 2)))

  ;; Mutable list traversal cannot replace the caller's list spine.
  (error? (nav-clearval! nav/all (list 1 2)))

  ;; A pair field cannot be removed, although its nested values can be cleared.
  (error? (nav-clearval nav/car '(1 . 2))))

(mat navigator-review-shared-bulk-updates
  (let* ([source (make-ixbox '#(1 2 3))]
         [result (nav-transform nav/children add1 source)])
    (and (equal? '#(2 3 4) (ixbox-items result))
         (equal? '#(1 2 3) (ixbox-items source))))

  (let* ([source (make-kvbox '((a . 1) (b . 2)))]
         [result (nav-transform nav/children add1 source)])
    (and (equal? '((a . 2) (b . 3)) (kvbox-entries result))
         (equal? '((a . 1) (b . 2)) (kvbox-entries source))))

  (let* ([source (make-hashtable string-hash string=?)]
         [key (string-copy "a")])
    (hashtable-set! source key 1)
    (let ([result (nav-transform nav/all add1 source)])
      (and (= 2 (hashtable-ref result (string-copy "a") 0))
           (= 1 (hashtable-ref source key 0))
           (eq? string=? (hashtable-equivalence-function result)))))

  (let ([source (list 0 1 2 3 4)] [visited '()])
    (and (eq? source (nav-transform! (nav/slice 1 5 2)
                                    (lambda (value) (set! visited (cons value visited))
                                      (+ value 10)) source))
         (equal? '(0 11 2 13 4) source)
         (equal? '(3 1) visited)))

  (equal? '(1 2 3) (nav-transform (nav/slice 0 3 1000000000000000000000) add1 '(0 2 3)))

  (let ([source (list 0 2 3)])
    (and (eq? source (nav-transform! (nav/slice 0 3 1000000000000000000000) add1 source))
         (equal? '(1 2 3) source))))

(mat navigator-review-clearing-absent-defaults
  (let ([source (vector 1)] [seen '()])
    (and (eq? source
              (nav-clearval (nav/path (nav/nth/default 3 #f)
                                     (nav/pred (lambda (value)
                                                 (set! seen (cons value seen)) #t))) source))
         (equal? '(#f) seen)
         (equal? '#(1) source)))

  (let ([source (vector 1)])
    (eq? source (nav-clearval! (nav/path (nav/nth/default 3 #f) nav/none) source)))

  (let ([source '((a . 1))])
    (eq? source (nav-clearval (nav/path (nav/key/default 'missing #f) nav/none) source)))

  (let ([source (ht 'a 1)] [seen '()])
    (and (eq? source
              (nav-clearval! (nav/path (nav/key/default 'missing #f)
                                      (nav/pred (lambda (value)
                                                  (set! seen (cons value seen)) #t))) source))
         (equal? '(#f) seen)
         (= 1 (hashtable-size source))))

  ;; Errors from a suffix still propagate when it runs on a default focus.
  (error? (nav-clearval (nav/path (nav/nth/default 3 #f) (nav/key 'x)) '#(1))))

(mat navigator-review-immutable-mutation
  ;; Mutable updates must reject immutable vectors before changing shared storage.
  (let ([value (immutable-vector 1)])
    (and (guard (exn [else #t]) (nav-transform! (nav/nth 0) add1 value) #f)
         (= 1 (vector-ref value 0))))

  ;; A missing optional suffix must not create a default location.
  (equal? '() (nav-transform (nav/path (nav/key/default 'x 0) nav/none) add1 '())))

(mat navigator-review-choice-single-traversal
  (equal? '(1 2)
          (nav-select (nav/path (nav/choice nav/children nav/stay) (nav/pred number?))
                      '#(1 2)))

  (call-with-port (open-string-input-port "1 2")
    (lambda (port)
      (and (equal? '(1) (nav-select (nav/choice (nav/getter 'read get-datum)) port))
           (= 2 (get-datum port)))))

  (let ([calls 0])
    (and (equal? '(2)
                 (nav-select (nav/choice (nav/pred (lambda (value)
                                                     (set! calls (+ calls 1))
                                                     (= calls 1)))) 2))
         (= calls 1)))

  (equal? '#(2 3)
          (nav-transform (nav/path (nav/choice nav/children nav/stay) (nav/pred number?))
                         add1 '#(1 2))))

(mat navigator-review-optional-siblings
  (let ([path (nav/maybe (nav/path nav/all (nav/key 'x)))])
    (and (equal? '(1 3) (nav-select path '#(((x . 1)) ((y . 2)) ((x . 3)))))
         (equal? '#(((x . 2)) ((y . 2)) ((x . 4)))
                 (nav-transform path add1 '#(((x . 1)) ((y . 2)) ((x . 3)))))
         (equal? '#(() ((y . 2)) ())
                 (nav-clearval path '#(((x . 1)) ((y . 2)) ((x . 3))))))))

(mat navigator-review-default-no-focus
  (andmap
   (lambda (suffix)
     (and (equal? '#(1) (nav-transform (nav/path (nav/nth/default 3 0) suffix) add1 '#(1)))
          (equal? '() (nav-transform (nav/path (nav/key/default 'x 0) suffix) add1 '()))
          (let ([data (ht)])
            (and (eq? data (nav-transform! (nav/path (nav/key/default 'x 0) suffix) add1 data))
                 (= 0 (hashtable-size data))))
          (let ([data (vector 1)])
            (eq? data (nav-transform! (nav/path (nav/nth/default 3 0) suffix) add1 data)))))
   (list nav/none (nav/pred odd?))))

(mat navigator-review-entries-clear-suffix
  (equal? '#(((y . 2)))
          (nav-clearval (nav/path nav/entries nav/cdr (nav/key 'x)) '#(((x . 1) (y . 2)))))

  (equal? '#(1) (nav-clearval (nav/path nav/entries nav/none) '#(1)))

  (let ([data (vector (ht 'x 1 'y 2))])
    (and (eq? data (nav-clearval! (nav/path nav/entries nav/cdr (nav/key 'x)) data))
         (not (hashtable-contains? (vector-ref data 0) 'x))))

  ;; Removing an entry itself is still unsupported.
  (error? (nav-clearval nav/entries '#(1))))

(mat navigator-review-recursive-binding-identity
  (equal? '(1)
          (nav-select
           (nav/letrec ([outer (nav/key 'a)])
             (let ([saved outer])
               (nav/letrec ([outer (nav/key 'b)]) saved)))
           '((a . 1) (b . 2))))

  (let ([saved #f])
    (nav/letrec ([outer (nav/key 'a)])
      (begin (set! saved (nav/letrec ([inner outer]) inner)) nav/stay))
    (equal? '(1) (nav-select saved '((a . 1)))))

  ;; A fresh public operation cannot use a symbolic reference from the caller's group.
  (let ([failed? #f])
    (nav-select (nav/letrec ([outer nav/stay])
                  (nav/pred (lambda (value)
                              (set! failed?
                                    (guard (exn [else #t])
                                      (nav-select (make-nav-ref 'outer) value) #f))
                              #t))) 1)
    failed?))

(define-record-type (review-length-box make-review-length-box review-length-box?)
  (fields length))
(nav-register-indexed! review-length-box? review-length-box-length
                      (lambda (box index) index)
                      (lambda (box index value) box)
                      (lambda (box index value) (void)))

(mat navigator-review-indexed-length-contract
  (equal? '(0 1 2) (nav-select nav/all (make-review-length-box 3.0)))

  (let ([seen '()] [data (make-review-length-box 3.0)])
    (and (eq? data (nav-transform nav/all
                                 (lambda (value) (set! seen (cons value seen)) value) data))
         (equal? '(2 1 0) seen)))

  ;; Invalid protocol lengths must fail before indexed iteration can diverge.
  (error? (nav-select nav/all (make-review-length-box -1))))

(mat navigator-review-submap-key-policy
  (let ([data (make-treemap string-ci=? string-ci<?)])
    (treemap-set! data (string-copy "alpha") 1)
    (and (equal? '(1) (nav-select (nav/path (nav/submap (string-copy "ALPHA"))
                                          (nav/key (string-copy "alpha"))) data))
         (= 2 (treemap-ref
               (nav-transform (nav/path (nav/submap (string-copy "ALPHA"))
                                        (nav/key (string-copy "alpha"))) add1 data)
               (string-copy "alpha"))))))

(mat navigator-review-set-reindex
  (let* ([member (ht 'x 1 'y 2)]
         [data (make-hashset hashtable-size eq?)])
    (hashset-add! data member)
    (and (eq? data (nav-clearval! (nav/path nav/all (nav/key 'x)) data))
         (hashset-contains? data member)
         (= 1 (hashtable-size member))))

  (equal? '() (hashset->list (nav-clearval nav/all (hashset 1 2 3))))

  (equal? '() (treeset->list (nav-clearval nav/all (treeset = < 1 2 3)))))

(mat navigator-review-readonly-ancestors
  (let* ([child (ht 'x 1)] [root (immutable-vector child)])
    (and (eq? root (nav-transform! (nav/path (nav/nth 0) (nav/key 'x)) add1 root))
         (= 2 (ht-ref child 'x))))

  (let ([root (immutable-vector 1)])
    (eq? root (nav-transform! (nav/path nav/all nav/none) add1 root)))

  ;; Immutable character/byte storage must reject actual replacements.
  (let ([text (string->immutable-string (string-copy "a"))])
    (and (guard (exn [else #t]) (nav-transform! nav/all char-upcase text) #f)
         (string=? text "a")))

  ;; Immutable bytevectors must reject writes in release builds too.
  (let ([bytes (bytevector->immutable-bytevector (bytevector 1))])
    (and (guard (exn [else #t]) (nav-transform! nav/all add1 bytes) #f)
         (= 1 (bytevector-u8-ref bytes 0)))))

(define-record-type (review-dual-box make-review-dual-box review-dual-box?) (fields value))
(nav-register-indexed! review-dual-box? (lambda (box) 1)
                      (lambda (box index) (review-dual-box-value box))
                      (lambda (box index value) (make-review-dual-box value))
                      (lambda (box index value) (void)))
(nav-register-keyed! review-dual-box?
                    (lambda (box key default)
                      (if (eq? key 'a) (review-dual-box-value box) default))
                    (lambda (box key value) (make-review-dual-box value))
                    (lambda (box key value) (void))
                    (lambda (box key) box) (lambda (box key) (void))
                    (lambda (box) (list (cons 'a (review-dual-box-value box)))))

(mat navigator-review-dual-protocol-entries
  (let ([seen '()])
    (let ([result (nav-transform nav/entries
                                 (lambda (entry) (set! seen (cons entry seen))
                                   (cons (car entry) (+ 1 (cdr entry))))
                                 (make-review-dual-box 7))])
      (and (equal? '((0 . 7)) seen) (= 8 (review-dual-box-value result)))))

  (equal? '((0 . 7)) (nav-select nav/entries (make-review-dual-box 7))))

(mat navigator-review-recursive-snapshot
  (let* ([body (list (nav/key 'a))] [path (nav/rec visit body)])
    (set-car! body (nav/key 'b))
    (equal? '(1) (nav-select path '((a . 1) (b . 2)))))

  (equal? (nav->datum (nav/rec visit (list (nav/pred number?))))
          (nav->datum (nav/rec visit (list (nav/pred number?))))))

(mat navigator-review-specialized-submap
  (let* ([source (fxtreemap fx= fx< '(1 . 10) '(2 . 20))]
         [result (nav-select-one (nav/submap 1) source)])
    (and (fxtreemap? result) (= 1 (fxtreemap-size result))
         (= 10 (fxtreemap-ref result 1)) (= 2 (fxtreemap-size source)))))

(mat navigator-review-indexed-length-rounded-boundary
  ;; Rounding the fixnum maximum to inexact can produce the first invalid fixnum.
  (error? (nav-select-first nav/all
                           (make-review-length-box
                            (exact->inexact (+ 1 (most-positive-fixnum)))))))

(mat navigator-review-public-operation-scopes
  (andmap
   (lambda (select)
     (let ([isolated? #f])
       (nav-select
        (nav/letrec ([outer nav/stay])
          (nav/pred (lambda (value)
                      (set! isolated?
                            (guard (exn [else #t])
                              (select (make-nav-ref 'outer) value) #f))
                      #t))) 1)
       isolated?))
   (list nav-select nav-selected? nav-select-first)))

(mat navigator-review-optional-multiple-keys
  (let ([path (nav/maybe (nav/key-values 'x 'missing 'y))])
    (and (equal? '(1 3) (nav-select path '((x . 1) (y . 3))))
         (equal? '((x . 2) (y . 4)) (nav-transform path add1 '((x . 1) (y . 3))))
         (null? (nav-clearval path '((x . 1) (y . 3))))))

  (let ([path (nav/maybe (nav/submap 'x 'missing 'y))])
    (and (equal? '((x . 1) (y . 3)) (nav-select path '((x . 1) (y . 3))))
         (equal? '((x . 2) (y . 4))
                 (nav-transform path (lambda (entry) (cons (car entry) (+ 1 (cdr entry))))
                                '((x . 1) (y . 3))))
         (null? (nav-clearval path '((x . 1) (y . 3)))))))

(mat navigator-review-symbolic-lexical-environments
  (equal? '(1)
          (nav-select (nav/letrec ([outer (nav/key 'a)])
                        (nav/letrec ([inner (make-nav-ref 'outer)]) inner))
                      '((a . 1))))

  (let ([saved #f])
    (nav/letrec ([outer (nav/key 'a)])
      (begin (set! saved (nav/letrec ([inner (make-nav-ref 'outer)]) inner)) nav/stay))
    (equal? '(1) (nav-select saved '((a . 1))))))

(mat navigator-review-readonly-map-ancestor
  (let* ([child (ht 'x 1)] [root (hashtable-copy (ht 'child child) #f)])
    (and (eq? root (nav-transform! (nav/path (nav/key 'child) (nav/key 'x)) add1 root))
         (= 2 (ht-ref child 'x))))

  (let* ([child (ht 'x 1 'y 2)] [root (hashtable-copy (ht 'child child) #f)])
    (and (eq? root (nav-clearval! (nav/path nav/entries nav/cdr (nav/key 'x)) root))
         (not (hashtable-contains? child 'x)))))

(mat navigator-review-treeset-member-reindex
  (let* ([one (ht 'a 1)] [two (ht 'a 1 'x 2)] [three (ht 'a 1 'b 2 'x 3)]
         [data (make-treeset (lambda (left right) (= (hashtable-size left) (hashtable-size right)))
                             (lambda (left right) (< (hashtable-size left) (hashtable-size right))))])
    (for-each (lambda (member) (treeset-add! data member)) (list one two three))
    (and (eq? data (nav-clearval! (nav/path nav/all (nav/maybe (nav/key 'x))) data))
         (= 2 (treeset-size data))
         (equal? '(1 2) (map hashtable-size (treeset->list data))))))

(mat navigator-review-entries-removal-boundary
  ;; Clearing an entire alist entry is unsupported; the removal marker is itself a pair.
  (error? (nav-clearval nav/entries '((a . 1))))

  ;; A hashtable entry cannot be replaced by the internal removal marker either.
  (error? (nav-clearval nav/entries (ht 'a 1)))

  ;; Mutable entry clearing must reject removal before corrupting the original pair.
  (error? (nav-clearval! nav/entries (list (cons 'a 1))))

  ;; Direct removal from a hashtable through entries remains unsupported in mutation.
  (error? (nav-clearval! nav/entries (ht 'a 1))))

(define review-delegated-all
  (make-nav 'delegated-all
            (lambda (value emit) (nav-traverse nav/all value emit))
            (lambda (value update) (nav-transform nav/all update value))
            (lambda (value update!) (nav-transform! nav/all update! value))))

(mat navigator-delegated-continuation-scopes
  (let ([path (nav/letrec ([leaf (nav/key 'x)])
                (nav/path review-delegated-all (make-nav-ref 'leaf)))]
        [data (list (list (cons 'x 1)) (list (cons 'x 2)))])
    (and (equal? '(1 2) (nav-select path data))
         (equal? '(((x . 2)) ((x . 3))) (nav-transform path add1 data))
         (eq? data (nav-transform! path add1 data))
         (equal? '(((x . 2)) ((x . 3))) data)))

  (let ([path (nav/maybe (nav/path review-delegated-all (nav/key 'x)))]
        [data (list (list (cons 'x 1)) (list (cons 'y 2)) (list (cons 'x 3)))])
    (and (equal? '(1 3) (nav-select path data))
         (equal? '(((x . 2)) ((y . 2)) ((x . 4))) (nav-transform path add1 data))
         (eq? data (nav-transform! path add1 data))
         (equal? '(((x . 2)) ((y . 2)) ((x . 4))) data)))

  (let ([wrapper (make-nav 'delegate-stay
                          (lambda (value emit) (nav-traverse nav/stay value emit))
                          (lambda (value update) (nav-transform nav/stay update value))
                          (lambda (value update!) (update! value)))])
    (equal? '((x . 1))
            (nav-transform (nav/path (nav/key/default 'x 0) wrapper) add1 '()))))

(mat navigator-ordered-callbacks
  (equal? '((a . 0) (a . 1) (b . 2))
          (nav-transform/i (nav/path nav/entries nav/cdr) (lambda (index value) index)
                           '((a . 1) (a . 2) (b . 3))))

  (equal? '((0 . 1) (1 . 2) (2 . 3))
          (nav-transform/i nav/keys (lambda (index key) index) '((a . 1) (b . 2) (c . 3))))

  (andmap
   (lambda (source)
     (let ([selected (nav-select nav/all source)] [visited (make-list-builder)])
       (nav-transform nav/all (lambda (value) (visited value) value) source)
       (and (equal? selected (visited))
            (let ([visited (make-list-builder)])
              (nav-transform! nav/all (lambda (value) (visited value) value) source)
              (equal? selected (visited))))))
   (list (hashset 1 2 3) (treeset = < 1 2 3)))

  (let ([source (treemap = < '(1 . a) '(2 . b) '(3 . c))]
        [visited (make-list-builder)])
    (nav-transform nav/entries (lambda (entry) (visited (car entry)) entry) source)
    (equal? '(1 2 3) (visited))))

(mat navigator-default-value-is-public-data
  (equal? '(#f) (nav-select (nav/key/default 'missing) '()))

  (equal? '((missing . #f)) (nav-transform (nav/key/default 'missing) values '()))

  (let* ([result (nav-transform (nav/key/default 'missing) values (ht))]
         [fallback (nav-select-one (nav/key/default 'other) '())])
    (and (hashtable-contains? result 'missing)
         (equal? '(#f) (nav-select (nav/key 'missing) result))
         (equal? '((present . #f))
                 (nav-setval (nav/key 'present) fallback '((present . 1)))))))

(mat navigator-key-clear-suffixes
  (equal? '((a . 1)) (nav-clearval (nav/path nav/keys nav/none) '((a . 1))))

  (null? (nav-clearval (nav/path nav/keys nav/none) '()))

  (equal? '#(1 2) (nav-clearval (nav/path nav/keys nav/none) '#(1 2)))

  (let* ([key (ht 'x 1 'y 2)] [source (make-eq-hashtable)])
    (hashtable-set! source key 'payload)
    (let ([result (nav-clearval (nav/path nav/keys (nav/key 'x)) source)])
      (and (= 2 (hashtable-size key))
           (let ([new-key (nav-select-one nav/keys result)])
             (and (= 1 (hashtable-size new-key))
                  (eq? 'payload (hashtable-ref result new-key #f)))))))

  ;; A key itself still cannot be removed through the read-only keys navigator.
  (error? (nav-clearval nav/keys '((a . 1)))))

(define review-map-key-size=?
  (lambda (left right) (= (hashtable-size left) (hashtable-size right))))
(define review-map-key-size<?
  (lambda (left right) (< (hashtable-size left) (hashtable-size right))))

(mat navigator-mutated-map-keys-reindex
  (andmap
   (lambda (path)
     (let ([key (ht 'x 1 'y 2)] [source (make-hashtable hashtable-size eq?)])
       (hashtable-set! source key 'payload)
       (and (eq? source (nav-clearval! path source))
            (= 1 (hashtable-size source))
            (= 1 (hashtable-size key))
            (eq? 'payload (hashtable-ref source key #f)))))
   (list (nav/path nav/entries nav/car (nav/key 'x))
         (nav/path nav/keys (nav/key 'x))))

  (let* ([one (ht 'a 1)] [two (ht 'a 1 'x 2)] [three (ht 'a 1 'b 2 'x 3)]
         [source (make-treemap review-map-key-size=? review-map-key-size<?)])
    (for-each (lambda (key) (treemap-set! source key 'payload)) (list one two three))
    (and (eq? source (nav-clearval! (nav/path nav/entries nav/car
                                            (nav/maybe (nav/key 'x))) source))
         (= 2 (treemap-size source))
         (equal? '(1 2) (map hashtable-size (nav-select nav/keys source)))))

  ;; A suffix failure after key mutation must still leave the map correctly indexed.
  (let ([key (ht 'x 1 'y 2)] [source (make-hashtable hashtable-size eq?)])
    (hashtable-set! source key 'payload)
    (and (guard (exn [else #t])
           (nav-clearval! (nav/path nav/keys
                                   (nav/pred (lambda (key)
                                               (hashtable-delete! key 'x)
                                               (error 'key "suffix failed")))) source)
           #f)
         (= 1 (hashtable-size source))
         (eq? 'payload (hashtable-ref source key #f)))))

(mat navigator-submap-canonical-keys
  (let* ([stored (vector 7 'stored)] [query (vector 7 'query)]
         [same? (lambda (left right) (= (vector-ref left 0) (vector-ref right 0)))]
         [less? (lambda (left right) (< (vector-ref left 0) (vector-ref right 0)))]
         [table (make-hashtable (lambda (key) (vector-ref key 0)) same?)]
         [tree (make-treemap same? less?)])
    (hashtable-set! table stored 'payload)
    (treemap-set! tree stored 'payload)
    (andmap
     (lambda (source)
       (let ([calls 0])
         (and (eq? stored (nav-select-one (nav/path (nav/submap query) nav/keys) source))
              (let ([result (nav-transform (nav/path (nav/submap query) nav/none)
                                           (lambda (value) (set! calls (+ calls 1)) value)
                                           source)])
                (and (= 0 calls) (eq? stored (nav-select-one nav/keys result)))))))
     (list table (hashtable-copy table #f) tree))))

(mat navigator-optional-clear-unsupported
  (andmap
   (lambda (path)
     (and (= 7 (nav-clearval (nav/maybe path) 7))
          (= 7 (nav-clearval! (nav/maybe path) 7))))
   (list nav/all nav/values))

  (= 7 (nav-clearval (nav/choice nav/all nav/none) 7))

  ;; Fixed-size focuses are present: removing them must still fail inside nav/maybe.
  (error? (nav-clearval (nav/maybe nav/all) '#(1))))

(mat navigator-mutating-entry-identity
  (let* ([child (ht 'x 1 'y 2)] [entry (cons 'a child)] [source (list entry)]
         [path (nav/path nav/entries (nav/pred (lambda (value) (eq? value entry)))
                         nav/cdr (nav/key 'x))])
    (and (eq? source (nav-clearval! path source))
         (eq? entry (car source))
         (not (hashtable-contains? child 'x)))))

(define-record-type review-legacy-map (fields table))
(nav-register-keyed!
 review-legacy-map?
 (lambda (value key default) (hashtable-ref (review-legacy-map-table value) key default))
 (lambda (value key replacement)
   (let ([copy (hashtable-copy (review-legacy-map-table value) #t)])
     (hashtable-set! copy key replacement)
     (make-review-legacy-map copy)))
 (lambda (value key replacement)
   (hashtable-set! (review-legacy-map-table value) key replacement))
 (lambda (value key)
   (let ([copy (hashtable-copy (review-legacy-map-table value) #t)])
     (hashtable-delete! copy key)
     (make-review-legacy-map copy)))
 (lambda (value key) (hashtable-delete! (review-legacy-map-table value) key))
 (lambda (value)
   (let-values ([(keys vals) (hashtable-entries (review-legacy-map-table value))])
     (map cons (vector->list keys) (vector->list vals)))))

(mat navigator-legacy-keyed-protocol
  (let* ([stored (vector 1 'stored)] [query (vector 1 'query)]
         [table (make-hashtable (lambda (key) (vector-ref key 0))
                                (lambda (left right)
                                  (= (vector-ref left 0) (vector-ref right 0))))]
         [source (make-review-legacy-map table)])
    (hashtable-set! table stored 'value)
    (and (eq? stored (nav-select-one (nav/path (nav/submap query) nav/keys) source))
         (let ([result (nav-transform (nav/path (nav/submap query) nav/none) values source)])
           (eq? stored (nav-select-one nav/keys result)))))

  (let* ([key (ht 'drop 1 'keep 2)] [table (make-hashtable hashtable-size eq?)]
         [source (make-review-legacy-map table)])
    (hashtable-set! table key 'value)
    (and (eq? source (nav-clearval! (nav/path nav/keys (nav/key 'drop)) source))
         (= 1 (hashtable-size table))
         (eq? 'value (hashtable-ref table key #f)))))


;; Each worker keeps its dynamic scope open while the other crosses its callback.
(define navigator-thread-overlap
  (lambda (left right)
    (let ([mutex (make-mutex)] [changed (make-condition)] [phase 0]
          [left-result #f] [right-result #f])
      (define signal!
        (lambda (next)
          (with-mutex mutex (set! phase next) (condition-broadcast changed))))
      (define wait
        (lambda (next)
          (with-mutex mutex
            (let loop ()
              (when (< phase next) (condition-wait changed mutex) (loop))))))
      (let* ([a (fork-thread
                 (lambda ()
                   (set! left-result
                         (left (lambda () (signal! 1) (wait 2))))
                   (signal! 3)))]
             [b (fork-thread
                 (lambda ()
                   (wait 1)
                   (set! right-result
                         (right (lambda () (signal! 2) (wait 3))))))])
        (thread-join a)
        (thread-join b)
        (list left-result right-result)))))

(mat navigator-thread-isolation
  (equal?
   (navigator-thread-overlap
    (lambda (overlap)
      (nav-transform (nav/path (nav/key/default 'new 0)
                               (nav/pred (lambda (value) (overlap) #t))) add1 '()))
    (lambda (overlap)
      (nav-transform (nav/pred (lambda (value) (overlap) #f)) values 42)))
   '(((new . 1)) 42))

  (let ([results
         (navigator-thread-overlap
          (lambda (overlap)
            (nav/letrec ([outer (nav/key 'x)])
              (begin (overlap) (nav/letrec ([inner (make-nav-ref 'outer)]) inner))))
          (lambda (overlap)
            (nav/letrec ([outer (nav/key 'y)]) (begin (overlap) nav/stay))))])
    (equal? (nav-select (car results) '((x . 10) (y . 20))) '(10)))

  (let ([child-path #f])
    (nav/letrec ([outer (nav/key 'x)])
      (begin
        (thread-join
         (fork-thread
          (lambda ()
            (set! child-path (nav/letrec ([inner (make-nav-ref 'outer)]) inner)))))
        nav/stay))
    ;; A child construction has no lexical outer binding from its parent's dynamic scope.
    ;; Selecting the unresolved symbolic outer reference must raise an error.
    (guard (exn [else #t])
      (nav-select child-path '((x . 10)))
      #f)))


(mat navigator-shared-pure-path
  (let ([path (nav/path nav/all (nav/key/default 'x 0))] [first? #t])
    (equal?
     (navigator-thread-overlap
      (lambda (overlap)
        (nav-transform path
                       (lambda (value)
                         (if first? (set! first? #f) (overlap))
                         (+ value 1))
                       '(() ())))
      (lambda (overlap)
        (nav-transform path (lambda (value) (overlap) (+ value 1)) '(()))))
     '((((x . 1)) ((x . 1))) (((x . 1))))))

  (let ([path (nav/path nav/all (nav/key/default 'x 0))] [child-result #f])
    (and (equal?
          (nav-transform
           path
           (lambda (value)
             (thread-join
              (fork-thread
               (lambda ()
                 (set! child-result (nav-transform path add1 '(() ()))))))
             (+ value 1))
           '(() ()))
          '(((x . 1)) ((x . 1))))
         (equal? child-result '(((x . 1)) ((x . 1)))))))

;; Pure shared paths and independent roots remain correct under both timer
;; preemption and explicit suspension, including worker migration.
(mat navigator-fiber-isolation
  (let ([check
         (lambda (workers suspend?)
           (let ([done (make-channel)]
                 [path (nav/path nav/all (nav/key/default 'x 0))]
                 [optional (nav/choice (nav/path nav/all (nav/key 'x)) nav/none)])
             (define work
               (lambda ()
                 (let loop ([remaining 100] [correct? #t])
                   (if (= remaining 0)
                       (channel-put! done correct?)
                       (let ([result
                              (nav-transform
                               path
                               (lambda (value)
                                 (when suspend? (fiber-yield))
                                 (+ value 1))
                               '(() () ()))]
                             [selected
                              (nav-select optional
                                          '(((x . 1)) ((missing . 2)) ((x . 3))))])
                         (loop (- remaining 1)
                               (and correct?
                                    (equal? result '(((x . 1)) ((x . 1)) ((x . 1))))
                                    (equal? selected '(1 3)))))))))
             (run-fibers
              (lambda ()
                (do ([task 0 (+ task 1)]) ((= task 8))
                  (spawn-fiber work))
                (let collect ([remaining 8] [correct? #t])
                  (if (= remaining 0)
                      correct?
                      (let ([result (channel-get done)])
                        (collect (- remaining 1) (and correct? result))))))
              workers)))])
    (and (check 1 #f) (check 4 #f) (check 1 #t) (check 4 #t))))


(mat navigator-resource-free-cursor
  (let ([cursor (nav-traverse->iter nav/all '(1 2 3))])
    (and (equal? (iter->list cursor) '(1 2 3))
         (not (iter-finalized? cursor))
         (begin
           (iter-reset! cursor)
           (equal? (iter->list cursor) '(1 2 3))))))


;; Predicates count only the final read, after all publishers have joined. The
;; predicates and adapter callbacks therefore introduce no caller-owned races.
(mat navigator-registry-publication
  (let ([check
         (lambda (register recognize)
           (let ([count 0] [count? #f] [mutex (make-mutex)]
                 [changed (make-condition)] [go? #f])
             (let* ([predicate (lambda (value)
                                 (when count? (set! count (+ count 1)))
                                 #f)]
                    [threads
                     (map (lambda (worker)
                            (fork-thread
                             (lambda ()
                               (with-mutex mutex
                                 (let wait ()
                                   (unless go?
                                     (condition-wait changed mutex)
                                     (wait))))
                               (do ([index 0 (+ index 1)]) ((= index 256))
                                 (register predicate)))))
                          '(0 1 2 3 4 5 6 7))])
               (with-mutex mutex (set! go? #t) (condition-broadcast changed))
               (for-each thread-join threads)
               (set! count? #t)
               (recognize)
               (set! count? #f)
               (= count 2048))))])
    (and
     (check (lambda (predicate)
              (nav-register-indexed! predicate void void void void))
            (lambda () (guard (exn [else (void)]) (nav-select nav/all 'unknown))))
     (check (lambda (predicate)
              (nav-register-keyed! predicate void void void void void void))
            (lambda () (guard (exn [else (void)]) (nav-select nav/keys 'unknown))))
     (check (lambda (predicate)
              (nav-register-set! predicate void void void void void))
            (lambda () (guard (exn [else (void)]) (nav-select nav/all 'unknown)))))))


(mat navigator-review-key-repair-suspension
  (andmap
   (lambda (workers)
     (eq? 'payload
          (run-fibers
           (lambda ()
             (let ([parent (make-hashtable (lambda (key) (fiber-yield) 0) eq?)])
               (hashtable-set! parent 'key 'payload)
               (nav-clearval! (nav/path nav/keys
                                       (nav/pred (lambda (key) (fiber-yield) #f))) parent)
               (hashtable-ref parent 'key #f)))
           workers)))
   '(1 4))

  ;; A suffix exception still repairs a mutated key; the repair hash may suspend.
  (run-fibers
   (lambda ()
     (let* ([key (vector 1)]
            [parent (make-hashtable (lambda (key) (fiber-yield) (vector-ref key 0)) eq?)])
       (hashtable-set! parent key 'payload)
       (and (guard (exn [else #t])
              (nav-clearval!
               (nav/path nav/keys
                         (nav/pred (lambda (key)
                                     (vector-set! key 0 2)
                                     (fiber-yield)
                                     (error 'key "suffix failed")))) parent)
              #f)
            (eq? 'payload (hashtable-ref parent key #f)))))
   1))

(mat navigator-specialized-array-families
  (andmap
   (lambda (source expected)
     (let ([copy (nav-transform nav/all add1 source)])
       (and (equal? expected (nav-select nav/all source))
            (equal? (map add1 expected) (nav-select nav/all copy))
            (equal? (list (cadr expected)) (nav-select (nav/slice 1 2) source))
            (begin (nav-transform! nav/all add1 source) #t)
            (equal? (map add1 expected) (nav-select nav/all source)))))
   (list (fxarray 1 2) (flarray 1.0 2.0) (bytearray 1 2))
   '((1 2) (1.0 2.0) (1 2)))

  ;; Specialized bulk setters preserve the element type restrictions.
  (error? (nav-transform nav/all (lambda (value) 'invalid) (fxarray 1)))

  ;; Bytearray updates must remain within the byte domain.
  (error? (nav-transform! nav/all (lambda (value) 256) (bytearray 1))))

(define-record-type review-bulk-indexed (fields values))

(mat navigator-indexed-bulk-hooks
  (let ([walks 0] [copies 0] [mutations 0] [refs 0])
    (define walk
      (lambda (source start stop step visit)
        (set! walks (+ walks 1))
        (let loop ([index start])
          (when (< index stop)
            (visit index (vector-ref (review-bulk-indexed-values source) index))
            (loop (+ index step))))))
    (define mutate
      (lambda (source indexes update)
        (set! mutations (+ mutations 1))
        (for-each (lambda (index)
                    (let ([values (review-bulk-indexed-values source)])
                      (vector-set! values index (update (vector-ref values index))))) indexes)
        source))
    (nav-register-indexed!
     review-bulk-indexed?
     (lambda (source) (vector-length (review-bulk-indexed-values source)))
     (lambda (source index)
       (set! refs (+ refs 1)) (vector-ref (review-bulk-indexed-values source) index))
     (lambda (source index value) (error 'setter "bulk update used scalar copy"))
     (lambda (source index value) (error 'setter "bulk mutation used scalar setter"))
     walk
     (lambda (source indexes update)
       (set! copies (+ copies 1))
       (mutate (make-review-bulk-indexed (vector-copy (review-bulk-indexed-values source)))
               indexes update))
     mutate)
    (let* ([source (make-review-bulk-indexed (vector 1 2 3))]
           [selected (nav-select nav/all source)]
           [entries (nav-select nav/entries source)]
           [sliced (nav-select (nav/slice 0 3 2) source)]
           [copy (nav-transform nav/all add1 source)])
      (and (equal? '(1 2 3) selected)
           (equal? '((0 . 1) (1 . 2) (2 . 3)) entries)
           (equal? '(1 3) sliced)
           (equal? '#(2 3 4) (review-bulk-indexed-values copy))
           (equal? '#(1 2 3) (review-bulk-indexed-values source))
           (begin (nav-transform! nav/all add1 source) #t)
           (equal? '#(2 3 4) (review-bulk-indexed-values source))
           (= walks 3) (= copies 1) (= mutations 2) (= refs 0)))))

(mat navigator-bulk-adapter-suspension
  (andmap
   (lambda (workers)
     (run-fibers
      (lambda ()
        (let ([source (dlist 1 2 3 4)] [path (nav/slice 1 4 2)])
          (let ([copy (nav-transform path
                                     (lambda (value) (fiber-yield) (+ value 10)) source)])
            (and (equal? '(1 2 3 4) (dlist->list source))
                 (equal? '(1 12 3 14) (dlist->list copy))
                 (equal? '(1 12 3 14)
                         (nav-select (nav/path nav/all
                                               (nav/pred (lambda (value) (fiber-yield) #t)))
                                     copy))
                 (begin (nav-transform! path (lambda (value) (fiber-yield) (* value 2)) source)
                        #t)
                 (equal? '(1 4 3 8) (dlist->list source))))))
      workers))
   '(1 4)))

(mat navigator-bulk-hook-validation
  ;; A non-procedure optional bulk hook is rejected before registry publication.
  (error? (nav-register-indexed! review-bulk-indexed? void void void void 42)))

(mat navigator-bulk-adapter-lazy-cursors
  (andmap
   (lambda (constructor)
     (let ([cursor (nav-traverse->iter (nav/slice 1 5 2) (constructor 1 2 3 4 5))])
       (and (equal? '(2 4) (iter->list cursor))
            (not (iter-finalized? cursor))
            (begin (iter-reset! cursor) #t)
            (equal? '(2 4) (iter->list cursor)))))
   (list array fxarray bytearray dlist)))

(mat navigator-entry-index-resumption
  (andmap
   (lambda (constructor)
     (let ([saved #f] [phase 0] [indexes '()])
       (guard (condition [else #f])
         (let ([result
                (nav-transform
                 nav/entries
                 (lambda (entry)
                   (set! indexes (cons (car entry) indexes))
                   (if saved entry
                       ;; The resumed update still targets this lexical index-0 location.
                       (call/cc (lambda (continuation) (set! saved continuation) entry))))
                 (constructor 1 2))])
           (if (= phase 0)
               (begin (set! phase 1) (saved (cons 0 10)))
               (and (equal? '(10 2) (nav-select nav/all result))
                    (equal? '(1 1 0) indexes)))))))
   (list vector list array fxarray bytearray dlist)))

(mat navigator-entry-index-resumption-validation
  ;; A replayed entry must retain its actual index, even if the old pass advanced its counter.
  (let ([saved #f] [phase 0])
    (guard (condition [else (= phase 1)])
      (nav-transform
       nav/entries
       (lambda (entry)
         (if saved entry
             ;; Resume the actual index-0 update with an invalid replacement index.
             (call/cc (lambda (continuation) (set! saved continuation) entry))))
       (vector 1 2))
      (if (= phase 0)
          (begin (set! phase 1) (saved (cons 2 10)))
          #f))))

(mat navigator-entry-clear-resumption
  (andmap
   (lambda (constructor)
     (let ([saved #f] [phase 0] [source (constructor 1 2)])
       (guard (condition [else #f])
         (let ([result
                (nav-clearval!
                 (nav/path nav/entries
                           (nav/pred
                            (lambda (entry)
                              (if saved #f
                                  ;; Both the original and resumed predicates retain every entry.
                                  (call/cc (lambda (continuation)
                                             (set! saved continuation) #f))))))
                 source)])
           (if (= phase 0)
               (begin (set! phase 1) (saved #f))
               (and (eq? result source) (equal? '(1 2) (nav-select nav/all source))))))))
   (list vector list array fxarray bytearray dlist)))

(mat navigator-sparse-range-boundaries
  (andmap
   (lambda (constructor)
     (let ([source (constructor 0 1 2 3 4)])
       (and (equal? '(0) (nav-select (nav/slice 0 5 5) source))
            (equal? '(1 3) (nav-select (nav/slice 1 4 2) source))
            (equal? '() (nav-select (nav/slice 5 5 100) source))
            (equal? '(4) (nav-select (nav/slice 4 5 100) source)))))
   (list list dlist)))

(mat navigator-default-false-resumption
  (andmap
   (lambda (step data)
     (let ([saved #f] [phase 0] [calls 0])
       (let ([result
              (nav-transform
               (nav/path step
                         (nav/pred
                          (lambda (value)
                            (if saved #t
                                ;; Resume before the suffix decides whether it reaches an update.
                                (call/cc (lambda (resume) (set! saved resume) #t))))))
               (lambda (value) (set! calls (+ calls 1)) (+ value 1)) data)])
         (if (= phase 0)
             (begin (set! phase 1) (saved #f))
             (and (equal? result data) (= calls 1))))))
   (list (nav/key/default 'x 0) (nav/nth/default 2 0))
   (list '() (vector 9))))

(mat navigator-hashtable-rebuild-hash-policy
  (andmap
   (lambda (equivalent?)
     (let* ([hash (lambda (key) 0)] [source (make-hashtable hash equivalent?)])
       (hashtable-set! source 'a 1)
       (hashtable-set! source 'b 2)
       (andmap
        (lambda (result)
          (and (eq? hash (hashtable-hash-function result))
               (eq? equivalent? (hashtable-equivalence-function result))
               (= 1 (hashtable-ref result 'a 0))))
        (list (nav-select-one (nav/submap 'a) source)
              (nav-transform nav/keys (lambda (key) key) source)
              (nav-transform nav/entries (lambda (entry) entry) source)))))
   (list eq? eqv?)))

(mat navigator-default-resumption-retains-sibling-evidence
  (let ([saved #f] [phase 0])
    (let ([result
           (nav-transform
            (nav/path (nav/key/default 'x '(1 2)) nav/all
                      (nav/pred
                       (lambda (value)
                         (if (or saved (= value 1)) #t
                             ;; The first sibling has already reached the terminal callback.
                             (call/cc (lambda (resume) (set! saved resume) #t))))))
            add1 '())])
      (if (= phase 0)
          (begin (set! phase 1) (saved #f))
          (equal? '((x 2 2)) result)))))

(mat navigator-default-terminal-resumption
  (let ([saved #f] [phase 0])
    (let ([result
           (nav-transform
            (nav/key/default 'x 0)
            (lambda (value)
              ;; A resumed terminal return still authorizes inserting this missing key.
              (call/cc (lambda (resume) (set! saved resume) 1)))
            '())])
      (if (= phase 0)
          (begin (set! phase 1) (saved 9))
          (equal? '((x . 9)) result)))))

(mat navigator-default-live-fiber-evidence
  (andmap
   (lambda (workers)
     (let ([done (make-channel)]
           [step (make-nav
                  'yield-after-update
                  (lambda (value emit) (emit value))
                  (lambda (value update)
                    (let ([result (update value)]) (fiber-yield) result))
                  (lambda (value update!)
                    (let ([result (update! value)]) (fiber-yield) result)))])
       (run-fibers
        (lambda ()
          (do ([task 0 (+ task 1)]) ((= task 8))
            (spawn-fiber
             (lambda ()
               (channel-put!
                done
                (equal? '((x . 1))
                        (nav-transform (nav/path (nav/key/default 'x 0) step)
                                       (lambda (value) (fiber-yield) (add1 value)) '()))))))
          (let collect ([remaining 8] [correct? #t])
            (if (= remaining 0) correct?
                (let ([result (channel-get done)])
                  (collect (- remaining 1) (and correct? result))))))
        workers)))
   '(1 4)))

(mat navigator-default-resumption-with-fiber-suspension
  (andmap
   (lambda (workers)
     (run-fibers
      (lambda ()
        (let ([saved #f] [phase 0])
          (let ([result
                 (nav-transform
                  (nav/path (nav/key/default 'x '(1 2)) nav/all
                            (nav/pred
                             (lambda (value)
                               (fiber-yield)
                               (if (or saved (= value 1)) #t
                                   ;; Later terminal callbacks suspend before this predicate replays.
                                   (call/cc (lambda (resume) (set! saved resume) #t))))))
                  (lambda (value) (fiber-yield) (add1 value)) '())])
            (if (= phase 0)
                (begin (set! phase 1) (fiber-yield) (saved #f))
                (equal? '((x 2 2)) result)))))
      workers))
   '(1 4)))

(mat navigator-default-repeated-predicate-resumption
  (let ([saved #f] [phase 0] [calls 0])
    (let ([result
           (nav-transform
            (nav/path (nav/key/default 'x 0)
                      (nav/pred
                       (lambda (value)
                         ;; Reuse one captured predicate continuation with different decisions.
                         (call/cc (lambda (resume) (set! saved resume) #t)))))
            (lambda (value) (set! calls (+ calls 1)) (add1 value)) '())])
      (case phase
        [(0) (and (equal? '((x . 1)) result)
                  (begin (set! phase 1) (saved #f)))]
        [(1) (and (null? result) (= calls 1)
                  (begin (set! phase 2) (saved #t)))]
        [else (and (equal? '((x . 1)) result) (= calls 2))]))))


;; Complete before each resumption. A false replay must discard evidence from the
;; original true branch; a later true replay must establish evidence again.
(define conditional-resumption-results
  (lambda (wrap operate source)
    (let ([saved #f] [pending '(#f #f #t)] [results '()])
      (let ([result
             ;; Empty nav/must is the expected error; other conditions are test failures.
             (guard (condition
                     [(and (who-condition? condition)
                           (eq? 'nav/must (condition-who condition))) 'required-focus-error])
               (operate
                (wrap (nav/pred
                       (lambda (value)
                         ;; Capture directly in the predicate, not an opaque traversal callback.
                         (call/cc (lambda (resume) (set! saved resume) #t)))))
                source))])
        (set! results (cons result results))
        (if (null? pending) (reverse results)
            (let ([decision (car pending)])
              (set! pending (cdr pending))
              (saved decision)))))))

(mat navigator-conditional-focus-resumption
  (equal? '(8 required-focus-error required-focus-error 8)
          (conditional-resumption-results
           nav/must (lambda (path source) (nav-transform path add1 source)) 7))

  (equal? '(8 8 8 8)
          (conditional-resumption-results
           (lambda (path) (nav/choice path nav/stay))
           (lambda (path source) (nav-transform path add1 source)) 7))

  ;; Selection tests observe callbacks directly, without assuming collector rollback.
  (equal? '(7 required-focus-error required-focus-error 7)
          (conditional-resumption-results
           nav/must
           (lambda (path source)
             (let ([last #f]) (nav-traverse path source (lambda (value) (set! last value))) last))
           7))

  (equal? '(7 9 9 7)
          (conditional-resumption-results
           (lambda (path)
             (nav/choice path (nav/getter 'fallback (lambda (value) 9))))
           (lambda (path source)
             (let ([last #f]) (nav-traverse path source (lambda (value) (set! last value))) last))
           7)))

;; Keep the cursor live while checking that its old consumer's private vector dies.
(define navigator-consumer-released?
  (lambda (make-cursor)
    (let ([cursor (make-cursor)] [reference #f])
      (define consume
        (lambda ()
          (let ([payload (make-vector 100000 'payload)])
            (set! reference (weak-cons payload #f))
            ;; The failure case is caught by this consumer, as required by the bridge.
            (guard (condition [else 'expected-failure]) (iter-next! cursor))
            ;; Exhaust the one-focus case too: a live producer still needs its stack.
            (iter-next! cursor)
            (vector-ref payload 0))))
      (consume)
      (collect (collect-maximum-generation))
      (collect (collect-maximum-generation))
      (let ([released? (bwp-object? (car reference))])
        (iter-reset! cursor)
        released?))))

(mat navigator-cursor-releases-consumer
  (andmap navigator-consumer-released?
          (list (lambda () (nav-traverse->iter nav/none 7))
                (lambda () (nav-traverse->iter nav/stay 7))
                ;; An absent required focus exercises the producer-error transfer.
                (lambda () (nav-traverse->iter (nav/key 'missing) 7))
                (lambda () (sequence (tidentity) (nav-traverse->iter nav/none 7))))))


(mat navigator-conditional-resumption-retains-prefix
  (andmap
   (lambda (wrap)
     (let ([saved #f] [phase 0])
       (let ([result
              (nav-transform
               (wrap (nav/path nav/all
                               (nav/pred
                                (lambda (value)
                                  (if (= value 1) #t
                                      ;; An earlier sibling has already established focus evidence.
                                      (call/cc (lambda (resume) (set! saved resume) #t)))))))
               add1 '(1 2))])
         (if (= phase 0) (begin (set! phase 1) (saved #f))
             (equal? '(2 2) result)))))
   (list nav/must (lambda (path) (nav/choice path nav/stay)))))

(mat navigator-nested-conditional-resumption
  (equal? '(8 required-focus-error required-focus-error 8)
          (conditional-resumption-results
           (lambda (path) (nav/must (nav/choice path nav/none)))
           (lambda (path source) (nav-transform path add1 source)) 7))

  (equal? '(8 8 8 8)
          (conditional-resumption-results
           (lambda (path) (nav/choice (nav/maybe path) nav/stay))
           (lambda (path source) (nav-transform path add1 source)) 7))

  (equal? '(() required-focus-error required-focus-error ())
          (conditional-resumption-results
           (lambda (path) (nav/path nav/all (nav/must path))) nav-clearval '(7)))

  (equal? '(#(7) required-focus-error required-focus-error #(7))
          (conditional-resumption-results
           nav/must
           (lambda (path source) (nav-transform! path (lambda (value) value) source))
           (vector 7))))

(mat navigator-conditional-independent-fibers
  (andmap
   (lambda (workers)
     (let ([done (make-channel)]
           [path (nav/must (nav/choice (nav/path nav/all (nav/pred positive?)) nav/stay))])
       (run-fibers
        (lambda ()
          (do ([task 0 (+ task 1)]) ((= task 12))
            (spawn-fiber
             (lambda ()
               (channel-put!
                done
                (equal? '(2 -2 4)
                        (nav-transform path (lambda (value) (fiber-yield) (add1 value))
                                       '(1 -2 3)))))))
          (let loop ([remaining 12] [correct? #t])
            (if (= remaining 0) correct?
                (let ([result (channel-get done)])
                  (loop (- remaining 1) (and correct? result))))))
        workers)))
   '(1 4)))


(mat navigator-conditional-delegated-resumption
  (equal? '(8 required-focus-error required-focus-error 8)
          (conditional-resumption-results
           (lambda (path)
             (nav/must
              (nav/path
               (make-nav
                'delegate
                (lambda (value emit) (nav-traverse nav/stay value emit))
                (lambda (value update) (nav-transform nav/stay update value))
                (lambda (value update!) (nav-transform! nav/stay update! value)))
               path)))
           (lambda (path source) (nav-transform path add1 source)) 7)))

(mat navigator-conditional-independent-threads
  (let ([path (nav/must (nav/path nav/all (nav/pred positive?)))])
    (define check
      (lambda (source expected)
        (let loop ([pass 0])
          (or (= pass 100)
              (and (equal? expected (nav-transform path add1 source))
                   (loop (+ pass 1)))))))
    (equal? '#(#t #t)
            (run-threads 2 (lambda () (check '(1 -2 3) '(2 -2 4)))
                           (lambda () (check '(10 0 -1) '(11 0 -1)))))))

(mat navigator-conditional-resumption-in-fibers
  (andmap
   (lambda (workers)
     (run-fibers
      (lambda ()
        (equal? '(8 required-focus-error required-focus-error 8)
                (conditional-resumption-results
                 nav/must
                 (lambda (path source)
                   (nav-transform path (lambda (value) (fiber-yield) (add1 value)) source))
                 7)))
      workers))
   '(1 4)))


(mat navigator-conditional-terminal-resumption
  (andmap
   (lambda (path)
     (let ([saved #f] [phase 0])
       (let ([result
              (nav-transform path
                             (lambda (value)
                               ;; Resume the updater after conditional focus was already reached.
                               (call/cc (lambda (resume) (set! saved resume) (add1 value))))
                             7)])
         (if (= phase 0) (begin (set! phase 1) (saved 9)) (= result 9)))))
   (list (nav/must nav/stay) (nav/choice nav/stay nav/stay))))

(mat navigator-conditional-callback-values
  (let ([calls 0])
    (nav-traverse (nav/must nav/all) '(1 2)
                  (lambda (value) (set! calls (+ calls 1)) (values)))
    (= calls 2))

  (let ([calls 0])
    (nav-traverse (nav/choice nav/all nav/stay) '(1 2)
                  (lambda (value) (set! calls (+ calls 1)) (values 'a 'b)))
    (= calls 2)))

;; The setter runs after the terminal callback. Resuming it must retain that
;; successful update, including when no nav/must or nav/choice cell is active.
(define navigator-default-setter-resumption-results
  (lambda (prefix wrap source suspend)
    (let ([saved #f] [pending '(9 11 13)] [results '()] [updates 0])
      (let ([result
             (nav-transform
              (nav/path
               prefix
               (wrap
                (nav/getter-setter
                 'identity values
                 (lambda (parent child)
                   (suspend)
                   ;; Complete every pass before resuming the same setter continuation.
                   (call/cc (lambda (resume) (set! saved resume) child))))))
              (lambda (value) (set! updates (+ updates 1)) (add1 value)) source)])
        (set! results (cons result results))
        (if (null? pending)
            (list (reverse results) updates)
            (let ([replacement (car pending)])
              (set! pending (cdr pending))
              (suspend)
              (saved replacement)))))))

(mat navigator-default-setter-resumption
  (andmap
   (lambda (wrap)
     (equal? '((((x . 1)) ((x . 9)) ((x . 11)) ((x . 13))) 1)
             (navigator-default-setter-resumption-results
              (nav/key/default 'x 0) wrap '() void)))
   (list values nav/must (lambda (path) (nav/choice path nav/stay))))

  (equal? '((#(7 0 1) #(7 0 9) #(7 0 11) #(7 0 13)) 1)
          (navigator-default-setter-resumption-results
           (nav/nth/default 2 0) values '#(7) void))

  (andmap
   (lambda (workers)
     (run-fibers
      (lambda ()
        (equal? '((((x . 1)) ((x . 9)) ((x . 11)) ((x . 13))) 1)
                (navigator-default-setter-resumption-results
                 (nav/key/default 'x 0) nav/must '() fiber-yield)))
      workers 100))
   '(1 4)))

(mat navigator-scope-timer-migration
  (andmap
   (lambda (workers)
     (let ([done (make-channel)]
           [recursive (nav/letrec ([leaf nav/stay]) (make-nav-ref 'leaf))]
           [default (nav/key/default 'x 0)]
           [conditional (nav/must (nav/path nav/all (nav/not-pred negative?)))])
       (run-fibers
        (lambda ()
          (do ([task 0 (+ task 1)]) ((= task 16))
            (spawn-fiber
             (lambda ()
               (channel-put!
                done
                ;; Lost private scope can raise unresolved-reference/required-focus errors.
                (guard (condition [else #f])
                  (let loop ([remaining 10000])
                    (or (= remaining 0)
                        (and (equal? '(7) (nav-select recursive 7))
                             (equal? '((x . 1)) (nav-transform default add1 '()))
                             (equal? '(2 -2 4)
                                     (nav-transform conditional add1 '(1 -2 3)))
                             (loop (- remaining 1))))))))))
          (let loop ([remaining 16] [correct? #t])
            (if (= remaining 0) correct?
                (let ([result (channel-get done)])
                  (loop (- remaining 1) (and correct? result))))))
        workers 100)))
   '(1 4)))

(mat navigator-not-pred-resumption
  (let ([saved #f] [pending '(#f #f #t)] [results '()])
    (let ([result
           ;; An initially true predicate leaves nav/must empty; false replays select.
           (guard (condition
                   [(and (who-condition? condition)
                         (eq? 'nav/must (condition-who condition))) 'required-focus-error])
             (nav-transform
              (nav/must
               (nav/not-pred
                (lambda (value)
                  ;; Resume the predicate itself through the single negation boundary.
                  (call/cc (lambda (resume) (set! saved resume) #t)))))
              add1 7))])
      (set! results (cons result results))
      (if (null? pending)
          (equal? '(required-focus-error 8 8 required-focus-error) (reverse results))
          (let ([decision (car pending)])
            (set! pending (cdr pending))
            (saved decision)))))

  ;; Negative: a predicate-origin focus error must propagate through nav/maybe.
  (error? (nav-select (nav/maybe
                      (nav/not-pred
                       (lambda (value) (nav-select-one (nav/key 'missing) '()))))
                     7)))

;; A producer continuation has no consumer destination between completed pulls.
;; Negative: resuming one must raise the documented bridge error on every attempt,
;; including from later producer segments and after full exhaustion.
(define navigator-detached-producer-results
  (lambda (capture-at exhaust?)
    (let ([saved #f] [pending '(#t #f raise)] [results '()] [initial? #t])
      (let ([cursor
             (nav-traverse->iter
              (nav/path nav/all
                        (nav/pred
                         (lambda (value)
                           (if (= value capture-at)
                               (let ([decision
                                      ;; Complete the pull before invoking the saved producer.
                                      (call/cc (lambda (resume) (set! saved resume) #t))])
                                 (if (eq? decision 'raise)
                                     (error 'detached-producer "callback failure")
                                     decision))
                               #t))))
              '(1 2 3))])
        (let ([result
               (guard (condition
                       [(and (who-condition? condition)
                             (eq? 'nav-traverse->iter (condition-who condition))) 'rejected]
                       [else 'unexpected-error])
                 (if exhaust?
                     (iter->list cursor)
                     (let pull ([remaining capture-at])
                       (let ([value (iter-next! cursor)])
                         (if (= remaining 1) value (pull (- remaining 1)))))))])
          (unless initial? (set! results (cons result results)))
          (set! initial? #f)
          (if (null? pending)
              (reverse results)
              (let ([decision (car pending)])
                (set! pending (cdr pending))
                (saved decision))))))))

(mat navigator-detached-producer-resumption
  (andmap
   (lambda (capture-at)
     (andmap
      (lambda (exhaust?)
        (equal? '(rejected rejected rejected)
                (navigator-detached-producer-results capture-at exhaust?)))
      '(#f #t)))
   '(1 2 3)))

(mat navigator-list-index-boundaries
  (equal? '(a) (nav-select nav/first '(a b c)))

  (equal? '(c) (nav-select (nav/nth 2) '(a b c)))

  (equal? '(missing) (nav-select (nav/nth/default 3 'missing) '(a b c)))

  (equal? '(missing)
          (nav-select (nav/nth/default (+ (most-positive-fixnum) 1) 'missing) '(a b c)))

  (equal? '(missing) (nav-select (nav/nth/default 0 'missing) '()))

  ;; Negative: improper lists remain unsupported, even when their head exists.
  (error? (nav-select nav/first '(a . b)))

  ;; Negative: cyclic lists must still be rejected before indexed traversal.
  (let ([source (list 'a)])
    (set-cdr! source source)
    (guard (condition [else #t])
      (nav-select nav/first source)
      #f)))

(define tagged-indexed-pair?
  (lambda (value)
    (and (pair? value) (eq? (car value) 'tagged-indexed-pair) (vector? (cdr value)))))

(nav-register-indexed!
 tagged-indexed-pair?
 (lambda (container) (vector-length (cdr container)))
 (lambda (container index) (vector-ref (cdr container) index))
 (lambda (container index value)
   (let ([copy (vector-copy (cdr container))])
     (vector-set! copy index value)
     (cons 'tagged-indexed-pair copy)))
 (lambda (container index value) (vector-set! (cdr container) index value)))

(define tagged-keyed-pair?
  (lambda (value)
    (and (pair? value) (eq? (car value) 'tagged-keyed-pair) (hashtable? (cdr value)))))

(nav-register-keyed!
 tagged-keyed-pair?
 (lambda (container key default) (hashtable-ref (cdr container) key default))
 (lambda (container key value)
   (let ([copy (hashtable-copy (cdr container) #t)])
     (hashtable-set! copy key value)
     (cons 'tagged-keyed-pair copy)))
 (lambda (container key value) (hashtable-set! (cdr container) key value))
 (lambda (container key)
   (let ([copy (hashtable-copy (cdr container) #t)])
     (hashtable-delete! copy key)
     (cons 'tagged-keyed-pair copy)))
 (lambda (container key) (hashtable-delete! (cdr container) key))
 (lambda (container)
   (let-values ([(keys values) (hashtable-entries (cdr container))])
     (map cons (vector->list keys) (vector->list values)))))

(mat navigator-recursive-pair-adapters
  (let ([source (cons 'tagged-indexed-pair (vector 1 (vector 2)))])
    (and (equal? '(1 #(2)) (nav-select nav/all source))
         (equal? '(1 #(2)) (nav-select (nav/path nav/children nav/stay) source))
         (equal? '(1 2) (nav-select nav/leaves source))
         (equal? '(1 2) (nav-select (nav/walker number?) source))
         (equal? '(1 2) (iter->list (nav-traverse->iter nav/leaves source)))
         (equal? '(2 #(2))
                 (nav-select nav/all
                             (nav-transform (nav/path nav/children (nav/pred number?))
                                            add1 source)))))

  (let* ([table (make-eq-hashtable)]
         [source (cons 'tagged-keyed-pair table)])
    (hashtable-set! table 'items (vector 1 (vector 2)))
    (and (equal? '(#(1 #(2))) (nav-select nav/all source))
         (equal? '(#(1 #(2))) (nav-select (nav/path nav/children nav/stay) source))
         (equal? '(1 2) (nav-select nav/leaves source))
         (equal? '(1 2) (nav-select (nav/walker number?) source))))

  ;; Unregistered improper pairs remain atomic leaves rather than implicit containers.
  (and (null? (nav-select (nav/path nav/children nav/stay) '(1 . 2)))
       (equal? '((1 . 2)) (nav-select nav/leaves '(1 . 2)))))


(mat navigator-immediate-children-composition
  (andmap
   (lambda (path)
     (equal? '(a () (b) #(c)) (nav-select path '(a () (b) #(c)))))
   (list nav/children
         (nav/path nav/children nav/stay)
         (nav/path nav/stay nav/children)
         (nav/maybe nav/children)
         (nav/must nav/children)
         (nav/choice nav/children nav/stay)
         (nav/multi-path nav/children)
         (nav/if nav/stay nav/children nav/none)
         (nav/rec visit nav/children)))

  (equal? '(a () (b) #(c))
          (iter->list (nav-traverse->iter nav/children '(a () (b) #(c)))))

  (equal? '((1 . 2)) (nav-select nav/children (vector (cons 1 2))))

  (null? (nav-select nav/children '()))

  (null? (nav-select nav/children '#()))

  (null? (nav-select nav/children 42)))

(mat navigator-optional-indexed-clearing-bounds
  (andmap
   (lambda (make-source)
     (andmap
      (lambda (size)
        (let* ([source (apply make-source (make-list size 1))]
               [before (nav-select nav/all source)])
          (andmap
           (lambda (index)
             (let ([path (nav/maybe (nav/nth index))])
               (and (null? (nav-select path source))
                    (equal? before (nav-select nav/all (nav-clearval path source)))
                    (eq? source (nav-clearval! path source))
                    (equal? before (nav-select nav/all source)))))
           ;; Both the first absent index and an oversized index are optional misses.
           (list size (+ 1 (most-positive-fixnum))))))
      '(0 3)))
   (list array fxarray (lambda values (apply flarray (map inexact values))) bytearray dlist)))

(mat navigator-default-vector-length-type-safety
  ;; A valid natural index can still require an unrepresentable vector length.
  (andmap
   (lambda (index)
     (navigator-type-error?
      (lambda () (nav-setval (nav/nth/default index 0) 1 (vector)))))
   (list (quotient (most-positive-fixnum) 2)
         (most-positive-fixnum) (+ 1 (most-positive-fixnum)))))

(define-record-type (bad-keyed-adapter make-bad-keyed-adapter bad-keyed-adapter?)
  (fields entries))
(define-record-type (bad-set-adapter make-bad-set-adapter bad-set-adapter?)
  (fields members))
(define-record-type (bad-empty-adapter make-bad-empty-adapter bad-empty-adapter?))

(nav-register-keyed!
   bad-keyed-adapter?
   (lambda (value key default) default)
   (lambda (value key child) value)
   (lambda (value key child) (void))
   (lambda (value key) value)
   (lambda (value key) (void))
   bad-keyed-adapter-entries)

(nav-register-set!
   bad-set-adapter?
   bad-set-adapter-members
   (lambda (value members) value)
   (lambda (value members) (void))
   (lambda (value member) value)
   (lambda (value member) (void)))

(nav-register-keyed!
 bad-empty-adapter?
 (lambda (value key default) default)
 (lambda (value key child) value)
 (lambda (value key child) (void))
 (lambda (value key) value)
 (lambda (value key) (void))
 (lambda (value) '())
 (lambda (value) 42))

(mat navigator-adapter-result-type-safety
  ;; Keyed entries must be an association list, even when the callback returns #f.
  (andmap
   (lambda (entries)
     (navigator-type-error?
      (lambda () (nav-select nav/entries (make-bad-keyed-adapter entries)))))
   (list #f 42 (vector 1) (list 1 2) (cons '(a . 1) 2)))

  ;; Set values must be a proper list before traversal or replacement.
  (andmap
   (lambda (members)
     (navigator-type-error?
      (lambda () (nav-select nav/all (make-bad-set-adapter members)))))
   (list #f 42 (vector 1) (cons 1 2)))

  ;; An empty builder must return this adapter's container type, including with no writes.
  (andmap
   (lambda (path)
     (navigator-type-error?
      (lambda () (nav-transform path values (make-bad-empty-adapter)))))
   (list nav/all nav/keys (nav/submap))))
