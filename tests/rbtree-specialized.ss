(import (cspp) (cspp treemap) (cspp treeset)
        (only (cspp private rbtree) $rbtree-verify *dummy-v* rbtree-visit
              rbtree-search rbtree-successor rbtree-predecessor
              rbtree-min rbtree-max rbtree-inorder-cursor))

(mat rbtree-query-two-values
     (let ([tm (treemap = < '(1 . a) '(2 . b))])
       (and (call-with-values (lambda () (rbtree-search 'test tm (lambda (k v) (= k 2))))
              (lambda (k v) (and (= k 2) (eq? v 'b))))
            (call-with-values (lambda () (rbtree-successor 'test tm 1))
              (lambda (k v) (and (= k 2) (eq? v 'b))))
            (call-with-values (lambda () (rbtree-predecessor 'test tm 2))
              (lambda (k v) (and (= k 1) (eq? v 'a))))
            (call-with-values (lambda () (rbtree-min 'test tm))
              (lambda (k v) (and (= k 1) (eq? v 'a))))
            (call-with-values (lambda () (rbtree-max 'test tm))
              (lambda (k v) (and (= k 2) (eq? v 'b)))))))

(mat rbtree-query-sentinel-values
     (let ([tm (treemap = < '(1 . a))])
       (and (call-with-values (lambda () (rbtree-search 'test tm (lambda (k v) #f)))
              (lambda (k v) (and (eq? k *dummy-v*) (eq? v *dummy-v*))))
            (call-with-values (lambda () (rbtree-successor 'test tm 1))
              (lambda (k v) (and (eq? k *dummy-v*) (eq? v *dummy-v*))))
            (call-with-values (lambda () (rbtree-predecessor 'test tm 1))
              (lambda (k v) (and (eq? k *dummy-v*) (eq? v *dummy-v*))))
            (call-with-values (lambda () (rbtree-min 'test (treemap = <)))
              (lambda (k v) (and (eq? k *dummy-v*) (eq? v *dummy-v*))))
            (call-with-values (lambda () (rbtree-max 'test (treemap = <)))
              (lambda (k v) (and (eq? k *dummy-v*) (eq? v *dummy-v*))))
            (let ([cursor (rbtree-inorder-cursor tm)])
              (call-with-values cursor
                (lambda (k v)
                  (and (= k 1)
                       (eq? v 'a)
                       (call-with-values cursor
                         (lambda (k v)
                           (and (eq? k *dummy-v*) (eq? v *dummy-v*)))))))))))

;; Regression: key-only nodes must be traversable without reading a value slot.
(mat rbtree-key-only-node-accessors
     (let ([set (treeset fx= fx< 3 1 2)])
       (and (equal? '(1 2 3) (treeset->list set))
            (= 2 (treeset-successor set 1))
            (= 1 (treeset-min set))
            (= 3 (treeset-max set)))))

(mat rbtree-set-traversal-sentinel
     (let ([set (treeset fx= fx< 1 2)] [sentinel? #t])
       (rbtree-visit 'rbtree-set-traversal-sentinel
                     (lambda (key value)
                       (set! sentinel? (and sentinel? (eq? value *dummy-v*))))
                     set)
       sentinel?))

(mat rbtree-map-successor-value
     (let ([map (treemap equal? < '(4 . root) '(2 . left) '(6 . right)
                         '(5 . successor) '(7 . last))])
       (treemap-delete! map 4)
       (and (not (treemap-contains? map 4))
            (eq? 'successor (treemap-ref map 5)))))

;; Exercise rotations and deletion repair on the actual public treeset backend.
(mat rbtree-balanced-mutations
     (let ([items (fxvshuffle! (fxviota 1000))]
           [set (make-treeset fx= fx<)])
       (do ([i 0 (fx1+ i)]) ((fx= i 1000))
         (treeset-add! set i)
         ($rbtree-verify set))
       (fxvfor-each (lambda (item)
                     (treeset-delete! set item)
                     ($rbtree-verify set))
                   items)
       (treeset-empty? set)))

(mat rbtree-preserves-pair-items
     (let ([set (treeset equal? (lambda (a b) (< (car a) (car b))) '(1 . a) '(2 . b))])
       (and (equal? '(1 . a) (treeset-min set))
            (equal? '(2 . b) (treeset-successor set '(1 . a))))))

(mat fixnum-backend-derived-results
     (let* ([set (fxtreeset fx= fx< 3 1 2)]
            [map (fxtreemap fx= fx< '(3 . 30) '(1 . 10) '(2 . 20))])
       (and (fxtreeset? (fxtreeset-filter odd? set))
            (fxtreeset? (fxtreeset-map fx1+ set))
            (fxtreemap? (fxtreemap-filter (lambda (key value) (odd? key)) map))
            (fxtreemap? (fxtreemap-map (lambda (key value) (values key value)) map))
            (begin
              (fxtreeset-delete! set 2)
              (fxtreemap-delete! map 2)
              (and ($rbtree-verify set) ($rbtree-verify map))))))

;; Error cases: empty specialized trees and callback-generated keys must be checked.
(mat fixnum-backend-validation
     (error? (fxtreeset-contains? (make-fxtreeset fx= fx<) 'bad))

     (error? (fxtreemap-contains? (make-fxtreemap fx= fx<) 'bad))

     (error? (fxtreeset-map (lambda (item) 'bad) (fxtreeset fx= fx< 1)))

     (error? (fxtreemap-map (lambda (key value) (values 'bad value))
                            (fxtreemap fx= fx< '(1 . 10)))))

(mat fixnum-backend-balanced-mutations
     (let ([items (fxvshuffle! (fxviota 500))]
           [set (make-fxtreeset fx= fx<)]
           [map (make-fxtreemap fx= fx<)])
       (fxvfor-each
         (lambda (item)
           (fxtreeset-add! set item)
           (fxtreemap-set! map item item)
           ($rbtree-verify set)
           ($rbtree-verify map))
         items)
       (fxvfor-each
         (lambda (item)
           (fxtreeset-delete! set item)
           (fxtreemap-delete! map item)
           ($rbtree-verify set)
           ($rbtree-verify map))
         items)
       (and (fxtreeset-empty? set) (fxtreemap-empty? map))))

(mat fixnum-boundaries-and-mixed-folds
     (let ([set (fxtreeset fx= fx> (most-negative-fixnum) 0 (most-positive-fixnum))]
           [one (fxtreeset fx= fx< 1 2)]
           [two (fxtreeset fx= fx< 3 4)]
           [three (fxtreeset fx= fx< 5 6)])
       (and ($rbtree-verify set)
            (= (most-positive-fixnum) (fxtreeset-min set))
            (= 21 (fxtreeset-fold-left + 0 one two three))
            (= 21 (fxtreeset-fold-right + 0 one two three))
            (fxtreeset? (fxtreeset+ one two three)))))

;; Error cases: multi-set operations reject mixed generic/fixnum operands.
(mat fxtreeset-mixed-family-rejection
     (error? (fxtreeset+ (fxtreeset fx= fx< 1) (treeset = < 2)))
     (error? (treeset+ (treeset = < 1) (fxtreeset fx= fx< 2))))

(mat fixnum-conversions-and-writers
     (let ([table (make-eqv-hashtable)])
       (hashtable-set! table 1 10)
       (let ([map (hashtable->fxtreemap fx= fx< table)]
             [set (vector->fxtreeset fx= fx< '#(3 1 2 2))])
         (and (fxtreemap? map)
              (fxtreeset? set)
              (equal? set (list->fxtreeset fx= fx< '(1 2 3)))
              (equal? '#(1 2 3) (fxtreeset->vector set))
              (equal? (format "~s" map) (format "~s" (treemap fx= fx< '(1 . 10))))
              (equal? (format "~s" set) (format "~s" (treeset fx= fx< 1 2 3)))))))
