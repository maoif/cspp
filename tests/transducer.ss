(import (cspp))


(mat registered-source-transduction
     (= 15 (transduce (tcompose (tfilter even?) (tmap add1))
                      (rffxsum)
                      (array 1 2 3 4 5 6)))
     (transducible? (queue 1 2 3)))


(mat hashtable-source-dispatch

     (let ([ht (make-hashtable equal-hash equal?)])
       (hashtable-set! ht 'a 2)
       (hashtable-set! ht 'b 1)
       (let ([iter-values (sort < (iter->list (source->iter ht)))]
             [transduced-values (sort < (transduce (tidentity) (rflist) ht))])
         (equal? iter-values transduced-values)))

     )


(mat transducer-records

     (transducer? (tidentity))
     (eq? 'tidentity (transducer-name (tidentity)))
     (eq? 'tidentity (transducer-name (tcompose)))
     (reducer? (rflist))
     (eq? 'rflist (reducer-name (rflist)))
     (procedure? (completion (rflist)))

     (let ([x (reduced 10)])
       (and (reduced? x)
            (= 10 (unreduced x))
            (eq? x (ensure-reduced x))))
     (= 11 (unreduced 11))
     (reduced? (ensure-reduced 12))
     (let* ([proc (completion (preserving-reduced (rflist)))]
            [acc (reduced '(done))])
       (eq? acc (proc acc 'x)))

     (eduction? (eduction (tidentity) '(1 2 3)))
     (transducible? '(1 2 3))
     (transducible? '#(1 2 3))
     (transducible? #vu8(1 2 3))
     (transducible? "abc")
     (transducible? (list->iter '(1 2 3)))
     (not (transducible? 123))

     ;; reducer boundary rejects a bare completing procedure.
     (error? (completion (case-lambda [() '()] [(acc) acc] [(acc x) (cons x acc)])))
     ;; transducer boundary rejects a bare procedure.
     (error? (transducer-name (lambda (r) r)))
     ;; constructors require symbolic names and procedures.
     (error? (make-transducer "map" (lambda (r) r)))
     (error? (make-reducer "list" (case-lambda [() '()] [(acc) acc] [(acc x) (cons x acc)]))))


(mat transducer-public-boundaries

     ;; source mode only accepts supported traversal modes.
     (error? (current-transducer-source-mode 'unknown))

     ;; composition helpers require transducer records.
     (error? (tcompose (tidentity) (lambda (reducer) reducer)))
     (error? (tchain (list (tidentity) (lambda (reducer) reducer))))

     ;; mapping, filtering, and side-effect transducers require procedures.
     (error? (tmap 12))
     (error? (tmap/i 12))
     (error? (tfilter 12))
     (error? (tfilter/i 12))
     (error? (tremove 12))
     (error? (tkeep 12))
     (error? (tkeep/i 12))
     (error? (tmapcat 12))
     (error? (ttap 12))
     (error? (tinspect 'debug 12))

     ;; stateful transducer bounds and predicates are checked at construction.
     (error? (ttake -1))
     (error? (tdrop -1))
     (error? (ttake-while 12))
     (error? (tdrop-while 12))
     (error? (tpartition-all 0))
     (error? (tdedupe-by 12))
     (error? (tdistinct-by 12))

     ;; terminal operations reject invalid reducers, callbacks, and sources.
     (error? (transduce (tidentity) (lambda (acc x) acc) '(1 2)))
     (error? (tfor-each (tidentity) 12 '(1 2)))
     (error? (eduction (tidentity) 12))
     (error? (source->iter 12))

     ;; type-specific terminals reject sources outside their declared type.
     (error? (list-transduce (tidentity) (rflist) '#(1 2)))
     (error? (vector-transduce (tidentity) (rflist) '(1 2)))
     (error? (bytevector-transduce (tidentity) (rflist) '(1 2)))
     (error? (string-transduce (tidentity) (rflist) '(#\a #\b)))
     (error? (fxvector-transduce (tidentity) (rflist) '#(1 2)))
     (error? (flvector-transduce (tidentity) (rflist) '#(1.0 2.0)))
     (error? (iter-transduce (tidentity) (rflist) '(1 2)))
     (error? (hashtable-transduce (tidentity) (rflist) '(1 2)))
     (error? (port-bytes-transduce (tidentity) (rflist) 12)))


(mat transducer-basic-pipelines

     (equal? '(2 4 6 8 10)
             (into 'list
                   (tcompose (tmap add1) (tfilter even?) (ttake 5))
                   '(0 1 2 3 4 5 6 7 8 9 10 11)))

     (equal? '#(2 4 6)
             (into 'vector
                   (tcompose (tmap add1) (tfilter even?))
                   '(1 2 3 4 5)))

     (= 6 (transduce (tmap string-length)
                     (rffxsum)
                     '("aa" "bbb" "c")))

     (equal? '(1 3 5)
             (transduce (tfilter odd?)
                        (rflist)
                        '()
                        '#(1 2 3 4 5)))

     (= 2 (transduce (tfilter positive?)
                     (rfcount)
                     '#(-2 0 4 6 -1)))

     (equal? '(2 4)
             (bytevector-transduce (tfilter even?)
                                   (rflist)
                                   #vu8(1 2 3 4 5)))

     (string=? "CHEZ"
               (string-transduce (tmap char-upcase)
                                 (make-reducer
                                  'chars
                                  (case-lambda
                                    [() '()]
                                    [(acc) (list->string (reverse acc))]
                                    [(acc x) (cons x acc)]))
                                 "chez"))

     (equal? '(11 21 31)
             (vector-transduce (tcompose (tmap add1) (ttake 3))
                               (rflist)
                               '#(10 20 30 40)))

     (equal? '(0 2 4)
             (list-transduce (tcompose (tdrop 2) (ttake 3))
                             (rflist)
                             '(9 8 0 2 4 6)))

     (equal? '(2 4 6)
             (iter-transduce (tcompose (tremove odd?) (ttake 3))
                             (rflist)
                             (list->iter '(1 2 3 4 5 6 7 8))))

     ;; unknown output destination is rejected.
     (error? (into 'set (tidentity) '(1 2 3)))
     ;; source boundary rejects unsupported source values.
     (error? (transduce (tidentity) (rflist) 12)))


(mat transducer-control-flow

     (equal? '(3 4 5)
             (into 'list
                   (tcompose (tdrop-while (lambda (x) (< x 3)))
                             (ttake-while (lambda (x) (< x 6))))
                   '(0 1 2 3 4 5 6 7)))

     (equal? '(a c e)
             (into 'list (ttake-nth 2) '(a b c d e)))

     (equal? '(b c d)
             (into 'list (tslice 1 4) '(a b c d e)))

     (equal? '(b d f)
             (into 'list (tslice 1 6 2) '(a b c d e f g)))

     (equal? '(1 2 3)
             (into 'list
                   (tchain (list (tmap add1) (ttake 3)))
                   '(0 1 2 3 4)))

     (let ([xf (ttake 2)])
       (and (equal? '(1 2) (into 'list xf '(1 2 3 4)))
            (equal? '(5 6) (into 'list xf '(5 6 7 8)))))

     (equal? '(#\a #\c)
             (into 'list
                   (tkeep (lambda (x)
                            (and (number? x) (integer->char (+ x 96)))))
                   '(1 #f a 3)))

     (= 6.5 (transduce (tidentity) (rfflsum) '#(1.0 2.5 3.0)))
     (= 10 (transduce (tidentity) (rfsum) '(1 2 3 4)))
     (= 4 (transduce (tidentity) (rfcount) "chez"))
     (equal? '#(a b c) (transduce (tidentity) (rfvector) '(a b c)))
     (equal? '(c b a) (transduce (tidentity) (rfreverselist) '(a b c)))
     (equal? '(2 3 4)
             (transduce (tidentity)
                        (rflist)
                        (eduction (tmap add1) '(1 2 3)))))


(mat transducer-phase2-transforms

     (equal? '((0 . a) (1 . b) (2 . c))
             (into 'list (tmap/i cons) '(a b c)))

     (equal? '(a c)
             (into 'list
                   (tfilter/i (lambda (i x) (even? i)))
                   '(a b c d)))

     (equal? '(0 2)
             (into 'list
                   (tkeep/i (lambda (i x) (and (even? i) i)))
                   '(a b c d)))

     (equal? '(uno 2 tres)
             (into 'list
                   (treplace '((1 . uno) (3 . tres)))
                   '(1 2 3)))

     (equal? '(1 2 3 4 #\a #\b)
             (into 'list
                   (tcat)
                   '((1 2) #(3 4) "ab")))

     (equal? '(1 1 2 2 3 3)
             (into 'list
                   (tmapcat (lambda (x) (list x x)))
                   '(1 2 3)))

     (equal? '(1 2 3 4 #\a)
             (into 'list
                   (tflatten)
                   '(1 (2 #(3 (4))) "a")))

     ;; replacement tables must be association lists.
     (error? (treplace '#((1 . one)))))


(mat transducer-phase2-stateful

     (equal? '(#(1 2) #(3 4))
             (into 'list (tpartition 2) '(1 2 3 4 5)))

     (equal? '(#(1 2) #(3 4) #(5))
             (into 'list (tpartition-all 2) '(1 2 3 4 5)))

     (equal? '(#(1 1) #(2 2 2) #(3))
             (into 'list (tpartition-by id) '(1 1 2 2 2 3)))

     (equal? '(1 2 1)
             (into 'list (tdedupe) '(1 1 2 2 1 1)))

     (equal? '(-1 2 3)
             (into 'list (tdedupe-by abs) '(-1 1 2 -2 3)))

     (equal? '(1 2 3)
             (into 'list (tdistinct) '(1 2 1 3 2)))

     (equal? '(-1 -2 3)
             (into 'list (tdistinct-by abs) '(-1 1 -2 2 3)))

     (equal? '(a x b x c)
             (into 'list (tinterpose 'x) '(a b c)))

     (let ([seen '()])
       (and (equal? '(1 2 3)
                    (into 'list
                          (ttap (lambda (x) (set! seen (cons x seen))))
                          '(1 2 3)))
            (equal? '(3 2 1) seen)))

     (let ([seen '()])
       (and (equal? '(a b)
                    (into 'list
                          (tinspect 'phase2
                                    (lambda (who x)
                                      (set! seen (cons (list who x) seen))))
                          '(a b)))
            (equal? '((phase2 b) (phase2 a)) seen)))

     (let ([seen '()])
       (tfor-each (tmap add1)
                  (lambda (x) (set! seen (cons x seen)))
                  '(1 2 3))
       (equal? '(4 3 2) seen))

     ;; partition sizes must be positive.
     (error? (tpartition 0))
     ;; partition-by requires a procedure.
     (error? (tpartition-by 12)))


(mat transducer-phase2-sources

     (string=? "ABC"
               (transduce (tmap char-upcase) (rfstring) "abc"))

     (equal? #vu8(2 4 6)
             (transduce (tmap (lambda (x) (fx* x 2)))
                        (rfbytevector)
                        #vu8(1 2 3)))

     (equal? '(2 3 4)
             (fxvector-transduce (tmap add1)
                                 (rflist)
                                 '#vfx(1 2 3)))

     (= 6.5 (flvector-transduce (tidentity)
                                (rfflsum)
                                '#vfl(1.0 2.5 3.0)))

     (let ([h (make-eq-hashtable)])
       (hashtable-set! h 'a 10)
       (hashtable-set! h 'b 20)
       (= 30 (hashtable-transduce (tidentity) (rfsum) h)))

     (let ([p (open-input-string "aa\nbbb\nc")])
       (let ([res (port-lines-transduce (tmap string-length) (rflist) p)]
             [open? (not (port-closed? p))])
         (close-port p)
         (and open? (equal? '(2 3 1) res))))

     (let ([p (open-bytevector-input-port #vu8(1 2 3))])
       (let ([res (port-bytes-transduce (tmap add1) (rflist) p)]
             [open? (not (port-closed? p))])
         (close-port p)
         (and open? (equal? '(2 3 4) res))))

     (equal? '(1 3)
             (list-transduce (tslice 1 5 2) (rflist) '(0 1 2 3 4 5)))

     (equal? '(1 3)
             (vector-transduce (tslice 1 5 2) (rflist) '#(0 1 2 3 4)))

     (equal? '(2 4)
             (fxvector-transduce (tslice 2 6 2) (rflist) '#vfx(0 1 2 3 4 5)))

     (equal? '(1.5 3.5)
             (flvector-transduce (tslice 1 4 2) (rflist) '#vfl(0.5 1.5 2.5 3.5)))

     (equal? '(2 4)
             (bytevector-transduce (tslice 2 6 2) (rflist) #vu8(0 1 2 3 4 5)))

     (equal? '(#\b #\d)
             (string-transduce (tslice 1 5 2) (rflist) "abcdef"))

     (equal? '(b d)
             (iter-transduce (tslice 1 5 2)
                             (rflist)
                             (list->iter '(a b c d e))))

     (equal? '(b d)
             (transduce (tslice 1 5 2)
                        (rflist)
                        (eduction (tidentity) '(a b c d e))))

     (iter? (source->iter '(1 2 3)))
     (eq? 'direct (current-transducer-source-mode))
     (equal? '(2 3 4)
             (parameterize ([current-transducer-source-mode 'iter])
               (transduce (tmap add1) (rflist) '#(1 2 3))))

     ;; slice step must be positive.
     (error? (tslice 0 3 0))
     ;; type-specific terminals no longer accept source slicing arities.
     (error? (list-transduce (tidentity) (rflist) '(1 2 3) 2))
     (error? (vector-transduce (tidentity) (rflist) '#(1 2 3) 0 3))
     (error? (bytevector-transduce (tidentity) (rflist) #vu8(1 2 3) 0 3 1))
     ;; caller-owned textual ports are required for line traversal.
     (error? (port-lines-transduce (tidentity) (rflist) 12)))


(mat transducer-phase3-sequence

     (let ([pulled 0])
       (let ([iter (sequence (tcompose (ttap (lambda (x) (set! pulled (+ pulled 1))))
                                       (ttake 2))
                             '(1 2 3))])
         (and (= 0 pulled)
              (= 1 (iter-next! iter))
              (= 1 pulled)
              (= 2 (iter-next! iter))
              (= 2 pulled)
              (iter-end? (iter-next! iter))
              (= 2 pulled))))

     (let ([iter (sequence (tcompose (tdrop 2) (ttake 3))
                           '(0 1 2 3 4 5 6))])
       (and (= 2 (iter-next! iter))
            (= 3 (iter-next! iter))
            (= 4 (iter-next! iter))
            (iter-end? (iter-next! iter))))

     (equal? '(2 4)
             (iter->list
              (sequence (tmap add1)
                        (eduction (tfilter odd?) '(1 2 3)))))

     (equal? '(2 3 4)
             (iter->list (source->iter (eduction (tmap add1) '(1 2 3)))))

     (equal? '(1 2 3)
             (iter->list (source->iter #vu8(1 2 3))))

     (equal? '(1 2 3)
             (iter->list (source->iter '#vfx(1 2 3))))

     (equal? '(1.0 2.5)
             (iter->list (source->iter '#vfl(1.0 2.5))))

     (= 8 (transduce1 (tfilter odd?) (rfsum) '(2 3 4 5)))
     (= 6.5 (transduce1 (tidentity) (rfflsum) '#vfl(1.0 2.5 3.0)))

     ;; transduce1 rejects an empty transformed stream.
     (error? (transduce1 (tfilter odd?) (rfsum) '(2 4 6))))


(mat transducer-phase3-reducers

     (= 1 (transduce (tidentity) (rfmin <) '(3 1 2)))
     (= 3 (transduce (tidentity) (rfmax <) '(3 1 2)))
     (= 24 (transduce (tidentity) (rfproduct) '(2 3 4)))
     (= 1 (transduce (tidentity) (rfproduct) '()))
     (eq? 'yes
          (transduce (tidentity)
                     (rfany (lambda (x) (and (symbol? x) 'yes)))
                     '(1 2 ok 3)))
     (not (transduce (tidentity) (rfany odd?) '(2 4 6)))
     (transduce (tidentity) (rfevery positive?) '(1 2 3))
     (not (transduce (tidentity) (rfevery positive?) '(1 -2 3)))

     ;; rfmin errors when no values are reduced.
     (error? (transduce (tidentity) (rfmin <) '()))
     ;; reducer predicates are required.
     (error? (rfany 12))
     (error? (rfevery 12))
     (error? (rfmin 12))
     ;; slicing transducers require valid positive step values.
     (error? (ttake-nth 0))
     (error? (tslice 0 3 0)))

(mat transducer-review-reducer-contracts
  (transduce (tpartition-by id)
             (rfany (lambda (chunk) (= (vector-ref chunk 0) 1))) '(1 2 3))

  (not (transduce (tpartition-by id)
                 (rfevery (lambda (chunk) (= (vector-ref chunk 0) 2))) '(1 2 3)))

  (let ([seen '()])
    (transduce (tcompose (tpartition-by id) (ttake 1)
                        (ttap (lambda (chunk) (set! seen (cons chunk seen)))))
               (rflist) '(1 2 3))
    (equal? seen '(#(1))))

  (let ([minimum (rfmin <)] [maximum (rfmax <)])
    (and (= 1 (transduce (tidentity) minimum '(1 2)))
         (= 4 (transduce (tidentity) minimum '(4 5)))
         (= 5 (transduce (tidentity) maximum '(4 5)))
         (= 2 (transduce (tidentity) maximum '(1 2)))))

  (= 0 (transduce (tidentity) (rfmin <) 0 '(3 4)))

  (= 9 (transduce (tidentity) (rfmax <) 9 '(3 4)))

  (= 0 (transduce (tidentity) (rfmin <) 0 '()))

  (= 1 (transduce1 (tidentity) (rfmin <) '(1 2 3)))

  (= 7 (transduce1 (tidentity) (rfmin <) '(7)))

  (= 7 (transduce1 (tidentity) (rfmax <) '(7)))

  ;; Empty reductions still raise after a reducer has completed a previous pass.
  (let ([minimum (rfmin <)])
    (transduce (tidentity) minimum '(1))
    (guard (condition [else #t])
      (transduce (tidentity) minimum '())
      #f))

  (equal? '(1 2) (into 'list (ttake (+ 1 (most-positive-fixnum))) '(1 2)))

  (null? (into 'list (tdrop (+ 1 (most-positive-fixnum))) '(1 2)))

  (equal? '(1 2) (into 'list (ttake 2.0) '(1 2 3)))

  (equal? '(3) (into 'list (tdrop 2.0) '(1 2 3))))

(define transducer-drain
  (lambda (iterator)
    (let ([builder (make-list-builder)])
      (let loop ()
        (let ([value (iter-next! iterator)])
          (if (iter-end? value)
              (builder)
              (begin (builder value) (loop))))))))

(mat transducer-review-sequence-lifecycle
  (let* ([source (list->iter '(1 2 3))]
         [output (sequence (tcompose (tmap/i cons) (ttake 2)) source)])
    (and (equal? '(0 . 1) (iter-next! output))
         (begin (iter-reset! output) #t)
         (equal? '((0 . 1) (1 . 2)) (transducer-drain output))
         (not (iter-finalized? source))
         (begin (iter-reset! output) #t)
         (equal? '((0 . 1) (1 . 2)) (transducer-drain output))
         (begin (iter-finalize! output) #t)
         (iter-finalized? source)))

  (let ([output (sequence (tpartition-all 2) '(1 2 3))])
    (and (equal? '(#(1 2) #(3)) (transducer-drain output))
         (begin (iter-reset! output) #t)
         (equal? '(#(1 2) #(3)) (transducer-drain output))))

  (let ([source (list->iter '(1 2 3))])
    (iter-next! source)
    (let ([output (sequence (tidentity) source)])
      (and (= 2 (iter-next! output))
           (= 3 (iter-next! output))
           (iter-end? (iter-next! output))
           (not (iter-finalized? source))))))

(define-record-type (review-source make-review-source review-source?)
  (fields items (mutable finalizations)))

(iter-register-source!
 review-source?
 (lambda (source)
   (let ([iterator (list->iter (review-source-items source))])
     (make-iter
      (lambda () (iter-next! iterator))
      (lambda () (iter-reset! iterator))
      (lambda ()
        (review-source-finalizations-set! source (+ 1 (review-source-finalizations source)))
        (iter-finalize! iterator))))))

(mat transducer-review-generated-source-ownership
  (andmap
   (lambda (mode)
     (parameterize ([current-transducer-source-mode mode])
       (let ([source (make-review-source '(1 2 3) 0)])
         (and (= 6 (transduce (tidentity) (rfsum) source))
              (= 1 (review-source-finalizations source))))))
   '(direct iter))

  (let ([source (make-review-source '(1 2 3) 0)])
    (and (equal? '(1) (into 'list (ttake 1) source))
         (= 1 (review-source-finalizations source))))

  ;; A callback failure must release the iterator created for a registered source.
  (let ([source (make-review-source '(1 2 3) 0)])
    (and (guard (exn [else #t])
           (transduce (tmap (lambda (x) (error 'review "callback failed"))) (rfsum) source)
           #f)
         (= 1 (review-source-finalizations source))))

  (let ([source (make-review-source '(1 2 3) 0)])
    (and (eq? 'escaped
              (call/1cc
               (lambda (stop)
                 (transduce (ttap (lambda (x) (stop 'escaped))) (rfsum) source))))
         (= 1 (review-source-finalizations source))))

  (let ([source (list->iter '(1 2 3))])
    (and (= 1 (transduce (ttake 1) (rfsum) source))
         (not (iter-finalized? source))
         (= 2 (iter-next! source))
         (begin (iter-finalize! source) #t))))

(mat transducer-review-sequence-initialization
  (let ([pulled 0] [completed 0])
    (let* ([xform
            (make-transducer
             'initially-reduced
             (lambda (reducer)
               (let ([proc (completion reducer)])
                 (make-reducer
                  'initially-reduced
                  (case-lambda
                    [() (reduced (proc))]
                    [(acc) (set! completed (+ completed 1)) (proc acc)]
                    [(acc x) (proc acc x)])))))]
           [source (make-iter (lambda () (set! pulled (+ pulled 1)) 1) void)]
           [output (sequence xform source)])
      (and (iter-end? (iter-next! output))
           (= 0 pulled)
           (= 1 completed)
           (not (iter-finalized? source))
           (begin (iter-reset! output) #t)
           (iter-end? (iter-next! output))
           (= 0 pulled)
           (= 2 completed)
           (begin (iter-finalize! output) #t))))

  (let ([xform
         (make-transducer
          'initial-emission
          (lambda (reducer)
            (let ([proc (completion reducer)])
              (make-reducer
               'initial-emission
               (case-lambda
                 [() (proc (proc) 'start)]
                 [(acc) (proc acc)]
                 [(acc x) (proc acc x)])))))])
    (let ([output (sequence xform '(1 2))])
      (and (equal? '(start 1 2) (transducer-drain output))
           (begin (iter-reset! output) #t)
           (equal? '(start 1 2) (transducer-drain output))
           (begin (iter-finalize! output) #t)))))

(mat transducer-review-sequence-errors
  ;; A failing sequence step closes its generated source and exhausts that pass.
  (let* ([source (make-review-source '(1 2) 0)]
         [output (sequence (tmap (lambda (x) (error 'review "sequence failed"))) source)])
    (and (guard (exn [else #t]) (iter-next! output) #f)
         (= 1 (review-source-finalizations source))
         (iter-end? (iter-next! output))
         (begin (iter-reset! output) #t)
         (guard (exn [else #t]) (iter-next! output) #f)
         (= 2 (review-source-finalizations source))
         (begin (iter-finalize! output) #t)
         (= 2 (review-source-finalizations source))))

  ;; Construction failure after opening an adapter must release its source iterator.
  (let ([source (make-review-source '(1 2) 0)])
    (and (guard (exn [else #t])
           (sequence (make-transducer 'broken (lambda (reducer) (error 'review "bad xform")))
                     source)
           #f)
         (= 1 (review-source-finalizations source))))

  ;; Failing traversal does not close a caller-supplied iterator until explicit finalization.
  (let* ([source (list->iter '(1 2))]
         [output (sequence (tmap (lambda (x) (error 'review "sequence failed"))) source)])
    (and (guard (exn [else #t]) (iter-next! output) #f)
         (not (iter-finalized? source))
         (iter-end? (iter-next! output))
         (begin (iter-finalize! output) #t)
         (iter-finalized? source))))

(define review-initial-emission
  (lambda (value)
    (make-transducer
     'review-initial-emission
     (lambda (reducer)
       (let ([proc (completion reducer)])
         (make-reducer
          'review-initial-emission
          (case-lambda
            [() (proc (proc) value)]
            [(acc) (proc acc)]
            [(acc x) (proc acc x)])))))))

(mat transducer-review-terminal-initialization
  (= 13 (transduce (review-initial-emission 10) (rfsum) '(1 2)))

  (= 13 (list-transduce (review-initial-emission 10) (rfsum) '(1 2)))

  (= 13 (vector-transduce (review-initial-emission 10) (rfsum) '#(1 2)))

  (= 13 (bytevector-transduce (review-initial-emission 10) (rfsum) #vu8(1 2)))

  (= 13 (fxvector-transduce (review-initial-emission 10) (rfsum) (fxvector 1 2)))

  (= 13.0 (flvector-transduce (review-initial-emission 10) (rfsum) (flvector 1.0 2.0)))

  (= 205 (string-transduce (tcompose (tmap char->integer) (review-initial-emission 10))
                           (rfsum) "ab"))

  (let ([table (make-eq-hashtable)])
    (hashtable-set! table 'a 1)
    (hashtable-set! table 'b 2)
    (= 13 (hashtable-transduce (review-initial-emission 10) (rfsum) table)))

  (let ([source (list->iter '(1 2))])
    (let ([result (iter-transduce (review-initial-emission 10) (rfsum) source)])
      (iter-finalize! source)
      (= 13 result)))

  (= 13 (call-with-port (open-string-input-port "1\n2")
                       (lambda (port)
                         (port-lines-transduce
                          (tcompose (tmap string->number) (review-initial-emission 10))
                          (rfsum) port))))

  (= 13 (call-with-port (open-bytevector-input-port #vu8(1 2))
                       (lambda (port)
                         (port-bytes-transduce (review-initial-emission 10) (rfsum) port))))

  (= 13 (transduce1 (review-initial-emission 10) (rfsum) '(1 2)))

  (= 15 (transduce (review-initial-emission 10) (rfsum)
                   (eduction (tmap add1) '(1 2))))

  (= 3 (transduce (review-initial-emission 10) (rfsum) 0 '(1 2)))

  (= 3 (list-transduce (review-initial-emission 10) (rfsum) 0 '(1 2)))

  (equal? '(10) (into 'list (review-initial-emission 10) '()))

  ;; Invalid reducers must be rejected at the driver boundary before record access.
  (andmap
   (lambda (terminal)
     (guard (exn [(and (who-condition? exn)
                       (memq (condition-who exn) '(pcheck transduce))) #t]
                 [else #f])
       (terminal (tidentity) 12 '(1 2))
       #f))
   (list list-transduce)))

(define-record-type (review-failing-finalizer-source
                     make-review-failing-finalizer-source review-failing-finalizer-source?)
  (fields (mutable attempts)))

(iter-register-source!
 review-failing-finalizer-source?
 (lambda (source)
   (make-iter
    (lambda () iter-end) void
    (lambda ()
      (review-failing-finalizer-source-attempts-set!
       source (+ 1 (review-failing-finalizer-source-attempts source)))
      (error 'review "finalizer failed")))))

(mat transducer-review-failing-finalizer
  ;; A finalizer error on exhaustion must be propagated without an automatic retry.
  (let* ([source (make-review-failing-finalizer-source 0)]
         [output (sequence (tidentity) source)])
    (and (guard (exn [else #t]) (iter-next! output) #f)
         (= 1 (review-failing-finalizer-source-attempts source))
         (iter-end? (iter-next! output)))))

(mat transducer-review-failing-finalizer-reset
  ;; Reset must not repeat a failing old-source finalizer in its error handler.
  (let* ([source (make-review-failing-finalizer-source 0)]
         [output (sequence (tidentity) source)])
    (and (guard (exn [else #t]) (iter-reset! output) #f)
         (= 1 (review-failing-finalizer-source-attempts source)))))

(mat transducer-review-wide-count-seeds
  (= (+ 1 (most-positive-fixnum))
     (transduce (tidentity) (rfcount) (most-positive-fixnum) '(item)))

  (= (+ 4 (most-positive-fixnum))
     (transduce (tidentity) (rfcount) (+ 2 (most-positive-fixnum)) '(a b))))
