(import (cspp))

;; Type errors must be reported before an unchecked primitive can fault.
(define transducer-type-error?
  (lambda (run)
    (guard (condition [(who-condition? condition) #t] [else #f])
      (run)
      #f)))

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

(mat transducer-review-boolean-seeds
  (let ([calls 0])
    (and (eq? 'decided
              (transduce (tidentity)
                         (rfany (lambda (x) (set! calls (+ calls 1)) #f))
                         'decided '(#f #f)))
         (= calls 0)))

  (let ([calls 0])
    (and (not (transduce (tidentity)
                        (rfevery (lambda (x) (set! calls (+ calls 1)) #t))
                        #f '(#t #t)))
         (= calls 0)))

  (let ([calls 0])
    (and (transduce1 (tidentity)
                     (rfany (lambda (x) (set! calls (+ calls 1)) #f)) '(#t #f))
         (= calls 0)))

  (let ([calls 0])
    (and (not (transduce1 (tidentity)
                          (rfevery (lambda (x) (set! calls (+ calls 1)) #t)) '(#f #t)))
         (= calls 0)))

  (andmap
   (lambda (terminal+source)
     (let ([terminal (car terminal+source)] [source (cdr terminal+source)] [calls 0])
       (and (terminal (tidentity)
                      (rfany (lambda (x) (set! calls (+ calls 1)) #f)) #t source)
            (not (terminal (tidentity)
                           (rfevery (lambda (x) (set! calls (+ calls 1)) #t)) #f source))
            (= calls 0))))
   (list (cons list-transduce '(1 2))
         (cons vector-transduce '#(1 2))
         (cons bytevector-transduce #vu8(1 2))
         (cons string-transduce "ab")
         (cons fxvector-transduce (fxvector 1 2))
         (cons flvector-transduce (flvector 1.0 2.0))
         (cons hashtable-transduce
               (let ([table (make-eq-hashtable)])
                 (hashtable-set! table 'a 1)
                 table)))))

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
         (not (iter-finalized? source))))

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

(mat transducer-review-wide-count-seeds
  (= (+ 1 (most-positive-fixnum))
     (transduce (tidentity) (rfcount) (most-positive-fixnum) '(item)))

  (= (+ 4 (most-positive-fixnum))
     (transduce (tidentity) (rfcount) (+ 2 (most-positive-fixnum)) '(a b))))

(mat transducer-review-cat-source-protocol
  (equal? '(1 2 3 4)
          (into 'list (tcat) (list (array 1 2) (eduction (tmap add1) '(2 3)))))

  (equal? '(1 2) (into 'list (tmapcat (lambda (value) (array value))) '(1 2))))

(mat transducer-review-cat-lifecycle
  (let ([source (make-review-source '(1 2 3) 0)])
    (and (equal? '(1) (into 'list (tcompose (tcat) (ttake 1)) (list source '(4 5))))
         (= 1 (review-source-finalizations source))))

  (let ([source (list->iter '(1 2 3))])
    (let ([result (into 'list (tcompose (tcat) (ttake 1)) (list source))])
      (and (equal? '(1) result) (not (iter-finalized? source))
           (= 2 (iter-next! source)) (begin (iter-finalize! source) #t))))

  (let ([source (list->iter '(1 2 3))])
    (let ([result (into 'list (tcompose (tcat) (ttake 1))
                        (list (eduction (tmap add1) source)))])
      (and (equal? '(2) result) (not (iter-finalized? source))
           (= 2 (iter-next! source)) (begin (iter-finalize! source) #t))))

  (equal? '(1 4 5) (into 'list (tcat) (list (eduction (ttake 1) '(1 2 3)) '(4 5))))

  (equal? '(#(1 2) #(3 4))
          (into 'list (tcompose (tcat) (tpartition-all 2))
                (list (eduction (tidentity) '(1 2 3)) '(4)))))

(define-record-type (review-generator-source make-review-generator-source review-generator-source?)
  (fields path items (mutable finalizations)))

(iter-register-source!
 review-generator-source?
 (lambda (source)
   (let ([producer (nav-traverse->iter (review-generator-source-path source)
                                      (review-generator-source-items source))])
     (make-iter
      (lambda () (iter-next! producer))
      (lambda () (iter-reset! producer))
      (lambda ()
        (review-generator-source-finalizations-set!
         source (+ 1 (review-generator-source-finalizations source)))
        (iter-finalize! producer))))))

(mat transducer-generator-sequence
  (equal? '(1 2 3 4)
          (iter->list (sequence (tidentity) (nav-traverse->iter nav/all '(1 2 3 4)))))

  (equal? '(1 2 3 4)
          (iter->list (sequence (tidentity)
                                (sequence (tidentity)
                                          (nav-traverse->iter nav/all '(1 2 3 4))))))

  (equal? '(#(1 2) #(3))
          (iter->list (sequence (tpartition-all 2)
                                (nav-traverse->iter nav/all '(1 2 3)))))

  (let* ([source (make-review-generator-source nav/all '(1 2 3 4) 0)]
         [output (sequence (tmapcat (lambda (value) (list value value))) source)])
    (and (= 1 (iter-next! output)) (= 1 (iter-next! output))
         (= 0 (review-generator-source-finalizations source))
         (equal? '(2 2 3 3 4 4) (transducer-drain output))
         (= 0 (review-generator-source-finalizations source))
         (begin (iter-reset! output) #t)
         (equal? '(1 1 2 2 3 3 4 4) (iter->list output))
         (= 1 (review-generator-source-finalizations source)))))


(define-record-type (review-pipeline-source make-review-pipeline-source review-pipeline-source?)
  (fields iterator (mutable finalizations)))
(iter-register-source!
 review-pipeline-source?
 (lambda (source)
   (let ([upstream (review-pipeline-source-iterator source)])
     (make-iter
      (lambda () (iter-next! upstream))
      (lambda () (iter-reset! upstream))
      (lambda ()
        (review-pipeline-source-finalizations-set!
         source (+ 1 (review-pipeline-source-finalizations source)))
        (iter-finalize! upstream))))))

(define review-continuation-iter
  (lambda (items)
    (let ([resume #f] [consumer #f] [done? #f])
      (define reset!
        (lambda ()
          (set! done? #f)
          (set! consumer #f)
          (set! resume
                (lambda ()
                  (for-each
                   (lambda (item)
                     ;; Suspend each producer segment until its next consumer arrives.
                     (call/1cc (lambda (continue)
                                 (set! resume continue)
                                 (consumer item))))
                   items)
                  (set! done? #t)
                  (consumer iter-end)))))
      (reset!)
      (make-iter
       (lambda ()
         (if done?
             iter-end
             ;; Each pull uses the current caller's one-shot return continuation.
             (call/1cc (lambda (return) (set! consumer return) (resume)))))
       reset!))))

(mat transducer-independent-continuation-sources
  (equal? '(1 2 3 4)
          (iter->list (sequence (tidentity) (review-continuation-iter '(1 2 3 4)))))

  (equal? '(#(1 2) #(3))
          (iter->list (sequence (tpartition-all 2) (review-continuation-iter '(1 2 3)))))

  (equal? '(1 1 2 2 3 3)
          (iter->list (sequence (tmapcat (lambda (value) (list value value)))
                                (sequence (tidentity) (review-continuation-iter '(1 2 3)))))))

(mat transducer-prestarted-continuation-sources
  (let ([producer (review-continuation-iter '(1 2 3))])
    (and (= 1 (iter-next! producer))
         (equal? '(2 3) (iter->list (sequence (tidentity) producer)))))

  (let ([producer (review-continuation-iter '(1 2 3))])
    (and (= 1 (iter-next! producer))
         (equal? '(2 3 4) (iter->list (sequence (tcat) (list producer '(4)))))))

  (let ([producer (review-continuation-iter '(1 2 3))])
    (and (= 1 (iter-next! producer))
         (equal? '(#(2 3) #(4))
                 (iter->list (sequence (tcompose (tcat) (tpartition-all 2))
                                       (list producer '(4)))))))

  (let* ([producer (review-continuation-iter '(1 2 3))]
         [source (make-review-pipeline-source producer 0)])
    (and (= 1 (iter-next! producer))
         (equal? '(2 3) (into 'list (tidentity) source))
         (= 1 (review-pipeline-source-finalizations source)))))

(define transducer-test-gate
  (lambda ()
    (let ([mutex (make-mutex)] [changed (make-condition)] [open? #f])
      (values
       (lambda ()
         (with-mutex mutex
           (let loop ()
             (unless open? (condition-wait changed mutex) (loop)))))
       (lambda ()
         (with-mutex mutex (set! open? #t) (condition-broadcast changed)))))))

(define independent-transducer-pulls?
  (lambda (fork-in-callback?)
    (let-values ([(wait-start start!) (transducer-test-gate)]
                 [(wait-enter entered!) (transducer-test-gate)]
                 [(wait-release release!) (transducer-test-gate)])
      (let* ([other (make-iter (lambda () (entered!) (wait-release) 'other) void)]
             [worker (and (not fork-in-callback?)
                          (fork-thread (lambda () (wait-start) (iter-next! other))))]
             [output (sequence
                      (ttap (lambda (value)
                              (when (= value 1)
                                (if fork-in-callback?
                                    (set! worker (fork-thread (lambda () (iter-next! other))))
                                    (start!))
                                (wait-enter))))
                      '(1 2 3))]
             [first (iter-next! output)]
             [second (iter-next! output)])
        (release!)
        (thread-join worker)
        (iter-finalize! output)
        (equal? (list first second) '(1 2))))))

(mat transducer-thread-isolation
  (independent-transducer-pulls? #f)

  ;; Forking inside a reducer must not give the child its parent's mutable pull scope.
  (independent-transducer-pulls? #t)

  (let-values ([(wait-read read!) (transducer-test-gate)]
               [(wait-done done!) (transducer-test-gate)])
    (let* ([observed #f]
           [worker (fork-thread
                    (lambda ()
                      (wait-read)
                      (set! observed (current-transducer-source-mode))
                      (done!)))])
      (parameterize ([current-transducer-source-mode 'iter]) (read!) (wait-done))
      (thread-join worker)
      (eq? observed 'direct)))

  ;; Configuration, unlike internal traversal state, is intentionally inherited.
  (let ([observed #f])
    (parameterize ([current-transducer-source-mode 'iter])
      (thread-join (fork-thread
                    (lambda () (set! observed (current-transducer-source-mode))))))
    (eq? observed 'iter)))

(import (cspp concurrency fiber))

(define transducer-yield-in-phase
  (lambda (phase)
    (make-transducer
     'yield-in-phase
     (lambda (downstream)
       (let ([proc (completion downstream)])
         (make-reducer
          'yield-in-phase
          (case-lambda
            [() (when (eq? phase 'init) (fiber-yield)) (proc)]
            [(acc)
             (when (eq? phase 'complete) (fiber-yield))
             (proc acc)]
            [(acc value)
             (when (eq? phase 'step) (fiber-yield))
             (proc acc value)])))))))

(mat transducer-fiber-suspension
  (andmap
   (lambda (workers)
     (andmap
      (lambda (phase)
        (equal? '(1 2 3)
                (run-fibers
                 (lambda () (iter->list (sequence (transducer-yield-in-phase phase) '(1 2 3))))
                 workers 1000)))
      '(init step complete)))
   '(1 4))

  (andmap
   (lambda (workers)
     (= 1000 (run-fibers
              (lambda () (length (iter->list (sequence (tidentity) (iota 1000)))))
              workers 100)))
   '(1 4))

  (equal? '(1 2 3)
          (run-fibers
           (lambda () (transduce (transducer-yield-in-phase 'step) (rflist) (array 1 2 3)))
           4 1000)))

(mat transducer-terminal-resource-chain
  (let* ([source (make-review-source '(1 2 3) 0)]
         [output (sequence (tidentity) source)])
    (and (equal? '(1 2 3) (transducer-drain output))
         (= 0 (review-source-finalizations source))
         (begin (iter-reset! output) #t)
         (equal? '(1) (into 'list (ttake 1) output))
         (= 1 (review-source-finalizations source))))

  (let ([source (make-review-source '(1 2 3) 0)])
    (equal? '(#(1 2 3))
            (into 'list
                  (tcompose (tcat)
                            (ttap (lambda (value)
                                    (unless (= 0 (review-source-finalizations source))
                                      (error 'resource-test "premature finalization"))))
                            (tpartition-all 3))
                  (list source))))

  ;; Finalizer failures propagate from a terminal; lazy exhaustion never invokes cleanup.
  (let* ([source (make-review-failing-finalizer-source 0)]
         [output (sequence (tidentity) source)])
    (and (iter-end? (iter-next! output))
         (= 0 (review-failing-finalizer-source-attempts source))
         (guard (exn [else #t]) (iter->list output) #f)
         (= 1 (review-failing-finalizer-source-attempts source))))

  ;; A callback error propagates. The chain's post-error state is intentionally untested.
  (guard (exn [else #t])
    (transduce (tmap (lambda (value) (error 'callback "failed"))) (rflist) '(1 2))
    #f))

(mat transducer-shared-descriptions-in-fibers
  (let* ([xform (tcompose (tmap/i cons) (ttake 3) (tpartition-all 2))]
         [reducer (rfvector)]
         [expected '#(#((0 . 10) (1 . 20)) #((2 . 30)))])
    (andmap
     (lambda (workers)
       (run-fibers
        (lambda ()
          (let ([done (make-channel)])
            (do ([task 0 (fx1+ task)]) ((fx= task 4))
              (spawn-fiber
               (lambda ()
                 (channel-put!
                  done
                  (let loop ([round 0])
                    (or (fx= round 50)
                        (begin
                          (fiber-yield)
                          (and (equal? expected (transduce xform reducer '(10 20 30 40)))
                               (loop (fx1+ round))))))))))
            (let loop ([remaining 4] [okay? #t])
              (if (fxzero? remaining) okay?
                  (let ([next (channel-get done)])
                    (loop (fx1- remaining) (and okay? next)))))))
        workers 100))
     '(1 4))))

(mat transducer-fiber-mode-scope
  (run-fibers
   (lambda ()
     (let ([ready (make-channel)] [release (make-channel)] [done (make-channel)])
       (spawn-fiber
        (lambda ()
          (parameterize ([current-transducer-source-mode 'iter])
            (channel-put! ready #t)
            (channel-get release)
            (fiber-yield)
            (channel-put! done (eq? 'iter (current-transducer-source-mode))))))
       (channel-get ready)
       (let ([default? (eq? 'direct (current-transducer-source-mode))])
         (channel-put! release #t)
         (let ([scoped? (channel-get done)])
           (and default? scoped? (eq? 'direct (current-transducer-source-mode)))))))
   4 100))

(mat transducer-continuation-resumption
  (let ([resume #f] [resumptions 0] [results '()])
    (let ([result (transduce
                   (tmap (lambda (value)
                           (if (= value 2)
                               ;; Save a callback continuation; no mutable source is rewound.
                               (call/cc (lambda (continuation) (set! resume continuation) value))
                               value)))
                   (rflist) '(1 2 3))])
      (set! results (cons result results))
      (if (< resumptions 2)
          (begin (set! resumptions (+ resumptions 1)) (resume (+ 8 resumptions)))
          (equal? '((1 10 3) (1 9 3) (1 2 3)) results)))))

(mat transducer-suspension-keeps-emissions
  (run-fibers
   (lambda ()
     (let* ([xform
             (make-transducer
              'emitting-yield
              (lambda (downstream)
                (let ([proc (completion downstream)])
                  (make-reducer
                   'emitting-yield
                   (case-lambda
                     [() (let ([acc (proc (proc) 'start)]) (fiber-yield) acc)]
                     [(acc)
                      (let ([acc (proc acc 'finish-first)])
                        (fiber-yield)
                        (proc (proc acc 'finish-last)))]
                     [(acc value) (proc acc value)])))))]
            [output (sequence xform '(1 2))])
       (and (equal? '(start 1 2 finish-first finish-last) (transducer-drain output))
            (begin (iter-reset! output) #t)
            (equal? '(start 1 2 finish-first finish-last) (iter->list output)))))
   4 100)

  (run-fibers
   (lambda ()
     (let ([tail '(1 2 3)])
       (let ([source (make-iter
                      (lambda ()
                        (fiber-yield)
                        (if (null? tail) iter-end
                            (let ([value (car tail)]) (set! tail (cdr tail)) value)))
                      (lambda () (set! tail '(1 2 3))))])
         (equal? '(1 2 3) (iter->list (sequence (tidentity) source))))))
   4 100))


(mat transducer-review-empty-limits
  ;; No producer read or preceding callback is needed for a known-empty limit.
  (andmap
   (lambda (xform)
     (andmap
      (lambda (mode)
        (parameterize ([current-transducer-source-mode mode])
          (andmap
           (lambda (run)
             (let ([pulls 0] [closed 0])
               (let ([source (make-iter
                              (lambda () (set! pulls (+ pulls 1)) 'unexpected)
                              void (lambda () (set! closed (+ closed 1))))])
                 (and (equal? '() (run xform source))
                      (= pulls 0) (= closed 1)))))
           (list (lambda (xform source) (transduce xform (rflist) source))
                 (lambda (xform source) (transduce xform (rflist) '() source))
                 (lambda (xform source) (iter-transduce xform (rflist) source))
                 (lambda (xform source) (iter-transduce xform (rflist) '() source))
                 (lambda (xform source) (iter->list (sequence xform source)))))))
      '(direct iter)))
   (list (ttake 0) (tslice 0 0) (tslice 5 5)))

  ;; A buffering stage must not wait for a batch upstream of an empty limit.
  (let ([pulls 0])
    (let ([source (make-iter
                   (lambda ()
                     (set! pulls (+ pulls 1))
                     (when (> pulls 8) (error 'producer "unexpected input read"))
                     pulls) void)])
      (and (equal? '() (into 'list (tcompose (tpartition 1000) (ttake 0)) source))
           (= pulls 0))))

  (let ([effects 0])
    (and (equal? '() (into 'list
                           (tcompose (ttap (lambda (value) (set! effects (+ effects 1))))
                                     (tslice 3 3))
                           '(1 2 3 4)))
         (= effects 0)))

  ;; Lazy reset must recreate the stop flag as well as ordinary reducer counters.
  (let ([pulls 0])
    (let ([cursor (sequence (tcompose (tpartition-all 3) (ttake 0))
                            (make-iter (lambda () (set! pulls (+ pulls 1)) pulls) void))])
      (and (equal? '() (iter->list cursor))
           (begin (iter-reset! cursor) #t)
           (equal? '() (iter->list cursor))
           (= pulls 0))))

  (iter-end? (iter-next! (sequence (tidentity) iter-end))))

(mat transducer-review-empty-limit-lifecycle
  ;; Initializers still run for unseeded reductions; explicit seeds bypass them.
  (let ([initialized 0] [completed 0] [stepped 0])
    (let ([reducer (make-reducer
                    'empty-limit
                    (case-lambda
                      [() (set! initialized (+ initialized 1)) 10]
                      [(acc) (set! completed (+ completed 1)) (+ acc 1)]
                      [(acc value) (set! stepped (+ stepped 1)) (+ acc value)]))])
      (and (= 11 (transduce (ttake 0) reducer '(1 2)))
           (= 21 (transduce (ttake 0) reducer 20 '(1 2)))
           (= initialized 1) (= completed 2) (= stepped 0))))

  (call-with-port
   (open-string-input-port "first\nsecond\n")
   (lambda (port)
     (and (equal? '() (port-lines-transduce (tslice 5 5) (rflist) port))
          (string=? "first" (get-line port)))))

  (call-with-port
   (open-bytevector-input-port #vu8(1 2))
   (lambda (port)
     (and (equal? '() (port-bytes-transduce (ttake 0) (rflist) '() port))
          (= 1 (get-u8 port))))))

(mat transducer-review-reset-nested-cleanup
  ;; Reset rewinds the outer source, while tcat retains the inner destructive position.
  ;; Historical inner resources still belong to successful terminal cleanup.
  (let ([remaining '(1)] [resets 0] [closed 0])
    (let* ([inner (make-iter
                   (lambda ()
                     (if (null? remaining) iter-end
                         (let ([value (car remaining)])
                           (set! remaining (cdr remaining)) value)))
                   (lambda () (set! resets (+ resets 1)) (set! remaining '(1)))
                   (lambda () (set! closed (+ closed 1))))]
           [cursor (sequence (tcat) (list inner))])
      (and (equal? '(1) (let collect ([values '()])
                         (let ([value (iter-next! cursor)])
                           (if (iter-end? value) (reverse values)
                               (collect (cons value values))))))
           (= closed 0)
           (begin (iter-reset! cursor) #t)
           (= resets 0)
           (equal? '() (iter->list cursor))
           (= closed 1)))))

(define-record-type review-unused-source (fields values))

(mat transducer-empty-pass-skips-source-materialization
  (let ([created 0])
    (iter-register-source!
     review-unused-source?
     (lambda (source)
       (set! created (+ created 1))
       (list->iter (review-unused-source-values source))))
    (let* ([source (make-review-unused-source '(1 2))]
           [cursor (sequence (ttake 0) source)])
      (and (equal? '() (into 'list (ttake 0) source))
           (equal? '() (iter->list cursor))
           (begin (iter-reset! cursor) #t)
           (equal? '() (iter->list cursor))
           (= created 0)))))

(import (cspp private iter-state))

(mat transducer-shared-cleanup-forest
  ;; Child-link counts measure graph work, not wall-clock timing. Distinct zero-input
  ;; roots share one resource; its finalizer must run exactly once.
  (let ([visits 0] [closed 0])
    (let loop ([depth 0]
               [previous (make-iter (lambda () iter-end) void
                                    (lambda () (set! closed (+ closed 1))))]
               [roots '()])
      (if (= depth 64)
          (and (equal? '() (into 'list (tcat) roots))
               (= closed 1) (< visits 256))
          (let ([next (mk-$iter (lambda () iter-end) void #f
                               (lambda () (set! visits (+ visits 1)) (list previous)))])
            (loop (+ depth 1) next (cons next roots))))))

  ;; A free wrapper is retained through its existing free child. A later input
  ;; acquires a resource reachable only through that earlier wrapper at cleanup.
  (let ([children (list (list->iter '()))] [closed 0])
    (let* ([resource (make-iter (lambda () iter-end) void
                                (lambda () (set! closed (+ closed 1))))]
           [wrapper (mk-$iter (lambda () iter-end) void #f (lambda () children))]
           [source (list wrapper 'acquire)]
           [xform (tcompose
                   (tmap (lambda (value)
                           (if (eq? value 'acquire)
                               (begin (set! children (list resource)) '())
                               value)))
                   (tcat))])
      (and (equal? '() (into 'list xform source)) (= closed 1)))))

(mat transducer-free-wrapper-retention
  ;; Cleanup links, not a GC timing assumption, show whether exhausted free roots are retained.
  (andmap
   (lambda (wrap)
     (let ([cursor (sequence (tmapcat (lambda (value) (wrap (range 1)))) (range 32))])
       (let pass ([remaining 3])
         (and (= 32 (length (iter->list cursor)))
              (not (iter-finalized? cursor))
              (= 1 (length (($iter-children cursor))))
              (or (= remaining 1)
                  (begin (iter-reset! cursor) (pass (- remaining 1))))))))
   (list (lambda (source) (iter-concat source))
         (lambda (source) (iter-zip source (range 1)))
         (lambda (source) (iter-interleave source (range 0)))
         (lambda (source) (iter-distinct = source))
         (lambda (source) (iter-sorted < source)))))

(mat transducer-empty-dynamic-cleanup-candidate
  ;; A dynamic graph can gain its first child after its empty wrapper was consumed.
  (andmap
   (lambda (wrap)
     (let ([children '()] [closed 0])
       (let* ([resource (make-iter (lambda () iter-end) void
                                   (lambda () (set! closed (+ closed 1))))]
              [dynamic (mk-$iter (lambda () iter-end) void #f (lambda () children))]
              [wrapper (wrap dynamic)]
              [xform (tcompose
                      (tmap (lambda (value)
                              (if (eq? value 'acquire)
                                  (begin (set! children (list resource)) '())
                                  value)))
                      (tcat))])
         (and (equal? '() (into 'list xform (list wrapper 'acquire)))
              (= closed 1)
              (iter-finalized? wrapper)))))
   (list (lambda (source) source)
         (lambda (source) (iter-concat source))
         (lambda (source) (iter-zip source (range 0)))
         (lambda (source) (iter-interleave source (range 0)))
         (lambda (source) (iter-distinct = source))
         (lambda (source) (iter-sorted < source)))))

(mat transduce1-seed-resumption
  (let ([saved #f] [phase 0])
    (guard (condition [else #f])
      (let ([result
             (transduce1
              (tmap (lambda (value)
                      (if saved value
                          ;; Capture before the first transformed value seeds the reduction.
                          (call/cc (lambda (resume) (set! saved resume) value)))))
              (rfsum) '(1 2 3))])
        (if (= phase 0)
            (begin (set! phase 1) (saved 10))
            (= result 15)))))

  ;; Reject an empty transformed source when resumption discards its original only seed.
  (let ([saved #f] [phase 0])
    (guard (condition [(and (= phase 1) (who-condition? condition)
                            (eq? 'transduce1 (condition-who condition))
                            (message-condition? condition)
                            (equal? "empty transformed source" (condition-message condition)))
                       #t]
                      [else #f])
      (transduce1
       (tfilter (lambda (value)
                  (if saved #t
                      ;; Revisit the original filter decision without accepting its value.
                      (call/cc (lambda (resume) (set! saved resume) #t)))))
       (rfsum) '(1))
      (if (= phase 0) (begin (set! phase 1) (saved #f)) #f)))

  (not (transduce1 (tidentity)
                  (make-reducer 'false-seed
                                (case-lambda [() 'unused] [(acc) acc] [(acc x) acc]))
                  '(#f 1))))

(mat transducer-free-nested-sequence-retention
  ;; Exhausted free sequences must not accumulate as cleanup roots across reset passes.
  (andmap
   (lambda (make-source)
     (let ([cursor (sequence
                    (tmapcat (lambda (value)
                               (sequence (tcompose (tmap add1) (ttake 1)) (make-source))))
                    (range 32))])
       (let pass ([remaining 3])
         (and (= 32 (length (iter->list cursor)))
              (not (iter-finalized? cursor))
              (= 1 (length (($iter-children cursor))))
              (or (= remaining 1)
                  (begin (iter-reset! cursor) (pass (- remaining 1))))))))
   (list (lambda () (range 1)) (lambda () '(0)) (lambda () (vector 0))
         (lambda () (eduction (tidentity) '(0))))))

(mat transducer-sequence-resource-capability
  ;; A sequence may acquire a nested resource even when its upstream list is free.
  (let ([closed 0])
    (let* ([inner (make-iter (lambda () iter-end) void
                             (lambda () (set! closed (+ closed 1))))]
           [nested (sequence (tcompose (tidentity) (tcat)) (list inner))]
           [cursor (sequence (tcat) (list nested))])
      (and (equal? '() (iter->list cursor))
           (= closed 1) (iter-finalized? nested)))))

(define-record-type review-late-sequence-source (fields cursor))

(mat transducer-sequence-late-resource-discovery
  (begin
    (iter-register-source! review-late-sequence-source? review-late-sequence-source-cursor)
    #t)

  ;; Both a supplied dynamic cursor and an unknown adapter can acquire a resource later.
  (andmap
   (lambda (source-wrapper)
     (let ([children '()] [closed 0])
       (let* ([resource (make-iter (lambda () iter-end) void
                                   (lambda () (set! closed (+ closed 1))))]
              [dynamic (mk-$iter (lambda () iter-end) void #f (lambda () children))]
              [nested (sequence (tidentity) (source-wrapper dynamic))]
              [xform (tcompose
                      (tmap (lambda (value)
                              (if (eq? value 'acquire)
                                  (begin (set! children (list resource)) '())
                                  value)))
                      (tcat))])
         (and (equal? '() (into 'list xform (list nested 'acquire)))
              (= closed 1) (iter-finalized? nested)))))
   (list (lambda (cursor) cursor) make-review-late-sequence-source)))


(define sequence-history-released?
  (lambda (stage expected-count consume)
    (let ([references '()])
      (let ([cursor
             (sequence
              (tcompose
               (tmap (lambda (value)
                       (let ([payload (make-vector 256 value)])
                         (set! references (cons (weak-cons payload #f) references))
                         payload)))
               stage
               (tmap (lambda (value) 0)))
              (range 10))])
        (let pass ([remaining 2])
          (and (= expected-count (length (consume cursor)))
               (begin
                 (collect (collect-maximum-generation))
                 (collect (collect-maximum-generation))
                 (andmap (lambda (reference) (bwp-object? (car reference))) references))
               (iter-end? (iter-next! cursor))
               (or (= remaining 1)
                   (begin (iter-reset! cursor) (pass (- remaining 1))))))))))

(mat transducer-completed-sequence-releases-history
  ;; Keep completed cursors live without keeping generated inputs in sources or outputs.
  (andmap
   (lambda (consume)
     (andmap
      (lambda (case) (sequence-history-released? (car case) (cdr case) consume))
      (list (cons (tdistinct) 10) (cons (tdedupe) 10)
            (cons (tpartition 100) 0) (cons (tpartition-all 100) 1)
            (cons (tcompose (tdistinct) (ttake 3)) 3))))
   (list transducer-drain iter->list))

  ;; An accumulator can retain memory even when the applied reducer has no history.
  (let ([reference #f])
    (let ([cursor
           (sequence
            (make-transducer
             'private-accumulator
             (lambda (downstream)
               (make-reducer
                'private-accumulator
                (case-lambda
                  [() (let ([payload (make-vector 256 'accumulator)])
                        (set! reference (weak-cons payload #f))
                        payload)]
                  [(acc) acc]
                  [(acc value) acc]))))
            '(1 2 3))])
      (and (equal? '() (transducer-drain cursor))
           (begin
             (collect (collect-maximum-generation))
             (collect (collect-maximum-generation))
             (bwp-object? (car reference)))
           (iter-end? (iter-next! cursor))
           (begin (iter-reset! cursor) (equal? '() (iter->list cursor)))))))

(mat transducer-completed-sequence-resumption
  (andmap
   (lambda (capture-in-source?)
     (let ([saved #f] [phase 0])
       (let* ([capture
               (lambda (value)
                 (if saved value
                     ;; Resume a callback after successful completion released reducer state.
                     (call/cc (lambda (resume) (set! saved resume) value))))]
              [cursor (sequence (if capture-in-source? (tidentity) (tmap capture))
                                (if capture-in-source?
                                    (iter-map capture (range 3))
                                    (range 3)))])
         (let ([value (iter-next! cursor)])
           (if (= phase 0)
               (begin (transducer-drain cursor) (set! phase 1) (saved 9))
               (and (if capture-in-source? (iter-end? value) (= value 9))
                    (iter-end? (iter-next! cursor))))))))
   '(#f #t)))

(mat transducer-sequence-resumed-construction-releases-state
  (andmap
   (lambda (capture-phase)
     (let ([saved #f] [phase 0] [reference #f])
       (define capture
         (lambda (where)
           (when (and (eq? where capture-phase) (not saved))
             ;; Revisit application, initialization, or completion after the pass has ended.
             (call/cc (lambda (resume) (set! saved resume))))))
       (define payload
         (lambda ()
           (let ([value (make-vector 256 'state)])
             (set! reference (weak-cons value #f))
             value)))
       (let ([cursor
              (sequence
               (make-transducer
                'capture-construction
                (lambda (downstream)
                  (capture 'apply)
                  (let ([state (payload)])
                    (make-reducer
                     'private-state
                     (case-lambda
                       [() (capture 'init)
                        (if (eq? capture-phase 'init) (payload) state)]
                       [(acc) (capture 'complete)
                        (if (eq? capture-phase 'complete) (payload) acc)]
                       [(acc value) state])))))
               '())])
         (let ([result (iter-next! cursor)])
           (if (= phase 0)
               (begin (set! phase 1) (saved (void)))
               (begin
                 ;; The saved continuation may retain state; discard it before checking.
                 (set! saved #f)
                 (collect (collect-maximum-generation))
                 (collect (collect-maximum-generation))
                 (and (iter-end? result) (bwp-object? (car reference))
                      (iter-end? (iter-next! cursor)))))))))
   '(apply init complete)))

(define-record-type review-resumable-adapter-source (fields convert))

(iter-register-source!
 review-resumable-adapter-source?
 (lambda (source) ((review-resumable-adapter-source-convert source))))

(mat transducer-sequence-retains-committed-adapter
  (andmap
   (lambda (completed?)
     (let ([saved #f] [phase 0] [remaining '(1 2 3 4)] [conversions 0]
           [closed (vector 0 0 0)] [resets (vector 0 0 0)] [late-pulls 0])
       (define late-source
         (lambda (slot)
           (make-iter
            (lambda () (set! late-pulls (+ late-pulls 1)) 99)
            (lambda () (vector-set! resets slot (+ 1 (vector-ref resets slot))))
            (lambda () (vector-set! closed slot (+ 1 (vector-ref closed slot)))))))
       (let* ([original
               (make-iter
                (lambda ()
                  (if (null? remaining) iter-end
                      (let ([value (car remaining)]) (set! remaining (cdr remaining)) value)))
                (lambda ()
                  (vector-set! resets 0 (+ 1 (vector-ref resets 0)))
                  (set! remaining '(1 2 3 4)))
                (lambda () (vector-set! closed 0 (+ 1 (vector-ref closed 0)))))]
              [first-late (late-source 1)] [second-late (late-source 2)]
              [cursor
               (sequence
                (tidentity)
                (make-review-resumable-adapter-source
                 (lambda ()
                   (set! conversions (+ conversions 1))
                   ;; Revisit deferred conversion after the same cursor has progressed.
                   (call/cc (lambda (resume) (set! saved resume) original)))))])
         (let ([value (iter-next! cursor)])
           (cond
            [(= phase 0)
             (and (= value 1) (= 2 (iter-next! cursor))
                  (or (not completed?) (equal? '(3 4) (transducer-drain cursor)))
                  (begin (set! phase 1) (saved first-late)))]
            [(= phase 1)
             (and (if completed? (iter-end? value) (= value 3))
                  (= late-pulls 0)
                  (begin (set! phase 2) (saved second-late)))]
            [else
             (let ([okay?
                    (and (if completed? (iter-end? value) (= value 4))
                         (= late-pulls 0) (iter-end? (iter-next! cursor))
                         (begin (iter-reset! cursor) #t)
                         (equal? resets '#(1 0 0))
                         (equal? '(1 2 3 4) (transducer-drain cursor))
                         (= conversions 1)
                         (equal? '() (iter->list cursor))
                         (equal? closed '#(1 0 0))
                         (begin (iter-finalize! cursor) #t)
                         (equal? closed '#(1 0 0))
                         (iter-finalized? original)
                         (not (iter-finalized? first-late))
                         (not (iter-finalized? second-late)))])
               ;; Ignored conversion results were supplied by the caller, who closes them.
               (iter-finalize! original)
               (iter-finalize! first-late)
               (iter-finalize! second-late)
               okay?)])))))
   '(#f #t)))

(mat transducer-sequence-adapter-resumption-drains-buffer-first
  (let ([saved #f] [phase 0] [pulls 0] [late-pulls 0])
    (let* ([source (iter-map (lambda (value) (set! pulls (+ pulls 1)) value) (range 1 4))]
           [late (make-iter (lambda () (set! late-pulls (+ late-pulls 1)) 99) void)]
           [cursor
            (sequence
             (tmapcat (lambda (value) (list value (* value 10))))
             (make-review-resumable-adapter-source
              (lambda ()
                ;; Revisit conversion while an earlier input's expansion is still buffered.
                (call/cc (lambda (resume) (set! saved resume) source)))))])
      (let ([value (iter-next! cursor)])
        (cond [(= phase 0) (and (= value 1) (begin (set! phase 1) (saved late)))]
              [(= phase 1) (and (= value 10) (= pulls 1)
                               (begin (set! phase 2) (saved late)))]
              [(= phase 2) (and (= value 2) (= pulls 2)
                               (begin (set! phase 3) (saved late)))]
              [else (and (= value 20) (= pulls 2) (= late-pulls 0)
                         (equal? '(3 30) (iter->list cursor)) (= pulls 3))])))))

(mat transducer-sequence-yielding-adapter-construction
  (andmap
   (lambda (workers)
     (run-fibers
      (lambda ()
        (let ([done (make-channel)] [xform (tcompose (tmap add1) (ttake 3))])
          (do ([task 0 (fx1+ task)]) ((fx= task 16))
            (spawn-fiber
             (lambda ()
               (let ([conversions 0] [closed 0] [resets 0] [position 0])
                 (let ([cursor
                        (sequence
                         xform
                         (make-review-resumable-adapter-source
                          (lambda ()
                            (set! conversions (+ conversions 1))
                            (fiber-yield)
                            (make-iter
                             (lambda ()
                               (fiber-yield)
                               (let ([value position]) (set! position (+ position 1)) value))
                             (lambda () (set! position 0) (set! resets (+ resets 1)))
                             (lambda () (set! closed (+ closed 1)))))))])
                   (channel-put!
                    done
                    (and (equal? '(1 2 3) (transducer-drain cursor)) (= closed 0)
                         (begin (iter-reset! cursor) #t)
                         (equal? '(1 2 3) (iter->list cursor))
                         (= conversions 1) (= resets 1) (= closed 1))))))))
          (let loop ([remaining 16] [okay? #t])
            (if (fxzero? remaining) okay?
                (let ([result (channel-get done)])
                  (loop (fx1- remaining) (and okay? result)))))))
      workers 100))
   '(1 4)))

(mat transducer-numeric-type-safety
  ;; Specialized numeric reducers reject elements with the wrong representation.
  (andmap
   (lambda (value)
     (transducer-type-error?
      (lambda () (transduce (tidentity) (rffxsum) (list value)))))
   (list #f 'bad "bad" (cons 1 2) 1.0 (+ 1 (most-positive-fixnum))))

  ;; Exact numbers are not valid flonum inputs.
  (andmap
   (lambda (value)
     (transducer-type-error?
      (lambda () (transduce (tidentity) (rfflsum) (list value)))))
   (list #f 'bad "bad" (cons 1 2) 1 1/2 1+2i))

  ;; Invalid numeric seeds must also be rejected when no step runs.
  (andmap
   (lambda (make-reducer)
     (andmap
      (lambda (seed)
        (transducer-type-error?
         (lambda () (transduce (tidentity) (make-reducer) seed '()))))
      (list #f 'bad "bad" (cons 1 2))))
   (list rfcount rfsum rfproduct rffxsum rfflsum))

  ;; The first transduce1 value is still subject to the reducer's accumulator type.
  (and (transducer-type-error? (lambda () (transduce1 (tidentity) (rffxsum) '(1.0))))
       (transducer-type-error? (lambda () (transduce1 (tidentity) (rfflsum) '(1)))))

  ;; Fixnum inputs and seeds do not guarantee that their sum is still a fixnum.
  (transducer-type-error?
   (lambda () (transduce (tidentity) (rffxsum) (most-positive-fixnum) '(1)))))

(mat transducer-collection-seed-type-safety
  ;; Completed containers, scalars, and improper lists are not collection accumulators.
  (andmap
   (lambda (make-reducer)
     (andmap
      (lambda (seed)
        (andmap
         (lambda (source)
           (transducer-type-error?
            (lambda () (transduce (tidentity) (make-reducer) seed source))))
         '(() (1))))
      (list 17 #f (vector 1) "bad" (cons 1 17))))
   (list rflist rfreverselist rfvector rfstring rfbytevector))

  ;; An opaque builder accumulator belongs to the reducer that initialized it.
  (let* ([first (rfvector)] [second (rfvector)] [seed ((completion first))])
    (transducer-type-error?
     (lambda () (transduce (tidentity) second seed '()))))

  (let* ([reducer (rfvector)] [seed ((completion reducer))])
    (equal? '#(1 2) (transduce (tidentity) reducer seed '(1 2)))))
