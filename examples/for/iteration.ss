(import (cspp) (cspp iter))
(load "examples/common.ss")

(example-run!
 'for-example
 "Explore parallel and nested loops, source types, filtering, folds, and early exit."
 '((limit . "Maximum squares to collect, a nonnegative fixnum (default: 4)."))
 (lambda (ref)
   (let ([limit (example-option-natural 'for-example ref 'limit 4)])
     (unless (fixnum? limit)
       (errorf 'for-example "--limit must fit in a fixnum"))

;;;;===----------------------------------------------------------------------===
;;;; Simple for loops
;;;;===----------------------------------------------------------------------===

     ;; A literal bound visits zero through bound minus one. Print once per iteration.
     (for ([number 3])
       (example-print (list 'simple-range number)))

     ;; Use :list when the source is a list of values.
     (for ([name :list '(Ada Grace)])
       (example-print (list 'simple-list name)))

     ;; A string source visits its characters in order.
     (for ([letter "ABC"])
       (example-print (list 'simple-string letter)))

     ;; Multiple sources advance together and stop when the shortest source ends.
     (for ([name :list '(Ada Grace Linus)] [score '#(10 20)])
       (example-print (list 'simple-pairs name score)))

;;;;===----------------------------------------------------------------------===
;;;; Source types, guards, indexes, and collection
;;;;===----------------------------------------------------------------------===

     ;; Parallel iteration pairs sources and stops when the shortest source ends.
     ;; String and quoted-vector literals need no explicit source type.
     (example-print
      (list 'parallel
            (for/list ([letter "ABC"] [score '#(10 20)])
              (list letter score))))

     ;; A source guard filters before pairing. A clause guard filters the resulting pairs.
     (example-print
      (list 'source-guard
            (for/list ([score :list '(1 2 3 4) :guard (even? score)]
                       [name :list '(Ada Grace)])
              (list name score))))
     (example-print
      (list 'clause-guard
            (for/list ([score :list '(1 2 3 4)] [name :list '(Ada Grace)]
                       [:guard (even? score)])
              (list name score))))

     ;; Source indexes retain original positions; the global index counts body evaluations.
     ;; An indexed guard counts its passes, even when a later guard rejects the item.
     (example-print
      (list 'indexes-and-bindings
            (for/list ([:index visit]
                       [score :vector '#(5 10 15 20 25 30) :index position
                              :guard (odd? position)]
                       [:let doubled (* score 2)]
                       [:guard (> doubled 20) :index accepted]
                       [:guard (> doubled 40)])
              (list position visit accepted doubled))))

     ;; Multiple-value bindings can unpack a procedure result for later clauses and the body.
     (example-print
      (list 'multiple-value-bindings
            (for/list ([value :list '(7 8 9)]
                       [:let-values (quotient remainder) (div-and-mod value 3)]
                       [:guard (zero? remainder)])
              (list value quotient remainder))))

     ;; Sequence slicing uses exclusive bounds. :rev reverses the selected traversal.
     (example-print
      (list 'slice
            (for/list ([value :vector '#(0 10 20 30 40 50)
                              :from 1 :to 6 :step 2 :rev])
              value)))

     ;; :nums accepts general real ranges; :fixnums uses fixnum arithmetic.
     (example-print
      (list 'numeric-ranges
            (for/list ([value :nums 0 1 1/4]) value)
            (for/list ([value :fixnums 5 0 -2]) value)))

     ;; :iota uses exact integers; :fxiota uses the smaller fixnum domain.
     ;; Explicit string and list sources support the same slicing options as vectors.
     (example-print
      (list 'more-sequence-types
            (for/list ([value :iota 4]) value)
            (for/list ([letter :string "iteration" :from 1 :to 8 :step 2]) letter)
            (for/list ([value :list '(0 10 20 30 40) :from 1 :to 5 :step 2 :rev])
              value)))

     ;; :length bounds collection after filtering; zero produces an empty list.
     (example-print
      (list 'bounded-squares
            (for/list ([:length limit] [value :fxiota 10] [:guard (odd? value)])
              (fx* value value))))

     ;; An explicit vector length preallocates storage; unvisited slots keep :fill.
     (example-print
      (list 'preallocated-vector
            (for/vector ([:length 5] [:fill 'unused] [value :fxiota 3])
              (fx* value value))))

     ;; Typed vectors use the same collection forms as ordinary vectors.
     (example-print
      (list 'typed-vectors
            (for/vector ([integer :fxvector (fxvector 1 2 3)]
                         [real :flvector (flvector 0.5 1.5 2.5)])
              (+ integer real))))

     ;; Typed bytevector traversal reads complete u16 values and exposes byte offsets.
     (example-print
      (list 'bytevector-u16
            (for/list ([word :bytevector #vu8(0 1 0 2 0 3) :type u16 :big :index offset])
              (list offset word))))

     ;; The same bytes have different signed and unsigned interpretations.
     ;; Reversed multi-byte traversal preserves the complete chunks and their byte offsets.
     (example-print
      (list 'bytevector-signed-and-reversed
            (for/list ([value :bytevector #vu8(0 127 128 255) :type u8]) value)
            (for/list ([value :bytevector #vu8(0 127 128 255) :type s8]) value)
            (for/list ([value :bytevector #vu8(0 1 0 2 0 3) :type u16 :big :rev
                             :index offset])
              (list offset value))))
     ;; Floating-point bytevector sources decode IEEE values using the chosen endianness.
     (let ([bytes (make-bytevector 16 0)])
       (bytevector-ieee-double-set! bytes 0 1.5 (endianness big))
       (bytevector-ieee-double-set! bytes 8 2.5 (endianness big))
       (example-print
        (list 'bytevector-double
              (for/list ([value :bytevector bytes :type double :big]) value))))

     ;; Hashtable iteration binds both key and value. Sort for deterministic display;
     ;; hashtable iteration itself does not promise an order.
     (let ([scores (make-eq-hashtable)])
       (hashtable-set! scores 'Ada 10)
       (hashtable-set! scores 'Grace 20)
       (example-print
        (list 'hashtable
              (sort (lambda (left right) (string<? (symbol->string (car left))
                                                 (symbol->string (car right))))
                    (for/list ([(name score) :hashtable scores])
                      (cons name score)))))
       ;; Key-only and value-only sources avoid unpacking an unused companion value.
       (example-print
        (list 'hashtable-keys-and-values
              (sort (lambda (left right) (string<? (symbol->string left)
                                                   (symbol->string right)))
                    (for/list ([name :hashtable-keys scores]) name))
              (sort < (for/list ([score :hashtable-values scores]) score)))))

;;;;===----------------------------------------------------------------------===
;;;; Numeric, boolean, record, vector, and datatype-driven folds
;;;;===----------------------------------------------------------------------===

     ;; for is useful for effects; for/fold updates an accumulator with each body result.
     (let ([squares (make-vector 3)])
       (for ([index :fxiota 3])
         (vector-set! squares index (fx* index index)))
       (example-print (list 'for-effects squares)))
     (example-print
      (list 'fold
            (for/fold ([:init total 0] [:finish (list 'sum total)]
                       [value :list '(10 20 30)])
              (+ total value))))

     ;; The seed defines the reduction: multiplication starts at one, addition at zero.
     ;; Parallel sources make a dot product, and flonum sources can use flonum arithmetic.
     (example-print
      (list 'numeric-folds
            (for/fold ([:init product 1] [:finish product] [value :fixnums 1 5])
              (fx* product value))
            (for/fold ([:init total 0.0] [:finish total]
                       [value :flvector (flvector 0.5 1.5 2.5)])
              (fl+ total value))
            (for/fold ([:init dot 0] [:finish dot]
                       [left :fxvector (fxvector 1 2 3)]
                       [right :fxvector (fxvector 4 5 6)])
              (fx+ dot (fx* left right)))))

     ;; for/fold accumulates in traversal order. Reversing the source and flipping the
     ;; operation's arguments expresses a right fold; subtraction makes the difference visible.
     (example-print
      (list 'left-and-right-folds
            (for/fold ([:init result 0] [:finish result] [value :list '(10 1 2)])
              (- result value))
            (for/fold ([:init result 0] [:finish result] [value :list '(10 1 2) :rev])
              (- value result))))

     ;; An accumulator can be a record carrying several statistics, not just a number.
     ;; :finish converts the final record to a readable summary after normal exhaustion.
     (let ()
       (record Statistics (count total maximum))
       (example-print
        (list 'record-fold
              (for/fold ([:init stats (Statistics 0 0 #f)]
                         [:finish (list 'count (Statistics-count stats)
                                        'mean (/ (Statistics-total stats)
                                                 (Statistics-count stats))
                                        'maximum (Statistics-maximum stats))]
                         [value :list '(10 20 30)])
                (Statistics (fx1+ (Statistics-count stats))
                            (+ (Statistics-total stats) value)
                            (max (or (Statistics-maximum stats) value) value))))))

     ;; A mutable vector is another accumulator option. Each body updates and returns it;
     ;; the finalizer converts its bucket counts to a list after traversal.
     (example-print
      (list 'vector-histogram-fold
            (for/fold ([:init counts (make-fxvector 3 0)]
                       [:finish (fxvector->list counts)]
                       [bucket :list '(0 1 0 2 1 0)])
              (fxvector-set! counts bucket (fx1+ (fxvector-ref counts bucket)))
              counts)))

     ;; Datatype values can be ordinary sequence items. Match each action in the fold body.
     ;; Reset is a singleton value; Add and Scale are constructors with checked fields.
     (let ()
       (datatype Action
         [Add (amount :predicate number?)]
         [Scale (factor :predicate number?)]
         [Reset])
       (example-print
        (list 'datatype-fold
              (for/fold ([:init balance 0] [:finish balance]
                         [action :list (list (Add 3) (Scale 2) Reset (Add 5))])
                (match action
                  [,(datatype Action Add :fields (amount ,amount)) (+ balance amount)]
                  [,(datatype Action Scale :values ,factor) (* balance factor)]
                  [,(datatype Action Reset) 0])))))

     ;; No body runs for an empty source: the seed and the zero global index reach :finish.
     (example-print
      (list 'empty-fold
            (for/fold ([:init total 100] [:index count] [:finish (list total count)]
                       [value :list '()])
              (+ total value))))

     ;; A boolean fold can short-circuit with :break. An empty input would retain #t.
     (example-print
      (list 'all-positive-fold
            (for/fold ([:init all? #t] [:finish all?]
                       [value :list '(3 2 -1 9)] [:break (not (positive? value)) #f])
              (and all? (positive? value)))))

;;;;===----------------------------------------------------------------------===
;;;; Nested traversal and the scope of indexes
;;;;===----------------------------------------------------------------------===

     ;; for* nests sources. Each inner source can depend on its current outer item.
     (example-print
      (list 'dependent-nesting
            (for*/list ([row :fixnums 1 4] [column :fxiota row])
              (list row column))))
     (example-print
      (list 'nested-vector
            (for*/vector ([:length 4] [row 2] [column 2])
              (list row column))))
     (example-print
      (list 'nested-fold
            (for*/fold ([:init total 0] [:finish total] [row 3] [column 2])
              (+ total (* row column)))))

     ;; A leading index counts body evaluations across every level. Standalone indexes
     ;; after sources are local counters: they restart each time that level is entered.
     ;; The row counter counts accepted rows; the source indexes keep original positions.
     (example-print
      (list 'nested-indexes
            (for*/list ([:index visit]
                        [row :vector '#(skip Ada skip Grace) :index row-position]
                        [:guard (not (eq? row 'skip))]
                        [:index row-number]
                        [score :list '(10 20 30) :index score-position]
                        [:guard (> score 10)]
                        [:index column-number])
              (list visit row-position row-number score-position column-number row score))))

;;;;===----------------------------------------------------------------------===
;;;; Break, stop, explicit exit results, and accumulator replacement
;;;;===----------------------------------------------------------------------===

     ;; :break exits only the current nested level; :stop exits every level.
     (example-print
      (list 'break-inner
            (for*/list ([row 3] [column 3] [:break (= column 1)])
              (list row column))))
     (example-print
      (list 'stop-all
            (for*/list ([row 3] [column 3] [:stop (= column 1)])
              (list row column))))
     (example-print
      (list 'search
            (for* ([row 3] [column 3]
                   [:stop (= (+ row column) 3) (list row column)])
              (void))))

     ;; A parallel :break with an explicit result returns immediately and skips :finish.
     ;; Here it preserves the partial accumulator instead of using the exhaustion result.
     (example-print
      (list 'fold-break-result
            (for/fold ([:init total 0] [:finish (list 'exhausted total)]
                       [value :list '(10 20 30 40)]
                       [:break (> (+ total value) 35) (list 'threshold total value)])
              (+ total value))))

     ;; An inner :break without a result preserves the accumulator and advances the outer loop.
     ;; With a result, it replaces the accumulator before that outer loop resumes.
     (example-print
      (list 'nested-fold-breaks
            (for*/fold ([:init total 0] [:finish total] [row 2] [column 3]
                        [:break (= column 1)])
              (+ total 1))
            (for*/fold ([:init total 0] [:finish total] [row 2] [column 3]
                        [:break (= column 1) (+ total 10)])
              (+ total 1))))

     ;; An outer-level :break ends the whole traversal. An inner :stop also leaves all
     ;; levels. Both explicit results bypass :finish, which normally labels exhaustion.
     (example-print
      (list 'outer-break-result
            (for*/fold ([:init total 0] [:finish (list 'exhausted total)]
                        [row 3] [:break (= row 1) (list 'outer-break total)]
                        [column 3])
              (+ total 1))))
     (example-print
      (list 'fold-stop-result
            (for*/fold ([:init total 0] [:index visit]
                        [:finish (list 'exhausted total)]
                        [row 3] [column 3]
                        [:stop (= (+ row column) 3) (list 'stopped total visit row column)])
              (+ total 1))))

;;;;===----------------------------------------------------------------------===
;;;; Iterator ownership and cleanup
;;;;===----------------------------------------------------------------------===

     ;; :iter consumes an existing iterator. The caller finalizes it even on early exit.
     (let ([source (list->iter '(10 20 30))])
       (dynamic-wind
         void
         (lambda ()
           (example-print
            (list 'iterator
                  (for/list ([:length 2] [value :iter source]) value))))
         (lambda () (iter-finalize! source)))))
   0)
 (command-line-arguments))
