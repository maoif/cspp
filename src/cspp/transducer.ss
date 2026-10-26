(library (cspp transducer)
  (export transducer? reducer? reduced reduced? unreduced ensure-reduced
          preserving-reduced completion

          make-transducer transducer-name tidentity tcompose tchain

          tmap tmap/i tfilter tfilter/i tremove tkeep tkeep/i treplace
          tcat tmapcat tflatten

          ttake tdrop ttake-while tdrop-while ttake-nth tslice
          tpartition tpartition-all tpartition-by
          tdedupe tdedupe-by tdistinct tdistinct-by tinterpose

          ttap tinspect

          transduce transduce1 into sequence tfor-each
          list-transduce vector-transduce bytevector-transduce
          string-transduce iter-transduce fxvector-transduce
          flvector-transduce hashtable-transduce
          port-lines-transduce port-bytes-transduce

          make-reducer reducer-name
          rflist rfreverselist rfvector rfstring rfbytevector
          rfcount rfsum rffxsum rfflsum rfproduct
          rfmin rfmax rfany rfevery

          eduction eduction? transducible? source->iter
          current-transducer-source-mode)
  (import (cspp chez)
          (cspp internal)
          (cspp iter)
          (cspp list)
          (cspp utils))

  (define-record-type ($reduced mk-$reduced $reduced?)
    (fields (immutable value))
    (opaque #t)
    (sealed #t))

  (define-record-type ($reducer mk-$reducer $reducer?)
    (fields (immutable name)
            (immutable proc)
            (immutable metadata))
    (opaque #t)
    (sealed #t))

  (define-record-type ($transducer mk-$transducer $transducer?)
    (fields (immutable name)
            (immutable reducer-proc)
            (immutable metadata))
    (opaque #t)
    (sealed #t))

  (define-record-type ($eduction mk-$eduction $eduction?)
    (fields (immutable xform)
            (immutable source))
    (opaque #t)
    (sealed #t))

  #|proc:transducer?
  The `transducer?` procedure returns `#t` when `x` is a transducer record.
  |#
  (define transducer? $transducer?)

  #|proc:reducer?
  The `reducer?` procedure returns `#t` when `x` is a reducer record.
  |#
  (define reducer? $reducer?)

  #|proc:reduced?
  The `reduced?` procedure returns `#t` when `x` is a reduced wrapper.
  |#
  (define reduced? $reduced?)

  #|proc:eduction?
  The `eduction?` procedure returns `#t` when `x` is an eduction record.
  |#
  (define eduction? $eduction?)

  (define all-transducers?
    (lambda (x*)
      (and (list? x*) (andmap transducer? x*))))

  (define byte?
    (lambda (x)
      (and (fixnum? x) (fx<= 0 x) (fx<= x 255))))

  (define alist?
    (lambda (x)
      (and (list? x) (andmap pair? x))))

  (define transducer-input-textual-port?
    (lambda (x)
      (and (input-port? x) (textual-port? x) (not (port-closed? x)))))

  (define transducer-input-binary-port?
    (lambda (x)
      (and (input-port? x) (binary-port? x) (not (port-closed? x)))))

  #|proc:current-transducer-source-mode
  The `current-transducer-source-mode` parameter controls generic source
  traversal. Supported values are `'direct` and `'iter`.
  |#
  (define current-transducer-source-mode
    (make-parameter
     'direct
     (lambda (mode)
       (case mode
         [(direct iter) mode]
         [else (errorf 'current-transducer-source-mode
                       "unsupported source mode: ~a"
                       mode)]))))

  (define source-end? reduced?)
  (define default-init (gensym))

  (define reducer-init
    (lambda (reducer)
      (($reducer-proc reducer))))

  (define reducer-complete
    (lambda (reducer acc)
      (($reducer-proc reducer) acc)))

  (define reducer-step
    (lambda (reducer acc x)
      (($reducer-proc reducer) acc x)))

  (define apply-transducer
    (lambda (xform reducer)
      (let ([next (($transducer-reducer-proc xform) reducer)])
        (if (reducer? next)
            next
            (errorf 'transduce "transducer ~a returned a non-reducer: ~a"
                    ($transducer-name xform)
                    next)))))

  (define run-list
    (lambda (reducer acc ls)
      (let loop ([acc acc] [ls ls])
        (cond [(source-end? acc) acc]
              [(null? ls) acc]
              [else (loop (reducer-step reducer acc (car ls)) (cdr ls))]))))

  (define run-vector
    (lambda (reducer acc vec)
      (let ([len (vector-length vec)])
        (let loop ([acc acc] [i 0])
          (cond [(source-end? acc) acc]
                [(fx= i len) acc]
                [else (loop (reducer-step reducer acc (vector-ref vec i))
                            (fx+ i 1))])))))

  (define run-bytevector
    (lambda (reducer acc bv)
      (let ([len (bytevector-length bv)])
        (let loop ([acc acc] [i 0])
          (cond [(source-end? acc) acc]
                [(fx= i len) acc]
                [else (loop (reducer-step reducer acc (bytevector-u8-ref bv i))
                            (fx+ i 1))])))))

  (define run-string
    (lambda (reducer acc str)
      (let ([len (string-length str)])
        (let loop ([acc acc] [i 0])
          (cond [(source-end? acc) acc]
                [(fx= i len) acc]
                [else (loop (reducer-step reducer acc (string-ref str i))
                            (fx+ i 1))])))))

  (define run-fxvector
    (lambda (reducer acc vec)
      (let ([len (fxvector-length vec)])
        (let loop ([acc acc] [i 0])
          (cond [(source-end? acc) acc]
                [(fx= i len) acc]
                [else (loop (reducer-step reducer acc (fxvector-ref vec i))
                            (fx+ i 1))])))))

  (define run-flvector
    (lambda (reducer acc vec)
      (let ([len (flvector-length vec)])
        (let loop ([acc acc] [i 0])
          (cond [(source-end? acc) acc]
                [(fx= i len) acc]
                [else (loop (reducer-step reducer acc (flvector-ref vec i))
                            (fx+ i 1))])))))

  (define run-hashtable
    (lambda (reducer acc ht)
      (run-vector reducer acc (hashtable-values ht))))

  (define run-iter
    (lambda (reducer acc iter)
      (let loop ([acc acc])
        (if (source-end? acc)
            acc
            (let ([x (iter-next! iter)])
              (if (iter-end? x)
                  acc
                  (loop (reducer-step reducer acc x))))))))

  ;; Generated source iterators belong to this traversal; caller-supplied iterators do not.
  (define run-owned-iter
    (lambda (reducer acc iterator)
      (dynamic-wind
        void
        (lambda () (run-iter reducer acc iterator))
        (lambda ()
          (unless (iter-finalized? iterator)
            (iter-finalize! iterator))))))

  (define run-port-lines
    (lambda (reducer acc port)
      (let loop ([acc acc])
        (if (source-end? acc)
            acc
            (let ([x (get-line port)])
              (if (eof-object? x)
                  acc
                  (loop (reducer-step reducer acc x))))))))

  (define run-port-bytes
    (lambda (reducer acc port)
      (let loop ([acc acc])
        (if (source-end? acc)
            acc
            (let ([x (get-u8 port)])
              (if (eof-object? x)
                  acc
                  (loop (reducer-step reducer acc x))))))))

  (define run-source
    (lambda (reducer acc source)
      (if (and (eq? (current-transducer-source-mode) 'iter)
               (not (iter? source)))
          (run-owned-iter reducer acc (source->iter source))
          (cond [(list? source) (run-list reducer acc source)]
                [(vector? source) (run-vector reducer acc source)]
                [(bytevector? source) (run-bytevector reducer acc source)]
                [(string? source) (run-string reducer acc source)]
                [(fxvector? source) (run-fxvector reducer acc source)]
                [(flvector? source) (run-flvector reducer acc source)]
                [(hashtable? source) (run-hashtable reducer acc source)]
                [(iter? source) (run-iter reducer acc source)]
                [else (run-owned-iter reducer acc (iter-source->iter source))]))))

  (define transduce-run
    (lambda (who xform reducer init source runner)
      (pcheck ([transducer? xform] [reducer? reducer])
              (let ([effective (apply-transducer xform reducer)])
                (finish-transduce effective
                                  (runner effective
                                          (if (eq? init default-init)
                                              (reducer-init effective)
                                              init)
                                          source))))))

  (define finish-transduce
    (lambda (reducer acc)
      (reducer-complete reducer (unreduced acc))))

  (define transduce-explicit
    (lambda (xform reducer init source)
      (pcheck ([transducer? xform] [reducer? reducer] [transducible? source])
              (cond [($eduction? source)
                     (let ([combined (tcompose ($eduction-xform source) xform)])
                       (transduce-explicit combined reducer init ($eduction-source source)))]
                    [else
                     (let ([effective (apply-transducer xform reducer)])
                       (finish-transduce effective
                                         (run-source effective
                                                     (if (eq? init default-init)
                                                         (reducer-init effective)
                                                         init)
                                                     source)))]))))

  (define transduce-default
    (lambda (xform reducer source)
      (pcheck ([transducer? xform] [reducer? reducer])
              (transduce-explicit xform reducer default-init source))))

  (define list->bytevector
    (lambda (ls)
      (let* ([len (length ls)]
             [bv (make-bytevector len)])
        (let loop ([ls ls] [i 0])
          (if (null? ls)
              bv
              (let ([x (car ls)])
                (if (byte? x)
                    (begin
                      (bytevector-u8-set! bv i x)
                      (loop (cdr ls) (fx+ i 1)))
                    (errorf 'rfbytevector "expected byte value, got ~a" x))))))))

  (define make-list-like-reducer
    (lambda (name complete)
      (mk-$reducer
       name
       (case-lambda
         [() '()]
         [(acc) (complete acc)]
         [(acc x) (cons x acc)])
       #f)))

  (define make-list-builder-reducer
    (lambda (name complete)
      (let ([tag (gensym)])
        (define builder-acc?
          (lambda (acc)
            (and (pair? acc) (eq? (car acc) tag))))
        (mk-$reducer
         name
         (case-lambda
           [() (cons tag (make-list-builder))]
           [(acc)
            (complete
             (if (builder-acc? acc)
                 ((cdr acc))
                 (reverse acc)))]
           [(acc x)
            (if (builder-acc? acc)
                (begin
                  ((cdr acc) x)
                  acc)
                (cons x acc))])
         #f))))

  #|proc:reduced
  The `reduced` procedure wraps `x` to request early termination from a
  transducer driver.
  |#
  (define reduced
    (lambda (x)
      (mk-$reduced x)))

  #|proc:unreduced
  The `unreduced` procedure returns the wrapped value of a reduced result, or
  `x` unchanged when `x` is not reduced.
  |#
  (define unreduced
    (lambda (x)
      (if (reduced? x) ($reduced-value x) x)))

  #|proc:ensure-reduced
  The `ensure-reduced` procedure returns `x` unchanged when it is already a
  reduced value, otherwise it wraps `x` with `reduced`.
  |#
  (define ensure-reduced
    (lambda (x)
      (if (reduced? x) x (reduced x))))

  #|proc:preserving-reduced
  The `preserving-reduced` procedure returns a reducer that forwards to
  `reducer` while leaving an incoming reduced accumulator untouched.
  |#
  (define preserving-reduced
    (lambda (reducer)
      (pcheck ([reducer? reducer])
              (mk-$reducer
               ($reducer-name reducer)
               (case-lambda
                 [() (reducer-init reducer)]
                 [(acc) (reducer-complete reducer acc)]
                 [(acc x)
                  (if (reduced? acc)
                      acc
                      (reducer-step reducer acc x))])
               #f))))

  #|proc:completion
  The `completion` procedure returns the completing reducing procedure stored by reducer record
  `reducer`. Its signatures are `() -> initial-accumulator`, `(accumulator) -> completed-result`,
  and `(accumulator value) -> next-accumulator-or-reduced`.
  |#
  (define completion
    (lambda (reducer)
      (pcheck ([reducer? reducer])
              ($reducer-proc reducer))))

  #|proc:make-transducer
  The `make-transducer` procedure returns a transducer record named `name`.
  `proc` must accept a reducer and return a reducer. Unseeded terminals initialize that
  transformed reducer; explicit seeds bypass its initializer.
  |#
  (define make-transducer
    (lambda (name proc)
      (pcheck ([symbol? name] [procedure? proc])
              (mk-$transducer name proc #f))))

  #|proc:transducer-name
  The `transducer-name` procedure returns the symbolic name of `xform`.
  |#
  (define transducer-name
    (lambda (xform)
      (pcheck ([transducer? xform])
              ($transducer-name xform))))

  #|proc:tidentity
  The `tidentity` procedure returns a transducer that leaves a reducer
  unchanged.
  |#
  (define tidentity
    (lambda ()
      (make-transducer 'tidentity (lambda (reducer) reducer))))

  #|proc:tcompose
  The `tcompose` procedure composes transducers in left-to-right pipeline
  order.
  |#
  (define tcompose
    (lambda xform*
      (pcheck ([all-transducers? xform*])
              (if (null? xform*)
                  (tidentity)
                  (make-transducer
                   'tcompose
                   (lambda (reducer)
                     (let loop ([xform* xform*])
                       (if (null? xform*)
                           reducer
                           (apply-transducer (car xform*)
                                             (loop (cdr xform*)))))))))))

  #|proc:tchain
  The `tchain` procedure composes the transducers in `xform-list` in
  left-to-right pipeline order.
  |#
  (define tchain
    (lambda (xform-list)
      (pcheck ([all-transducers? xform-list])
              (apply tcompose xform-list))))

  #|proc:tmap
  The `tmap` procedure returns a transducer that emits `(proc x)` for each
  input value `x`.
  |#
  (define tmap
    (lambda (proc)
      (pcheck ([procedure? proc])
              (make-transducer
               'tmap
               (lambda (reducer)
                 (mk-$reducer
                  'tmap
                  (case-lambda
                    [() (reducer-init reducer)]
                    [(acc) (reducer-complete reducer acc)]
                    [(acc x) (reducer-step reducer acc (proc x))])
                  #f))))))

  #|proc:tmap/i
  The `tmap/i` procedure returns a transducer that emits `(proc i x)` for each
  input value `x`, where `i` is the zero-based input index.
  |#
  (define tmap/i
    (lambda (proc)
      (pcheck ([procedure? proc])
              (make-transducer
               'tmap/i
               (lambda (reducer)
                 (let ([i 0])
                   (mk-$reducer
                    'tmap/i
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc) (reducer-complete reducer acc)]
                      [(acc x)
                       (let ([j i])
                         (set! i (+ i 1))
                         (reducer-step reducer acc (proc j x)))])
                    #f)))))))

  #|proc:tfilter
  The `tfilter` procedure returns a transducer that emits only values accepted
  by `pred`.
  |#
  (define tfilter
    (lambda (pred)
      (pcheck ([procedure? pred])
              (make-transducer
               'tfilter
               (lambda (reducer)
                 (mk-$reducer
                  'tfilter
                  (case-lambda
                    [() (reducer-init reducer)]
                    [(acc) (reducer-complete reducer acc)]
                    [(acc x) (if (pred x)
                                 (reducer-step reducer acc x)
                                 acc)])
                  #f))))))

  #|proc:tfilter/i
  The `tfilter/i` procedure returns a transducer that emits values accepted by
  `(pred i x)`, where `i` is the zero-based input index.
  |#
  (define tfilter/i
    (lambda (pred)
      (pcheck ([procedure? pred])
              (make-transducer
               'tfilter/i
               (lambda (reducer)
                 (let ([i 0])
                   (mk-$reducer
                    'tfilter/i
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc) (reducer-complete reducer acc)]
                      [(acc x)
                       (let ([j i])
                         (set! i (+ i 1))
                         (if (pred j x)
                             (reducer-step reducer acc x)
                             acc))])
                    #f)))))))

  #|proc:tremove
  The `tremove` procedure returns a transducer that emits only values rejected
  by `pred`.
  |#
  (define tremove
    (lambda (pred)
      (pcheck ([procedure? pred])
              (tfilter (lambda (x) (not (pred x)))))))

  #|proc:tkeep
  The `tkeep` procedure emits `(proc x)` for each input value whose result is
  not `#f`.
  |#
  (define tkeep
    (lambda (proc)
      (pcheck ([procedure? proc])
              (make-transducer
               'tkeep
               (lambda (reducer)
                 (mk-$reducer
                  'tkeep
                  (case-lambda
                    [() (reducer-init reducer)]
                    [(acc) (reducer-complete reducer acc)]
                    [(acc x)
                     (let ([y (proc x)])
                       (if y (reducer-step reducer acc y) acc))])
                  #f))))))

  #|proc:tkeep/i
  The `tkeep/i` procedure applies `(proc i x)` to each input and emits the
  result only when it is not `#f`.
  |#
  (define tkeep/i
    (lambda (proc)
      (pcheck ([procedure? proc])
              (make-transducer
               'tkeep/i
               (lambda (reducer)
                 (let ([i 0])
                   (mk-$reducer
                    'tkeep/i
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc) (reducer-complete reducer acc)]
                      [(acc x)
                       (let* ([j i]
                              [y (begin (set! i (+ i 1)) (proc j x))])
                         (if y (reducer-step reducer acc y) acc))])
                    #f)))))))

  #|proc:treplace
  The `treplace` procedure returns a transducer that replaces input values
  according to association list `alist` using `equal?`.
  |#
  (define treplace
    (lambda (alist)
      (pcheck ([alist? alist])
              (tmap (lambda (x)
                      (let ([cell (assoc x alist)])
                        (if cell (cdr cell) x)))))))

  ;; An eduction owns its transducer state, but must neither initialize nor
  ;; complete the surrounding reduction. Remember downstream termination separately
  ;; from an inner eduction's own early termination.
  (define emit-eduction
    (lambda (reducer acc source)
      (let ([stopped? #f])
        (let ([result
               (transduce
                ($eduction-xform source)
                (make-reducer
                 'tcat
                 (case-lambda
                   [() acc]
                   [(result) result]
                   [(result value)
                    (if stopped?
                        (ensure-reduced result)
                        (let ([next (reducer-step reducer result value)])
                          (when (reduced? next) (set! stopped? #t))
                          next))]))
                ($eduction-source source))])
          (if stopped? (ensure-reduced result) result)))))

  (define flatten-emit
    (lambda (reducer acc value)
      (cond [(source-end? acc) acc]
            [(eduction? value) (emit-eduction reducer acc value)]
            [(transducible? value) (run-source reducer acc value)]
            [else (reducer-step reducer acc value)])))

  (define flatten-rec
    (lambda (reducer acc x)
      (cond [(source-end? acc) acc]
            [(list? x)
             (let loop ([acc acc] [ls x])
               (cond [(source-end? acc) acc]
                     [(null? ls) acc]
                     [else (loop (flatten-rec reducer acc (car ls)) (cdr ls))]))]
            [(vector? x)
             (let ([len (vector-length x)])
               (let loop ([acc acc] [i 0])
                 (cond [(source-end? acc) acc]
                       [(fx= i len) acc]
                       [else (loop (flatten-rec reducer acc (vector-ref x i))
                                   (fx+ i 1))])))]
            [(bytevector? x) (run-bytevector reducer acc x)]
            [(string? x) (run-string reducer acc x)]
            [(fxvector? x) (run-fxvector reducer acc x)]
            [(flvector? x) (run-flvector reducer acc x)]
            [else (reducer-step reducer acc x)])))

  #|proc:tcat
  The `tcat` procedure returns a transducer that concatenates each transducible input collection
  into the output stream, including registered sources, iterators, and eductions.
  Other inputs are emitted as scalar values. Generated iterators are finalized on completion,
  early termination, or escape. Caller-supplied iterators remain open. Inner eductions complete
  their own state without completing the surrounding reduction.
  |#
  (define tcat
    (lambda ()
      (make-transducer
       'tcat
       (lambda (reducer)
         (mk-$reducer
          'tcat
          (case-lambda
            [() (reducer-init reducer)]
            [(acc) (reducer-complete reducer acc)]
            [(acc x) (flatten-emit reducer acc x)])
          #f)))))

  #|proc:tmapcat
  The `tmapcat` procedure returns a transducer that applies unary `(proc value)` to each
  input and concatenates each produced transducible collection. Scalar results are emitted
  unchanged, following `tcat`.
  |#
  (define tmapcat
    (lambda (proc)
      (pcheck ([procedure? proc])
              (tcompose (tmap proc) (tcat)))))

  #|proc:tflatten
  The `tflatten` procedure recursively emits leaf values from nested supported
  collections.
  |#
  (define tflatten
    (lambda ()
      (make-transducer
       'tflatten
       (lambda (reducer)
         (mk-$reducer
          'tflatten
          (case-lambda
            [() (reducer-init reducer)]
            [(acc) (reducer-complete reducer acc)]
            [(acc x) (flatten-rec reducer acc x)])
          #f)))))

  #|proc:ttake
  The `ttake` procedure returns a stateful transducer that emits at most `n`
  input values.
  |#
  (define ttake
    (lambda (n)
      (pcheck ([natural? n])
              (make-transducer
               'ttake
               (lambda (reducer)
                 (let ([left n])
                   (mk-$reducer
                    'ttake
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc) (reducer-complete reducer acc)]
                      [(acc x)
                       (if (zero? left)
                           (reduced acc)
                           (begin
                             (set! left (- left 1))
                             (let ([next (reducer-step reducer acc x)])
                               (if (or (zero? left) (reduced? next))
                                   (ensure-reduced next)
                                   next))))])
                    #f)))))))

  #|proc:tdrop
  The `tdrop` procedure returns a stateful transducer that skips the first `n`
  input values and emits the rest.
  |#
  (define tdrop
    (lambda (n)
      (pcheck ([natural? n])
              (make-transducer
               'tdrop
               (lambda (reducer)
                 (let ([left n])
                   (mk-$reducer
                    'tdrop
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc) (reducer-complete reducer acc)]
                      [(acc x)
                       (if (positive? left)
                           (begin (set! left (- left 1)) acc)
                           (reducer-step reducer acc x))])
                    #f)))))))

  #|proc:ttake-while
  The `ttake-while` procedure emits input values while `pred` returns true,
  then requests early termination before emitting the first failing value.
  |#
  (define ttake-while
    (lambda (pred)
      (pcheck ([procedure? pred])
              (make-transducer
               'ttake-while
               (lambda (reducer)
                 (mk-$reducer
                  'ttake-while
                  (case-lambda
                    [() (reducer-init reducer)]
                    [(acc) (reducer-complete reducer acc)]
                    [(acc x)
                     (if (pred x)
                         (reducer-step reducer acc x)
                         (reduced acc))])
                  #f))))))

  #|proc:tdrop-while
  The `tdrop-while` procedure skips input values while `pred` returns true,
  then emits the first failing value and all remaining values.
  |#
  (define tdrop-while
    (lambda (pred)
      (pcheck ([procedure? pred])
              (make-transducer
               'tdrop-while
               (lambda (reducer)
                 (let ([dropping? #t])
                   (mk-$reducer
                    'tdrop-while
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc) (reducer-complete reducer acc)]
                      [(acc x)
                       (if (and dropping? (pred x))
                           acc
                           (begin
                             (set! dropping? #f)
                             (reducer-step reducer acc x)))])
                    #f)))))))

  #|proc:ttake-nth
  The `ttake-nth` procedure emits every `n`th input value, starting with index
  zero.
  |#
  (define ttake-nth
    (lambda (n)
      (pcheck ([positive-natural? n])
              (make-transducer
               'ttake-nth
               (lambda (reducer)
                 (let ([i 0])
                   (mk-$reducer
                    'ttake-nth
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc) (reducer-complete reducer acc)]
                      [(acc x)
                       (let ([j i])
                         (set! i (+ i 1))
                         (if (zero? (modulo j n))
                             (reducer-step reducer acc x)
                             acc))])
                    #f)))))))

  (define tslice/check
    (lambda (start stop step)
      (unless (and (natural? start) (natural? stop) (positive-natural? step))
        (errorf 'tslice "invalid slice start/stop/step: ~a ~a ~a" start stop step))
      (when (> start stop)
        (errorf 'tslice "slice start greater than stop: ~a > ~a" start stop))))

  #|proc:tslice
  The `tslice` procedure emits input values with indexes in `[start, stop)`.
  With three arguments, `step` selects indexes by positive stride.
  |#
  (define tslice
    (case-lambda
      [(start stop) (tslice start stop 1)]
      [(start stop step)
       (tslice/check start stop step)
       (make-transducer
        'tslice
        (lambda (reducer)
          (let ([i 0])
            (mk-$reducer
             'tslice
             (case-lambda
               [() (reducer-init reducer)]
               [(acc) (reducer-complete reducer acc)]
               [(acc x)
                (let ([j i])
                  (set! i (+ i 1))
                  (cond [(>= j stop) (reduced acc)]
                        [(and (>= j start)
                              (zero? (modulo (- j start) step)))
                         (let ([next (reducer-step reducer acc x)])
                           (if (or (reduced? next) (>= i stop))
                               (ensure-reduced next)
                               next))]
                        [(>= i stop) (reduced acc)]
                        [else acc]))])
             #f))))]))

  (define list-builder->vector
    (lambda (builder)
      (list->vector (builder))))

  #|proc:tpartition
  The `tpartition` procedure emits vectors of exactly `n` values and drops the
  trailing partial partition.
  |#
  (define tpartition
    (lambda (n)
      (pcheck ([positive-natural? n])
              (make-transducer
               'tpartition
               (lambda (reducer)
                 (let ([buf (make-list-builder)] [count 0])
                   (mk-$reducer
                    'tpartition
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc) (reducer-complete reducer acc)]
                      [(acc x)
                       (buf x)
                       (set! count (+ count 1))
                       (if (= count n)
                           (let ([chunk (list-builder->vector buf)])
                             (set! buf (make-list-builder))
                             (set! count 0)
                             (reducer-step reducer acc chunk))
                           acc)])
                    #f)))))))

  #|proc:tpartition-all
  The `tpartition-all` procedure emits vectors of up to `n` values, including
  the trailing partial partition during completion.
  |#
  (define tpartition-all
    (lambda (n)
      (pcheck ([positive-natural? n])
              (make-transducer
               'tpartition-all
               (lambda (reducer)
                 (let ([buf (make-list-builder)] [count 0])
                   (mk-$reducer
                    'tpartition-all
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc)
                       (let ([acc (if (> count 0)
                                      (reducer-step reducer acc (list-builder->vector buf))
                                      acc)])
                         (reducer-complete reducer (unreduced acc)))]
                      [(acc x)
                       (buf x)
                       (set! count (+ count 1))
                       (if (= count n)
                           (let ([chunk (list-builder->vector buf)])
                             (set! buf (make-list-builder))
                             (set! count 0)
                             (reducer-step reducer acc chunk))
                           acc)])
                    #f)))))))

  #|proc:tpartition-by
  The `tpartition-by` procedure starts a new vector partition whenever
  `(proc x)` changes according to `equal?`.
  |#
  (define tpartition-by
    (lambda (proc)
      (pcheck ([procedure? proc])
              (make-transducer
               'tpartition-by
               (lambda (reducer)
                 (let ([started? #f] [key #f] [buf (make-list-builder)])
                   (mk-$reducer
                    'tpartition-by
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc)
                       (let ([acc (if started?
                                      (reducer-step reducer acc (list-builder->vector buf))
                                      acc)])
                         (reducer-complete reducer (unreduced acc)))]
                      [(acc x)
                       (let ([k (proc x)])
                         (cond [(not started?)
                                (set! started? #t)
                                (set! key k)
                                (buf x)
                                acc]
                               [(equal? k key)
                                (buf x)
                                acc]
                               [else
                                (let ([chunk (list-builder->vector buf)])
                                  (set! key k)
                                  (set! buf (make-list-builder))
                                  (buf x)
                                  (let ([next (reducer-step reducer acc chunk)])
                                    (when (reduced? next)
                                      (set! started? #f))
                                    next))]))])
                    #f)))))))

  #|proc:tdedupe
  The `tdedupe` procedure removes consecutive duplicate values using `equal?`.
  |#
  (define tdedupe
    (lambda ()
      (tdedupe-by id)))

  #|proc:tdedupe-by
  The `tdedupe-by` procedure removes consecutive values whose computed keys are
  `equal?`.
  |#
  (define tdedupe-by
    (lambda (proc)
      (pcheck ([procedure? proc])
              (make-transducer
               'tdedupe-by
               (lambda (reducer)
                 (let ([started? #f] [key #f])
                   (mk-$reducer
                    'tdedupe-by
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc) (reducer-complete reducer acc)]
                      [(acc x)
                       (let ([k (proc x)])
                         (if (and started? (equal? k key))
                             acc
                             (begin
                               (set! started? #t)
                               (set! key k)
                               (reducer-step reducer acc x))))])
                    #f)))))))

  #|proc:tdistinct
  The `tdistinct` procedure emits only the first occurrence of each value using
  `equal?`.
  |#
  (define tdistinct
    (lambda ()
      (tdistinct-by id)))

  #|proc:tdistinct-by
  The `tdistinct-by` procedure emits only the first occurrence of each computed
  key using `equal?`.
  |#
  (define tdistinct-by
    (lambda (proc)
      (pcheck ([procedure? proc])
              (make-transducer
               'tdistinct-by
               (lambda (reducer)
                 (let ([seen (make-hashtable equal-hash equal?)])
                   (mk-$reducer
                    'tdistinct-by
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc) (reducer-complete reducer acc)]
                      [(acc x)
                       (let ([k (proc x)])
                         (if (hashtable-contains? seen k)
                             acc
                             (begin
                               (hashtable-set! seen k #t)
                               (reducer-step reducer acc x))))])
                    #f)))))))

  #|proc:tinterpose
  The `tinterpose` procedure emits `sep` between input values.
  |#
  (define tinterpose
    (lambda (sep)
      (make-transducer
       'tinterpose
       (lambda (reducer)
         (let ([first? #t])
           (mk-$reducer
            'tinterpose
            (case-lambda
              [() (reducer-init reducer)]
              [(acc) (reducer-complete reducer acc)]
              [(acc x)
               (if first?
                   (begin
                     (set! first? #f)
                     (reducer-step reducer acc x))
                   (let ([acc (reducer-step reducer acc sep)])
                     (if (reduced? acc)
                         acc
                         (reducer-step reducer acc x))))])
            #f))))))

  #|proc:ttap
  The `ttap` procedure calls `(proc x)` for each input value, then emits `x`.
  |#
  (define ttap
    (lambda (proc)
      (pcheck ([procedure? proc])
              (make-transducer
               'ttap
               (lambda (reducer)
                 (mk-$reducer
                  'ttap
                  (case-lambda
                    [() (reducer-init reducer)]
                    [(acc) (reducer-complete reducer acc)]
                    [(acc x)
                     (proc x)
                     (reducer-step reducer acc x)])
                  #f))))))

  #|proc:tinspect
  The `tinspect` procedure calls `(proc who x)` for each input value, then
  emits `x`.
  |#
  (define tinspect
    (lambda (who proc)
      (pcheck ([procedure? proc])
              (make-transducer
               'tinspect
               (lambda (reducer)
                 (mk-$reducer
                  'tinspect
                  (case-lambda
                    [() (reducer-init reducer)]
                    [(acc) (reducer-complete reducer acc)]
                    [(acc x)
                     (proc who x)
                     (reducer-step reducer acc x)])
                  #f))))))

  #|proc:transduce
  The `transduce` procedure runs transducer `xform` over transducible `source` using reducer
  record `reducer`. With three arguments, the transformed reducer supplies the initial
  accumulator; with four arguments, `init` supplies it and initialization is skipped.
  The result is the reducer's completion value after source exhaustion or an early `reduced`
  result. Iterators created for source adapters are finalized on exhaustion, early termination,
  errors, or nonlocal exits. Caller-supplied iterators remain open.
  |#
  (define transduce
    (case-lambda
      [(xform reducer source)
       (transduce-default xform reducer source)]
      [(xform reducer init source)
       (transduce-explicit xform reducer init source)]))

  #|proc:transduce1
  The `transduce1` procedure runs transducer `xform` over transducible `source` using its first
  transformed value as the accumulator for reducer record `reducer`. The result is the reducer's
  completion value. An empty transformed source raises an error. The first value must be a valid
  accumulator for `reducer`.
  |#
  (define transduce1
    (lambda (xform reducer source)
      (pcheck ([transducer? xform] [reducer? reducer] [transducible? source])
              (let ([seen? #f])
                (transduce
                 xform
                 (make-reducer
                  'transduce1
                  (case-lambda
                    [() #f]
                    [(acc)
                     (if seen?
                         (reducer-complete reducer acc)
                         (errorf 'transduce1 "empty transformed source"))]
                    [(acc x)
                     (if seen?
                         (reducer-step reducer acc x)
                         (begin
                           (set! seen? #t)
                           x))]))
                 source)))))

  #|proc:sequence
  The `sequence` procedure returns an iterator over the transformed values from
  `source`. Values are pulled from `source` only as the returned iterator is
  advanced. An iterator source starts at its current position and remains open at
  exhaustion. Resetting the result resets an iterator source and the transducer state;
  other sources receive a new traversal. Explicitly finalizing the result finalizes its
  upstream iterator, including a caller-supplied iterator.
  Initialization emissions are retained, and an initially reduced accumulator finishes without
  pulling input. A failed pass releases an owned source and stays exhausted until reset.
  An escape during construction, reset, or advance releases an owned source and exhausts the pass.
  Each input value's entire transformed expansion is buffered before returning its first value.
  |#
  (define sequence
    (lambda (xform source)
      (pcheck ([transducer? xform] [transducible? source])
              (if (eduction? source)
                  (sequence (tcompose ($eduction-xform source) xform)
                            ($eduction-source source))
                  (let ([upstream #f]
                        [cleanup-attempted? #f]
                        [effective #f]
                        [acc #f]
                        [done? #f]
                        [q-head '()]
                        [q-tail '()])
                    (define enqueue!
                      (lambda (x)
                        (let ([cell (cons x '())])
                          (if (null? q-head)
                              (begin
                                (set! q-head cell)
                                (set! q-tail cell))
                              (begin
                                (set-cdr! q-tail cell)
                                (set! q-tail cell))))))
                    (define dequeue!
                      (lambda ()
                        (let ([x (car q-head)])
                          (set! q-head (cdr q-head))
                          (when (null? q-head)
                            (set! q-tail '()))
                          x)))
                    (define queue-empty?
                      (lambda ()
                        (null? q-head)))
                    (define emit-reducer
                      (make-reducer
                       'sequence
                       (case-lambda
                         [() (void)]
                         [(acc) acc]
                         [(acc x) (enqueue! x) acc])))
                    (define finalize-upstream!
                      (lambda ()
                        (when (and upstream (not (iter-finalized? upstream)))
                          (set! cleanup-attempted? #t)
                          (iter-finalize! upstream))))
                    (define abandon-pass!
                      (lambda ()
                        (set! done? #t)
                        (set! q-head '())
                        (set! q-tail '())
                        (unless (or (iter? source) cleanup-attempted?)
                          (finalize-upstream!))))
                    (define fail!
                      (lambda (exn)
                        (abandon-pass!)
                        (raise exn)))
                    (define reset-state!
                      (lambda ()
                        (let ([completed? #f])
                          (dynamic-wind
                            void
                            (lambda ()
                              (guard (exn [else (fail! exn)])
                                (if (and upstream (iter? source))
                                    (iter-reset! upstream)
                                    (begin
                                      (finalize-upstream!)
                                      (set! upstream (source->iter source))))
                                (set! cleanup-attempted? #f)
                                (set! done? #f)
                                (set! q-head '())
                                (set! q-tail '())
                                (set! effective (apply-transducer xform emit-reducer))
                                (set! acc (reducer-init effective))
                                (set! completed? #t)))
                            (lambda ()
                              (unless completed?
                                (abandon-pass!)))))))
                    (define finish!
                      (lambda ()
                        (unless done?
                          (set! done? #t)
                          (set! acc (reducer-complete effective (unreduced acc)))
                          (unless (iter? source)
                            (finalize-upstream!)))))
                    (define next!
                      (lambda ()
                        (let ([completed? #f])
                          (dynamic-wind
                            void
                            (lambda ()
                              (let ([result
                                     (guard (exn [else (fail! exn)])
                                       (let loop ()
                                         (cond [(not (queue-empty?)) (dequeue!)]
                                               [done? iter-end]
                                               [(reduced? acc) (finish!) (loop)]
                                               [else
                                                (let ([value (iter-next! upstream)])
                                                  (if (iter-end? value)
                                                      (finish!)
                                                      (begin
                                                        (set! acc (reducer-step effective acc value))
                                                        (when (reduced? acc) (finish!))))
                                                  (loop))])))])
                                (set! completed? #t)
                                result))
                            (lambda ()
                              (unless completed? (abandon-pass!)))))))
                    (reset-state!)
                    (make-iter next! reset-state! finalize-upstream!))))))

  #|proc:into
  The `into` procedure transduces transducible `source` with transducer `xform` into the
  destination named by symbol `to`. Supported destinations are `'list`, `'reverse-list`,
  `'vector`, `'string`, and `'bytevector`. The result has the requested type.
  |#
  (define into
    (lambda (to xform source)
      (pcheck ([symbol? to] [transducer? xform] [transducible? source])
              (case to
                [(list) (transduce xform (rflist) source)]
                [(reverse-list) (transduce xform (rfreverselist) source)]
                [(vector) (transduce xform (rfvector) source)]
                [(string) (transduce xform (rfstring) source)]
                [(bytevector) (transduce xform (rfbytevector) source)]
                [else (errorf 'into "unsupported transducer destination: ~a" to)]))))

  #|proc:list-transduce
  The `list-transduce` procedure transduces list `source` with direct pair
  traversal.
  |#
  (define list-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([list? source])
               (transduce-run 'list-transduce xform reducer default-init source run-list))]
      [(xform reducer init source)
       (pcheck ([list? source])
               (transduce-run 'list-transduce xform reducer init source run-list))]))

  #|proc:vector-transduce
  The `vector-transduce` procedure transduces vector `source` with direct
  indexed traversal.
  |#
  (define vector-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([vector? source])
               (transduce-run 'vector-transduce xform reducer default-init source run-vector))]
      [(xform reducer init source)
       (pcheck ([vector? source])
               (transduce-run 'vector-transduce xform reducer init source run-vector))]))

  #|proc:bytevector-transduce
  The `bytevector-transduce` procedure transduces bytevector `source` by
  reading unsigned bytes.
  |#
  (define bytevector-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([bytevector? source])
               (transduce-run 'bytevector-transduce xform reducer default-init source run-bytevector))]
      [(xform reducer init source)
       (pcheck ([bytevector? source])
               (transduce-run 'bytevector-transduce xform reducer init source run-bytevector))]))

  #|proc:string-transduce
  The `string-transduce` procedure transduces string `source` by reading
  characters.
  |#
  (define string-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([string? source])
               (transduce-run 'string-transduce xform reducer default-init source run-string))]
      [(xform reducer init source)
       (pcheck ([string? source])
               (transduce-run 'string-transduce xform reducer init source run-string))]))

  #|proc:iter-transduce
  The `iter-transduce` procedure transduces iterator `source` by repeatedly
  calling `iter-next!`.
  |#
  (define iter-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([iter? source])
               (transduce-run 'iter-transduce xform reducer default-init source run-iter))]
      [(xform reducer init source)
       (pcheck ([iter? source])
               (transduce-run 'iter-transduce xform reducer init source run-iter))]))

  #|proc:fxvector-transduce
  The `fxvector-transduce` procedure transduces fxvector `source` with direct
  indexed traversal.
  |#
  (define fxvector-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([fxvector? source])
               (transduce-run 'fxvector-transduce xform reducer default-init source run-fxvector))]
      [(xform reducer init source)
       (pcheck ([fxvector? source])
               (transduce-run 'fxvector-transduce xform reducer init source run-fxvector))]))

  #|proc:flvector-transduce
  The `flvector-transduce` procedure transduces flvector `source` with direct
  indexed traversal.
  |#
  (define flvector-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([flvector? source])
               (transduce-run 'flvector-transduce xform reducer default-init source run-flvector))]
      [(xform reducer init source)
       (pcheck ([flvector? source])
               (transduce-run 'flvector-transduce xform reducer init source run-flvector))]))

  #|proc:hashtable-transduce
  The `hashtable-transduce` procedure transduces hashtable values with direct
  traversal over the hashtable values vector.
  |#
  (define hashtable-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([hashtable? source])
               (transduce-run 'hashtable-transduce xform reducer default-init source run-hashtable))]
      [(xform reducer init source)
       (pcheck ([hashtable? source])
               (transduce-run 'hashtable-transduce xform reducer init source run-hashtable))]))

  #|proc:port-lines-transduce
  The `port-lines-transduce` procedure transduces lines from caller-owned
  textual input port `source`. It does not close `source`.
  |#
  (define port-lines-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([transducer-input-textual-port? source])
               (transduce-run 'port-lines-transduce xform reducer default-init source run-port-lines))]
      [(xform reducer init source)
       (pcheck ([transducer-input-textual-port? source])
               (transduce-run 'port-lines-transduce xform reducer init source run-port-lines))]))

  #|proc:port-bytes-transduce
  The `port-bytes-transduce` procedure transduces bytes from caller-owned
  binary input port `source`. It does not close `source`.
  |#
  (define port-bytes-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([transducer-input-binary-port? source])
               (transduce-run 'port-bytes-transduce xform reducer default-init source run-port-bytes))]
      [(xform reducer init source)
       (pcheck ([transducer-input-binary-port? source])
               (transduce-run 'port-bytes-transduce xform reducer init source run-port-bytes))]))

  #|proc:tfor-each
  The `tfor-each` procedure runs `proc` for side effects on each transformed
  value from `source` and returns unspecified values.
  |#
  (define tfor-each
    (lambda (xform proc source)
      (pcheck ([transducer? xform] [procedure? proc] [transducible? source])
              (transduce
               xform
               (make-reducer
                'tfor-each
                (case-lambda
                  [() (void)]
                  [(acc) (void)]
                  [(acc x) (proc x) acc]))
               source)
              (void))))

  #|proc:make-reducer
  The `make-reducer` procedure returns a reducer named by symbol `name`. Procedure `proc`
  implements `() -> initial-accumulator`, `(accumulator) -> completed-result`, and `(accumulator
  value) -> next-accumulator-or-reduced`. Initialization creates independent accumulation state
  for each reduction; completion finalizes the result once. A step wraps its accumulator with
  `reduced` to request early termination.
  |#
  (define make-reducer
    (lambda (name proc)
      (pcheck ([symbol? name] [procedure? proc])
              (mk-$reducer name proc #f))))

  #|proc:reducer-name
  The `reducer-name` procedure returns the symbolic name of `reducer`.
  |#
  (define reducer-name
    (lambda (reducer)
      (pcheck ([reducer? reducer])
              ($reducer-name reducer))))

  #|proc:rflist
  The `rflist` procedure returns a reducer that accumulates values into a list
  in input order.
  |#
  (define rflist
    (lambda ()
      (make-list-like-reducer 'rflist reverse)))

  #|proc:rfreverselist
  The `rfreverselist` procedure returns a reducer that accumulates values into
  a list in reverse input order.
  |#
  (define rfreverselist
    (lambda ()
      (make-list-like-reducer 'rfreverselist id)))

  #|proc:rfvector
  The `rfvector` procedure returns a reducer that accumulates values into a
  vector in input order.
  |#
  (define rfvector
    (lambda ()
      (make-list-builder-reducer 'rfvector list->vector)))

  #|proc:rfstring
  The `rfstring` procedure returns a reducer that accumulates characters into
  a string in input order.
  |#
  (define rfstring
    (lambda ()
      (make-list-builder-reducer 'rfstring list->string)))

  #|proc:rfbytevector
  The `rfbytevector` procedure returns a reducer that accumulates exact byte
  values into a bytevector in input order.
  |#
  (define rfbytevector
    (lambda ()
      (make-list-builder-reducer 'rfbytevector list->bytevector)))

  #|proc:rfcount
  The `rfcount` procedure returns a reducer that counts transformed values.
  |#
  (define rfcount
    (lambda ()
      (make-reducer
       'rfcount
       (case-lambda
         [() 0]
         [(acc) acc]
         [(acc x) (+ acc 1)]))))

  #|proc:rfsum
  The `rfsum` procedure returns a reducer that sums transformed numeric values
  with `+`.
  |#
  (define rfsum
    (lambda ()
      (make-reducer
       'rfsum
       (case-lambda
         [() 0]
         [(acc) acc]
         [(acc x) (+ acc x)]))))

  #|proc:rffxsum
  The `rffxsum` procedure returns a reducer that sums transformed fixnum values
  with `fx+`.
  |#
  (define rffxsum
    (lambda ()
      (make-reducer
       'rffxsum
       (case-lambda
         [() 0]
         [(acc) acc]
         [(acc x) (fx+ acc x)]))))

  #|proc:rfflsum
  The `rfflsum` procedure returns a reducer that sums transformed flonum values
  with `fl+`.
  |#
  (define rfflsum
    (lambda ()
      (make-reducer
       'rfflsum
       (case-lambda
         [() 0.0]
         [(acc) acc]
         [(acc x) (fl+ acc x)]))))

  #|proc:rfproduct
  The `rfproduct` procedure returns a reducer that multiplies transformed
  numeric values with `*`.
  |#
  (define rfproduct
    (lambda ()
      (make-reducer
       'rfproduct
       (case-lambda
         [() 1]
         [(acc) acc]
         [(acc x) (* acc x)]))))

  (define extrema-reducer
    (lambda (name better?)
      (let ([empty (gensym)])
        (make-reducer
         name
         (case-lambda
           [() empty]
           [(acc)
            (if (eq? acc empty)
                (errorf name "empty transformed source")
                acc)]
           [(acc x)
            (if (or (eq? acc empty) (better? x acc)) x acc)])))))

  #|proc:rfmin
  The `rfmin` procedure returns a reducer that keeps the minimum value according to `(less? left
  right)`, a strict ordering predicate. Empty unseeded reductions raise an error; explicit
  accumulators and `transduce1` seeds participate in comparisons.
  |#
  (define rfmin
    (lambda (less?)
      (pcheck ([procedure? less?])
              (extrema-reducer 'rfmin less?))))

  #|proc:rfmax
  The `rfmax` procedure returns a reducer that keeps the maximum value according to `(less? left
  right)`, a strict ordering predicate. Empty unseeded reductions raise an error; explicit
  accumulators and `transduce1` seeds participate in comparisons.
  |#
  (define rfmax
    (lambda (less?)
      (pcheck ([procedure? less?])
              (extrema-reducer 'rfmax (lambda (x acc) (less? acc x))))))

  #|proc:rfany
  The `rfany` procedure returns a reducer that applies unary predicate `(pred x)`. Reduction
  returns its first true result and terminates early, or `#f` if no value matches.
  A true incoming accumulator is retained without calling `pred` again.
  |#
  (define rfany
    (lambda (pred)
      (pcheck ([procedure? pred])
              (make-reducer
               'rfany
               (case-lambda
                 [() #f]
                 [(acc) acc]
                 [(acc x)
                  (if acc
                      (reduced acc)
                      (let ([res (pred x)])
                        (if res (reduced res) #f)))])))))

  #|proc:rfevery
  The `rfevery` procedure returns a reducer that applies unary predicate `(pred x)`. Reduction
  returns `#f` at the first false result and terminates early, or `#t` otherwise.
  A false incoming accumulator is retained without calling `pred` again.
  |#
  (define rfevery
    (lambda (pred)
      (pcheck ([procedure? pred])
              (make-reducer
               'rfevery
               (case-lambda
                 [() #t]
                 [(acc) acc]
                 [(acc x)
                  (if (and acc (pred x))
                      #t
                      (reduced #f))])))))

  #|proc:eduction
  The `eduction` procedure returns an eduction record containing `xform` and
  `source`. Traversing the eduction applies `xform` before the terminal
  operation's transducer.
  |#
  (define eduction
    (lambda (xform source)
      (pcheck ([transducer? xform] [transducible? source])
              (mk-$eduction xform source))))

  #|proc:source->iter
  The `source->iter` procedure converts a supported Phase 2 transducer source
  to an iterator for explicit interop or debug traversal. It wraps the lower-level
  `iter-source->iter` conversion and additionally handles transducer eduction sources.
  |#
  (define source->iter
    (lambda (source)
      (pcheck ([transducible? source])
              (if (eduction? source)
                  (sequence (tidentity) source)
                  (iter-source->iter source)))))

  #|proc:transducible?
  The `transducible?` procedure returns `#t` for Phase 1 sources accepted by
  `transduce`.
  |#
  (define transducible?
    (lambda (x)
      (or (iterable? x)
          (eduction? x)))))
