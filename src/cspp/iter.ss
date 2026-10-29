(library (cspp iter)
  (export range make-indexed-iter
          list->iter vector->iter string->iter
          bytevector->iter fxvector->iter flvector->iter
          port->iter port-lines->iter port-chars->iter port-data->iter
          file->iter file-lines->iter file-chars->iter file-data->iter
          iter->list iter-source->iter iterable? iter-register-source!

          get-iter make-iter iter-end iter-end? iter-next! iter-reset! iter-finalize!
          iter-finalized? iter?
          iter-for-each iter-map iter-filter iter-take iter-drop iter-fold
          iter-append iter-concat iter-distinct iter-sorted iter-zip iter-interleave
          iter->vector

          iter-sum iter-product iter-avg
          iter-fxsum iter-fxproduct iter-fxavg
          iter-flsum iter-flproduct iter-flavg
          iter-max iter-min)
  (import (chezscheme)
          (cspp list)
          (cspp vector)
          (cspp utils)
          (cspp internal)
          (cspp private iter-state)
          (cspp control))

;;;;===----------------------------------------------------------------------===
;;;; Iterator kinds, dependencies, and lifecycle
;;;;===----------------------------------------------------------------------===

  ;; An iterator is a destructive cursor: next! advances mutable producer state.
  ;; A pass starts at construction/reset and ends at exhaustion or terminal completion.
  ;; A pipeline is the ordered map/filter/take/drop configuration stored on that cursor;
  ;; adding a stage mutates and returns the same iterator. Reset retains configuration
  ;; but rewinds producer state and built-in stage counters. User callback state is not reset.
  ;;
  ;; A constructor makes a cursor; an intermediate combinator configures or wraps it;
  ;; a terminal consumes values and finalizes the entire reachable resource chain on
  ;; success, including early completion. Exhaustion means next! returns iter-end,
  ;; not resource release. A resource is an external handle needing cleanup (file/port);
  ;; ownership means responsibility for that cleanup, not exclusive use of a cursor.
  ;; Resource-free cursors need neither ownership nor finalization. A wrapper's child
  ;; links describe its upstream iterators; they keep finalization responsibility until
  ;; a terminal succeeds or iter-finalize! is called. Shared handles are finalized once.
  ;; A restart dependency is an upstream cursor that must reset before the wrapper's
  ;; local state. Reset visits each reachable dependency once per identity. Cleanup
  ;; links may also retain historical nested resources that need no implicit restart.
  ;;
  ;; Independent operations have no conflicting mutable accesses requiring ordering:
  ;; separate cursors over unchanged data are independent, even in threads/fibers.
  ;; Dependent operations advance/reset/finalize one cursor, mutate an aliased input,
  ;; or share mutable callback data. Their caller must order/synchronize them. The
  ;; library does not serialize these caller bugs. Registration is internally synchronized.
  ;;
  ;; Continuation resumption restores control/bindings, not producer positions, stage
  ;; counters, output buffers, callback effects or I/O. Fiber suspension preserves the
  ;; live pass. Resuming after reset/finalization is a dependent lifecycle interaction;
  ;; no reopening, rollback or snapshot is provided. Errors and nonlocal exits leave
  ;; the chain's state undefined, with no automatic cleanup or recovery guarantee.

  #|proc:iter?
  Return whether `value` is an iterator cursor, including the `iter-end` sentinel.
  |#
  (define iter? $iter?)

  #|proc:iter-finalized?
  Return whether resource finalization has disabled cursor `iterator`. Resource-free
  cursors always return `#f`; normal exhaustion alone does not set this flag.
  |#
  (define iter-finalized?
    (lambda (iterator) (pcheck ([$iter? iterator]) ($iter-finalized? iterator))))

  #|value:iter-end
  The unique iterator exhaustion sentinel; it is also a permanently empty, resource-free
  cursor. It is not an ordinary element value. Pulling always returns itself; reset and
  finalization are no-ops. Adding pipeline stages to this shared marker is an error.
  |#
  (define iter-end (mk-$iter (lambda () iter-end) void))
  #|proc:iter-end?
  Return whether `value` is the unique iterator exhaustion sentinel.
  |#
  (define iter-end? (lambda (value) (eq? value iter-end)))

  (define source-adapters (box '()))

  (define adapter-snapshot
    (lambda ()
      (let ([snapshot (unbox source-adapters)])
        (memory-order-acquire)
        snapshot)))

  #|proc:iter-register-source!
  Register `predicate` and `iterator-maker` as an iterator source adapter.
  Both parameters must be procedures. The maker receives one matching source and must
  return an iterator. Registering the same predicate procedure object twice raises an
  error without changing the registry. Predicate signature: `(source) -> boolean`.
  Publication is atomic; each lookup uses one newest-first immutable snapshot, and
  predicates/makers run without registry locks. Distinct overlapping predicates are allowed,
  and newer registrations are checked first.
  Adapters should traverse source storage directly, rebuild state from current contents
  on reset, and treat active-pass mutation as unspecified. Hashtable-backed adapters may
  snapshot keys when the source API has no lazy cursor.
  |#
  (define-who iter-register-source!
    (lambda (predicate iterator-maker)
      (pcheck ([procedure? predicate iterator-maker])
              (let retry ()
                (let ([snapshot (adapter-snapshot)])
                  (when (assq predicate snapshot)
                    (errorf 'iter-register-source! "predicate procedure is already registered"))
                  (let ([updated (cons (cons predicate iterator-maker) snapshot)])
                    ;; CAS has no implied ordering and may fail spuriously in Chez.
                    (memory-order-release)
                    (unless (box-cas! source-adapters snapshot updated) (retry))))))))

  (define find-source-adapter
    (lambda (source)
      (let loop ([adapters (adapter-snapshot)])
        (cond [(null? adapters) #f]
              [((caar adapters) source) (cdar adapters)]
              [else (loop (cdr adapters))]))))

  #|proc:make-iter
  Construct a cursor from `next!-proc`, with signature `() -> value-or-iter-end`, and
  `reset!-proc`, with signature `() -> unspecified`, which rewinds that producer.
  Optional `fini-proc`, with signature `() -> unspecified`, declares resource ownership
  and releases that resource at terminal completion or explicit finalization. Omit it,
  or pass `void`, for a resource-free cursor. A custom wrapper must include cleanup of
  its upstream resources in this finalizer, and its reset callback manages any opaque
  upstream state. Built-in graph deduplication cannot discover such hidden dependencies.
  All three callbacks may suspend in fibers.
  Kind: cursor constructor; each call creates independent producer state.
  |#
  (define make-iter
    (case-lambda
      [(next!-proc reset!-proc)
       (mk-$iter next!-proc reset!-proc)]
      [(next!-proc reset!-proc fini-proc)
       (mk-$iter next!-proc reset!-proc fini-proc)]))

  ;; Pipeline entries are #(kind procedure initial-count remaining-count).
  ;; Take is a stop condition, not a predicate: even a downstream filter that
  ;; rejects the final taken item must not trigger another producer pull.
  (define pipeline-stopped?
    (lambda (ops)
      (exists (lambda (op) (and (eq? (vector-ref op 0) 'take)
                               (fxzero? (vector-ref op 3)))) ops)))

  #|proc:iter-next!
  Return the next value from `iter`, or `iter-end` when exhausted. Advancing a
  resource-finalized iterator is an error. Exhaustion itself does not finalize resources.
  |#
  (define iter-next!
    (lambda (iter)
      (pcheck ([$iter? iter])
              (when ($iter-finalized? iter)
                (errorf 'iter-next! "finalized iterator cannot be advanced!"))
              (let ([ops (($iter-ops iter))])
                (let pull ()
                  (if (pipeline-stopped? ops)
                      iter-end
                      (let ([value (($iter-next!-proc iter))])
                        (if (iter-end? value)
                            iter-end
                            (let apply-ops ([ops ops] [value value])
                              (if (null? ops)
                                  value
                                  (let* ([op (car ops)] [kind (vector-ref op 0)])
                                    (case kind
                                      [(proc) (apply-ops (cdr ops) ((vector-ref op 1) value))]
                                      [(filter) (if ((vector-ref op 1) value)
                                                    (apply-ops (cdr ops) value) (pull))]
                                      [(take)
                                       ;; Resumed upstream callbacks observe the current allowance,
                                       ;; which may already be exhausted. Never decrement past zero.
                                       (if (fxzero? (vector-ref op 3))
                                           iter-end
                                           (begin
                                             (vector-set! op 3 (fx1- (vector-ref op 3)))
                                             (apply-ops (cdr ops) value)))]
                                      [(drop)
                                       (if (fxzero? (vector-ref op 3))
                                           (apply-ops (cdr ops) value)
                                           (begin
                                             (vector-set! op 3 (fx1- (vector-ref op 3)))
                                             (pull)))]))))))))))))

  #|proc:iter-reset!
  Reset `iter` and its per-pass pipeline counters, retaining its map/filter/take/drop
  configuration. Each reachable restart dependency is reset once per identity before its
  wrapper's local state. Reset does not reopen finalized resources. Return an unspecified value.
  |#
  (define iter-reset!
    (lambda (iter)
      (pcheck ([$iter? iter])
              (reset-iter-chain! iter))))

  #|proc:iter-finalize!
  Release each resource reachable from `iter` once, including resources in upstream
  combinators. Resource-free iterators are unaffected and remain resettable. Repeated
  finalization is harmless. Return an unspecified value; cleanup errors leave state undefined.
  |#
  (define iter-finalize!
    (lambda (iter)
      (pcheck ([$iter? iter]) (finalize-iter-chain! iter))))

  (define iter-ops-add!
    (lambda (iter op)
      (when (iter-end? iter)
        (errorf 'iter "cannot configure the shared exhaustion sentinel"))
      (($iter-ops iter) op)))

  #|proc:make-indexed-iter
  The `make-indexed-iter` procedure returns an indexed iterator constructor named by
  `constructor-name`. The `source-predicate` procedure has signature `(source) -> boolean`.
  The `source-length` procedure has signature `(source) -> nonnegative integer`, and the
  `source-ref` procedure has signature `(source index) -> value`. The returned constructor
  accepts a source with optional `start`, `stop`, and nonzero `step` integers and returns an
  resource-free iterator. Integer-valued inexact bounds/steps/lengths are normalized to
  exact integers before `source-ref` is called. Negative bounds count from the source end;
  reset rereads source length and renormalizes bounds. Mutation during a pass is unspecified.
  Kind: constructor factory; each returned constructor call creates independent cursor state.
  |#
  (define make-indexed-iter
    (lambda (constructor-name source-predicate source-length source-ref)
      (pcheck ([symbol? constructor-name]
               [procedure? source-predicate source-length source-ref])
              (letrec ([make-source-iter
                        (lambda (source original-start original-stop step full-source?)
                          (let ([index 0] [stop 0]
                                [original-start (exact original-start)]
                                [original-stop (exact original-stop)]
                                [step (exact step)])
                            (define reset!
                              (lambda ()
                                (let* ([raw-length (source-length source)]
                                       [length (if (and (integer? raw-length) (>= raw-length 0))
                                                   (exact raw-length)
                                                   (errorf constructor-name
                                                           "invalid source length: ~a" raw-length))]
                                       [start-index
                                        (if (>= original-start 0)
                                            original-start
                                            (+ length original-start))]
                                       [stop-index
                                        (if full-source?
                                            length
                                            (if (>= original-stop 0)
                                                original-stop
                                                (+ length original-stop)))])
                                  (if (= length 0)
                                      (begin
                                        (set! index 0)
                                        (set! stop 0))
                                      (begin
                                        (set! index
                                              (cond [(< start-index 0) (if (> step 0) 0 -1)]
                                                    [(>= start-index length)
                                                     (if (> step 0) length (- length 1))]
                                                    [else start-index]))
                                        (set! stop
                                              (cond [(<= stop-index -1) -1]
                                                    [(>= stop-index length) length]
                                                    [else stop-index])))))))
                            (reset!)
                            (mk-$iter
                             (lambda ()
                               (if (if (> step 0)
                                       (>= index stop)
                                       (<= index stop))
                                   iter-end
                                   (let ([value (source-ref source index)])
                                     (set! index (+ index step))
                                     value)))
                             reset!)))])
                (case-lambda
                  [(source)
                   (pcheck ([source-predicate source])
                           (make-source-iter source 0 0 1 #t))]
                  [(source stop)
                   (pcheck ([source-predicate source] [integer? stop])
                           (make-source-iter source 0 stop 1 #f))]
                  [(source start stop)
                   (pcheck ([source-predicate source] [integer? start stop])
                           (make-source-iter source start stop 1 #f))]
                  [(source start stop step)
                   (pcheck ([source-predicate source] [integer? start stop step])
                           (when (= step 0)
                             (errorf constructor-name "step cannot be 0"))
                           (make-source-iter source start stop step #f))])))))

  #|proc:list->iter
  The `list->iter` procedure returns a forward iterator over `source`. The optional integer
  `start` and `stop` parameters select a half-open range, and positive `step` selects the
  distance between values. The iterator returns each selected value until it reaches its end.
  Reset resolves the selected start from the current source again, so changes between
  passes are visible. Mutation during a pass is unspecified.
  Kind: resource-free cursor constructor; each call creates independent producer state.
  |#
  (define-who list->iter
    (case-lambda
      [(source)
       (pcheck-list (source)
                    (let ([remaining source])
                      (mk-$iter
                       (lambda ()
                         (if (null? remaining)
                             iter-end
                             (let ([next (car remaining)])
                               (cdr! remaining)
                               next)))
                       (lambda () (set! remaining source)))))]
      [(source stop)
       (pcheck-list (source) (list->iter source 0 stop 1))]
      [(source start stop)
       (pcheck-list (source) (list->iter source start stop 1))]
      [(source start stop step)
       (pcheck ([list? source] [integer? start stop step])
               (when (<= step 0)
                 (errorf who "step must be positive"))
               (let ([index start] [remaining '()])
                 (define reset!
                   (lambda ()
                     (set! index start)
                     (set! remaining
                           (let loop ([tail source] [count start])
                             (cond [(null? tail) '()]
                                   [(= count 0) tail]
                                   [else (loop (cdr tail) (sub1 count))])))))
                 (reset!)
                 (mk-$iter
                  (lambda ()
                    (if (or (null? remaining) (>= index stop))
                        iter-end
                        (let ([value (car remaining)])
                          (let loop ([tail remaining] [count step])
                            (cond [(null? tail) (set! remaining '())]
                                  [(= count 0)
                                   (set! index (+ index step))
                                   (set! remaining tail)]
                                  [else (loop (cdr tail) (sub1 count))]))
                          value)))
                  reset!)))]))

  #|proc:vector->iter
  The `vector->iter` procedure returns an iterator over `source`. The optional integer
  `start`, `stop`, and nonzero `step` parameters select and direct a half-open indexed range.
  The iterator returns each selected vector value until it reaches its end.
  Kind: cursor constructor; each call creates independent producer state.
  |#
  (define vector->iter
    (make-indexed-iter 'vector->iter vector? vector-length vector-ref))

  (define hashtable->iter
    (lambda (val)
      (pcheck-hashtable (val)
                        (let ([keys #f] [i 0] [len 0])
                          (define reset!
                            (lambda ()
                              (set! keys (hashtable-keys val))
                              (set! len (vector-length keys))
                              (set! i 0)))
                          (reset!)
                          (mk-$iter
                           (lambda ()
                             (if (fx= i len)
                                 iter-end
                                 (let ([key (vector-ref keys i)])
                                   (set! i (fx1+ i))
                                   (hashtable-ref val key #f))))
                           reset!)))))

  #|proc:string->iter
  The `string->iter` procedure returns an iterator over `source`. The optional integer
  `start`, `stop`, and nonzero `step` parameters select and direct a half-open indexed range.
  The iterator returns each selected character until it reaches its end.
  Kind: cursor constructor; each call creates independent producer state.
  |#
  (define string->iter
    (make-indexed-iter 'string->iter string? string-length string-ref))

  #|proc:bytevector->iter
  The `bytevector->iter` procedure returns an iterator over unsigned bytes in `source`. The
  optional integer `start`, `stop`, and nonzero `step` parameters select and direct a half-open
  indexed range. The iterator returns each selected byte until it reaches its end.
  Kind: cursor constructor; each call creates independent producer state.
  |#
  (define bytevector->iter
    (make-indexed-iter 'bytevector->iter bytevector? bytevector-length bytevector-u8-ref))

  #|proc:fxvector->iter
  The `fxvector->iter` procedure returns an iterator over fixnums in `source`. The optional
  integer `start`, `stop`, and nonzero `step` parameters select and direct a half-open indexed
  range. The iterator returns each selected fixnum until it reaches its end.
  Kind: cursor constructor; each call creates independent producer state.
  |#
  (define fxvector->iter
    (make-indexed-iter 'fxvector->iter fxvector? fxvector-length fxvector-ref))

  #|proc:flvector->iter
  The `flvector->iter` procedure returns an iterator over flonums in `source`. The optional
  integer `start`, `stop`, and nonzero `step` parameters select and direct a half-open indexed
  range. The iterator returns each selected flonum until it reaches its end.
  Kind: cursor constructor; each call creates independent producer state.
  |#
  (define flvector->iter
    (make-indexed-iter 'flvector->iter flvector? flvector-length flvector-ref))

  #|proc:iterable?
  Return whether `source` is a built-in or registered iterator source.
  |#
  (define iterable?
    (lambda (source)
      (or ($iter? source)
          (list? source)
          (vector? source)
          (string? source)
          (bytevector? source)
          (fxvector? source)
          (flvector? source)
          (hashtable? source)
          (bool (find-source-adapter source)))))

  #|proc:iter-source->iter
  Convert a built-in or registered `source` to an iterator.
  Registered adapters are consulted after all built-in source types.
  This is the low-level iterator-library conversion procedure. The transducer
  library's `source->iter` wrapper additionally accepts transducer-specific sources.
  Hashtable passes snapshot keys because ChezScheme provides no lazy table cursor;
  values are read from the source table as they are requested, and reset snapshots keys again.
  Other mutable sources are traversed directly and reset against current contents.
  Mutation during an active pass is unspecified.
  |#
  (define-who iter-source->iter
    (lambda (source)
      (pcheck ([iterable? source])
              (cond [($iter? source) source]
                    [(list? source) (list->iter source)]
                    [(vector? source) (vector->iter source)]
                    [(string? source) (string->iter source)]
                    [(bytevector? source) (bytevector->iter source)]
                    [(fxvector? source) (fxvector->iter source)]
                    [(flvector? source) (flvector->iter source)]
                    [(hashtable? source) (hashtable->iter source)]
                    [else ((find-source-adapter source) source)]))))


;;;; ports are opened and closed by the caller

  (define define-textual-port->iter
    (lambda (who get-proc)
      (lambda (port)
        (pcheck ([input-port? port])
                (pcheck-open-textual-port
                 (port)
                 (mk-$iter
                  (lambda () (let ([x (get-proc port)])
                               (if (eof-object? x) iter-end x)))
                  (lambda () (set-port-position! port 0))
                  void))))))
  #|proc:port->iter
  Construct a cursor reading one line per pull from open textual input `port`.
  The caller retains port ownership: terminal completion does not close it. Reset seeks
  to position zero and requires a positionable port. Concurrent use of that port is dependent.
  |#
  (define port->iter
    (define-textual-port->iter 'port->iter get-line))
  #|proc:port-lines->iter
  Construct a cursor reading one line per pull from open textual input `port`.
  The caller retains port ownership: terminal completion does not close it. Reset seeks
  to position zero and requires a positionable port. Concurrent use of that port is dependent.
  |#
  (define port-lines->iter
    (define-textual-port->iter 'port-lines->iter get-line))
  #|proc:port-chars->iter
  Construct a cursor reading one character per pull from open textual input `port`.
  The caller retains port ownership: terminal completion does not close it. Reset seeks
  to position zero and requires a positionable port. Concurrent use of that port is dependent.
  |#
  (define port-chars->iter
    (define-textual-port->iter 'port-chars->iter get-char))
  #|proc:port-data->iter
  Construct a cursor reading one datum per pull from open textual input `port`.
  The caller retains port ownership: terminal completion does not close it. Reset seeks
  to position zero and requires a positionable port. Concurrent use of that port is dependent.
  |#
  (define port-data->iter
    (define-textual-port->iter 'port-data->iter get-datum))

  (define port-bytes->iter
    (lambda (port)
      (pcheck-open-binary-port
       (port)
       (mk-$iter
        (lambda () (let ([x (get-u8 port)])
                     (if (eof-object? x)
                         iter-end
                         x)))
        (lambda () (set-port-position! port 0))
        void))))



;;;; files are opened by iter, and closed in fini!

  (define define-file->iter
    (lambda (who open-proc get-proc)
      (lambda (file)
        (pcheck-file (file)
                     (let ([port (open-proc file)])
                       (mk-$iter
                        (lambda () (let ([x (get-proc port)])
                                     (if (eof-object? x)
                                         iter-end
                                         x)))
                        (lambda () (set-port-position! port 0))
                        (lambda () (close-port port))))))))
  #|proc:file->iter
  Open path `file` and construct a cursor reading one line per pull. It owns the
  opened port: successful terminal completion or explicit finalization closes it once.
  Exhaustion keeps it open. Reset seeks to zero while open; finalized files are not reopened.
  |#
  (define file->iter
    (define-file->iter 'file->iter open-input-file get-line))
  (define file-bytes->iter
    (define-file->iter 'file-bytes->iter open-file-input-port get-u8))
  #|proc:file-lines->iter
  Open path `file` and construct a cursor reading one line per pull. It owns the
  opened port: successful terminal completion or explicit finalization closes it once.
  Exhaustion keeps it open. Reset seeks to zero while open; finalized files are not reopened.
  |#
  (define file-lines->iter
    (define-file->iter 'file-lines->iter open-input-file get-line))
  #|proc:file-chars->iter
  Open path `file` and construct a cursor reading one character per pull. It owns the
  opened port: successful terminal completion or explicit finalization closes it once.
  Exhaustion keeps it open. Reset seeks to zero while open; finalized files are not reopened.
  |#
  (define file-chars->iter
    (define-file->iter 'file-chars->iter open-input-file get-char))
  #|proc:file-data->iter
  Open path `file` and construct a cursor reading one datum per pull. It owns the
  opened port: successful terminal completion or explicit finalization closes it once.
  Exhaustion keeps it open. Reset seeks to zero while open; finalized files are not reopened.
  |#
  (define file-data->iter
    (define-file->iter 'file-data->iter open-input-file get-datum))

  #|proc:get-iter
  Return an iterator for `val`. The symbol `who` identifies the caller in the
  error raised when `val` is not an iterable source.
  |#
  (define get-iter
    (lambda (who val)
      (if (iterable? val)
          (iter-source->iter val)
          (errorf who "cannot be iterated: ~a" val))))

  #|proc:range
  The `range` procedure returns an iterator over numbers from `start` toward exclusive `stop`.
  The nonzero `step` parameter is added after each value and must point toward `stop`. When only
  `stop` is supplied, `start` is zero; when `step` is omitted, it is one. Equal bounds produce
  an empty iterator.
  Kind: cursor constructor; each call creates independent producer state.
  |#
  (define-who range
    (case-lambda
      [(stop) (range 0 stop 1)]
      [(start stop) (range start stop 1)]
      [(start stop step)
       (pcheck ([number? start stop step])
               (when (= step 0)
                 (errorf who "step cannot be 0"))
               (unless (or (= start stop)
                           (and (< start stop) (> step 0))
                           (and (> start stop) (< step 0)))
                 (errorf who "step does not point from ~a toward ~a" start stop))
               (let ([value start])
                 (mk-$iter
                  (lambda ()
                    (if (if (> step 0)
                            (>= value stop)
                            (<= value stop))
                        iter-end
                        (let ([current value])
                          (set! value (+ value step))
                          current)))
                  (lambda () (set! value start)))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterator operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define all-iters? (lambda (x*) (andmap $iter? x*)))


;;; intermediate ops
;;; op types:
;;; - proc:   processes the item and returns a result for later processing
;;; - filter: processes the item and returns a boolean indicating
;;;           whether the item will be used for later processing
;;; Intermediate ops always return an iterator.

  #|proc:iter-map
  Add `proc` to the pipeline of `iter` and return `iter`. The procedure `proc`
  has signature `(value) -> mapped-value`. Kind: in-place pipeline configuration;
  it runs once per value reaching this stage and is retained by reset.
  |#
  (define iter-map
    (lambda (proc iter)
      (pcheck ([$iter? iter] [procedure? proc])
              (iter-ops-add! iter (vector 'proc proc #f #f))
              iter)))
  #|proc:iter-filter
  Add `proc` to the pipeline of `iter` and return `iter`. The predicate `proc`
  has signature `(value) -> boolean`. Kind: in-place pipeline configuration;
  rejected values advance the producer again, subject to any exhausted take stage.
  |#
  (define iter-filter
    (lambda (proc iter)
      (pcheck ([$iter? iter] [procedure? proc])
              (iter-ops-add! iter (vector 'filter proc #f #f))
              iter)))
  #|proc:iter-take
  Add an operation that retains at most the first nonnegative fixnum `n` values
  reaching this pipeline stage, then return `iter`. Kind: in-place limiting stage.
  Once exhausted it stops before another producer pull; zero never pulls. Reset restores `n`.
  Continuation resumption observes the current allowance and cannot make it negative.
  |#
  (define iter-take
    (lambda (n iter)
      (pcheck ([$iter? iter] [fixnum? n])
              (when (fxnegative? n) (errorf 'iter-take "negative item count: ~a" n))
              (iter-ops-add! iter (vector 'take #f n n))
              iter)))
  #|proc:iter-drop
  Add an operation that discards the first nonnegative fixnum `n` values
  reaching this pipeline stage, then return `iter`. Kind: in-place skipping stage;
  reset restores its remaining count to `n`.
  |#
  (define iter-drop
    (lambda (n iter)
      (pcheck ([$iter? iter] [fixnum? n])
              (when (fxnegative? n) (errorf 'iter-drop "negative item count: ~a" n))
              (iter-ops-add! iter (vector 'drop #f n n))
              iter)))

  ;; (iter-append (a0 a1 ...) (b0 b1 ...)) ->
  ;; (a0 a1 ... b0 b1 ...)
  #|proc:iter-append
  Return a sequential concatenation wrapper over `iter` and iterators in `iter*`, using
  `iter-concat` semantics. With one source return it directly. Repeated occurrences reset
  that same cursor before later passes; switching sources never finalizes them.
  |#
  (define iter-append
    (lambda (iter . iter*)
      (pcheck ([$iter? iter] [all-iters? iter*])
              (if (null? iter*) iter (apply iter-concat iter iter*)))))
  ;; (iter-zip (a0 a1 ...) (b0 b1 ...)) ->
  ;; ((a0 b0) (a1 b1) ...)
  #|proc:iter-zip
  Return an iterator of lists containing corresponding values from `iter` and
  `iter*`. Kind: lazy lockstep wrapper; traversal stops when any source ends, possibly
  consuming one value from other sources on its last pull. Further pulls stay exhausted
  without reading sources until reset. Reset resets all sources and clears that stop flag.
  Repeating a handle consumes successive values from that cursor; it does not clone it.
  With one source return it directly. Terminal completion finalizes all source resources.
  |#
  (define iter-zip
    (lambda (iter . iter*)
      (pcheck ([$iter? iter])
              (if (null? iter*)
                  iter
                  (pcheck ([all-iters? iter*])
                          (let ([iter* (cons iter iter*)] [done? #f])
                            (mk-$iter
                             (lambda ()
                               (if done?
                                   iter-end
                                   (let ([values (map iter-next! iter*)])
                                     (if (memq iter-end values)
                                         (begin (set! done? #t) iter-end)
                                         values))))
                             (lambda () (set! done? #f))
                             #f (lambda () iter*))))))))
  ;; (iter-interleave (a0 a1 ...) (b0 b1 ...)) ->
  ;; (a0 b0 a1 b1 ...)
  #|proc:iter-interleave
  Return an iterator that alternates values from `iter` and `iter*`, skipping
  exhausted sources until all have ended. Kind: lazy round-robin wrapper; reset resets
  all sources and its scheduling cursor. Exhausted slots leave the active scheduling ring;
  traversal costs O(values + source slots). Repeated handles share one producer position.
  Continuation resumption retains the current ring; already removed slots stay removed.
  With one source return it directly. Terminal completion finalizes all source resources.
  |#
  (define iter-interleave
    (lambda (iter . iter*)
      (pcheck ([$iter? iter])
              (if (null? iter*)
                  iter
                  (pcheck ([all-iters? iter*])
                          (let* ([sources (cons iter iter*)]
                                 [iterators (list->vector sources)]
                                 [length (vector-length iterators)]
                                 [next-slots (make-vector length)]
                                 [previous-slots (make-vector length)]
                                 [live-slots (make-vector length #t)]
                                 [active length] [index 0])
                            ;; A doubly linked active ring supports idempotent removal
                            ;; after a callback resumes with an advanced current schedule.
                            ;; All links/membership belong to this cursor's current pass.
                            (define reset-local!
                              (lambda ()
                                (do ([slot 0 (fx1+ slot)]) ((fx= slot length))
                                  (vector-set! next-slots slot
                                               (if (fx= slot (fx1- length)) 0 (fx1+ slot)))
                                  (vector-set! previous-slots slot
                                               (if (fxzero? slot) (fx1- length) (fx1- slot)))
                                  (vector-set! live-slots slot #t))
                                (set! active length)
                                (set! index 0)))
                            (reset-local!)
                            (mk-$iter
                             (lambda ()
                               (let loop ()
                                 (if (fxzero? active)
                                     iter-end
                                     (let* ([slot index]
                                            [value (iter-next! (vector-ref iterators slot))])
                                       ;; The callback may resume after later pulls. Read links
                                       ;; now, and never unlink or decrement an old slot twice.
                                       (cond [(fxzero? active) iter-end]
                                             [(not (vector-ref live-slots slot)) (loop)]
                                             [else
                                              (let ([next (vector-ref next-slots slot)])
                                                (set! index next)
                                                (if (iter-end? value)
                                                    (let ([previous
                                                           (vector-ref previous-slots slot)])
                                                      (vector-set! live-slots slot #f)
                                                      (set! active (fx1- active))
                                                      (vector-set! next-slots previous next)
                                                      (vector-set! previous-slots next previous)
                                                      (loop))
                                                    value))])))))
                             reset-local!
                             #f (lambda () sources))))))))
  #|proc:iter-concat
  Return a lazy sequential wrapper over `iter` and all iterators in `iter*`.
  A first occurrence starts at its current position; before a later occurrence of the
  same handle, reset it to the beginning, retaining its configured pipeline. For example,
  `(iter-concat lines lines)` reads the remaining file, then the whole file. Sources must
  support reset for repeated occurrences; nonseekable streams cannot be restarted.
  Switching sources never finalizes them. Reset resets sources and restarts the wrapper;
  terminal completion finalizes the chain, closing repeated resource handles only once.
  |#
  (define iter-concat
    (lambda (iter . iter*)
      (pcheck ([$iter? iter] [all-iters? iter*])
              (let* ([sources (cons iter iter*)] [remaining sources] [seen (make-eq-hashtable)])
                (hashtable-set! seen iter #t)
                (mk-$iter
                 (lambda ()
                   (let loop ()
                     (if (null? remaining)
                         iter-end
                         (let ([x (iter-next! (car remaining))])
                           (if (iter-end? x)
                               (begin
                                 (set! remaining (cdr remaining))
                                 (unless (null? remaining)
                                   (let ([next (car remaining)])
                                     (if (hashtable-ref seen next #f) (iter-reset! next)
                                         (hashtable-set! seen next #t))))
                                 (loop))
                               x)))))
                 (lambda ()
                   (set! remaining sources)
                   (hashtable-clear! seen)
                   (hashtable-set! seen iter #t))
                 #f (lambda () sources))))))
  #|proc:iter-distinct
  Return an iterator that emits the first value from `iter` for each equivalence
  class according to `(equal? previous current) -> boolean`. Kind: lazy wrapper with
  per-pass history of emitted values; reset clears history and resets the source.
  Terminal completion finalizes upstream resources. History may grow without bound.
  |#
  (define iter-distinct
    (lambda (equal? iter)
      (pcheck ([procedure? equal?] [$iter? iter])
              (let ([seen '()])
                (mk-$iter
                 (lambda ()
                   (let loop ()
                     (let ([x (iter-next! iter)])
                       (if (iter-end? x)
                           iter-end
                           (if (exists (lambda (y) (equal? y x)) seen)
                               (loop)
                               (begin (set! seen (cons x seen)) x))))))
                 (lambda () (set! seen '()))
                 #f (lambda () (list iter)))))))
  #|proc:iter-sorted
  Consume `iter` immediately, sort its values with `(<? left right) -> boolean`, and
  return a cursor over that snapshot. Kind: eager buffering wrapper requiring finite input.
  Reset resets the source and rebuilds the snapshot. Within a shared graph, each upstream
  cursor resets once; eager rebuilds consume that shared position in dependency order.
  Use distinct source cursors for independent snapshots. Construction and rebuilding leave
  resources open; a successful terminal or explicit finalization closes the chain.
  |#
  (define iter-sorted
    (lambda (<? iter)
      (pcheck ([procedure? <?] [$iter? iter])
              (let ([snapshot '#()] [index 0])
                (define rebuild!
                  (lambda ()
                    (let ([builder (make-list-builder)])
                      (let collect ()
                        (let ([value (iter-next! iter)])
                          (unless (iter-end? value)
                            (builder value)
                            (collect))))
                      (set! snapshot (list->vector (builder))))
                    (vsort! <? snapshot)
                    (set! index 0)))
                (rebuild!)
                (mk-$iter
                 (lambda () (if (fx>= index (vector-length snapshot))
                                iter-end
                                (let ([x (vector-ref snapshot index)])
                                  (set! index (fx1+ index)) x)))
                 rebuild!
                 #f (lambda () (list iter)))))))

;;; terminal ops

  #|proc:iter-for-each
  Call `proc` once for each value from `iter`, finalize `iter`, and return an
  unspecified value. The procedure `proc` accepts one iterator value.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter-for-each
    (lambda (proc iter)
      (pcheck ([$iter? iter] [procedure? proc])
              (let iter-loop ()
                (let ([x (iter-next! iter)])
                  (if (eq? x iter-end)
                      (iter-finalize! iter)
                      (begin (proc x)
                             (iter-loop))))))))
  #|proc:iter-fold
  Fold `iter` from left to right with initial accumulator `acc`, finalize the
  iterator, and return the final accumulator. The procedure `proc` accepts the
  current accumulator and one value and returns the next accumulator, with signature
  `(accumulator value) -> accumulator`. Kind: consuming terminal; finalization follows success.
  |#
  (define iter-fold
    (lambda (proc acc iter)
      (pcheck ([procedure? proc] [$iter? iter])
              (let iter-loop ([acc acc])
                (let ([x (iter-next! iter)])
                  (if (eq? x iter-end)
                      (begin (iter-finalize! iter) acc)
                      (iter-loop (proc acc x))))))))

  ;; A separate sentinel allows #f to participate as ordinary comparison data.
  (define extrema-empty (gensym "extrema-empty"))
  (define iter-extreme
    (lambda (precedes? iterator)
      (let ([result (iter-fold
                     (lambda (best value)
                       (if (or (eq? best extrema-empty) (precedes? value best)) value best))
                     extrema-empty iterator)])
        (if (eq? result extrema-empty) #f result))))

  #|proc:iter-max
  Return the maximal value from `iterator`, or `#f` when empty, and finalize its chain.
  Optional `greater?` has signature `(candidate current-best) -> boolean`; true selects
  the candidate. It defaults to `>`. A stored `#f` is valid comparison data.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter-max
    (case-lambda
      [(iterator) (iter-max > iterator)]
      [(greater? iterator)
       (pcheck ([procedure? greater?] [$iter? iterator]) (iter-extreme greater? iterator))]))

  #|proc:iter-min
  Return the minimal value from `iterator`, or `#f` when empty, and finalize its chain.
  Optional `less?` has signature `(candidate current-best) -> boolean`; true selects
  the candidate. It defaults to `<`. A stored `#f` is valid comparison data.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter-min
    (case-lambda
      [(iterator) (iter-min < iterator)]
      [(less? iterator)
       (pcheck ([procedure? less?] [$iter? iterator]) (iter-extreme less? iterator))]))
  #|proc:iter-avg
  Return the arithmetic mean of numeric values from `iter`, or `#f` when empty.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter-avg
    (lambda (iter)
      (let loop ([i 0] [sum 0])
        (let ([x (iter-next! iter)])
          (if (iter-end? x)
              (begin (iter-finalize! iter) (if (= i 0) #f (/ sum i)))
              (loop (add1 i) (+ x sum)))))))
  #|proc:iter-sum
  Return the numeric sum of values from `iter` and finalize the iterator.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter-sum
    (lambda (iter)
      (iter-fold (lambda (acc x) (+ acc x)) 0 iter)))
  #|proc:iter-product
  Return the numeric product of values from `iter` and finalize the iterator.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter-product
    (lambda (iter)
      (iter-fold (lambda (acc x) (* acc x)) 1 iter)))

  (define iter-fxmax
    (lambda (iter)
      (iter-max fx> iter)))
  (define iter-fxmin
    (lambda (iter)
      (iter-min fx< iter)))
  #|proc:iter-fxavg
  Return the fixnum arithmetic mean of values from `iter`, or `#f` when empty.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter-fxavg
    (lambda (iter)
      (let loop ([i 0] [sum 0])
        (let ([x (iter-next! iter)])
          (if (iter-end? x)
              (begin (iter-finalize! iter) (if (fx= i 0) #f (fx/ sum i)))
              (loop (add1 i) (fx+ x sum)))))))
  #|proc:iter-fxsum
  Return the fixnum sum of values from `iter` and finalize the iterator.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter-fxsum
    (lambda (iter)
      (iter-fold (lambda (acc x) (fx+ acc x)) 0 iter)))
  #|proc:iter-fxproduct
  Return the fixnum product of values from `iter` and finalize the iterator.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter-fxproduct
    (lambda (iter)
      (iter-fold (lambda (acc x) (fx* acc x)) 1 iter)))

  (define iter-flmax
    (lambda (iter)
      (iter-max fl> iter)))
  (define iter-flmin
    (lambda (iter)
      (iter-min fl< iter)))
  #|proc:iter-flavg
  Return the flonum arithmetic mean of values from `iter`, or `#f` when empty.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter-flavg
    (lambda (iter)
      (let loop ([i 0] [sum 0.0])
        (let ([x (iter-next! iter)])
          (if (iter-end? x)
              (begin (iter-finalize! iter) (if (fx= i 0) #f (fl/ sum (inexact i))))
              (loop (add1 i) (fl+ x sum)))))))
  #|proc:iter-flsum
  Return the flonum sum of values from `iter` and finalize the iterator.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter-flsum
    (lambda (iter)
      (iter-fold (lambda (acc x) (fl+ acc x)) 0.0 iter)))
  #|proc:iter-flproduct
  Return the flonum product of values from `iter` and finalize the iterator.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter-flproduct
    (lambda (iter)
      (iter-fold (lambda (acc x) (fl* acc x)) 1.0 iter)))

  ;; TODO type-specialized ops


;;; conversions

  #|proc:iter->list
  Collect values from `iter` into a list in traversal order, finalize `iter`,
  and return the list.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter->list
    (lambda (iter)
      (pcheck ([$iter? iter])
              (let ([lb (make-list-builder)])
                (let iter-loop ()
                  (let ([x (iter-next! iter)])
                    (if (eq? x iter-end)
                        (begin (iter-finalize! iter)
                               (lb))
                        (begin (lb x)
                               (iter-loop)))))))))
  #|proc:iter->vector
  Collect values from `iter` into a vector in traversal order and finalize `iter`.
  Kind: consuming terminal; successful completion finalizes the resource chain.
  |#
  (define iter->vector
    (lambda (iter)
      (pcheck ([$iter? iter])
              (let ([xs (iter->list iter)])
                (list->vector xs)))))





  )
