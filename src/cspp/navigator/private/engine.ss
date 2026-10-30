(library (cspp navigator private engine)
  (export nav-select nav-select-one nav-select-first nav-select-count nav-selected?
          nav-traverse nav-traverse/i nav-traverse->iter
          nav-transform nav-transform/i nav-transform! nav-transform!/i
          nav-setval nav-setval! nav-clearval nav-clearval!
          nav-selected->list nav-selected->vector nav-selected-transduce
          select-path transform-path transform-path! clear-path clear-path!)
  (import (chezscheme)
          (cspp iter)
          (cspp list)
          (cspp transducer)
          (cspp utils)
          (cspp navigator private core)
          (cspp navigator private data))

  ;; An environment is a stack of recursive-name binding frames. Each resolved
  ;; reference uses its defining (lexical) environment, rather than whichever group
  ;; happens to be active when an outer suffix or delegated callback resumes.
  (define current-recursive-env (make-nav-scope-parameter '()))

  ;; Wrapped paths inherit whether their result has an outer selection suffix.
  ;; Public selection entry points start a fresh context, including nested public calls.
  ;; Terminal means no suffix remains anywhere in the composed traversal. This
  ;; affects nav/children's terminal tree selection, rather than just the current path.
  (define current-selection-terminal? (make-nav-scope-parameter #t))

  ;; A custom step may delegate through a fresh public operation. Its engine
  ;; continuation still belongs to the defining traversal, including optional
  ;; sibling handling, recursive references, and conditional/default evidence.
  ;; Capture the engine callback's dynamic scope at definition time: a custom step
  ;; can invoke it inside nav-transform/nav-traverse, whose fresh public scope must
  ;; not erase the outer path's bindings, optional handling, or update counter.
  ;; The equality fast path avoids parameterization when no scope changed.
  ;; Checkpoints belong inside the restored scope so delegated custom operations
  ;; cannot reset a different public operation's terminal evidence. Pass the
  ;; captured counter to avoid another lookup on every child. Focus evidence is
  ;; checkpointed at path entry and user callbacks; a returning conditional suffix
  ;; reasserts its focus, so engine children need only default-update checkpoints.
  (define bind-continuation
    (lambda (continue)
      (let ([env (current-recursive-env)]
            [optional? (current-optional-focus?)]
            [counter (current-update-count)]
            [focus-scope (current-focus-scope)]
            [terminal? (current-selection-terminal?)])
        (lambda (value)
          (let* ([active (current-focus-scope)]
                 ;; Inner conditions in this operation remain active until this
                 ;; suffix returns. A fresh public operation has another token;
                 ;; only that boundary needs to reinstall the defining focus scope.
                 [scope (if (eq? (car active) (car focus-scope)) active focus-scope)])
            (if (and (eq? env (current-recursive-env))
                     (eq? optional? (current-optional-focus?))
                     (eq? counter (current-update-count))
                     (eq? scope active)
                     (eq? terminal? (current-selection-terminal?)))
                (nav-with-evidence-checkpoint counter '() (lambda () (continue value)))
                (parameterize ([current-recursive-env env]
                               [current-optional-focus? optional?]
                               [current-update-count counter]
                               [current-focus-scope scope]
                               [current-selection-terminal? terminal?])
                  (nav-with-evidence-checkpoint counter '()
                                                (lambda () (continue value))))))))))

  (define emit-descendant-children
    (lambda (value emit)
      (emit-children
       value
       (lambda (child)
         (emit child)
         (when (child-container? child)
           (emit-descendant-children child emit))))))

  (define emit-nav-children-tree
    (lambda (value emit)
      (emit-children
       value
       (lambda (child)
         (when (child-container? child)
           (emit child))))
      (emit-children
       value
       (lambda (child)
         (when (child-container? child)
           (emit-descendant-children child emit))))))

  (define nav-children-step?
    (lambda (step)
      (and ($navigator? step)
           (eq? ($navigator-kind step) 'recursive)
           (eq? ($navigator-name step) 'nav/children))))

  ;; Bound references carry a compiled body and its defining lexical environment.
  ;; Symbolic references search captured frames, whose entries are stable references.
  (define resolve-nav-ref
    (lambda (who step)
      (or ($nav-ref-target step)
          (let loop ([env (current-recursive-env)])
            (if (null? env)
                (nav-error who "unresolved recursive reference: ~s" ($nav-ref-name step))
                (let ([binding (assq ($nav-ref-name step) (car env))])
                  (if binding
                      (or ($nav-ref-target (cdr binding))
                          (nav-error who "uninitialized recursive reference: ~s"
                                     ($nav-ref-name step)))
                      (loop (cdr env)))))))))

  ;; The engine keeps two path cursors. `steps` is the path segment currently
  ;; being executed. `tails` is a stack of suspended outer suffixes. When a step
  ;; expands to an inner path, recursive group, or recursive reference, the
  ;; current `rest` must resume after that inner path finishes, so it is pushed
  ;; onto `tails`, together with its recursive environment. Restoring that environment
  ;; before running an outer suffix keeps nested recursive bindings lexically scoped.
  (define push-tail
    (lambda (rest tails)
      (cons (cons (current-recursive-env) rest) tails)))

  (define run-select
    (lambda (steps tails value emit)
      (if (null? steps)
          (if (null? tails)
              (emit value)
              (nav-with-scope ([current-recursive-env (caar tails)])
                (run-select (cdar tails) (cdr tails) value emit)))
          (let ([step (car steps)] [rest (cdr steps)])
            (cond [($navigator? step)
                   (let* ([outer-terminal? (current-selection-terminal?)]
                          [terminal? (and outer-terminal? (null? rest)
                                          (for-all (lambda (tail) (null? (cdr tail))) tails))])
                     (if (and terminal? (nav-children-step? step))
                         (emit-nav-children-tree value emit)
                         (nav-with-scope ([current-selection-terminal? terminal?])
                           (nav-call-step
                            ($navigator-select-proc step) value
                            (bind-continuation
                             (lambda (child)
                               (nav-with-scope ([current-selection-terminal? outer-terminal?])
                                 (run-select rest tails child emit))))
                            (void)))))]
                  [($recursive-nav-group? step)
                   (let ([tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env ($recursive-nav-group-env step)])
                       (run-select (nav-path-steps (nav-compile-path
                                                    ($recursive-nav-group-root step)))
                                   tails
                                   value
                                   emit)))]
                  [($nav-ref? step)
                   (let ([resolved (resolve-nav-ref 'nav-select step)]
                         [tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env (cdr resolved)])
                       (run-select (nav-path-steps (nav-compile-path (car resolved)))
                                   tails value emit)))]
                  [($path? step)
                   (run-select ($path-steps step) (push-tail rest tails) value emit)]
                  [else (nav-error 'nav-select "unsupported path step: ~s" step)])))))

  (define select-path
    (case-lambda
      [(path data emit)
       (nav-with-evidence-checkpoint
        (current-update-count) (cdr (current-focus-scope))
        (lambda () (run-select (nav-path-steps (nav-compile-path path)) '() data emit)))]
      [(path data emit terminal?)
       (nav-with-scope ([current-selection-terminal? terminal?])
         (select-path path data emit))]))

  (define run-transform
    (lambda (steps tails value update)
      (if (null? steps)
          (if (null? tails)
              (update value)
              (nav-with-scope ([current-recursive-env (caar tails)])
                (run-transform (cdar tails) (cdr tails) value update)))
          (let ([step (car steps)] [rest (cdr steps)])
            (cond [($navigator? step)
                   (nav-call-step ($navigator-transform-proc step)
                    value
                    (bind-continuation
                     (lambda (child)
                       (run-transform rest tails child update))) value)]
                  [($recursive-nav-group? step)
                   (let ([tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env ($recursive-nav-group-env step)])
                       (run-transform (nav-path-steps (nav-compile-path
                                                       ($recursive-nav-group-root step)))
                                      tails
                                      value
                                      update)))]
                  [($nav-ref? step)
                   (let ([resolved (resolve-nav-ref 'nav-transform step)]
                         [tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env (cdr resolved)])
                       (run-transform (nav-path-steps (nav-compile-path (car resolved)))
                                   tails value update)))]
                  [($path? step)
                   (run-transform ($path-steps step) (push-tail rest tails) value update)]
                  [else (nav-error 'nav-transform "unsupported path step: ~s" step)])))))

  (define transform-path
    (lambda (path proc data)
      (nav-with-evidence-checkpoint
       (current-update-count) (cdr (current-focus-scope))
       (lambda ()
         (run-transform (nav-path-steps (nav-compile-path path)) '() data proc)))))

  (define run-transform!
    (lambda (steps tails value update!)
      (if (null? steps)
          (if (null? tails)
              (update! value)
              (nav-with-scope ([current-recursive-env (caar tails)])
                (run-transform! (cdar tails) (cdr tails) value update!)))
          (let ([step (car steps)] [rest (cdr steps)])
            (cond [($navigator? step)
                   (nav-call-step ($navigator-transform!-proc step)
                    value
                    (bind-continuation
                     (lambda (child)
                       (run-transform! rest tails child update!))) value)]
                  [($recursive-nav-group? step)
                   (let ([tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env ($recursive-nav-group-env step)])
                       (run-transform! (nav-path-steps (nav-compile-path
                                                        ($recursive-nav-group-root step)))
                                       tails
                                       value
                                       update!)))]
                  [($nav-ref? step)
                   (let ([resolved (resolve-nav-ref 'nav-transform! step)]
                         [tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env (cdr resolved)])
                       (run-transform! (nav-path-steps (nav-compile-path (car resolved)))
                                   tails value update!)))]
                  [($path? step)
                   (run-transform! ($path-steps step) (push-tail rest tails) value update!)]
                  [else (nav-error 'nav-transform! "unsupported path step: ~s" step)])))))

  (define run-clear
    (lambda (steps tails value clear)
      (if (null? steps)
          (if (null? tails)
              (clear value)
              (nav-with-scope ([current-recursive-env (caar tails)])
                (run-clear (cdar tails) (cdr tails) value clear)))
          (let ([step (car steps)] [rest (cdr steps)])
            (cond [($navigator? step)
                   (nav-call-step ($navigator-clear-proc step)
                    value
                    (bind-continuation
                     (lambda (child)
                       (run-clear rest tails child clear))) value)]
                  [($recursive-nav-group? step)
                   (let ([tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env ($recursive-nav-group-env step)])
                       (run-clear (nav-path-steps (nav-compile-path
                                                   ($recursive-nav-group-root step)))
                                  tails
                                  value clear)))]
                  [($nav-ref? step)
                   (let ([resolved (resolve-nav-ref 'nav-clearval step)]
                         [tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env (cdr resolved)])
                       (run-clear (nav-path-steps (nav-compile-path (car resolved)))
                                   tails value clear)))]
                  [($path? step)
                   (run-clear ($path-steps step) (push-tail rest tails) value clear)]
                  [else (nav-error 'nav-clearval "unsupported path step: ~s" step)])))))

  (define run-clear!
    (lambda (steps tails value clear!)
      (if (null? steps)
          (if (null? tails)
              (clear! value)
              (nav-with-scope ([current-recursive-env (caar tails)])
                (run-clear! (cdar tails) (cdr tails) value clear!)))
          (let ([step (car steps)] [rest (cdr steps)])
            (cond [($navigator? step)
                   (nav-call-step ($navigator-clear!-proc step)
                    value
                    (bind-continuation
                     (lambda (child)
                       (run-clear! rest tails child clear!))) value)]
                  [($recursive-nav-group? step)
                   (let ([tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env ($recursive-nav-group-env step)])
                       (run-clear! (nav-path-steps (nav-compile-path
                                                    ($recursive-nav-group-root step)))
                                   tails
                                   value clear!)))]
                  [($nav-ref? step)
                   (let ([resolved (resolve-nav-ref 'nav-clearval! step)]
                         [tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env (cdr resolved)])
                       (run-clear! (nav-path-steps (nav-compile-path (car resolved)))
                                   tails value clear!)))]
                  [($path? step)
                   (run-clear! ($path-steps step) (push-tail rest tails) value clear!)]
                  [else (nav-error 'nav-clearval! "unsupported path step: ~s" step)])))))

  (define transform-path!
    (lambda (path proc data)
      (nav-with-evidence-checkpoint
       (current-update-count) (cdr (current-focus-scope))
       (lambda ()
         (run-transform! (nav-path-steps (nav-compile-path path)) '() data proc)))))

  (define clear-path
    (lambda (path proc data)
      (nav-with-evidence-checkpoint
       (current-update-count) (cdr (current-focus-scope))
       (lambda () (run-clear (nav-path-steps (nav-compile-path path)) '() data proc)))))

  (define clear-path!
    (lambda (path proc data)
      (nav-with-evidence-checkpoint
       (current-update-count) (cdr (current-focus-scope))
       (lambda () (run-clear! (nav-path-steps (nav-compile-path path)) '() data proc)))))

  #|proc:nav-traverse
  The `nav-traverse` procedure calls `proc` once for each value selected by
  `path` from `data`, in traversal order. `proc` has signature `(value) -> unspecified`;
  the traversal returns an unspecified value and does not change `data` itself.
  Kind: Eager callback traversal; each call has independent traversal state.
  |#
  (define nav-traverse
    (lambda (path data proc)
      (pcheck ([procedure? proc])
              (parameterize ([current-recursive-env '()]
                             [current-focus-scope (make-nav-focus-scope)]
                             [current-update-count #f]
                             [current-optional-focus? #f])
                (select-path path data proc #t)))))

  #|proc:nav-traverse/i
  The `nav-traverse/i` procedure calls `proc` with the selected-value index and
  selected value for each value selected by `path` from `data`.
  `proc` has signature `(index value) -> unspecified`; the result is unspecified.
  Indexes start at zero for each invocation; continuation resumption does not rewind them.
  Kind: Eager callback traversal; each call has independent traversal state.
  |#
  (define nav-traverse/i
    (lambda (path data proc)
      (pcheck ([procedure? proc])
              (let ([index 0])
                (nav-traverse path data
                              (lambda (value)
                                (proc index value)
                                (set! index (+ index 1))))))))

  #|proc:nav-select
  The `nav-select` procedure returns a list of all values selected by `path`
  from `data`.
  Kind: Eager selector; each call owns its collection/counting state.
  |#
  (define nav-select
    (lambda (path data)
      (let ([builder (make-list-builder)])
        (nav-traverse path data builder)
        (builder))))

  #|proc:nav-selected->list
  The `nav-selected->list` procedure returns a list of all values selected by
  `path` from `data`.
  Kind: Eager selector; each call owns its collection/counting state.
  |#
  (define nav-selected->list nav-select)

  #|proc:nav-select-count
  The `nav-select-count` procedure returns the number of values selected by
  `path` from `data`.
  Kind: Eager selector; each call owns its collection/counting state.
  |#
  (define nav-select-count
    (lambda (path data)
      (let ([count 0])
        (nav-traverse path data
                      (lambda (value)
                        (set! count (+ count 1))))
        count)))

  #|proc:nav-selected?
  The `nav-selected?` procedure returns `#t` when `path` selects at least one
  value from `data`.
  Kind: Short-circuit selector; stops after the first focus.
  |#
  (define nav-selected?
    (lambda (path data)
      (guard [exn [(nav-absent-focus? exn) #f]]
        ;; Use a one-shot continuation to stop traversal after the first focus.
        (call/1cc
         (lambda (return)
           (nav-traverse path data (lambda (value) (return #t)))
           #f)))))

  #|proc:nav-select-first
  The `nav-select-first` procedure returns the first value selected by `path`
  from `data`, or `default` when provided and no value is selected. Without a
  default, it returns `#f` for no selection.
  Kind: Short-circuit selector; stops after the first focus.
  |#
  (define nav-select-first
    (case-lambda
      [(path data) (nav-select-first path data #f)]
      [(path data default)
       (guard [exn [(nav-absent-focus? exn) default]]
         ;; Use a one-shot continuation to return the first selected value.
         (call/1cc
          (lambda (return)
            (nav-traverse path data (lambda (value) (return value)))
            default)))]))

  #|proc:nav-select-one
  The `nav-select-one` procedure returns the only value selected by `path` from
  `data` and signals an error unless exactly one value is selected.
  Kind: Eager selector; each call owns its collection/counting state.
  |#
  (define nav-select-one
    (lambda (path data)
      (let ([count 0] [result #f])
        (nav-traverse path data
                      (lambda (value)
                        (set! count (+ count 1))
                        (set! result value)))
        (if (= count 1)
            result
            (nav-error 'nav-select-one "expected one selected value, got ~s" count)))))

  #|proc:nav-traverse->iter
  The `nav-traverse->iter` procedure returns an iterator over the values
  selected by `path` from `data`. Traversal errors are raised by the advance that
  encounters them. After an error the iterator is exhausted until reset.
  `iter-reset!` restarts the path against the current `data` using fresh private traversal state.
  Exhaustion returns `iter-end`. The cursor owns no file/port, so finalization is a no-op;
  terminal consumption does not prevent a later reset. Traversal callbacks run on demand.
  Separate cursors over a pure path and unchanged data may run concurrently; operations on
  the same cursor are dependent. Continuation resumption does not rewind its mutable cursor.
  A producer continuation is a Scheme continuation captured in a navigation callback during
  a pull. Detached resumption invokes it between completed pulls, including after exhaustion.
  This is unsupported: no active consumer is waiting for its output. A transfer then raises
  a `nav-traverse->iter` error.
  Ordinary fiber suspension during a pull preserves its consumer destination. Resuming after
  reset or a nonlocal exit has undefined cursor state, as for other dependent lifecycle changes.
  A suspended producer retains its traversal stack, which can include the first consumer's
  calling context, until completion or reset. After completion or failure the cursor retains
  no consumer continuation.
  Kind: Lazy iterator bridge; each returned iterator has its own destructive cursor.
  |#
  (define nav-traverse->iter
    (lambda (path data)
      ;; `resume` is the producer continuation after its last emitted focus;
      ;; `return` is the consumer continuation for the current iter-next! call.
      ;; Each yield saves the next producer segment and returns one focus. Each
      ;; advance replaces return before resuming, so later errors belong to the
      ;; current consumer. Clear return after transferring back, on both yield
      ;; and completion. A live resume can still retain its original stack tail.
      ;; `failure` carries a producer condition across the transfer; it is
      ;; re-raised outside the suspended producer's handler.
      (let ([compiled (nav-compile-path path)]
            [resume #f] [return #f] [done? #f] [failure '()])
        (define return-to-consumer
          (lambda (value)
            ;; A completed pull clears its consumer to release arbitrary caller data.
            ;; A saved producer continuation restores control, not this mutable slot;
            ;; reject detached transfers instead of calling #f or retaining old callers.
            ;; This check also covers completion and exception transfers. It requires
            ;; no wind hook: ordinary fiber suspension keeps the live pull's consumer.
            (if return
                (return value)
                (nav-error 'nav-traverse->iter
                           "producer continuation resumed without an active iterator pull"))))
        (define reset!
          (lambda ()
            (set! return #f)
            (set! done? #f)
            (set! failure '())
            (set! resume
                  (lambda ()
                    (guard (exn [else
                                 (set! failure (list exn))
                                 (set! done? #t)
                                 (set! resume #f)
                                 (return-to-consumer iter-end)])
                      (parameterize ([current-recursive-env '()]
                                     [current-focus-scope (make-nav-focus-scope)]
                                     [current-update-count #f]
                                     [current-optional-focus? #f])
                        (select-path
                         compiled data
                         (lambda (value)
                           ;; Suspend each producer segment once, until the next advance.
                           (call/1cc
                            (lambda (continue)
                              (set! resume continue)
                              (return-to-consumer value)))) #t))
                      (set! done? #t)
                      (set! resume #f)
                      (return-to-consumer iter-end))))))
        (reset!)
        (make-iter
         (lambda ()
           (if done?
               iter-end
               (let ([value
                      ;; Each advance supplies its own one-shot consumer continuation.
                      (call/1cc
                       (lambda (consumer)
                         (set! return consumer)
                         (resume)))])
                 ;; The transfer consumed this continuation. Keeping it would retain
                 ;; arbitrary data on the old consumer's stack, even after exhaustion.
                 (set! return #f)
                 ;; Raise outside the suspended producer so the current caller handles it.
                 (if (null? failure)
                     value
                     (let ([exn (car failure)])
                       (set! failure '())
                       (raise exn))))))
         reset!))))

  #|proc:nav-selected->vector
  The `nav-selected->vector` procedure returns a vector of all values selected
  by `path` from `data`.
  Kind: Eager selector; each call owns its collection/counting state.
  |#
  (define nav-selected->vector
    (lambda (path data)
      (let ([items (make-vector 8)] [count 0])
        (define grow!
          (lambda ()
            (let* ([old-len (vector-length items)]
                   [new-items (make-vector (* old-len 2))])
              (let loop ([i 0])
                (when (< i old-len)
                  (vector-set! new-items i (vector-ref items i))
                  (loop (+ i 1))))
              (set! items new-items))))
        (nav-traverse path data
                     (lambda (value)
                       (when (= count (vector-length items))
                         (grow!))
                       (vector-set! items count value)
                       (set! count (+ count 1))))
        (let ([out (make-vector count)])
          (let loop ([i 0])
            (when (< i count)
              (vector-set! out i (vector-ref items i))
              (loop (+ i 1))))
          out))))

  #|proc:nav-selected-transduce
  The `nav-selected-transduce` procedure applies transducer record `xform` and reducer
  record `reducer` to values selected by `path` from `data`, returning the completed
  reduction result. The four-argument form uses the reducer's initializer; the
  five-argument form uses `init`. Traversal stops when the reduction terminates early.
  Successful completion finalizes the private iterator chain according to the terminal
  reduction contract. The selection cursor owns no external resource.
  Kind: Eager reduction bridge; creates a fresh selection cursor for this reduction.
  |#
  (define nav-selected-transduce
    (case-lambda
      [(path xform reducer data)
       (pcheck ([transducer? xform] [reducer? reducer])
               (transduce xform reducer (nav-traverse->iter path data)))]
      [(path xform reducer init data)
       (pcheck ([transducer? xform] [reducer? reducer])
               (transduce xform reducer init (nav-traverse->iter path data)))]))

  #|proc:nav-transform
  The `nav-transform` procedure returns a rebuilt `data` value with each value
  focused by `path` replaced by the result of applying `proc` to it.
  `proc` has signature `(old-value) -> new-value`. Persistent rebuilding does not undo
  callback effects or make a mutating callback pure. Internal update tracking is per call.
  Kind: Persistent transform; rebuilds focused ancestry and may share unchanged children.
  |#
  (define nav-transform
    (lambda (path proc data)
      (pcheck ([procedure? proc])
              (parameterize ([current-update-count (cons 0 0)]
                             [current-recursive-env '()]
                             [current-focus-scope (make-nav-focus-scope)]
                             [current-optional-focus? #f])
                (transform-path
                 path
                 (lambda (value)
                   (let ([result (proc value)])
                     (nav-note-update!)
                     result))
                 data)))))

  #|proc:nav-transform/i
  The `nav-transform/i` procedure is like `nav-transform`, but `proc` receives
  the selected-value index before the selected value.
  `proc` has signature `(index old-value) -> new-value`; indexes start at zero per call.
  Continuation resumption does not restore the mutable index counter.
  Kind: Persistent transform; rebuilds focused ancestry and may share unchanged children.
  |#
  (define nav-transform/i
    (lambda (path proc data)
      (pcheck ([procedure? proc])
              (let ([index 0])
                (nav-transform path
                               (lambda (value)
                                 (let ([new-value (proc index value)])
                                   (set! index (+ index 1))
                                   new-value))
                               data)))))

  #|proc:nav-transform!
  The `nav-transform!` procedure mutates locations focused by `path` in `data`
  by applying `proc` to each old value, then returns `data`.
  `proc` has signature `(old-value) -> new-value`. The root object must remain identical;
  an unsupported replacement raises an error. Errors may leave partial mutations.
  Kind: Destructive transform; conflicting accesses to the same data require caller ordering.
  |#
  (define nav-transform!
    (lambda (path proc data)
      (pcheck ([procedure? proc])
              (let ([result (parameterize ([current-update-count (cons 0 0)]
                                           [current-recursive-env '()]
                                           [current-focus-scope (make-nav-focus-scope)]
                                           [current-optional-focus? #f])
                              (transform-path! path
                                               (lambda (value)
                                                 (let ([result (proc value)])
                                                   (nav-note-update!)
                                                   result))
                                               data))])
                (if (eq? result data)
                    data
                    (nav-error 'nav-transform! "cannot replace root focus in place: ~s"
                               data))))))

  #|proc:nav-transform!/i
  The `nav-transform!/i` procedure is like `nav-transform!`, but `proc`
  receives the selected-value index before the selected value.
  `proc` has signature `(index old-value) -> new-value`; the result is the original `data`.
  Continuation resumption does not restore the mutable index counter or earlier mutations.
  Kind: Destructive transform; conflicting accesses to the same data require caller ordering.
  |#
  (define nav-transform!/i
    (lambda (path proc data)
      (pcheck ([procedure? proc])
              (let ([index 0])
                (nav-transform! path
                                (lambda (value)
                                  (let ([new-value (proc index value)])
                                    (set! index (+ index 1))
                                    new-value))
                                data)))))

  #|proc:nav-setval
  The `nav-setval` procedure purely replaces each value focused by `path` with
  `value` in `data`, returning the rebuilt root.
  Kind: Persistent transform; rebuilds focused ancestry and may share unchanged children.
  |#
  (define nav-setval
    (lambda (path value data)
      (nav-transform path (lambda (old) value) data)))

  #|proc:nav-setval!
  The `nav-setval!` procedure mutates each value focused by `path` to `value`
  in `data`, then returns `data`.
  Kind: Destructive transform; conflicting accesses to the same data require caller ordering.
  |#
  (define nav-setval!
    (lambda (path value data)
      (nav-transform! path (lambda (old) value) data)))

  #|proc:nav-clearval
  The `nav-clearval` procedure removes or clears values focused by `path` from
  `data` when the final navigator supports removal, returning the rebuilt root.
  Removing the input root itself is unsupported.
  Kind: Persistent transform; rebuilds focused ancestry and may share unchanged children.
  |#
  (define nav-clearval
    (lambda (path data)
      (let ([result (parameterize ([current-recursive-env '()]
                                   [current-focus-scope (make-nav-focus-scope)]
                                   [current-update-count #f]
                                   [current-optional-focus? #f])
                      (clear-path path (lambda (value) nav-missing) data))])
        (if (nav-missing? result)
            (nav-error 'nav-clearval "cannot remove root value")
            result))))

  #|proc:nav-clearval!
  The `nav-clearval!` procedure mutates `data` to remove or clear values
  focused by `path` when the final navigator supports removal, then returns `data`.
  Removing the input root itself is unsupported; errors may leave partial mutations.
  Kind: Destructive transform; conflicting accesses to the same data require caller ordering.
  |#
  (define nav-clearval!
    (lambda (path data)
      (let ([result (parameterize ([current-recursive-env '()]
                                   [current-focus-scope (make-nav-focus-scope)]
                                   [current-update-count #f]
                                   [current-optional-focus? #f])
                      (clear-path! path (lambda (value) nav-missing) data))])
        (if (nav-missing? result)
            (nav-error 'nav-clearval! "cannot remove root value")
            result)))))
