(library (cspp navigator private core)
  (export $navigator? $path? $recursive-nav-group? $nav-ref?
          make-$navigator make-$path make-$recursive-nav-group make-$nav-ref
          $navigator-name $navigator-kind $navigator-metadata
          $navigator-select-proc $navigator-transform-proc $navigator-transform!-proc
          $navigator-clear-proc $navigator-clear!-proc
          $path-steps $recursive-nav-group-bindings $recursive-nav-group-root
          $recursive-nav-group-env
          $nav-ref-name $nav-ref-target $nav-ref-target-set!
          nav? nav-path? nav/empty-path nav-empty-path?
          make-nav nav-name nav-coerce nav/path nav-compile-path nav/path-append
          make-nav-ref nav-ref? nav-missing nav-missing?
          make-nav-scope-parameter nav-with-scope current-update-count
          make-nav-focus-scope current-focus-scope nav-with-evidence-checkpoint nav-note-update!
          current-optional-focus? nav-call-step
          nav-error nav-focus-error nav-focus-error? nav-absent-focus?
          nav-call-user nav-call-protected nav-protect-callback
          nav-unsupported-error nav-required-error
          nav-path-steps nav-normalize-step nav-debug-name-core nav->datum-core
          install-nav-key-proc! install-nav-nth-proc!)
  (import (chezscheme)
          (cspp utils))

  (define-record-type ($navigator make-$navigator $navigator?)
    (fields (immutable name)
            (immutable kind)
            (immutable metadata)
            (immutable select-proc)
            (immutable transform-proc)
            (immutable transform!-proc)
            (immutable clear-proc)
            (immutable clear!-proc))
    (opaque #t)
    (sealed #t))

  (define-record-type ($path make-$path $path?)
    (fields (immutable steps))
    (opaque #t)
    (sealed #t))

  (define-record-type ($recursive-nav-group make-$recursive-nav-group $recursive-nav-group?)
    (fields (immutable bindings)
            (immutable root)
            (immutable env))
    (opaque #t)
    (sealed #t))

  (define-record-type ($nav-ref make-$nav-ref $nav-ref?)
    (fields (immutable name) (mutable target))
    (opaque #t)
    (sealed #t))

;;;;===----------------------------------------------------------------------===
;;;; Reusable paths and invocation scope
;;;;===----------------------------------------------------------------------===

  ;; API kinds: a reusable step describes how to focus children; a path composes
  ;; steps without storing traversal progress. A combinator constructs a step/path
  ;; from other descriptions. A selector observes focuses; an eager traversal runs
  ;; callbacks before returning. A persistent transform rebuilds affected ancestry
  ;; and can share untouched data. A destructive transform mutates supplied data.
  ;; A lazy iterator bridge exposes one mutable cursor over a deferred traversal.
  ;; A recursive builder links stable lexical references before publishing a group.
  ;; Adapter registration publishes container behavior for later protocol lookups.
  ;; Descriptions need no reset/finalization; an active iterator needs reset to
  ;; restart. Resource ownership means responsibility for finalizing an acquired
  ;; resource (such as a file/port), not exclusive possession of a description.
  ;;
  ;; A pure path is a fully constructed path with stable configuration/captured
  ;; dependencies, side-effect-free navigation callbacks, and no shared traversal
  ;; progress. Such a path may be shared by independent threads and fibers.
  ;; Independent operations have no conflicting mutable accesses requiring an order.
  ;; Dependent operations share a mutated target, callback state, or active iterator;
  ;; their caller must order or synchronize them, including across different roots
  ;; that alias one mutable child. Paths do not serialize caller-owned mutations.
  ;;
  ;; Continuation resumption restores control/bindings, not snapshots of counters,
  ;; builders, heap updates, callback effects, or destructive source consumption.
  ;; Each public operation starts fresh scopes; delegated suffix callbacks retain
  ;; the defining operation's scopes, even inside another public operation.
  ;;
  ;; A scope parameter is private dynamic context, installed only by parameterize.
  ;; Chez thread parameters isolate simultaneous workers; parameterize's wind hooks
  ;; reinstall bindings on ordinary fiber suspension/resumption and worker migration.
  ;; The owner ID records the thread that installed a binding, NOT fiber identity.
  ;; A newly forked thread inherits a value but not its scope: an owner mismatch
  ;; reads the default, preventing inherited mutable counters or construction scopes.
  ;; Reading a binding and its owner must be indivisible with respect to timer
  ;; preemption: migration between the two reads would compare workers A and B.
  ;; Likewise, tag and install a value on the same worker. These short regions use
  ;; only private primitives; they neither call user code nor yield. Interrupt
  ;; masking prevents migration here, not simultaneous execution on other threads;
  ;; make-thread-parameter already isolates those threads' binding slots.
  #|proc:make-nav-scope-parameter
  Return a private scope parameter with initial value `default`. Calling it without an
  argument reads the active scope; calling it with one value installs a binding owned by
  the current thread. Use `parameterize` or `nav-with-scope` for scoped installation.
  |#
  (define make-nav-scope-parameter
    (lambda (default)
      (let ([binding (make-thread-parameter #f)])
        (case-lambda
          [()
           ;; This fixed read sequence cannot allocate, raise, or call user code.
           ;; Pair the primitives directly to avoid a dynamic-wind frame on every
           ;; scope lookup. Keep allocating writes in the exception-safe form below.
           (disable-interrupts)
           (let* ([entry (binding)]
                  [value (if (and entry (eqv? (car entry) (get-thread-id)))
                             (cdr entry)
                             default)])
             (enable-interrupts)
             value)]
          [(value) (with-interrupts-disabled
                     (binding (cons (get-thread-id) value)))]))))

  ;; Native thread-parameter setters synchronize with thread creation in Chez.
  ;; Avoid redundant installations while preserving wind hooks for real changes.
  #|macro:nav-with-scope
  Evaluate `body` with one `parameter` bound to `value`, avoiding redundant installation
  when the existing value is identical. Actual changes use `parameterize` for dynamic
  restoration across continuation resumption and fiber suspension.
  |#
  (define-syntax nav-with-scope
    (syntax-rules ()
      [(_ ([parameter value]) body ...)
       (let ([new-value value])
         (if (eq? (parameter) new-value)
             (begin body ...)
             (parameterize ([parameter new-value]) body ...)))]))

;;;;===----------------------------------------------------------------------===
;;;; Focus, continuation, and absence terminology
;;;;===----------------------------------------------------------------------===

  ;; A focus is a value reached by a navigator step. A path composes steps; the
  ;; suffix is the part still to run after the current step selects a child.
  ;; The engine passes each step a continuation procedure that runs this suffix.
  ;; This ordinary callback differs from the captured Scheme continuations used
  ;; by nav-traverse->iter to suspend and resume an entire traversal.
  ;;
  ;; The terminal callback runs only after the full path reaches a focus. A virtual
  ;; default focus supplies a fallback for an absent location without creating it.
  ;; Its suffix can select nothing, so visiting the default alone must not insert it.
  ;; Terminal evidence is a per-operation count of successful terminal returns.
  ;; Its private pair stores (count . active-default-depth).
  ;;
  ;; Focus evidence is a private boolean cell for one nav/must traversal or one
  ;; nav/choice candidate. It records reaching that wrapped path's callback, even
  ;; if the outer suffix selects nothing. Active cells form a dynamically scoped
  ;; stack so a suffix can checkpoint both its inner and enclosing conditions.
  ;; A focus scope is (operation-token . evidence-cells). Each public operation
  ;; creates a unique token and an empty stack. Same-operation suffixes inherit
  ;; active inner cells until callback return; callbacks delegated through a fresh
  ;; public operation restore their defining scope. Comparing tokens avoids winding
  ;; a new parameter binding for every child of a conditional. No evidence lives
  ;; in a reusable path description.
  ;;
  ;; An evidence prefix is the evidence established before entering a path/suffix,
  ;; including earlier siblings. A checkpoint saves that prefix in lexical bindings.
  ;; Resuming a completed checkpoint restores it; nested checkpoints
  ;; restore their later prefixes, retaining earlier siblings. Live fiber suspension
  ;; does not rewind evidence: only normal return marks completion. Checkpoints
  ;; allocate only inside conditionals or virtual defaults. Focus evidence needs
  ;; checkpoints at wrapped-path entry and ordinary user callbacks (predicates/getters/setters),
  ;; not every engine child: returning from the conditional callback reasserts its
  ;; reached focus. Defaults need terminal-evidence checkpoints at suffix and
  ;; ordinary callback boundaries: a setter runs after the terminal return and
  ;; must preserve that later prefix when an enclosing suffix restores its own.
  ;; Example: nav/must around a predicate saves #f on entry. An initial true
  ;; decision reaches a callback that sets #t on return; replaying the predicate
  ;; as false restores #f, so nav/must
  ;; reports no focus. A later sibling's predicate checkpoint saves #t if an earlier
  ;; sibling already selected, so replaying that later sibling preserves the prefix.
  ;; This restores bookkeeping, not targets, callback effects, or mutable builders.
  ;; Captures inside custom traversal/adapter or hash/equivalence/comparison callbacks
  ;; after earlier suffix calls are unsupported for focus/default evidence: their
  ;; opaque progress cannot be reconstructed. Backward resumption inside a path/suffix
  ;; that has not returned normally is unsupported. Overlapping branches of one
  ;; operation are dependent; their caller must order them.
  (define current-update-count (make-nav-scope-parameter #f))
  #|proc:make-nav-focus-scope
  Return a fresh private focus scope: a unique operation identity paired with an empty
  evidence-cell stack. Nested conditions share that identity and extend the stack;
  independent public invocations receive distinct identities and cells.
  |#
  (define make-nav-focus-scope
    (lambda () (cons (list #f) '())))
  (define current-focus-scope (make-nav-scope-parameter (cons #f '())))

  #|proc:nav-with-evidence-checkpoint
  Run thunk `run`, with signature `() -> values`, returning its values unchanged.
  `counter` is the defining operation's update-evidence pair or `#f`; `focuses` is its
  stack of active conditional evidence cells. Save their prefixes, tracking `counter`
  only inside virtual defaults. Continuation resumption across a completed checkpoint
  restores the saved evidence; suspension of a live checkpoint preserves its progress.
  Without active evidence, call `run` directly. Kind: private execution helper;
  no heap, source, or callback rollback.
  |#
  (define nav-with-evidence-checkpoint
    (lambda (counter focuses run)
      (let ([counter (and counter (not (zero? (cdr counter))) counter)])
        (if (and (not counter) (null? focuses))
            (run)
            (let ([prefix (and counter (car counter))]
                  [focus-prefix (map car focuses)]
                  [completed? #f])
              (call-with-values
                (lambda ()
                  (dynamic-wind
                    (lambda ()
                      (when completed?
                        (when counter (set-car! counter prefix))
                        (for-each set-car! focuses focus-prefix)
                        (set! completed? #f)))
                    run
                    void))
                (lambda results
                  ;; Mark completion only outside the winder: timer preemption can
                  ;; suspend at any instruction while its body is still live.
                  (set! completed? #t)
                  (apply values results))))))))

  #|proc:nav-note-update!
  Record one successful terminal callback return in the active operation, if any.
  Return an unspecified value. Kind: private evidence update; it owns no resources.
  |#
  (define nav-note-update!
    (lambda ()
      (let ([counter (current-update-count)])
        (when counter (set-car! counter (+ 1 (car counter)))))))

  ;; Private identity token: lookup uses it for absence, and clear continuations
  ;; return it to request removal. A retained child may instead be a rebuilt value.
  ;; #f is valid user data/default, so it cannot represent absence or removal.
  ;; Fixed-size or read-only parent locations must reject actual removal requests.
  (define nav-missing (list 'nav-missing))
  (define nav-missing? (lambda (x) (eq? x nav-missing)))

  #|proc:nav?
  The `nav?` procedure returns `#t` when `x` is a navigator record.
  |#
  (define nav? $navigator?)

  #|proc:nav-path?
  The `nav-path?` procedure returns `#t` when `x` is a compiled navigator path.
  |#
  (define nav-path? $path?)

  #|proc:nav/empty-path
  The `nav/empty-path` value is the compiled empty path that focuses the root value.
  Kind: Reusable path value; the empty composition focuses the input root.
  |#
  (define nav/empty-path (make-$path '()))

  #|proc:nav-empty-path?
  The `nav-empty-path?` procedure returns `#t` when `path` is a compiled path
  containing no steps.
  |#
  (define nav-empty-path?
    (lambda (path)
      (pcheck ([$path? path])
              (null? ($path-steps path)))))

  (define nav-error
    (lambda (who fmt . args)
      (apply errorf who fmt args)))

  (define-condition-type &nav-focus-error &error
    make-nav-focus-error nav-focus-error?)

  (define nav-focus-error
    (lambda (who fmt . args)
      (raise (condition (make-nav-focus-error)
                        (make-who-condition who)
                        (make-message-condition (apply format fmt args))))))

  ;; Optional scope means an absent/unsupported focus is skipped at the failing step,
  ;; retaining successful siblings in the same collection walk. It does not suppress
  ;; all errors: a present location that cannot be removed raises an ordinary error,
  ;; and navigation errors raised by user callbacks are marked separately below.
  (define current-optional-focus? (make-nav-scope-parameter #f))
  (define nav-call-step
    (lambda (proc value continue fallback)
      (if (current-optional-focus?)
          (guard (exn [(nav-absent-focus? exn) fallback])
            (proc value continue))
          (proc value continue))))

  (define-condition-type &nav-callback-error &condition
    make-nav-callback-error nav-callback-error?)

  (define nav-absent-focus?
    (lambda (exn)
      (and (nav-focus-error? exn) (not (nav-callback-error? exn)))))

  ;; Keep original condition components/messages, but mark callback-origin failures
  ;; so optional paths cannot mistake them for absent focuses. Engine-owned suffix
  ;; callbacks need this protection too, without installing per-child checkpoints.
  #|proc:nav-call-protected
  Apply procedure `proc` to the supplied arguments and return its values unchanged.
  Mark callback-origin absent-focus conditions so optional paths propagate them.
  `proc` accepts the supplied arguments and may return any number of values.
  Kind: private callback error boundary; it does not checkpoint focus evidence.
  |#
  (define nav-call-protected
    (lambda (proc . args)
      (guard (exn [(nav-absent-focus? exn)
                   (raise (condition exn (make-nav-callback-error)))])
        (apply proc args))))

  ;; Ordinary user callbacks are supported capture points. Save both evidence kinds
  ;; here, including a setter's successful terminal prefix. An enclosing suffix's
  ;; checkpoint predates the updater and cannot by itself restore that later prefix.
  #|proc:nav-call-user
  Apply user procedure `proc` to the supplied arguments, protecting its absent-focus
  errors and returning its values unchanged. `proc` accepts those arguments and may return
  any number of values. Checkpoint active conditional and virtual-default evidence here.
  Kind: private ordinary-callback boundary; opaque traversal callbacks use separate protection.
  |#
  (define nav-call-user
    (lambda (proc . args)
      (let ([counter (current-update-count)] [focuses (cdr (current-focus-scope))])
        (if (and (null? focuses) (or (not counter) (zero? (cdr counter))))
            (apply nav-call-protected proc args)
            (nav-with-evidence-checkpoint
             counter focuses (lambda () (apply nav-call-protected proc args)))))))

  (define nav-protect-callback
    (lambda (proc)
      (lambda args (apply nav-call-user proc args))))

  ;; Custom navigators invoke an engine continuation as well as their own code.
  ;; Preserve continuation-origin absence for an enclosing optional path to handle.
  (define nav-protect-traversal
    (lambda (proc)
      (lambda (value continue)
        (let ([downstream-condition #f])
          (guard (exn [(and (nav-absent-focus? exn)
                            (not (eq? exn downstream-condition)))
                       (raise (condition exn (make-nav-callback-error)))])
            (proc value
                  (lambda (child)
                    (guard (exn [else (set! downstream-condition exn) (raise exn)])
                      (continue child)))))))))

  (define nav-unsupported-error
    (lambda (who nav value action)
      (nav-error who "navigator ~a does not support ~a for value: ~s"
                 ($navigator-name nav) action value)))

  (define nav-required-error
    (lambda (who nav value detail)
      (nav-error who "navigator ~a failed on required focus ~s: ~a"
                 ($navigator-name nav) value detail)))

  #|proc:make-nav
  The `make-nav` procedure creates a custom navigator named by the symbol `name`.
  `select-proc` has signature `(parent emit) -> unspecified`; it calls `(emit child)`
  once per selected child, in traversal order. `transform-proc` has signature
  `(parent update) -> rebuilt-parent`; it replaces each child with `(update child)`.
  `transform!-proc` has signature `(parent update!) -> parent`; it mutates each focus
  using `(update! child)`, which returns the replacement child. The result is a navigator.
  Custom navigators created by this procedure do not support clearing. Callbacks may suspend
  in fibers. For conditional focus checks and virtual-default materialization, capturing a
  continuation inside a traversal callback after earlier suffix calls is unsupported:
  resumption cannot restore its opaque progress. Completed engine suffixes have their own
  evidence checkpoints. Capturing inside an ordinary predicate/getter is supported within
  the bounded checkpoint rules described above.
  Kind: Reusable custom step constructor; callbacks determine the supported update modes.
  |#
  (define make-nav
    (lambda (name select-proc transform-proc transform!-proc)
      (pcheck ([symbol? name] [procedure? select-proc transform-proc transform!-proc])
              (make-$navigator name 'custom `(nav-custom ,name)
                               (nav-protect-traversal select-proc)
                               (nav-protect-traversal transform-proc)
                               (nav-protect-traversal transform!-proc)
                               (lambda (value clear)
                                 (nav-error name "custom navigator cannot clear: ~s" value))
                               (lambda (value clear!)
                                 (nav-error name "custom navigator cannot clear!: ~s" value))))))

  #|proc:nav-name
  The `nav-name` procedure returns the symbolic debug name of navigator `nav`.
  |#
  (define nav-name
    (lambda (nav)
      (pcheck ([$navigator? nav])
              ($navigator-name nav))))

  #|proc:make-nav-ref
  The `make-nav-ref` procedure creates a symbolic recursive navigator reference
  named by the symbol `name`. It resolves within the defining environment of the active
  recursive group; fresh public operations start with no active bindings.
  Kind: Recursive reference constructor; its symbolic name resolves during traversal.
  |#
  (define make-nav-ref
    (lambda (name)
      (pcheck ([symbol? name])
              (make-$nav-ref name #f))))

  (define nav-ref? $nav-ref?)

  (define nav-path-steps
    (lambda (path)
      (pcheck ([$path? path])
              ($path-steps path))))

  (define nav-key-proc
    (let ([proc #f])
      (case-lambda
        [() (or proc (nav-error 'nav/key "nav/key is not installed"))]
        [(p) (set! proc p)])))

  (define nav-nth-proc
    (let ([proc #f])
      (case-lambda
        [() (or proc (nav-error 'nav/nth "nav/nth is not installed"))]
        [(p) (set! proc p)])))

  (define install-nav-key-proc!
    (lambda (proc)
      (pcheck ([procedure? proc])
              (nav-key-proc proc))))

  (define install-nav-nth-proc!
    (lambda (proc)
      (pcheck ([procedure? proc])
              (nav-nth-proc proc))))

  (define nav-normalize-step
    (lambda (x)
      (cond [($navigator? x) (list x)]
            [($path? x) ($path-steps x)]
            [($recursive-nav-group? x) (list x)]
            [($nav-ref? x) (list x)]
            [(symbol? x) (list ((nav-key-proc) x))]
            [(and (integer? x) (>= x 0)) (list ((nav-nth-proc) x))]
            [else (nav-error 'nav-compile-path "cannot coerce path step: ~s" x)])))

  #|proc:nav-coerce
  The `nav-coerce` procedure converts `x` into a navigator or path. Navigator
  and path values are returned in equivalent form, symbols become key
  navigators, and non-negative integer indexes become nth navigators.
  Kind: Reusable description conversion; no traversal is started.
  |#
  (define nav-coerce
    (lambda (x)
      (cond [($navigator? x) x]
            [($path? x) x]
            [else
             (let ([steps (nav-normalize-step x)])
               (if (and (pair? steps) (null? (cdr steps)))
                   (car steps)
                   (make-$path steps)))])))

  #|proc:nav-compile-path
  The `nav-compile-path` procedure converts path-like value `pathish` into a
  compiled path. Lists are treated as lists of path steps and nested paths are
  flattened.
  Kind: Reusable path compiler; no traversal is started.
  |#
  (define nav-compile-path
    (lambda (pathish)
      (cond [($path? pathish) pathish]
            [($navigator? pathish) (make-$path (list pathish))]
            [($recursive-nav-group? pathish) (make-$path (list pathish))]
            [($nav-ref? pathish) (make-$path (list pathish))]
            [(list? pathish)
             (make-$path (apply append (map nav-normalize-step pathish)))]
            [else (make-$path (nav-normalize-step pathish))])))

  #|proc:nav/path
  The `nav/path` procedure creates a compiled path from the path-like values
  `args`. Navigator values, path values, recursive values, symbols, and
  non-negative integer indexes are accepted; nested paths are flattened.
  Kind: Reusable path combinator; arguments are composed from left to right.
  |#
  (define nav/path
    (lambda args
      (make-$path (apply append (map nav-normalize-step args)))))

  #|proc:nav/path-append
  The `nav/path-append` procedure concatenates each path-like value in `paths`
  and returns one compiled path.
  Kind: Reusable path combinator; path arguments are concatenated in order.
  |#
  (define nav/path-append
    (lambda paths
      (make-$path
       (apply append
              (map (lambda (path) ($path-steps (nav-compile-path path)))
                   paths)))))

  (define nav-debug-name-core
    (lambda (x)
      (cond [($navigator? x) ($navigator-name x)]
            [($path? x) 'nav/path]
            [($recursive-nav-group? x) 'nav/letrec]
            [($nav-ref? x) ($nav-ref-name x)]
            [else #f])))

  (define nav->datum-core
    (lambda (x recur)
      (cond [($navigator? x) ($navigator-metadata x)]
            [($path? x) `(nav/path ,@(map recur ($path-steps x)))]
            [($nav-ref? x) `(nav-ref ,($nav-ref-name x))]
            [($recursive-nav-group? x)
             `(nav/letrec ,(map (lambda (binding)
                                  (list (car binding) (recur (cdr binding))))
                                ($recursive-nav-group-bindings x))
                          ,(recur ($recursive-nav-group-root x)))]
            [else x]))))
