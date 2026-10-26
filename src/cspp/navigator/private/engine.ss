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
          (cspp navigator private core))

  (define current-recursive-env (make-parameter #f))

  (define collection?
    (lambda (value)
      (or (pair? value)
          (vector? value)
          (fxvector? value)
          (flvector? value)
          (hashtable? value))))

  (define emit-immediate-children
    (lambda (value emit)
      (cond [(pair? value)
             (for-each emit value)]
            [(vector? value)
             (let ([len (vector-length value)])
               (let loop ([i 0])
                 (when (< i len)
                   (emit (vector-ref value i))
                   (loop (+ i 1)))))]
            [(fxvector? value)
             (let ([len (fxvector-length value)])
               (let loop ([i 0])
                 (when (< i len)
                   (emit (fxvector-ref value i))
                   (loop (+ i 1)))))]
            [(flvector? value)
             (let ([len (flvector-length value)])
               (let loop ([i 0])
                 (when (< i len)
                   (emit (flvector-ref value i))
                   (loop (+ i 1)))))]
            [(hashtable? value)
             (let ([vals (hashtable-values value)])
               (let ([len (vector-length vals)])
                 (let loop ([i 0])
                   (when (< i len)
                     (emit (vector-ref vals i))
                     (loop (+ i 1))))))]
            [else (void)])))

  (define emit-descendant-children
    (lambda (value emit)
      (emit-immediate-children
       value
       (lambda (child)
         (emit child)
         (when (collection? child)
           (emit-descendant-children child emit))))))

  (define emit-nav-children-tree
    (lambda (value emit)
      (emit-immediate-children
       value
       (lambda (child)
         (when (collection? child)
           (emit child))))
      (emit-immediate-children
       value
       (lambda (child)
         (when (collection? child)
           (emit-descendant-children child emit))))))

  (define nav-children-step?
    (lambda (step)
      (and ($navigator? step)
           (eq? ($navigator-name step) 'nav/children))))

  (define resolve-nav-ref
    (lambda (who step)
      (let ([env (current-recursive-env)])
        (if env
            (let ([binding (assq ($nav-ref-name step) ($recursive-nav-group-bindings env))])
              (if binding
                  (cdr binding)
                  (nav-error who "unresolved recursive reference: ~s" ($nav-ref-name step))))
            (nav-error who "unresolved recursive reference: ~s" ($nav-ref-name step))))))

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
              (parameterize ([current-recursive-env (caar tails)])
                (run-select (cdar tails) (cdr tails) value emit)))
          (let ([step (car steps)] [rest (cdr steps)])
            (cond [($navigator? step)
                   (if (and (null? rest) (null? tails) (nav-children-step? step))
                       (emit-nav-children-tree value emit)
                       (($navigator-select-proc step)
                        value
                        (lambda (child)
                          (run-select rest tails child emit))))]
                  [($recursive-nav-group? step)
                   (let ([tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env step])
                       (run-select (nav-path-steps (nav-compile-path
                                                    ($recursive-nav-group-root step)))
                                   tails
                                   value
                                   emit)))]
                  [($nav-ref? step)
                   (run-select (nav-path-steps (nav-compile-path
                                                (resolve-nav-ref 'nav-select step)))
                               (push-tail rest tails)
                               value
                               emit)]
                  [($path? step)
                   (run-select ($path-steps step) (push-tail rest tails) value emit)]
                  [else (nav-error 'nav-select "unsupported path step: ~s" step)])))))

  (define select-path
    (lambda (path data emit)
      (run-select (nav-path-steps (nav-compile-path path)) '() data emit)))

  (define run-transform
    (lambda (steps tails value update)
      (if (null? steps)
          (if (null? tails)
              (update value)
              (parameterize ([current-recursive-env (caar tails)])
                (run-transform (cdar tails) (cdr tails) value update)))
          (let ([step (car steps)] [rest (cdr steps)])
            (cond [($navigator? step)
                   (($navigator-transform-proc step)
                    value
                    (lambda (child)
                      (run-transform rest tails child update)))]
                  [($recursive-nav-group? step)
                   (let ([tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env step])
                       (run-transform (nav-path-steps (nav-compile-path
                                                       ($recursive-nav-group-root step)))
                                      tails
                                      value
                                      update)))]
                  [($nav-ref? step)
                   (run-transform (nav-path-steps (nav-compile-path
                                                   (resolve-nav-ref 'nav-transform step)))
                                  (push-tail rest tails)
                                  value
                                  update)]
                  [($path? step)
                   (run-transform ($path-steps step) (push-tail rest tails) value update)]
                  [else (nav-error 'nav-transform "unsupported path step: ~s" step)])))))

  (define transform-path
    (lambda (path proc data)
      (run-transform (nav-path-steps (nav-compile-path path)) '() data proc)))

  (define run-transform!
    (lambda (steps tails value update!)
      (if (null? steps)
          (if (null? tails)
              (update! value)
              (parameterize ([current-recursive-env (caar tails)])
                (run-transform! (cdar tails) (cdr tails) value update!)))
          (let ([step (car steps)] [rest (cdr steps)])
            (cond [($navigator? step)
                   (($navigator-transform!-proc step)
                    value
                    (lambda (child)
                      (run-transform! rest tails child update!)))]
                  [($recursive-nav-group? step)
                   (let ([tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env step])
                       (run-transform! (nav-path-steps (nav-compile-path
                                                        ($recursive-nav-group-root step)))
                                       tails
                                       value
                                       update!)))]
                  [($nav-ref? step)
                   (run-transform! (nav-path-steps (nav-compile-path
                                                    (resolve-nav-ref 'nav-transform! step)))
                                   (push-tail rest tails)
                                   value
                                   update!)]
                  [($path? step)
                   (run-transform! ($path-steps step) (push-tail rest tails) value update!)]
                  [else (nav-error 'nav-transform! "unsupported path step: ~s" step)])))))

  (define run-clear
    (lambda (steps tails value clear)
      (if (null? steps)
          (if (null? tails)
              (clear value)
              (parameterize ([current-recursive-env (caar tails)])
                (run-clear (cdar tails) (cdr tails) value clear)))
          (let ([step (car steps)] [rest (cdr steps)])
            (cond [($navigator? step)
                   (($navigator-clear-proc step)
                    value
                    (lambda (child)
                      (run-clear rest tails child clear)))]
                  [($recursive-nav-group? step)
                   (let ([tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env step])
                       (run-clear (nav-path-steps (nav-compile-path
                                                   ($recursive-nav-group-root step)))
                                  tails
                                  value clear)))]
                  [($nav-ref? step)
                   (run-clear (nav-path-steps (nav-compile-path
                                               (resolve-nav-ref 'nav-clearval step)))
                              (push-tail rest tails)
                              value clear)]
                  [($path? step)
                   (run-clear ($path-steps step) (push-tail rest tails) value clear)]
                  [else (nav-error 'nav-clearval "unsupported path step: ~s" step)])))))

  (define run-clear!
    (lambda (steps tails value clear!)
      (if (null? steps)
          (if (null? tails)
              (clear! value)
              (parameterize ([current-recursive-env (caar tails)])
                (run-clear! (cdar tails) (cdr tails) value clear!)))
          (let ([step (car steps)] [rest (cdr steps)])
            (cond [($navigator? step)
                   (($navigator-clear!-proc step)
                    value
                    (lambda (child)
                      (run-clear! rest tails child clear!)))]
                  [($recursive-nav-group? step)
                   (let ([tails (push-tail rest tails)])
                     (parameterize ([current-recursive-env step])
                       (run-clear! (nav-path-steps (nav-compile-path
                                                    ($recursive-nav-group-root step)))
                                   tails
                                   value clear!)))]
                  [($nav-ref? step)
                   (run-clear! (nav-path-steps (nav-compile-path
                                                (resolve-nav-ref 'nav-clearval! step)))
                               (push-tail rest tails)
                               value clear!)]
                  [($path? step)
                   (run-clear! ($path-steps step) (push-tail rest tails) value clear!)]
                  [else (nav-error 'nav-clearval! "unsupported path step: ~s" step)])))))

  (define transform-path!
    (lambda (path proc data)
      (run-transform! (nav-path-steps (nav-compile-path path)) '() data proc)))

  (define clear-path
    (lambda (path proc data)
      (run-clear (nav-path-steps (nav-compile-path path)) '() data proc)))

  (define clear-path!
    (lambda (path proc data)
      (run-clear! (nav-path-steps (nav-compile-path path)) '() data proc)))

  #|proc:nav-traverse
  The `nav-traverse` procedure calls `proc` once for each value selected by
  `path` from `data`, in traversal order.
  |#
  (define nav-traverse
    (lambda (path data proc)
      (pcheck ([procedure? proc])
              (select-path path data proc))))

  #|proc:nav-traverse/i
  The `nav-traverse/i` procedure calls `proc` with the selected-value index and
  selected value for each value selected by `path` from `data`.
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
  |#
  (define nav-select
    (lambda (path data)
      (let ([builder (make-list-builder)])
        (nav-traverse path data builder)
        (builder))))

  #|proc:nav-selected->list
  The `nav-selected->list` procedure returns a list of all values selected by
  `path` from `data`.
  |#
  (define nav-selected->list nav-select)

  #|proc:nav-select-count
  The `nav-select-count` procedure returns the number of values selected by
  `path` from `data`.
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
  |#
  (define nav-selected?
    (lambda (path data)
      (guard [exn [(nav-absent-focus? exn) #f]]
        ;; Use a one-shot continuation to stop traversal after the first focus.
        (call/1cc
         (lambda (return)
           (select-path path data (lambda (value) (return #t)))
           #f)))))

  #|proc:nav-select-first
  The `nav-select-first` procedure returns the first value selected by `path`
  from `data`, or `default` when provided and no value is selected. Without a
  default, it returns `#f` for no selection.
  |#
  (define nav-select-first
    (case-lambda
      [(path data) (nav-select-first path data #f)]
      [(path data default)
       (guard [exn [(nav-absent-focus? exn) default]]
         ;; Use a one-shot continuation to return the first selected value.
         (call/1cc
          (lambda (return)
            (select-path path data (lambda (value) (return value)))
            default)))]))

  #|proc:nav-select-one
  The `nav-select-one` procedure returns the only value selected by `path` from
  `data` and signals an error unless exactly one value is selected.
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
  |#
  (define nav-traverse->iter
    (lambda (path data)
      (let ([compiled (nav-compile-path path)]
            [resume #f] [return #f] [done? #f] [failure '()])
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
                                 (return iter-end)])
                      (select-path
                       compiled data
                       (lambda (value)
                         ;; Suspend each producer segment once, until the next advance.
                         (call/1cc
                          (lambda (continue)
                            (set! resume continue)
                            (return value)))))
                      (set! done? #t)
                      (set! resume #f)
                      (return iter-end))))))
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
                 ;; Raise outside the suspended producer so the current caller handles it.
                 (if (null? failure)
                     value
                     (let ([exn (car failure)])
                       (set! failure '())
                       (raise exn))))))
         reset!
         (lambda ()
           (set! done? #t)
           (set! resume #f)
           (set! return #f))))))

  #|proc:nav-selected->vector
  The `nav-selected->vector` procedure returns a vector of all values selected
  by `path` from `data`.
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
        (select-path path data
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
  |#
  (define nav-transform
    (lambda (path proc data)
      (pcheck ([procedure? proc])
              (transform-path path proc data))))

  #|proc:nav-transform/i
  The `nav-transform/i` procedure is like `nav-transform`, but `proc` receives
  the selected-value index before the selected value.
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
  |#
  (define nav-transform!
    (lambda (path proc data)
      (pcheck ([procedure? proc])
              (transform-path! path proc data))))

  #|proc:nav-transform!/i
  The `nav-transform!/i` procedure is like `nav-transform!`, but `proc`
  receives the selected-value index before the selected value.
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
  `value` in `data`.
  |#
  (define nav-setval
    (lambda (path value data)
      (nav-transform path (lambda (old) value) data)))

  #|proc:nav-setval!
  The `nav-setval!` procedure mutates each value focused by `path` to `value`
  in `data`, then returns `data`.
  |#
  (define nav-setval!
    (lambda (path value data)
      (nav-transform! path (lambda (old) value) data)))

  #|proc:nav-clearval
  The `nav-clearval` procedure removes or clears values focused by `path` from
  `data` when the final navigator supports removal.
  |#
  (define nav-clearval
    (lambda (path data)
      (let ([result (clear-path path (lambda (value) nav-missing) data)])
        (if (nav-missing? result)
            (nav-error 'nav-clearval "cannot remove root value")
            result))))

  #|proc:nav-clearval!
  The `nav-clearval!` procedure mutates `data` to remove or clear values
  focused by `path` when the final navigator supports removal.
  |#
  (define nav-clearval!
    (lambda (path data)
      (let ([result (clear-path! path (lambda (value) nav-missing) data)])
        (if (nav-missing? result)
            (nav-error 'nav-clearval! "cannot remove root value")
            result)))))
