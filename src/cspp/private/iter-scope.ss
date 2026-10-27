(library (cspp private iter-scope)
  (export current-iter-pull)
  (import (chezscheme))

  ;; A pull is one complete iter-next! call: producer advancement plus any map/filter
  ;; pipeline callbacks, possibly advancing the producer repeatedly to find a value.
  ;; A thunk is a zero-argument procedure representing that work.
  ;;
  ;; A pull scope is a dynamic handler with signature (pull-thunk) -> value-or-iter-end.
  ;; #f means no handler. iter-next! calls the handler around its raw implementation;
  ;; transducer drivers install handlers to distinguish source work from reducer work.
  ;; Handlers must execute the thunk once, return its value, and forward through the
  ;; previously active handler so nested reductions retain all enclosing cleanup scopes.
  ;;
  ;; A continuation captures the remaining computation and its dynamic parameters.
  ;; A generator can resume a continuation from an earlier pull. Crossing dynamic-wind
  ;; boundaries during this transfer is normal; it does not by itself prove failure.
  ;; Handlers therefore mark the pull before calling it and restore state only when it
  ;; returns normally. An escape (a nonlocal exit that does not return) leaves the mark
  ;; set. Exceptions are handled separately by the owning driver's guard.
  (define current-iter-pull (make-parameter #f)))
