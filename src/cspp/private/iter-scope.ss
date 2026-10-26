(library (cspp private iter-scope)
  (export current-iter-pull)
  (import (chezscheme))

  ;; Cleanup scopes install a handler with signature (pull-thunk) -> value.
  ;; iter-next! invokes it around the source call, so ordinary and continuation
  ;; sources follow the same ownership rules, including pulls nested in tcat.
  ;; A handler restores its state on normal return and forwards to its parent.
  (define current-iter-pull (make-parameter #f)))
