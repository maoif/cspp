(library (cspp system errors)
  (export
          ;; base system conditions
          system-error?
          make-system-error
          system-error-operation
          system-error-code
          system-error-message
          system-error-context

          ;; specialized system conditions
          system-unsupported-error?
          make-system-unsupported-error
          system-not-found-error?
          make-system-not-found-error
          system-permission-error?
          make-system-permission-error
          system-timeout-error?
          make-system-timeout-error
          system-exit-error?
          make-system-exit-error

          ;; raising and FFI helpers
          raise-system-error
          raise-system-unsupported)
  (import (cspp chez)
          (cspp utils))

  #|proc:system-error?
  The `system-error?` procedure returns `#t` when its argument is a system error condition,
  otherwise `#f`.
  The `condition` parameter is the object to test.
  |#
  #|proc:system-error-operation
  The `system-error-operation` procedure returns the operation stored in a system error condition.
  The `condition` parameter is a system error condition.
  |#
  #|proc:system-error-code
  The `system-error-code` procedure returns the error code stored in a system error condition, or
  `#f` when no code is available.
  The `condition` parameter is a system error condition.
  |#
  #|proc:system-error-message
  The `system-error-message` procedure returns the message stored in a system error condition.
  The `condition` parameter is a system error condition.
  |#
  #|proc:system-error-context
  The `system-error-context` procedure returns the association-list context stored in a system
  error condition.
  The `condition` parameter is a system error condition.
  |#
  (define-condition-type &system-error &error
    %make-system-error
    system-error?
    (operation system-error-operation)
    (code system-error-code)
    (message system-error-message)
    (context system-error-context))

  #|proc:system-unsupported-error?
  The `system-unsupported-error?` procedure returns `#t` when its argument is an unsupported
  system operation condition, otherwise `#f`.
  The `condition` parameter is the object to test.
  |#
  (define-condition-type &system-unsupported-error &system-error
    %make-system-unsupported-error
    system-unsupported-error?)

  #|proc:system-not-found-error?
  The `system-not-found-error?` procedure returns `#t` when its argument is a system not-found
  condition, otherwise `#f`.
  The `condition` parameter is the object to test.
  |#
  (define-condition-type &system-not-found-error &system-error
    %make-system-not-found-error
    system-not-found-error?)

  #|proc:system-permission-error?
  The `system-permission-error?` procedure returns `#t` when its argument is a system permission
  condition, otherwise `#f`.
  The `condition` parameter is the object to test.
  |#
  (define-condition-type &system-permission-error &system-error
    %make-system-permission-error
    system-permission-error?)

  #|proc:system-timeout-error?
  The `system-timeout-error?` procedure returns `#t` when its argument is a system timeout
  condition, otherwise `#f`.
  The `condition` parameter is the object to test.
  |#
  (define-condition-type &system-timeout-error &system-error
    %make-system-timeout-error
    system-timeout-error?)

  #|proc:system-exit-error?
  The `system-exit-error?` procedure returns `#t` when its argument is a system process-exit
  condition, otherwise `#f`.
  The `condition` parameter is the object to test.
  |#
  (define-condition-type &system-exit-error &system-error
    %make-system-exit-error
    system-exit-error?)

  (define $system-operation?
    (lambda (x)
      (or (symbol? x) (eq? x #f))))

  (define $system-code?
    (lambda (x)
      (or (and (integer? x) (exact? x)) (eq? x #f))))

  (define $system-context?
    (lambda (x)
      (and (list? x) (andmap pair? x))))

  #|proc:make-system-error
  The `make-system-error` procedure constructs a system error condition.
  The `operation` parameter is a symbol naming the operation that failed, or `#f` when unknown.
  The `code` parameter is an exact integer error code, or `#f` when no code is available.
  The `message` parameter is a string describing the failure.
  The `context` parameter is an association list with additional failure details.
  |#
  (define make-system-error
    (lambda (operation code message context)
      (pcheck ([$system-operation? operation] [$system-code? code] [string? message] [$system-context? context])
              (%make-system-error operation code message context))))

  (define $make-special-system-error
    (lambda (make-condition operation message context)
      (pcheck ([$system-operation? operation] [string? message] [$system-context? context])
              (make-condition operation #f message context))))

  #|proc:make-system-unsupported-error
  The `make-system-unsupported-error` procedure constructs a system error condition for an
  unsupported operation.
  The `operation` parameter is a symbol naming the unsupported operation, or `#f` when unknown.
  The `message` parameter is a string describing the failure.
  The `context` parameter is an association list with additional failure details.
  |#
  (define make-system-unsupported-error
    (case-lambda
      [(operation message)
       (make-system-unsupported-error operation message '())]
      [(operation message context)
       ($make-special-system-error %make-system-unsupported-error operation message context)]))

  #|proc:make-system-not-found-error
  The `make-system-not-found-error` procedure constructs a system error condition for a missing
  system resource.
  The `operation` parameter is a symbol naming the operation that failed, or `#f` when unknown.
  The `message` parameter is a string describing the failure.
  The `context` parameter is an association list with additional failure details.
  |#
  (define make-system-not-found-error
    (case-lambda
      [(operation message)
       (make-system-not-found-error operation message '())]
      [(operation message context)
       ($make-special-system-error %make-system-not-found-error operation message context)]))

  #|proc:make-system-permission-error
  The `make-system-permission-error` procedure constructs a system error condition for a
  permission failure.
  The `operation` parameter is a symbol naming the operation that failed, or `#f` when unknown.
  The `code` parameter is an exact integer error code, or `#f` when no code is available.
  The `message` parameter is a string describing the failure.
  The `context` parameter is an association list with additional failure details.
  |#
  (define make-system-permission-error
    (case-lambda
      [(operation message)
       (make-system-permission-error operation #f message '())]
      [(operation message context)
       (make-system-permission-error operation #f message context)]
      [(operation code message context)
       (pcheck ([$system-operation? operation] [$system-code? code] [string? message] [$system-context? context])
               (%make-system-permission-error operation code message context))]))

  #|proc:make-system-timeout-error
  The `make-system-timeout-error` procedure constructs a system error condition for a timeout.
  The `operation` parameter is a symbol naming the operation that failed, or `#f` when unknown.
  The `message` parameter is a string describing the failure.
  The `context` parameter is an association list with additional failure details.
  |#
  (define make-system-timeout-error
    (case-lambda
      [(operation message)
       (make-system-timeout-error operation message '())]
      [(operation message context)
       ($make-special-system-error %make-system-timeout-error operation message context)]))

  #|proc:make-system-exit-error
  The `make-system-exit-error` procedure constructs a system error condition for an unsuccessful
  process exit.
  The `operation` parameter is a symbol naming the operation that failed, or `#f` when unknown.
  The `message` parameter is a string describing the failure.
  The `context` parameter is an association list with additional failure details.
  |#
  (define make-system-exit-error
    (case-lambda
      [(operation message)
       (make-system-exit-error operation message '())]
      [(operation message context)
       ($make-special-system-error %make-system-exit-error operation message context)]))

  #|proc:raise-system-error
  The `raise-system-error` procedure raises a system error condition.
  The `operation` parameter is a symbol naming the operation that failed, or `#f` when unknown.
  The `code` parameter is an exact integer error code, or `#f` when no code is available.
  The `message` parameter is a string describing the failure.
  The `context` parameter is an association list with additional failure details.
  |#
  (define raise-system-error
    (lambda (operation code message context)
      (raise (make-system-error operation code message context))))

  #|proc:raise-system-unsupported
  The `raise-system-unsupported` procedure raises a system error condition for an unsupported
  operation.
  The `operation` parameter is a symbol naming the unsupported operation, or `#f` when unknown.
  The `message` parameter is a string describing the failure.
  |#
  (define raise-system-unsupported
    (lambda (operation message)
      (raise (make-system-unsupported-error operation message))))


)
