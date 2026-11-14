(library (cspp net errors)
  (export make-net-error
          net-error?
          net-error-who
          net-error-kind
          net-error-operation
          net-error-message
          net-error-status
          net-error-errno
          net-error-endpoint
          net-error-retryable?
          net-error-cause
          net-error-data
          net-error-matches?
          call-with-net-error
          raise-net-error)
  (import (cspp chez)
          (cspp utils))

  (define-condition-type &net-error &error
    %make-net-error
    net-error?
    (who net-error-who)
    (kind net-error-kind)
    (operation net-error-operation)
    (message net-error-message)
    (status net-error-status)
    (errno net-error-errno)
    (endpoint net-error-endpoint)
    (retryable? net-error-retryable?)
    (cause net-error-cause)
    (data net-error-data))

  (define check-fields
    (lambda (who who-value kind operation message status errno endpoint retryable? cause)
      (unless (or (symbol? who-value) (eq? who-value #f))
        (errorf who "expected symbol or #f for error source"))
      (unless (symbol? kind) (errorf who "expected a symbol for error kind"))
      (unless (or (symbol? operation) (eq? operation #f))
        (errorf who "expected symbol or #f for operation"))
      (unless (string? message) (errorf who "expected string for error message"))
      (unless (or (symbol? status) (eq? status #f))
        (errorf who "expected symbol or #f for status"))
      (unless (or (fixnum? errno) (eq? errno #f))
        (errorf who "expected fixnum or #f for errno"))
      (unless (or (string? endpoint) (eq? endpoint #f))
        (errorf who "expected string or #f for endpoint"))
      (unless (boolean? retryable?) (errorf who "expected boolean for retryable?"))
      (unless (or (condition? cause) (eq? cause #f))
        (errorf who "expected condition or #f for cause"))))

  #|proc:make-net-error
The `make-net-error` procedure constructs a structured network error condition.
The compatibility arity is `(who kind message data)`. The full arity is
`(kind operation message status errno endpoint retryable? cause data)`.
The return value is a `net-error` condition.
|#
  (define-who make-net-error
    (case-lambda
      [(who kind message)
       (make-net-error who kind message #f)]
      [(who kind message data)
       (%make-net-error who kind #f message #f #f #f #f #f data)]
      [(kind operation message status errno endpoint retryable? cause data)
       (pcheck ([symbol? kind operation] [string? message] [boolean? retryable?])
         (check-fields who #f kind operation message status errno endpoint retryable? cause)
         (%make-net-error #f kind operation message status errno endpoint retryable? cause data))]))

  #|proc:raise-net-error
The `raise-net-error` procedure raises a structured network error condition.
It accepts the same compatibility and full arities as `make-net-error`, and returns no value.
|#
  (define-who raise-net-error
    (case-lambda
      [(who kind message)
       (raise-net-error who kind message #f)]
      [(who kind message data)
       (raise (make-net-error who kind message data))]
      [(kind operation message status errno endpoint retryable? cause data)
       (raise (make-net-error kind operation message status errno endpoint retryable? cause data))]))

  #|proc:net-error-matches?
The `net-error-matches?` procedure tests exact expectations in alist `fields` against `error`.
Supported keys are the exported net-error field names; the return value is boolean.
|#
  (define-who net-error-matches?
    (lambda (error fields)
      (pcheck ([net-error? error] [list? fields])
        (andmap
         (lambda (entry)
           (and (pair? entry)
                (case (car entry)
                  [(who) (equal? (cdr entry) (net-error-who error))]
                  [(kind) (equal? (cdr entry) (net-error-kind error))]
                  [(operation) (equal? (cdr entry) (net-error-operation error))]
                  [(message) (equal? (cdr entry) (net-error-message error))]
                  [(status) (equal? (cdr entry) (net-error-status error))]
                  [(errno) (equal? (cdr entry) (net-error-errno error))]
                  [(endpoint) (equal? (cdr entry) (net-error-endpoint error))]
                  [(retryable?) (equal? (cdr entry) (net-error-retryable? error))]
                  [(cause) (eq? (cdr entry) (net-error-cause error))]
                  [(data) (equal? (cdr entry) (net-error-data error))]
                  [else #f])))
         fields))))

  #|proc:call-with-net-error
The `call-with-net-error` procedure calls zero-argument `thunk` and returns its value.
When `thunk` raises a net error, it calls one-argument `handler` with that condition and returns
the handler result. Other conditions are re-raised unchanged.
|#
  (define-who call-with-net-error
    (lambda (thunk handler)
      (pcheck ([procedure? thunk handler])
        (guard (condition
                [(net-error? condition) (handler condition)]
                [else (raise condition)])
          (thunk)))))
)
