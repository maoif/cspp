#!chezscheme
(library (cspp test assertion)
  (export test-failure? test-failure-message test-failure-expected
          test-failure-actual make-test-failure
          test-assert test-assert-equal test-assert-raises
          test-assert-not-raises
          test-true test-false test-eq test-eqv test-equal test-=
          test-pred test-raises test-not-raises test-fail)
  (import (cspp chez)
          (cspp utils)
          (cspp test private common))

  #|doc
  Every exported procedure in this library supports independent concurrent calls from OS threads
  and fibers, including during an active test run. Assertions return `#t` on success and raise
  in their caller on failure; `test-fail` always raises. They do not record runner results.
  Callers must synchronize shared mutable arguments and callbacks, join workers, and propagate
  worker failures to the controlling test body. See `docs/test-and-benchmark.md` for an example.
  General continuation replay is not guaranteed; ordinary callback return, exceptions, and fiber
  suspension are supported. Failure values retain their original operands without copying them.
  |#

  (define-condition-type &test-failure &error
    %make-test-failure
    $test-failure?
    (message $test-failure-message)
    (expected $test-failure-expected)
    (actual $test-failure-actual))

  (define $raise-test-failure
    (lambda (message expected actual)
      (raise (make-test-failure message expected actual))))

  #|proc:test-failure?
The `test-failure?` procedure returns whether `value` is a framework assertion
failure condition.
|#
  (define test-failure?
    (lambda (value)
      ($test-failure? value)))

  #|proc:test-failure-message
The `test-failure-message` procedure returns the diagnostic message stored in
framework assertion failure `condition`.
|#
  (define test-failure-message
    (lambda (condition)
      (pcheck ([test-failure? condition])
              ($test-failure-message condition))))

  #|proc:test-failure-expected
The `test-failure-expected` procedure returns the expected value stored in
framework assertion failure `condition`.
|#
  (define test-failure-expected
    (lambda (condition)
      (pcheck ([test-failure? condition])
              ($test-failure-expected condition))))

  #|proc:test-failure-actual
The `test-failure-actual` procedure returns the actual value stored in
framework assertion failure `condition`.
|#
  (define test-failure-actual
    (lambda (condition)
      (pcheck ([test-failure? condition])
              ($test-failure-actual condition))))

  #|proc:make-test-failure
The `make-test-failure` procedure creates a framework assertion failure
condition. `message` is a string describing the failed assertion, `expected` is
the expected value recorded for diagnostics, and `actual` is the actual value
recorded for diagnostics.
|#
  (define make-test-failure
    (lambda (message expected actual)
      (pcheck ([string? message])
              (condition (%make-test-failure message expected actual)
                         (make-message-condition message)))))

  #|proc:test-fail
The `test-fail` procedure raises a framework assertion failure. `message` is a
string describing the failure, `expected` is the expected value recorded for
diagnostics, and `actual` is the actual value recorded for diagnostics.
|#
  (define test-fail
    (lambda (message expected actual)
      (pcheck ([string? message])
              ($raise-test-failure message expected actual))))

  #|proc:test-assert
The `test-assert` procedure succeeds when `condition` is true and raises a
framework assertion failure otherwise. `message` is a string describing the
assertion, `expected` is the expected value recorded on failure, and `actual` is
the actual value recorded on failure.
|#
  (define test-assert
    (lambda (condition message expected actual)
      (pcheck ([string? message])
              (if condition
                  #t
                  ($raise-test-failure message expected actual)))))

  #|proc:test-assert-equal
The `test-assert-equal` procedure compares `expected` and `actual` with
`same?`. `same?` is a two-argument predicate with signature
`(lambda (expected actual) ...)`. `message` is recorded if the comparison
fails.
Concurrent callers must supply a comparison and operands safe for concurrent use.
|#
  (define test-assert-equal
    (lambda (same? expected actual message)
      (pcheck ([procedure? same?] [string? message])
              (test-assert (same? expected actual) message expected actual))))

  #|proc:test-assert-raises
The `test-assert-raises` procedure succeeds when zero-argument procedure
`thunk` raises a condition accepted by `predicate`. `predicate` has signature
`(lambda (condition) ...)`. If `thunk` raises no condition, or raises a
condition rejected by `predicate`, a framework assertion failure is raised.
`message` is recorded on failure. Callback arities are checked before invoking `thunk`.
|#
  (define test-assert-raises
    (lambda (predicate thunk message)
      (pcheck ([procedure? predicate thunk] [string? message])
              (unless (and (test-callback-accepts? predicate 1)
                           (test-callback-accepts? thunk 0))
                (errorf 'test-assert-raises "expected unary predicate and zero-argument thunk"))
              (let ([raised
                     (guard (condition
                             [else
                              (if (predicate condition)
                                  #t
                                  ($raise-test-failure message predicate condition))])
                       (thunk)
                       #f)])
                (unless raised
                  ($raise-test-failure message predicate 'no-condition))
                #t))))

  #|proc:test-assert-not-raises
The `test-assert-not-raises` procedure succeeds when zero-argument procedure
`thunk` returns without raising a condition. If `thunk` raises any condition,
a framework assertion failure is raised. `message` is recorded on failure.
Callback arities are checked before invoking `thunk`.
|#
  (define test-assert-not-raises
    (lambda (thunk message)
      (pcheck ([procedure? thunk] [string? message])
              (unless (test-callback-accepts? thunk 0)
                (errorf 'test-assert-not-raises "thunk must accept zero arguments"))
              (guard (condition
                      [else ($raise-test-failure message 'no-condition condition)])
                (thunk)
                #t))))

  #|proc:test-true
The `test-true` procedure succeeds when `actual` is true and raises a framework
assertion failure otherwise.
|#
  (define test-true
    (lambda (actual)
      (test-assert actual "expected true value" #t actual)))

  #|proc:test-false
The `test-false` procedure succeeds when `actual` is `#f` and raises a
framework assertion failure otherwise.
|#
  (define test-false
    (lambda (actual)
      (test-assert (not actual) "expected false value" #f actual)))

  #|proc:test-eq
The `test-eq` procedure succeeds when `expected` and `actual` are `eq?` and
raises a framework assertion failure otherwise.
|#
  (define test-eq
    (lambda (expected actual)
      (test-assert-equal eq? expected actual "expected eq? values")))

  #|proc:test-eqv
The `test-eqv` procedure succeeds when `expected` and `actual` are `eqv?` and
raises a framework assertion failure otherwise.
|#
  (define test-eqv
    (lambda (expected actual)
      (test-assert-equal eqv? expected actual "expected eqv? values")))

  #|proc:test-equal
The `test-equal` procedure succeeds when `expected` and `actual` are `equal?`
and raises a framework assertion failure otherwise.
|#
  (define test-equal
    (lambda (expected actual)
      (test-assert-equal equal? expected actual "expected equal? values")))

  #|proc:test-=
The `test-=` procedure succeeds when numbers `expected` and `actual` are `=`
and raises a framework assertion failure otherwise.
It returns `#t` on success and may be called from any thread or fiber. A worker's failure
must be propagated to the controlling body; this procedure does not update runner state.
|#
  (define test-=
    (lambda (expected actual)
      (pcheck ([number? expected actual])
              (test-assert-equal = expected actual "expected numerically equal values"))))

  #|proc:test-pred
The `test-pred` procedure succeeds when unary predicate `predicate` accepts
`actual` and raises a framework assertion failure otherwise. `predicate` has
signature `(lambda (actual) ...)`.
Concurrent callers must supply a predicate and argument safe for concurrent use.
|#
  (define test-pred
    (lambda (predicate actual)
      (pcheck ([procedure? predicate])
              (test-assert (predicate actual) "expected predicate to accept value"
                           predicate actual))))

  #|proc:test-raises
The `test-raises` procedure succeeds when zero-argument procedure `thunk`
raises a condition accepted by `predicate`. `predicate` has signature
`(lambda (condition) ...)`.
|#
  (define test-raises
    (lambda (predicate thunk)
      (test-assert-raises predicate thunk "expected matching condition")))

  #|proc:test-not-raises
The `test-not-raises` procedure succeeds when zero-argument procedure `thunk`
returns without raising any condition.
|#
  (define test-not-raises
    (lambda (thunk)
      (test-assert-not-raises thunk "expected no condition"))))
