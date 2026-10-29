#!chezscheme
(library (cspp test capture)
  (export test-capture-ports test-check-output-expectations)
  (import (cspp chez)
          (cspp utils)
          (cspp test private common)
          (cspp test assertion))

  (define $capture-stdout?
    (lambda (mode)
      (or (eq? mode 'stdout)
          (eq? mode 'combined)
          (and (list? mode) (memq 'stdout mode)))))

  (define $capture-stderr?
    (lambda (mode)
      (or (eq? mode 'stderr)
          (eq? mode 'combined)
          (and (list? mode) (memq 'stderr mode)))))

  (define $capture-mode?
    (lambda (mode)
      (or (not mode)
          (memq mode '(stdout stderr combined))
          (and (list? mode) (for-all (lambda (stream) (memq stream '(stdout stderr))) mode)))))

  #|proc:test-capture-ports
  The `test-capture-ports` procedure calls `thunk` while capturing selected
  current output ports. `mode` is `#f` for no capture, `'stdout`, `'stderr`,
  `'combined`, or a list containing `'stdout` and/or `'stderr`.
  The `thunk` parameter has signature `(lambda () value ...)`. The procedure
  returns three values: the thunk values as a list, captured stdout, and captured
  stderr. Combined capture preserves output order in stdout and returns empty stderr.
  Only selected capture ports are opened, and they are closed before returning.
  Call from the controlling thread, outside fibers. Join workers using inherited ports before
  returning; concurrent writers need caller synchronization. Continuation reentry is undefined.
  |#
  (define test-capture-ports
    (lambda (mode thunk)
      (pcheck ([$capture-mode? mode] [procedure? thunk])
              (let ([capture-stdout? ($capture-stdout? mode)]
                    [capture-stderr? ($capture-stderr? mode)])
                (if (not (or capture-stdout? capture-stderr?))
                    (values (call-with-values thunk list) "" "")
                    (let* ([stdout-port (and capture-stdout? (open-output-string))]
                           [stderr-port (and capture-stderr?
                                             (if (eq? mode 'combined)
                                                 stdout-port
                                                 (open-output-string)))])
                      (dynamic-wind
                        (lambda () (void))
                        (lambda ()
                          (let ([values-list
                                 (parameterize
                                   ([current-output-port (or stdout-port (current-output-port))]
                                    [current-error-port (or stderr-port (current-error-port))])
                                   (call-with-values thunk list))])
                            (values values-list
                                    (if stdout-port (get-output-string stdout-port) "")
                                    (if (and stderr-port (not (eq? mode 'combined)))
                                        (get-output-string stderr-port)
                                        ""))))
                        (lambda ()
                          (when stdout-port (close-output-port stdout-port))
                          (when (and stderr-port (not (eq? stderr-port stdout-port)))
                            (close-output-port stderr-port))))))))))

  #|proc:test-check-output-expectations
The `test-check-output-expectations` procedure checks captured `stdout` and
`stderr` strings against output expectations in `metadata`. The `metadata`
parameter is an association list. The `stdout` and `stderr` parameters are
strings.
|#
  (define test-check-output-expectations
    (lambda (metadata stdout stderr)
      (pcheck ([list? metadata] [string? stdout stderr])
              (let ([expected-stdout (test-metadata-ref metadata 'stdout #f)]
                    [expected-stderr (test-metadata-ref metadata 'stderr #f)]
                    [expected-output (test-metadata-ref metadata 'output #f)])
                (when (and expected-stdout (not (string=? expected-stdout stdout)))
                  (raise (make-test-failure "stdout mismatch" expected-stdout stdout)))
                (when (and expected-stderr (not (string=? expected-stderr stderr)))
                  (raise (make-test-failure "stderr mismatch" expected-stderr stderr)))
                (when (and expected-output
                           (not (string=? expected-output (string-append stdout stderr))))
                  (raise (make-test-failure "combined output mismatch"
                                            expected-output
                                            (string-append stdout stderr))))
                (void)))))

  )
