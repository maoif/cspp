#!chezscheme
(library (cspp test descriptor)
  (export test-descriptor? test-suite-descriptor? test-case-descriptor?
          test-expand-descriptor? test-compile-descriptor? test-concrete-case?
          test-result? test-summary? test-config? test-registry?
          make-test-case make-test-suite make-test-expand make-test-compile
          test-descriptor-name test-descriptor-phase test-descriptor-metadata
          test-descriptor-children test-descriptor-body
          make-test-config default-test-config test-config-with
          test-config-reporter test-config-output test-config-xfail-strict?
          test-config-stop-on-failure? test-config-color
          make-test-registry current-test-registry test-register!
          test-clear-registry! test-registry-descriptors
          make-test-concrete-case test-concrete-case-id test-concrete-case-name
          test-concrete-case-descriptor test-concrete-case-parameters
          test-concrete-case-metadata
          make-test-result test-result-id test-result-name test-result-status
          test-result-message test-result-condition test-result-stdout
          test-result-stderr test-result-parameters test-result-source
          make-test-summary test-summary-total test-summary-passed
          test-summary-failed test-summary-errored test-summary-skipped
          test-summary-xfail test-summary-xpass test-summary-results)
  (import (cspp chez)
          (cspp utils)
          (cspp test private common))

  (define-record-type ($test-descriptor $make-test-descriptor $test-descriptor?)
    (fields kind name phase metadata children body))

  (define-record-type ($test-config $make-test-config $test-config?)
    (fields reporter output xfail-strict? stop-on-failure? color))

  (define-record-type ($test-registry $make-test-registry $test-registry?)
    (fields (mutable reversed $test-registry-reversed $test-registry-reversed-set!)
            (mutable snapshot $test-registry-snapshot $test-registry-snapshot-set!)))

  (define-record-type ($test-concrete-case $make-test-concrete-case $test-concrete-case?)
    (fields id name descriptor parameters metadata))

  (define-record-type ($test-result $make-test-result $test-result?)
    (fields id name status message condition stdout stderr parameters source))

  (define-record-type ($test-summary $make-test-summary $test-summary?)
    (fields total passed failed errored skipped xfail xpass results))

  (define $descriptor-kind?
    (lambda (kind)
      (and (memq kind '(suite case expand compile)) #t)))

  (define $result-status?
    (lambda (status)
      (and (memq status '(passed failed errored skipped xfail xpass)) #t)))

  (define $reporter-option?
    (lambda (reporter)
      (or (not reporter) (test-reporter? reporter))))

  (define $output-option?
    (lambda (output)
      (or (output-port? output) (not output))))

  (define $check-config-option
    (lambda (option)
      (unless (pair? option)
        (errorf 'test-config-with "invalid config option: ~a" option))
      (case (car option)
        [(reporter)
         (unless ($reporter-option? (cdr option))
           (errorf 'test-config-with "invalid config option: ~a" option))]
        [(output)
         (unless ($output-option? (cdr option))
           (errorf 'test-config-with "invalid config option: ~a" option))]
        [(xfail-strict? stop-on-failure?)
         (unless (boolean? (cdr option))
           (errorf 'test-config-with "invalid config option: ~a" option))]
        [(color)
         (unless (test-color-mode? (cdr option))
           (errorf 'test-config-with "invalid config option: ~a" option))]
        [else (errorf 'test-config-with "unknown config option: ~a" (car option))])))

  (define $test-id?
    (lambda (value)
      (or (natural? value) (symbol? value) (string? value))))

  (define $all-descriptors?
    (lambda (value)
      (and (list? value) (andmap test-descriptor? value))))

  #|proc:test-descriptor?
The `test-descriptor?` procedure returns whether `value` is a test descriptor.
|#
  (define test-descriptor?
    (lambda (value)
      ($test-descriptor? value)))

  #|proc:test-suite-descriptor?
The `test-suite-descriptor?` procedure returns whether `value` is a suite
descriptor containing child descriptors.
|#
  (define test-suite-descriptor?
    (lambda (value)
      (and ($test-descriptor? value)
           (eq? ($test-descriptor-kind value) 'suite))))

  #|proc:test-case-descriptor?
The `test-case-descriptor?` procedure returns whether `value` is a runtime test
case descriptor.
|#
  (define test-case-descriptor?
    (lambda (value)
      (and ($test-descriptor? value)
           (eq? ($test-descriptor-kind value) 'case))))

  #|proc:test-expand-descriptor?
The `test-expand-descriptor?` procedure returns whether `value` is an expansion
test descriptor.
|#
  (define test-expand-descriptor?
    (lambda (value)
      (and ($test-descriptor? value)
           (eq? ($test-descriptor-kind value) 'expand))))

  #|proc:test-compile-descriptor?
The `test-compile-descriptor?` procedure returns whether `value` is a compile
test descriptor.
|#
  (define test-compile-descriptor?
    (lambda (value)
      (and ($test-descriptor? value)
           (eq? ($test-descriptor-kind value) 'compile))))

  #|proc:test-concrete-case?
The `test-concrete-case?` procedure returns whether `value` is a concrete test
case produced from a descriptor.
|#
  (define test-concrete-case?
    (lambda (value)
      ($test-concrete-case? value)))

  #|proc:test-result?
The `test-result?` procedure returns whether `value` is a test execution result.
|#
  (define test-result?
    (lambda (value)
      ($test-result? value)))

  #|proc:test-summary?
The `test-summary?` procedure returns whether `value` is a test summary.
|#
  (define test-summary?
    (lambda (value)
      ($test-summary? value)))

  #|proc:test-config?
The `test-config?` procedure returns whether `value` is a test runner
configuration.
|#
  (define test-config?
    (lambda (value)
      ($test-config? value)))

  #|proc:test-registry?
The `test-registry?` procedure returns whether `value` is a test descriptor
registry.
|#
  (define test-registry?
    (lambda (value)
      ($test-registry? value)))

  #|proc:make-test-case
  The `make-test-case` procedure creates a runtime test descriptor. `name` is the
  symbol naming the case, and `metadata` is an association list of descriptor metadata.
  The runner calls `body` as `(lambda () unspecified)` or as
  `(lambda (parameters) unspecified)`, where `parameters` is an association list
  containing fixture values followed by the concrete case parameters.
  Metadata keys must be supported, unique symbols with correctly typed values;
  warning policy is `allow` or `error`, tags are symbols, and version requirements
  contain three natural numbers. Invalid body arity or metadata is rejected at construction.
  Fixture names must be unique and must not overlap parameter names; the runner
  rejects conflicting effective bindings before acquiring fixtures.
  |#
  (define make-test-case
    (lambda (name body metadata)
      (pcheck ([symbol? name] [procedure? body] [list? metadata])
              (unless (or (logbit? 0 (procedure-arity-mask body))
                          (logbit? 1 (procedure-arity-mask body)))
                (errorf 'make-test-case "body must accept zero or one argument: ~s" body))
              (test-check-metadata 'make-test-case metadata)
              ($make-test-descriptor 'case name 'runtime metadata '() body))))

  #|proc:make-test-suite
The `make-test-suite` procedure creates a suite descriptor. `name` is the symbol
naming the suite, `children` is a list of test descriptors contained by the
suite, and `metadata` is an association list of descriptor metadata.
|#
  (define make-test-suite
    (lambda (name children metadata)
      (pcheck ([symbol? name] [$all-descriptors? children] [list? metadata])
              (test-check-metadata 'make-test-suite metadata)
              ($make-test-descriptor 'suite name 'suite metadata children #f))))

  #|proc:make-test-expand
The `make-test-expand` procedure creates an expansion test descriptor. `name` is
the symbol naming the descriptor, `form` is the datum to expand, and `metadata`
is an association list of descriptor metadata. The form is expanded in an
immutable `(cspp)` environment; include custom macro definitions in `form`.
|#
  (define make-test-expand
    (lambda (name form metadata)
      (pcheck ([symbol? name] [list? metadata])
              (test-check-metadata 'make-test-expand metadata)
              ($make-test-descriptor 'expand name 'expand metadata '() form))))

  #|proc:make-test-compile
  The `make-test-compile` procedure creates a compile test descriptor. `name` is
  the symbol naming the descriptor, `forms` is a list of source data to compile,
  and `metadata` is an association list of descriptor metadata.
  |#
  (define make-test-compile
    (lambda (name forms metadata)
      (pcheck ([symbol? name] [list? forms metadata])
              (test-check-metadata 'make-test-compile metadata)
              ($make-test-descriptor 'compile name 'compile metadata '() forms))))

  #|proc:test-descriptor-name
The `test-descriptor-name` procedure returns the symbolic name stored in
`descriptor`.
|#
  (define test-descriptor-name
    (lambda (descriptor)
      (pcheck ([test-descriptor? descriptor])
              ($test-descriptor-name descriptor))))

  #|proc:test-descriptor-phase
The `test-descriptor-phase` procedure returns the execution phase symbol stored
in `descriptor`.
|#
  (define test-descriptor-phase
    (lambda (descriptor)
      (pcheck ([test-descriptor? descriptor])
              ($test-descriptor-phase descriptor))))

  #|proc:test-descriptor-metadata
The `test-descriptor-metadata` procedure returns the metadata association list
stored in `descriptor`.
|#
  (define test-descriptor-metadata
    (lambda (descriptor)
      (pcheck ([test-descriptor? descriptor])
              ($test-descriptor-metadata descriptor))))

  #|proc:test-descriptor-children
The `test-descriptor-children` procedure returns the child descriptors stored in
suite `descriptor`; non-suite descriptors return the empty list.
|#
  (define test-descriptor-children
    (lambda (descriptor)
      (pcheck ([test-descriptor? descriptor])
              ($test-descriptor-children descriptor))))

  #|proc:test-descriptor-body
  The `test-descriptor-body` procedure returns the body stored in `descriptor`.
  For runtime cases the procedure accepts zero arguments or the concrete parameter
  association list as described by `make-test-case`. Expansion descriptors return one
  source datum, compile descriptors return a list of source forms, and suites return `#f`.
  |#
  (define test-descriptor-body
    (lambda (descriptor)
      (pcheck ([test-descriptor? descriptor])
              ($test-descriptor-body descriptor))))

  #|proc:make-test-config
  The `make-test-config` procedure creates a test runner configuration.
  `reporter` is a reporter object or `#f`, `output` is the output port or `#f`,
  `xfail-strict?` makes unexpected passes have status `failed` instead of `xpass`,
  `stop-on-failure?` controls whether the runner stops at the first failure, and
  `color` is `auto`, `always`, or `never`. An explicit `always` or `never`
  overrides the progress reporter's color policy during this run; `auto` retains it.
  |#
  (define make-test-config
    (lambda (reporter output xfail-strict? stop-on-failure? color)
      (pcheck ([$reporter-option? reporter]
               [$output-option? output]
               [boolean? xfail-strict? stop-on-failure?]
               [test-color-mode? color])
              ($make-test-config reporter output xfail-strict? stop-on-failure? color))))

  #|proc:default-test-config
The `default-test-config` procedure returns the default test runner
configuration.
|#
  (define default-test-config
    (lambda ()
      (make-test-config #f (current-output-port) #f #f 'auto)))

  #|proc:test-config-with
The `test-config-with` procedure returns a copy of `config` with fields changed
according to option association list `options`. Each option is a pair whose car
is one of `reporter`, `output`, `xfail-strict?`, `stop-on-failure?`, or `color`;
each cdr must have the same type accepted by `make-test-config` for that field.
|#
  (define test-config-with
    (lambda (config options)
      (pcheck ([test-config? config] [list? options])
              (let loop ([options options] [config config])
                (if (null? options)
                    config
                    (let ([option (car options)])
                      ($check-config-option option)
                      (case (car option)
                        [(reporter)
                         (loop (cdr options)
                               (make-test-config (cdr option)
                                                 (test-config-output config)
                                                 (test-config-xfail-strict? config)
                                                 (test-config-stop-on-failure? config)
                                                 (test-config-color config)))]
                        [(output)
                         (loop (cdr options)
                               (make-test-config (test-config-reporter config)
                                                 (cdr option)
                                                 (test-config-xfail-strict? config)
                                                 (test-config-stop-on-failure? config)
                                                 (test-config-color config)))]
                        [(xfail-strict?)
                         (loop (cdr options)
                               (make-test-config (test-config-reporter config)
                                                 (test-config-output config)
                                                 (cdr option)
                                                 (test-config-stop-on-failure? config)
                                                 (test-config-color config)))]
                        [(stop-on-failure?)
                         (loop (cdr options)
                               (make-test-config (test-config-reporter config)
                                                 (test-config-output config)
                                                 (test-config-xfail-strict? config)
                                                 (cdr option)
                                                 (test-config-color config)))]
                        [(color)
                         (loop (cdr options)
                               (make-test-config (test-config-reporter config)
                                                 (test-config-output config)
                                                 (test-config-xfail-strict? config)
                                                 (test-config-stop-on-failure? config)
                                                 (cdr option)))]
                        [else (errorf 'test-config-with "unknown config option: ~a" (car option))])))))))

  #|proc:test-config-reporter
The `test-config-reporter` procedure returns the reporter object stored in
`config`, or `#f` when reporting is disabled.
|#
  (define test-config-reporter
    (lambda (config)
      (pcheck ([test-config? config])
              ($test-config-reporter config))))

  #|proc:test-config-output
The `test-config-output` procedure returns the output port stored in `config`,
or `#f` when output is disabled.
|#
  (define test-config-output
    (lambda (config)
      (pcheck ([test-config? config])
              ($test-config-output config))))

  #|proc:test-config-xfail-strict?
The `test-config-xfail-strict?` procedure returns whether unexpected passes are
treated strictly by `config`.
|#
  (define test-config-xfail-strict?
    (lambda (config)
      (pcheck ([test-config? config])
              ($test-config-xfail-strict? config))))

  #|proc:test-config-stop-on-failure?
The `test-config-stop-on-failure?` procedure returns whether `config` stops a
run at the first failure.
|#
  (define test-config-stop-on-failure?
    (lambda (config)
      (pcheck ([test-config? config])
              ($test-config-stop-on-failure? config))))

  #|proc:test-config-color
The `test-config-color` procedure returns the color mode symbol stored in
`config`.
|#
  (define test-config-color
    (lambda (config)
      (pcheck ([test-config? config])
              ($test-config-color config))))

  #|proc:make-test-registry
The `make-test-registry` procedure creates an empty mutable test descriptor
registry.
|#
  (define make-test-registry
    (lambda ()
      ($make-test-registry '() '())))

  #|proc:current-test-registry
The `current-test-registry` parameter stores the default mutable test
descriptor registry.
|#
  (define current-test-registry
    (make-parameter (make-test-registry)))

  #|proc:test-register!
  The `test-register!` procedure appends `descriptor` to `registry` in constant time
  and returns `descriptor`. Previously returned descriptor lists are preserved.
  |#
  (define test-register!
    (lambda (registry descriptor)
      (pcheck ([test-registry? registry] [test-descriptor? descriptor])
              (begin
                ($test-registry-reversed-set!
                 registry (cons descriptor ($test-registry-reversed registry)))
                ($test-registry-snapshot-set! registry #f)
                descriptor))))

  #|proc:test-clear-registry!
The `test-clear-registry!` procedure removes all descriptors from `registry`.
|#
  (define test-clear-registry!
    (lambda (registry)
      (pcheck ([test-registry? registry])
              ($test-registry-reversed-set! registry '())
              ($test-registry-snapshot-set! registry '()))))

  #|proc:test-registry-descriptors
  The `test-registry-descriptors` procedure returns an ordered snapshot of the
  descriptors currently stored in `registry`. After registration, the first read
  takes linear time; repeated reads reuse the snapshot until the registry changes.
  |#
  (define test-registry-descriptors
    (lambda (registry)
      (pcheck ([test-registry? registry])
              (or ($test-registry-snapshot registry)
                  (let ([snapshot (reverse ($test-registry-reversed registry))])
                    ($test-registry-snapshot-set! registry snapshot)
                    snapshot)))))

  #|proc:make-test-concrete-case
The `make-test-concrete-case` procedure creates a concrete test case. `id` is a
natural-number identifier, `name` is the symbolic concrete case name,
`descriptor` is the source descriptor, `parameters` is an association list of
case parameters, and `metadata` is an association list of case metadata.
|#
  (define make-test-concrete-case
    (lambda (id name descriptor parameters metadata)
      (pcheck ([natural? id] [symbol? name] [test-descriptor? descriptor]
               [list? parameters metadata])
              (test-check-metadata 'make-test-concrete-case metadata)
              (unless (let loop ([entries parameters] [seen '()])
                        (or (null? entries)
                            (and (pair? (car entries)) (symbol? (caar entries))
                                 (not (memq (caar entries) seen))
                                 (loop (cdr entries) (cons (caar entries) seen)))))
                (errorf 'make-test-concrete-case "parameters must have unique symbolic keys: ~s"
                        parameters))
              ($make-test-concrete-case id name descriptor parameters metadata))))

  #|proc:test-concrete-case-id
The `test-concrete-case-id` procedure returns the natural-number identifier
stored in concrete test `case`.
|#
  (define test-concrete-case-id
    (lambda (concrete-case)
      (pcheck ([test-concrete-case? concrete-case])
              ($test-concrete-case-id concrete-case))))

  #|proc:test-concrete-case-name
The `test-concrete-case-name` procedure returns the symbolic name stored in
concrete test `case`.
|#
  (define test-concrete-case-name
    (lambda (concrete-case)
      (pcheck ([test-concrete-case? concrete-case])
              ($test-concrete-case-name concrete-case))))

  #|proc:test-concrete-case-descriptor
The `test-concrete-case-descriptor` procedure returns the source descriptor
stored in concrete test `case`.
|#
  (define test-concrete-case-descriptor
    (lambda (concrete-case)
      (pcheck ([test-concrete-case? concrete-case])
              ($test-concrete-case-descriptor concrete-case))))

  #|proc:test-concrete-case-parameters
The `test-concrete-case-parameters` procedure returns the parameter association
list stored in concrete test `case`.
|#
  (define test-concrete-case-parameters
    (lambda (concrete-case)
      (pcheck ([test-concrete-case? concrete-case])
              ($test-concrete-case-parameters concrete-case))))

  #|proc:test-concrete-case-metadata
The `test-concrete-case-metadata` procedure returns the metadata association
list stored in concrete test `case`.
|#
  (define test-concrete-case-metadata
    (lambda (concrete-case)
      (pcheck ([test-concrete-case? concrete-case])
              ($test-concrete-case-metadata concrete-case))))

  #|proc:make-test-result
The `make-test-result` procedure creates a test result. `id` is the concrete
case identifier, `name` is the concrete case name, `status` is one of `passed`,
`failed`, `errored`, `skipped`, `xfail`, or `xpass`, `message` is a result
message or `#f`, `condition` is the captured condition or `#f`, `stdout` and
`stderr` are captured output strings, `parameters` is an association list, and
`source` identifies the source descriptor or location.
|#
  (define make-test-result
    (lambda (id name status message condition stdout stderr parameters source)
      (pcheck ([$test-id? id] [symbol? name] [$result-status? status]
               [(lambda (value) (or (string? value) (not value))) message]
               [string? stdout stderr] [list? parameters])
              ($make-test-result id name status message condition stdout stderr parameters source))))

  #|proc:test-result-id
The `test-result-id` procedure returns the concrete case identifier stored in
`result`.
|#
  (define test-result-id
    (lambda (result)
      (pcheck ([test-result? result])
              ($test-result-id result))))

  #|proc:test-result-name
The `test-result-name` procedure returns the concrete case name stored in
`result`.
|#
  (define test-result-name
    (lambda (result)
      (pcheck ([test-result? result])
              ($test-result-name result))))

  #|proc:test-result-status
The `test-result-status` procedure returns the status symbol stored in
`result`.
|#
  (define test-result-status
    (lambda (result)
      (pcheck ([test-result? result])
              ($test-result-status result))))

  #|proc:test-result-message
The `test-result-message` procedure returns the message string stored in
`result`, or `#f` when no message was recorded.
|#
  (define test-result-message
    (lambda (result)
      (pcheck ([test-result? result])
              ($test-result-message result))))

  #|proc:test-result-condition
The `test-result-condition` procedure returns the condition stored in `result`,
or `#f` when no condition was recorded.
|#
  (define test-result-condition
    (lambda (result)
      (pcheck ([test-result? result])
              ($test-result-condition result))))

  #|proc:test-result-stdout
The `test-result-stdout` procedure returns the captured stdout string stored in
`result`.
|#
  (define test-result-stdout
    (lambda (result)
      (pcheck ([test-result? result])
              ($test-result-stdout result))))

  #|proc:test-result-stderr
The `test-result-stderr` procedure returns the captured stderr string stored in
`result`.
|#
  (define test-result-stderr
    (lambda (result)
      (pcheck ([test-result? result])
              ($test-result-stderr result))))

  #|proc:test-result-parameters
The `test-result-parameters` procedure returns the parameter association list
stored in `result`.
|#
  (define test-result-parameters
    (lambda (result)
      (pcheck ([test-result? result])
              ($test-result-parameters result))))

  #|proc:test-result-source
The `test-result-source` procedure returns the source object stored in
`result`.
|#
  (define test-result-source
    (lambda (result)
      (pcheck ([test-result? result])
              ($test-result-source result))))

  #|proc:make-test-summary
The `make-test-summary` procedure creates a summary from natural-number counts:
`total`, `passed`, `failed`, `errored`, `skipped`, `xfail`, and `xpass`.
|#
  (define make-test-summary
    (case-lambda
      [(total passed failed errored skipped xfail xpass)
       (make-test-summary total passed failed errored skipped xfail xpass '())]
      [(total passed failed errored skipped xfail xpass results)
       (pcheck ([natural? total passed failed errored skipped xfail xpass] [list? results])
               ($make-test-summary total passed failed errored skipped xfail xpass results))]))

  #|proc:test-summary-total
The `test-summary-total` procedure returns the total test count stored in
`summary`.
|#
  (define test-summary-total
    (lambda (summary)
      (pcheck ([test-summary? summary])
              ($test-summary-total summary))))

  #|proc:test-summary-passed
The `test-summary-passed` procedure returns the passed count stored in
`summary`.
|#
  (define test-summary-passed
    (lambda (summary)
      (pcheck ([test-summary? summary])
              ($test-summary-passed summary))))

  #|proc:test-summary-failed
The `test-summary-failed` procedure returns the failed count stored in
`summary`.
|#
  (define test-summary-failed
    (lambda (summary)
      (pcheck ([test-summary? summary])
              ($test-summary-failed summary))))

  #|proc:test-summary-errored
The `test-summary-errored` procedure returns the errored count stored in
`summary`.
|#
  (define test-summary-errored
    (lambda (summary)
      (pcheck ([test-summary? summary])
              ($test-summary-errored summary))))

  #|proc:test-summary-skipped
The `test-summary-skipped` procedure returns the skipped count stored in
`summary`.
|#
  (define test-summary-skipped
    (lambda (summary)
      (pcheck ([test-summary? summary])
              ($test-summary-skipped summary))))

  #|proc:test-summary-xfail
The `test-summary-xfail` procedure returns the expected-failure count stored in
`summary`.
|#
  (define test-summary-xfail
    (lambda (summary)
      (pcheck ([test-summary? summary])
              ($test-summary-xfail summary))))

  #|proc:test-summary-xpass
The `test-summary-xpass` procedure returns the unexpected-pass count stored in
`summary`.
|#
  (define test-summary-xpass
    (lambda (summary)
      (pcheck ([test-summary? summary])
              ($test-summary-xpass summary))))

  #|proc:test-summary-results
The `test-summary-results` procedure returns the result records stored in
`summary`.
|#
  (define test-summary-results
    (lambda (summary)
      (pcheck ([test-summary? summary])
              ($test-summary-results summary))))

  )
