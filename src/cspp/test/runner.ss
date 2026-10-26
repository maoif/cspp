#!chezscheme
(library (cspp test runner)
  (export test-expand-parameters test-list test-select
          test-run test-run-registry test-run-files test-main)
  (import (cspp chez)
          (cspp utils)
          (cspp test assertion)
          (cspp test capture)
          (cspp test private common)
          (cspp test descriptor)
          (cspp test reporter))

  (define $metadata-merge
    (lambda (parent child)
      (let loop ([metadata parent] [child child])
        (if (null? child)
            metadata
            (loop (test-alist-set metadata (caar child) (cdar child))
                  (cdr child))))))

  (define $expand-parameter-table
    (lambda (table)
      (let ([names (car table)]
            [rows (cdr table)])
        (map (lambda (row) (map cons names row)) rows))))

  (define $append-map
    (lambda (proc items)
      (let loop ([items items] [out '()])
        (if (null? items)
            (reverse out)
            (loop (cdr items) (append (reverse (proc (car items))) out))))))

  (define $parameter-product
    (lambda (tables)
      (test-check-parameter-tables 'test-expand-parameters tables)
      (let loop ([tables tables] [cases '(() )])
        (if (null? tables)
            cases
            (let ([rows ($expand-parameter-table (car tables))])
              (loop (cdr tables)
                    ($append-map
                     (lambda (case-parameters)
                       (map (lambda (row-parameters)
                              (append case-parameters row-parameters))
                            rows))
                     cases)))))))

  (define $expand-descriptor
    (lambda (descriptor inherited-metadata id)
      (let ([metadata ($metadata-merge inherited-metadata
                                       (test-descriptor-metadata descriptor))])
        (if (test-suite-descriptor? descriptor)
            (let loop ([children (test-descriptor-children descriptor)]
                       [id id]
                       [out '()])
              (if (null? children)
                  (values (reverse out) id)
                  (let-values ([(child-cases next-id)
                                ($expand-descriptor (car children) metadata id)])
                    (loop (cdr children)
                          next-id
                          (append (reverse child-cases) out)))))
            (let ([parameter-cases
                   ($parameter-product (test-metadata-ref metadata 'parameterize '()))])
              (let loop ([parameter-cases parameter-cases]
                         [id id]
                         [out '()])
                (if (null? parameter-cases)
                    (values (reverse out) id)
                    (loop (cdr parameter-cases)
                          (fx+ id 1)
                          (cons (make-test-concrete-case
                                 id
                                 (test-descriptor-name descriptor)
                                 descriptor
                                 (car parameter-cases)
                                 metadata)
                                out)))))))))

  (define $expand-descriptors
    (lambda (descriptors)
      (let loop ([descriptors descriptors] [id 0] [out '()])
        (if (null? descriptors)
            (reverse out)
            (let-values ([(cases next-id)
                          ($expand-descriptor (car descriptors) '() id)])
              (loop (cdr descriptors) next-id (append (reverse cases) out)))))))

  (define $string-prefix?
    (lambda (prefix string)
      (let ([prefix-length (string-length prefix)]
            [string-length (string-length string)])
        (and (fx<= prefix-length string-length)
             (let loop ([index 0])
               (or (fx= index prefix-length)
                   (and (char=? (string-ref prefix index)
                                (string-ref string index))
                        (loop (fx+ index 1)))))))))

  (define $all-tags-present?
    (lambda (required present)
      (let loop ([required required])
        (or (null? required)
            (and (memq (car required) present)
                 (loop (cdr required)))))))

  (define $test-selectors?
    (lambda (selectors)
      (and (list? selectors)
           (let loop ([selectors selectors])
             (or (null? selectors)
                 (let ([selector (car selectors)])
                   (and (pair? selector)
                        (case (car selector)
                          [(name-prefix) (string? (cdr selector))]
                          [(include-tags) (list? (cdr selector))]
                          [else #f])
                        (loop (cdr selectors)))))))))

  (define $case-selected?
    (lambda (concrete-case selectors)
      (let loop ([selectors selectors])
        (if (null? selectors)
            #t
            (let ([selector (car selectors)])
              (case (car selector)
                [(name-prefix)
                 (and ($string-prefix?
                       (cdr selector)
                       (symbol->string (test-concrete-case-name concrete-case)))
                      (loop (cdr selectors)))]
                [(include-tags)
                 (and ($all-tags-present?
                       (cdr selector)
                       (test-metadata-ref
                        (test-concrete-case-metadata concrete-case)
                        'tags
                        '()))
                      (loop (cdr selectors)))]
                [else (errorf 'test-select "unknown selector: ~a" (car selector))]))))))

  #|proc:test-expand-parameters
The `test-expand-parameters` procedure returns concrete test cases expanded
from `descriptor`. For suite descriptors, child descriptors are flattened and
inherit suite metadata, with child metadata overriding duplicate parent keys.
For case, expansion, and compile descriptors, `parameterize` metadata supplies
parameter tables. Each table has the form `((name ...) row ...)`, and multiple
tables are combined as a Cartesian product in table order.
Names must be symbols unique across all tables; malformed tables and rows are rejected
before expansion, including tables that would otherwise produce no cases.
|#
  (define test-expand-parameters
    (lambda (descriptor)
      (pcheck ([test-descriptor? descriptor])
              (let-values ([(cases next-id) ($expand-descriptor descriptor '() 0)])
                cases))))

  #|proc:test-select
The `test-select` procedure returns concrete test cases from `registry` that
match `selectors`. `registry` is a test registry. `selectors` is an association
list; `(name-prefix . string)` keeps cases whose symbolic name starts with the
string, and `(include-tags . (tag ...))` keeps cases whose metadata `tags`
contain every listed tag.
|#
  (define test-select
    (lambda (registry selectors)
      (pcheck ([test-registry? registry] [$test-selectors? selectors])
              (let ([cases
                     ($expand-descriptors (test-registry-descriptors registry))])
                (let loop ([cases cases] [out '()])
                  (cond
                   [(null? cases) (reverse out)]
                   [($case-selected? (car cases) selectors)
                    (loop (cdr cases) (cons (car cases) out))]
                   [else (loop (cdr cases) out)]))))))

  #|proc:test-list
The `test-list` procedure returns concrete test cases from `registry` selected
by `selectors`. `registry` is a test registry, and `selectors` accepts the same
association-list filters as `test-select`.
|#
  (define test-list
    (lambda (registry selectors)
      (pcheck ([test-registry? registry] [$test-selectors? selectors])
              (test-select registry selectors))))

  (define $call-body
    (lambda (body parameters)
      (let ([arity-mask (procedure-arity-mask body)])
        (unless (or (logbit? 1 arity-mask) (logbit? 0 arity-mask))
          (errorf 'test-run "body must accept zero or one argument: ~a" body))
        (apply body (if (logbit? 1 arity-mask) (list parameters) '())))))

  (define $rule-active?
    (lambda (metadata key parameters)
      (let ([rule (test-metadata-ref metadata key #f)])
        (and rule
             (let ([predicate (car rule)])
               ;; Macro-generated metadata stores procedures directly, but
               ;; low-level tests may use quoted lambda data.
               ((if (procedure? predicate)
                    predicate
                    (eval predicate (environment '(chezscheme))))
                parameters))))))

  (define $rule-message
    (lambda (metadata key)
      (let ([rule (test-metadata-ref metadata key #f)])
        (if rule (cdr rule) ""))))

  (define $run-body-with-fixtures
    (lambda (metadata body parameters)
      (let ([fixture-values '()] [failed? #f])
        (dynamic-wind
          (lambda () (void))
          (lambda ()
            (guard (condition [else (set! failed? #t) (raise condition)])
              ($run-allowing-warnings metadata
                (lambda ()
                  (for-each
                   (lambda (fixture)
                     (let ([value ((cdr fixture))])
                       (set! fixture-values (cons (cons (car fixture) value) fixture-values))))
                   (test-metadata-ref metadata 'fixtures '()))
                  (let ([fixtures (reverse fixture-values)]
                        [before (test-metadata-ref metadata 'before #f)])
                    (when before (before fixtures))
                    ($call-body body (append fixtures parameters)))))))
          (lambda ()
            (guard (condition [else (unless failed? (raise condition))])
              ($run-allowing-warnings metadata
                (lambda ()
                  (let ([after (test-metadata-ref metadata 'after #f)])
                    (when after (after (reverse fixture-values))))))))))))

  (define $case-result
    (lambda (concrete-case status message condition stdout stderr)
      (make-test-result
       (test-concrete-case-id concrete-case)
       (test-concrete-case-name concrete-case)
       status
       message
       condition
       stdout
       stderr
       (test-concrete-case-parameters concrete-case)
       (test-metadata-ref (test-concrete-case-metadata concrete-case) 'source #f))))

  (define $classify-condition
    (lambda (metadata condition)
      (let ([handler (test-metadata-ref metadata 'condition-handler #f)]
            [warnings (test-metadata-ref metadata 'warnings 'allow)])
        (cond
         [handler
          (case (handler condition)
            [(handled) 'passed]
            [(failed) 'failed]
            [else 'errored])]
         [(and (warning? condition) (eq? warnings 'error)) 'failed]
         [(warning? condition) 'passed]
         [(test-failure? condition) 'failed]
         [else 'errored]))))

  (define $condition-message
    (lambda (status condition)
      (cond
       [(test-failure? condition) (test-failure-message condition)]
       [(eq? status 'errored) "unexpected condition"]
       [else ""])))

  (define $status-result
    (lambda (concrete-case status condition xfail? xfail-message stdout stderr)
      (cond
       [(eq? status 'passed)
        ($case-result concrete-case (if xfail? 'xpass 'passed)
                      (if xfail? xfail-message "") condition stdout stderr)]
       [xfail? ($case-result concrete-case 'xfail xfail-message condition stdout stderr)]
       [else
        ($case-result concrete-case status ($condition-message status condition)
                      condition stdout stderr)])))

  (define $success-result
    (lambda (concrete-case metadata condition xfail? xfail-message stdout stderr)
      ;; Output assertions are runner checks, not body conditions for the custom handler.
      (guard (condition
              [else ($status-result concrete-case
                                    (if (test-failure? condition) 'failed 'errored)
                                    condition xfail? xfail-message stdout stderr)])
        (test-check-output-expectations metadata stdout stderr)
        ($status-result concrete-case 'passed condition xfail? xfail-message stdout stderr))))

  (define $requirement-skip-message
    (lambda (metadata)
      (let ([version (test-metadata-ref metadata 'requires-chez-version #f)]
            [file (test-metadata-ref metadata 'requires-file #f)])
        (cond
         [(and version (chez-version<? (chez-version) version))
          (format "requires Chez >= ~a.~a.~a" (car version) (cadr version) (caddr version))]
         [(and file (not (file-exists? file)))
          (format "requires file: ~a" file)]
         [else #f]))))

  (define $requirement-result
    (lambda (concrete-case metadata)
      (let ([message ($requirement-skip-message metadata)])
        (and message
             ($case-result concrete-case 'skipped message #f "" "")))))

  (define $expected-condition-predicate
    (lambda (metadata)
      (let ([predicate (test-metadata-ref metadata 'raises #f)])
        (cond
         [(procedure? predicate) predicate]
         [predicate (eval predicate (environment '(chezscheme)))]
         [else #f]))))

  (define $run-with-expected-condition
    (lambda (metadata thunk)
      (let ([predicate ($expected-condition-predicate metadata)])
        (if predicate
            (let ([matched?
                   ;; Escape on a match without resuming the body after an expected warning.
                   (call/1cc
                    (lambda (matched)
                      (with-exception-handler
                        (lambda (condition)
                          (cond
                           [(predicate condition) (matched #t)]
                           [($allowed-warning? metadata condition) (void)]
                           [else
                            (test-fail "expected matching condition" predicate condition)]))
                        (lambda () (thunk) #f))))])
              (unless matched?
                (test-fail "expected matching condition" predicate 'no-condition)))
            (thunk)))))

  (define $run-phase-body
    (lambda (concrete-case)
      (let* ([descriptor (test-concrete-case-descriptor concrete-case)]
             [metadata (test-concrete-case-metadata concrete-case)])
        (case (test-descriptor-phase descriptor)
          [(runtime)
           ($run-body-with-fixtures metadata
             (lambda (parameters)
               ($run-with-expected-condition metadata
                 (lambda () ($call-body (test-descriptor-body descriptor) parameters))))
             (test-concrete-case-parameters concrete-case))]
          [(expand)
           ($run-with-expected-condition
            metadata (lambda () (expand (test-descriptor-body descriptor)
                                         (environment '(cspp)))))]
          [(compile)
           ($run-with-expected-condition metadata
             (lambda () ($compile-case descriptor)))]
          [else (errorf 'test-run "unsupported test phase: ~a"
                        (test-descriptor-phase descriptor))]))))

  (define $allowed-warning?
    (lambda (metadata condition)
      (and (warning? condition)
           (not (test-metadata-ref metadata 'condition-handler #f))
           (not (eq? (test-metadata-ref metadata 'warnings 'allow) 'error)))))

  (define $run-allowing-warnings
    (lambda (metadata thunk)
      (with-exception-handler
        (lambda (condition)
          (if ($allowed-warning? metadata condition)
              (void)
              (raise condition)))
        thunk)))

  (define $strict-xpass-result
    (lambda (concrete-case config result)
      (if (and (eq? (test-result-status result) 'xpass)
               (test-metadata-ref (test-concrete-case-metadata concrete-case)
                                  'xfail-strict (test-config-xfail-strict? config)))
          ($case-result concrete-case 'failed "unexpected pass (strict xfail)" #f
                        (test-result-stdout result) (test-result-stderr result))
          result)))

  (define $run-case-with-policy
    (lambda (concrete-case config)
      (let* ([metadata (test-concrete-case-metadata concrete-case)]
             [parameters (test-concrete-case-parameters concrete-case)]
             [xfail? #f]
             [xfail-message ""])
        (guard (condition
                [else ($case-result concrete-case 'errored "unexpected policy condition"
                                    condition "" "")])
          (cond
           [($requirement-result concrete-case metadata)]
           [($rule-active? metadata 'skip-when parameters)
            ($case-result concrete-case 'skipped ($rule-message metadata 'skip-when) #f "" "")]
           [else
            (set! xfail? ($rule-active? metadata 'xfail-when parameters))
            (set! xfail-message ($rule-message metadata 'xfail-when))
            (let* ([captured
                    (call-with-values
                      (lambda ()
                        (test-capture-ports
                         (test-metadata-ref metadata 'capture #f)
                         (lambda ()
                           (guard (condition [else (list 'condition condition)])
                             ($run-allowing-warnings metadata
                               (lambda () ($run-phase-body concrete-case)))
                             '(success)))))
                      list)]
                   [outcome (car (car captured))]
                   [stdout (cadr captured)]
                   [stderr (caddr captured)])
              ($strict-xpass-result
               concrete-case config
               (guard (condition
                       [else ($case-result concrete-case 'errored "unexpected policy condition"
                                           condition stdout stderr)])
                 (let* ([raised? (eq? (car outcome) 'condition)]
                        [condition (and raised? (cadr outcome))]
                        [status (if raised? ($classify-condition metadata condition) 'passed)])
                   (if (eq? status 'passed)
                       ($success-result concrete-case metadata condition
                                        xfail? xfail-message stdout stderr)
                       ($status-result concrete-case status condition
                                       xfail? xfail-message stdout stderr))))))])))))

  (define $compile-temp-path
    (lambda ()
      (format "test-compile-~a-~a.ss"
              (random 1000000)
              (time-nanosecond (current-time)))))

  (define $compile-output-path
    (lambda (path)
      (let ([length (string-length path)])
        (if (and (fx>= length 3)
                 (string=? (substring path (fx- length 3) length) ".ss"))
            (string-append (substring path 0 (fx- length 3)) ".so")
            (string-append path ".so")))))

  (define $compile-case
    (lambda (descriptor)
      (let ([path ($compile-temp-path)])
        (dynamic-wind
          (lambda () (void))
          (lambda ()
            (call-with-output-file path
              (lambda (out)
                ;; Compiler input must retain complete source regardless of diagnostic limits.
                (parameterize ([print-graph #t] [print-level #f] [print-length #f])
                  (for-each (lambda (form) (write form out) (newline out))
                            (test-descriptor-body descriptor))))
              'replace)
            (parameterize ([compile-file-message #f] [generate-wpo-files #f])
              (compile-file path)))
          (lambda ()
            (when (file-exists? path) (delete-file path))
            (let ([output-path ($compile-output-path path)])
              (when (file-exists? output-path) (delete-file output-path))))))))

  (define $stop-after-result?
    (lambda (config result)
      (and (test-config-stop-on-failure? config)
           (memq (test-result-status result) '(failed errored)))))

  #|proc:test-run
  The `test-run` procedure executes `tests` using runner configuration `config`
  and returns a list of test result records. `tests` must contain either all
  descriptors or all concrete cases (for example, from `test-select`). Concrete
  cases run in supplied order without re-expansion, preserving IDs, parameters,
  and inherited metadata. An empty list returns no results. Mixed lists are rejected.
  Allowed continuable warnings resume the case body unless accepted by `raises`.
  A `raises` predicate is called once per condition; rejected allowed warnings resume
  until a matching condition is raised. Skip and expected-failure rules apply to every phase. An
  unexpected pass has status `failed` under effective strict xfail policy and
  `xpass` otherwise; only `failed` and `errored` stop a stop-on-failure run.
  |#
  (define test-run
    (lambda (tests config)
      (pcheck ([list? tests] [test-config? config])
              (let* ([cases (cond
                             [(andmap test-concrete-case? tests) tests]
                             [(andmap test-descriptor? tests) ($expand-descriptors tests)]
                             [else (errorf 'test-run
                                     "tests must all be descriptors or all concrete cases: ~a"
                                     tests)])]
                     [results
                      (let loop ([cases cases] [out '()])
                        (if (null? cases)
                            (reverse out)
                            (let ([result ($run-case-with-policy (car cases) config)])
                              (if ($stop-after-result? config result)
                                  (reverse (cons result out))
                                  (loop (cdr cases) (cons result out))))))])
                (when (and (test-config-reporter config)
                           (test-config-output config))
                  (parameterize ([test-current-report-color (test-config-color config)])
                    (test-report (test-config-reporter config)
                                 (test-config-output config)
                                 (test-summarize results))))
                results))))

  #|proc:test-run-registry
  The `test-run-registry` procedure executes every descriptor registered in
  `registry` using runner `config` and returns a list of test result records.
  Case policies are the same as those described by `test-run`.
  |#
  (define test-run-registry
    (lambda (registry config)
      (pcheck ([test-registry? registry] [test-config? config])
              (test-run (test-registry-descriptors registry) config))))

  #|proc:test-run-files
The `test-run-files` procedure loads each path in `paths`, collects tests
registered in a fresh registry, and executes them with `config`. `paths` is a
list of file path strings, and `config` is a test runner configuration.
|#
  (define test-run-files
    (lambda (paths config)
      (pcheck ([list? paths] [test-config? config])
              (let ([registry (make-test-registry)])
                (parameterize ([current-test-registry registry])
                  (for-each
                   (lambda (path)
                     (unless (string? path)
                       (errorf 'test-run-files "path is not a string: ~a" path))
                     (load path))
                   paths)
                  (test-run-registry registry config))))))

  (define $parse-test-main-argv
    (lambda (argv)
      (let loop ([argv argv] [reporter 'text] [files '()])
        (cond
         [(null? argv) (values reporter (reverse files))]
         [(string=? (car argv) "--reporter")
          (unless (pair? (cdr argv))
            (errorf 'test-main "--reporter requires text, datum, or progress"))
          (let ([reporter (string->symbol (cadr argv))])
            (unless (memq reporter '(text datum progress))
              (errorf 'test-main "unknown reporter: ~a" (cadr argv))))
          (loop (cddr argv) (string->symbol (cadr argv)) files)]
         [else
          (loop (cdr argv) reporter (cons (car argv) files))]))))

  #|proc:test-main
The `test-main` procedure runs test files described by `argv` and returns a
process-style status code. `argv` is a list of strings; it accepts
`--reporter text`, `--reporter datum`, `--reporter progress`, and file paths.
Unknown reporter names and missing reporter arguments raise an error.
|#
  (define test-main
    (lambda (argv)
      (pcheck ([(lambda (value) (and (list? value) (for-all string? value))) argv])
              (call-with-values
                (lambda () ($parse-test-main-argv argv))
                (lambda (reporter-name files)
                  (let* ([reporter (case reporter-name
                                     [(datum) (test-datum-reporter)]
                                     [(progress) (test-progress-reporter 'auto)]
                                     [else (test-text-reporter)])]
                         [config (test-config-with
                                  (default-test-config)
                                  `((reporter . ,reporter)
                                    (output . ,(current-output-port))))])
                    (let ([results (test-run-files files config)])
                      (if (exists (lambda (result)
                                    (memq (test-result-status result)
                                          '(failed errored)))
                                  results)
                          1
                          0))))))))

  )
