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

  (define $parameter-row
    (lambda (names row)
      (let loop ([names names] [row row] [out '()])
        (cond
         [(null? names) (reverse out)]
         [(null? row)
          (errorf 'test-expand-parameters "parameter row is shorter than names: ~a" row)]
         [else
          (loop (cdr names) (cdr row) (cons (cons (car names) (car row)) out))]))))

  (define $expand-parameter-table
    (lambda (table)
      (let ([names (car table)]
            [rows (cdr table)])
        (map (lambda (row) ($parameter-row names row)) rows))))

  (define $append-map
    (lambda (proc items)
      (let loop ([items items] [out '()])
        (if (null? items)
            (reverse out)
            (loop (cdr items) (append (reverse (proc (car items))) out))))))

  (define $parameter-product
    (lambda (tables)
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
                          [else #t])
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

  (define $body-accepts-parameters?
    (lambda (body)
      (let ([arity (procedure-arity body)])
        (cond
         [(eq? arity #f) #t]
         [(integer? arity) (not (fx= arity 0))]
         [(list? arity) (or (memq 1 arity)
                            (exists (lambda (entry)
                                      (and (pair? entry)
                                           (<= (car entry) 1)))
                                    arity))]
         [else #t]))))

  (define $call-body
    (lambda (body parameters)
      (if ($body-accepts-parameters? body)
          (body parameters)
          (body))))

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

  (define $setup-fixtures
    (lambda (metadata)
      (map (lambda (fixture)
             (cons (car fixture) ((cdr fixture))))
           (test-metadata-ref metadata 'fixtures '()))))

  (define $teardown-fixtures
    (lambda (metadata fixture-values)
      (let ([after (test-metadata-ref metadata 'after #f)])
        (when after
          (after fixture-values)))))

  (define $run-body-with-fixtures
    (lambda (metadata body parameters)
      (let ([fixture-values '()])
        (dynamic-wind
          (lambda ()
            (set! fixture-values ($setup-fixtures metadata))
            (let ([before (test-metadata-ref metadata 'before #f)])
              (when before
                (before fixture-values))))
          (lambda ()
            ($call-body body (append fixture-values parameters)))
          (lambda ()
            ($teardown-fixtures metadata fixture-values))))))

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

  (define $condition-result
    (lambda (concrete-case metadata condition xfail? xfail-message stdout stderr)
      (let ([status ($classify-condition metadata condition)])
        (cond
         [xfail? ($case-result concrete-case 'xfail xfail-message condition stdout stderr)]
         [else
          ($case-result concrete-case
                        status
                        ($condition-message status condition)
                        condition
                        stdout
                        stderr)]))))

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

  (define $run-runtime-case
    (lambda (concrete-case config)
      (let* ([metadata (test-concrete-case-metadata concrete-case)]
             [parameters (test-concrete-case-parameters concrete-case)])
        (cond
         [($requirement-result concrete-case metadata)]
         [($rule-active? metadata 'skip-when parameters)
          ($case-result concrete-case 'skipped ($rule-message metadata 'skip-when) #f "" "")]
         [else
          (let ([xfail? ($rule-active? metadata 'xfail-when parameters)]
                [xfail-message ($rule-message metadata 'xfail-when)])
            (guard (condition
                    [else
                     ($condition-result concrete-case
                                        metadata
                                        condition
                                        xfail?
                                        xfail-message
                                        ""
                                        "")])
              (let ([capture-mode (test-metadata-ref metadata 'capture #f)]
                    [body (lambda ()
                            ($run-body-with-fixtures
                             metadata
                             (test-descriptor-body (test-concrete-case-descriptor concrete-case))
                             parameters))])
                (if capture-mode
                    (call-with-values
                      (lambda ()
                        (test-capture-ports capture-mode body))
                      (lambda (values-list stdout stderr)
                        (test-check-output-expectations metadata stdout stderr)
                        (if xfail?
                            ($case-result concrete-case 'xpass xfail-message #f stdout stderr)
                            ($case-result concrete-case 'passed "" #f stdout stderr))))
                    (begin
                      (body)
                      (test-check-output-expectations metadata "" "")
                      (if xfail?
                          ($case-result concrete-case 'xpass xfail-message #f "" "")
                          ($case-result concrete-case 'passed "" #f "" "")))))))]))))

  (define $expected-condition-predicate
    (lambda (metadata)
      (let ([predicate (test-metadata-ref metadata 'raises #f)])
        (cond
         [(procedure? predicate) predicate]
         [(symbol? predicate) (eval predicate (environment '(chezscheme)))]
         [predicate (eval predicate (environment '(chezscheme)))]
         [else #f]))))

  (define $run-with-expected-condition
    (lambda (concrete-case thunk)
      (let* ([metadata (test-concrete-case-metadata concrete-case)]
             [predicate ($expected-condition-predicate metadata)])
        (if predicate
            (guard (condition
                    [else
                     (if (predicate condition)
                         ($case-result concrete-case 'passed "" condition "" "")
                         ($condition-result concrete-case
                                            metadata
                                            condition
                                            #f
                                            ""
                                            ""
                                            ""))])
              (thunk)
              ($case-result concrete-case 'failed "expected condition was not raised" #f "" ""))
            (guard (condition
                    [else
                     ($condition-result concrete-case
                                        metadata
                                        condition
                                        #f
                                        ""
                                        ""
                                        "")])
              (thunk)
              ($case-result concrete-case 'passed "" #f "" ""))))))

  (define $run-expand-case
    (lambda (concrete-case config)
      (or ($requirement-result concrete-case (test-concrete-case-metadata concrete-case))
          ($run-with-expected-condition
           concrete-case
           (lambda ()
             (expand (test-descriptor-body (test-concrete-case-descriptor concrete-case))))))))

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

  (define $run-compile-case
    (lambda (concrete-case config)
      (or ($requirement-result concrete-case (test-concrete-case-metadata concrete-case))
          ($run-with-expected-condition
           concrete-case
           (lambda ()
             (let ([path ($compile-temp-path)])
               (dynamic-wind
                 (lambda () #f)
                 (lambda ()
                   (call-with-output-file path
                     (lambda (out)
                       (for-each
                        (lambda (form)
                          (write form out)
                          (newline out))
                        (test-descriptor-body (test-concrete-case-descriptor concrete-case))))
                     'replace)
                   (compile-file path))
                 (lambda ()
                   (when (file-exists? path)
                     (delete-file path))
                   (let ([output-path ($compile-output-path path)])
                     (when (file-exists? output-path)
                       (delete-file output-path)))))))))))

  (define $run-concrete-case
    (lambda (concrete-case config)
      (case (test-descriptor-phase (test-concrete-case-descriptor concrete-case))
        [(runtime) ($run-runtime-case concrete-case config)]
        [(expand) ($run-expand-case concrete-case config)]
        [(compile) ($run-compile-case concrete-case config)]
        [else ($case-result concrete-case 'errored "unsupported test phase" #f "" "")])))

  (define $stop-after-result?
    (lambda (config result)
      (and (test-config-stop-on-failure? config)
           (memq (test-result-status result) '(failed errored xpass)))))

  #|proc:test-run
The `test-run` procedure executes test `descriptors` using runner `config` and
returns a list of test result records.
|#
  (define test-run
    (lambda (descriptors config)
      (pcheck ([list? descriptors] [test-config? config])
              (unless (andmap test-descriptor? descriptors)
                (errorf 'test-run "descriptors must all be test descriptors: ~a" descriptors))
              (let* ([cases ($expand-descriptors descriptors)]
                     [results
                      (let loop ([cases cases] [out '()])
                        (if (null? cases)
                            (reverse out)
                            (let ([result ($run-concrete-case (car cases) config)])
                              (if ($stop-after-result? config result)
                                  (reverse (cons result out))
                                  (loop (cdr cases) (cons result out))))))])
                (when (and (test-config-reporter config)
                           (test-config-output config))
                  (test-report (test-config-reporter config)
                               (test-config-output config)
                               (test-summarize results)))
                results))))

  #|proc:test-run-registry
The `test-run-registry` procedure executes every descriptor registered in
`registry` using runner `config` and returns a list of test result records.
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
         [(and (pair? (cdr argv)) (string=? (car argv) "--reporter"))
          (loop (cddr argv) (string->symbol (cadr argv)) files)]
         [else
          (loop (cdr argv) reporter (cons (car argv) files))]))))

  #|proc:test-main
The `test-main` procedure runs test files described by `argv` and returns a
process-style status code. `argv` is a list of strings; it accepts
`--reporter text`, `--reporter datum`, and file paths.
|#
  (define test-main
    (lambda (argv)
      (pcheck ([list? argv])
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
                                          '(failed errored xpass)))
                                  results)
                          1
                          0))))))))

  )
