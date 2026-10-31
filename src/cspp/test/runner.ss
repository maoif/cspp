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

  (define $check-fixture-bindings
    (lambda (metadata parameter-names)
      (test-check-fixture-names 'test-run
        (map car (test-metadata-ref metadata 'fixtures '())) parameter-names)))

  (define-record-type ($test-leaf $make-test-leaf $test-leaf?)
    (fields descriptor metadata tables count selected?))

  (define $prepare-descriptors
    (lambda (descriptors selectors)
      (let ([leaves '()])
        (define prepare
          (lambda (descriptor inherited)
            (let ([metadata ($metadata-merge inherited (test-descriptor-metadata descriptor))])
              (if (test-suite-descriptor? descriptor)
                  (for-each (lambda (child) (prepare child metadata))
                            (test-descriptor-children descriptor))
                  (let* ([tables (test-metadata-ref metadata 'parameterize '())]
                         [selected? ($descriptor-selected?
                                      (test-descriptor-name descriptor) metadata selectors)])
                    ;; Validate every leaf before executing any body, including excluded leaves.
                    (test-check-parameter-tables 'test-expand-parameters tables)
                    ($check-fixture-bindings metadata (apply append (map car tables)))
                    (set! leaves
                      (cons ($make-test-leaf
                              descriptor metadata
                              (and selected?
                                   ;; Snapshot caller-owned table rows before any body runs.
                                   (map (lambda (table)
                                          (cons (list-copy (car table)) (map list-copy (cdr table))))
                                        tables))
                              (fold-left (lambda (count table) (* count (length (cdr table))))
                                         1 tables)
                              selected?)
                            leaves)))))))
        (for-each (lambda (descriptor) (prepare descriptor '())) descriptors)
        (reverse leaves))))

  (define $walk-parameter-tables
    (lambda (tables visit)
      (or (exists (lambda (axis) (null? (cdr axis))) tables)
          (let loop ([tables tables] [reversed '()])
            (if (null? tables)
                (visit (reverse reversed))
                (for-all
                  (lambda (row)
                    (loop (cdr tables)
                          (fold-left (lambda (parameters name value)
                                       (cons (cons name value) parameters))
                                     reversed (caar tables) row)))
                  (cdar tables)))))))

  (define $walk-prepared-cases
    (lambda (leaves visit)
      (let ([id 0])
        (for-all
          (lambda (leaf)
            (if ($test-leaf-selected? leaf)
                ($walk-parameter-tables ($test-leaf-tables leaf)
                  (lambda (parameters)
                    (let* ([descriptor ($test-leaf-descriptor leaf)]
                           [concrete (make-test-concrete-case
                                       id (test-descriptor-name descriptor) descriptor parameters
                                       ($test-leaf-metadata leaf))])
                      (set! id (+ id 1))
                      (visit concrete))))
                (begin (set! id (+ id ($test-leaf-count leaf))) #t)))
          leaves))))

  (define $collect-cases
    (lambda (descriptors selectors)
      (let ([cases '()])
        ($walk-prepared-cases ($prepare-descriptors descriptors selectors)
          (lambda (concrete) (set! cases (cons concrete cases)) #t))
        (reverse cases))))

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
                          [(include-tags)
                           (and (list? (cdr selector)) (andmap symbol? (cdr selector)))]
                          [else #f])
                        (loop (cdr selectors)))))))))

  (define $descriptor-selected?
    (lambda (name metadata selectors)
      (let loop ([selectors selectors])
        (if (null? selectors)
            #t
            (let ([selector (car selectors)])
              (case (car selector)
                [(name-prefix)
                 (and ($string-prefix?
                       (cdr selector)
                       (symbol->string name))
                      (loop (cdr selectors)))]
                [(include-tags)
                 (and ($all-tags-present?
                       (cdr selector)
                       (test-metadata-ref metadata 'tags '()))
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
              ($collect-cases (list descriptor) '()))))

  #|proc:test-select
The `test-select` procedure returns concrete test cases from `registry` that
match `selectors`. `registry` is a test registry. `selectors` is an association
list; `(name-prefix . string)` keeps cases whose symbolic name starts with the
string, and `(include-tags . (tag ...))` keeps cases whose metadata `tags`
contain every listed tag. Descriptor filters run before parameter products are materialized;
concrete case IDs retain their positions in the complete unfiltered expansion.
|#
  (define test-select
    (lambda (registry selectors)
      (pcheck ([test-registry? registry] [$test-selectors? selectors])
              ($collect-cases (test-registry-descriptors registry) selectors))))

  #|proc:test-list
The `test-list` procedure returns concrete test cases from `registry` selected
by `selectors`. `registry` is a test registry, and `selectors` accepts the same
association-list filters as `test-select`.
|#
  (define test-list
    (lambda (registry selectors)
      (pcheck ([test-registry? registry] [$test-selectors? selectors])
              (test-select registry selectors))))

  (define $current-predicate-cache (make-parameter #f))

  (define $resolve-predicate
    (lambda (predicate)
      (if (procedure? predicate)
          predicate
          (let* ([cache ($current-predicate-cache)]
                 [cached (and cache (hashtable-ref cache predicate #f))])
            (or cached
                (let ([callback (eval predicate (environment '(chezscheme)))])
                  (unless (test-callback-accepts? callback 1)
                    (errorf 'test-run "condition predicate must accept one argument: ~s" predicate))
                  (when cache (hashtable-set! cache predicate callback))
                  callback))))))

  (define $rule-active?
    (lambda (metadata key parameters)
      (let ([rule (test-metadata-ref metadata key #f)])
        (and rule
             (let ([predicate (car rule)])
               ;; Macro-generated metadata stores procedures directly, but
               ;; low-level tests may use quoted lambda data.
               (($resolve-predicate predicate) parameters))))))

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
              (let ([inputs
                     ($run-allowing-warnings metadata
                       (lambda ()
                         (for-each
                          (lambda (fixture)
                            (let ([value ((cdr fixture))])
                              (set! fixture-values
                                (cons (cons (car fixture) value) fixture-values))))
                          (test-metadata-ref metadata 'fixtures '()))
                         (let ([fixtures (reverse fixture-values)]
                               [before (test-metadata-ref metadata 'before #f)])
                           (when before (before fixtures))
                           (append fixtures parameters))))])
                (body inputs))))
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

  ;; Tag body conditions while they unwind through cleanup. Keep the original raised
  ;; object intact, and distinguish a later preparation or cleanup condition from it.
  (define-record-type ($body-condition $make-body-condition $body-condition?)
    (fields condition))

  (define $run-tested-body
    (lambda (metadata thunk)
      (guard (condition [else (raise ($make-body-condition condition))])
        (if (test-metadata-ref metadata 'condition-handler #f)
            (thunk)
            ($run-allowing-warnings metadata thunk)))))

  (define $classify-condition
    (lambda (metadata condition body-condition?)
      (let ([handler (and body-condition?
                          (test-metadata-ref metadata 'condition-handler #f))]
            [warnings (test-metadata-ref metadata 'warnings 'allow)])
        (cond
         [handler
          (case (handler condition)
            [(handled) 'passed]
            [(failed) 'failed]
            [else 'errored])]
         [(test-failure? condition) 'failed]
         [(serious-condition? condition) 'errored]
         [(and (warning? condition) (eq? warnings 'error)) 'failed]
         [(warning? condition) 'passed]
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
         [predicate ($resolve-predicate predicate)]
         [else #f]))))

  (define $run-with-expected-condition
    (lambda (metadata thunk)
      (let ([predicate ($expected-condition-predicate metadata)])
        ($run-tested-body metadata
          (lambda ()
            (if predicate
                (let ([matched?
                       ;; Escape on a match without resuming the body after an expected warning.
                       (call/1cc
                        (lambda (matched)
                          (with-exception-handler
                            (lambda (condition)
                              (cond
                               [(predicate condition) (matched #t)]
                               [(and (not (test-metadata-ref metadata 'condition-handler #f))
                                     ($allowed-warning? metadata condition))
                                (void)]
                               [else
                                (test-fail "expected matching condition" predicate condition)]))
                            (lambda () (thunk) #f))))])
                  (unless matched?
                    (test-fail "expected matching condition" predicate 'no-condition)))
                (thunk)))))))

  (define $run-phase-body
    (lambda (concrete-case)
      (let* ([descriptor (test-concrete-case-descriptor concrete-case)]
             [metadata (test-concrete-case-metadata concrete-case)])
        (case (test-descriptor-phase descriptor)
          [(runtime)
           ($run-body-with-fixtures metadata
             (lambda (parameters)
               (let ([thunk (test-prepare-runtime-body descriptor parameters)])
                 ($run-with-expected-condition metadata thunk)))
             (test-concrete-case-parameters concrete-case))]
          [(expand)
           ($run-with-expected-condition
            metadata (lambda () (expand (test-descriptor-body descriptor)
                                         (environment '(cspp)))))]
          [(compile) ($compile-case descriptor metadata)]
          [else (errorf 'test-run "unsupported test phase: ~a"
                        (test-descriptor-phase descriptor))]))))

  (define $allowed-warning?
    (lambda (metadata condition)
      (and (warning? condition)
           (not (serious-condition? condition))
           (not (test-failure? condition))
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
          ($check-fixture-bindings metadata (map car parameters))
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
                           (guard (condition
                                   [($body-condition? condition)
                                    (list 'body-condition ($body-condition-condition condition))]
                                   [else (list 'condition condition)])
                             ($run-phase-body concrete-case)
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
                 (let* ([raised? (not (eq? (car outcome) 'success))]
                        [condition (and raised? (cadr outcome))]
                        [status (if raised?
                                    ($classify-condition metadata condition
                                                         (eq? (car outcome) 'body-condition))
                                    'passed)])
                   (if (eq? status 'passed)
                       ($success-result concrete-case metadata condition
                                        xfail? xfail-message stdout stderr)
                       ($status-result concrete-case status condition
                                       xfail? xfail-message stdout stderr))))))])))))

  (define $compile-temp-path
    (lambda ()
      ;; Compile-time user code can change the working directory before cleanup runs.
      (format "~a/test-compile-~a-~a.ss"
              (current-directory)
              (random 1000000)
              (time-nanosecond (current-time)))))

  (define $compile-output-path
    (lambda (path)
      (let ([length (string-length path)])
        (if (and (fx>= length 3)
                 (string=? (substring path (fx- length 3) length) ".ss"))
            (string-append (substring path 0 (fx- length 3)) ".so")
            (string-append path ".so")))))

  (define $close-compile-output
    (lambda (out)
      (guard (raised
              [else
               ;; Discard pending data after a failed flush so the port can close.
               (clear-output-port out)
               (close-output-port out)
               (raise raised)])
        (close-output-port out))))

  (define $compile-source-file
    (lambda (path metadata)
      (let ([in (open-file-input-port path)])
        (dynamic-wind
          (lambda () (void))
          (lambda ()
            (let ([source (make-source-file-descriptor path in #t)])
              (set! in (transcoded-port in (current-transcoder)))
              (let ([out (open-file-output-port ($compile-output-path path)
                           (file-options replace))])
                (dynamic-wind
                  (lambda () (void))
                  (lambda ()
                    ($run-with-expected-condition metadata
                      (lambda ()
                        ;; Own both ports even when an expected warning exits the compiler.
                        ;; No WPO or coverage ports are supplied for temporary source.
                        (parameterize ([compile-file-message #f] [generate-wpo-files #f]
                                       [generate-covin-files #f] [case-sensitive #t])
                          (compile-port in out source)))))
                  (lambda () ($close-compile-output out))))))
          (lambda () (close-input-port in))))))

  (define $compile-case
    (lambda (descriptor metadata)
      (let ([path ($compile-temp-path)])
        (dynamic-wind
          (lambda () (void))
          (lambda ()
            (let ([out (open-output-file path 'replace)])
              (dynamic-wind
                (lambda () (void))
                (lambda ()
                  ;; Compiler input must retain full source and generated identifier identity.
                  (parameterize ([print-graph #t] [print-level #f] [print-length #f]
                                 [print-gensym #t] [case-sensitive #t])
                    (for-each (lambda (form) (write form out) (newline out))
                              (test-descriptor-body descriptor))))
                (lambda () ($close-compile-output out))))
            ($compile-source-file path metadata))
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
  until a matching condition is raised. Quoted predicates are resolved once per run.
  Skip and expected-failure rules apply to every phase. An
  unexpected pass has status `failed` under effective strict xfail policy and
  `xpass` otherwise; only `failed` and `errored` stop a stop-on-failure run.
  Descriptor domains are validated first; concrete rows are then produced incrementally,
  allowing stop-on-failure to avoid allocating later Cartesian combinations.
  Call from the controlling OS thread, outside fibers. Bodies may join worker assertions,
  but must propagate their failures before returning. Worker callbacks must not call runners,
  registries, capture, or reporting APIs. See `docs/test-and-benchmark.md` for the requirements.
  |#
  (define test-run
    (lambda (tests config)
      (pcheck ([list? tests] [test-config? config])
              (test-check-reporter-output 'test-run
                (test-config-reporter config) (test-config-output config))
              (parameterize ([$current-predicate-cache (make-eq-hashtable)])
                (let* ([concrete? (andmap test-concrete-case? tests)]
                       [prepared (cond
                                   [concrete? #f]
                                   [(andmap test-descriptor? tests) ($prepare-descriptors tests '())]
                                   [else (errorf 'test-run
                                           "tests must all be descriptors or all concrete cases: ~s"
                                           tests)])]
                       [reversed '()])
                  (let ([visit (lambda (concrete)
                                 (let ([result ($run-case-with-policy concrete config)])
                                   (set! reversed (cons result reversed))
                                   (not ($stop-after-result? config result))))])
                    (if concrete? (for-all visit tests) ($walk-prepared-cases prepared visit)))
                  (let ([results (reverse reversed)])
                    (when (and (test-config-reporter config) (test-config-output config))
                      (parameterize ([test-current-report-color (test-config-color config)])
                        (test-report (test-config-reporter config)
                                     (test-config-output config) (test-summarize results))))
                    results))))))

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
      (pcheck ([(lambda (value) (and (list? value) (andmap string? value))) paths]
               [test-config? config])
              (test-check-reporter-output 'test-run-files
                (test-config-reporter config) (test-config-output config))
              (let ([registry (make-test-registry)])
                (parameterize ([current-test-registry registry])
                  (for-each
                   (lambda (path)
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
