(import (cspp)
        (cspp test))

(mat test-legacy-optional-requirements
  (begin (load "mat-requires-fixture.ss") #t))

(define test-string-contains?
  (lambda (string needle)
    (let ([string-length (string-length string)]
          [needle-length (string-length needle)])
      (let loop ([index 0])
        (cond
         [(fx> (fx+ index needle-length) string-length) #f]
         [(string=? (substring string index (fx+ index needle-length)) needle) #t]
         [else (loop (fx+ index 1))])))))

(define test-error-message-contains?
  (lambda (thunk needle)
    (guard (condition
            [else
             (test-string-contains?
              (call-with-string-output-port
                (lambda (out) (display-condition condition out)))
              needle)])
      (thunk)
      #f)))

(mat test-descriptors
  (test-case-descriptor?
   (make-test-case 'sample (lambda () #t) '()))
  (test-suite-descriptor?
   (make-test-suite 'suite (list (make-test-case 'child (lambda () #t) '())) '()))
  (test-expand-descriptor?
   (make-test-expand 'expansion '(+ 1 2) '()))
  (test-compile-descriptor?
   (make-test-compile 'compile '((define x 1) x) '()))
  (let ([descriptor (make-test-case 'sample (lambda () #t) '((tags . (fast))))])
    (and (eq? (test-descriptor-name descriptor) 'sample)
         (eq? (test-descriptor-phase descriptor) 'runtime)
         (equal? (test-descriptor-metadata descriptor) '((tags . (fast))))))
  (let ([suite (make-test-suite 'suite (list (make-test-case 'child (lambda () #t) '())) '())])
    (= (length (test-descriptor-children suite)) 1))
  (test-config? (default-test-config))
  (let ([config (test-config-with (default-test-config)
                  '((reporter . #f)
                    (output . #f)
                    (xfail-strict? . #t)
                    (stop-on-failure? . #t)
                    (color . never)))])
    (and (not (test-config-reporter config))
         (not (test-config-output config))
         (test-config-xfail-strict? config)
         (test-config-stop-on-failure? config)
         (eq? (test-config-color config) 'never)))
  ;; Public constructors reject invalid names.
  (error? (make-test-case "bad" (lambda () #t) '()))
  ;; Public constructors reject non-procedure runtime bodies.
  (error? (make-test-case 'bad-body 'not-a-procedure '()))
  ;; Negative config test: option entries must be pairs.
  (test-error-message-contains?
   (lambda () (test-config-with (default-test-config) '(reporter)))
   "invalid config option")
  ;; Negative config test: xfail-strict? must be boolean.
  (test-error-message-contains?
   (lambda () (test-config-with (default-test-config) '((xfail-strict? . yes))))
   "invalid config option")
  ;; Negative config test: color must be auto, always, or never.
  (test-error-message-contains?
   (lambda () (test-config-with (default-test-config) '((color . sometimes))))
   "invalid config option"))

(mat test-assertions
  (begin (test-true #t) #t)
  (begin (test-false #f) #t)
  (begin (test-eq 'a 'a) #t)
  (begin (test-eqv 1 1) #t)
  (begin (test-equal '(1 2) '(1 2)) #t)
  (begin (test-= 3 (+ 1 2)) #t)
  (begin (test-pred string? "abc") #t)
  ;; Negative assertion test: failing truth assertion raises a framework failure.
  (test-failure? (guard (c [else c]) (test-true #f)))
  ;; Negative assertion test: framework failures also carry a standard message condition.
  (let ([condition (guard (c [else c]) (test-true #f))])
    (and (test-failure? condition)
         (message-condition? condition)
         (equal? (condition-message condition) "expected true value")))
  ;; Negative assertion test: mismatched expected and actual values are recorded.
  (let ([condition (guard (c [else c]) (test-equal '(1) '(2)))])
    (and (test-failure? condition)
         (equal? (test-failure-expected condition) '(1))
         (equal? (test-failure-actual condition) '(2))))
  ;; Negative code test: expected violation is accepted.
  (begin
    (test-raises violation?
      (lambda ()
        (vector-ref '#(1 2 3) 9)))
    #t)
  ;; Negative assertion test: missing expected condition fails the assertion.
  (test-failure?
   (guard (c [else c])
     (test-raises violation?
       (lambda () 1))))
  (begin (test-not-raises (lambda () (+ 1 2))) #t))

(mat test-registry-and-expansion
  (let ([registry (make-test-registry)]
        [descriptor (make-test-case 'registered (lambda () #t) '())])
    (test-register! registry descriptor)
    (equal? (test-registry-descriptors registry) (list descriptor)))
  (let ([registry (make-test-registry)])
    (test-register! registry (make-test-case 'a (lambda () #t) '()))
    (test-clear-registry! registry)
    (null? (test-registry-descriptors registry)))
  (let* ([descriptor (make-test-case 'param
                       (lambda () #t)
                       '((parameterize . (((x y) (1 2) (3 4))))))]
         [cases (test-expand-parameters descriptor)])
    (and (= (length cases) 2)
         (equal? (map test-concrete-case-parameters cases)
                 '(((x . 1) (y . 2))
                   ((x . 3) (y . 4))))))
  (let* ([descriptor (make-test-case 'product
                       (lambda () #t)
                       '((parameterize . (((x) (1) (2))
                                          ((y) (a) (b))))))]
         [cases (test-expand-parameters descriptor)])
    (and (= (length cases) 4)
         (equal? (map test-concrete-case-parameters cases)
                 '(((x . 1) (y . a))
                   ((x . 1) (y . b))
                   ((x . 2) (y . a))
                   ((x . 2) (y . b))))))
  (let ([registry (make-test-registry)])
    (test-register! registry (make-test-case 'alpha/one (lambda () #t) '((tags . (fast)))))
    (test-register! registry (make-test-case 'beta/two (lambda () #t) '((tags . (slow)))))
    (equal? (map test-concrete-case-name (test-select registry '((name-prefix . "alpha"))))
            '(alpha/one)))
  (let ([registry (make-test-registry)])
    (test-register! registry (make-test-case 'alpha/one (lambda () #t) '((tags . (fast)))))
    (test-register! registry (make-test-case 'beta/two (lambda () #t) '((tags . (slow)))))
    (equal? (map test-concrete-case-name (test-select registry '((include-tags . (slow)))))
            '(beta/two))))

(define test-silent-config
  (lambda ()
    (test-config-with (default-test-config)
      '((reporter . #f)
        (output . #f)
        (color . never)))))

(mat test-runtime-runner
  (let* ([descriptor (make-test-case 'pass (lambda () (test-true #t)) '())]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 1)
         (eq? (test-result-status (car results)) 'passed)))
  (let* ([descriptor (make-test-case 'fail (lambda () (test-equal 1 2)) '())]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 1)
         (eq? (test-result-status (car results)) 'failed)
         (test-failure? (test-result-condition (car results)))))
  (let* ([descriptor (make-test-case 'error (lambda () (error 'boom "bad")) '())]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 1)
         (eq? (test-result-status (car results)) 'errored)))
  (let* ([descriptor (make-test-case 'skip (lambda () (test-fail "must not run"))
                                     '((skip-when . ((lambda (params) #t) . "disabled"))))]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 1)
         (eq? (test-result-status (car results)) 'skipped)))
  (let* ([descriptor (make-test-case 'xfail (lambda () (test-equal 1 2))
                                     '((xfail-when . ((lambda (params) #t) . "known bug"))))]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 1)
         (eq? (test-result-status (car results)) 'xfail)))
  (let* ([descriptor (make-test-case 'xpass (lambda () (test-true #t))
                                     '((xfail-when . ((lambda (params) #t) . "known bug"))))]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 1)
         (eq? (test-result-status (car results)) 'xpass)))
  (let* ([descriptor (make-test-case 'param-pass
                       (lambda (params)
                         (test-= (cdr (assq 'expected params))
                                 (+ (cdr (assq 'x params)) 1)))
                       '((parameterize . (((x expected) (1 2) (2 3))))))]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 2)
         (andmap (lambda (result) (eq? (test-result-status result) 'passed)) results))))

(mat test-output-capture
  (call-with-values
    (lambda ()
      (test-capture-ports
       '(stdout stderr)
       (lambda ()
         (display "hello")
         (display "bad" (current-error-port)))))
    (lambda (values stdout stderr)
      (and (equal? stdout "hello")
           (equal? stderr "bad"))))
  (let* ([descriptor (make-test-case 'stdout
                       (lambda () (display "hello\n"))
                       '((capture . stdout) (stdout . "hello\n")))]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (and (eq? (test-result-status result) 'passed)
         (equal? (test-result-stdout result) "hello\n")))
  (let* ([descriptor (make-test-case 'stdout-fail
                       (lambda () (display "actual\n"))
                       '((capture . stdout) (stdout . "expected\n")))]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (and (eq? (test-result-status result) 'failed)
         (test-failure? (test-result-condition result)))))

(mat test-expand-and-compile-runner
  (let* ([descriptor (make-test-expand 'expand-ok '(+ 1 2) '())]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (eq? (test-result-status result) 'passed))
  (let* ([descriptor (make-test-expand 'expand-bad
                       '(let ([x 1] [x 2]) x)
                       '((raises . syntax-violation?)))]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (eq? (test-result-status result) 'passed))
  (let* ([descriptor (make-test-compile 'compile-ok '((define x 1) (+ x 2)) '())]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (eq? (test-result-status result) 'passed))
  (let* ([descriptor (make-test-compile 'compile-bad
                       '((let ([x 1] [x 2]) x))
                       '((raises . syntax-violation?)))]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (eq? (test-result-status result) 'passed)))

(test-clear-registry! (current-test-registry))

(test-case macro/runtime-pass
  :tags (macro fast)
  :parameterize ([x expected]
                 [1 2]
                 [2 3])
  (test-= expected (+ x 1)))

(test-suite macro/suite
  :tags (suite-tag)
  (make-test-case 'suite-child (lambda () (test-true #t)) '()))

(test-expand macro/expand-ok
  '(+ 1 2))

(test-compile macro/compile-ok
  (define macro-compile-x 1)
  (+ macro-compile-x 2))

(mat test-definition-macros
  (= (length (test-registry-descriptors (current-test-registry))) 4)
  (let ([results (test-run-registry (current-test-registry) (test-silent-config))])
    (and (= (length results) 5)
         (andmap (lambda (result) (eq? (test-result-status result) 'passed)) results)))
  (let ([selected (test-select (current-test-registry) '((include-tags . (suite-tag))))])
    (and (= (length selected) 1)
         (eq? (test-concrete-case-name (car selected)) 'suite-child))))

(test-clear-registry! (current-test-registry))

(test-case macro/parameterize-product
  :parameterize ([x]
                 [1]
                 [2])
  :parameterize ([y z]
                 [10 11]
                 [20 21])
  (test-true
   (or (and (= x 1) (= y 10) (= z 11))
       (and (= x 1) (= y 20) (= z 21))
       (and (= x 2) (= y 10) (= z 11))
       (and (= x 2) (= y 20) (= z 21)))))

(mat test-parameterize-macros
  (let ([cases (test-select (current-test-registry)
                            '((name-prefix . "macro/parameterize-product")))])
    (and (= (length cases) 4)
         (equal? (map test-concrete-case-parameters cases)
                 '(((x . 1) (y . 10) (z . 11))
                   ((x . 1) (y . 20) (z . 21))
                   ((x . 2) (y . 10) (z . 11))
                   ((x . 2) (y . 20) (z . 21))))))
  ;; Negative macro config test: duplicate names in one :parameterize table are rejected.
  (test-error-message-contains?
   (lambda ()
     (eval '(test-case macro/parameterize-duplicate-row-name
              :parameterize ([x x]
                             [1 2])
              (test-true #t))))
   "duplicate parameter name")
  ;; Negative macro config test: duplicate names across :parameterize tables are rejected.
  (test-error-message-contains?
   (lambda ()
     (eval '(test-case macro/parameterize-duplicate-table-name
              :parameterize ([x]
                             [left])
              :parameterize ([x y]
                             [right paired])
              (test-true #t))))
   "duplicate parameter name")
  (let ([results (test-run-registry (current-test-registry) (test-silent-config))])
    (and (= (length results) 4)
         (andmap (lambda (result) (eq? (test-result-status result) 'passed)) results))))

(mat test-reporters
  (let* ([results (list (make-test-result 'a 'a 'passed "" #f "" "" '() #f)
                        (make-test-result 'b 'b 'failed "bad" #f "" "" '() #f))]
         [summary (test-summarize results)])
    (and (= (test-summary-total summary) 2)
         (= (test-summary-passed summary) 1)
         (= (test-summary-failed summary) 1)))
  (let ([out (open-output-string)])
    (test-report (test-text-reporter) out
                 (test-summarize
                  (list (make-test-result 'a 'a 'passed "" #f "" "" '() #f))))
    (let ([text (get-output-string out)])
      (and (string? text)
           (> (string-length text) 0))))
  (let ([out (open-output-string)])
    (test-report (test-datum-reporter) out
                 (test-summarize
                  (list (make-test-result 'a 'a 'passed "" #f "" "" '() #f))))
    (let ([datum (read (open-input-string (get-output-string out)))])
      (and (pair? datum)
           (eq? (car datum) 'test-summary)))))

(test-clear-registry! (current-test-registry))

(test-case requirement/chez-version-pass
  :requires-chez-version (0 0 0)
  (test-true #t))

(test-case requirement/chez-version-skip
  :requires-chez-version (999 0 0)
  (test-fail "must be skipped"))

(test-case requirement/file-pass
  :requires-file "test.ss"
  (test-true #t))

(test-case requirement/file-skip
  :requires-file "missing-cspp-test-file"
  (test-fail "must be skipped"))

(mat test-requirement-configs
  (let ([results (test-run-registry (current-test-registry) (test-silent-config))])
    (equal? (map test-result-status results)
            '(passed skipped passed skipped))))

(define test-fixture-log '())

(define test-record-fixture-event!
  (lambda (event)
    (set! test-fixture-log (cons event test-fixture-log))))

(test-clear-registry! (current-test-registry))

(test-case fixture/basic
  :fixture ([value (begin (test-record-fixture-event! 'setup) 41)])
  :after (lambda (params) (test-record-fixture-event! 'after))
  (test-= 42 (+ value 1)))

(test-case fixture/cleanup-on-failure
  :fixture ([value (begin (test-record-fixture-event! 'setup-fail) 1)])
  :after (lambda (params) (test-record-fixture-event! 'after-fail))
  (test-equal 'expected value))

(mat test-fixtures
  (begin
    (set! test-fixture-log '())
    #t)
  (let ([results (test-run-registry (current-test-registry) (test-silent-config))])
    (and (equal? (map test-result-status results) '(passed failed))
         (if (memq 'after test-fixture-log) #t #f)
         (if (memq 'after-fail test-fixture-log) #t #f))))

(test-clear-registry! (current-test-registry))

(test-case condition/custom-handler
  :condition-handler
  (lambda (condition)
    (if (message-condition? condition)
        'handled
        'unhandled))
  (raise (condition (make-message-condition "handled by test"))))

(test-case condition/warning-as-error
  :warnings 'error
  (warningf 'condition/warning-as-error "warning should fail this test"))

(mat test-condition-policies
  (let ([results (test-run-registry (current-test-registry) (test-silent-config))])
    (equal? (map test-result-status results)
            '(passed failed))))

(mat test-styled-reporter
  (test-reporter? (test-progress-reporter 'never))
  (let ([out (open-output-string)])
    (test-report (test-progress-reporter 'never) out
                 (test-summarize
                  (list (make-test-result 'a 'a 'passed "" #f "" "" '() #f)
                        (make-test-result 'b 'b 'failed "bad" #f "" "" '() #f)
                        (make-test-result 'c 'c 'skipped "skip" #f "" "" '() #f))))
    (test-string-contains? (get-output-string out)
      ".Fs\npassed: 1\nfailed: 1\nerrored: 0\nskipped: 1\nxfail: 0\nxpass: 0\ntotal: 3\n"))
  (let ([out (open-output-string)])
    (test-report (test-progress-reporter 'always) out
                 (test-summarize
                  (list (make-test-result 'a 'a 'passed "" #f "" "" '() #f))))
    (> (string-length (get-output-string out)) 0)))

(define test-write-file
  (lambda (path text)
    (call-with-output-file path
      (lambda (out) (display text out))
      'replace)))

(mat test-run-files
  (let ([path "tmp-cspp-test-run-file.ss"])
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (test-write-file
         path
         "(import (cspp) (cspp test))\n(test-clear-registry! (current-test-registry))\n(test-case loaded (test-true #t))\n")
        (let ([results (test-run-files (list path) (test-silent-config))])
          (and (= (length results) 1)
               (eq? (test-result-status (car results)) 'passed))))
      (lambda ()
        (when (file-exists? path)
          (delete-file path))))))

(mat test-main-api
  (let ([path "tmp-cspp-test-main-file.ss"])
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (test-write-file
         path
         "(import (cspp) (cspp test))\n(test-clear-registry! (current-test-registry))\n(test-case main-loaded (test-true #t))\n")
        (let ([out (open-output-string)])
          (= (parameterize ([current-output-port out])
               (test-main (list "--reporter" "datum" path)))
             0)))
      (lambda ()
        (when (file-exists? path)
          (delete-file path))))))

(mat test-nested-registration
  ;; A skipped suite must not leave an independently runnable child in the registry.
  (parameterize ([current-test-registry (make-test-registry)])
    (let ([calls 0])
      (test-suite outer :skip "disabled"
        (test-suite inner
          (test-case child (set! calls (fx+ calls 1)))))
      (let ([results (test-run-registry (current-test-registry) (test-silent-config))])
        (and (= (length (test-registry-descriptors (current-test-registry))) 1)
             (equal? (map test-result-status results) '(skipped))
             (= calls 0)))))

  (parameterize ([current-test-registry (make-test-registry)])
    (let ([calls 0])
      (test-suite outer :tags (inherited)
        (test-case child (set! calls (fx+ calls 1))))
      (let ([selected (test-select (current-test-registry) '((include-tags . (inherited))))]
            [results (test-run-registry (current-test-registry) (test-silent-config))])
        (and (= (length selected) 1)
             (= (length results) 1)
             (= calls 1)))))

  ;; A shared test group must apply its skip policy to its registered children.
  (parameterize ([current-test-registry (make-test-registry)])
    (test :skip "disabled group" (test-case grouped (test-true #f)))
    (equal? (map test-result-status
                 (test-run-registry (current-test-registry) (test-silent-config)))
            '(skipped))))

(mat test-conditional-parameter-bindings
  (parameterize ([current-test-registry (make-test-registry)])
    (test-case conditional
      :parameterize ([x] [1] [2])
      :skip-when (= x 1) "skip first"
      (test-= x 2))
    (equal? (map test-result-status
                 (test-run-registry (current-test-registry) (test-silent-config)))
            '(skipped passed)))

  (parameterize ([current-test-registry (make-test-registry)])
    (test-case conditional
      :parameterize ([x] [1] [2])
      :only-when (= x 2) "only second"
      (test-= x 2))
    (equal? (map test-result-status
                 (test-run-registry (current-test-registry) (test-silent-config)))
            '(skipped passed)))

  ;; Only the failing parameter row is expected to fail.
  (parameterize ([current-test-registry (make-test-registry)])
    (test-case conditional
      :parameterize ([x] [1] [2])
      :xfail-when (= x 1) "known first-row failure"
      (test-= x 2))
    (equal? (map test-result-status
                 (test-run-registry (current-test-registry) (test-silent-config)))
            '(xfail passed))))

(mat test-global-case-identifiers
  (let ([registry (make-test-registry)])
    (test-register! registry
      (make-test-case 'first (lambda () #t) '((parameterize . (((x) (1) (2)))))))
    (test-register! registry
      (make-test-suite 'suite (list (make-test-case 'child (lambda () #t) '())) '()))
    (and (equal? (map test-concrete-case-id (test-list registry '())) '(0 1 2))
         (equal? (map test-concrete-case-id
                      (test-select registry '((name-prefix . "child")))) '(2))
         (equal? (map test-result-id (test-run-registry registry (test-silent-config)))
                 '(0 1 2)))))

(mat test-warning-continuation
  ;; Allowed warnings must not skip a later failing assertion.
  (let ([continued? #f])
    (let ([result (car (test-run
                        (list (make-test-case 'warning
                                (lambda ()
                                  (warningf 'warning "allowed")
                                  (set! continued? #t)
                                  (test-true #f)) '()))
                        (test-silent-config)))])
      (and continued? (eq? (test-result-status result) 'failed))))

  (let ([continued? #f])
    (let ([result (car (test-run
                        (list (make-test-case 'warning
                                (lambda ()
                                  (warningf 'warning "allowed")
                                  (set! continued? #t)) '()))
                        (test-silent-config)))])
      (and continued? (eq? (test-result-status result) 'passed)))))

(mat test-expected-condition-warning-policy
  ;; Allowed warnings must resume until the expected error, with one predicate call per condition.
  (for-all
    (lambda (policy)
      (let ([continued? #f] [seen '()])
        (let ([result
               (car (test-run
                      (list (make-test-case 'warning-before-error
                              (lambda ()
                                (warningf 'warning-before-error "allowed")
                                (set! continued? #t)
                                (error 'warning-before-error "expected"))
                              (cons (cons 'raises
                                      (lambda (condition)
                                        (set! seen (cons (if (warning? condition) 'warning 'error)
                                                         seen))
                                        (error? condition)))
                                    policy)))
                      (test-silent-config)))])
          (and continued?
               (equal? seen '(error warning))
               (eq? (test-result-status result) 'passed)))))
    '(() ((warnings . allow))))

  ;; Error: an allowed warning alone must not satisfy an expected error.
  (let ([continued? #f])
    (let* ([result
            (car (test-run
                   (list (make-test-case 'missing-error
                           (lambda ()
                             (warningf 'missing-error "allowed")
                             (set! continued? #t))
                           `((raises . ,error?))))
                   (test-silent-config)))]
           [condition (test-result-condition result)])
      (and continued?
           (eq? (test-result-status result) 'failed)
           (test-failure? condition)
           (eq? (test-failure-actual condition) 'no-condition))))

  ;; Explicit warning expectations take precedence, including under warnings-as-error.
  (for-all
    (lambda (policy)
      (let ([continued? #f] [calls 0])
        (let ([result
               (car (test-run
                      (list (make-test-case 'expected-warning
                              (lambda ()
                                (warningf 'expected-warning "expected")
                                (set! continued? #t))
                              (cons (cons 'raises
                                      (lambda (condition)
                                        (set! calls (+ calls 1))
                                        (warning? condition)))
                                    policy)))
                      (test-silent-config)))])
          (and (not continued?) (= calls 1)
               (eq? (test-result-status result) 'passed)))))
    '(() ((warnings . error))))

  ;; Error: rejected warnings must still abort under strict policy or a custom handler.
  (for-all
    (lambda (policy)
      (let ([continued? #f])
        (let ([result
               (car (test-run
                      (list (make-test-case 'rejected-warning
                              (lambda ()
                                (warningf 'rejected-warning "rejected")
                                (set! continued? #t)
                                (error 'rejected-warning "expected"))
                              (cons (cons 'raises error?) policy)))
                      (test-silent-config)))])
          (and (not continued?) (eq? (test-result-status result) 'failed)))))
    (list '((warnings . error))
          `((condition-handler . ,(lambda (condition) 'failed))))))

(mat test-failed-output-preservation
  ;; Both streams must survive an assertion failure.
  (let ([result (car (test-run
                      (list (make-test-case 'output
                              (lambda ()
                                (display "before")
                                (display "error" (current-error-port))
                                (test-true #f))
                              '((capture . (stdout stderr)))))
                      (test-silent-config)))])
    (and (eq? (test-result-status result) 'failed)
         (string=? (test-result-stdout result) "before")
         (string=? (test-result-stderr result) "error")))

  ;; An output mismatch must preserve the actual captured output.
  (let ([result (car (test-run
                      (list (make-test-case 'output (lambda () (display "actual"))
                              '((capture . stdout) (stdout . "expected"))))
                      (test-silent-config)))])
    (and (eq? (test-result-status result) 'failed)
         (string=? (test-result-stdout result) "actual"))))

(mat test-setup-error-cleanup
  ;; Fixtures already acquired must be released if before raises.
  (let ([cleaned '()])
    (let ([result (car (test-run
                        (list (make-test-case 'setup (lambda () #t)
                                `((fixtures . ((one . ,(lambda () 'open))))
                                  (before . ,(lambda (params) (error 'before "failed")))
                                  (after . ,(lambda (params) (set! cleaned params))))))
                        (test-silent-config)))])
      (and (eq? (test-result-status result) 'errored)
           (equal? cleaned '((one . open))))))

  ;; A later fixture failure must still clean the earlier fixture.
  (let ([cleaned '()])
    (let ([result (car (test-run
                        (list (make-test-case 'setup (lambda () #t)
                                `((fixtures . ((one . ,(lambda () 'open))
                                               (two . ,(lambda () (error 'two "failed")))))
                                  (after . ,(lambda (params) (set! cleaned params))))))
                        (test-silent-config)))])
      (and (eq? (test-result-status result) 'errored)
           (equal? cleaned '((one . open)))))))

(mat test-phase-policies
  ;; Skip and xfail policies apply to expansion and compilation errors too.
  (let ([bad '(let ([x 1] [x 2]) x)])
    (andmap
      (lambda (constructor)
        (let ([form (if (eq? constructor make-test-compile) (list bad) bad)])
          (equal? (map test-result-status
                       (test-run
                         (list
                           (constructor 'skip form
                             `((skip-when . (,(lambda (params) #t) . "skip"))))
                           (constructor 'xfail form
                             `((xfail-when . (,(lambda (params) #t) . "known bug")))))
                         (test-silent-config)))
                  '(skipped xfail))))
      (list make-test-expand make-test-compile))))

(mat test-predicate-error-isolation
  ;; A broken policy predicate must produce one error and allow later cases to run.
  (let ([bad-rule (cons (lambda (params) (error 'rule "bad predicate")) "reason")])
    (andmap
      (lambda (key)
        (guard (condition [else #f])
          (equal? (map test-result-status
                       (test-run
                         (list
                           (make-test-case 'bad (lambda () #t) (list (cons key bad-rule)))
                           (make-test-case 'next (lambda () #t) '()))
                         (test-silent-config)))
                  '(errored passed))))
      '(skip-when xfail-when))))

(mat test-compile-quiet
  (let ([text (call-with-string-output-port
                (lambda (out)
                  (parameterize ([current-output-port out] [current-error-port out])
                    (test-run
                      (list (make-test-compile 'compile '((define quiet-x 1) quiet-x) '()))
                      (test-silent-config)))))])
    (string=? text "")))

(mat test-xfail-strictness
  (andmap
    (lambda (strict?)
      (let ([config (test-config-with (test-silent-config)
                      `((xfail-strict? . ,strict?) (stop-on-failure? . #t)))])
        (equal? (map test-result-status
                     (test-run
                       (list
                         (make-test-case 'xpass (lambda () #t)
                           `((xfail-when . (,(lambda (params) #t) . "known bug"))))
                         (make-test-case 'next (lambda () #t) '()))
                       config))
                (if strict? '(failed) '(xpass passed)))))
    '(#f #t))

  ;; Per-case strictness overrides the global policy in either direction.
  (andmap
    (lambda (strict?)
      (let ([config (test-config-with (test-silent-config)
                      `((xfail-strict? . ,(not strict?)) (stop-on-failure? . #t)))])
        (parameterize ([current-test-registry (make-test-registry)])
          (test-case expected :xfail "known bug" :xfail-strict strict? (test-true #t))
          (let ([result (car (test-run-registry (current-test-registry) config))])
            (and (eq? (test-result-status result) (if strict? 'failed 'xpass))
                 (or (not strict?)
                     (test-string-contains? (test-result-message result) "unexpected pass")))))))
    '(#f #t))

  (let ([path "tmp-cspp-test-strict-main.ss"])
    (dynamic-wind
      (lambda () (void))
      (lambda ()
        (andmap
          (lambda (strict?)
            (test-write-file path
              (format "(import (cspp) (cspp test))\n(test-case x :xfail ~s :xfail-strict ~s #t)\n"
                      "known bug" strict?))
            (let ([status #f])
              (call-with-string-output-port
                (lambda (out)
                  (parameterize ([current-output-port out])
                    (set! status (test-main (list path))))))
              (= status (if strict? 1 0))))
          '(#f #t)))
      (lambda () (when (file-exists? path) (delete-file path))))))

(mat test-compile-form-list-contract
  (let ([result (car (test-run
                      (list (make-test-compile 'forms '((let ([x 1]) x)) '()))
                      (test-silent-config)))])
    (eq? (test-result-status result) 'passed))

  ;; A compile body must be a proper list of forms.
  (error? (make-test-compile 'invalid '#(1 2) '())))

(mat test-runtime-exception-expectations
  ;; Runtime :raises must accept only the declared exception type.
  (parameterize ([current-test-registry (make-test-registry)])
    (test-case expected :raises error? (error 'expected "boom"))
    (test-case missing :raises error? #t)
    (test-case wrong :raises syntax-violation? (error 'wrong "boom"))
    (equal? (map test-result-status
                 (test-run-registry (current-test-registry) (test-silent-config)))
            '(passed failed failed)))

  ;; Expected exceptions must preserve fixture cleanup and captured diagnostics.
  (let ([cleaned? #f])
    (parameterize ([current-test-registry (make-test-registry)])
      (test-case expected
        :raises error?
        :capture stdout
        :stdout "before"
        :fixture ([resource 42])
        :after (lambda (fixtures) (set! cleaned? #t))
        (display "before")
        (error 'expected "boom"))
      (let ([result (car (test-run-registry (current-test-registry) (test-silent-config)))])
        (and cleaned? (eq? (test-result-status result) 'passed)
             (string=? (test-result-stdout result) "before"))))))

(mat test-parameter-row-width-validation
  ;; Low-level tables must reject extra and missing cells, including zero-name rows.
  (andmap
    (lambda (table)
      (guard (condition [else #t])
        (test-expand-parameters
          (make-test-case 'bad (lambda () #t) (list (cons 'parameterize (list table)))))
        #f))
    '(((x) (1 2)) ((x y) (1)) (() (1))))

  ;; Macro tables must reject extra cells before any body runs.
  (guard (condition [else #t])
    (eval '(test-case too-wide :parameterize ([x] [1 2]) #t))
    #f))

(mat test-reporter-config-validation
  ;; A procedure and an unrelated record are not reporter objects.
  (andmap
    (lambda (reporter)
      (guard (condition [else #t])
        (make-test-config reporter #f #f #f 'never)
        #f))
    (list (lambda (out summary) #t) (make-test-registry)))

  ;; Config updates must reject the same invalid reporter types as construction.
  (guard (condition [else #t])
    (test-config-with (default-test-config) `((reporter . ,(lambda (out summary) #t))))
    #f)

  (let ([text (call-with-string-output-port
                (lambda (out)
                  (test-run (list (make-test-case 'valid (lambda () #t) '()))
                    (make-test-config (test-text-reporter) out #f #f 'never))))])
    (test-string-contains? text "passed: 1")))

(mat test-inherited-fixture-bindings
  ;; Inherited names must bind in child bodies even through a macro wrapper.
  (guard (condition [else #f])
    (eval
      '(parameterize ([current-test-registry (make-test-registry)])
         (let ([acquired 0] [released 0])
           (define-syntax wrapped-case
             (syntax-rules () [(_ name expr) (test-case name expr)]))
           (test-suite shared
             :fixture ([resource (begin (set! acquired (+ acquired 1)) 42)])
             :after (lambda (fixtures) (set! released (+ released 1)))
             (wrapped-case first (test-= resource 42))
             (test-suite nested (test-case second (test-= resource 42))))
           (let ([results (test-run-registry (current-test-registry)
                            (test-config-with (default-test-config) '((output . #f))))])
             (and (= acquired 2) (= released 2)
                  (equal? (map test-result-status results) '(passed passed))))))
      (environment '(cspp))))

  ;; A child fixture list overrides the suite list, with matching lexical names.
  (guard (condition [else #f])
    (eval
      '(parameterize ([current-test-registry (make-test-registry)])
         (test-suite shared :fixture ([resource 42])
           (test-case child :fixture ([resource 7]) (test-= resource 7)))
         (equal? (map test-result-status
                      (test-run-registry (current-test-registry)
                        (test-config-with (default-test-config) '((output . #f)))))
                 '(passed)))
      (environment '(cspp))))

  ;; Parameterized suite bindings must also remain available to child expressions.
  (guard (condition [else #f])
    (eval
      '(parameterize ([current-test-registry (make-test-registry)])
         (test-suite shared :parameterize ([number] [1] [2])
           (test-case child (test-true (memv number '(1 2)))))
         (equal? (map test-result-status
                      (test-run-registry (current-test-registry)
                        (test-config-with (default-test-config) '((output . #f)))))
                 '(passed passed)))
      (environment '(cspp)))))

(mat test-registry-snapshot-order
  (let ([registry (make-test-registry)]
        [first (make-test-case 'first (lambda () #t) '())]
        [second (make-test-case 'second (lambda () #t) '())])
    (test-register! registry first)
    (let ([snapshot (test-registry-descriptors registry)])
      (test-register! registry second)
      (let ([all (test-registry-descriptors registry)])
        (test-clear-registry! registry)
        (test-register! registry second)
        (and (equal? (map test-descriptor-name snapshot) '(first))
             (equal? (map test-descriptor-name all) '(first second))
             (equal? (map test-descriptor-name (test-registry-descriptors registry))
                     '(second)))))))

(mat test-registry-linear-allocation
  ;; Repeated registration must not copy the full registry on every insertion.
  (let ([registry (make-test-registry)]
        [descriptor (make-test-case 'same (lambda () #t) '())])
    (collect)
    (let ([before (statistics)])
      (do ([index 0 (fx+ index 1)]) ((fx= index 2000))
        (test-register! registry descriptor))
      (let ([bytes (sstats-bytes (sstats-difference (statistics) before))])
        (and (< bytes 200000)
             (= (length (test-registry-descriptors registry)) 2000))))))

(mat test-capture-mode-validation
  ;; Misspelled symbols, invalid list entries, and improper lists must be rejected.
  (andmap
    (lambda (mode)
      (let ([called? #f])
        (guard (condition [else (not called?)])
          (test-capture-ports mode (lambda () (set! called? #t)))
          #f)))
    '(bogus (stdout bogus) (stderr . stdout) 42))

  ;; Error: invalid capture metadata must reject at construction without running the body.
  (let ([called? #f])
    (guard (raised [else (not called?)])
      (make-test-case 'bad-capture (lambda () (set! called? #t)) '((capture . bogus)))
      #f)))

(mat test-capture-selected-streams
  (andmap
    (lambda (mode)
      (let ([outer (open-output-string)])
        (dynamic-wind
          (lambda () (void))
          (lambda ()
            (parameterize ([current-output-port outer] [current-error-port outer])
              (let-values ([(values-list stdout stderr)
                            (test-capture-ports mode
                              (lambda ()
                                (display "out")
                                (display "err" (current-error-port))
                                (values 1 2)))])
                (and (equal? values-list '(1 2))
                     (case mode
                       [(#f) (and (string=? stdout "") (string=? stderr "")
                                  (string=? (get-output-string outer) "outerr"))]
                       [(stdout) (and (string=? stdout "out") (string=? stderr "")
                                      (string=? (get-output-string outer) "err"))]
                       [(stderr) (and (string=? stdout "") (string=? stderr "err")
                                      (string=? (get-output-string outer) "out"))]
                       [(combined) (and (string=? stdout "outerr") (string=? stderr "")
                                        (string=? (get-output-string outer) ""))])))))
          (lambda () (close-output-port outer)))))
    '(#f stdout stderr combined)))

(mat test-disabled-capture-allocation
  ;; Disabled capture must not create string ports for every case.
  (let ([body (lambda () (values))])
    (collect)
    (let ([before (statistics)])
      (do ([index 0 (fx+ index 1)]) ((fx= index 1000))
        (call-with-values (lambda () (test-capture-ports #f body))
          (lambda (values-list stdout stderr) (void))))
      (< (sstats-bytes (sstats-difference (statistics) before)) 256000))))

(mat test-runtime-expectation-does-not-mask-setup
  ;; An expected body exception must not accept a fixture acquisition error.
  (parameterize ([current-test-registry (make-test-registry)])
    (test-case bad-setup :raises error?
      :fixture ([resource (error 'setup "acquisition failed")])
      #t)
    (equal? (map test-result-status
                 (test-run-registry (current-test-registry) (test-silent-config)))
            '(errored))))

(mat test-inherited-parameter-conditional-binding
  (guard (condition [else #f])
    (eval
      '(parameterize ([current-test-registry (make-test-registry)])
         (test-suite shared :parameterize ([number] [1] [2])
           (test-case child :skip-when (= number 1) "skip first"
             (test-= number 2)))
         (equal? (map test-result-status
                      (test-run-registry (current-test-registry)
                        (test-config-with (default-test-config) '((output . #f)))))
                 '(skipped passed)))
      (environment '(cspp)))))

(mat test-inherited-bindings-with-generated-case-name
  ;; A wrapper's generated case name must not determine its caller expressions' context.
  (guard (condition [else #f])
    (eval
      '(parameterize ([current-test-registry (make-test-registry)])
         (define-syntax wrapped-case
           (syntax-rules () [(_ expr) (test-case generated expr)]))
         (test-suite shared :fixture ([resource 42])
           (wrapped-case (test-= resource 42)))
         (equal? (map test-result-status
                      (test-run-registry (current-test-registry)
                        (test-config-with (default-test-config) '((output . #f)))))
                 '(passed)))
      (environment '(cspp))))

  (guard (condition [else #f])
    (eval
      '(parameterize ([current-test-registry (make-test-registry)])
         (define-syntax wrapped-case
           (syntax-rules ()
             [(_ skip? expression)
              (test-case generated :skip-when skip? "skip first" expression)]))
         (test-suite shared :parameterize ([number] [1] [2])
           (wrapped-case (= number 1) (test-= number 2)))
         (equal? (map test-result-status
                      (test-run-registry (current-test-registry)
                        (test-config-with (default-test-config) '((output . #f)))))
                 '(skipped passed)))
      (environment '(cspp)))))

(mat test-lexical-binding-boundaries
  ;; Suite inputs bind policy expressions and bodies; skipped cases do not acquire fixtures.
  (parameterize ([current-test-registry (make-test-registry)])
    (let ([acquired 0])
      (test-suite shared
        :fixture ([resource (begin (set! acquired (+ acquired 1)) 42)])
        :parameterize ([number] [1] [2])
        (test-suite nested
          (test-case child :skip-when (= number 1) "skip first"
            (test-= resource 42)
            (test-= number 2))))
      (and (equal? (map test-result-status
                       (test-run-registry (current-test-registry) (test-silent-config)))
                   '(skipped passed))
           (= acquired 1))))

  ;; A wrapper's private variable must retain its lexical binding in policies and bodies.
  (guard (condition [else #f])
    (eval
      '(parameterize ([current-test-registry (make-test-registry)])
         (define-syntax wrapped-case
           (syntax-rules ()
             [(_ expression)
              (let ([number 99])
                (test-case generated :skip-when (= number 1) "private number is 99"
                  (test-= number 99)
                  expression))]))
         (test-suite shared :parameterize ([number] [1] [2])
           (wrapped-case (test-true (memv number '(1 2)))))
         (equal? (map test-result-status
                      (test-run-registry (current-test-registry)
                        (test-config-with (default-test-config) '((output . #f)))))
                 '(passed passed)))
      (environment '(cspp))))

  ;; Ordinary inner lexical bindings shadow inherited fixture and parameter names.
  (parameterize ([current-test-registry (make-test-registry)])
    (test-suite shared :fixture ([resource 42]) :parameterize ([number] [1] [2])
      (let ([resource 99] [number 100])
        (test-case child :only-when (= number 100) "inner number"
          (test-= resource 99)
          (test-= number 100))))
    (equal? (map test-result-status
                 (test-run-registry (current-test-registry) (test-silent-config)))
            '(passed passed)))

  ;; Inherited inputs are mutable lexical variables, freshly bound for each callback and case.
  (parameterize ([current-test-registry (make-test-registry)])
    (test-suite shared :fixture ([resource 42]) :parameterize ([number] [1] [2])
      (test-case child
        :only-when (begin (set! number (+ number 10)) (> number 10)) "mutated rule"
        (define read-number (lambda () number))
        (test-true (memv (read-number) '(1 2)))
        (set! number (+ number 100))
        (set! resource (+ resource 1))
        (test-true (memv (read-number) '(101 102)))
        (test-= resource 43))
      (test-case sibling (test-= resource 42) (test-true (memv number '(1 2)))))
    (let ([results (test-run-registry (current-test-registry) (test-silent-config))])
      (and (equal? (map test-result-status results) '(passed passed passed passed))
           (equal? (map test-result-parameters results)
                   '(((number . 1)) ((number . 2)) ((number . 1)) ((number . 2)))))))

  ;; Expected-failure predicates receive parameters, while bodies also receive fixtures.
  (parameterize ([current-test-registry (make-test-registry)])
    (test-suite shared :fixture ([resource 42]) :parameterize ([number] [1] [2])
      (test-case child :xfail-when (= number 1) "known first row"
        (test-= resource 42)
        (test-= number 2)))
    (equal? (map test-result-status
                 (test-run-registry (current-test-registry) (test-silent-config)))
            '(xfail passed)))

  ;; Error: fixtures have no values in pre-acquisition rules or descriptor construction code.
  (for-all
    (lambda (form)
      (guard (condition [else (syntax-violation? condition)])
        (eval form (environment '(cspp)))
        #f))
    '((test-suite shared :fixture ([resource 42])
        (test-case child :skip-when (= resource 42) "unavailable fixture" #t))
      (test-suite shared :parameterize ([number] [1])
        (test-case child :stdout (number->string number) #t))))

  ;; Quoted cyclic data must not be traversed while inheriting fixture or parameter bindings.
  (guard (condition [else #f])
    (eval
      '(parameterize ([current-test-registry (make-test-registry)])
         (test-suite shared :fixture ([resource 42]) :parameterize ([number] [1])
           (test-case child
             (let ([cycle '#0=(payload . #0#)])
               (test-eq cycle (cdr cycle))
               (test-= resource 42)
               (test-= number 1))))
         (equal? (map test-result-status
                      (test-run-registry (current-test-registry)
                        (test-config-with (default-test-config) '((output . #f)))))
                 '(passed)))
      (environment '(cspp)))))

(mat test-runtime-body-arities
  (let ([seen '()])
    (let ([results
           (test-run
             (map (lambda (body) (make-test-case 'arity body
                                  '((parameterize . (((x) (7)))))))
                  (list (lambda () (set! seen (cons 'zero seen)))
                        (lambda (parameters) (set! seen (cons parameters seen)))
                        (lambda arguments (set! seen (cons arguments seen)))
                        (lambda (parameters . rest)
                          (set! seen (cons (cons parameters rest) seen)))
                        (case-lambda
                          [() (set! seen (cons 'wrong seen))]
                          [(parameters) (set! seen (cons parameters seen))])))
             (test-silent-config))])
      (and (equal? (map test-result-status results) '(passed passed passed passed passed))
           (equal? (reverse seen)
                   '(zero ((x . 7)) (((x . 7))) (((x . 7))) ((x . 7))))))))

(mat test-cleanup-preserves-primary-condition
  ;; A cleanup error must not replace an assertion failure or let a handler pass it.
  (let* ([primary (make-test-failure "primary" #t #f)]
         [result (car (test-run
                        (list (make-test-case 'double-error (lambda () (raise primary))
                                `((after . ,(lambda (fixtures) (error 'cleanup "secondary")))
                                  (condition-handler . ,(lambda (raised)
                                                          (if (test-failure? raised)
                                                              'failed 'handled))))))
                        (test-silent-config)))])
    (and (eq? (test-result-status result) 'failed)
         (eq? (test-result-condition result) primary)))

  ;; Even a false raised value must take precedence over a cleanup error.
  (let ([result (car (test-run
                      (list (make-test-case 'false-primary (lambda () (raise #f))
                              `((after . ,(lambda (fixtures) (error 'cleanup "secondary"))))))
                      (test-silent-config)))])
    (and (eq? (test-result-status result) 'errored)
         (eq? (test-result-condition result) #f)))

  ;; Cleanup errors after successful bodies must still fail the case.
  (let ([result (car (test-run
                      (list (make-test-case 'cleanup-error (lambda () #t)
                              `((after . ,(lambda (fixtures) (error 'cleanup "secondary"))))))
                      (test-silent-config)))])
    (and (eq? (test-result-status result) 'errored)
         (eq? (condition-who (test-result-condition result)) 'cleanup))))

(mat test-current-chez-version
  (equal? (chez-version) (call-with-values scheme-version-number list))

  (let ([called? #f])
    (let ([results (test-run
                    (list (make-test-case 'supported-version (lambda () (set! called? #t))
                            `((requires-chez-version . ,(call-with-values scheme-version-number list)))))
                    (test-silent-config))])
      (and called? (equal? (map test-result-status results) '(passed))))))

(mat test-handled-conditions-follow-success-policies
  ;; Handled body conditions must still check captured stdout.
  (let* ([handler (lambda (raised) (if (test-failure? raised) 'failed 'handled))]
         [descriptor (make-test-case 'handled-output
                       (lambda () (display "wrong") (error 'body "handled"))
                       `((capture . stdout) (stdout . "expected")
                         (condition-handler . ,handler)))]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (and (eq? (test-result-status result) 'failed)
         (string=? (test-result-stdout result) "wrong")
         (test-failure? (test-result-condition result))))

  ;; A handled condition is a successful outcome, including under strict xfail policy.
  (let* ([descriptor (make-test-case 'handled-xfail
                       (lambda () (error 'body "handled"))
                       `((condition-handler . ,(lambda (raised) 'handled))
                         (xfail-when . (,(lambda (parameters) #t) . "known issue"))))]
         [loose (car (test-run (list descriptor) (test-silent-config)))]
         [strict (car (test-run (list descriptor)
                        (test-config-with (test-silent-config) '((xfail-strict? . #t)))))])
    (and (eq? (test-result-status loose) 'xpass)
         (eq? (test-result-status strict) 'failed))))

(mat test-compile-leaves-no-wpo
  (let* ([temporary-wpo?
          (lambda (name)
            (and (test-string-contains? name "test-compile-")
                 (test-string-contains? name ".wpo")))]
         [before (filter temporary-wpo? (directory-list "."))])
    (dynamic-wind
      (lambda () (void))
      (lambda ()
        (let ([result (parameterize ([generate-wpo-files #t])
                        (car (test-run
                               (list (make-test-compile 'no-wpo '((define x 1) (+ x 2)) '()))
                               (test-silent-config))))])
          (and (eq? (test-result-status result) 'passed)
               (equal? before (filter temporary-wpo? (directory-list "."))))))
      (lambda ()
        (for-each (lambda (name) (unless (member name before) (delete-file name)))
                  (filter temporary-wpo? (directory-list ".")))))))

(mat test-main-reporter-validation
  ;; Unknown reporters and missing option values must be rejected before file loading.
  (for-all (lambda (arguments)
             (guard (condition [else #t]) (test-main arguments) #f))
           '(("--reporter" "bogus") ("--reporter")))

  (call-with-port (open-output-string)
    (lambda (out)
      (parameterize ([current-output-port out])
        (and (= (test-main '("--reporter" "text")) 0)
             (= (test-main '("--reporter" "datum")) 0)
             (= (test-main '("--reporter" "progress")) 0))))))

(mat test-expansion-self-contained-source
  ;; Surrounding local bindings must not become silently accepted global references.
  (let-syntax ([reject (lambda (stx) (syntax-error stx "local rejection"))])
    (let* ([descriptor (test-expand external-local (reject))]
           [result (car (test-run (list descriptor) (test-silent-config)))])
      (and (eq? (test-result-status result) 'errored)
           (syntax-violation? (test-result-condition result)))))

  ;; Put the local transformer in the tested source to expand it during the run.
  (let* ([descriptor
          (test-expand self-contained-reject :raises syntax-violation?
            (let-syntax ([reject (lambda (stx) (syntax-error stx "source rejection"))])
              (reject)))]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (eq? (test-result-status result) 'passed))

  ;; Skip policies apply before a self-contained transformer can run or raise.
  (let* ([descriptor
          (test-expand self-contained-skipped :skip "disabled"
            (let-syntax ([reject (lambda (stx) (syntax-error stx "must not expand"))])
              (reject)))]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (eq? (test-result-status result) 'skipped)))

(mat test-select-validates-keys-before-filtering
  ;; Unknown selector keys must fail even with no cases or an earlier rejecting filter.
  (let ([registry (make-test-registry)])
    (and (guard (raised [else #t])
           (test-select registry '((bogus . #t))) #f)
         (begin
           (test-register! registry (make-test-case 'present (lambda () #t) '()))
           (guard (raised [else #t])
             (test-list registry '((name-prefix . "absent") (bogus . #t))) #f)))))

(mat test-selected-cases-compose-with-run
  (let ([registry (make-test-registry)] [seen '()] [hooks '()] [reported #f])
    (test-register! registry
      (make-test-case 'unselected (lambda () (error 'unselected "must not run")) '()))
    (test-register! registry
      (make-test-suite 'selected-suite
        (list (make-test-case 'selected
                (lambda (parameters)
                  (test-eq (cdr (assq 'resource parameters)) 'ready)
                  (set! seen (cons (cdr (assq 'n parameters)) seen))) '()))
        `((tags . (chosen)) (parameterize . (((n) (1) (2) (3))))
          (fixtures . ((resource . ,(lambda () 'ready))))
          (before . ,(lambda (fixtures) (set! hooks (cons 'before hooks))))
          (after . ,(lambda (fixtures) (set! hooks (cons 'after hooks)))))))
    (let* ([selected (test-select registry '((include-tags . (chosen))))]
           [subset (list (caddr selected) (car selected))]
           [results
            (call-with-port (open-output-string)
              (lambda (out)
                (test-run subset
                  (test-config-with (test-silent-config)
                    `((output . ,out)
                      (reporter . ,(make-test-reporter 'collect
                                     (lambda (port summary) (set! reported summary)))))))))])
      (and (equal? seen '(1 3))
           (equal? hooks '(after before after before))
           (equal? (map test-result-id results) (map test-concrete-case-id subset))
           (equal? (map test-result-parameters results) '(((n . 3)) ((n . 1))))
           (equal? (map test-result-status results) '(passed passed))
           (equal? (test-summary-results reported) results)))))

(mat test-selected-cases-preserve-policies
  ;; Error: a selected failing row must stop subsequent selected rows.
  (let* ([calls 0]
         [cases (test-expand-parameters
                  (make-test-suite 'policies
                    (list (make-test-case 'failure
                            (lambda () (set! calls (+ calls 1)) (test-true #f)) '()))
                    '((parameterize . (((n) (1) (2)))))))]
         [results (test-run (reverse cases)
                    (test-config-with (test-silent-config) '((stop-on-failure? . #t))))])
    (and (= calls 1) (= (length results) 1)
         (eq? (test-result-status (car results)) 'failed)
         (= (test-result-id (car results)) (test-concrete-case-id (cadr cases)))))

  (let* ([cases (test-expand-parameters
                  (make-test-suite 'skipped-parent
                    (list (make-test-case 'skipped-child
                            (lambda () (error 'skipped-child "must not run")) '()))
                    '((skip-when . ((lambda (parameters) #t) . "inherited")))))]
         [result (car (test-run cases (test-silent-config)))])
    (and (eq? (test-result-status result) 'skipped)
         (string=? (test-result-message result) "inherited")))

  ;; Error: mixed descriptor and concrete-case lists are ambiguous and must reject before running.
  (let* ([calls 0]
         [descriptor (make-test-case 'mixed (lambda () (set! calls (+ calls 1))) '())])
    (and (guard (raised [else #t])
           (test-run (list descriptor (car (test-expand-parameters descriptor))) (test-silent-config))
           #f)
         (= calls 0))))

(mat test-human-reports-show-failure-details
  ;; Error: human reports must identify failures and retain diagnostics from errored cases.
  (for-all
    (lambda (reporter)
      (let* ([results
              (test-run
                (list
                  (make-test-case 'assertion-case
                    (lambda ()
                      (display "captured-output")
                      (display "captured-error" (current-error-port))
                      (test-fail "assertion-detail" 'expected 'actual))
                    '((capture . (stdout stderr)) (source . "failure-source.ss")))
                  (make-test-case 'exception-case
                    (lambda () (error 'exception-origin "exception-detail")) '())
                  (make-test-case 'strict-case (lambda () #t)
                    '((xfail-strict . #t)
                      (xfail-when . ((lambda (parameters) #t) . "known")))))
                (test-silent-config))]
             [text (call-with-string-output-port
                     (lambda (out) (test-report reporter out (test-summarize results))))])
        (for-all (lambda (needle) (test-string-contains? text needle))
                 '("assertion-case" "assertion-detail" "captured-output" "captured-error"
                   "failure-source.ss" "exception-case" "exception-origin" "exception-detail"
                   "strict-case" "unexpected pass"))))
    (list (test-text-reporter) (test-progress-reporter 'never))))

(mat test-human-reports-show-assertion-operands
  ;; Error: generic assertion messages alone do not identify the compared values.
  (for-all
    (lambda (reporter)
      (let* ([results (test-run
                       (list (make-test-case 'operands
                               (lambda ()
                                 (test-equal '(expected-unique 1) '(actual-unique 2))) '()))
                       (test-silent-config))]
             [text (call-with-string-output-port
                     (lambda (out) (test-report reporter out (test-summarize results))))])
        (and (test-string-contains? text "expected: (expected-unique 1)")
             (test-string-contains? text "actual: (actual-unique 2)"))))
    (list (test-text-reporter) (test-progress-reporter 'never))))

(mat test-human-reports-show-cyclic-assertion-operands
  ;; Error: failed assertions may contain cyclic values, including a false expected value.
  (for-all
    (lambda (reporter)
      (let ([actual (list #f)])
        (set-car! actual actual)
        (let* ([results (test-run
                         (list (make-test-case 'cyclic-operands
                                 (lambda () (test-false actual)) '()))
                         (test-silent-config))]
               [text (call-with-string-output-port
                       (lambda (out) (test-report reporter out (test-summarize results))))])
          (and (test-string-contains? text "expected: #f")
               (test-string-contains? text "actual: #0=(#0#)")))))
    (list (test-text-reporter) (test-progress-reporter 'never))))

(mat test-main-failure-diagnostics
  ;; Error: the CLI must report the failed row's identity, parameters and captured streams.
  (let ([path "tmp-cspp-test-main-diagnostics.ss"])
    (dynamic-wind
      (lambda () (void))
      (lambda ()
        (test-write-file path
          "(import (cspp) (cspp test))\n(test-case cli-failed :parameterize ([size] [7]) :capture '(stdout stderr) (display \"stdout-detail\") (display \"stderr-detail\" (current-error-port)) (test-fail \"cli-assertion-detail\" 'expected 'actual))\n")
        (for-all
          (lambda (args)
            (let ([status #f])
              (let ([text (call-with-string-output-port
                            (lambda (out)
                              (parameterize ([current-output-port out])
                                (set! status (test-main (append args (list path)))))))])
                (and (= status 1)
                     (for-all (lambda (needle) (test-string-contains? text needle))
                              '("cli-failed" "size" "7" "cli-assertion-detail"
                                "stdout-detail" "stderr-detail"))))))
          '(() ("--reporter" "progress"))))
      (lambda () (when (file-exists? path) (delete-file path))))))

(mat test-compile-preserves-source-under-printer-limits
  ;; Error: truncating invalid source must not turn a compiler rejection into a pass.
  (for-all
    (lambda (limits)
      (let* ([invalid '((define bad (lambda (x x) x)))]
             [descriptors
              (list (make-test-compile 'invalid invalid '())
                    (make-test-compile 'expected-invalid invalid
                      `((raises . ,syntax-violation?)))
                    (make-test-compile 'valid
                      '((define good (lambda (x) (+ x 1)))) '()))]
             [results (parameterize ([print-length (car limits)]
                                     [print-level (cadr limits)])
                        (test-run descriptors (test-silent-config)))])
        (and (equal? (map test-result-status results) '(errored passed passed))
             (syntax-violation? (test-result-condition (car results))))))
    '((#f #f) (1 #f) (#f 1) (1 1))))

(mat test-human-reports-allow-absent-messages
  ;; Results constructed by callers may omit messages, including on failures and loose xpass.
  (for-all
    (lambda (reporter)
      (let ([text (call-with-string-output-port
                    (lambda (out)
                      (test-report reporter out
                        (test-summarize
                          (list (make-test-result 0 'absent-failure 'failed #f #f "" "" '() #f)
                                (make-test-result 1 'absent-xpass 'xpass #f #f "" "" '() #f))))))])
        (and (test-string-contains? text "absent-failure")
             (test-string-contains? text "absent-xpass"))))
    (list (test-text-reporter) (test-progress-reporter 'never))))

(mat test-datum-summary-preserves-counts-under-printer-limits
  ;; Diagnostic printer limits must not discard counts from the serialized summary.
  (for-all
    (lambda (limits)
      (let* ([summary
              (test-summarize
                (map (lambda (status)
                       (make-test-result status status status "" #f "" "" '() #f))
                     '(passed failed errored skipped xfail xpass)))]
             [text
              (parameterize ([print-length (car limits)] [print-level (cadr limits)])
                (call-with-string-output-port
                  (lambda (out) (test-report (test-datum-reporter) out summary))))])
        (call-with-port (open-input-string text)
          (lambda (in)
            (and (equal? (read in)
                         '(test-summary (total . 6) (passed . 1) (failed . 1)
                                        (errored . 1) (skipped . 1) (xfail . 1) (xpass . 1)))
                 (eof-object? (read in)))))))
    '((#f #f) (1 #f) (#f 1) (1 1))))

(mat test-output-expectations-do-not-reenter-condition-handlers
  ;; Error: output mismatches remain failures even when every body condition is handled.
  (for-all
    (lambda (raises?)
      (for-all
        (lambda (xfail?)
          (let ([calls 0])
            (let* ([descriptor
                    (make-test-case 'handled-output-mismatch
                      (lambda ()
                        (display "actual-out")
                        (display "actual-err" (current-error-port))
                        (when raises? (error 'body "handled")))
                      `((capture . (stdout stderr)) (stdout . "expected-out")
                        (condition-handler . ,(lambda (condition)
                                                (set! calls (+ calls 1)) 'handled))
                        (xfail-when . (,(lambda (parameters) xfail?) . "known mismatch"))))]
                   [result (car (test-run (list descriptor)
                                  (test-config-with (test-silent-config)
                                    '((xfail-strict? . #t)))))]
                   [condition (test-result-condition result)])
              (and (= calls (if raises? 1 0))
                   (eq? (test-result-status result) (if xfail? 'xfail 'failed))
                   (string=? (test-result-stdout result) "actual-out")
                   (string=? (test-result-stderr result) "actual-err")
                   (test-failure? condition)
                   (equal? (test-failure-expected condition) "expected-out")
                   (equal? (test-failure-actual condition) "actual-out")))))
        '(#f #t)))
    '(#f #t))

  ;; A handled condition with matching output is still an unexpected pass under strict xfail.
  (let* ([descriptor
          (make-test-case 'handled-output-match
            (lambda () (display "expected") (error 'body "handled"))
            `((capture . stdout) (stdout . "expected")
              (condition-handler . ,(lambda (condition) 'handled))
              (xfail-when . (,(lambda (parameters) #t) . "known issue"))))]
         [result (car (test-run (list descriptor)
                        (test-config-with (test-silent-config) '((xfail-strict? . #t)))))])
    (and (eq? (test-result-status result) 'failed)
         (string=? (test-result-stdout result) "expected")))

  ;; Error: a failing policy callback keeps captured diagnostics without invoking itself again.
  (let ([calls 0])
    (let ([result
           (car (test-run
                  (list (make-test-case 'broken-handler
                          (lambda () (display "diagnostic") (error 'body "original"))
                          `((capture . stdout)
                            (condition-handler . ,(lambda (condition)
                                                    (set! calls (+ calls 1))
                                                    (error 'handler "policy error"))))))
                  (test-silent-config)))])
      (and (= calls 1)
           (eq? (test-result-status result) 'errored)
           (string=? (test-result-stdout result) "diagnostic")
           (eq? (condition-who (test-result-condition result)) 'handler)))))

(mat test-procedural-parameter-name-validation
  ;; Error: procedural tables reject duplicate names just as the definition macros do.
  (for-all
    (lambda (tables)
      (test-error-message-contains?
        (lambda ()
          (test-expand-parameters
            (make-test-case 'duplicate (lambda (parameters) #t)
              (list (cons 'parameterize tables)))))
        "duplicate parameter name"))
    '((((x x) (1 2)))
      (((x) (1)) ((x) (2)))
      (((x)) ((x) (2)))
      (((x) (1)) ((y x) (2 3)))))

  ;; Error: malformed names and rows must be rejected before Cartesian expansion.
  (for-all
    (lambda (tables)
      (test-error-message-contains?
        (lambda ()
          (test-expand-parameters
            (make-test-case 'malformed (lambda (parameters) #t)
              (list (cons 'parameterize tables)))))
        "parameter"))
    '(not-tables (() ) ((x (1))) (((42) (1))) (((x . y) (1 2)))
      (((x) (1 . 2))) (((x)) ((y) (1 2)))))

  ;; Error: inherited tables use the same validation for every child and execution phase.
  (for-all
    (lambda (child)
      (test-error-message-contains?
        (lambda ()
          (test-expand-parameters
            (make-test-suite 'parent (list child)
              '((parameterize . (((x) (1)) ((x) (2))))))))
        "duplicate parameter name"))
    (list (make-test-case 'runtime (lambda () #t) '())
          (make-test-expand 'expand '(+ 1 2) '())
          (make-test-compile 'compile '((define x 1)) '())))

  ;; Distinct tables preserve row order, and explicit child tables replace inherited tables.
  (let* ([tables '(((x) (1) (2)) ((y) (a) (b)))]
         [descriptor (make-test-case 'product (lambda () #t)
                       (list (cons 'parameterize tables)))])
    (and (equal? (map test-concrete-case-parameters (test-expand-parameters descriptor))
                 '(((x . 1) (y . a)) ((x . 1) (y . b))
                   ((x . 2) (y . a)) ((x . 2) (y . b))))
         (equal? (map test-concrete-case-parameters
                      (test-expand-parameters
                        (make-test-suite 'parent (list descriptor)
                          '((parameterize . (((x) (9))))))))
                 '(((x . 1) (y . a)) ((x . 1) (y . b))
                   ((x . 2) (y . a)) ((x . 2) (y . b)))))))

(mat test-runner-color-policy
  ;; The runner's explicit color policy overrides the reporter; auto keeps its default.
  (for-all
    (lambda (row)
      (let* ([reporter (test-progress-reporter (car row))]
             [text
              (call-with-string-output-port
                (lambda (out)
                  (test-run (list (make-test-case 'colored (lambda () #t) '()))
                    (test-config-with (default-test-config)
                      `((reporter . ,reporter) (output . ,out) (color . ,(cadr row)))))))])
        (eq? (test-string-contains? text (string (integer->char 27))) (caddr row))))
    '((auto always #t) (always never #f) (never always #t)
      (always auto #t) (never auto #f) (auto auto #f)))

  ;; Direct reports preserve their constructor policy after a runner override.
  (let* ([reporter (test-progress-reporter 'always)]
         [summary (test-summarize (list (make-test-result 0 'direct 'passed "" #f "" "" '() #f)))])
    (call-with-string-output-port
      (lambda (out)
        (test-run '() (make-test-config reporter out #f #f 'never))))
    (test-string-contains?
      (call-with-string-output-port (lambda (out) (test-report reporter out summary)))
      (string (integer->char 27))))

  ;; A nested run must not leak its explicit color override into a delegated outer report.
  (let* ([progress (test-progress-reporter 'auto)]
         [reporter
          (make-test-reporter 'nested
            (lambda (out summary)
              (call-with-string-output-port
                (lambda (inner)
                  (test-run '() (make-test-config progress inner #f #f 'never))))
              (test-report progress out summary)))]
         [text
          (call-with-string-output-port
            (lambda (out)
              (test-run (list (make-test-case 'outer (lambda () #t) '()))
                        (make-test-config reporter out #f #f 'always))))])
    (test-string-contains? text (string (integer->char 27)))))

(mat test-fixture-binding-name-validation
  ;; Multiple references to one inherited fixture are one binding, including nested suites.
  (parameterize ([current-test-registry (make-test-registry)])
    (test-suite parent :fixture ([x 1])
      (test-suite nested
        (test-case child (test-= x 1) (test-= x 1))))
    (equal? (map test-result-status
                 (test-run-registry (current-test-registry) (test-silent-config)))
            '(passed)))

  ;; Error: fixtures cannot hide parameters or repeat names, including inherited bindings.
  (for-all
    (lambda (form)
      (guard (condition [else (syntax-violation? condition)])
        (eval form (environment '(cspp)))
        #f))
    '((test-case overlap :fixture ([x 9]) :parameterize ([x] [1] [2]) #t)
      (test-case duplicate :fixture ([x 1] [x 2]) #t)
      (test-suite parent :fixture ([x 9])
        (test-case child :parameterize ([x] [1] [2]) #t))
      (test-suite parent :parameterize ([x] [1] [2])
        (test-case child :fixture ([x 9]) #t))))

  ;; Error: low-level descriptors reject conflicts before any fixture is acquired.
  (let ([acquired? #f])
    (let ([metadata
           `((fixtures . ((x . ,(lambda () (set! acquired? #t) 9))))
             (parameterize . (((x) (1) (2)))))])
      (and (test-error-message-contains?
             (lambda ()
               (test-expand-parameters (make-test-case 'overlap (lambda () #t) metadata)))
             "fixture")
           (not acquired?))))

  ;; Error: caller-created concrete cases must validate before acquisition as well.
  (let ([acquired? #f])
    (let* ([descriptor (make-test-case 'overlap (lambda () #t) '())]
           [concrete
            (make-test-concrete-case 0 'overlap descriptor '((x . 1))
              `((fixtures . ((x . ,(lambda () (set! acquired? #t) 9))))))]
           [result (car (test-run (list concrete) (test-silent-config)))])
      (and (not acquired?) (eq? (test-result-status result) 'errored))))

  ;; Child overrides can remove a conflict; distinct fixture and parameter names still bind.
  (parameterize ([current-test-registry (make-test-registry)])
    (test-suite parent :fixture ([x 9])
      (test-case child :fixture ([resource 42]) :parameterize ([x] [1] [2])
        (test-= resource 42) (test-true (memv x '(1 2)))))
    (equal? (map test-result-status
                 (test-run-registry (current-test-registry) (test-silent-config)))
            '(passed passed))))

(mat test-reject-repeated-fixture-clauses
  ;; Error: repeated fixture clauses must reject rather than bind discarded setup names to #f.
  (for-all
    (lambda (form)
      (guard (condition [else (syntax-violation? condition)])
        (eval form (environment '(cspp)))
        #f))
    '((test-case repeated :fixture ([a 1]) :fixture ([b 2]) (test-true a))
      (test-suite repeated :fixture ([a 1]) :fixture ([b 2]) (test-case child #t))
      (test-case repeated :fixture () :fixture ([b 2]) #t)))

  ;; Error: rejecting repetition happens before fixture expressions or registration can run.
  (let ([registry (make-test-registry)])
    (parameterize ([current-test-registry registry])
      (and (guard (condition [else (syntax-violation? condition)])
             (eval '(test-case repeated
                      :fixture ([a (error 'fixture "must not acquire")])
                      :fixture ([b 2])
                      #t)
                   (environment '(cspp)))
             #f)
           (null? (test-registry-descriptors registry))))))

(mat test-compile-preserves-generated-identifiers
  ;; Generated formals with identical pretty names must remain distinct compiler identifiers.
  (for-all
    (lambda (mode)
      (let* ([left (gensym "temp")] [right (gensym "temp")]
             [valid (list (list 'define 'generated
                            (list 'lambda (list left right) left)))]
             [invalid (list (list 'define 'duplicate
                              (list 'lambda (list left left) left)))]
             [results
              (parameterize ([print-gensym mode])
                (test-run
                  (list (make-test-compile 'valid-generated valid '())
                        ;; Error: repeated use of the same generated formal remains invalid.
                        (make-test-compile 'invalid-generated invalid '())
                        (make-test-compile 'expected-invalid-generated invalid
                          `((raises . ,syntax-violation?))))
                  (test-silent-config)))])
        (and (equal? (map test-result-status results) '(passed errored passed))
             (syntax-violation? (test-result-condition (cadr results))))))
    '(#t #f pretty)))

(mat test-rejects-invalid-runtime-arity
  ;; Error: an unusable body must be rejected before raises can accept a runner error.
  (test-error-message-contains?
    (lambda () (make-test-case 'bad-arity (lambda (a b) #t)
                 (list (cons 'raises error?))))
    "zero or one"))

(mat test-compound-warnings-retain-failures
  ;; Error: a warning component must not suppress a test failure or serious condition.
  (for-all
    (lambda (row)
      (let* ([resumed? #f]
             [result (car (test-run
                            (list (make-test-case 'compound-warning
                                    (lambda ()
                                      (raise-continuable (condition (make-warning) (car row)))
                                      (set! resumed? #t))
                                    '()))
                            (test-silent-config)))])
        (and (eq? (test-result-status result) (cdr row)) (not resumed?))))
    (list (cons (make-test-failure "failure" #t #f) 'failed)
          (cons (make-error) 'errored))))

(mat test-compile-preparation-is-not-an-expected-condition
  ;; Error: inability to create compiler input must not satisfy a raises expectation.
  (or (not (file-exists? "/proc/self"))
      (let ([result
             (parameterize ([current-directory "/proc"])
               (car (test-run
                      (list (make-test-compile 'unwritable '((define x 1))
                              (list (cons 'raises i/o-error?))))
                      (test-silent-config))))])
        (eq? (test-result-status result) 'errored))))

(mat test-compile-isolates-reader-case
  ;; Error: ambient folding must not merge distinct source identifiers.
  (let ([results
         (parameterize ([case-sensitive #f])
           (test-run
             (list (make-test-compile 'distinct-case '((define f (lambda (A a) A))) '())
                   (make-test-compile 'no-false-expectation '((define f (lambda (A a) A)))
                     (list (cons 'raises syntax-violation?))))
             (test-silent-config)))])
    (equal? (map test-result-status results) '(passed failed))))

(mat test-compile-leaves-no-coverage-input
  (let* ([temporary-coverage?
          (lambda (name)
            (and (test-string-contains? name "test-compile-")
                 (test-string-contains? name ".covin")))]
         [before (filter temporary-coverage? (directory-list "."))])
    (dynamic-wind
      (lambda () (void))
      (lambda ()
        (let ([result
               (parameterize ([generate-covin-files #t] [compile-profile 'source])
                 (car (test-run
                        (list (make-test-compile 'no-coverage '((define x 1) (+ x 2)) '()))
                        (test-silent-config))))])
          (and (eq? (test-result-status result) 'passed)
               (equal? before (filter temporary-coverage? (directory-list "."))))))
      (lambda ()
        (for-each (lambda (name) (unless (member name before) (delete-file name)))
                  (filter temporary-coverage? (directory-list ".")))))))

(mat test-suite-immediate-expression-results
  (let ([registry (make-test-registry)])
    (parameterize ([current-test-registry registry])
      (let ([suite
             (test-suite immediate
               (begin
                 (test-case discarded (test-true #f))
                 (test-case returned (test-true #t)))
               (make-test-case 'procedural (lambda () #t) '()))])
        (and (equal? (map test-descriptor-name (test-descriptor-children suite))
                     '(returned procedural))
             (equal? (map test-descriptor-name (test-registry-descriptors registry))
                     '(immediate))
             (equal? (map test-result-status (test-run (list suite) (test-silent-config)))
                     '(passed passed))))))

  ;; Error: each immediate expression must return a descriptor, not a list or arbitrary value.
  (for-all
    (lambda (form)
      (guard (raised [else #t]) (eval form (environment '(cspp))) #f))
    '((test-suite invalid #t)
      (test-suite invalid (list (make-test-case 'child (lambda () #t) '()))))))

(mat test-strict-macro-inputs
  ;; Error: reject malformed names, misplaced options, and repeated singleton options at expansion.
  (for-all
    (lambda (form)
      (guard (raised [else (syntax-violation? raised)])
        (expand form (environment '(cspp)))
        #f))
    '((test-case (invalid name) #t)
      (test-suite 42)
      (test-case repeated :warnings 'allow :warnings 'error #t)
      (test-case repeated :skip "first" :only-when #t "second" #t)
      (test-case misplaced #t :warnings 'error)
      (test-case missing-body :tags (fast))
      (test-compile missing-body)
      (test-case invalid-tags :tags (42) #t))))

(mat test-strict-metadata-inputs
  ;; Error: malformed metadata and invalid field types must fail before registration or execution.
  (for-all
    (lambda (metadata)
      (guard (raised [else #t])
        (make-test-case 'invalid-metadata (lambda () #t) metadata)
        #f))
    '((invalid)
      ((1 . 2))
      ((warnings . typo))
      ((warnings . allow) (warnings . error))
      ((unknown-option . #t))
      ((tags . ("not-a-symbol")))
      ((xfail-strict . yes))
      ((requires-chez-version . (999)))
      ((requires-chez-version . (1 -1 0)))
      ((requires-file . 42))
      ((skip-when . (#t . "reason")))
      ((xfail-when . ((lambda (parameters) #t) . 42)))
      ((raises . 42))
      ((before . 42))
      ((fixtures . ((resource . 42))))
      ((stdout . 42)))))

(mat test-concrete-case-strict-parameters
  ;; Error: concrete parameters must have unique symbolic keys in a proper association list.
  (for-all
    (lambda (parameters)
      (guard (raised [else #t])
        (make-test-concrete-case 0 'bad
          (make-test-case 'bad (lambda () #t) '()) parameters '())
        #f))
    '((invalid) ((42 . value)) ((name . 1) (name . 2)))))

(mat test-selector-and-config-strict-inputs
  ;; Error: tag selectors use the same symbolic domain as descriptor tags.
  (for-all
    (lambda (selector)
      (guard (raised [else #t])
        (test-select (make-test-registry) selector)
        #f))
    '(((include-tags . (42))) ((include-tags . ("fast")))
      ((include-tags . (fast . slow)))))

  ;; Error: duplicate config keys must not discard an earlier value in one update.
  (guard (raised [else #t])
    (test-config-with (test-silent-config)
      '((xfail-strict? . #t) (xfail-strict? . #f)))
    #f))

(mat test-callback-contracts-before-execution
  ;; Error: assertion invocation errors must not satisfy expected body conditions.
  (for-all
    (lambda (predicate)
      (guard (raised [else #t])
        (test-raises predicate (lambda (required) #t))
        #f))
    (list condition? assertion-violation?))

  ;; Error: reporters reject callbacks that cannot receive output and summary.
  (guard (raised [else #t])
    (make-test-reporter 'invalid (lambda () #t))
    #f)

  ;; Optional and variadic callbacks remain valid when they accept the required arguments.
  (and (test-raises error? (lambda arguments (error 'expected "body")))
       (test-reporter? (make-test-reporter 'valid (lambda arguments (void))))))

(mat test-strict-result-and-registry-domains
  ;; Error: summaries and result parameter lists must contain correctly typed elements.
  (and (guard (raised [else #t])
         (make-test-summary 1 1 0 0 0 0 0 '(invalid)) #f)
       (guard (raised [else #t])
         (make-test-result 0 'invalid 'passed "" #f "" "" '(invalid) #f) #f)
       (guard (raised [else #t])
         (parameterize ([current-test-registry 42]) #f))))

(mat test-run-files-validates-all-path-types
  ;; Error: a later invalid path must reject before an earlier file is loaded.
  (let ([path (format "test-path-validation-~a.ss" (random 1000000))])
    (dynamic-wind
      (lambda ()
        (putprop 'test-path-validation 'loaded #f)
        (call-with-output-file path
          (lambda (out) (display "(putprop 'test-path-validation 'loaded #t)\n" out))
          'replace))
      (lambda ()
        (guard (raised [else (not (getprop 'test-path-validation 'loaded))])
          (test-run-files (list path 42) (test-silent-config))
          #f))
      (lambda ()
        (when (file-exists? path) (delete-file path))
        (remprop 'test-path-validation 'loaded)))))

(mat test-quoted-policy-resolution-is-shared-per-run
  ;; Equivalent quoted predicates must not compile separately for every parameter row.
  (let* ([descriptor
          (make-test-case 'quoted-policy (lambda () #t)
            `((parameterize . (,(cons '(number) (map list (iota 100)))))
              (skip-when . ((lambda (parameters) #f) . "skip"))))]
         [before (statistics)]
         [results (test-run (list descriptor) (test-silent-config))]
         [bytes (sstats-bytes (sstats-difference (statistics) before))])
    (and (= (length results) 100)
         (andmap (lambda (result) (eq? (test-result-status result) 'passed)) results)
         (< bytes 5000000))))

(mat test-wide-concrete-parameter-validation-is-linear
  (let* ([names (map (lambda (n) (string->symbol (format "parameter-~a" n))) (iota 512))]
         [parameters (map cons names (iota 512))]
         [descriptor (make-test-case 'wide (lambda () #t) '())])
    (and (test-concrete-case?
           (make-test-concrete-case 0 'wide descriptor parameters '()))
         ;; Error: sharing the validator must still detect a duplicate at the end.
         (guard (raised [else #t])
           (make-test-concrete-case 0 'wide descriptor
             (append parameters (list (cons (car names) 99))) '())
           #f))))

(mat test-filtering-and-early-stop-do-not-materialize-products
  (let* ([rows (map list (iota 200))]
         [descriptor (make-test-case 'excluded (lambda () (test-true #f))
                       `((parameterize . (,(cons '(left) rows) ,(cons '(right) rows)))))]
         [registry (make-test-registry)])
    (test-register! registry descriptor)
    (let* ([before (statistics)]
           [selected (test-select registry '((name-prefix . "included")))]
           [results (test-run (list descriptor)
                      (test-config-with (test-silent-config) '((stop-on-failure? . #t))))]
           [bytes (sstats-bytes (sstats-difference (statistics) before))])
      (and (null? selected) (= (length results) 1)
           (eq? (test-result-status (car results)) 'failed)
           (equal? (test-result-parameters (car results)) '((left . 0) (right . 0)))
           (< bytes 1000000)))))

(mat test-nested-suite-flattening-does-not-copy-descendants
  (let ([descriptor
         (let loop ([depth 100]
                    [descriptor
                     (make-test-case 'leaf (lambda () #t)
                       `((parameterize . (,(cons '(number) (map list (iota 2000)))))))])
           (if (zero? depth) descriptor
               (loop (- depth 1) (make-test-suite 'nested (list descriptor) '()))))])
    (let* ([before (statistics)]
           [cases (test-expand-parameters descriptor)]
           [bytes (sstats-bytes (sstats-difference (statistics) before))])
      (and (= (length cases) 2000)
           (= (test-concrete-case-id (car cases)) 0)
           (= (test-concrete-case-id (car (reverse cases))) 1999)
           (< bytes 5000000)))))

(mat test-streaming-preserves-selection-ids-and-row-snapshots
  (let ([registry (make-test-registry)])
    (test-register! registry
      (make-test-case 'excluded (lambda () #t) '((parameterize . (((n) (0) (1)))))))
    (test-register! registry
      (make-test-case 'included (lambda () #t) '((parameterize . (((n) (2) (3)))))))
    (let ([selected (test-select registry '((name-prefix . "included")))])
      (and (equal? (map test-concrete-case-id selected) '(2 3))
           (equal? (map test-concrete-case-parameters selected) '(((n . 2)) ((n . 3)))))))

  ;; Changes to caller-owned table rows during execution do not change the prepared run.
  (let* ([first (list 1)] [second (list 2)]
         [descriptor (make-test-case 'snapshot
                       (lambda (parameters)
                         (when (= (cdr (assq 'number parameters)) 1) (set-car! second 99)))
                       (list (cons 'parameterize (list (list '(number) first second)))))]
         [results (test-run (list descriptor) (test-silent-config))])
    (equal? (map test-result-parameters results) '(((number . 1)) ((number . 2))))))

(mat test-streaming-validates-later-descriptors-before-running
  ;; Error: deferred expansion must not defer invalid descriptor-domain checks past body effects.
  (let ([called? #f])
    (guard (raised [else (not called?)])
      (test-run
        (list (make-test-case 'first (lambda () (set! called? #t)) '())
              (make-test-case 'invalid (lambda () #t) '((parameterize . (((n) (1 2)))))))
        (test-silent-config))
      #f)))

(mat test-empty-later-axis-short-circuits-prefix-products
  (let* ([tables (append
                   (map (lambda (n)
                          (list (list (string->symbol (format "axis-~a" n))) '(0) '(1)))
                        (iota 15))
                   '(((empty))))]
         [descriptor (make-test-case 'empty-product (lambda () #t)
                       (list (cons 'parameterize tables)))]
         [before (statistics)]
         [cases (test-expand-parameters descriptor)]
         [bytes (sstats-bytes (sstats-difference (statistics) before))])
    (and (null? cases) (< bytes 1000000))))
