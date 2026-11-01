#!chezscheme
(library (cspp test common)
  (export test-reporter? make-test-reporter test-reporter-write
          test-make-textual-reporter test-check-reporter-output
          test-alist-ref test-alist-set test-remove-key test-metadata-ref
          test-natural-version-list?
          test-check-parameter-tables test-check-fixture-names test-check-metadata
          test-color-mode? test-current-report-color
          test-callback-accepts? test-symbol-alist?
          chez-version
          chez-version<? chez-version<=? chez-version=? chez-version>=? chez-version>?)
  (import (cspp chez)
          (cspp utils))

  (define-record-type ($test-reporter $make-test-reporter test-reporter?)
    (fields kind write textual-output?))

  (define $make-checked-reporter
    (lambda (kind write textual-output?)
      (unless (test-callback-accepts? write 2)
        (errorf 'make-test-reporter "write must accept output and summary"))
      ($make-test-reporter kind write textual-output?)))

  #|proc:test-color-mode?
  The internal `test-color-mode?` procedure returns whether `value` is a supported
  reporting color mode: `auto`, `always`, or `never`.
  |#
  (define test-color-mode?
    (lambda (value)
      (and (memq value '(auto always never)) #t)))

  #|proc:test-current-report-color
  The internal `test-current-report-color` parameter carries the runner's color
  override during reporting. `auto` retains each reporter's constructor policy;
  `always` and `never` override it. The default is `auto` for direct reports.
  |#
  (define test-current-report-color
    (make-parameter 'auto
      (lambda (color)
        (pcheck ([test-color-mode? color]) color))))

  #|proc:make-test-reporter
  The `make-test-reporter` procedure creates a reporter. `kind` is a symbol
  naming the reporter, and `write` is a procedure with signature
  `(lambda (output-port summary) unspecified)`.
  Custom reporters may use binary output ports when their callbacks support them.
  |#
  (define make-test-reporter
    (lambda (kind write)
      (pcheck ([symbol? kind] [procedure? write])
              ($make-checked-reporter kind write #f))))

  #|proc:test-make-textual-reporter
  The internal `test-make-textual-reporter` procedure creates a reporter named by symbol `kind`.
  Its `write` callback has signature `(lambda (output-port summary) unspecified)`.
  The returned reporter requires an open textual output port when reporting is enabled.
  |#
  (define test-make-textual-reporter
    (lambda (kind write)
      (pcheck ([symbol? kind] [procedure? write])
              ($make-checked-reporter kind write #t))))

  #|proc:test-check-reporter-output
  The internal `test-check-reporter-output` procedure validates `output` for `reporter`.
  `reporter` is a reporter or `#f`; `output` is an output port or `#f`. Disabled reporting
  requires no port compatibility check. Errors name symbol `who`; success returns unspecified
  values.
  |#
  (define test-check-reporter-output
    (lambda (who reporter output)
      (pcheck ([symbol? who]
               [(lambda (value) (or (not value) (test-reporter? value))) reporter]
               [(lambda (value) (or (not value) (output-port? value))) output])
              (when (and reporter output ($test-reporter-textual-output? reporter))
                (unless (and (textual-port? output) (not (port-closed? output)))
                  (errorf who "built-in reporter requires an open textual output port"))))))

  #|proc:test-reporter-write
  The `test-reporter-write` procedure returns the callback stored in `reporter`.
  The callback accepts `(output-port summary)` and returns unspecified values.
  This helper is internal to the test libraries.
  |#
  (define test-reporter-write
    (lambda (reporter)
      (pcheck ([test-reporter? reporter])
              ($test-reporter-write reporter))))

  (define $version-compare
    (lambda (left right)
      (cond
       [(and (null? left) (null? right)) 0]
       [(null? left)
        (if (zero? (car right))
            ($version-compare left (cdr right))
            -1)]
       [(null? right)
        (if (zero? (car left))
            ($version-compare (cdr left) right)
            1)]
       [(< (car left) (car right)) -1]
       [(> (car left) (car right)) 1]
       [else ($version-compare (cdr left) (cdr right))])))

  #|proc:test-alist-ref
The `test-alist-ref` procedure returns the value associated with symbol `key`
in association list `alist`. When `default` is supplied, it is returned for a
missing key; otherwise a missing key is an error.
|#
  (define test-alist-ref
    (case-lambda
      [(alist key)
       (pcheck ([list? alist] [symbol? key])
               (let ([probe (assq key alist)])
                 (if probe
                     (cdr probe)
                     (errorf 'test-alist-ref "missing key: ~a" key))))]
      [(alist key default)
       (pcheck ([list? alist] [symbol? key])
               (let ([probe (assq key alist)])
                 (if probe (cdr probe) default)))]))

  #|proc:test-remove-key
The `test-remove-key` procedure returns a copy of association list `alist`
without entries whose car is symbol `key`.
|#
  (define test-remove-key
    (lambda (key alist)
      (pcheck ([symbol? key] [list? alist])
              (let loop ([alist alist] [out '()])
                (cond
                 [(null? alist) (reverse out)]
                 [(eq? key (caar alist)) (loop (cdr alist) out)]
                 [else (loop (cdr alist) (cons (car alist) out))])))))

  #|proc:test-alist-set
The `test-alist-set` procedure returns a copy of association list `alist` with
symbol `key` associated with `value`, replacing any existing entries for `key`.
|#
  (define test-alist-set
    (lambda (alist key value)
      (pcheck ([list? alist] [symbol? key])
              (cons (cons key value) (test-remove-key key alist)))))

  #|proc:test-metadata-ref
The `test-metadata-ref` procedure returns the metadata value associated with
symbol `key` in metadata association list `metadata`. When `default` is
supplied, it is returned for a missing key; otherwise a missing key is an error.
|#
  (define test-metadata-ref
    (case-lambda
      [(metadata key)
       (pcheck ([list? metadata] [symbol? key])
               (test-alist-ref metadata key))]
      [(metadata key default)
       (pcheck ([list? metadata] [symbol? key])
               (test-alist-ref metadata key default))]))

  #|proc:test-check-parameter-tables
  The internal `test-check-parameter-tables` procedure validates `tables`, a list of
  parameter tables of the form `((name ...) row ...)`. Names must be symbols unique
  across all tables, and every row must be a proper list of the corresponding width.
  Invalid tables raise an error attributed to symbol `who`; valid tables return
  unspecified values. Macros and the runner share this validation.
  |#
  (define test-check-parameter-tables
    (lambda (who tables)
      (pcheck ([symbol? who])
              (unless (list? tables)
                (errorf who "parameter tables must be a proper list: ~s" tables))
              (let loop-tables ([tables tables] [seen '()])
                (unless (null? tables)
                  (let ([table (car tables)])
                    (unless (and (list? table) (pair? table) (list? (car table)))
                      (errorf who "invalid parameter table: ~s" table))
                    (let loop-names ([names (car table)] [seen seen])
                      (cond
                       [(null? names)
                        (let ([width (length (car table))])
                          (for-each
                            (lambda (row)
                              (unless (and (list? row) (= (length row) width))
                                (errorf who "parameter row length must match name list: ~s" row)))
                            (cdr table)))
                        (loop-tables (cdr tables) seen)]
                       [(not (symbol? (car names)))
                        (errorf who "parameter name must be a symbol: ~s" (car names))]
                       [(memq (car names) seen)
                        (errorf who "duplicate parameter name: ~s" (car names))]
                       [else (loop-names (cdr names) (cons (car names) seen))]))))))))

  #|proc:test-check-fixture-names
  The internal `test-check-fixture-names` procedure checks that `fixture-names`
  are symbols unique within their list and disjoint from `parameter-names`.
  Both arguments are lists of names. Errors are attributed to symbol `who`;
  valid names return unspecified values. Macro expansion and execution share this check.
  |#
  (define test-check-fixture-names
    (lambda (who fixture-names parameter-names)
      (pcheck ([symbol? who] [list? fixture-names parameter-names])
              (let loop ([names fixture-names] [seen '()])
                (unless (null? names)
                  (let ([name (car names)])
                    (unless (symbol? name)
                      (errorf who "fixture name must be a symbol: ~s" name))
                    (when (memq name seen)
                      (errorf who "duplicate fixture name: ~s" name))
                    (when (memq name parameter-names)
                      (errorf who "fixture name conflicts with parameter: ~s" name))
                    (loop (cdr names) (cons name seen))))))))

  #|proc:test-callback-accepts?
  The internal `test-callback-accepts?` procedure returns whether `value` is a
  procedure accepting `arity` arguments, where `arity` is a natural number.
  |#
  (define test-callback-accepts?
    (lambda (value arity)
      (pcheck ([natural? arity])
              (and (procedure? value) (logbit? arity (procedure-arity-mask value))))))

  #|proc:test-symbol-alist?
  The internal `test-symbol-alist?` procedure returns whether `value` is a proper
  association list with unique symbolic keys. Association values may be arbitrary data.
  |#
  (define test-symbol-alist?
    (lambda (value)
      (and (list? value)
           (let ([seen (make-eq-hashtable (length value))])
             (let loop ([entries value])
               (or (null? entries)
                   (let ([entry (car entries)])
                     (and (pair? entry) (symbol? (car entry))
                          (not (hashtable-contains? seen (car entry)))
                          (begin
                            (hashtable-set! seen (car entry) #t)
                            (loop (cdr entries)))))))))))

  (define $predicate-data?
    (lambda (value)
      (or (symbol? value)
          (and (list? value) (>= (length value) 3)
               (eq? (car value) 'lambda) (list? (cadr value))
               (= (length (cadr value)) 1) (symbol? (caadr value))))))

  #|proc:test-check-metadata
  The internal `test-check-metadata` procedure validates descriptor `metadata`, a
  symbol-keyed association list with unique, supported keys. Invalid fields raise
  an error attributed to symbol `who`; valid input returns unspecified values.
  Callbacks must accept the arguments used by the runner. Predicate data may be a
  procedure name or a one-argument lambda, resolved in Chez at run time.
  |#
  (define test-check-metadata
    (lambda (who metadata)
      (pcheck ([symbol? who] [list? metadata])
              (let loop ([entries metadata] [seen '()])
                (unless (null? entries)
                  (let ([entry (car entries)])
                    (unless (and (pair? entry) (symbol? (car entry)))
                      (errorf who "metadata entry must have a symbolic key: ~s" entry))
                    (when (memq (car entry) seen)
                      (errorf who "duplicate metadata key: ~s" (car entry)))
                    (let ([value (cdr entry)])
                      (unless
                        (case (car entry)
                          [(tags) (and (list? value) (andmap symbol? value))]
                          [(warnings) (memq value '(allow error))]
                          [(xfail-strict) (boolean? value)]
                          [(capture)
                           (or (not value) (memq value '(stdout stderr combined))
                               (and (list? value)
                                    (andmap (lambda (stream) (memq stream '(stdout stderr)))
                                            value)))]
                          [(stdout stderr output requires-file)
                           (or (not value) (string? value))]
                          [(requires-chez-version)
                           (or (not value)
                               (and (test-natural-version-list? value) (= (length value) 3)))]
                          [(before after condition-handler)
                           (or (not value) (test-callback-accepts? value 1))]
                          [(raises)
                           (or (not value) (test-callback-accepts? value 1) ($predicate-data? value))]
                          [(skip-when xfail-when)
                           (or (not value)
                               (and (pair? value) (string? (cdr value))
                                    (or (test-callback-accepts? (car value) 1)
                                        ($predicate-data? (car value)))))]
                          [(fixtures)
                           (and (list? value)
                                (andmap (lambda (fixture)
                                          (and (pair? fixture) (symbol? (car fixture))
                                               (test-callback-accepts? (cdr fixture) 0)))
                                        value))]
                          ;; Parameter domains are checked before Cartesian expansion.
                          [(parameterize) (list? value)]
                          [(source) #t]
                          [else (errorf who "unknown metadata key: ~s" (car entry))])
                        (errorf who "invalid metadata field ~s: ~s" (car entry) value)))
                    (loop (cdr entries) (cons (car entry) seen))))))))

  #|proc:test-natural-version-list?
The `test-natural-version-list?` procedure returns whether `value` is a list of
natural-number version components.
|#
  (define test-natural-version-list?
    (lambda (value)
      (and (list? value) (andmap natural? value))))

  #|proc:chez-version
The `chez-version` procedure returns the running Chez Scheme version as a list
of natural-number components from Chez's `scheme-version-number`.
|#
  (define chez-version
    (lambda ()
      (call-with-values scheme-version-number list)))

  #|proc:chez-version<?
The `chez-version<?` procedure returns whether version list `left` is less than
version list `right`.
|#
  (define chez-version<?
    (lambda (left right)
      (pcheck ([test-natural-version-list? left right])
              (< ($version-compare left right) 0))))

  #|proc:chez-version<=?
The `chez-version<=?` procedure returns whether version list `left` is less than
or equal to version list `right`.
|#
  (define chez-version<=?
    (lambda (left right)
      (pcheck ([test-natural-version-list? left right])
              (<= ($version-compare left right) 0))))

  #|proc:chez-version=?
The `chez-version=?` procedure returns whether version list `left` is equal to
version list `right`.
|#
  (define chez-version=?
    (lambda (left right)
      (pcheck ([test-natural-version-list? left right])
              (= ($version-compare left right) 0))))

  #|proc:chez-version>=?
The `chez-version>=?` procedure returns whether version list `left` is greater
than or equal to version list `right`.
|#
  (define chez-version>=?
    (lambda (left right)
      (pcheck ([test-natural-version-list? left right])
              (>= ($version-compare left right) 0))))

  #|proc:chez-version>?
The `chez-version>?` procedure returns whether version list `left` is greater
than version list `right`.
|#
  (define chez-version>?
    (lambda (left right)
      (pcheck ([test-natural-version-list? left right])
              (> ($version-compare left right) 0))))

  )
