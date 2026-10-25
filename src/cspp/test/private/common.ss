#!chezscheme
(library (cspp test private common)
  (export test-reporter? make-test-reporter test-reporter-write
          test-alist-ref test-alist-set test-remove-key test-metadata-ref
          test-natural-version-list?
          chez-version
          chez-version<? chez-version<=? chez-version=? chez-version>=? chez-version>?)
  (import (cspp chez)
          (cspp utils))

  (define-record-type ($test-reporter $make-test-reporter test-reporter?)
    (fields kind write))

  #|proc:make-test-reporter
  The `make-test-reporter` procedure creates a reporter. `kind` is a symbol
  naming the reporter, and `write` is a procedure with signature
  `(lambda (output-port summary) unspecified)`.
  |#
  (define make-test-reporter
    (lambda (kind write)
      (pcheck ([symbol? kind] [procedure? write])
              ($make-test-reporter kind write))))

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
