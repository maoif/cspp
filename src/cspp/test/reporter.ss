#!chezscheme
(library (cspp test reporter)
  (export test-reporter? make-test-reporter test-text-reporter
          test-datum-reporter test-progress-reporter test-report test-summarize)
  (import (cspp chez)
          (cspp utils)
          (cspp test descriptor)
          (cspp test private common))

  (define $count-status
    (lambda (status results)
      (let loop ([results results] [count 0])
        (cond
         [(null? results) count]
         [(eq? (test-result-status (car results)) status)
          (loop (cdr results) (fx+ count 1))]
         [else (loop (cdr results) count)]))))

  #|proc:test-summarize
The `test-summarize` procedure returns a summary record for `results`.
`results` is a list of test result records.
|#
  (define test-summarize
    (lambda (results)
      (pcheck ([list? results])
              (unless (andmap test-result? results)
                (errorf 'test-summarize "results must all be test results: ~a" results))
              (make-test-summary
               (length results)
               ($count-status 'passed results)
               ($count-status 'failed results)
               ($count-status 'errored results)
               ($count-status 'skipped results)
               ($count-status 'xfail results)
               ($count-status 'xpass results)
               results))))

  (define $write-text-summary
    (lambda (out summary)
      (fprintf out "passed: ~a\n" (test-summary-passed summary))
      (fprintf out "failed: ~a\n" (test-summary-failed summary))
      (fprintf out "errored: ~a\n" (test-summary-errored summary))
      (fprintf out "skipped: ~a\n" (test-summary-skipped summary))
      (fprintf out "xfail: ~a\n" (test-summary-xfail summary))
      (fprintf out "xpass: ~a\n" (test-summary-xpass summary))
      (fprintf out "total: ~a\n" (test-summary-total summary))))

  (define $ansi-wrap
    (lambda (color text enabled?)
      (if enabled?
          (string-append color text "\x1b;[0m")
          text)))

  (define $status-text
    (lambda (status color)
      (let ([enabled? (eq? color 'always)])
        (case status
          [(passed) ($ansi-wrap "\x1b;[32m" "." enabled?)]
          [(failed) ($ansi-wrap "\x1b;[31m" "F" enabled?)]
          [(errored) ($ansi-wrap "\x1b;[35m" "E" enabled?)]
          [(skipped) ($ansi-wrap "\x1b;[33m" "s" enabled?)]
          [(xfail) ($ansi-wrap "\x1b;[36m" "x" enabled?)]
          [(xpass) ($ansi-wrap "\x1b;[31m" "X" enabled?)]
          [else "?"]))))

  #|proc:test-text-reporter
The `test-text-reporter` procedure returns a reporter that writes a plain text
summary.
|#
  (define test-text-reporter
    (lambda ()
      (make-test-reporter 'text $write-text-summary)))

  (define $write-datum-summary
    (lambda (out summary)
      (write
       `(test-summary
         (total . ,(test-summary-total summary))
         (passed . ,(test-summary-passed summary))
         (failed . ,(test-summary-failed summary))
         (errored . ,(test-summary-errored summary))
         (skipped . ,(test-summary-skipped summary))
         (xfail . ,(test-summary-xfail summary))
         (xpass . ,(test-summary-xpass summary)))
       out)
      (newline out)))

  #|proc:test-datum-reporter
The `test-datum-reporter` procedure returns a reporter that writes a Scheme
datum summary.
|#
  (define test-datum-reporter
    (lambda ()
      (make-test-reporter 'datum $write-datum-summary)))

  #|proc:test-progress-reporter
The `test-progress-reporter` procedure returns a reporter that writes one
status character per result followed by the standard text summary. `color` is
`auto`, `always`, or `never`; only `always` forces ANSI styling.
|#
  (define test-progress-reporter
    (lambda (color)
      (unless (memq color '(auto always never))
        (errorf 'test-progress-reporter "invalid color mode: ~a" color))
      (make-test-reporter
       'progress
       (lambda (out summary)
         (for-each
          (lambda (result)
            (display ($status-text (test-result-status result) color) out))
          (test-summary-results summary))
         (newline out)
         ($write-text-summary out summary)))))

  #|proc:test-report
The `test-report` procedure writes `summary` to `output-port` using `reporter`.
|#
  (define test-report
    (lambda (reporter output-port summary)
      (pcheck ([test-reporter? reporter] [output-port? output-port] [test-summary? summary])
              ((test-reporter-write reporter) output-port summary))))

  )
