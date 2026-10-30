(library (cspp benchmark)
  (export benchmark? benchmark-suite? benchmark-state? benchmark-result?
          benchmark-sample? benchmark-config? benchmark-fixture?
          benchmark-reporter?

          benchmark-name
          benchmark-state benchmark-state-name benchmark-state-suite
          benchmark-state-args benchmark-state-arg
          benchmark-state-template-args benchmark-state-template-arg
          benchmark-state-iteration benchmark-state-iterations
          benchmark-state-sample benchmark-state-user-data
          benchmark-state-set-user-data!
          benchmark-state-counter-set! benchmark-state-counter-add!

          make-benchmark benchmark-register! benchmark-unregister!
          benchmark-clear-registry! benchmark-registry-benchmarks
          current-benchmark-registry

          make-benchmark-config default-benchmark-config benchmark-config-with
          benchmark-config-warmup benchmark-config-samples
          benchmark-config-min-time benchmark-config-max-iterations
          benchmark-config-output benchmark-config-filter
          benchmark-config-reporter benchmark-config-cost-center?

          benchmark-run benchmark-run-one benchmark-run-suite benchmark-select
          benchmark-expand benchmark-summarize
          benchmark-pause-timing benchmark-resume-timing
          benchmark-do-not-optimize benchmark-clobber-memory

          make-benchmark-reporter benchmark-text-reporter
          benchmark-datum-reporter benchmark-csv-reporter
          benchmark-json-reporter benchmark-rich-reporter benchmark-report
          benchmark-save-baseline benchmark-load-baseline
          benchmark-percent-difference benchmark-absolute-difference
          benchmark-compare-results benchmark-comparison?
          benchmark-comparison-name benchmark-comparison-metric
          benchmark-comparison-baseline benchmark-comparison-current
          benchmark-comparison-baseline-value benchmark-comparison-current-value
          benchmark-comparison-absolute-difference
          benchmark-comparison-percent-difference
          benchmark-comparison-regression? benchmark-comparison-improvement?
          benchmark-sample-cost-center-allocation-count
          benchmark-sample-cost-center-instruction-count
          benchmark-sample-cost-center-time-ns

          benchmark-result-name benchmark-result-args
          benchmark-result-template-args benchmark-result-samples
          benchmark-result-summary benchmark-result-counters
          benchmark-result-error
          benchmark-sample-iterations benchmark-sample-sstats
          benchmark-sample-cost-center benchmark-sample-counters

          define-benchmark define-benchmark-suite define-benchmark-fixture
          define-fixture-benchmark define-benchmark-template
          instantiate-benchmark-template benchmark-options)
  (import (cspp chez)
          (cspp utils)
          (cspp internal)
          (cspp rich))

  #|doc
  Execution requirements are documented in `docs/test-and-benchmark.md`. Coordinate definitions,
  registries, runners, reporting, and persistence synchronously from one controlling OS thread,
  outside fibers. Concurrent coordinator calls, reentrant reporting, and continuation reentry
  are undefined. Benchmark state, counters, and timing controls belong to the controlling thread.
  Record predicates and read-only descriptor/config/result/sample/comparison accessors support
  workers when their payloads remain stable. Numeric difference helpers and optimization barriers
  also support independent worker calls. Bodies must join their workers before returning.
  Measurements use controlling-thread CPU time, elapsed wall time, and process-wide allocation
  and garbage-collection statistics. Worker CPU time is not aggregated.
  |#


;;;;===----------------------------------------------------------------------===
;;;; records and low-level helpers
;;;;===----------------------------------------------------------------------===

  (define-record-type ($benchmark $mk-benchmark benchmark?)
    (fields name body options axes setup teardown
            (immutable fixture $benchmark-fixture-ref)
            (immutable suite $benchmark-suite-ref)
            template-args))

  (define-record-type ($benchmark-fixture $mk-benchmark-fixture benchmark-fixture?)
    (fields name setup teardown))

  (define-record-type ($benchmark-suite $mk-benchmark-suite benchmark-suite?)
    (fields name benchmarks setup teardown))

  (define-record-type ($benchmark-state $mk-benchmark-state benchmark-state?)
    (fields name suite args template-args
            (mutable iteration $benchmark-state-iteration $benchmark-state-iteration-set!)
            iterations sample
            (mutable user-data benchmark-state-user-data benchmark-state-set-user-data!)
            (mutable counters $benchmark-state-counters $benchmark-state-counters-set!)
            (mutable counter-index $benchmark-state-counter-index $benchmark-state-counter-index-set!)
            (mutable timing-paused? $benchmark-state-timing-paused? $benchmark-state-timing-paused?-set!)
            (mutable pause-start $benchmark-state-pause-start $benchmark-state-pause-start-set!)
            (mutable paused-sstats $benchmark-state-paused-sstats $benchmark-state-paused-sstats-set!)))

  (define-record-type ($benchmark-sample $mk-benchmark-sample benchmark-sample?)
    (fields iterations sstats cost-center counters))

  (define-record-type ($benchmark-result $mk-benchmark-result benchmark-result?)
    ;; Internal samples #f preserves the absence of sample data in historical baselines.
    (fields name args template-args samples summary counters error))

  (define-record-type ($benchmark-config $mk-benchmark-config benchmark-config?)
    (fields warmup samples min-time max-iterations output filter reporter cost-center? stop-on-error?
            explicit-options))

  (define-record-type ($benchmark-reporter $mk-benchmark-reporter benchmark-reporter?)
    (fields start result finish textual-output?))

  (define-record-type ($benchmark-comparison $mk-benchmark-comparison benchmark-comparison?)
    (fields name metric baseline current baseline-value current-value absolute-difference
            percent-difference regression? improvement?))

  (define-record-type ($benchmark-registry $mk-benchmark-registry benchmark-registry?)
    (fields (mutable reversed $benchmark-registry-reversed $benchmark-registry-reversed-set!)
            (mutable snapshot $benchmark-registry-snapshot $benchmark-registry-snapshot-set!)))

  (define $benchmark-registry-benchmarks
    (lambda (registry)
      (or ($benchmark-registry-snapshot registry)
          (let ([snapshot (reverse ($benchmark-registry-reversed registry))])
            ($benchmark-registry-snapshot-set! registry snapshot)
            snapshot))))

  (define $void-procedure (lambda args (void)))

  (define $false-procedure (lambda args #f))

  (define $identity (lambda (x) x))

  (define $symbol-alist?
    (lambda (value)
      (and (list? value)
           (let ([seen (make-eq-hashtable (length value))])
             (for-all
               (lambda (entry)
                 (and (pair? entry) (symbol? (car entry))
                      (not (hashtable-contains? seen (car entry)))
                      (begin (hashtable-set! seen (car entry) #t) #t)))
               value)))))

  (define $alist-ref
    (case-lambda
      [(alist key)
       (let ([probe (assq key alist)])
         (if probe
             (cdr probe)
             (errorf '$alist-ref "missing key: ~a" key)))]
      [(alist key default)
       (let ([probe (assq key alist)])
         (if probe (cdr probe) default))]))

  (define $option-ref
    (case-lambda
      [(options key default)
       (let ([probe (assq key options)])
         (if probe (cdr probe) default))]))

  (define $remove-key
    (lambda (key alist)
      (let loop ([alist alist] [out '()])
        (cond
         [(null? alist) (reverse out)]
         [(eq? key (caar alist)) (loop (cdr alist) out)]
         [else (loop (cdr alist) (cons (car alist) out))]))))

  (define $replace-option
    (lambda (options key value)
      (cons (cons key value) ($remove-key key options))))

  (define $integer-sqrt
    (lambda (n)
      (inexact->exact (floor (sqrt (exact->inexact n))))))

  (define $list-last
    (lambda (xs)
      (if (null? (cdr xs))
          (car xs)
          ($list-last (cdr xs)))))

  (define $sort-ascending
    (lambda (xs)
      (list-sort < xs)))

  (define $time->ns
    (lambda (t)
      (+ (* (time-second t) 1000000000)
         (time-nanosecond t))))

  (define $ns->time
    (lambda (ns)
      (let ([ns (max 0 ns)])
        (make-time 'time-duration
                   (modulo ns 1000000000)
                   (quotient ns 1000000000)))))

  (define $zero-sstats
    (lambda ()
      (make-sstats (make-time 'time-duration 0 0)
                   (make-time 'time-duration 0 0)
                   0
                   0
                   (make-time 'time-duration 0 0)
                   (make-time 'time-duration 0 0)
                   0)))

  (define $nonnegative-time
    (lambda (t)
      (if (< ($time->ns t) 0)
          (make-time 'time-duration 0 0)
          t)))

  (define $nonnegative-count
    (lambda (n)
      (if (< n 0) 0 n)))

  (define $sanitize-sstats
    (lambda (s)
      (make-sstats ($nonnegative-time (sstats-cpu s))
                   ($nonnegative-time (sstats-real s))
                   ($nonnegative-count (sstats-bytes s))
                   ($nonnegative-count (sstats-gc-count s))
                   ($nonnegative-time (sstats-gc-cpu s))
                   ($nonnegative-time (sstats-gc-real s))
                   ($nonnegative-count (sstats-gc-bytes s)))))

  (define $sstats-add
    (lambda (left right)
      (make-sstats ($ns->time (+ ($time->ns (sstats-cpu left))
                                 ($time->ns (sstats-cpu right))))
                   ($ns->time (+ ($time->ns (sstats-real left))
                                  ($time->ns (sstats-real right))))
                   (+ (sstats-bytes left) (sstats-bytes right))
                   (+ (sstats-gc-count left) (sstats-gc-count right))
                   ($ns->time (+ ($time->ns (sstats-gc-cpu left))
                                 ($time->ns (sstats-gc-cpu right))))
                   ($ns->time (+ ($time->ns (sstats-gc-real left))
                                  ($time->ns (sstats-gc-real right))))
                   (+ (sstats-gc-bytes left) (sstats-gc-bytes right)))))

  (define $make-benchmark-state
    (lambda (name suite args template-args iteration iterations sample user-data counters timing-paused?)
      ($mk-benchmark-state name suite args template-args iteration iterations sample
                           user-data counters #f timing-paused? #f ($zero-sstats))))

  (define $sample-cpu-ns
    (lambda (sample)
      (let ([iterations (inexact->exact (benchmark-sample-iterations sample))])
        (if (zero? iterations)
            0
            (/ ($time->ns (sstats-cpu (benchmark-sample-sstats sample)))
               iterations)))))

  (define $sample-real-ns
    (lambda (sample)
      (let ([iterations (inexact->exact (benchmark-sample-iterations sample))])
        (if (zero? iterations)
            0
            (/ ($time->ns (sstats-real (benchmark-sample-sstats sample)))
               iterations)))))

  (define $sample-bytes
    (lambda (sample)
      (let ([iterations (inexact->exact (benchmark-sample-iterations sample))])
        (if (zero? iterations)
            0
            (/ (sstats-bytes (benchmark-sample-sstats sample))
               iterations)))))

  (define $mean
    (lambda (xs)
      (if (null? xs)
          0
          (/ (apply + xs) (length xs)))))

  (define $median
    (lambda (xs)
      (if (null? xs)
          0
          (let* ([sorted ($sort-ascending xs)]
                 [len (length sorted)]
                 [mid (quotient len 2)])
            (if (odd? len)
                (list-ref sorted mid)
                (/ (+ (list-ref sorted (- mid 1))
                      (list-ref sorted mid))
                   2))))))

  (define $stddev
    (lambda (xs)
      (if (or (null? xs) (null? (cdr xs)))
          0
          (let ([mean ($mean xs)])
            (sqrt (/ (apply + (map (lambda (x)
                                     (let ([delta (- x mean)])
                                       (* delta delta)))
                                   xs))
                     (length xs)))))))

  (define $summary-values
    (lambda (xs)
      (if (null? xs)
          '((median . 0) (mean . 0) (min . 0) (max . 0) (stddev . 0)
            (confidence-interval . (0 0)))
          (let* ([mean ($mean xs)]
                 [stddev ($stddev xs)]
                 [margin (if (null? (cdr xs))
                             0
                             (* 1.96 (/ stddev (sqrt (length xs)))))])
            `((median . ,($median xs))
              (mean . ,mean)
              (min . ,(apply min xs))
              (max . ,(apply max xs))
              (stddev . ,stddev)
              (confidence-interval . ,(list (- mean margin) (+ mean margin))))))))

  (define $merge-counters
    (lambda (samples)
      (let ([index (make-eq-hashtable)])
        (let loop-samples ([samples samples] [out '()])
          (if (null? samples)
              (reverse out)
              (let loop-counters ([counters (benchmark-sample-counters (car samples))]
                                  [out out])
                (if (null? counters)
                    (loop-samples (cdr samples) out)
                    (let* ([entry (car counters)]
                           [key (car entry)]
                           [value (cdr entry)]
                           [old (hashtable-ref index key #f)])
                      (if old
                          (begin
                            (set-cdr! old (+ (cdr old) value))
                            (loop-counters (cdr counters) out))
                          (let ([entry (cons key value)])
                            (hashtable-set! index key entry)
                            (loop-counters (cdr counters) (cons entry out))))))))))))

  (define $string-replace
    (lambda (text old new)
      (let ([old-len (string-length old)]
            [text-len (string-length text)]
            [out (open-output-string)])
        (let loop ([i 0])
          (cond
           [(>= i text-len)
            (get-output-string out)]
           [(and (<= (+ i old-len) text-len)
                 (string=? old (substring text i (+ i old-len))))
            (display new out)
            (loop (+ i old-len))]
           [else
            (write-char (string-ref text i) out)
            (loop (+ i 1))])))))

  (define $write-readable-datum
    (lambda (value out)
      (parameterize ([print-graph #t] [print-level #f] [print-length #f] [print-gensym #t]
                     [case-sensitive #t])
        (write value out))))

  (define $write-string
    (lambda (value)
      (call-with-string-output-port (lambda (out) ($write-readable-datum value out)))))

  (define $readback-equal?
    (lambda (value text)
      (guard (raised [else #f])
        (call-with-port (open-input-string text)
          (lambda (in) (parameterize ([case-sensitive #t]) (equal? value (read in))))))))

  (define $diagnostic-datum
    (lambda (value)
      (let ([text ($write-string value)])
        (if ($readback-equal? value text) value text))))

  (define $csv-field
    (lambda (value)
      (let* ([text ($write-string value)]
             [escaped ($string-replace text "\"" "\"\"")])
        (string-append "\"" escaped "\""))))

  (define $json-string
    (lambda (text out)
      (write-char (integer->char 34) out)
      (let loop ([i 0])
        (when (< i (string-length text))
          (let ([ch (string-ref text i)])
            (cond
             [(char=? ch (integer->char 34)) (display "\\\"" out)]
             [(char=? ch #\\) (display "\\\\" out)]
             [(char=? ch #\backspace) (display "\\b" out)]
             [(char=? ch #\page) (display "\\f" out)]
             [(char=? ch #\newline) (display "\\n" out)]
             [(char=? ch #\return) (display "\\r" out)]
             [(char=? ch #\tab) (display "\\t" out)]
             [(< (char->integer ch) 32)
              (display "\\u" out)
              (let ([digits (number->string (char->integer ch) 16)])
                (display (make-string (- 4 (string-length digits)) #\0) out)
                (display digits out))]
             [else (write-char ch out)])
            (loop (+ i 1)))))
      (write-char (integer->char 34) out)))

  (define $json-key
    (lambda (key)
      (cond
       [(gensym? key) ($write-string key)]
       [(symbol? key) (symbol->string key)]
       [(string? key) key]
       [(char? key) (string key)]
       [else ($write-string key)])))

  (define $json-number-string
    (lambda (n)
      ;; Chez's optional mantissa-width suffix is Scheme syntax, not JSON numeric syntax.
      (parameterize ([print-precision #f] [print-subnormal-precision #f])
        (number->string
          (if (and (exact? n) (rational? n) (not (integer? n)))
              (let ([approximation (exact->inexact n)])
                ;; Keep a readable exact string if conversion loses the range entirely.
                (if (and (finite? approximation) (not (zero? approximation)))
                    approximation
                    n))
              n)))))

  (define $json-safe-number-string?
    (lambda (text)
      (let ([len (string-length text)])
        (let loop ([i 0])
          (cond
           [(>= i len) #t]
           [(char=? (string-ref text i) #\/) #f]
           [(char=? (string-ref text i) #\i) #f]
           [(and (char=? (string-ref text i) #\+)
                 (< (+ i 1) len)
                 (char=? (string-ref text (+ i 1)) #\i))
            #f]
           [(and (char=? (string-ref text i) #\-)
                 (< (+ i 1) len)
                 (char=? (string-ref text (+ i 1)) #\i))
            #f]
           [(and (char=? (string-ref text i) #\i)
                 (< (+ i 2) len)
                 (char=? (string-ref text (+ i 1)) #\n)
                 (char=? (string-ref text (+ i 2)) #\f))
            #f]
           [(and (char=? (string-ref text i) #\n)
                 (< (+ i 2) len)
                 (char=? (string-ref text (+ i 1)) #\a)
                 (char=? (string-ref text (+ i 2)) #\n))
            #f]
           [else (loop (+ i 1))])))))

  (define $write-json-number
    (lambda (n out)
      (let ([text ($json-number-string n)])
        (if ($json-safe-number-string? text)
            (display text out)
            ($json-string text out)))))

  (define $alist?
    (lambda (x)
      (and (list? x)
           (andmap pair? x))))

  (define $write-json-list
    (lambda (xs out ancestors)
      (display "[" out)
      (let loop ([xs xs] [first? #t])
        (unless (null? xs)
          (unless first? (display "," out))
          ($write-json (car xs) out ancestors)
          (loop (cdr xs) #f)))
      (display "]" out)))

  (define $write-json-object
    (lambda (alist out ancestors)
      (let* ([keys (map (lambda (entry) ($json-key (car entry))) alist)]
             [seen (make-hashtable string-hash string=?)]
             [unique? (andmap (lambda (key)
                                (and (not (hashtable-contains? seen key))
                                     (begin (hashtable-set! seen key #t) #t)))
                              keys)])
        (if (not unique?)
            ;; JSON object names cannot represent every distinct Scheme key.
            ($json-string ($write-string alist) out)
            (begin
              (display "{" out)
              (let loop ([alist alist] [keys keys] [first? #t])
                (unless (null? alist)
                  (unless first? (display "," out))
                  ($json-string (car keys) out)
                  (display ":" out)
                  ($write-json (cdar alist) out ancestors)
                  (loop (cdr alist) (cdr keys) #f)))
              (display "}" out))))))

  (define $write-json
    (lambda (datum out ancestors)
      (cond
       ;; Only ancestors indicate recursion; shared acyclic values keep their JSON shape.
       [(and (pair? datum) (memq datum ancestors))
        ($json-string ($write-string datum) out)]
       [(eq? datum #t) (display "true" out)]
       [(eq? datum #f) (display "false" out)]
       [(number? datum) ($write-json-number datum out)]
       [(string? datum) ($json-string datum out)]
       [(symbol? datum) ($json-string (symbol->string datum) out)]
       [(null? datum) (display "[]" out)]
       [($alist? datum) ($write-json-object datum out (cons datum ancestors))]
       [(list? datum) ($write-json-list datum out (cons datum ancestors))]
       [else ($json-string ($write-string datum) out)])))

  (define $captured-benchmark-error
    (lambda (raised)
      (or raised
          (condition (make-error)
                     (make-message-condition "benchmark raised #f")
                     (make-irritants-condition (list #f))))))

  (define $condition->datum
    (lambda (exn)
      (if (condition? exn)
          `((who . ,(and (who-condition? exn) ($diagnostic-datum (condition-who exn))))
            (message . ,(if (message-condition? exn)
                            (condition-message exn)
                            "benchmark error"))
            (irritants . ,(if (irritants-condition? exn)
                              (map $diagnostic-datum (condition-irritants exn))
                              '())))
          `((who . #f)
            (message . ,($write-string exn))
            (irritants . ())))))

  (define $datum->condition
    (lambda (datum)
      (let* ([who ($alist-ref datum 'who #f)]
             [message ($alist-ref datum 'message "benchmark baseline error")]
             [irritants ($alist-ref datum 'irritants '())]
             [parts (append (list (make-error)
                                  (make-message-condition message))
                            (if who
                                (list (make-who-condition who))
                                '())
                            (if (null? irritants)
                                '()
                                (list (make-irritants-condition irritants))))])
        (apply condition parts))))

  (define $metric-mean
    (lambda (result metric)
      (let ([entry (assq metric (benchmark-result-summary result))])
        (if entry
            ($alist-ref (cdr entry) 'mean 0)
            0))))

  (define $comparison-metric-mean
    (lambda (result metric)
      (and (not (benchmark-result-error result))
           (not (null? ($benchmark-result-samples result)))
           (let* ([entry (assq metric (benchmark-result-summary result))]
                  [mean (and entry ($alist-ref (cdr entry) 'mean #f))])
             (and (real? mean) (finite? mean) mean)))))

  (define $benchmark-report-value
    (lambda (value)
      (cond
       [(number? value) (number->string value)]
       [(symbol? value) (symbol->string value)]
       [(string? value) value]
       [(not value) ""]
       [else ($write-string value)])))

  (define $benchmark-result-iterations
    (lambda (result)
      (if (null? (benchmark-result-samples result))
          0
          (benchmark-sample-iterations
           (car (benchmark-result-samples result))))))

  (define $benchmark-result-column-value
    (lambda (result key)
      (case key
        [(name) (benchmark-result-name result)]
        [(args) (benchmark-result-args result)]
        [(template-args) (benchmark-result-template-args result)]
        [(iterations) ($benchmark-result-iterations result)]
        [(cpu-ns) ($metric-mean result 'cpu-ns)]
        [(real-ns) ($metric-mean result 'real-ns)]
        [(bytes) ($metric-mean result 'bytes)]
        [(counters) (benchmark-result-counters result)]
        [(error)
         (let ([error (benchmark-result-error result)])
           (and error ($alist-ref ($condition->datum error) 'message)))]
        [else (errorf 'benchmark-rich-reporter "unknown result column: ~a" key)])))

  (define $benchmark-result-column-justify
    (lambda (key)
      (case key
        [(iterations cpu-ns real-ns bytes) 'right]
        [else 'left])))

  (define $benchmark-rich-default-columns
    '((name . "name")
      (args . "args")
      (template-args . "template-args")
      (iterations . "iterations")
      (cpu-ns . "cpu/ns")
      (real-ns . "real/ns")
      (bytes . "bytes/iter")
      (counters . "counters")
      (error . "error")))

  (define $benchmark-text-default-columns
    '((name . "name")
      (args . "args")
      (template-args . "template-args")
      (iterations . "iterations")
      (cpu-ns . "cpu/ns")
      (real-ns . "real/ns")
      (bytes . "bytes/iter")
      (counters . "counters")
      (error . "error")))

  (define $string-pad-left
    (lambda (text width)
      (let ([len (string-length text)])
        (if (>= len width)
            text
            (string-append (make-string (- width len) #\space) text)))))

  (define $string-pad-right
    (lambda (text width)
      (let ([len (string-length text)])
        (if (>= len width)
            text
            (string-append text (make-string (- width len) #\space))))))

  (define $benchmark-column-widths
    (lambda (rows columns)
      (map (lambda (column)
             (let ([label (cdr column)])
               (let loop ([rows rows] [width (string-length label)])
                 (if (null? rows)
                     width
                     (loop (cdr rows)
                           (max width
                                (string-length (cdr (assq (car column) (car rows))))))))))
           columns)))

  (define $benchmark-format-row
    (lambda (row columns widths)
      (let loop ([columns columns] [widths widths] [out '()])
        (if (null? columns)
            (apply string-append (reverse out))
            (let* ([key (caar columns)]
                   [text (cdr (assq key row))]
                   [width (car widths)]
                   [formatted (if (eq? 'right ($benchmark-result-column-justify key))
                                  ($string-pad-left text width)
                                  ($string-pad-right text width))]
                   [piece (if (null? out)
                              formatted
                              (string-append "  " formatted))])
              (loop (cdr columns) (cdr widths) (cons piece out)))))))

  (define $benchmark-results->rows
    (lambda (results columns)
      (map (lambda (result)
             (map (lambda (column)
                    (cons (car column)
                          ($benchmark-report-value
                           ($benchmark-result-column-value result (car column)))))
                  columns))
           results)))

  (define $benchmark-text-lines
    (lambda (results)
      (let* ([columns $benchmark-text-default-columns]
             [rows ($benchmark-results->rows results columns)]
             [widths ($benchmark-column-widths rows columns)]
             [header-row (map (lambda (column)
                                (cons (car column) (cdr column)))
                              columns)])
        (cons ($benchmark-format-row header-row columns widths)
              (map (lambda (row)
                     ($benchmark-format-row row columns widths))
                   rows)))))

  (define $benchmark-rich-table
    (lambda (results options)
      (let ([table (make-rich-table)]
            [columns ($option-ref options 'columns $benchmark-rich-default-columns)])
        (rich-table-title-set! table ($option-ref options 'title "Benchmark results"))
        (rich-table-caption-set! table ($option-ref options 'caption #f))
        (rich-table-box-set! table ($option-ref options 'box 'ascii))
        (rich-table-show-header?-set! table ($option-ref options 'show-header? #t))
        (rich-table-show-lines?-set! table ($option-ref options 'show-lines? #t))
        (rich-table-padding-set! table ($option-ref options 'padding 1))
        (for-each (lambda (column)
                    (rich-table-add-column! table
                                            (cdr column)
                                            ($benchmark-result-column-justify (car column))))
                  columns)
        (for-each (lambda (result)
                    (apply rich-table-add-row!
                           table
                           (map (lambda (column)
                                  ($benchmark-report-value
                                   ($benchmark-result-column-value result (car column))))
                                columns)))
                  results)
        table)))

  (define $result-key
    (lambda (result)
      (list (benchmark-result-name result)
            (benchmark-result-args result)
            (benchmark-result-template-args result))))

  (define $sample->datum
    (lambda (sample)
      `((iterations . ,(benchmark-sample-iterations sample))
        (cpu-ns . ,($time->ns (sstats-cpu (benchmark-sample-sstats sample))))
        (real-ns . ,($time->ns (sstats-real (benchmark-sample-sstats sample))))
        (bytes . ,(sstats-bytes (benchmark-sample-sstats sample)))
        (gc-count . ,(sstats-gc-count (benchmark-sample-sstats sample)))
        (gc-cpu-ns . ,($time->ns (sstats-gc-cpu (benchmark-sample-sstats sample))))
        (gc-real-ns . ,($time->ns (sstats-gc-real (benchmark-sample-sstats sample))))
        (gc-bytes . ,(sstats-gc-bytes (benchmark-sample-sstats sample)))
        (cost-center . ,(benchmark-sample-cost-center sample))
        (counters . ,(benchmark-sample-counters sample)))))

  ;; Validate persisted data once, before constructing records used by statistics/reporters.
  (define $baseline-check
    (lambda (predicate value context)
      (unless (predicate value)
        (errorf 'benchmark-load-baseline "invalid ~a: ~s" context value))))

  (define $baseline-alist
    (lambda (datum context)
      ($baseline-check list? datum context)
      (let ([seen (make-eq-hashtable)])
        (for-each
          (lambda (entry)
            (unless (and (pair? entry) (symbol? (car entry))
                         (not (hashtable-contains? seen (car entry))))
              (errorf 'benchmark-load-baseline "invalid or duplicate ~a field: ~s" context entry))
            (hashtable-set! seen (car entry) #t))
          datum))))

  (define $baseline-counters
    (lambda (datum context)
      ($baseline-alist datum context)
      (for-each (lambda (entry) ($baseline-check number? (cdr entry) context)) datum)))

  (define $baseline-summary
    (lambda (datum)
      ($baseline-alist datum 'summary)
      (for-each
        (lambda (metric)
          ($baseline-alist (cdr metric) (car metric))
          (for-each
            (lambda (entry)
              (let ([value (cdr entry)])
                (if (eq? (car entry) 'confidence-interval)
                    ($baseline-check
                      (lambda (value)
                        (and (list? value) (= (length value) 2) (andmap real? value)))
                      value 'confidence-interval)
                    ($baseline-check
                      (lambda (value)
                        (and (real? value)
                             (or (not (eq? (car entry) 'stddev)) (>= value 0))))
                      value (car entry)))))
            (cdr metric)))
        datum)))

  (define $datum->sample
    (lambda (datum)
      ($baseline-alist datum 'sample)
      (for-each
        (lambda (key)
          ($baseline-check (lambda (value)
                             (and (natural? value)
                                  (or (eq? key 'iterations) (exact? value))))
                           ($alist-ref datum key 0) key))
        '(iterations cpu-ns real-ns bytes gc-count gc-cpu-ns gc-real-ns gc-bytes))
      ($baseline-counters ($alist-ref datum 'counters '()) 'sample-counters)
      (let ([data ($alist-ref datum 'cost-center #f)])
        (when data ($baseline-counters data 'cost-center)))
      (let ([sstats (make-sstats ($ns->time ($alist-ref datum 'cpu-ns 0))
                                 ($ns->time ($alist-ref datum 'real-ns 0))
                                 ($alist-ref datum 'bytes 0)
                                 ($alist-ref datum 'gc-count 0)
                                 ($ns->time ($alist-ref datum 'gc-cpu-ns 0))
                                 ($ns->time ($alist-ref datum 'gc-real-ns 0))
                                 ($alist-ref datum 'gc-bytes 0))])
        ($mk-benchmark-sample ($alist-ref datum 'iterations 0)
                              sstats
                              ($alist-ref datum 'cost-center #f)
                              ($alist-ref datum 'counters '())))))

  (define $result->datum
    (lambda (result)
      `((name . ,(benchmark-result-name result))
        (args . ,(benchmark-result-args result))
        (template-args . ,(benchmark-result-template-args result))
        ,@(if ($benchmark-result-samples result)
              `((samples . ,(map $sample->datum (benchmark-result-samples result))))
              '())
        (summary . ,(benchmark-result-summary result))
        (counters . ,(benchmark-result-counters result))
        (error . ,(and (benchmark-result-error result)
                       ($condition->datum (benchmark-result-error result)))))))

  (define $datum->result
    (lambda (datum)
      ($baseline-alist datum 'result)
      ($baseline-check symbol? ($alist-ref datum 'name #f) 'result-name)
      ($baseline-alist ($alist-ref datum 'args '()) 'args)
      ($baseline-alist ($alist-ref datum 'template-args '()) 'template-args)
      (when (assq 'samples datum)
        ($baseline-check list? ($alist-ref datum 'samples) 'samples))
      ($baseline-summary ($alist-ref datum 'summary '()))
      ($baseline-counters ($alist-ref datum 'counters '()) 'result-counters)
      ($baseline-check boolean? ($alist-ref datum 'error? #f) 'error?)
      (let ([error-datum ($alist-ref datum 'error #f)])
        (when error-datum
          ($baseline-alist error-datum 'error)
          ($baseline-check string? ($alist-ref error-datum 'message "") 'error-message)
          ($baseline-check list? ($alist-ref error-datum 'irritants '()) 'error-irritants)))
      ($mk-benchmark-result ($alist-ref datum 'name)
                            ($alist-ref datum 'args '())
                            ($alist-ref datum 'template-args '())
                            (and (assq 'samples datum)
                                 (map $datum->sample ($alist-ref datum 'samples)))
                            ($alist-ref datum 'summary '())
                            ($alist-ref datum 'counters '())
                            (let ([error-datum ($alist-ref datum 'error #f)])
                              (cond
                               [error-datum ($datum->condition error-datum)]
                               [($alist-ref datum 'error? #f)
                                ($datum->condition '())]
                               [else #f])))))

  (define $result->json
    (lambda (result)
      `((name . ,(benchmark-result-name result))
        (args . ,(benchmark-result-args result))
        (template_args . ,(benchmark-result-template-args result))
        (summary . ,(benchmark-result-summary result))
        (counters . ,(benchmark-result-counters result))
        (error . ,(and (benchmark-result-error result) #t)))))

  (define $symbol-append
    (lambda (left right)
      (string->symbol (string-append (symbol->string left)
                                     "/"
                                     (symbol->string right)))))

  (define $seconds->ns
    (lambda (seconds)
      (floor (* (if (exact? seconds) seconds (inexact->exact seconds)) 1000000000))))

  (define $sstats-real-ns
    (lambda (s)
      ($time->ns (sstats-real s))))


;;;;===----------------------------------------------------------------------===
;;;; public state and descriptor procedures
;;;;===----------------------------------------------------------------------===

  #|proc:benchmark-name
The `benchmark-name` procedure returns the symbolic name of benchmark descriptor
`benchmark`.
|#
  (define benchmark-name
    (lambda (benchmark)
      (pcheck ([benchmark? benchmark])
              ($benchmark-name benchmark))))

  #|proc:benchmark-state
The `benchmark-state` procedure creates a benchmark state for low-level tests or
custom runners. `name` is the concrete run name, `suite` is a suite name or
`#f`, `args` is an argument alist, `template-args` is a template argument alist,
`iteration` is the current iteration index, `iterations` is the current sample's
iteration count, and `sample` is the current sample index.
Both argument alists must have unique symbolic keys; their values may be arbitrary objects.
|#
  (define benchmark-state
    (lambda (name suite args template-args iteration iterations sample)
      (pcheck ([symbol? name]
               [(lambda (value) (or (not value) (symbol? value))) suite]
               [$symbol-alist? args template-args] [natural? iteration iterations sample])
              ($make-benchmark-state name suite args template-args iteration iterations sample '() '() #f))))

  #|proc:benchmark-state-name
The `benchmark-state-name` procedure returns the concrete run name stored in
`state`.
|#
  (define benchmark-state-name
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-name state))))

  #|proc:benchmark-state-suite
The `benchmark-state-suite` procedure returns the suite name stored in `state`,
or `#f` when the run is not part of a suite.
|#
  (define benchmark-state-suite
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-suite state))))

  #|proc:benchmark-state-args
The `benchmark-state-args` procedure returns the runtime argument alist stored
in `state`.
|#
  (define benchmark-state-args
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-args state))))

  #|proc:benchmark-state-arg
The `benchmark-state-arg` procedure returns the value associated with symbolic
argument `name` in `state`. When `default` is supplied, it is returned if the
argument is absent; otherwise absence is an error.
|#
  (define benchmark-state-arg
    (case-lambda
      [(state name)
       (pcheck ([benchmark-state? state] [symbol? name])
               ($alist-ref ($benchmark-state-args state) name))]
      [(state name default)
       (pcheck ([benchmark-state? state] [symbol? name])
               ($alist-ref ($benchmark-state-args state) name default))]))

  #|proc:benchmark-state-template-args
The `benchmark-state-template-args` procedure returns the template argument
alist stored in `state`.
|#
  (define benchmark-state-template-args
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-template-args state))))

  #|proc:benchmark-state-template-arg
The `benchmark-state-template-arg` procedure returns the value associated with
symbolic template argument `name` in `state`. When `default` is supplied, it is
returned if the template argument is absent; otherwise absence is an error.
|#
  (define benchmark-state-template-arg
    (case-lambda
      [(state name)
       (pcheck ([benchmark-state? state] [symbol? name])
               ($alist-ref ($benchmark-state-template-args state) name))]
      [(state name default)
       (pcheck ([benchmark-state? state] [symbol? name])
               ($alist-ref ($benchmark-state-template-args state) name default))]))

  #|proc:benchmark-state-iteration
The `benchmark-state-iteration` procedure returns the current iteration index
stored in `state`.
|#
  (define benchmark-state-iteration
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-iteration state))))

  #|proc:benchmark-state-iterations
The `benchmark-state-iterations` procedure returns the iteration count for the
current sample stored in `state`.
|#
  (define benchmark-state-iterations
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-iterations state))))

  #|proc:benchmark-state-sample
The `benchmark-state-sample` procedure returns the current sample index stored
in `state`.
|#
  (define benchmark-state-sample
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-sample state))))

  (define $counter-entry
    (lambda (state name)
      (let ([index (or ($benchmark-state-counter-index state)
                       (let ([index (make-eq-hashtable)])
                         (for-each (lambda (entry) (hashtable-set! index (car entry) entry))
                                   ($benchmark-state-counters state))
                         ($benchmark-state-counter-index-set! state index)
                         index))])
        (or (hashtable-ref index name #f)
            (let ([entry (cons name 0)])
              (hashtable-set! index name entry)
              ($benchmark-state-counters-set!
               state (cons entry ($benchmark-state-counters state)))
              entry)))))

  #|proc:benchmark-state-counter-set!
The `benchmark-state-counter-set!` procedure sets counter `name` in `state` to
numeric `value` for the current sample.
|#
  (define benchmark-state-counter-set!
    (lambda (state name value)
      (pcheck ([benchmark-state? state] [symbol? name] [number? value])
              (set-cdr! ($counter-entry state name) value))))

  #|proc:benchmark-state-counter-add!
The `benchmark-state-counter-add!` procedure adds numeric `value` to counter
`name` in `state` for the current sample.
|#
  (define benchmark-state-counter-add!
    (lambda (state name value)
      (pcheck ([benchmark-state? state] [symbol? name] [number? value])
              (let ([entry ($counter-entry state name)])
                (set-cdr! entry (+ value (cdr entry)))))))

  (define $check-option-keys
    (lambda (who options allowed repeatable)
      (let loop ([options options] [seen '()])
        (unless (null? options)
          (let ([option (car options)])
            (unless (and (pair? option) (memq (car option) allowed))
              (errorf who "unknown or unused option: ~s" option))
            (when (and (memq (car option) seen) (not (memq (car option) repeatable)))
              (errorf who "duplicate option: ~s" (car option)))
            (loop (cdr options) (cons (car option) seen)))))))

  (define $check-benchmark-options
    (lambda (who options allowed)
      ($check-option-keys who options allowed '(args arg-range dense-arg-range))
      (for-each
        (lambda (option)
          (unless (and (pair? option) (memq (car option) allowed))
            (errorf who "unknown or unused option: ~s" option))
          (let ([value (cdr option)])
            (unless
              (case (car option)
                [(setup suite-setup)
                 (or (and (procedure? value) (logbit? 1 (procedure-arity-mask value)))
                     (and (eq? who 'make-benchmark) (not value)))]
                [(teardown suite-teardown)
                 (or (and (procedure? value) (logbit? 2 (procedure-arity-mask value)))
                     (and (eq? who 'make-benchmark) (not value)))]
                [(fixture) (or (not value) (benchmark-fixture? value))]
                [(suite) (or (not value) (symbol? value))]
                [(template-args) ($symbol-alist? value)]
                [(arg-range dense-arg-range)
                 (and (list? value) (= (length value) 4) (symbol? (car value)))]
                [else #t])
              (errorf who "invalid option: ~s" option))))
        options)
      (let ([config-options (filter (lambda (option) (memq (car option) $benchmark-config-keys))
                                    options)])
        (unless (null? config-options)
          ;; Reuse the configuration validator, including when a run will override these defaults.
          (benchmark-config-with (default-benchmark-config) config-options)))))

  #|proc:make-benchmark
The `make-benchmark` procedure creates a benchmark descriptor named by symbol
`name`. `body` is a procedure with signature `(lambda (state arg ... setup-value)
...)`; `options` is a benchmark option alist, usually produced by
`benchmark-options`. Options include argument tables/ranges, sample setup/teardown,
fixture and suite context, template arguments, and runner configuration defaults.
Unknown options and invalid hooks/configuration values are rejected at construction.
Setup hooks must accept one argument and teardown hooks must accept two arguments.
Singleton options cannot repeat; argument tables and ranges may repeat with distinct names.
Setup has signature `(lambda (state) value)`; teardown has signature
`(lambda (state value) unspecified)` and runs after the body, even without setup.
When setup raises, it must release its own partial resources; sample teardown is not called.
Without setup, teardown receives `#f` and the body receives no extra argument.
A fixture supplies both hooks; combining it with non-false descriptor hooks is an error.
`unit`, `complexity`, and `throughput` are descriptive metadata; they do not change
measurement or reporting. Argument axes are validated at construction; the runner
traverses their Cartesian product incrementally, preserving option and row order.
|#
  (define make-benchmark
    (lambda (name body options)
      (pcheck ([symbol? name] [procedure? body] [list? options])
              ($check-benchmark-options 'make-benchmark options
                (append '(args arg-range dense-arg-range setup teardown fixture suite template-args
                          unit complexity throughput)
                        $benchmark-config-keys))
              (let ([args ($normalize-arguments options)]
                    [setup ($option-ref options 'setup #f)]
                    [teardown ($option-ref options 'teardown #f)]
                    [fixture ($option-ref options 'fixture #f)])
                (when (and fixture (or setup teardown))
                  (errorf 'make-benchmark "fixture conflicts with setup or teardown hooks"))
                ($mk-benchmark name
                               body
                               options
                               args
                               (if fixture ($benchmark-fixture-setup fixture) setup)
                               (if fixture ($benchmark-fixture-teardown fixture) teardown)
                               fixture
                               ($option-ref options 'suite #f)
                               ($option-ref options 'template-args '()))))))

  #|proc:benchmark-register!
The `benchmark-register!` procedure adds `benchmark` to `registry`. `registry`
must be a benchmark registry, usually `(current-benchmark-registry)`.
|#
  (define benchmark-register!
    (lambda (registry benchmark)
      (pcheck ([benchmark-registry? registry] [benchmark? benchmark])
              (begin
                ($benchmark-registry-reversed-set!
                 registry
                 (cons benchmark ($benchmark-registry-reversed registry)))
                ($benchmark-registry-snapshot-set! registry #f)
                benchmark))))

  #|proc:benchmark-unregister!
The `benchmark-unregister!` procedure removes benchmarks named by symbol `name`
from `registry`.
|#
  (define benchmark-unregister!
    (lambda (registry name)
      (pcheck ([benchmark-registry? registry] [symbol? name])
              ($benchmark-registry-reversed-set!
               registry
               (filter (lambda (benchmark) (not (eq? name ($benchmark-name benchmark))))
                       ($benchmark-registry-reversed registry)))
              ($benchmark-registry-snapshot-set! registry #f))))

  #|proc:benchmark-clear-registry!
The `benchmark-clear-registry!` procedure removes all benchmarks from
`registry`.
|#
  (define benchmark-clear-registry!
    (lambda (registry)
      (pcheck ([benchmark-registry? registry])
              ($benchmark-registry-reversed-set! registry '())
              ($benchmark-registry-snapshot-set! registry '()))))

  #|proc:benchmark-registry-benchmarks
The `benchmark-registry-benchmarks` procedure returns the benchmark descriptors
currently stored in `registry`.
|#
  (define benchmark-registry-benchmarks
    (lambda (registry)
      (pcheck ([benchmark-registry? registry])
              ($benchmark-registry-benchmarks registry))))

  #|proc:current-benchmark-registry
The `current-benchmark-registry` parameter stores the default benchmark
registry used by definition macros and `(benchmark-run)`.
|#
  (define current-benchmark-registry
    (make-parameter ($mk-benchmark-registry '() '())
      (lambda (registry) (pcheck ([benchmark-registry? registry]) registry))))


;;;;===----------------------------------------------------------------------===
;;;; configs and reporters
;;;;===----------------------------------------------------------------------===

  (define $default-reporter #f)

  #|proc:benchmark-result-name
The `benchmark-result-name` procedure returns the concrete name stored in
benchmark `result`.
|#
  (define benchmark-result-name
    (lambda (result)
      (pcheck ([benchmark-result? result])
              ($benchmark-result-name result))))

  #|proc:benchmark-result-args
The `benchmark-result-args` procedure returns the runtime argument alist stored
in benchmark `result`.
|#
  (define benchmark-result-args
    (lambda (result)
      (pcheck ([benchmark-result? result])
              ($benchmark-result-args result))))

  #|proc:benchmark-result-template-args
The `benchmark-result-template-args` procedure returns the template argument
alist stored in benchmark `result`.
|#
  (define benchmark-result-template-args
    (lambda (result)
      (pcheck ([benchmark-result? result])
              ($benchmark-result-template-args result))))

  #|proc:benchmark-result-samples
The `benchmark-result-samples` procedure returns the measured sample records
stored in benchmark `result`.
|#
  (define benchmark-result-samples
    (lambda (result)
      (pcheck ([benchmark-result? result])
              (or ($benchmark-result-samples result) '()))))

  #|proc:benchmark-result-summary
The `benchmark-result-summary` procedure returns the summary alist stored in
benchmark `result`.
|#
  (define benchmark-result-summary
    (lambda (result)
      (pcheck ([benchmark-result? result])
              ($benchmark-result-summary result))))

  #|proc:benchmark-result-counters
The `benchmark-result-counters` procedure returns the merged custom counter
alist stored in benchmark `result`.
|#
  (define benchmark-result-counters
    (lambda (result)
      (pcheck ([benchmark-result? result])
              ($benchmark-result-counters result))))

  #|proc:benchmark-result-error
The `benchmark-result-error` procedure returns the captured exception stored in
benchmark `result`, or `#f` when the run completed. A raised `#f` is wrapped
in an error condition whose irritants contain the original value.
|#
  (define benchmark-result-error
    (lambda (result)
      (pcheck ([benchmark-result? result])
              ($benchmark-result-error result))))

  #|proc:benchmark-sample-iterations
The `benchmark-sample-iterations` procedure returns the iteration count stored
in sample `sample`.
|#
  (define benchmark-sample-iterations
    (lambda (sample)
      (pcheck ([benchmark-sample? sample])
              ($benchmark-sample-iterations sample))))

  #|proc:benchmark-sample-sstats
The `benchmark-sample-sstats` procedure returns the sanitized Chez sstats
record stored in sample `sample`.
|#
  (define benchmark-sample-sstats
    (lambda (sample)
      (pcheck ([benchmark-sample? sample])
              ($benchmark-sample-sstats sample))))

  #|proc:benchmark-sample-cost-center
The `benchmark-sample-cost-center` procedure returns optional cost-center data
stored in sample `sample`, or `#f` in v1.
|#
  (define benchmark-sample-cost-center
    (lambda (sample)
      (pcheck ([benchmark-sample? sample])
              ($benchmark-sample-cost-center sample))))

  #|proc:benchmark-sample-cost-center-allocation-count
The `benchmark-sample-cost-center-allocation-count` procedure returns the
optional allocation count stored in `sample` cost-center data, or `#f` when the
sample has no allocation count.
|#
  (define benchmark-sample-cost-center-allocation-count
    (lambda (sample)
      (pcheck ([benchmark-sample? sample])
              (let ([data ($benchmark-sample-cost-center sample)])
                (and data ($alist-ref data 'allocation-count #f))))))

  #|proc:benchmark-sample-cost-center-instruction-count
The `benchmark-sample-cost-center-instruction-count` procedure returns the
optional instruction count stored in `sample` cost-center data, or `#f` when
the sample has no instruction count.
|#
  (define benchmark-sample-cost-center-instruction-count
    (lambda (sample)
      (pcheck ([benchmark-sample? sample])
              (let ([data ($benchmark-sample-cost-center sample)])
                (and data ($alist-ref data 'instruction-count #f))))))

  #|proc:benchmark-sample-cost-center-time-ns
The `benchmark-sample-cost-center-time-ns` procedure returns the optional
cost-center CPU time in nanoseconds stored in `sample`, or `#f` when the sample
has no cost-center time.
|#
  (define benchmark-sample-cost-center-time-ns
    (lambda (sample)
      (pcheck ([benchmark-sample? sample])
              (let ([data ($benchmark-sample-cost-center sample)])
                (and data ($alist-ref data 'time-ns #f))))))

  #|proc:benchmark-sample-counters
The `benchmark-sample-counters` procedure returns the custom counter alist
stored in sample `sample`.
|#
  (define benchmark-sample-counters
    (lambda (sample)
      (pcheck ([benchmark-sample? sample])
              ($benchmark-sample-counters sample))))

  #|proc:make-benchmark-reporter
The `make-benchmark-reporter` procedure creates a reporter descriptor from
callbacks. `start` receives `(results output-port)`, `result` receives
`(result output-port)`, and `finish` receives `(results output-port)`.
Custom reporters may use binary output ports when their callbacks support them.
|#
  (define make-benchmark-reporter
    (lambda (start result finish)
      (pcheck ([procedure? start result finish])
              (unless (andmap (lambda (callback) (logbit? 2 (procedure-arity-mask callback)))
                              (list start result finish))
                (errorf 'make-benchmark-reporter "callbacks must accept two arguments"))
              ($mk-benchmark-reporter start result finish #f))))

  (define $make-textual-benchmark-reporter
    (lambda (start result finish)
      ($mk-benchmark-reporter start result finish #t)))

  (define $check-reporter-output
    (lambda (who reporter out)
      (when (and reporter out ($benchmark-reporter-textual-output? reporter))
        (unless (and (textual-port? out) (not (port-closed? out)))
          (errorf who "built-in reporter requires an open textual output port")))))

  #|proc:benchmark-comparison-name
The `benchmark-comparison-name` procedure returns the benchmark name associated
with comparison `comparison`.
|#
  (define benchmark-comparison-name
    (lambda (comparison)
      (pcheck ([benchmark-comparison? comparison])
              ($benchmark-comparison-name comparison))))

  #|proc:benchmark-comparison-metric
The `benchmark-comparison-metric` procedure returns the summary metric compared
by comparison `comparison`.
|#
  (define benchmark-comparison-metric
    (lambda (comparison)
      (pcheck ([benchmark-comparison? comparison])
              ($benchmark-comparison-metric comparison))))

  #|proc:benchmark-comparison-baseline
The `benchmark-comparison-baseline` procedure returns the baseline result stored
in comparison `comparison`.
|#
  (define benchmark-comparison-baseline
    (lambda (comparison)
      (pcheck ([benchmark-comparison? comparison])
              ($benchmark-comparison-baseline comparison))))

  #|proc:benchmark-comparison-current
The `benchmark-comparison-current` procedure returns the current result stored
in comparison `comparison`.
|#
  (define benchmark-comparison-current
    (lambda (comparison)
      (pcheck ([benchmark-comparison? comparison])
              ($benchmark-comparison-current comparison))))

  #|proc:benchmark-comparison-baseline-value
The `benchmark-comparison-baseline-value` procedure returns the numeric
baseline metric value stored in comparison `comparison`.
|#
  (define benchmark-comparison-baseline-value
    (lambda (comparison)
      (pcheck ([benchmark-comparison? comparison])
              ($benchmark-comparison-baseline-value comparison))))

  #|proc:benchmark-comparison-current-value
The `benchmark-comparison-current-value` procedure returns the numeric current
metric value stored in comparison `comparison`.
|#
  (define benchmark-comparison-current-value
    (lambda (comparison)
      (pcheck ([benchmark-comparison? comparison])
              ($benchmark-comparison-current-value comparison))))

  #|proc:benchmark-comparison-absolute-difference
The `benchmark-comparison-absolute-difference` procedure returns
`current - baseline` for comparison `comparison`.
|#
  (define benchmark-comparison-absolute-difference
    (lambda (comparison)
      (pcheck ([benchmark-comparison? comparison])
              ($benchmark-comparison-absolute-difference comparison))))

  #|proc:benchmark-comparison-percent-difference
The `benchmark-comparison-percent-difference` procedure returns the percent
difference for comparison `comparison`.
|#
  (define benchmark-comparison-percent-difference
    (lambda (comparison)
      (pcheck ([benchmark-comparison? comparison])
              ($benchmark-comparison-percent-difference comparison))))

  #|proc:benchmark-comparison-regression?
The `benchmark-comparison-regression?` procedure returns whether comparison
`comparison` exceeds the configured positive regression thresholds.
|#
  (define benchmark-comparison-regression?
    (lambda (comparison)
      (pcheck ([benchmark-comparison? comparison])
              ($benchmark-comparison-regression? comparison))))

  #|proc:benchmark-comparison-improvement?
The `benchmark-comparison-improvement?` procedure returns whether comparison
`comparison` exceeds the configured negative improvement thresholds.
|#
  (define benchmark-comparison-improvement?
    (lambda (comparison)
      (pcheck ([benchmark-comparison? comparison])
              ($benchmark-comparison-improvement? comparison))))

  #|proc:benchmark-text-reporter
  The `benchmark-text-reporter` procedure returns the default plain textual
  reporter. It identifies runtime and template arguments in each row. Its error
  column describes captured failures beside their measurements.
  |#
  (define benchmark-text-reporter
    (lambda ()
      ($make-textual-benchmark-reporter
       (lambda (results out)
         (for-each (lambda (line)
                     (display line out)
                     (newline out))
                   ($benchmark-text-lines results)))
       $void-procedure
       (lambda (results out)
         (flush-output-port out)))))

  #|proc:benchmark-datum-reporter
The `benchmark-datum-reporter` procedure returns a reporter that writes one
complete Scheme datum per result to the output port, independent of printer limits.
Shared and cyclic data use readable graph notation without printer warnings.
Generated symbols retain their identity regardless of the ambient gensym printer mode.
|#
  (define benchmark-datum-reporter
    (lambda ()
      ($make-textual-benchmark-reporter
       $void-procedure
       (lambda (result out)
         ($write-readable-datum
                  `((name . ,(benchmark-result-name result))
                    (args . ,(benchmark-result-args result))
                    (template-args . ,(benchmark-result-template-args result))
                    (summary . ,(benchmark-result-summary result))
                    (counters . ,(benchmark-result-counters result))
                    (error . ,(and (benchmark-result-error result) #t)))
                  out)
         (newline out))
       (lambda (results out)
         (flush-output-port out)))))

  #|proc:benchmark-csv-reporter
The `benchmark-csv-reporter` procedure returns a reporter that writes benchmark
results as comma-separated values. Each row contains the result name, argument
data, template data, summary means, counters, and whether the result captured an
error. Missing metrics or means in legacy summaries are reported as zero.
|#
  (define benchmark-csv-reporter
    (lambda ()
      ($make-textual-benchmark-reporter
       (lambda (results out)
         (fprintf out "name,args,template_args,cpu_ns_mean,real_ns_mean,bytes_mean,counters,error~%"))
       (lambda (result out)
         (let ([cpu ($metric-mean result 'cpu-ns)]
               [real ($metric-mean result 'real-ns)]
               [bytes ($metric-mean result 'bytes)])
           (fprintf out "~a,~a,~a,~a,~a,~a,~a,~a~%"
                    ($csv-field (benchmark-result-name result))
                    ($csv-field (benchmark-result-args result))
                    ($csv-field (benchmark-result-template-args result))
                    ($csv-field cpu)
                    ($csv-field real)
                    ($csv-field bytes)
                    ($csv-field (benchmark-result-counters result))
                    ($csv-field (and (benchmark-result-error result) #t)))))
       (lambda (results out)
         (flush-output-port out)))))

  #|proc:benchmark-json-reporter
The `benchmark-json-reporter` procedure returns a reporter that writes benchmark
results as one JSON object containing a `results` array. The JSON writer is
self-contained and supports the Scheme data emitted by benchmark results.
Cyclic substructures use readable Scheme strings when recursion would repeat a value.
Generated symbol keys use their readable unique names. Associations with colliding
JSON names use a readable Scheme string preserving all keys and values. Exact fractions outside the
nonzero finite flonum range use exact Scheme strings; other fractions use decimal numbers.
|#
  (define benchmark-json-reporter
    (lambda ()
      (let ([first? #t])
        ($make-textual-benchmark-reporter
         (lambda (results out)
           (set! first? #t)
           (display "{\"results\":[" out))
         (lambda (result out)
           (unless first?
             (display "," out))
           (set! first? #f)
           ($write-json ($result->json result) out '()))
         (lambda (results out)
           (display "]}" out)
           (newline out)
           (flush-output-port out))))))

  #|proc:benchmark-rich-reporter
The `benchmark-rich-reporter` procedure returns a reporter that writes benchmark
results as a rich table. `options` is an optional alist that customizes table
style. Supported keys are `title`, `caption`, `box`, `show-header?`,
`show-lines?`, `padding`, and `columns`. `columns` is an alist whose keys select
result fields and whose values are column labels; supported field keys are
`name`, `args`, `template-args`, `iterations`, `cpu-ns`, `real-ns`, `bytes`,
`counters`, and `error`. Invalid or duplicate options are rejected at construction.
|#
  (define benchmark-rich-reporter
    (case-lambda
      [()
       (benchmark-rich-reporter '())]
      [(options)
       (pcheck ([list? options])
               ($check-option-keys 'benchmark-rich-reporter options
                 '(title caption box show-header? show-lines? padding columns) '())
               (let ([columns ($option-ref options 'columns $benchmark-rich-default-columns)])
                 (unless (and (list? columns)
                              (andmap (lambda (column)
                                        (and (pair? column)
                                             (assq (car column) $benchmark-rich-default-columns)
                                             (string? (cdr column))))
                                      columns))
                   (errorf 'benchmark-rich-reporter "invalid columns: ~s" columns)))
               ;; Reuse the rich table's own field validators before any benchmark runs.
               ($benchmark-rich-table '() options)
               ($make-textual-benchmark-reporter
                $void-procedure
                $void-procedure
                (lambda (results out)
                  (rich-print out ($benchmark-rich-table results options))
                  (newline out)
                  (flush-output-port out))))]))

  (define $benchmark-config-keys
    '(warmup samples min-time max-iterations output filter reporter cost-center? stop-on-error?))

  (define $make-checked-config
    (lambda (warmup samples min-time max-iterations output selector reporter
             cost-center? stop-on-error? explicit-options)
      (unless (and (natural? warmup) (natural? samples)
                   (natural? max-iterations) (> max-iterations 0)
                   (real? min-time) (finite? min-time) (>= min-time 0)
                   (or (not output) (output-port? output))
                   (or (not selector) (symbol? selector) (string? selector) (procedure? selector))
                   (or (not reporter) (benchmark-reporter? reporter))
                   (boolean? cost-center?) (boolean? stop-on-error?))
        (errorf 'make-benchmark-config "invalid benchmark configuration"))
      ($mk-benchmark-config warmup samples min-time max-iterations output selector reporter
                            cost-center? stop-on-error? explicit-options)))

  #|proc:make-benchmark-config
  The `make-benchmark-config` procedure returns a runner configuration whose fields
  are explicit overrides of descriptor defaults. `warmup` and `samples` are natural
  sample counts, `min-time` is a finite nonnegative target in seconds, and
  `max-iterations` is a positive iteration cap. `output` is an output port or `#f`;
  `filter` is `#f`, a name symbol, a name prefix string, or a predicate receiving a
  benchmark descriptor and returning whether to select it. `reporter` is a reporter
  or `#f`. Boolean `cost-center?` requests optional instrumentation, and Boolean
  `stop-on-error?` stops after the first captured error when true. Cost-center
  instrumentation is reserved for future support; measured cost-center data is `#f`.
  |#
  (define make-benchmark-config
    (lambda (warmup samples min-time max-iterations output filter reporter cost-center? stop-on-error?)
      (pcheck ([natural? warmup samples max-iterations] [real? min-time]
               [boolean? cost-center? stop-on-error?])
              ($make-checked-config warmup samples min-time max-iterations output filter reporter
                                    cost-center? stop-on-error? $benchmark-config-keys))))

  #|proc:benchmark-config-warmup
The `benchmark-config-warmup` procedure returns the warmup sample count stored
in runner `config`.
|#
  (define benchmark-config-warmup
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-warmup config))))

  #|proc:benchmark-config-samples
The `benchmark-config-samples` procedure returns the measured sample count
stored in runner `config`.
|#
  (define benchmark-config-samples
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-samples config))))

  #|proc:benchmark-config-min-time
The `benchmark-config-min-time` procedure returns the target minimum sample time
stored in runner `config`.
|#
  (define benchmark-config-min-time
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-min-time config))))

  #|proc:benchmark-config-max-iterations
The `benchmark-config-max-iterations` procedure returns the iteration cap stored
in runner `config`.
|#
  (define benchmark-config-max-iterations
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-max-iterations config))))

  #|proc:benchmark-config-output
The `benchmark-config-output` procedure returns the output target stored in
runner `config`.
|#
  (define benchmark-config-output
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-output config))))

  #|proc:benchmark-config-filter
The `benchmark-config-filter` procedure returns the benchmark selector stored
in runner `config`.
|#
  (define benchmark-config-filter
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-filter config))))

  #|proc:benchmark-config-reporter
The `benchmark-config-reporter` procedure returns the reporter stored in runner
`config`, or `#f` when reporting is disabled.
|#
  (define benchmark-config-reporter
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-reporter config))))

  #|proc:benchmark-config-cost-center?
The `benchmark-config-cost-center?` procedure returns whether optional
cost-center measurement is enabled in runner `config`.
|#
  (define benchmark-config-cost-center?
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-cost-center? config))))

  #|proc:default-benchmark-config
  The `default-benchmark-config` procedure returns a fresh default runner
  configuration using the current output port. Its fields remain defaults rather
  than explicit overrides, so descriptor options can replace them.
  |#
  (define default-benchmark-config
    (lambda ()
      (unless $default-reporter
        (set! $default-reporter (benchmark-text-reporter)))
      ($make-checked-config 1 10 0.01 1000000 (current-output-port) #f
                            $default-reporter #f #f '())))

  (define $benchmark-config-options
    (lambda (config)
      (list (cons 'warmup ($benchmark-config-warmup config))
            (cons 'samples ($benchmark-config-samples config))
            (cons 'min-time ($benchmark-config-min-time config))
            (cons 'max-iterations ($benchmark-config-max-iterations config))
            (cons 'output ($benchmark-config-output config))
            (cons 'filter ($benchmark-config-filter config))
            (cons 'reporter ($benchmark-config-reporter config))
            (cons 'cost-center? ($benchmark-config-cost-center? config))
            (cons 'stop-on-error? ($benchmark-config-stop-on-error? config)))))

  #|proc:benchmark-config-with
  The `benchmark-config-with` procedure returns a validated copy of `config` with
  fields changed by option alist `options`. Each changed field becomes an explicit
  run override. Explicit run settings take precedence over descriptor options;
  descriptor options take precedence over untouched default settings.
  Each key may occur once within an update; successive updates may override earlier values.
  |#
  (define benchmark-config-with
    (lambda (config options)
      (pcheck ([benchmark-config? config] [list? options])
              ($check-option-keys 'benchmark-config-with options $benchmark-config-keys '())
              (if (null? options)
                  config
                  (let loop ([options options]
                             [fields ($benchmark-config-options config)]
                             [explicit ($benchmark-config-explicit-options config)])
                    (if (null? options)
                        (apply $make-checked-config
                               (append (map (lambda (key) ($alist-ref fields key))
                                            $benchmark-config-keys)
                                       (list explicit)))
                        (let ([option (car options)])
                          (unless (and (pair? option) (memq (car option) $benchmark-config-keys))
                            (errorf 'benchmark-config-with "unknown config option: ~s" option))
                          (loop (cdr options)
                                ($replace-option fields (car option) (cdr option))
                                (if (memq (car option) explicit)
                                    explicit
                                    (cons (car option) explicit))))))))))

  (define $effective-benchmark-config
    (lambda (benchmark config)
      (benchmark-config-with config
        (filter (lambda (option)
                  (and (memq (car option) $benchmark-config-keys)
                       (not (memq (car option) ($benchmark-config-explicit-options config)))))
                ($benchmark-options benchmark)))))


;;;;===----------------------------------------------------------------------===
;;;; argument normalization and syntax
;;;;===----------------------------------------------------------------------===

  (define $range-values
    (lambda (start end step geometric?)
      (unless (and (real? start) (finite? start)
                   (real? end) (finite? end)
                   (real? step) (finite? step)
                   (if geometric?
                       (and (> start 0) (> step 1))
                       (> step 0)))
        (errorf '$range-values "range must have finite bounds and an advancing positive step"))
      (let loop ([value start] [out '()])
        (if (> value end)
            (reverse out)
            (let ([next (if geometric? (* value step) (+ value step))])
              (unless (> next value)
                (errorf '$range-values "range step does not advance: ~s" value))
              (loop next (cons value out)))))))

  (define $normalize-arguments
    (lambda (options)
      ;; Keep validated axes factorized; combinations exist only during traversal.
      (let ([seen (make-eq-hashtable)])
        (map
          (lambda (option)
            (let ([axis
                   (case (car option)
                     [(args) (cdr option)]
                     [(arg-range dense-arg-range)
                      (let ([spec (cdr option)])
                        (cons (list (car spec))
                              (map list ($range-values (cadr spec) (caddr spec) (cadddr spec)
                                          (eq? (car option) 'arg-range)))))])])
              (unless (and (list? axis) (pair? axis)
                           (list? (car axis)) (pair? (car axis)))
                (errorf 'make-benchmark "invalid argument table: ~s" axis))
              (for-each
                (lambda (name)
                  (unless (symbol? name)
                    (errorf 'make-benchmark "argument names must be symbols: ~s" name))
                  (when (hashtable-contains? seen name)
                    (errorf 'make-benchmark "duplicate argument name across options: ~s" name))
                  (hashtable-set! seen name #t))
                (car axis))
              (let ([width (length (car axis))])
                (for-each
                  (lambda (row)
                    (unless (and (list? row) (= (length row) width))
                      (errorf 'make-benchmark "argument row length must match names: ~s" row)))
                  (cdr axis)))
              ;; Preserve the constructor's row snapshot without copying products.
              (cons (list-copy (car axis)) (map list-copy (cdr axis)))))
          (filter (lambda (option) (memq (car option) '(args arg-range dense-arg-range)))
                  options)))))

  (define $walk-argument-axes
    (lambda (axes visit)
      (or (exists (lambda (axis) (null? (cdr axis))) axes)
          (let loop ([axes axes] [reversed '()])
            (if (null? axes)
                (visit (reverse reversed))
                (for-all
                  (lambda (row)
                    (loop (cdr axes)
                          (fold-left (lambda (bindings name value)
                                       (cons (cons name value) bindings))
                                     reversed (caar axes) row)))
                  (cdar axes)))))))

  #|macro:benchmark-options
  The `benchmark-options` macro converts benchmark option syntax into an option
  alist. `:args` receives a grouped table `([name ...] [value ...] ...)`; each
  `name` is an argument name and each value row supplies one concrete benchmark
  case. Repeated `:args` tables are expanded as a Cartesian product, and argument
  names must not be duplicated in one table or across argument options.
  Singleton clauses must not repeat, and range clauses require exactly four elements.
  `:arg-range [name start end factor]` requires finite bounds, a positive start,
  and a factor greater than one. `:dense-arg-range [name start end step]` requires
  finite bounds and a positive step; its start may be zero. Both ranges include
  their upper bound when reached and reject steps that fail to advance.
  |#
  (define-syntax benchmark-options
    (lambda (stx)
      (define keyword?
        (lambda (x)
          (and (identifier? x)
               (let ([datum (syntax->datum x)])
                 (and (symbol? datum)
                      (let ([text (symbol->string datum)])
                        (and (> (string-length text) 0)
                             (char=? (string-ref text 0) #\:))))))))
      (define keyword->option-name
        (lambda (datum)
          (string->symbol (substring (symbol->string datum) 1
                                     (string-length (symbol->string datum))))))
      (define duplicate-symbol
        (lambda (names)
          (let loop ([names names] [seen '()])
            (cond
             [(null? names) #f]
             [(memq (car names) seen) (car names)]
             [else (loop (cdr names) (cons (car names) seen))]))))
      (define shared-symbol
        (lambda (left right)
          (let loop ([left left])
            (cond
             [(null? left) #f]
             [(memq (car left) right) (car left)]
             [else (loop (cdr left))]))))
      (define parse-old-row
        (lambda (row)
          (syntax-case row ()
            [(name start end step)
             (identifier? #'name)
             #'(name start end step)]
            [_ (syntax-error row "invalid benchmark argument row")])))
      (define parse-args-spec
        (lambda (key rest seen-names)
          (unless (= (length rest) 1)
            (syntax-error key ":args expects one grouped argument table"))
          (let ([datum (syntax->datum (car rest))])
            (unless (and (list? datum) (pair? datum) (list? (car datum)))
              (syntax-error (car rest) ":args expects ([name ...] [value ...] ...)"))
            (let ([names (car datum)]
                  [value-rows (cdr datum)])
              (when (null? names)
                (syntax-error (car rest) ":args requires at least one argument name"))
              (unless (andmap symbol? names)
                (syntax-error (car rest) ":args names must be identifiers"))
              (let ([duplicate (duplicate-symbol names)])
                (when duplicate
                  (syntax-error (car rest) (format "duplicate :args name: ~a" duplicate))))
              (let ([shared (shared-symbol names seen-names)])
                (when shared
                  (syntax-error (car rest) (format "duplicate argument name across options: ~a" shared))))
              (for-each (lambda (row)
                          (unless (and (list? row) (= (length row) (length names)))
                            (syntax-error (car rest) ":args value row length must match name list")))
                        value-rows)
              (values names
                      #`(cons 'args '#,(datum->syntax key datum)))))))
      (define parse-option
        (lambda (key rest seen-names)
          (let ([datum (syntax->datum key)])
            (case datum
              [(:args)
               (parse-args-spec key rest seen-names)]
              [(:arg-product)
               (syntax-error key ":arg-product has been removed; use repeated :args forms")]
              [(:arg-range)
               (unless (= (length rest) 1)
                 (syntax-error key ":arg-range expects one row"))
               (let ([row (syntax->datum (parse-old-row (car rest)))])
                 (when (memq (car row) seen-names)
                   (syntax-error (car rest) (format "duplicate argument name across options: ~a" (car row))))
                 (values (list (car row))
                         #`(cons 'arg-range '#,(datum->syntax key row))))]
              [(:dense-arg-range)
               (unless (= (length rest) 1)
                 (syntax-error key ":dense-arg-range expects one row"))
               (let ([row (syntax->datum (parse-old-row (car rest)))])
                 (when (memq (car row) seen-names)
                   (syntax-error (car rest) (format "duplicate argument name across options: ~a" (car row))))
                 (values (list (car row))
                         #`(cons 'dense-arg-range '#,(datum->syntax key row))))]
              [(:setup)
               (unless (= (length rest) 1)
                 (syntax-error key ":setup expects one expression"))
               (values '() #`(cons 'setup #,(car rest)))]
              [(:teardown)
               (unless (= (length rest) 1)
                 (syntax-error key ":teardown expects one expression"))
               (values '() #`(cons 'teardown #,(car rest)))]
              [(:suite-setup)
               (unless (= (length rest) 1)
                 (syntax-error key ":suite-setup expects one expression"))
               (values '() #`(cons 'suite-setup #,(car rest)))]
              [(:suite-teardown)
               (unless (= (length rest) 1)
                 (syntax-error key ":suite-teardown expects one expression"))
               (values '() #`(cons 'suite-teardown #,(car rest)))]
              [(:unit :complexity)
               (unless (= (length rest) 1)
                 (syntax-error key "benchmark option expects one value"))
               (values '()
                       (if (identifier? (car rest))
                           #`(cons '#,(datum->syntax key (keyword->option-name datum))
                                   '#,(car rest))
                           #`(cons '#,(datum->syntax key (keyword->option-name datum))
                                   #,(car rest))))]
              [(:throughput)
               (values '()
                       #`(cons 'throughput '#,(datum->syntax key (map syntax->datum rest))))]
              [(:warmup :samples :min-time :max-iterations :reporter :output :filter :cost-center? :stop-on-error?)
               (unless (= (length rest) 1)
                 (syntax-error key "benchmark option expects one expression"))
               (values '()
                       #`(cons '#,(datum->syntax key (keyword->option-name datum))
                               #,(car rest)))]
              [else (syntax-error key "unknown benchmark option")]))))
      (define parse-options
        (lambda (clauses)
          (let loop ([clauses clauses] [seen '()])
            (unless (null? clauses)
              (let ([key (syntax->datum (caar clauses))])
                (when (and (memq key seen)
                           (not (memq key '(:args :arg-range :dense-arg-range))))
                  (syntax-error (caar clauses) "duplicate benchmark option"))
                (loop (cdr clauses) (cons key seen)))))
          (let loop ([clauses clauses] [seen-names '()] [out '()])
            (if (null? clauses)
                (reverse out)
                (let-values ([(new-names option)
                              (parse-option (caar clauses) (cdar clauses) seen-names)])
                  (loop (cdr clauses)
                        (append seen-names new-names)
                        (cons option out)))))))
      (define split-options
        (lambda (items)
          (let loop ([items items] [key #f] [current '()] [out '()])
            (cond
             [(null? items)
              (if key
                  (reverse (cons (cons key (reverse current)) out))
                  (reverse out))]
             [(keyword? (car items))
              (loop (cdr items)
                    (car items)
                    '()
                    (if key
                        (cons (cons key (reverse current)) out)
                        out))]
             [key
              (loop (cdr items) key (cons (car items) current) out)]
             [else (syntax-error (car items) "benchmark option list must begin with a keyword")]))))
      (syntax-case stx ()
        [(_ (item ...))
         #`(list #,@(parse-options (split-options #'(item ...))))]
        [_ (syntax-error stx "invalid benchmark-options form")])))

  #|macro:define-benchmark
The `define-benchmark` macro defines and registers a benchmark descriptor named
by `name`. `options` configures arguments, lifecycle hooks, and runner defaults;
`body` is a procedure that receives state, argument values in option order, and
optional setup value. Runtime arguments are configured with grouped `:args`
tables such as `(:args ([size start] [1024 0] [2048 128]))`; repeated `:args`
tables are expanded as a Cartesian product. If body and teardown both raise,
the body condition takes precedence.
|#
  (define-syntax define-benchmark
    (syntax-rules ()
      [(_ name options body)
       (define name
         (let ([benchmark (make-benchmark 'name body (benchmark-options options))])
           (benchmark-register! (current-benchmark-registry) benchmark)
           benchmark))]))

  #|macro:define-benchmark-fixture
The `define-benchmark-fixture` macro defines a reusable fixture descriptor named
by `name`. Its setup hook receives state and its teardown hook receives state
and the setup value. Only `:setup` and `:teardown` options are accepted, and each
supplied hook must accept its documented arguments.
Setup must clean up partial resources if it raises; teardown runs only after successful setup.
|#
  (define-syntax define-benchmark-fixture
    (syntax-rules ()
      [(_ name options)
       (define name
         (let ([opts (benchmark-options options)])
           ($check-benchmark-options 'define-benchmark-fixture opts '(setup teardown))
           ($mk-benchmark-fixture 'name
                                  ($option-ref opts 'setup $false-procedure)
                                  ($option-ref opts 'teardown $void-procedure))))]))

  #|macro:define-fixture-benchmark
The `define-fixture-benchmark` macro defines and registers benchmark `name`
using fixture descriptor `fixture`. `options` has the same argument syntax as
`define-benchmark`; `body` receives state, argument values in option order, and
the fixture setup value.
Descriptor `:setup` and `:teardown` hooks cannot be combined with a fixture.
|#
  (define-syntax define-fixture-benchmark
    (syntax-rules ()
      [(_ name fixture-descriptor options body)
       (define name
         (let* ([opts (benchmark-options options)]
                [benchmark (make-benchmark 'name body
                                           (cons (cons 'fixture fixture-descriptor) opts))])
           (benchmark-register! (current-benchmark-registry) benchmark)
           benchmark))]))

  #|macro:define-benchmark-suite
  The `define-benchmark-suite` macro defines a suite descriptor named by `name`
  containing the listed benchmark descriptors. The optional `:suite-setup` procedure
  receives the suite state and returns a setup value. The `:suite-teardown` procedure
  receives that state and setup value, or `#f` if setup raised; it can inspect user
  data in the state to release partially acquired resources. Only these two hook
  options are accepted; measurement defaults belong to child benchmark descriptors.
  Setup must accept one argument and teardown must accept two arguments.
  Every member expression must evaluate to a benchmark descriptor.
  |#
  (define-syntax define-benchmark-suite
    (syntax-rules ()
      [(_ name options benchmark ...)
       (define name
         (let ([opts (benchmark-options options)]
               [children (list benchmark ...)])
           ($check-benchmark-options 'define-benchmark-suite opts '(suite-setup suite-teardown))
           (unless (andmap benchmark? children)
             (errorf 'define-benchmark-suite "suite members must be benchmark descriptors"))
           ($mk-benchmark-suite 'name
                                children
                                ($option-ref opts 'suite-setup $false-procedure)
                                ($option-ref opts 'suite-teardown $void-procedure))))]))

  #|macro:define-benchmark-template
The `define-benchmark-template` macro defines a template procedure named by
`name`. The generated procedure accepts template parameters `param ...` and
returns a benchmark descriptor. `options` has the same grouped `:args` argument
syntax as `define-benchmark`.
|#
  (define-syntax define-benchmark-template
    (syntax-rules ()
      [(_ name (param ...) options body)
       (define name
         (lambda (param ...)
           (make-benchmark 'name body
                           (append (benchmark-options options)
                                   (list (cons 'template-args
                                               (list (cons 'param param) ...)))))))]))

  #|macro:instantiate-benchmark-template
The `instantiate-benchmark-template` macro registers concrete benchmark
descriptors from template `template`. Each row supplies values positionally for
the template parameters, and the first row value becomes the concrete name
suffix.
|#
  (define-syntax instantiate-benchmark-template
    (syntax-rules ()
      [(_ template ([label value ...] ...))
       (begin
         (define template-instance-marker (void))
         (let ([benchmark (template 'label value ...)])
           (let ([renamed (make-benchmark ($symbol-append 'template 'label)
                                          ($benchmark-body benchmark)
                                          ($replace-option ($benchmark-options benchmark)
                                                           'template-args
                                                           ($benchmark-template-args benchmark)))])
             (benchmark-register! (current-benchmark-registry) renamed)))
         ...)]))


;;;;===----------------------------------------------------------------------===
;;;; expansion and runner
;;;;===----------------------------------------------------------------------===

  (define $current-suite-context (make-parameter #f))

  (define $current-benchmark-report-collector (make-parameter #f))

  (define $dispatch-benchmark-report
    (lambda (results reporter out)
      (let ([collector ($current-benchmark-report-collector)])
        (if collector
            (collector results reporter out)
            (benchmark-report results reporter out)))))

  (define-record-type ($report-groups $make-report-groups $report-groups?)
    (fields index (mutable ordered $report-groups-ordered $report-groups-ordered-set!)))

  (define $make-report-collector
    (lambda ()
      ($make-report-groups (make-eq-hashtable) '())))

  (define $collect-report!
    (lambda (groups results reporter out)
      (when (and reporter out)
        (let* ([index ($report-groups-index groups)]
               [ports (or (hashtable-ref index reporter #f)
                          (let ([ports (make-eq-hashtable)])
                            (hashtable-set! index reporter ports)
                            ports))]
               [group (or (hashtable-ref ports out #f)
                          (let ([group (cons (cons reporter out) '())])
                            (hashtable-set! ports out group)
                            ($report-groups-ordered-set! groups
                              (cons group ($report-groups-ordered groups)))
                            group))])
          (for-each (lambda (result) (set-cdr! group (cons result (cdr group)))) results)))))

  (define $dispatch-reports
    (lambda (groups)
      (for-each
        (lambda (group)
          ($dispatch-benchmark-report (reverse (cdr group)) (caar group) (cdar group)))
        (reverse ($report-groups-ordered groups)))))

  (define $expand-benchmark-case
    (lambda (benchmark args)
      (cons ($make-benchmark-state ($benchmark-name benchmark)
                                  (or ($current-suite-context) ($benchmark-suite-ref benchmark))
                                  args ($benchmark-template-args benchmark) 0 1 0 '() '() #f)
            benchmark)))

  #|proc:benchmark-expand
The `benchmark-expand` procedure expands `benchmark` into concrete run pairs
for `config`. Each pair contains a benchmark state in the car and the descriptor
to run in the cdr.
|#
  (define benchmark-expand
    (lambda (benchmark config)
      (pcheck ([benchmark? benchmark] [benchmark-config? config])
              (let ([expanded '()])
                ($walk-argument-axes ($benchmark-axes benchmark)
                  (lambda (args)
                    (set! expanded (cons ($expand-benchmark-case benchmark args) expanded))
                    #t))
                (reverse expanded)))))

  #|proc:benchmark-select
The `benchmark-select` procedure selects benchmarks from `registry` according
to `selector`, which may be `#f`, a symbol, a string, or a predicate procedure.
|#
  (define benchmark-select
    (lambda (registry selector)
      (pcheck ([benchmark-registry? registry])
              (let ([benchmarks ($benchmark-registry-benchmarks registry)])
                (cond
                 [(not selector) benchmarks]
                 [(symbol? selector)
                  (filter (lambda (benchmark)
                            (eq? selector ($benchmark-name benchmark)))
                          benchmarks)]
                 [(string? selector)
                  (filter (lambda (benchmark)
                            (let ([name (symbol->string ($benchmark-name benchmark))])
                              (and (<= (string-length selector) (string-length name))
                                   (string=? selector
                                             (substring name 0 (string-length selector))))))
                          benchmarks)]
                 [(procedure? selector)
                  (filter selector benchmarks)]
                 [else (errorf 'benchmark-select "invalid selector: ~s" selector)])))))

  (define $call-with-values-list
    (lambda (producer consumer)
      (call-with-values producer
        (lambda values
          (consumer values)))))

  (define $run-body-iterations
    (lambda (body state arguments iterations)
      (let loop ([i 0])
        (when (< i iterations)
          ($benchmark-state-iteration-set! state i)
          (apply body state arguments)
          (loop (+ i 1))))))

  (define $setup-for-sample
    (lambda (benchmark state)
      (let ([setup ($benchmark-setup benchmark)])
        (values (if setup (setup state) #f)
                ($benchmark-teardown benchmark)
                (and setup #t)))))

  (define $measure-sample
    (lambda (benchmark state values iterations)
      (let ([sample-state ($make-benchmark-state ($benchmark-state-name state)
                                                ($benchmark-state-suite state)
                                                ($benchmark-state-args state)
                                                ($benchmark-state-template-args state)
                                                0
                                                iterations
                                                ($benchmark-state-sample state)
                                                (benchmark-state-user-data state)
                                                '()
                                                #f)])
        ($call-with-values-list
         (lambda ()
           ($setup-for-sample benchmark sample-state))
         (lambda (setup-results)
           (let ([setup-value (car setup-results)]
                 [teardown (cadr setup-results)]
                 [arguments (if (caddr setup-results)
                                (append values (list (car setup-results)))
                                values)]
                 [cleanup? #t]
                 [failed? #f])
             (dynamic-wind
               (lambda () (void))
               (lambda ()
                 (guard (exn [else (set! failed? #t) (raise exn)])
                   ;; Setup is outside measurement, including any completed pause intervals.
                   ($benchmark-state-paused-sstats-set! sample-state ($zero-sstats))
                   (let* ([b1 (statistics)]
                          [b2 (statistics)])
                     (when ($benchmark-state-timing-paused? sample-state)
                       ($benchmark-state-pause-start-set! sample-state b2))
                     ($run-body-iterations ($benchmark-body benchmark) sample-state
                                           arguments iterations)
                     (when ($benchmark-state-timing-paused? sample-state)
                       (benchmark-resume-timing sample-state))
                     (let* ([after (statistics)]
                            [elapsed (sstats-difference after b2)]
                            [overhead (sstats-difference b2 b1)]
                            [active (sstats-difference
                                     elapsed ($benchmark-state-paused-sstats sample-state))]
                            [adjusted ($sanitize-sstats (sstats-difference active overhead))])
                       ($mk-benchmark-sample iterations adjusted #f
                         ;; Keep completed samples independent of cleanup and retained states.
                         (map (lambda (entry) (cons (car entry) (cdr entry)))
                              ($benchmark-state-counters sample-state)))))))
               (lambda ()
                 (when cleanup?
                   (set! cleanup? #f)
                   (when ($benchmark-state-timing-paused? sample-state)
                     (benchmark-resume-timing sample-state))
                   (when teardown
                     (guard (exn [else (unless failed? (raise exn))])
                       (teardown sample-state setup-value))))))))))))

  (define $warmup
    (lambda (benchmark state values config)
      (let loop ([i 0])
        (when (< i (benchmark-config-warmup config))
          ($measure-sample benchmark state values 1)
          (loop (+ i 1))))))

  (define $choose-sample
    (lambda (benchmark state values config sample-index)
      (let loop ([iterations 1])
        (let* ([sample-state ($make-benchmark-state ($benchmark-state-name state)
                                                    ($benchmark-state-suite state)
                                                    ($benchmark-state-args state)
                                                    ($benchmark-state-template-args state)
                                                    0
                                                    iterations
                                                    sample-index
                                                    (benchmark-state-user-data state)
                                                    '()
                                                    #f)]
               [sample ($measure-sample benchmark sample-state values iterations)]
               [target ($seconds->ns (benchmark-config-min-time config))])
          (if (or (zero? target)
                  (>= ($sstats-real-ns (benchmark-sample-sstats sample)) target)
                  (>= iterations (benchmark-config-max-iterations config)))
              sample
              (loop (min (benchmark-config-max-iterations config)
                         (* iterations 2))))))))

  (define $run-expanded
    (lambda (expanded config)
      (let* ([state (car expanded)]
             [benchmark (cdr expanded)]
             ;; Expansion preserves positional argument order in the state alist.
             [values (map cdr ($benchmark-state-args state))])
        (guard (exn
                [else
                 ($mk-benchmark-result ($benchmark-state-name state)
                                       ($benchmark-state-args state)
                                       ($benchmark-state-template-args state)
                                       '()
                                       (benchmark-summarize '())
                                       '()
                                       ($captured-benchmark-error exn))])
          ($warmup benchmark state values config)
          (let loop ([i 0] [samples '()])
            (if (< i (benchmark-config-samples config))
                (loop (+ i 1)
                      (cons ($choose-sample benchmark state values config i) samples))
                (let* ([samples (reverse samples)]
                       [summary (benchmark-summarize samples)]
                       [counters ($merge-counters samples)])
                  ($mk-benchmark-result ($benchmark-state-name state)
                                        ($benchmark-state-args state)
                                        ($benchmark-state-template-args state)
                                        samples
                                        summary
                                        counters
                                        #f))))))))

  #|proc:benchmark-run-one
  The `benchmark-run-one` procedure runs one concrete expanded benchmark pair
  `expanded` with runner `config` and returns one benchmark result. Explicit config
  fields override descriptor options, which override default settings. Reporting fields
  are unused: this procedure neither invokes a reporter nor validates its output port.
  |#
  (define benchmark-run-one
    (lambda (expanded config)
      (pcheck ([pair? expanded] [benchmark-config? config])
              ($run-expanded expanded ($effective-benchmark-config (cdr expanded) config)))))

  #|proc:benchmark-run-suite
  The `benchmark-run-suite` procedure runs suite descriptor `suite` with runner
  `config` and returns its benchmark results. Failed setup skips children and returns
  one error result named after the suite. Teardown always runs, receiving `#f` when
  setup failed; a setup error takes precedence over a later teardown error. Failed
  teardown after successful setup appends a suite error after completed child results.
  Reports are emitted after teardown, grouping children by their effective reporter
  and output. Suite errors join the configured reporter's group when enabled.
  |#
  (define benchmark-run-suite
    (lambda (suite config)
      (pcheck ([benchmark-suite? suite] [benchmark-config? config])
              ($check-reporter-output 'benchmark-run-suite
                (benchmark-config-reporter config) (benchmark-config-output config))
              (let ([suite-state (benchmark-state ($benchmark-suite-name suite)
                                                  ($benchmark-suite-name suite)
                                                  '()
                                                  '()
                                                  0
                                                  1
                                                  0)]
                    [setup-value #f]
                    [failure #f]
                    [results '()]
                    [report-groups ($make-report-collector)])
                (define collect-report!
                  (lambda (results reporter out)
                    ($collect-report! report-groups results reporter out)))
                (dynamic-wind
                  (lambda () (void))
                  (lambda ()
                    (guard (exn [else (set! failure ($captured-benchmark-error exn))])
                      (set! setup-value (($benchmark-suite-setup suite) suite-state))
                      (set! results
                        (parameterize ([$current-suite-context ($benchmark-suite-name suite)]
                                       [$current-benchmark-report-collector collect-report!])
                          (benchmark-run ($benchmark-suite-benchmarks suite) config)))))
                  (lambda ()
                    (guard (exn [else (unless failure (set! failure ($captured-benchmark-error exn)))])
                      (($benchmark-suite-teardown suite) suite-state setup-value))))
                (when failure
                  (let ([result ($mk-benchmark-result ($benchmark-suite-name suite)
                                                      '() '() '()
                                                      (benchmark-summarize '())
                                                      '() failure)]
                        [reporter (benchmark-config-reporter config)]
                        [out (benchmark-config-output config)])
                    (when (and reporter out)
                      (collect-report! (list result) reporter out))
                    (set! results (append results (list result)))))
                ($dispatch-reports report-groups)
                results))))

  (define $benchmark-matches-selector?
    (lambda (benchmark selector)
      (cond
       [(not selector) #t]
       [(symbol? selector) (eq? selector ($benchmark-name benchmark))]
       [(string? selector)
        (let ([name (symbol->string ($benchmark-name benchmark))])
          (and (<= (string-length selector) (string-length name))
               (string=? selector (substring name 0 (string-length selector)))))]
       [else (selector benchmark)])))

  (define $run-benchmarks
    (lambda (benchmarks config)
      (let ([report-groups ($make-report-collector)] [selected? #f])
        (define record-report!
          (lambda (results effective)
            ($collect-report! report-groups results
              (benchmark-config-reporter effective) (benchmark-config-output effective))))
        (define finish
          (lambda (results)
            ;; With no selected descriptors, the run configuration owns the empty report.
            (unless selected? (record-report! '() config))
            ($dispatch-reports report-groups)
            (reverse results)))
        (let loop-benchmarks ([benchmarks benchmarks] [results '()])
          (if (null? benchmarks)
              (finish results)
              (let* ([benchmark (car benchmarks)]
                     [effective ($effective-benchmark-config benchmark config)])
                (if (not ($benchmark-matches-selector?
                          benchmark (benchmark-config-filter effective)))
                    (loop-benchmarks (cdr benchmarks) results)
                    (begin
                      ($check-reporter-output 'benchmark-run
                        (benchmark-config-reporter effective) (benchmark-config-output effective))
                      (set! selected? #t)
                      (record-report! '() effective)
                      (let* ([next-results results]
                             [completed?
                              ($walk-argument-axes ($benchmark-axes benchmark)
                                (lambda (args)
                                  (let ([result ($run-expanded
                                                  ($expand-benchmark-case benchmark args) effective)])
                                    (record-report! (list result) effective)
                                    (set! next-results (cons result next-results))
                                    (not (and ($benchmark-config-stop-on-error? effective)
                                              (benchmark-result-error result))))))])
                        (if completed?
                            (loop-benchmarks (cdr benchmarks) next-results)
                            (finish next-results)))))))))))

  #|proc:benchmark-run
  The `benchmark-run` procedure runs registered benchmarks, descriptors, or a
  registry with a runner configuration and returns their measured results. It
  supports `(benchmark-run)`, `(benchmark-run config)`, `(benchmark-run registry
  config)`, and `(benchmark-run benchmarks config)`. Explicit config fields override
  descriptor options, which override defaults. Reporting groups results sharing a
  reporter and output port. Enabled stopping returns results through the first error
  and skips all later argument cases and descriptors. Empty or fully filtered runs
  emit an empty report through the configured reporter and output when both are enabled.
  Built-in reporter ports must be open and textual; effective ports are checked before execution.
  |#
  (define benchmark-run
    (case-lambda
      [()
       (benchmark-run (current-benchmark-registry) (default-benchmark-config))]
      [(config)
       (pcheck ([benchmark-config? config])
               (benchmark-run (current-benchmark-registry) config))]
      [(source config)
       (pcheck ([benchmark-config? config])
               (cond
                [(benchmark-suite? source) (benchmark-run-suite source config)]
                [(benchmark-registry? source)
                 ($run-benchmarks ($benchmark-registry-benchmarks source) config)]
                [(benchmark? source) ($run-benchmarks (list source) config)]
                [(and (list? source) (andmap benchmark? source))
                 ($run-benchmarks source config)]
                [else (errorf 'benchmark-run "invalid benchmark source: ~s" source)]))]))

  #|proc:benchmark-summarize
The `benchmark-summarize` procedure computes CPU nanosecond, real nanosecond,
and allocated-byte per-iteration summary alists from measured `samples`.
Division by iteration counts preserves exact fractional measurements.
|#
  (define benchmark-summarize
    (lambda (samples)
      (pcheck ([list? samples])
              `((cpu-ns . ,($summary-values (map $sample-cpu-ns samples)))
                (real-ns . ,($summary-values (map $sample-real-ns samples)))
                (bytes . ,($summary-values (map $sample-bytes samples)))))))

  #|proc:benchmark-save-baseline
The `benchmark-save-baseline` procedure writes benchmark `results` to file
`path` as versioned Scheme data. The file can be read back with
`benchmark-load-baseline`. Runtime and template argument keys must be readable
Scheme data; unsupported values such as procedures or ports are rejected before
changing `path`. Unreadable error who fields and irritants are saved as diagnostic strings.
Generated symbol keys retain their identity independently of ambient printer settings.
|#
  (define benchmark-save-baseline
    (lambda (results path)
      (pcheck ([list? results] [string? path])
              (let* ([datum `((format . cspp-benchmark-baseline)
                              (version . 1)
                              (results . ,(map $result->datum results)))]
                     [text ($write-string datum)])
                (unless ($readback-equal? datum text)
                  (errorf 'benchmark-save-baseline
                    "results contain unreadable data; argument keys must be readable Scheme values"))
                (let ([out (open-output-file path 'replace)])
                  (dynamic-wind
                    (lambda () (void))
                    (lambda () (display text out) (newline out))
                    (lambda ()
                      (guard (raised
                              [else
                               ;; Discard pending data after a failed flush so the port can close.
                               (clear-output-port out)
                               (close-output-port out)
                               (raise raised)])
                        (close-output-port out)))))))))

  #|proc:benchmark-load-baseline
The `benchmark-load-baseline` procedure reads benchmark results previously
written by `benchmark-save-baseline` from string file `path`, returning a list of
result records. Malformed records and negative or nonintegral sample measurements
raise an error identifying the field. Omitted fields retain legacy defaults;
historical summary-only records are preserved without synthesizing samples.
|#
  (define benchmark-load-baseline
    (lambda (path)
      (pcheck ([string? path])
              (let ([datum (parameterize ([case-sensitive #t])
                             (let ([in (open-input-file path)])
                               (dynamic-wind
                                 (lambda () (void))
                                 (lambda ()
                                   (let ([datum (read in)])
                                     (unless (eof-object? (read in))
                                       (errorf 'benchmark-load-baseline "unexpected trailing datum"))
                                     datum))
                                 (lambda () (close-input-port in)))))])
                ($baseline-alist datum 'baseline)
                ($baseline-check list? ($alist-ref datum 'results '()) 'results)
                (unless (eq? ($alist-ref datum 'format #f) 'cspp-benchmark-baseline)
                  (errorf 'benchmark-load-baseline "invalid benchmark baseline file: ~a" path))
                (let ([version ($alist-ref datum 'version #f)])
                  (unless (and (number? version) (= version 1))
                    (errorf 'benchmark-load-baseline "unsupported benchmark baseline version: ~a"
                            version)))
                (map $datum->result ($alist-ref datum 'results '()))))))

  #|proc:benchmark-absolute-difference
The `benchmark-absolute-difference` procedure returns `current - baseline` for
numeric measurements `baseline` and `current`. Finite real inputs are subtracted
exactly, preserving their represented values without intermediate overflow or underflow.
|#
  (define benchmark-absolute-difference
    (lambda (baseline current)
      (pcheck ([number? baseline current])
              (if (and (real? baseline) (finite? baseline)
                       (real? current) (finite? current))
                  (- (inexact->exact current) (inexact->exact baseline))
                  (- current baseline)))))

  #|proc:benchmark-percent-difference
The `benchmark-percent-difference` procedure returns the percent change from
real measurement `baseline` to real measurement `current`. Complex inputs are rejected. When
`baseline` is zero, equal values return zero and non-equal values return
positive or negative infinity according to the sign of `current`. NaN inputs propagate NaN.
Finite inputs use exact arithmetic on their represented values to avoid intermediate overflow.
|#
  (define benchmark-percent-difference
    (lambda (baseline current)
      (pcheck ([real? baseline current])
              (cond
               [(zero? baseline)
                (cond
                 [(zero? current) 0]
                 [(nan? current) +nan.0]
                 [(positive? current) +inf.0]
                 [else -inf.0])]
               [(and (finite? baseline) (finite? current))
                (* 100 (/ (benchmark-absolute-difference baseline current)
                          (inexact->exact baseline)))]
               [else (* 100 (/ (- current baseline) baseline))]))))

  #|proc:benchmark-compare-results
  The `benchmark-compare-results` procedure compares current benchmark `current`
  results with `baseline` results. `options` is an alist supporting `metric`,
  `threshold-percent`, `threshold-absolute`, and `noise-threshold-percent`.
  Each key may appear once; unknown keys and non-symbol metrics are rejected.
  Thresholds must be finite nonnegative real numbers; invalid thresholds raise an
  error naming the option, including when the result lists are empty or do not match.
  The returned list excludes results with captured errors or without a finite real
  mean for the selected metric. Runs with no samples are excluded, including after
  baseline save/load. Historical baselines without sample data remain comparable;
  their omission of sample data is preserved when saved. A measured zero is comparable.
  Percentage magnitude determines significance; the absolute change determines direction.
  |#
  (define benchmark-compare-results
    (case-lambda
      [(baseline current)
       (benchmark-compare-results baseline current '())]
      [(baseline current options)
       (pcheck ([list? baseline current options])
               (unless (and (andmap benchmark-result? baseline)
                            (andmap benchmark-result? current))
                 (errorf 'benchmark-compare-results "expected lists of benchmark results"))
               (let loop ([options options] [seen '()])
                 (unless (null? options)
                   (let ([option (car options)])
                     (unless (and (pair? option)
                                  (memq (car option)
                                    '(metric threshold-percent threshold-absolute
                                      noise-threshold-percent)))
                       (errorf 'benchmark-compare-results "unknown or malformed option: ~s" option))
                     (when (memq (car option) seen)
                       (errorf 'benchmark-compare-results "duplicate option: ~s" (car option)))
                     (when (and (eq? (car option) 'metric) (not (symbol? (cdr option))))
                       (errorf 'benchmark-compare-results "metric must be a symbol: ~s" (cdr option)))
                     (loop (cdr options) (cons (car option) seen)))))
               (let ([metric ($option-ref options 'metric 'real-ns)]
                     [threshold-percent ($option-ref options 'threshold-percent 5)]
                     [threshold-absolute ($option-ref options 'threshold-absolute 0)]
                     [noise-threshold-percent ($option-ref options 'noise-threshold-percent 0)])
                 (for-each
                   (lambda (key value)
                     (unless (and (real? value) (finite? value) (>= value 0))
                       (errorf 'benchmark-compare-results
                         "~a must be a finite nonnegative real: ~s" key value)))
                   '(threshold-percent threshold-absolute noise-threshold-percent)
                   (list threshold-percent threshold-absolute noise-threshold-percent))
                 (let ([baseline-index
                        (let ([index (make-hashtable equal-hash equal?)])
                          (for-each
                            (lambda (result)
                              (let ([key ($result-key result)])
                                ;; Keep the first match, as the original ordered search did.
                                (unless (hashtable-contains? index key)
                                  (hashtable-set! index key result))))
                            baseline)
                          index)])
                   (let loop ([current current] [out '()])
                     (if (null? current)
                         (reverse out)
                         (let* ([cur (car current)]
                                [base (hashtable-ref baseline-index ($result-key cur) #f)])
                           (let ([base-value (and base ($comparison-metric-mean base metric))]
                                 [cur-value ($comparison-metric-mean cur metric)])
                             (if (and base-value cur-value)
                                 (let* ([absolute (benchmark-absolute-difference base-value cur-value)]
                                        [percent (benchmark-percent-difference base-value cur-value)]
                                        [abs-percent (abs percent)]
                                        [significant? (and (>= (abs absolute) threshold-absolute)
                                                           (>= abs-percent noise-threshold-percent))]
                                        [regression? (and significant?
                                                          (positive? absolute)
                                                          (>= abs-percent threshold-percent))]
                                        [improvement? (and significant?
                                                           (negative? absolute)
                                                           (>= (abs percent) threshold-percent))])
                                   (loop (cdr current)
                                         (cons ($mk-benchmark-comparison
                                                (benchmark-result-name cur)
                                                metric
                                                base
                                                cur
                                                base-value
                                                cur-value
                                                absolute
                                                percent
                                                regression?
                                                improvement?)
                                               out)))
                                 (loop (cdr current) out)))))))))]))

  #|proc:benchmark-pause-timing
The `benchmark-pause-timing` procedure pauses timing for measured benchmark
`state`. Work performed while timing is paused is subtracted from the measured
sample when `benchmark-resume-timing` is called.
|#
  (define benchmark-pause-timing
    (lambda (state)
      (pcheck ([benchmark-state? state])
              (unless ($benchmark-state-timing-paused? state)
                ($benchmark-state-pause-start-set! state (statistics))
                ($benchmark-state-timing-paused?-set! state #t)))))

  #|proc:benchmark-resume-timing
The `benchmark-resume-timing` procedure resumes timing for measured benchmark
`state` and accumulates the elapsed paused statistics for later subtraction.
|#
  (define benchmark-resume-timing
    (lambda (state)
      (pcheck ([benchmark-state? state])
              (when ($benchmark-state-timing-paused? state)
                (let* ([after (statistics)]
                       [paused (sstats-difference after ($benchmark-state-pause-start state))])
                  ($benchmark-state-paused-sstats-set!
                   state
                   ($sstats-add ($benchmark-state-paused-sstats state) paused))
                  ($benchmark-state-pause-start-set! state #f)
                  ($benchmark-state-timing-paused?-set! state #f))))))

  #|proc:benchmark-do-not-optimize
The `benchmark-do-not-optimize` procedure returns `value` through Chez's
`black-box` optimization barrier so benchmark inputs or results appear to be
consumed by an unknown context.
|#
  (define benchmark-do-not-optimize
    (lambda (value)
      (black-box value)))

  #|proc:benchmark-clobber-memory
The `benchmark-clobber-memory` procedure is a weak v1 mutation visibility
barrier. It returns `value` through `benchmark-do-not-optimize`; it does not
flush CPU caches or enforce hardware memory ordering.
|#
  (define benchmark-clobber-memory
    (lambda (value)
      (benchmark-do-not-optimize value)))

  #|proc:benchmark-report
The `benchmark-report` procedure writes existing benchmark `results` through
`reporter` to output port `out`. Every result must be a benchmark result record;
invalid collections are rejected before any reporter callback runs.
Built-in reporters require an open textual port; custom reporters define their own port usage.
|#
  (define benchmark-report
    (lambda (results reporter out)
      (pcheck ([list? results] [benchmark-reporter? reporter] [output-port? out])
              (unless (andmap benchmark-result? results)
                (errorf 'benchmark-report "expected a list of benchmark results"))
              ($check-reporter-output 'benchmark-report reporter out)
              (($benchmark-reporter-start reporter) results out)
              (for-each (lambda (result)
                          (($benchmark-reporter-result reporter) result out))
                        results)
              (($benchmark-reporter-finish reporter) results out)))))
