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


;;;;===----------------------------------------------------------------------===
;;;; records and low-level helpers
;;;;===----------------------------------------------------------------------===

  (define-record-type ($benchmark $mk-benchmark benchmark?)
    (fields name body options args arg-names setup teardown
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
    (fields start result finish))

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
      (let ([iterations (benchmark-sample-iterations sample)])
        (if (zero? iterations)
            0
            (/ ($time->ns (sstats-cpu (benchmark-sample-sstats sample)))
               iterations)))))

  (define $sample-real-ns
    (lambda (sample)
      (let ([iterations (benchmark-sample-iterations sample)])
        (if (zero? iterations)
            0
            (/ ($time->ns (sstats-real (benchmark-sample-sstats sample)))
               iterations)))))

  (define $sample-bytes
    (lambda (sample)
      (let ([iterations (benchmark-sample-iterations sample)])
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

  (define $write-string
    (lambda (value)
      (parameterize ([print-graph #t] [print-level #f] [print-length #f])
        (call-with-string-output-port (lambda (out) (write value out))))))

  (define $readback-equal?
    (lambda (value text)
      (guard (raised [else #f])
        (call-with-port (open-input-string text)
          (lambda (in) (equal? value (read in)))))))

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
       [(symbol? key) (symbol->string key)]
       [(string? key) key]
       [(char? key) (string key)]
       [else ($write-string key)])))

  (define $json-number-string
    (lambda (n)
      (number->string (if (and (rational? n) (not (integer? n)))
                          (exact->inexact n)
                          n))))

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
      (display "{" out)
      (let loop ([alist alist] [first? #t])
        (unless (null? alist)
          (unless first? (display "," out))
          ($json-string ($json-key (caar alist)) out)
          (display ":" out)
          ($write-json (cdar alist) out ancestors)
          (loop (cdr alist) #f)))
      (display "}" out)))

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

  (define $datum->sample
    (lambda (datum)
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
|#
  (define benchmark-state
    (lambda (name suite args template-args iteration iterations sample)
      (pcheck ([symbol? name] [list? args template-args] [natural? iteration iterations sample])
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

  #|proc:make-benchmark
The `make-benchmark` procedure creates a benchmark descriptor named by symbol
`name`. `body` is a procedure with signature `(lambda (state arg ... setup-value)
...)`; `options` is a benchmark option alist, usually produced by
`benchmark-options`.
|#
  (define make-benchmark
    (lambda (name body options)
      (pcheck ([symbol? name] [procedure? body] [list? options])
              (let ([args ($normalize-arguments options)])
                ($mk-benchmark name
                               body
                               options
                               args
                               ($argument-names args)
                               ($option-ref options 'setup #f)
                               ($option-ref options 'teardown #f)
                               ($option-ref options 'fixture #f)
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
    (make-parameter ($mk-benchmark-registry '() '())))


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
|#
  (define make-benchmark-reporter
    (lambda (start result finish)
      (pcheck ([procedure? start result finish])
              ($mk-benchmark-reporter start result finish))))

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
      (make-benchmark-reporter
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
Scheme datum per result to the output port.
|#
  (define benchmark-datum-reporter
    (lambda ()
      (make-benchmark-reporter
       $void-procedure
       (lambda (result out)
         (write `((name . ,(benchmark-result-name result))
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
error.
|#
  (define benchmark-csv-reporter
    (lambda ()
      (make-benchmark-reporter
       (lambda (results out)
         (fprintf out "name,args,template_args,cpu_ns_mean,real_ns_mean,bytes_mean,counters,error~%"))
       (lambda (result out)
         (let* ([summary (benchmark-result-summary result)]
                [cpu (cdr (assq 'mean (cdr (assq 'cpu-ns summary))))]
                [real (cdr (assq 'mean (cdr (assq 'real-ns summary))))]
                [bytes (cdr (assq 'mean (cdr (assq 'bytes summary))))])
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
|#
  (define benchmark-json-reporter
    (lambda ()
      (let ([first? #t])
        (make-benchmark-reporter
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
`counters`, and `error`.
|#
  (define benchmark-rich-reporter
    (case-lambda
      [()
       (benchmark-rich-reporter '())]
      [(options)
       (pcheck ([list? options])
               (make-benchmark-reporter
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
  |#
  (define benchmark-config-with
    (lambda (config options)
      (pcheck ([benchmark-config? config] [list? options])
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
                                (cons (car option) explicit)))))))))

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

  (define $duplicate-symbol?
    (lambda (names)
      (let loop ([names names] [seen '()])
        (cond
         [(null? names) #f]
         [(memq (car names) seen) (car names)]
         [else (loop (cdr names) (cons (car names) seen))]))))

  (define $any-shared-symbol?
    (lambda (left right)
      (let loop ([left left])
        (cond
         [(null? left) #f]
         [(memq (car left) right) (car left)]
         [else (loop (cdr left))]))))

  (define $zip-argument-row
    (lambda (names values)
      (let loop ([names names] [values values] [out '()])
        (cond
         [(and (null? names) (null? values)) (reverse out)]
         [(or (null? names) (null? values))
          (errorf '$zip-argument-row "argument value row length does not match names")]
         [else
          (loop (cdr names)
                (cdr values)
                (cons (cons (car names) (car values)) out))]))))

  (define $args-spec->rows
    (lambda (spec)
      (unless (and (pair? spec) (list? (car spec)) (not (null? (car spec))))
        (errorf '$args-spec->rows "invalid :args option: ~s" spec))
      (let ([names (car spec)]
            [value-rows (cdr spec)])
        (unless (andmap symbol? names)
          (errorf '$args-spec->rows "argument names must be symbols: ~s" names))
        (let ([duplicate ($duplicate-symbol? names)])
          (when duplicate
            (errorf '$args-spec->rows "duplicate argument name: ~a" duplicate)))
        (map (lambda (values)
               (unless (list? values)
                 (errorf '$args-spec->rows "invalid argument value row: ~s" values))
               (list names values ($zip-argument-row names values)))
             value-rows))))

  (define $normalize-arguments
    (lambda (options)
      (when (assq 'arg-product options)
        (errorf '$normalize-arguments ":arg-product has been removed; use repeated :args forms"))
      (let ([args-options (filter (lambda (option)
                                    (memq (car option) '(args arg-range dense-arg-range)))
                                  options)])
        (if (null? args-options)
            (list (list '() '() '()))
            (let loop ([options args-options] [rows (list (list '() '() '()))] [seen-names '()])
              (if (null? options)
                  rows
                  (let* ([option (car options)]
                         [new-rows
                          (case (car option)
                            [(args)
                             ($args-spec->rows (cdr option))]
                            [(arg-range)
                             (let* ([spec (cdr option)]
                                    [name (car spec)]
                                    [values ($range-values (cadr spec) (caddr spec) (cadddr spec) #t)])
                               (map (lambda (value)
                                      (list (list name) (list value) (list (cons name value))))
                                    values))]
                            [(dense-arg-range)
                             (let* ([spec (cdr option)]
                                    [name (car spec)]
                                    [values ($range-values (cadr spec) (caddr spec) (cadddr spec) #f)])
                               (map (lambda (value)
                                      (list (list name) (list value) (list (cons name value))))
                                    values))]
                            [else (errorf '$normalize-arguments "unknown argument option: ~a" (car option))])]
                         [new-names (case (car option)
                                      [(args) (cadr option)]
                                      [(arg-range dense-arg-range) (list (cadr option))]
                                      [else (if (null? new-rows) '() (car (car new-rows)))])]
                         [duplicate ($duplicate-symbol? new-names)]
                         [shared ($any-shared-symbol? new-names seen-names)])
                    (when duplicate
                      (errorf '$normalize-arguments "duplicate argument name: ~a" duplicate))
                    (when shared
                      (errorf '$normalize-arguments "duplicate argument name across argument options: ~a" shared))
                    (loop (cdr options)
                          (apply append
                                 (map (lambda (left)
                                        (map (lambda (right)
                                               (list (append (car left) (car right))
                                                     (append (cadr left) (cadr right))
                                                     (append (caddr left) (caddr right))))
                                             new-rows))
                                      rows))
                          (append seen-names new-names)))))))))

  (define $argument-names
    (lambda (args)
      (if (null? args)
          '()
          (car (car args)))))

  #|macro:benchmark-options
  The `benchmark-options` macro converts benchmark option syntax into an option
  alist. `:args` receives a grouped table `([name ...] [value ...] ...)`; each
  `name` is an argument name and each value row supplies one concrete benchmark
  case. Repeated `:args` tables are expanded as a Cartesian product, and argument
  names must not be duplicated in one table or across argument options.
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
            [(name value rest ...)
             (identifier? #'name)
             #'(name value rest ...)]
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
and the setup value.
|#
  (define-syntax define-benchmark-fixture
    (syntax-rules ()
      [(_ name options)
       (define name
         (let ([opts (benchmark-options options)])
           ($mk-benchmark-fixture 'name
                                  ($option-ref opts 'setup $false-procedure)
                                  ($option-ref opts 'teardown $void-procedure))))]))

  #|macro:define-fixture-benchmark
The `define-fixture-benchmark` macro defines and registers benchmark `name`
using fixture descriptor `fixture`. `options` has the same argument syntax as
`define-benchmark`; `body` receives state, argument values in option order, and
the fixture setup value.
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
  data in the state to release partially acquired resources.
  |#
  (define-syntax define-benchmark-suite
    (syntax-rules ()
      [(_ name options benchmark ...)
       (define name
         (let ([opts (benchmark-options options)])
           ($mk-benchmark-suite 'name
                                (list benchmark ...)
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

  #|proc:benchmark-expand
The `benchmark-expand` procedure expands `benchmark` into concrete run pairs
for `config`. Each pair contains a benchmark state in the car and the descriptor
to run in the cdr.
|#
  (define benchmark-expand
    (lambda (benchmark config)
      (pcheck ([benchmark? benchmark] [benchmark-config? config])
              (map (lambda (arg-row)
                     (let ([args (caddr arg-row)])
                       (cons ($make-benchmark-state ($benchmark-name benchmark)
                                                    (or ($current-suite-context)
                                                        ($benchmark-suite-ref benchmark))
                                                    args
                                                    ($benchmark-template-args benchmark)
                                                    0
                                                    1
                                                    0
                                                    '()
                                                    '()
                                                    #f)
                             benchmark)))
                   ($benchmark-args benchmark)))))

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
        (let ([fixture ($benchmark-fixture-ref benchmark)])
        (cond
         [fixture
          (values (($benchmark-fixture-setup fixture) state)
                  ($benchmark-fixture-teardown fixture)
                  #t)]
         [($benchmark-setup benchmark)
          (values (($benchmark-setup benchmark) state)
                  (or ($benchmark-teardown benchmark) $void-procedure)
                  #t)]
         [else
          (values #f $void-procedure #f)]))))

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
                 [setup-value? (caddr setup-results)]
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
                   (when setup-value?
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
  fields override descriptor options, which override default settings.
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
                    [report-groups '()])
                (define collect-report!
                  (lambda (results reporter out)
                    (let ([group (find (lambda (group)
                                         (and (eq? reporter (caar group))
                                              (eq? out (cdar group))))
                                       report-groups)])
                      (if group
                          (set-cdr! group (append (reverse results) (cdr group)))
                          (set! report-groups
                            (cons (cons (cons reporter out) (reverse results)) report-groups))))))
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
                (for-each
                  (lambda (group)
                    ($dispatch-benchmark-report (reverse (cdr group)) (caar group) (cdar group)))
                  (reverse report-groups))
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
      (let ([report-groups '()])
        (define record-report!
          (lambda (result effective)
            (let ([reporter (benchmark-config-reporter effective)]
                  [out (benchmark-config-output effective)])
              (when (and reporter out)
                (let ([group (find (lambda (group)
                                     (and (eq? reporter (caar group))
                                          (eq? out (cdar group))))
                                   report-groups)])
                  (if group
                      (set-cdr! group (cons result (cdr group)))
                      (set! report-groups
                            (cons (cons (cons reporter out) (list result)) report-groups))))))))
        (define finish
          (lambda (results)
            (for-each (lambda (group)
                        ($dispatch-benchmark-report
                         (reverse (cdr group)) (caar group) (cdar group)))
                      (reverse report-groups))
            (reverse results)))
        (let loop-benchmarks ([benchmarks benchmarks] [results '()])
          (if (null? benchmarks)
              (finish results)
              (let* ([benchmark (car benchmarks)]
                     [effective ($effective-benchmark-config benchmark config)])
                (if (not ($benchmark-matches-selector?
                          benchmark (benchmark-config-filter effective)))
                    (loop-benchmarks (cdr benchmarks) results)
                    (let loop-cases ([expanded (benchmark-expand benchmark effective)]
                                     [results results])
                      (if (null? expanded)
                          (loop-benchmarks (cdr benchmarks) results)
                          (let ([result ($run-expanded (car expanded) effective)])
                            (record-report! result effective)
                            (if (and ($benchmark-config-stop-on-error? effective)
                                     (benchmark-result-error result))
                                (finish (cons result results))
                                (loop-cases (cdr expanded) (cons result results)))))))))))))

  #|proc:benchmark-run
  The `benchmark-run` procedure runs registered benchmarks, descriptors, or a
  registry with a runner configuration and returns their measured results. It
  supports `(benchmark-run)`, `(benchmark-run config)`, `(benchmark-run registry
  config)`, and `(benchmark-run benchmarks config)`. Explicit config fields override
  descriptor options, which override defaults. Reporting groups results sharing a
  reporter and output port. Enabled stopping returns results through the first error
  and skips all later argument cases and descriptors.
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
                [(list? source) ($run-benchmarks source config)]
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
                (call-with-output-file path
                  (lambda (out) (display text out) (newline out))
                  'replace)))))

  #|proc:benchmark-load-baseline
The `benchmark-load-baseline` procedure reads benchmark results previously
written by `benchmark-save-baseline` from file `path`.
|#
  (define benchmark-load-baseline
    (lambda (path)
      (pcheck ([string? path])
              (let ([datum (call-with-input-file path read)])
                (unless (eq? ($alist-ref datum 'format #f) 'cspp-benchmark-baseline)
                  (errorf 'benchmark-load-baseline "invalid benchmark baseline file: ~a" path))
                (let ([version ($alist-ref datum 'version #f)])
                  (unless (and (number? version) (= version 1))
                    (errorf 'benchmark-load-baseline "unsupported benchmark baseline version: ~a"
                            version)))
                (map $datum->result ($alist-ref datum 'results '()))))))

  #|proc:benchmark-absolute-difference
The `benchmark-absolute-difference` procedure returns `current - baseline` for
numeric measurements `baseline` and `current`.
|#
  (define benchmark-absolute-difference
    (lambda (baseline current)
      (pcheck ([number? baseline current])
              (- current baseline))))

  #|proc:benchmark-percent-difference
The `benchmark-percent-difference` procedure returns the percent change from
numeric measurement `baseline` to numeric measurement `current`. When
`baseline` is zero, equal values return zero and non-equal values return
positive or negative infinity.
|#
  (define benchmark-percent-difference
    (lambda (baseline current)
      (pcheck ([number? baseline current])
              (cond
               [(zero? baseline)
                (cond
                 [(zero? current) 0]
                 [(positive? current) +inf.0]
                 [else -inf.0])]
               [else (* 100 (/ (- current baseline) baseline))]))))

  #|proc:benchmark-compare-results
  The `benchmark-compare-results` procedure compares current benchmark `current`
  results with `baseline` results. `options` is an alist supporting `metric`,
  `threshold-percent`, `threshold-absolute`, and `noise-threshold-percent`.
  The returned list excludes results with captured errors or without a finite real
  mean for the selected metric. Runs with no samples are excluded, including after
  baseline save/load. Historical baselines without sample data remain comparable;
  their omission of sample data is preserved when saved. A measured zero is comparable.
  |#
  (define benchmark-compare-results
    (case-lambda
      [(baseline current)
       (benchmark-compare-results baseline current '())]
      [(baseline current options)
       (pcheck ([list? baseline current options])
               (let ([baseline-index
                      (let ([index (make-hashtable equal-hash equal?)])
                        (for-each
                          (lambda (result)
                            (let ([key ($result-key result)])
                              ;; Keep the first match, as the original ordered search did.
                              (unless (hashtable-contains? index key)
                                (hashtable-set! index key result))))
                          baseline)
                        index)]
                     [metric ($option-ref options 'metric 'real-ns)]
                     [threshold-percent ($option-ref options 'threshold-percent 5)]
                     [threshold-absolute ($option-ref options 'threshold-absolute 0)]
                     [noise-threshold-percent ($option-ref options 'noise-threshold-percent 0)])
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
                                                        (>= percent threshold-percent))]
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
                               (loop (cdr current) out))))))))]))

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
`reporter` to output port `out`.
|#
  (define benchmark-report
    (lambda (results reporter out)
      (pcheck ([list? results] [benchmark-reporter? reporter] [output-port? out])
              (($benchmark-reporter-start reporter) results out)
              (for-each (lambda (result)
                          (($benchmark-reporter-result reporter) result out))
                        results)
              (($benchmark-reporter-finish reporter) results out)))))
