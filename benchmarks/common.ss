(import (cspp benchmark) (cspp cli))

(define benchmark-option
  (lambda (name parser help)
    (let ([option (make-option name)])
      ;; Explain the benchmark option's values, units, and default.
      (option-help-set! option help)
      (option-value-number-set! option 1)
      (option-value-parser-set! option parser)
      (option-no-short?-set! option #t)
      option)))

(define benchmark-cli
  (lambda (program overview body argument*)
    (let ([command (make-command program)])
      (command-overview-set! command overview)
      (command-help-set! command overview)
      (command-exec-set! command body)
      (command-options-add!
       command
       (benchmark-option 'size parser-natural "Number of elements, a nonnegative integer (default: 1000).")
       (benchmark-option 'samples parser-natural "Measured samples per case, a positive integer (default: 5).")
       (benchmark-option 'warmup parser-natural "Warmup runs per case, a nonnegative integer (default: 1).")
       (benchmark-option 'min-time parser-flonum "Minimum seconds per sample, nonnegative (default: 0.01).")
       (benchmark-option 'max-iterations parser-natural "Maximum iterations per sample, positive (default: 10000).")
       (benchmark-option 'format parser-string "Report format: text, csv, json, or datum (default: text).")
       (benchmark-option 'output parser-string "Report file to create or replace (default: standard output).")
       (benchmark-option 'filter parser-string "Run cases whose names contain this substring (default: all cases).")
       (let ([option (make-option 'cost-center)])
         (option-help-set! option "Request optional cost-center metrics; unavailable metrics are labeled (default: off).")
         (option-value-number-set! option 0)
         (option-no-short?-set! option #t)
         option))
      (run-cli-command! command argument*))))

(define benchmark-config-from-cli
  (lambda (ref)
    (let* ([base (default-benchmark-config)]
           [format-name (or (ref 'format) "text")]
           [reporter (benchmark-detailed-reporter format-name)]
           [config (benchmark-config-with
                    base
                    (list (cons 'warmup (or (ref 'warmup) 1))
                          (cons 'samples (or (ref 'samples) 5))
                          (cons 'min-time (or (ref 'min-time) 0.01))
                          (cons 'max-iterations (or (ref 'max-iterations) 10000))
                          (cons 'reporter reporter)
                          (cons 'filter (ref 'filter))
                          (cons 'cost-center? (and (ref 'cost-center) #t))))]
           [output (ref 'output)])
      (unless (and (positive? (benchmark-config-samples config))
                   (positive? (benchmark-config-max-iterations config))
                   (>= (benchmark-config-min-time config) 0))
        (errorf 'benchmark "samples and max-iterations must be positive; min-time must be nonnegative"))
      (if output
          (let ([port (open-file-output-port output (file-options no-fail replace)
                                             (buffer-mode block) (native-transcoder))])
            (values (benchmark-config-with config (list (cons 'output port))) port))
          (values config #f)))))

(define benchmark-size
  (lambda (ref)
    (let ([size (or (ref 'size) 1000)])
      (unless (and (fixnum? size) (fx<= size (isqrt (fxquotient (most-positive-fixnum) 2))))
        (errorf 'benchmark "size must fit the fixnum workloads, including their sums"))
      size)))

(define benchmark-run-cli!
  (lambda (program overview benchmarks ref)
    (let-values ([(config output-port) (benchmark-config-from-cli ref)])
      (dynamic-wind
        void
        (lambda ()
          ;; Run matching cases only, and propagate captured benchmark failures to the CLI.
          (let* ([filter-text (ref 'filter)]
                 [selected (if filter-text
                               (filter (lambda (item)
                                         (string-contains? (symbol->string (benchmark-name item))
                                                           filter-text))
                                       benchmarks)
                               benchmarks)])
            (when (null? selected) (errorf program "filter matched no benchmark cases"))
            ;; The CLI has already applied its substring filter; the library selector
            ;; uses prefix matching and must not filter these cases a second time.
            (let ([results (benchmark-run selected
                             (benchmark-config-with config '((filter . #f))))])
              (for-each (lambda (result)
                          (when (benchmark-result-error result)
                            (raise (benchmark-result-error result)))) results))))
        (lambda () (when output-port (close-port output-port))))
      0)))

(define benchmark-consume
  (lambda (value)
    (benchmark-do-not-optimize value)))

(define benchmark-time-ns
  (lambda (time) (+ (* (time-second time) 1000000000) (time-nanosecond time))))

(define benchmark-metadata
  (lambda ()
    `((cspp-version . ,(library-version '(cspp)))
      (scheme-version . ,(scheme-version))
      (machine . ,(machine-type))
      (build . ,(find (lambda (option) (string-startswith? option "VARIANT="))
                      (string-split (call-with-input-file ".cspp-build-options" get-string-all)
                                    #\space)))
      (date . ,(date-and-time)))))

(define benchmark-sample-data
  (lambda (sample)
    (let ([stats (benchmark-sample-sstats sample)])
      `((iterations . ,(benchmark-sample-iterations sample))
        (cpu-ns . ,(benchmark-time-ns (sstats-cpu stats)))
        (real-ns . ,(benchmark-time-ns (sstats-real stats)))
        (bytes . ,(sstats-bytes stats))
        (gc-count . ,(sstats-gc-count stats))
        (gc-bytes . ,(sstats-gc-bytes stats))
        (gc-cpu-ns . ,(benchmark-time-ns (sstats-gc-cpu stats)))
        (gc-real-ns . ,(benchmark-time-ns (sstats-gc-real stats)))
        (cost-center-allocation . ,(or (benchmark-sample-cost-center-allocation-count sample) 'unavailable))
        (cost-center-instructions . ,(or (benchmark-sample-cost-center-instruction-count sample) 'unavailable))))))

(define benchmark-result-data
  (lambda (result)
    `((name . ,(benchmark-result-name result))
      (args . ,(benchmark-result-args result))
      (summary . ,(benchmark-result-summary result))
      (samples . ,(map benchmark-sample-data (benchmark-result-samples result)))
      (error . ,(and (benchmark-result-error result) #t)))))

;; Encode the report's small data vocabulary without changing JSON/CSV output with log text.
(define benchmark-write-json
  (lambda (value out)
    (cond
      [(eq? value #f) (display "false" out)]
      [(eq? value #t) (display "true" out)]
      [(number? value) (display (if (integer? value) value (exact->inexact value)) out)]
      [(or (string? value) (symbol? value))
       (write-char #\" out)
       (string-for-each
        (lambda (ch)
          (case ch
            [(#\") (display "\\\"" out)]
            [(#\\) (display "\\\\" out)]
            [(#\newline) (display "\\n" out)]
            [(#\return) (display "\\r" out)]
            [(#\tab) (display "\\t" out)]
            [else (write-char ch out)]))
        (if (symbol? value) (symbol->string value) value))
       (write-char #\" out)]
      [else
       (let ([object? (and (pair? value)
                           (for-all (lambda (entry) (and (pair? entry) (symbol? (car entry)))) value))])
         (display (if object? "{" "[") out)
         (let loop ([rest value] [first? #t])
           (unless (null? rest)
             (unless first? (display "," out))
             (if object?
                 (begin (benchmark-write-json (caar rest) out)
                        (display ":" out)
                        (benchmark-write-json (cdar rest) out))
                 (benchmark-write-json (car rest) out))
             (loop (cdr rest) #f)))
         (display (if object? "}" "]") out))])))

(define benchmark-csv-cell
  (lambda (value out)
    (write-char #\" out)
    (string-for-each
     (lambda (ch) (if (char=? ch #\") (display "\"\"" out) (write-char ch out)))
     (call-with-string-output-port (lambda (port) (write value port))))
    (write-char #\" out)))

(define benchmark-detailed-reporter
  (lambda (format-name)
    (unless (member format-name '("text" "csv" "json" "datum"))
      (errorf 'benchmark "unknown report format ~s" format-name))
    ;; Use the benchmark reporter lifecycle to include metadata and raw metrics in every format.
    (make-benchmark-reporter
     (lambda (results out) (void))
     (lambda (result out) (void))
     (lambda (results out)
       (let ([metadata (benchmark-metadata)] [records (map benchmark-result-data results)])
         (cond
           [(string=? format-name "text")
            (benchmark-report results (benchmark-text-reporter) out)
            (pretty-print `((metadata . ,metadata) (results . ,records)) out)]
           [(string=? format-name "json")
            (benchmark-write-json `((metadata . ,metadata) (results . ,records)) out)
            (newline out)]
           [(string=? format-name "datum")
            (write `((metadata . ,metadata) (results . ,records)) out)
            (newline out)]
           [else
            (display "name,args,summary,samples,metadata,error\n" out)
            (for-each
             (lambda (record)
               (let loop ([cells (list (cdr (assq 'name record)) (cdr (assq 'args record))
                                       (cdr (assq 'summary record)) (cdr (assq 'samples record))
                                       metadata (cdr (assq 'error record)))] [first? #t])
                 (unless (null? cells)
                   (unless first? (display "," out))
                   (benchmark-csv-cell (car cells) out)
                   (loop (cdr cells) #f)))
               (newline out)) records)]))
       (flush-output-port out)))))
