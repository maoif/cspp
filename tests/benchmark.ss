(import (cspp))

(define (benchmark-test-config)
  (benchmark-config-with (default-benchmark-config)
    '((warmup . 0)
      (samples . 2)
      (min-time . 0)
      (max-iterations . 1)
      (reporter . #f)
      (output . #f))))

(define (benchmark-temp-file)
  (format "benchmark-baseline-~a-~a.dat" (random 1000000) (time-nanosecond (current-time))))

(define (benchmark-write-datum-file path datum)
  (call-with-output-file path
    (lambda (out)
      (write datum out)
      (newline out))
    'replace))

(define (benchmark-string-contains? text needle)
  (let ([text-len (string-length text)]
        [needle-len (string-length needle)])
    (let loop ([i 0])
      (cond
       [(> (+ i needle-len) text-len) #f]
       [(string=? needle (substring text i (+ i needle-len))) #t]
       [else (loop (+ i 1))]))))

(define (benchmark-json-number-safe? text)
  (let ([len (string-length text)])
    (let loop ([i 0] [in-string? #f] [escaped? #f])
      (cond
       [(>= i len) #t]
       [in-string?
        (let ([ch (string-ref text i)])
          (cond
           [escaped? (loop (+ i 1) #t #f)]
           [(char=? ch #\\) (loop (+ i 1) #t #t)]
           [(char=? ch (integer->char 34)) (loop (+ i 1) #f #f)]
           [else (loop (+ i 1) #t #f)]))]
       [else
        (let ([ch (string-ref text i)])
          (cond
           [(char=? ch (integer->char 34)) (loop (+ i 1) #t #f)]
           [(and (char-numeric? ch)
                 (< (+ i 1) len)
                 (char=? (string-ref text (+ i 1)) #\/))
            #f]
           [(and (char=? ch #\+)
                 (< (+ i 1) len)
                 (char=? (string-ref text (+ i 1)) #\i))
            #f]
           [(and (char=? ch #\i)
                 (< (+ i 2) len)
                 (char=? (string-ref text (+ i 1)) #\n)
                 (char=? (string-ref text (+ i 2)) #\f))
            #f]
           [(and (char=? ch #\-)
                 (< (+ i 1) len)
                 (char=? (string-ref text (+ i 1)) #\i))
            #f]
           [else (loop (+ i 1) #f #f)]))]))))

(define (benchmark-json-string-control-safe? text)
  (let ([len (string-length text)])
    (let loop ([i 0] [in-string? #f] [escaped? #f])
      (cond
       [(>= i len) #t]
       [in-string?
        (let ([ch (string-ref text i)])
          (cond
           [escaped? (loop (+ i 1) #t #f)]
           [(char=? ch #\\) (loop (+ i 1) #t #t)]
           [(char=? ch (integer->char 34)) (loop (+ i 1) #f #f)]
           [(< (char->integer ch) 32) #f]
           [else (loop (+ i 1) #t #f)]))]
       [(char=? (string-ref text i) (integer->char 34))
        (loop (+ i 1) #t #f)]
       [else (loop (+ i 1) #f #f)]))))

(define (benchmark-string-lines text)
  (let ([len (string-length text)])
    (let loop ([start 0] [i 0] [out '()])
      (cond
       [(= i len)
        (reverse (if (= start i)
                     out
                     (cons (substring text start i) out)))]
       [(char=? (string-ref text i) #\newline)
        (loop (+ i 1) (+ i 1) (cons (substring text start i) out))]
       [else (loop start (+ i 1) out)]))))

(define (benchmark-string-index text needle)
  (let ([text-len (string-length text)]
        [needle-len (string-length needle)])
    (let loop ([i 0])
      (cond
       [(> (+ i needle-len) text-len) #f]
       [(string=? needle (substring text i (+ i needle-len))) i]
       [else (loop (+ i 1))]))))

(benchmark-clear-registry! (current-benchmark-registry))

(define-benchmark bench-basic
  (:args ([n]
          [1]
          [2])
   :warmup 0
   :samples 2
   :min-time 0
   :max-iterations 1
   :unit nanosecond)
  (lambda (state n)
    (benchmark-state-counter-add! state 'calls 1)
    (benchmark-do-not-optimize n)))

(define-benchmark-fixture bench-fixture
  (:setup (lambda (state)
            (vector (benchmark-state-arg state 'n)))
   :teardown (lambda (state value)
               (vector-set! value 0 'closed))))

(define-fixture-benchmark bench-with-fixture bench-fixture
  (:args ([n]
          [3])
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1)
  (lambda (state n value)
    (benchmark-do-not-optimize (vector-ref value 0))))

(define-benchmark-template bench-template (kind make-seq ref)
  (:args ([n]
          [4])
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1)
  (lambda (state n)
    (let ([seq (make-seq n 9)])
      (benchmark-do-not-optimize (ref seq 0)))))

(instantiate-benchmark-template bench-template
  ([vector make-vector vector-ref]))

(define-benchmark bench-error
  (:args ([n]
          [1])
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1
   :reporter #f)
  (lambda (state n)
    (error 'bench-error "expected benchmark body error")))

(define-benchmark bench-product
  (:args ([n]
          [1]
          [2])
   :args ([m]
          [10]
          [20])
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1)
  (lambda (state n m)
    (benchmark-do-not-optimize (+ n m))))

(define-benchmark bench-nine-args
  (:args ([a b c]
          [1 2 3]
          [4 5 6])
   :args ([d e f]
          [10 20 30]
          [40 50 60])
   :args ([g h i]
          [100 200 300]
          [400 500 600])
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1)
  (lambda (state a b c d e f g h i)
    (benchmark-state-counter-set!
     state
     'sum
     (+ a b c d e f g h i))
    (benchmark-do-not-optimize
     (list a b c d e f g h i))))

(define bench-control-string
  (make-benchmark 'bench-control-string
                  (lambda (state text)
                    (benchmark-do-not-optimize text))
                  `((args (text)
                          (,(string #\a #\backspace #\b)))
                    (warmup . 0)
                    (samples . 1)
                    (min-time . 0)
                    (max-iterations . 1))))

(define-benchmark bench-paused
  (:args ([n]
          [1])
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1)
  (lambda (state n)
    (benchmark-pause-timing state)
    (let loop ([i 0])
      (when (< i 50000)
        (benchmark-do-not-optimize i)
        (loop (+ i 1))))
    (benchmark-resume-timing state)
    (benchmark-do-not-optimize n)))

(define-benchmark-suite bench-suite
  (:suite-setup (lambda (state) 'suite-open)
   :suite-teardown (lambda (state value) (void)))
  bench-basic
  bench-with-fixture)

(mat benchmark-records
     (benchmark? (make-benchmark 'manual (lambda (state) (void)) '()))
     (benchmark-config? (default-benchmark-config))
     (benchmark-reporter? (benchmark-text-reporter))
     (benchmark-fixture? bench-fixture)
     (benchmark-suite? bench-suite)
     (let ([state (benchmark-state 'state-test #f '((n . 7)) '() 0 1 0)])
       (and (benchmark-state? state)
            (eq? (benchmark-state-name state) 'state-test)
            (= (benchmark-state-arg state 'n) 7)
            (not (benchmark-state-arg state 'missing #f))))
     ;; unknown argument names are rejected
     (error? (benchmark-state-arg (benchmark-state 'state-test #f '() '() 0 1 0) 'missing))
     ;; public constructors validate boundary argument types
     (error? (make-benchmark "bad-name" (lambda (state) (void)) '())))

(mat benchmark-registry-and-expansion
     (let ([names (map benchmark-name
                       (benchmark-registry-benchmarks (current-benchmark-registry)))])
       (and (memq 'bench-basic names)
            (memq 'bench-with-fixture names)
            (if (memq 'bench-template/vector names) #t #f)))
     (= (length (benchmark-expand bench-basic (benchmark-test-config))) 2)
     (= (length (benchmark-expand bench-product (benchmark-test-config))) 4)
     (= (length (benchmark-expand bench-nine-args (benchmark-test-config))) 8)
     (equal? (map benchmark-state-args
                  (map car (benchmark-expand bench-product (benchmark-test-config))))
             '(((n . 1) (m . 10))
               ((n . 1) (m . 20))
               ((n . 2) (m . 10))
               ((n . 2) (m . 20))))
     (equal? (map benchmark-state-args
                  (map car (benchmark-expand bench-nine-args (benchmark-test-config))))
             '(((a . 1) (b . 2) (c . 3) (d . 10) (e . 20) (f . 30) (g . 100) (h . 200) (i . 300))
               ((a . 1) (b . 2) (c . 3) (d . 10) (e . 20) (f . 30) (g . 400) (h . 500) (i . 600))
               ((a . 1) (b . 2) (c . 3) (d . 40) (e . 50) (f . 60) (g . 100) (h . 200) (i . 300))
               ((a . 1) (b . 2) (c . 3) (d . 40) (e . 50) (f . 60) (g . 400) (h . 500) (i . 600))
               ((a . 4) (b . 5) (c . 6) (d . 10) (e . 20) (f . 30) (g . 100) (h . 200) (i . 300))
               ((a . 4) (b . 5) (c . 6) (d . 10) (e . 20) (f . 30) (g . 400) (h . 500) (i . 600))
               ((a . 4) (b . 5) (c . 6) (d . 40) (e . 50) (f . 60) (g . 100) (h . 200) (i . 300))
               ((a . 4) (b . 5) (c . 6) (d . 40) (e . 50) (f . 60) (g . 400) (h . 500) (i . 600))))
     (let* ([selected (benchmark-select (current-benchmark-registry) "bench-basic")]
            [names (map benchmark-name selected)])
       (equal? names '(bench-basic))))

(mat benchmark-argument-syntax
     ;; duplicate names in the same :args table are rejected
     (error? (eval '(benchmark-options (:args ([n n]
                                               [1 2])))))

     ;; duplicate names across repeated :args tables are rejected
     (error? (eval '(benchmark-options (:args ([n]
                                               [1])
                                        :args ([n]
                                               [2])))))

     ;; duplicate names across :args and range options are rejected
     (error? (eval '(benchmark-options (:args ([n]
                                               [1])
                                        :arg-range [n 1 8 2]))))

     ;; value rows must have the same width as the name row
     (error? (eval '(benchmark-options (:args ([n m]
                                               [1])))))

     ;; :arg-product has been removed; repeated :args tables replace it
     (error? (eval '(benchmark-options (:arg-product [n 1 2])))))

(mat benchmark-runner
     (let ([results (benchmark-run (list bench-basic) (benchmark-test-config))])
       (and (= (length results) 2)
            (andmap benchmark-result? results)
            (andmap (lambda (result)
                      (and (= (length (benchmark-result-samples result)) 2)
                           (not (benchmark-result-error result))
                           (assq 'cpu-ns (benchmark-result-summary result))
                           (assq 'real-ns (benchmark-result-summary result))
                           (assq 'bytes (benchmark-result-summary result))
                           (>= (cdr (assq 'calls (benchmark-result-counters result))) 1)
                           #t))
                    results)))
     (let ([results (benchmark-run (list bench-with-fixture) (benchmark-test-config))])
       (and (= (length results) 1)
            (not (benchmark-result-error (car results)))))
     (let ([results (benchmark-run (list bench-error) (benchmark-test-config))])
       (and (= (length results) 1)
            (if (benchmark-result-error (car results)) #t #f))))

(mat benchmark-argument-parameterize
     (let ([results (benchmark-run (list bench-nine-args) (benchmark-test-config))])
       (and (= (length results) 8)
            (andmap (lambda (result)
                      (let ([args (benchmark-result-args result)]
                            [sample-count (length (benchmark-result-samples result))])
                        (= (cdr (assq 'sum (benchmark-result-counters result)))
                           (* sample-count (apply + (map cdr args))))))
                    results))))

(mat benchmark-reporters
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-basic) (benchmark-test-config))
                         (benchmark-text-reporter)
                         out)
       (let ([text (get-output-string out)])
         (and (string? text)
              (> (string-length text) 0))))
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-basic) (benchmark-test-config))
                         (benchmark-datum-reporter)
                         out)
       (let ([text (get-output-string out)])
         (and (string? text)
              (> (string-length text) 0)))))

(mat benchmark-text-reporter-alignment
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-basic) (benchmark-test-config))
                         (benchmark-text-reporter)
                         out)
       (let* ([lines (benchmark-string-lines (get-output-string out))]
              [header (car lines)]
              [row (cadr lines)]
              [iterations-index (benchmark-string-index header "iterations")]
              [cpu-index (benchmark-string-index header "cpu/ns")]
              [real-index (benchmark-string-index header "real/ns")]
              [bytes-index (benchmark-string-index header "bytes/iter")]
              [counters-index (benchmark-string-index header "counters")])
         (and iterations-index
              cpu-index
              real-index
              bytes-index
              counters-index
              (char-numeric? (string-ref row (+ iterations-index 9)))
              (char-numeric? (string-ref row (+ cpu-index 5)))
              (char-numeric? (string-ref row (+ real-index 6)))
              (char-numeric? (string-ref row (+ bytes-index 9)))
              (char=? (string-ref row counters-index) (integer->char 40))))))

(mat benchmark-v2-reporters
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-basic) (benchmark-test-config))
                         (benchmark-csv-reporter)
                         out)
       (let ([text (get-output-string out)])
         (and (string? text)
              (benchmark-string-contains? text "name,args,template_args")
              (benchmark-string-contains? text "bench-basic"))))
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-basic) (benchmark-test-config))
                         (benchmark-json-reporter)
                         out)
       (let ([text (get-output-string out)])
         (and (string? text)
              (benchmark-string-contains? text "\"results\"")
              (benchmark-string-contains? text "\"bench-basic\""))))
     (let ([reporter (benchmark-rich-reporter)])
       (benchmark-reporter? reporter)))

(mat benchmark-rich-reporter
     (let* ([text (call-with-string-output-port
                   (lambda (out)
                     (benchmark-report
                      (benchmark-run (list bench-basic) (benchmark-test-config))
                      (benchmark-rich-reporter)
                      out)))]
            [lines (benchmark-string-lines text)]
            ;; The default 80-column layout may wrap names across physical rows.
            [name-parts (map (lambda (line)
                               (string-trim (cadr (string-split line #\|))))
                             (filter (lambda (line)
                                       (and (positive? (string-length line))
                                            (char=? (string-ref line 0) #\|)))
                                     lines))])
       (and (benchmark-string-contains? text "Benchmark results")
            (benchmark-string-contains? text "+")
            (benchmark-string-contains? text "|")
            (string=? (apply string-append name-parts) "namebench-basicbench-basic")
            (benchmark-string-contains? text "cpu/ns")
            (benchmark-string-contains? text "counters")
            (andmap (lambda (line) (<= (string-length line) 80)) lines)))
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-basic) (benchmark-test-config))
                         (benchmark-rich-reporter
                          '((title . "Tiny")
                            (box . rounded)
                            (show-lines? . #f)
                            (padding . 0)
                            (columns . ((name . "Case")
                                        (cpu-ns . "CPU")
                                        (error . "Error")))))
                         out)
       (let ([text (get-output-string out)])
         (and (benchmark-string-contains? text "Tiny")
              (benchmark-string-contains? text "Case")
              (benchmark-string-contains? text "CPU")
              (benchmark-string-contains? text "Error")
              (benchmark-string-contains? text "bench-basic")
              (not (benchmark-string-contains? text "real/ns"))))))

(mat benchmark-v2-json-safety
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-basic) (benchmark-test-config))
                         (benchmark-json-reporter)
                         out)
       (let ([text (get-output-string out)])
         (and (string? text)
              (benchmark-json-number-safe? text))))
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-control-string) (benchmark-test-config))
                         (benchmark-json-reporter)
                         out)
       (let ([text (get-output-string out)])
         (and (string? text)
              (benchmark-json-string-control-safe? text)
              (benchmark-string-contains? text "\\b")))))

(mat benchmark-v2-baselines-and-comparison
     (let* ([results (benchmark-run (list bench-basic) (benchmark-test-config))]
            [path (benchmark-temp-file)])
       (dynamic-wind
         (lambda () (void))
         (lambda ()
           (benchmark-save-baseline results path)
           (let ([loaded (benchmark-load-baseline path)])
             (and (= (length loaded) (length results))
                  (equal? (map benchmark-result-name loaded)
                          (map benchmark-result-name results)))))
         (lambda ()
           (when (file-exists? path)
             (delete-file path)))))
     (let* ([base (benchmark-run (list bench-basic) (benchmark-test-config))]
            [current (benchmark-run (list bench-basic) (benchmark-test-config))]
            [comparisons (benchmark-compare-results base current
                           '((metric . real-ns)
                             (threshold-percent . 1000000)
                             (threshold-absolute . 1000000000)
                             (noise-threshold-percent . 1000000)))])
       (and (= (length comparisons) (length current))
            (andmap benchmark-comparison? comparisons)
            (andmap (lambda (comparison)
                      (and (benchmark-comparison-name comparison)
                           (number? (benchmark-comparison-percent-difference comparison))
                           (not (benchmark-comparison-regression? comparison))))
                    comparisons)))
     (= (benchmark-percent-difference 100 110) 10)
     (= (benchmark-absolute-difference 100 90) -10))

(mat benchmark-v2-baseline-error-roundtrip
     (let* ([result (car (benchmark-run (list bench-error) (benchmark-test-config)))]
            [path (benchmark-temp-file)])
       (dynamic-wind
         (lambda () (void))
         (lambda ()
           (benchmark-save-baseline (list result) path)
           (let ([loaded (car (benchmark-load-baseline path))])
             (and (benchmark-result-error loaded)
                  (string=? (condition-message (benchmark-result-error loaded))
                            (condition-message (benchmark-result-error result))))))
         (lambda ()
           (when (file-exists? path)
             (delete-file path)))))
     (let* ([result (car (benchmark-run (list bench-error) (benchmark-test-config)))]
            [path (benchmark-temp-file)])
       (dynamic-wind
         (lambda () (void))
         (lambda ()
           (benchmark-write-datum-file
            path
            `((format . cspp-benchmark-baseline)
              (version . 1)
              (results . (((name . legacy-error)
                            (args . ())
                            (template-args . ())
                            (samples . ())
                            (summary . ())
                            (counters . ())
                            (error? . #t))))))
           (let ([loaded (car (benchmark-load-baseline path))])
             (if (benchmark-result-error loaded) #t #f)))
         (lambda ()
           (when (file-exists? path)
             (delete-file path))))))

(mat benchmark-v2-baseline-malformed-version
     ;; malformed file data should report the library's unsupported-version error
     (let ([path (benchmark-temp-file)])
       (dynamic-wind
         (lambda () (void))
         (lambda ()
           (benchmark-write-datum-file
            path
            '((format . cspp-benchmark-baseline)
              (version . two)
              (results . ())))
           (guard (exn
                   [else
                    (benchmark-string-contains?
                     (call-with-string-output-port
                       (lambda (out)
                         (display-condition exn out)))
                     "unsupported benchmark baseline version")])
             (benchmark-load-baseline path)
             #f))
         (lambda ()
           (when (file-exists? path)
             (delete-file path))))))

(mat benchmark-v2-summaries-and-pauses
     (let* ([results (benchmark-run (list bench-basic) (benchmark-test-config))]
            [summary (benchmark-result-summary (car results))]
            [real (cdr (assq 'real-ns summary))])
       (and (assq 'confidence-interval real)
            (= (length (cdr (assq 'confidence-interval real))) 2)
            #t))
     (let* ([results (benchmark-run (list bench-paused) (benchmark-test-config))]
            [sample (car (benchmark-result-samples (car results)))])
       (and (benchmark-result? (car results))
            (not (benchmark-result-error (car results)))
            (if (benchmark-sample-sstats sample) #t #f))))
