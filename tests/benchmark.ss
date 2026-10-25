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

(define benchmark-range-rejected?
  (lambda (option)
    ((make-engine
       (lambda ()
         (guard (exn [else #t])
           (make-benchmark 'invalid-range (lambda (state value) (void)) (list option))
           #f)))
     10000
     (lambda (ticks rejected?) rejected?)
     (lambda (engine) #f))))

(mat benchmark-range-progress
     ;; Error: a geometric range starting at zero cannot advance.
     (benchmark-range-rejected? '(arg-range n 0 8 2))

     ;; Error: a geometric factor of one cannot advance.
     (benchmark-range-rejected? '(arg-range n 1 8 1))

     ;; Error: a decreasing dense range moves away from its upper endpoint.
     (benchmark-range-rejected? '(dense-arg-range n 1 8 -1))

     ;; Error: non-finite endpoints do not describe a finite range.
     (benchmark-range-rejected? '(dense-arg-range n 1 +inf.0 1))

     ;; Error: rounding must not leave an inexact range stuck on the same value.
     (benchmark-range-rejected? '(dense-arg-range n 1e30 2e30 1))

     (let ([benchmark (make-benchmark 'dense-zero (lambda (state n) (void))
                        '((dense-arg-range n 0 2 1)))])
       (equal? (map (lambda (expanded) (benchmark-state-arg (car expanded) 'n))
                    (benchmark-expand benchmark (benchmark-test-config)))
               '(0 1 2)))

     (let ([benchmark (make-benchmark 'geometric (lambda (state n) (void))
                        '((arg-range n 1 8 2)))])
       (equal? (map (lambda (expanded) (benchmark-state-arg (car expanded) 'n))
                    (benchmark-expand benchmark (benchmark-test-config)))
               '(1 2 4 8))))

(mat benchmark-sample-state-lifecycle
     (let* ([seen '()]
            [benchmark (make-benchmark 'iteration-index
                         (lambda (state)
                           (when (= (benchmark-state-iterations state) 4)
                             (set! seen (cons (benchmark-state-iteration state) seen))))
                         '())]
            [result (car (benchmark-run benchmark
                           (benchmark-config-with (benchmark-test-config)
                             '((samples . 1) (min-time . 10) (max-iterations . 4)))))])
       (and (not (benchmark-result-error result))
            (equal? (reverse seen) '(0 1 2 3))))

     (let* ([setup-state #f]
            [body-state #f]
            [cleaned? #f]
            [benchmark
             (make-benchmark 'state-lifecycle
               (lambda (state setup-value)
                 (set! body-state state)
                 (benchmark-state-set-user-data! state 'from-body))
               `((setup . ,(lambda (state)
                              (set! setup-state state)
                              (benchmark-state-set-user-data! state 'from-setup)))
                 (teardown . ,(lambda (state setup-value)
                                 (set! cleaned?
                                   (and (eq? state setup-state)
                                        (eq? state body-state)
                                        (eq? (benchmark-state-user-data state) 'from-body)))))))])
       (benchmark-run benchmark (benchmark-test-config))
       cleaned?)

     ;; Error: resources recorded by a failing body must remain available to teardown.
     (let* ([cleaned? #f]
            [benchmark
             (make-benchmark 'state-error-cleanup
               (lambda (state setup-value)
                 (benchmark-state-set-user-data! state 'resource)
                 (error 'state-error-cleanup "expected body error"))
               `((setup . ,(lambda (state) #t))
                 (teardown . ,(lambda (state setup-value)
                                 (set! cleaned?
                                   (eq? (benchmark-state-user-data state) 'resource))))))]
            [result (car (benchmark-run benchmark (benchmark-test-config)))])
       (and cleaned? (if (benchmark-result-error result) #t #f)))

     (let ([seen '()])
       (define-benchmark suite-state-child ()
         (lambda (state) (set! seen (cons (benchmark-state-suite state) seen))))
       (define-benchmark-suite suite-state-parent () suite-state-child)
       (benchmark-run-suite suite-state-parent (benchmark-test-config))
       (benchmark-run suite-state-child (benchmark-test-config))
       (equal? (reverse seen) '(suite-state-parent suite-state-parent #f #f))))

(define benchmark-roundtrip-raised
  (lambda (raised)
    (let* ([path (benchmark-temp-file)]
           [benchmark (make-benchmark 'roundtrip-raised
                        (lambda (state) (raise raised)) '())]
           [results (benchmark-run benchmark (benchmark-test-config))])
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (benchmark-save-baseline results path)
          (benchmark-result-error (car (benchmark-load-baseline path))))
        (lambda () (when (file-exists? path) (delete-file path)))))))

(define benchmark-results-from-data
  (lambda (results)
    (let ([path (benchmark-temp-file)])
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (benchmark-write-datum-file path
            `((format . cspp-benchmark-baseline) (version . 1) (results . ,results)))
          (benchmark-load-baseline path))
        (lambda () (when (file-exists? path) (delete-file path)))))))

(mat benchmark-baseline-condition-components
     ;; Error: a captured condition need not include a who component.
     (let ([condition (benchmark-roundtrip-raised
                        (condition (make-error) (make-message-condition "without who")))])
       (and (error? condition)
            (string=? (condition-message condition) "without who")))

     ;; Error: a captured condition need not include any descriptive components.
     (let ([condition (benchmark-roundtrip-raised (make-error))])
       (and (error? condition) (message-condition? condition)))

     ;; Error: arbitrary raised values must still roundtrip as captured errors.
     (condition? (benchmark-roundtrip-raised 'raised-symbol)))

(mat benchmark-comparison-measurement-availability
     ;; Error: a failed current benchmark must never count as an improvement.
     (let* ([baseline (benchmark-results-from-data
                        '(((name . compare-failure)
                           (summary . ((real-ns . ((mean . 100))))))))]
            [benchmark (make-benchmark 'compare-failure
                         (lambda (state) (error 'compare-failure "expected body error")) '())]
            [current (benchmark-run benchmark (benchmark-test-config))])
       (and (null? (benchmark-compare-results baseline current))
            (null? (benchmark-compare-results current baseline))))

     (let ([baseline (benchmark-results-from-data
                       '(((name . missing-metric) (summary . ()))))]
           [current (benchmark-results-from-data
                      '(((name . missing-metric)
                         (summary . ((real-ns . ((mean . 100))))))))])
       (and (null? (benchmark-compare-results baseline current))
            (null? (benchmark-compare-results current baseline))))

     (let ([baseline (benchmark-results-from-data
                       '(((name . zero-metric)
                          (summary . ((real-ns . ((mean . 100))))))))]
           [current (benchmark-results-from-data
                      '(((name . zero-metric)
                         (summary . ((real-ns . ((mean . 0))))))))])
       (let ([comparisons (benchmark-compare-results baseline current)])
         (and (= (length comparisons) 1)
              (benchmark-comparison-improvement? (car comparisons))))))

(mat benchmark-runner-option-precedence
     (let* ([calls 0]
            [benchmark (make-benchmark 'descriptor-defaults
                         (lambda (state) (set! calls (+ calls 1)))
                         '((warmup . 0) (samples . 3) (min-time . 0)
                           (max-iterations . 1) (reporter . #f) (output . #f)))]
            [results (benchmark-run benchmark (default-benchmark-config))])
       (and (= calls 3) (= (length (benchmark-result-samples (car results))) 3)))

     (let* ([calls 0]
            [benchmark (make-benchmark 'explicit-overrides
                         (lambda (state) (set! calls (+ calls 1)))
                         '((warmup . 3) (samples . 3) (min-time . 1)
                           (max-iterations . 4)))]
            [results (benchmark-run benchmark (benchmark-test-config))])
       (and (= calls 2) (= (length (benchmark-result-samples (car results))) 2)))

     (let* ([benchmark (make-benchmark 'partial-overrides (lambda (state) (void))
                         '((warmup . 0) (samples . 3) (min-time . 0)
                           (max-iterations . 1) (reporter . #f) (output . #f)))]
            [config (benchmark-config-with (default-benchmark-config) '((samples . 1)))]
            [results (benchmark-run benchmark config)])
       (= (length (benchmark-result-samples (car results))) 1))

     (let* ([reported 0]
            [reporter (make-benchmark-reporter
                        (lambda (results out) (set! reported (+ reported (length results))))
                        (lambda (result out) (void))
                        (lambda (results out) (void)))]
            [benchmark (make-benchmark 'descriptor-reporter (lambda (state) (void))
                         `((warmup . 0) (samples . 1) (min-time . 0)
                           (max-iterations . 1) (reporter . ,reporter)))])
       (call-with-port (open-output-string)
         (lambda (out)
           (benchmark-run benchmark
             (benchmark-config-with (default-benchmark-config) `((output . ,out))))))
       (= reported 1))

     ;; Error: config copies must reject invalid sample counts before execution.
     (guard (exn [else #t])
       (benchmark-config-with (default-benchmark-config) '((samples . -1)))
       #f)

     ;; Error: a zero iteration cap cannot produce a valid measured sample.
     (guard (exn [else #t])
       (benchmark-config-with (default-benchmark-config) '((max-iterations . 0)))
       #f))

(mat benchmark-stop-on-error
     ;; Error: stopping must prevent later descriptors from executing.
     (let* ([later-ran? #f]
            [failed (make-benchmark 'stop-first
                      (lambda (state) (error 'stop-first "expected failure")) '())]
            [later (make-benchmark 'stop-later
                     (lambda (state) (set! later-ran? #t)) '())]
            [results (benchmark-run (list failed later)
                       (benchmark-config-with (benchmark-test-config)
                         '((stop-on-error? . #t))))])
       (and (= (length results) 1) (not later-ran?)))

     ;; Error: stopping must prevent later argument cases of the same descriptor.
     (let* ([seen '()]
            [benchmark (make-benchmark 'stop-arguments
                         (lambda (state n)
                           (set! seen (cons n seen))
                           (error 'stop-arguments "expected failure"))
                         '((args (n) (1) (2))))]
            [results (benchmark-run benchmark
                       (benchmark-config-with (benchmark-test-config)
                         '((stop-on-error? . #t))))])
       (and (= (length results) 1) (equal? seen '(1))))

     ;; Error: disabled stopping must retain the existing continue-on-error behavior.
     (let* ([seen '()]
            [benchmark (make-benchmark 'continue-arguments
                         (lambda (state n)
                           (set! seen (cons n seen))
                           (error 'continue-arguments "expected failure"))
                         '((args (n) (1) (2))))]
            [results (benchmark-run benchmark (benchmark-test-config))])
       (and (= (length results) 2) (equal? (reverse seen) '(1 2)))))

(mat benchmark-config-explicit-fields
     (let* ([benchmark (make-benchmark 'constructor-override (lambda (state) (void))
                         '((samples . 3)))]
            [config (make-benchmark-config 0 1 0 1 #f #f #f #f #f)]
            [result (benchmark-run-one (car (benchmark-expand benchmark config)) config)])
       (= (length (benchmark-result-samples result)) 1))

     (let* ([benchmark (make-benchmark 'expanded-default (lambda (state) (void))
                         '((warmup . 0) (samples . 3) (min-time . 0)
                           (max-iterations . 1) (reporter . #f) (output . #f)))]
            [config (default-benchmark-config)]
            [result (benchmark-run-one (car (benchmark-expand benchmark config)) config)])
       (= (length (benchmark-result-samples result)) 3))

     ;; Error: a descriptor stop setting applies when the caller leaves it unspecified.
     (let* ([seen '()]
            [benchmark (make-benchmark 'descriptor-stop
                         (lambda (state n)
                           (set! seen (cons n seen))
                           (error 'descriptor-stop "expected failure"))
                         '((args (n) (1) (2)) (stop-on-error? . #t)))]
            [results (benchmark-run benchmark (benchmark-test-config))])
       (and (= (length results) 1) (equal? seen '(1))))

     ;; Error: an explicit false stop setting overrides the descriptor's true default.
     (let* ([benchmark (make-benchmark 'explicit-continue
                         (lambda (state n) (error 'explicit-continue "expected failure"))
                         '((args (n) (1) (2)) (stop-on-error? . #t)))]
            [results (benchmark-run benchmark
                       (benchmark-config-with (benchmark-test-config)
                         '((stop-on-error? . #f))))])
       (= (length results) 2))

     (let ([groups '()])
       (call-with-port (open-output-string)
         (lambda (out)
           (let* ([reporter (make-benchmark-reporter
                             (lambda (results out)
                               (set! groups (cons (map benchmark-result-name results) groups)))
                             (lambda (result out) (void))
                             (lambda (results out) (void)))]
                  [first (make-benchmark 'group-first (lambda (state) (void)) '())]
                  [second (make-benchmark 'group-second (lambda (state) (void)) '())])
             (benchmark-run (list first second)
               (benchmark-config-with (benchmark-test-config)
                 `((reporter . ,reporter) (output . ,out)))))))
       (equal? groups '((group-first group-second))))

     (let* ([first (make-benchmark 'select-first (lambda (state) (void)) '())]
            [second (make-benchmark 'select-second (lambda (state) (void)) '())]
            [results (benchmark-run (list first second)
                       (benchmark-config-with (benchmark-test-config)
                         '((filter . select-second))))])
       (equal? (map benchmark-result-name results) '(select-second)))

     (call-with-port (open-output-string)
       (lambda (first-port)
         (call-with-port (open-output-string)
           (lambda (second-port)
             (let ([first-config (parameterize ([current-output-port first-port])
                                   (default-benchmark-config))]
                   [second-config (parameterize ([current-output-port second-port])
                                    (default-benchmark-config))])
               (and (eq? (benchmark-config-output first-config) first-port)
                    (eq? (benchmark-config-output second-config) second-port))))))))

(mat benchmark-dangling-pause-excludes-tail
     (let* ([benchmark
             (make-benchmark 'paused-tail
               (lambda (state)
                 (benchmark-pause-timing state)
                 (benchmark-do-not-optimize (make-bytevector 1048576))
                 (sleep-milliseconds 80))
               '())]
            [result (car (benchmark-run benchmark
                           (benchmark-config-with (benchmark-test-config)
                             '((samples . 1)))))]
            [sample (car (benchmark-result-samples result))]
            [stats (benchmark-sample-sstats sample)])
       (and (not (benchmark-result-error result))
            (< (sstats-bytes stats) 4096)
            (= (time-second (sstats-real stats)) 0)
            (< (time-nanosecond (sstats-real stats)) 40000000))))

(mat benchmark-setup-dispatch-does-not-allocate-per-iteration
     (let* ([benchmark
             (make-benchmark 'setup-dispatch
               (lambda (state left right setup-value)
                 (benchmark-do-not-optimize setup-value))
               `((args (left right) (1 2))
                 (setup . ,(lambda (state) 'setup-value))))]
            [result (car (benchmark-run benchmark
                           (benchmark-config-with (benchmark-test-config)
                             '((samples . 1) (min-time . 1) (max-iterations . 4096)))))]
            [sample (car (benchmark-result-samples result))])
       (and (not (benchmark-result-error result))
            (= (benchmark-sample-iterations sample) 4096)
            (< (sstats-bytes (benchmark-sample-sstats sample)) 4096))))

(mat benchmark-registry-insertion-allocation
     (let* ([registry (current-benchmark-registry)]
            [saved (benchmark-registry-benchmarks registry)]
            [descriptor (make-benchmark 'allocation-entry (lambda (state) (void)) '())])
       (dynamic-wind
         (lambda () (benchmark-clear-registry! registry))
         (lambda ()
           (let ([before (statistics)])
             (do ([index 0 (+ index 1)])
                 ((= index 512))
               (benchmark-register! registry descriptor))
             (let ([allocated (sstats-bytes (sstats-difference (statistics) before))])
               (and (< allocated 262144)
                    (= (length (benchmark-registry-benchmarks registry)) 512)))))
         (lambda ()
           (benchmark-clear-registry! registry)
           (for-each (lambda (descriptor) (benchmark-register! registry descriptor)) saved)))))

(mat benchmark-registry-order-and-snapshots
     (let* ([registry (current-benchmark-registry)]
            [saved (benchmark-registry-benchmarks registry)]
            [first (make-benchmark 'snapshot-first (lambda (state) (void)) '())]
            [second (make-benchmark 'snapshot-second (lambda (state) (void)) '())]
            [third (make-benchmark 'snapshot-third (lambda (state) (void)) '())])
       (dynamic-wind
         (lambda () (benchmark-clear-registry! registry))
         (lambda ()
           (benchmark-register! registry first)
           (benchmark-register! registry second)
           (let ([snapshot (benchmark-registry-benchmarks registry)])
             (benchmark-register! registry third)
             (let ([after-insert (map benchmark-name (benchmark-registry-benchmarks registry))])
               (benchmark-unregister! registry 'snapshot-second)
               (let ([after-remove (map benchmark-name (benchmark-registry-benchmarks registry))])
                 (benchmark-clear-registry! registry)
                 (and (equal? (map benchmark-name snapshot) '(snapshot-first snapshot-second))
                      (equal? after-insert '(snapshot-first snapshot-second snapshot-third))
                      (equal? after-remove '(snapshot-first snapshot-third))
                      (null? (benchmark-registry-benchmarks registry)))))))
         (lambda ()
           (benchmark-clear-registry! registry)
           (for-each (lambda (descriptor) (benchmark-register! registry descriptor)) saved)))))

(mat benchmark-suite-setup-error-is-a-result
     ;; Error: failed setup must skip children and release partial suite resources.
     (let ([child-ran? #f]
           [cleanup-value 'not-cleaned]
           [cleanup-resource #f]
           [reported '()]
           [primary (condition (make-error) (make-message-condition "suite setup failed"))])
       (define-benchmark failed-suite-child ()
         (lambda (state) (set! child-ran? #t)))
       (define-benchmark-suite failed-suite
         (:suite-setup (lambda (state)
                         (benchmark-state-set-user-data! state 'partial-resource)
                         (raise primary))
          :suite-teardown (lambda (state setup-value)
                            (set! cleanup-value setup-value)
                            (set! cleanup-resource (benchmark-state-user-data state))))
         failed-suite-child)
       (call-with-port (open-output-string)
         (lambda (out)
           (let* ([reporter (make-benchmark-reporter
                             (lambda (results out) (void))
                             (lambda (result out)
                               (set! reported (cons (benchmark-result-name result) reported)))
                             (lambda (results out) (void)))]
                  [results (benchmark-run-suite failed-suite
                             (benchmark-config-with (benchmark-test-config)
                               `((output . ,out) (reporter . ,reporter))))])
             (and (= (length results) 1)
                  (eq? (benchmark-result-name (car results)) 'failed-suite)
                  (eq? (benchmark-result-error (car results)) primary)
                  (not child-ran?)
                  (eq? cleanup-value #f)
                  (eq? cleanup-resource 'partial-resource)
                  (equal? reported '(failed-suite))))))))

(mat benchmark-suite-errors-preserve-primary
     ;; Error: teardown failure must not replace the original setup failure.
     (let ([primary (condition (make-error) (make-message-condition "primary setup error"))]
           [secondary (condition (make-error) (make-message-condition "secondary cleanup error"))])
       (define-benchmark unused-suite-child () (lambda (state) (void)))
       (define-benchmark-suite double-error-suite
         (:suite-setup (lambda (state) (raise primary))
          :suite-teardown (lambda (state setup-value) (raise secondary)))
         unused-suite-child)
       (let ([results (benchmark-run-suite double-error-suite (benchmark-test-config))])
         (and (= (length results) 1)
              (eq? (benchmark-result-error (car results)) primary)))))

(mat benchmark-suite-teardown-error-retains-children
     ;; Error: failed teardown must add a suite error without discarding measured children.
     (let ([seen-value #f]
           [primary (condition (make-error) (make-message-condition "suite teardown failed"))])
       (define-benchmark measured-suite-child () (lambda (state) (void)))
       (define-benchmark-suite teardown-error-suite
         (:suite-setup (lambda (state) 'suite-resource)
          :suite-teardown (lambda (state setup-value)
                            (set! seen-value setup-value)
                            (raise primary)))
         measured-suite-child)
       (let ([results (benchmark-run-suite teardown-error-suite (benchmark-test-config))])
         (and (= (length results) 2)
              (equal? (map benchmark-result-name results)
                      '(measured-suite-child teardown-error-suite))
              (not (benchmark-result-error (car results)))
              (eq? (benchmark-result-error (cadr results)) primary)
              (eq? seen-value 'suite-resource)))))

(mat benchmark-text-reporter-shows-errors
     ;; Error: a captured failure must be visibly distinct from a zero-cost measurement.
     (let* ([benchmark (make-benchmark 'visible-error
                         (lambda (state) (error 'visible-error "body failed visibly")) '())]
            [results (benchmark-run benchmark (benchmark-test-config))])
       (call-with-port (open-output-string)
         (lambda (out)
           (benchmark-report results (benchmark-text-reporter) out)
           (let ([text (get-output-string out)])
             (and (benchmark-string-contains? text "error")
                  (benchmark-string-contains? text "body failed visibly")))))))

(mat benchmark-suite-reports-one-json-document
     ;; Error: failed suite teardown must remain in the same JSON report as its children.
     (let ()
       (define-benchmark json-suite-child () (lambda (state) (void)))
       (define-benchmark-suite json-suite
         (:suite-teardown (lambda (state setup-value)
                            (error 'json-suite "JSON teardown failure")))
         json-suite-child)
       (call-with-port (open-output-string)
         (lambda (out)
           (let* ([results (benchmark-run-suite json-suite
                             (benchmark-config-with (benchmark-test-config)
                               `((output . ,out) (reporter . ,(benchmark-json-reporter)))))]
                  [text (get-output-string out)]
                  [results-key (benchmark-string-index text "\"results\"")])
             (and (= (length results) 2)
                  results-key
                  (not (benchmark-string-contains?
                        (substring text (+ results-key 9) (string-length text)) "\"results\""))
                  (benchmark-string-contains? text "\"json-suite-child\"")
                  (benchmark-string-contains? text "\"json-suite\"")))))))

(mat benchmark-suite-report-groups-retain-descriptor-outputs
     ;; Error: reporting a teardown failure must preserve each child's effective output group.
     (call-with-port (open-output-string)
       (lambda (first-out)
         (call-with-port (open-output-string)
           (lambda (second-out)
             (let* ([groups '()]
                    [reporter (make-benchmark-reporter
                                (lambda (results out)
                                  (set! groups
                                    (cons (cons out (map benchmark-result-name results)) groups)))
                                (lambda (result out) (void))
                                (lambda (results out) (void)))])
               (define-benchmark first-output-child () (lambda (state) (void)))
               (define-benchmark second-output-child (:output second-out)
                 (lambda (state) (void)))
               (define-benchmark-suite output-groups-suite
                 (:suite-teardown (lambda (state setup-value)
                                    (error 'output-groups-suite "group teardown failure")))
                 first-output-child second-output-child)
               (let ([config (parameterize ([current-output-port first-out])
                               (benchmark-config-with (default-benchmark-config)
                                 `((warmup . 0) (samples . 1) (min-time . 0)
                                   (max-iterations . 1) (reporter . ,reporter))))])
                 (benchmark-run-suite output-groups-suite config)
                 (and (= (length groups) 2)
                      (equal? (cdr (assq first-out groups))
                              '(first-output-child output-groups-suite))
                      (equal? (cdr (assq second-out groups)) '(second-output-child))))))))))

(mat benchmark-cleanup-preserves-primary-condition
     ;; Error: teardown must not replace a primary body exception.
     (let* ([primary (condition (make-error) (make-message-condition "primary"))]
            [cleaned? #f]
            [benchmark (make-benchmark 'double-error
                         (lambda (state value) (raise primary))
                         `((setup . ,(lambda (state) 'resource))
                           (teardown . ,(lambda (state value)
                                          (set! cleaned? #t)
                                          (error 'cleanup "secondary")))))]
            [result (car (benchmark-run benchmark (benchmark-test-config)))])
       (and cleaned? (eq? (benchmark-result-error result) primary)))

     ;; Error: teardown alone must still produce an errored benchmark result.
     (let* ([benchmark (make-benchmark 'cleanup-error (lambda (state value) (void))
                         `((setup . ,(lambda (state) 'resource))
                           (teardown . ,(lambda (state value) (error 'cleanup "secondary")))))]
            [result (car (benchmark-run benchmark (benchmark-test-config)))])
       (and (benchmark-result-error result)
            (eq? (condition-who (benchmark-result-error result)) 'cleanup))))

(mat benchmark-false-raised-values
     ;; Error: false raised objects must stop runs and survive baseline serialization.
     (let* ([later? #f]
            [benchmark (make-benchmark 'false-error (lambda (state) (raise #f)) '())]
            [later (make-benchmark 'later (lambda (state) (set! later? #t)) '())]
            [results (benchmark-run (list benchmark later)
                       (benchmark-config-with (benchmark-test-config)
                         '((stop-on-error? . #t))))])
       (and (= (length results) 1) (not later?)
            (condition? (benchmark-result-error (car results)))
            (null? (benchmark-compare-results results results))
            (condition? (benchmark-roundtrip-raised #f))))

     ;; Error: false suite setup errors still skip children and run cleanup with false.
     (let ([called? #f] [cleaned? #f])
       (define-benchmark false-suite-child () (lambda (state) (set! called? #t)))
       (define-benchmark-suite false-setup-suite
         (:suite-setup (lambda (state) (raise #f))
          :suite-teardown (lambda (state value) (set! cleaned? (eq? value #f))))
         false-suite-child)
       (let ([results (benchmark-run-suite false-setup-suite (benchmark-test-config))])
         (and (= (length results) 1) (not called?) cleaned?
              (condition? (benchmark-result-error (car results))))))

     ;; Error: false teardown errors must append a suite error after measured children.
     (let ()
       (define-benchmark false-teardown-child () (lambda (state) (void)))
       (define-benchmark-suite false-teardown-suite
         (:suite-teardown (lambda (state value) (raise #f))) false-teardown-child)
       (let ([results (benchmark-run-suite false-teardown-suite (benchmark-test-config))])
         (and (= (length results) 2)
              (not (benchmark-result-error (car results)))
              (condition? (benchmark-result-error (cadr results)))))))

(mat benchmark-argument-object-identity
     (let* ([first (vector 1)] [second (vector 1)] [seen '()]
            [benchmark (make-benchmark 'identity
                         (lambda (state value)
                           (set! seen
                             (cons (and (eq? value (benchmark-state-arg state 'value))
                                        value) seen)))
                         `((args . ((value) (,first) (,second)))))]
            [results (benchmark-run benchmark (benchmark-test-config))])
       (and (for-all (lambda (result) (not (benchmark-result-error result))) results)
            (equal? (map (lambda (value) (eq? value first)) (reverse seen)) '(#t #t #f #f))
            (eq? (car seen) second)))

     ;; The low-level runner must use the concrete state's values too.
     (let* ([first (vector 1)] [second (vector 1)] [seen #f]
            [benchmark (make-benchmark 'identity-one
                         (lambda (state value) (set! seen value))
                         `((args . ((value) (,first) (,second)))))]
            [expanded (benchmark-expand benchmark (benchmark-test-config))]
            [result (benchmark-run-one (cadr expanded) (benchmark-test-config))])
       (and (not (benchmark-result-error result)) (eq? seen second))))

(mat benchmark-comparison-excludes-unmeasured-runs
     ;; An explicit zero-sample run must not look like an allocation improvement.
     (let* ([baseline (benchmark-results-from-data
                        '(((name . unmeasured)
                           (summary . ((bytes . ((mean . 100))))))))]
            [benchmark (make-benchmark 'unmeasured (lambda (state) (void)) '())]
            [current (benchmark-run benchmark
                       (benchmark-config-with (benchmark-test-config) '((samples . 0))))]
            [path (benchmark-temp-file)])
       (dynamic-wind
         (lambda () (void))
         (lambda ()
           (benchmark-save-baseline current path)
           (and (null? (benchmark-compare-results baseline current '((metric . bytes))))
                (null? (benchmark-compare-results current baseline '((metric . bytes))))
                (null? (benchmark-compare-results baseline (benchmark-load-baseline path)
                         '((metric . bytes))))))
         (lambda () (when (file-exists? path) (delete-file path)))))

     ;; Summary-only historical data must stay comparable through save/load.
     (let* ([results (benchmark-results-from-data
                       '(((name . historical)
                          (summary . ((bytes . ((mean . 0))))))))]
            [path (benchmark-temp-file)])
       (dynamic-wind
         (lambda () (void))
         (lambda ()
           (benchmark-save-baseline results path)
           (= (length (benchmark-compare-results results (benchmark-load-baseline path)
                        '((metric . bytes)))) 1))
         (lambda () (when (file-exists? path) (delete-file path)))))

     ;; A sample that actually measured zero remains a valid comparison.
     (let ([results (benchmark-results-from-data
                      '(((name . measured-zero)
                         (samples . (((iterations . 1) (bytes . 0))))
                         (summary . ((bytes . ((mean . 0))))))))])
       (= (length (benchmark-compare-results results results '((metric . bytes)))) 1)))

(mat benchmark-summary-preserves-fractions
     (let* ([results (benchmark-results-from-data
                       '(((name . fractional)
                          (samples . (((iterations . 4) (cpu-ns . 6)
                                       (real-ns . 7) (bytes . 2))
                                      ((iterations . 2) (cpu-ns . 5)
                                       (real-ns . 5) (bytes . 3)))))))]
            [summary (benchmark-summarize (benchmark-result-samples (car results)))]
            [bytes (cdr (assq 'bytes summary))])
       (and (= (cdr (assq 'mean (cdr (assq 'cpu-ns summary)))) 2)
            (= (cdr (assq 'mean (cdr (assq 'real-ns summary)))) 17/8)
            (= (cdr (assq 'mean bytes)) 1)
            (= (cdr (assq 'median bytes)) 1)
            (= (cdr (assq 'min bytes)) 1/2)
            (= (cdr (assq 'max bytes)) 3/2))))

(mat benchmark-setup-pause-does-not-subtract-body
     (for-all
       (lambda (leave-paused?)
         (let* ([benchmark (make-benchmark 'setup-paused
                              (lambda (state value)
                                (when leave-paused? (benchmark-resume-timing state))
                                (milisleep 10))
                              `((setup . ,(lambda (state)
                                            (benchmark-pause-timing state)
                                            (milisleep 30)
                                            (unless leave-paused? (benchmark-resume-timing state))
                                            'ready))))]
                [result (car (benchmark-run benchmark
                               (benchmark-config-with (benchmark-test-config) '((samples . 1)))))]
                [real (cdr (assq 'mean (cdr (assq 'real-ns (benchmark-result-summary result)))))])
           (and (not (benchmark-result-error result)) (> real 5000000))))
       '(#f #t)))

(mat benchmark-baseline-rejects-unreadable-keys-before-replacement
     ;; Error: procedure-valued argument keys cannot be persisted as readable Scheme data.
     (for-all
       (lambda (options)
         (let* ([path (benchmark-temp-file)]
                [benchmark (make-benchmark 'unreadable (lambda arguments (void)) options)]
                [results (benchmark-run benchmark (benchmark-test-config))])
           (dynamic-wind
             (lambda () (benchmark-write-datum-file path 'original))
             (lambda ()
               (let ([rejected? (guard (raised [else #t])
                                  (benchmark-save-baseline results path) #f)])
                 (and rejected? (eq? (call-with-input-file path read) 'original))))
             (lambda () (when (file-exists? path) (delete-file path))))))
       (list `((args . ((callback) (,make-vector))))
             `((template-args . ((callback . ,make-vector)))))))

(mat benchmark-baseline-unreadable-irritants
     ;; Error: unreadable diagnostic irritants must not corrupt an otherwise usable baseline.
     (let* ([path (benchmark-temp-file)]
            [benchmark (make-benchmark 'diagnostic
                         (lambda (state) (error 'diagnostic "bad callback ~s" make-vector)) '())]
            [results (benchmark-run benchmark (benchmark-test-config))])
       (dynamic-wind
         (lambda () (void))
         (lambda ()
           (benchmark-save-baseline results path)
           (let ([raised (benchmark-result-error (car (benchmark-load-baseline path)))])
             (and (condition? raised)
                  (string? (car (condition-irritants raised))))))
         (lambda () (when (file-exists? path) (delete-file path))))))

(mat benchmark-text-identifies-arguments
     (let* ([results (benchmark-results-from-data
                       '(((name . sized) (args . ((size . 1))) (template-args . ((kind . vector))))))])
       (call-with-port (open-output-string)
         (lambda (out)
           (benchmark-report results (benchmark-text-reporter) out)
           (let ([text (get-output-string out)])
             (and (benchmark-string-contains? text "size")
                  (benchmark-string-contains? text "kind")
                  (benchmark-string-contains? text "vector")))))))

(mat benchmark-comparison-index-preserves-matching
     (let* ([baseline
             (benchmark-results-from-data
               (map (lambda (number mean)
                      `((name . case) (args . ((n . ,number)))
                        (summary . ((real-ns . ((mean . ,mean)))))))
                    '(1 1 2) '(10 99 20)))]
            [current (reverse (list (car baseline) (caddr baseline)))]
            [comparisons (benchmark-compare-results baseline current)])
       (and (equal? (map benchmark-comparison-baseline-value comparisons) '(20 10))
            (equal? (map benchmark-comparison-current comparisons) current))))

(mat benchmark-baseline-unreadable-who
     ;; Error: an unreadable diagnostic who field must not prevent baseline saving.
     (let* ([path (benchmark-temp-file)]
            [raised (condition (make-error) (make-who-condition make-vector)
                               (make-message-condition "unreadable who"))]
            [benchmark (make-benchmark 'diagnostic-who (lambda (state) (raise raised)) '())]
            [results (benchmark-run benchmark (benchmark-test-config))])
       (dynamic-wind
         (lambda () (void))
         (lambda ()
           (benchmark-save-baseline results path)
           (let ([loaded (benchmark-result-error (car (benchmark-load-baseline path)))])
             (and (condition? loaded) (string? (condition-who loaded)))))
         (lambda () (when (file-exists? path) (delete-file path))))))

(mat benchmark-rich-identifies-template-arguments
     (let ([results (benchmark-results-from-data
                      '(((name . template) (template-args . ((kind . vector))))))])
       (call-with-port (open-output-string)
         (lambda (out)
           (benchmark-report results (benchmark-rich-reporter) out)
           (let ([text (get-output-string out)])
             (and (benchmark-string-contains? text "template-args")
                  (benchmark-string-contains? text "kind")
                  (benchmark-string-contains? text "vector")))))))

(mat benchmark-exact-min-time-respects-iteration-cap
     ;; Finite exact targets must not overflow through an intermediate flonum.
     (let* ([benchmark (make-benchmark 'exact-target (lambda (state) (void)) '())]
            [result (car (benchmark-run benchmark
                           (benchmark-config-with (benchmark-test-config)
                             `((samples . 1) (max-iterations . 2)
                               (min-time . ,(expt 10 400))))))])
       (and (not (benchmark-result-error result))
            (= (benchmark-sample-iterations (car (benchmark-result-samples result))) 2))))

(mat benchmark-counters-update-without-repeated-allocation
     (let ([state (benchmark-state 'counter #f '() '() 0 1 0)])
       (benchmark-state-counter-set! state 'calls 0)
       (collect)
       (let ([before (statistics)])
         (do ([index 0 (fx+ index 1)]) [(fx= index 20000)]
           (benchmark-state-counter-add! state 'calls 1))
         ;; Counter updates must not add per-iteration list allocations to measured work.
         (< (sstats-bytes (sstats-difference (statistics) before)) 65536))))

(mat benchmark-counters-snapshot-before-cleanup
     (let* ([states '()]
            [benchmark
             (make-benchmark 'counter-snapshot
               (lambda (state value)
                 (benchmark-state-counter-add! state 'calls 1)
                 (benchmark-state-counter-set! state 'other 3)
                 (benchmark-state-counter-set! state 'other 4))
               `((setup . ,(lambda (state)
                             (set! states (cons state states))
                             (benchmark-state-counter-set! state 'calls 1)))
                 (teardown . ,(lambda (state value)
                                (benchmark-state-counter-set! state 'calls 1000)))))]
            [result (car (benchmark-run benchmark (benchmark-test-config)))])
       (for-each (lambda (state) (benchmark-state-counter-add! state 'calls 1000)) states)
       (and (not (benchmark-result-error result))
            (for-all (lambda (sample)
                       (let ([counters (benchmark-sample-counters sample)])
                         (and (= (length counters) 2)
                              (= (cdr (assq 'calls counters)) 2)
                              (= (cdr (assq 'other counters)) 4))))
                     (benchmark-result-samples result))
            (= (cdr (assq 'calls (benchmark-result-counters result))) 4)
            (= (cdr (assq 'other (benchmark-result-counters result))) 8))))

(mat benchmark-json-terminates-on-cyclic-arguments
     ;; Cycles through keys, object values, or list tails must not hang JSON reporting.
     (let ([key-cycle (list #f)]
           [value-cycle (list (cons 'self #f))]
           [tail-cycle (list 1)])
       (set-car! key-cycle key-cycle)
       (set-cdr! (car value-cycle) value-cycle)
       (set-cdr! tail-cycle tail-cycle)
       (for-all
         (lambda (value)
           (let* ([benchmark (make-benchmark 'cyclic (lambda (state value) (void))
                              `((args (value) (,value))))]
                  [results (benchmark-run benchmark (benchmark-test-config))])
             (call-with-port (open-output-string)
               (lambda (out)
                 (parameterize ([print-graph #f])
                   (benchmark-report results (benchmark-json-reporter) out))
                 (let ([text (get-output-string out)])
                   (and (json5-document? (parse-json5 text))
                        (benchmark-string-contains? text "#0=")))))))
         (list key-cycle value-cycle tail-cycle))))

;; These fixtures catch lost population deviation, wrong medians, and interval endpoints.
(mat benchmark-summary-known-values
     (for-all
       (lambda (row)
         (let* ([samples (map (lambda (value) `((iterations . 1) (bytes . ,value))) (car row))]
                [result (car (benchmark-results-from-data
                               `(((name . statistics) (samples . ,samples)))))]
                [summary (cdr (assq 'bytes
                           (benchmark-summarize (benchmark-result-samples result))))])
           (and (equal? (map (lambda (key) (cdr (assq key summary)))
                            '(median mean min max stddev))
                        (cadr row))
                (for-all (lambda (actual expected) (< (abs (- actual expected)) 1e-9))
                         (cdr (assq 'confidence-interval summary)) (caddr row)))))
       '((() (0 0 0 0 0) (0 0))
         ((4) (4 4 4 4 0) (4 4))
         ((0 2) (1 1 0 2 1) (-0.3859292911256331 2.385929291125633))
         ((0 3 6) (3 3 0 6 2.449489742783178) (0.22814141774873375 5.771858582251266))
         ((0 0 2 2) (1 1 0 2 1) (0.02 1.98)))))

;; Each gate must admit the boundary itself and reject values just below it.
(mat benchmark-comparison-threshold-tables
     (for-all
       (lambda (metric)
         (for-all
           (lambda (row)
             (let* ([base (benchmark-results-from-data
                            `(((name . threshold) (summary . ((,metric . ((mean . ,(car row)))))))))]
                    [current (benchmark-results-from-data
                               `(((name . threshold) (summary . ((,metric . ((mean . ,(cadr row)))))))))]
                    [comparison (car (benchmark-compare-results base current
                                       `((metric . ,metric) ,@(caddr row))))])
               (and (eq? (benchmark-comparison-regression? comparison) (cadddr row))
                    (eq? (benchmark-comparison-improvement? comparison) (list-ref row 4))
                    (= (benchmark-comparison-absolute-difference comparison) (list-ref row 5))
                    (= (benchmark-comparison-percent-difference comparison) (list-ref row 6)))))
           '((100 104 () #f #f 4 4)
             (100 105 () #t #f 5 5)
             (100 106 () #t #f 6 6)
             (100 95 () #f #t -5 -5)
             (100 96 () #f #f -4 -4)
             (100 110 ((threshold-absolute . 11)) #f #f 10 10)
             (100 110 ((threshold-absolute . 10)) #t #f 10 10)
             (100 110 ((noise-threshold-percent . 11)) #f #f 10 10)
             (100 110 ((noise-threshold-percent . 10)) #t #f 10 10)
             (100 90 ((noise-threshold-percent . 11)) #f #f -10 -10)
             (100 90 ((threshold-absolute . 11)) #f #f -10 -10)
             (0 0 () #f #f 0 0)
             (0 1 () #t #f 1 +inf.0)
             (1 0 () #f #t -1 -100))))
       '(cpu-ns real-ns bytes)))

(mat benchmark-calibration-stops-at-target
     (let* ([seen '()]
            [benchmark (make-benchmark 'target-reached
                         (lambda (state)
                           (set! seen (cons (benchmark-state-iterations state) seen))
                           (milisleep 2)) '())]
            [result (car (benchmark-run benchmark
                           (benchmark-config-with (benchmark-test-config)
                             '((samples . 2) (min-time . 0.0001) (max-iterations . 8)))))])
       (and (not (benchmark-result-error result))
            (equal? seen '(1 1))
            (equal? (map benchmark-sample-iterations (benchmark-result-samples result)) '(1 1)))))

(mat benchmark-baseline-rejects-malformed-models
     ;; Error: decoded records must reject invalid domains instead of clamping or normalizing them.
     (for-all
       (lambda (datum)
         (guard (raised [else
                         (and (who-condition? raised)
                              (eq? (condition-who raised) 'benchmark-load-baseline))])
           (benchmark-results-from-data (list datum))
           #f))
       '(((name . malformed) (samples . (((iterations . -1) (bytes . -100)))))
         ((name . malformed) (samples . (((iterations . 1) (cpu-ns . -1)))))
         ((name . malformed) (samples . (((iterations . 1) (real-ns . 1/2)))))
         ((name . malformed) (samples . (((iterations . 1) (gc-count . 1.0)))))
         ((name . malformed) (samples . (((iterations . 1) (bytes . many)))))
         ((name . malformed) (samples . (((iterations . 1) (counters . ((n . text)))))))
         ((name . malformed) (samples . (((iterations . 1) (bytes . 0) (bytes . 1)))))
         ((name . malformed) (samples . (((iterations . 1) (cost-center . not-an-alist)))))
         ((name . malformed) (samples . (not-a-sample)))
         ((name . malformed) (samples . not-a-list))
         ((name . malformed) (args . ((1 . value))))
         ((name . malformed) (template-args . ((n . 1) (n . 2))))
         ((name . malformed) (summary . ((bytes . ((mean . text))))))
         ((name . malformed) (summary . ((bytes . ((confidence-interval . (1)))))))
         ((name . malformed) (summary . ((bytes . ((stddev . -1))))))
         ((name . malformed) (counters . ((n . text))))
         ((name . malformed) (error . ((message . 1))))
         ((name . malformed) (error . ((irritants . improper))))
         ((name . "not-symbolic"))
         ((samples . ()))
         ((name . malformed) . improper)
         not-a-result)))

(mat benchmark-baseline-semantic-roundtrip
     (let* ([data '(((name . semantic) (args . ((n . 3))) (template-args . ((kind . list)))
                     (samples . (((iterations . 2) (cpu-ns . 6) (real-ns . 8) (bytes . 10)
                                  (gc-count . 1) (gc-cpu-ns . 2) (gc-real-ns . 4)
                                  (gc-bytes . 6) (counters . ((work . 7))))))
                     (summary . ((bytes . ((mean . 5))))) (counters . ((work . 7)))))]
            [before (car (benchmark-results-from-data data))]
            [path (benchmark-temp-file)])
       (dynamic-wind
         (lambda () (void))
         (lambda ()
           (benchmark-save-baseline (list before) path)
           (let* ([after (car (benchmark-load-baseline path))]
                  [sample (car (benchmark-result-samples after))]
                  [stats (benchmark-sample-sstats sample)])
             (and (equal? (benchmark-result-args after) '((n . 3)))
                  (equal? (benchmark-result-template-args after) '((kind . list)))
                  (equal? (benchmark-result-summary after) (benchmark-result-summary before))
                  (equal? (benchmark-result-counters after) '((work . 7)))
                  (equal? (benchmark-sample-counters sample) '((work . 7)))
                  (= (benchmark-sample-iterations sample) 2)
                  (= (sstats-bytes stats) 10) (= (sstats-gc-count stats) 1)
                  (= (sstats-gc-bytes stats) 6)
                  (= (time-nanosecond (sstats-cpu stats)) 6)
                  (= (time-nanosecond (sstats-real stats)) 8)
                  (= (time-nanosecond (sstats-gc-cpu stats)) 2)
                  (= (time-nanosecond (sstats-gc-real stats)) 4))))
         (lambda () (when (file-exists? path) (delete-file path))))))

(mat benchmark-baseline-rejects-malformed-envelopes
     ;; Error: valid reader data is not necessarily a baseline envelope or result collection.
     (for-all
       (lambda (datum)
         (let ([path (benchmark-temp-file)])
           (dynamic-wind
             (lambda () (void))
             (lambda ()
               (benchmark-write-datum-file path datum)
               (guard (raised [else (and (who-condition? raised)
                                        (eq? (condition-who raised) 'benchmark-load-baseline))])
                 (benchmark-load-baseline path)
                 #f))
             (lambda () (when (file-exists? path) (delete-file path))))))
       '(not-an-envelope
         ((format . cspp-benchmark-baseline) . improper)
         ((format . cspp-benchmark-baseline) (version . 1) (results . improper))
         ((format . cspp-benchmark-baseline) (version . 1) (results . (result)))
         ((format . cspp-benchmark-baseline) (version . 2) (results . ()))
         ((format . another-format) (version . 1) (results . ())))))

(mat benchmark-constructor-validates-option-context
     ;; Error: misspelled, unconsumed and malformed options must fail before a descriptor can run.
     (for-all
       (lambda (options)
         (guard (raised [else #t])
           (make-benchmark 'bad-options (lambda (state) (void)) options)
           #f))
       '(((sampls . 1))
         ((suite-setup . #f))
         (samples)
         ((1 . 2))
         ((setup . 1))
         ((teardown . invalid))
         ((fixture . invalid))
         ((suite . 42))
         ((template-args . not-an-alist))
         ((template-args . ((n . 1) (n . 2))))
         ((arg-range 42 1 1 2))
         ((dense-arg-range 42 1 1 1))
         ((arg-range n 1 1 2 ignored))
         ((dense-arg-range n 1 1))
         ((samples . -1))
         ((warmup . invalid))
         ((max-iterations . 0))
         ((min-time . +inf.0))
         ((output . invalid))
         ((filter . 42))
         ((reporter . text))
         ((cost-center? . yes))
         ((stop-on-error? . yes))))

     ;; Error: suites and fixtures reject options they cannot consume, and non-procedure hooks.
     (for-all
       (lambda (form)
         (guard (raised [else #t])
           (eval form (environment '(cspp)))
           #f))
       '((let () (define-benchmark-suite unused (:samples 0)) unused)
         (let () (define-benchmark-suite unused (:args ([n] [1]))) unused)
         (let () (define-benchmark-suite unused (:setup (lambda (state) #t))) unused)
         (let () (define-benchmark-suite invalid (:suite-setup #f)) invalid)
         (let () (define-benchmark-suite invalid (:suite-teardown 1)) invalid)
         (let () (define-benchmark-fixture unused (:samples 1)) unused)
         (let () (define-benchmark-fixture unused (:suite-setup (lambda (state) #t))) unused)
         (let () (define-benchmark-fixture invalid (:setup #f)) invalid)
         (let () (define-benchmark-fixture invalid (:teardown 1)) invalid))))


(mat benchmark-inexact-iteration-cap-baseline-roundtrip
     ;; Natural-number API arguments may be represented as inexact integral values.
     (let* ([benchmark (make-benchmark 'inexact-cap (lambda (state) (void)) '())]
            [results (benchmark-run benchmark
                       (benchmark-config-with (benchmark-test-config)
                         '((samples . 1) (min-time . 1e100) (max-iterations . 3.0))))]
            [path (benchmark-temp-file)])
       (dynamic-wind
         (lambda () (void))
         (lambda ()
           (benchmark-save-baseline results path)
           (let* ([loaded (car (benchmark-load-baseline path))]
                  [sample (car (benchmark-result-samples loaded))])
             (and (= (benchmark-sample-iterations sample) 3)
                  (equal? (benchmark-result-summary loaded)
                          (benchmark-result-summary (car results))))))
         (lambda () (when (file-exists? path) (delete-file path))))))

(mat benchmark-csv-reports-partial-legacy-summaries
     ;; Error: accepted legacy summaries may omit metrics or individual mean fields.
     (let* ([results (benchmark-results-from-data
                      '(((name . real-only) (summary . ((real-ns . ((mean . 100))))))
                        ((name . no-summary))
                        ((name . no-means)
                         (summary . ((cpu-ns . ((median . 3))) (real-ns . ())
                                     (bytes . ((mean . 7))))))))]
            [text (call-with-string-output-port
                    (lambda (out) (benchmark-report results (benchmark-csv-reporter) out)))])
       (equal? (benchmark-string-lines text)
               '("name,args,template_args,cpu_ns_mean,real_ns_mean,bytes_mean,counters,error"
                 "\"real-only\",\"()\",\"()\",\"0\",\"100\",\"0\",\"()\",\"#f\""
                 "\"no-summary\",\"()\",\"()\",\"0\",\"0\",\"0\",\"()\",\"#f\""
                 "\"no-means\",\"()\",\"()\",\"0\",\"0\",\"7\",\"()\",\"#f\""))))

(mat benchmark-datum-reports-ignore-printer-limits
     ;; Error: caller printer limits must not silently discard result fields or nested data.
     (let* ([results (benchmark-results-from-data
                      '(((name . complete) (args . ((n . (1 2 3))))
                         (summary . ((real-ns . ((mean . 100))))))))]
            [reporter (benchmark-datum-reporter)]
            [expected (call-with-string-output-port
                        (lambda (out)
                          (parameterize ([print-length #f] [print-level #f])
                            (benchmark-report results reporter out))))])
       (for-all
         (lambda (limits)
           (let ([text (call-with-string-output-port
                         (lambda (out)
                           (parameterize ([print-length (car limits)]
                                          [print-level (cadr limits)])
                             (benchmark-report results reporter out))))])
             (equal? text expected)))
         '((1 #f) (#f 1) (1 1)))))

(mat benchmark-datum-reports-cyclic-arguments-without-warnings
     ;; Error: readable cyclic arguments should roundtrip without printer warnings.
     (let ([cycle (list #f)] [warnings 0])
       (set-car! cycle cycle)
       (let* ([benchmark (make-benchmark 'cyclic-datum (lambda (state value) (void))
                          `((args (value) (,cycle))))]
              [results (benchmark-run benchmark (benchmark-test-config))]
              [text (call-with-string-output-port
                      (lambda (out)
                        (parameterize ([print-graph #f])
                          (with-exception-handler
                            (lambda (raised)
                              (if (warning? raised)
                                  (set! warnings (+ warnings 1))
                                  (raise raised)))
                            (lambda ()
                              (benchmark-report results (benchmark-datum-reporter) out))))))])
         (call-with-port (open-input-string text)
           (lambda (in)
             (let* ([datum (read in)]
                    [value (cdr (assq 'value (cdr (assq 'args datum))))])
               (and (= warnings 0)
                    (eq? value (car value))
                    (pair? (assq 'summary datum))
                    (eof-object? (read in)))))))))

(mat benchmark-json-preserves-subnormal-numbers
     ;; Error: Chez precision suffixes are not JSON and subnormal values must remain numeric.
     (let ([object-ref
            (lambda (object name)
              (json5-member-value
                (find (lambda (member) (string=? name (json5-member-name member)))
                      (vector->list (json5-object-members object)))))])
       (for-all
         (lambda (value)
           (let* ([benchmark (make-benchmark 'subnormal-json
                              (lambda (state argument)
                                (benchmark-state-counter-set! state 'value argument))
                              `((args (value) (,value))))]
                  [results (benchmark-run benchmark
                             (benchmark-config-with (benchmark-test-config) '((samples . 1))))])
             (for-all
               (lambda (precision)
                 (let* ([text (call-with-string-output-port
                                (lambda (out)
                                  (parameterize ([print-precision precision]
                                                 [print-subnormal-precision #t])
                                    (benchmark-report results (benchmark-json-reporter) out))))]
                        [document (parse-json5 text)]
                        [result (vector-ref
                                  (json5-array-elements
                                    (object-ref (json5-document-value document) "results")) 0)])
                   (and (= (object-ref (object-ref result "args") "value") value)
                        (= (object-ref (object-ref result "counters") "value") value))))
               '(#f 53))))
         '(1e-310 -1e-310 5e-324 -5e-324))))

(mat benchmark-comparison-rejects-invalid-thresholds
     ;; Error: NaN and other invalid thresholds must reject even when no results match.
     (let ([baseline (benchmark-results-from-data
                       '(((name . threshold-validation)
                          (summary . ((real-ns . ((mean . 100))))))))]
           [current (benchmark-results-from-data
                      '(((name . threshold-validation)
                         (summary . ((real-ns . ((mean . 200))))))))])
       (for-all
         (lambda (key)
           (for-all
             (lambda (value)
               (for-all
                 (lambda (results)
                   (guard (raised
                            [else (and (who-condition? raised)
                                       (eq? (condition-who raised) 'benchmark-compare-results)
                                       (message-condition? raised)
                                       (benchmark-string-contains?
                                         (call-with-string-output-port
                                           (lambda (out) (display-condition raised out)))
                                         (symbol->string key)))])
                     (benchmark-compare-results (car results) (cadr results)
                       (list (cons key value)))
                     #f))
                 (list (list baseline current) (list '() '()))))
             '(+nan.0 +inf.0 -inf.0 -1 -1/2 -0.5 1+2i #f invalid)))
         '(threshold-percent threshold-absolute noise-threshold-percent))))

(mat benchmark-comparison-accepts-finite-real-thresholds
     (let ([baseline (benchmark-results-from-data
                       '(((name . real-threshold)
                          (summary . ((real-ns . ((mean . 100))))))))]
           [current (benchmark-results-from-data
                      '(((name . real-threshold)
                         (summary . ((real-ns . ((mean . 110))))))))])
       (for-all
         (lambda (key)
           (for-all
             (lambda (row)
               (let ([comparison (car (benchmark-compare-results baseline current
                                       (list (cons key (car row)))))])
                 (eq? (benchmark-comparison-regression? comparison) (cdr row))))
             (list (cons 0 #t) (cons 1/2 #t) (cons 0.5 #t) (cons (expt 10 400) #f))))
         '(threshold-percent threshold-absolute noise-threshold-percent))))

(mat benchmark-calibration-growth-and-retained-samples
     ;; An unreachable target makes growth deterministic without replacing the measurement backend.
     (let* ([setups '()] [calls 0]
            [benchmark
             (make-benchmark 'calibration-growth
               (lambda (state setup-value)
                 (set! calls (+ calls 1))
                 (benchmark-state-counter-add! state 'work 1))
               `((setup . ,(lambda (state)
                              (set! setups
                                (cons (list (benchmark-state-sample state)
                                            (benchmark-state-iterations state)) setups))
                              'ready))))]
            [result (car (benchmark-run benchmark
                           (benchmark-config-with (benchmark-test-config)
                             `((warmup . 2) (samples . 2) (max-iterations . 5)
                               (min-time . ,(expt 10 400))))))]
            [samples (benchmark-result-samples result)])
       (and (not (benchmark-result-error result))
            (equal? (reverse setups)
                    '((0 1) (0 1)
                      (0 1) (0 2) (0 4) (0 5)
                      (1 1) (1 2) (1 4) (1 5)))
            (= calls 26)
            (equal? (map benchmark-sample-iterations samples) '(5 5))
            (equal? (map benchmark-sample-counters samples) '(((work . 5)) ((work . 5)))))))
