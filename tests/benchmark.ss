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

(mat benchmark-standalone-teardown
  ;; Cleanup-only hooks receive no setup value and run after successful and failing bodies.
  (for-all
    (lambda (raises?)
      (let ([events '()])
        (let* ([benchmark
                (make-benchmark 'cleanup-only
                  (lambda (state)
                    (benchmark-state-set-user-data! state 'acquired)
                    (set! events (cons 'body events))
                    (when raises? (error 'cleanup-only "body error")))
                  `((setup . #f)
                    (teardown . ,(lambda (state value)
                                   (set! events
                                     (cons (list 'cleanup value (benchmark-state-user-data state))
                                           events))))))]
               [result (car (benchmark-run benchmark
                              (benchmark-config-with (benchmark-test-config)
                                '((samples . 1)))))])
          (and (equal? (reverse events) '(body (cleanup #f acquired)))
               (if raises?
                   (and (benchmark-result-error result)
                        (eq? (condition-who (benchmark-result-error result)) 'cleanup-only))
                   (not (benchmark-result-error result)))))))
    '(#f #t))

  ;; Error: a teardown error is reported when there is no primary body failure.
  (let* ([benchmark
          (make-benchmark 'cleanup-error (lambda (state) (void))
            `((teardown . ,(lambda (state value) (error 'cleanup "cleanup error")))))]
         [result (car (benchmark-run benchmark (benchmark-test-config)))])
    (and (benchmark-result-error result)
         (eq? (condition-who (benchmark-result-error result)) 'cleanup)))

  ;; Error: a cleanup error must not replace a primary body failure.
  (let* ([benchmark
          (make-benchmark 'primary-error (lambda (state) (error 'body "primary"))
            `((teardown . ,(lambda (state value) (error 'cleanup "secondary")))))]
         [result (car (benchmark-run benchmark (benchmark-test-config)))])
    (and (benchmark-result-error result)
         (eq? (condition-who (benchmark-result-error result)) 'body))))

(mat benchmark-fixture-hook-conflicts
  ;; Error: descriptor hooks must not be silently ignored when a fixture supplies the lifecycle.
  (let ([calls 0])
    (define-benchmark-fixture lifecycle-fixture
      (:setup (lambda (state) (set! calls (+ calls 1)) 'fixture-value)))
    (and
      (for-all
        (lambda (hooks)
          (guard (condition [else (and (who-condition? condition)
                                      (eq? (condition-who condition) 'make-benchmark))])
            (make-benchmark 'conflict (lambda (state value) (void))
              (cons (cons 'fixture lifecycle-fixture) hooks))
            #f))
        (list `((setup . ,(lambda (state) 'local)))
              `((teardown . ,(lambda (state value) (void))))
              `((setup . ,(lambda (state) 'local))
                (teardown . ,(lambda (state value) (void))))))
      (= calls 0)))

  ;; Explicit absent hooks do not conflict, and fixture values still reach the body.
  (let ([events '()])
    (define-benchmark-fixture lifecycle-fixture
      (:setup (lambda (state) (set! events (cons 'setup events)) 'fixture-value)
       :teardown (lambda (state value) (set! events (cons value events)))))
    (let* ([benchmark
            (make-benchmark 'fixture-only
              (lambda (state value) (set! events (cons value events)))
              `((fixture . ,lifecycle-fixture) (setup . #f) (teardown . #f)))]
           [result (car (benchmark-run benchmark
                          (benchmark-config-with (benchmark-test-config) '((samples . 1)))))])
      (and (not (benchmark-result-error result))
           (equal? (reverse events) '(setup fixture-value fixture-value))))))

(mat benchmark-percent-difference-real-domain
  ;; Error: complex inputs must be rejected consistently for zero and nonzero baselines.
  (for-all
    (lambda (arguments)
      (guard (condition
              [else
               (and (who-condition? condition)
                    (eq? (condition-who condition) 'pcheck))])
        (apply benchmark-percent-difference arguments)
        #f))
    '((0 1+2i) (1 1+2i) (1+2i 0) (1+2i 1) (0 invalid) (invalid 0)))

  ;; Signed changes, exact rational inputs, and both signed inexact zeros keep their semantics.
  (for-all
    (lambda (row)
      (= (benchmark-percent-difference (car row) (cadr row)) (caddr row)))
    '((100 110 10) (100 90 -10) (1/2 3/4 50) (0 0 0) (0.0 0.0 0)
      (-0.0 1 +inf.0) (0.0 -1 -inf.0) (0 +inf.0 +inf.0) (0 -inf.0 -inf.0)))

  ;; A real NaN measurement propagates NaN rather than acquiring a negative direction at zero.
  (and (nan? (benchmark-percent-difference 0 +nan.0))
       (nan? (benchmark-percent-difference +nan.0 1))))

(mat benchmark-datum-preserves-generated-symbols
  ;; Distinct generated argument keys and shared references must survive datum serialization.
  (let* ([left (gensym "temp")] [right (gensym "temp")]
         [benchmark
          (make-benchmark 'generated-data (lambda (state key mirror) (void))
            `((args (key mirror) (,left ,left) (,right ,right))
              (template-args . ((kind . ,left)))))]
         [results (benchmark-run benchmark (benchmark-test-config))])
    (for-all
      (lambda (mode)
        (let ([text
               (parameterize ([print-gensym mode] [print-graph #f]
                              [print-length 1] [print-level 1])
                 (call-with-string-output-port
                   (lambda (out) (benchmark-report results (benchmark-datum-reporter) out))))])
          (call-with-port (open-input-string text)
            (lambda (in)
              (let* ([first (read in)] [second (read in)]
                     [first-args (cdr (assq 'args first))]
                     [second-args (cdr (assq 'args second))])
                (and (eq? (cdr (assq 'key first-args)) left)
                     (eq? (cdr (assq 'mirror first-args)) left)
                     (eq? (cdr (assq 'key second-args)) right)
                     (eq? (cdr (assq 'mirror second-args)) right)
                     (eq? (cdr (assq 'kind (cdr (assq 'template-args first)))) left)
                     (eof-object? (read in))))))))
      '(#t #f pretty))))

(mat benchmark-baseline-preserves-generated-symbols
  ;; Readable generated keys must save, reload, and match current cases under any gensym mode.
  (let* ([left (gensym "temp")] [right (gensym "temp")]
         [benchmark
          (make-benchmark 'generated-baseline (lambda (state key) (void))
            `((args (key) (,left) (,right)) (template-args . ((kind . ,left)))))]
         [results (benchmark-run benchmark (benchmark-test-config))]
         [path (benchmark-temp-file)])
    (dynamic-wind
      (lambda () (void))
      (lambda ()
        (for-all
          (lambda (mode)
            (guard (condition [else #f])
              (parameterize ([print-gensym mode] [print-graph #f]
                             [print-length 1] [print-level 1])
                (benchmark-save-baseline results path))
              (let ([loaded (benchmark-load-baseline path)])
                (and (equal? (map benchmark-result-args loaded)
                             (list (list (cons 'key left)) (list (cons 'key right))))
                     (equal? (map benchmark-result-template-args loaded)
                             (list (list (cons 'kind left)) (list (cons 'kind left))))
                     (= (length (benchmark-compare-results loaded results)) 2)))))
          '(#t #f pretty)))
      (lambda () (when (file-exists? path) (delete-file path))))))

(mat benchmark-baseline-isolates-reader-case
  ;; Error: ambient case folding must neither reject saving nor rename loaded identities.
  (let* ([benchmark (make-benchmark 'UPPER (lambda (state value) (void))
                      '((args (SIZE) (SomeSymbol))))]
         [results (benchmark-run benchmark (benchmark-test-config))]
         [path (benchmark-temp-file)])
    (dynamic-wind
      (lambda () (void))
      (lambda ()
        (guard (raised [else #f])
          (parameterize ([case-sensitive #f])
            (benchmark-save-baseline results path)
            (let ([loaded (benchmark-load-baseline path)])
              (and (eq? (benchmark-result-name (car loaded)) 'UPPER)
                   (equal? (benchmark-result-args (car loaded)) '((SIZE . SomeSymbol)))
                   (= (length (benchmark-compare-results results loaded)) 1))))))
      (lambda () (when (file-exists? path) (delete-file path))))))

(mat benchmark-json-preserves-extreme-rationals
  ;; Error: finite nonzero rationals outside flonum range must not become zero or infinity.
  (let* ([tiny (/ 1 (expt 10 400))] [huge (/ (expt 10 400) 3)]
         [benchmark (make-benchmark 'exact-json
                      (lambda (state)
                        (benchmark-state-counter-set! state 'tiny tiny)
                        (benchmark-state-counter-set! state 'huge huge)) '())]
         [results (benchmark-run benchmark (benchmark-test-config))]
         [text (call-with-string-output-port
                 (lambda (out) (benchmark-report results (benchmark-json-reporter) out)))])
    (and (json5-document? (parse-json5 text))
         (benchmark-string-contains? text (string-append "\"tiny\":\"" (number->string (* 2 tiny)) "\""))
         (benchmark-string-contains? text (string-append "\"huge\":\"" (number->string (* 2 huge)) "\"")))))

(mat benchmark-json-distinguishes-generated-keys
  ;; Error: generated keys with identical pretty names must not emit duplicate JSON names.
  (let* ([left (gensym "n")] [right (gensym "n")]
         [benchmark (make-benchmark 'generated-json (lambda (state a b) (void))
                      (list (cons 'args (list (list left right) '(1 2)))))]
         [results (benchmark-run benchmark (benchmark-test-config))]
         [text (call-with-string-output-port
                 (lambda (out) (benchmark-report results (benchmark-json-reporter) out)))])
    (let* ([document (parse-json5 text)]
           [root (json5-document-value document)]
           [object-ref (lambda (object name)
                         (json5-member-value
                           (find (lambda (member) (string=? name (json5-member-name member)))
                                 (vector->list (json5-object-members object)))))]
           [result (vector-ref (json5-array-elements (object-ref root "results")) 0)]
           [members (json5-object-members (object-ref result "args"))])
      (and (= (vector-length members) 2)
           (not (string=? (json5-member-name (vector-ref members 0))
                          (json5-member-name (vector-ref members 1))))))))

(mat benchmark-comparison-rejects-malformed-options
  ;; Error: unknown keys, duplicate keys, malformed entries, and non-symbol metrics must reject.
  (for-all
    (lambda (options)
      (guard (raised [else #t])
        (benchmark-compare-results '() '() options)
        #f))
    '(((threshold-precent . 10))
      ((metric . 1))
      ((threshold-percent . 5) (threshold-percent . 10))
      (invalid)
      ((metric . real-ns) . improper))))

(mat benchmark-comparison-preserves-extreme-finite-changes
  ;; Error: intermediate flonum overflow or underflow must not change threshold decisions.
  (and (= (benchmark-percent-difference 1e308 -1e308) -200)
       (= (benchmark-percent-difference (expt 10 400) 0.0) -100)
       (= (benchmark-percent-difference (/ 1 (expt 10 400)) 0.0) -100)
       (let* ([baseline
               (benchmark-results-from-data
                 `(((name . huge) (summary . ((real-ns . ((mean . ,(expt 10 400)))))))))]
              [current
               (benchmark-results-from-data
                 '(((name . huge) (summary . ((real-ns . ((mean . 0.0))))))))]
              [percent (car (benchmark-compare-results baseline current
                              '((threshold-percent . 150))))]
              [absolute (car (benchmark-compare-results baseline current
                               `((threshold-absolute . ,(expt 10 500)))))] )
         (and (not (benchmark-comparison-improvement? percent))
              (not (benchmark-comparison-improvement? absolute))))))

(mat benchmark-empty-runs-report
  (for-all
    (lambda (source)
      (call-with-port (open-output-string)
        (lambda (out)
          (let* ([events '()]
                 [reporter (make-benchmark-reporter
                             (lambda (results out) (set! events (cons 'start events)))
                             (lambda (result out) (set! events (cons 'result events)))
                             (lambda (results out) (set! events (cons 'finish events))))]
                 [config (benchmark-config-with (benchmark-test-config)
                           `((reporter . ,reporter) (output . ,out) (filter . "unmatched")))])
            (and (null? (benchmark-run source config))
                 (equal? (reverse events) '(start finish)))))))
    (list '()
          (make-benchmark 'filtered (lambda (state) (void)) '())
          (make-benchmark 'no-rows (lambda (state value) (void)) '((args (value))))))

  (call-with-port (open-output-string)
    (lambda (out)
      (and (null? (benchmark-run '()
                    (benchmark-config-with (benchmark-test-config)
                      `((reporter . ,(benchmark-json-reporter)) (output . ,out)))))
           (string=? (get-output-string out) "{\"results\":[]}\n"))))

  (call-with-port (open-output-string)
    (lambda (out)
      (and (null? (benchmark-run '()
                    (benchmark-config-with (benchmark-test-config)
                      `((reporter . ,(benchmark-csv-reporter)) (output . ,out)))))
           (benchmark-string-contains? (get-output-string out) "name,args,template_args")))))

(mat benchmark-rejects-duplicate-singleton-options
  ;; Error: singleton options cannot silently discard one of two conflicting values.
  (for-all
    (lambda (options)
      (guard (raised [else #t])
        (make-benchmark 'duplicate (lambda (state) (void)) options)
        #f))
    '(((samples . 1) (samples . 2))
      ((setup . #f) (setup . #f))
      ((unit . ns) (unit . ms))))

  ;; Error: config copies reject duplicate options within the same update.
  (guard (raised [else #t])
    (benchmark-config-with (benchmark-test-config) '((samples . 1) (samples . 2)))
    #f)

  ;; Error: malformed range rows and duplicate singleton clauses reject during expansion.
  (for-all
    (lambda (form)
      (guard (raised [else (syntax-violation? raised)])
        (expand form (environment '(cspp)))
        #f))
    '((benchmark-options (:samples 1 :samples 2))
      (benchmark-options (:setup #f :setup #f))
      (benchmark-options (:arg-range [n 1 8]))
      (benchmark-options (:dense-arg-range [n 1 8 1 ignored])))))

(mat benchmark-rejects-invalid-collections-before-side-effects
  ;; Error: malformed descriptor lists must reject before the first body executes.
  (let ([called? #f])
    (guard (raised [else (not called?)])
      (benchmark-run
        (list (make-benchmark 'first (lambda (state) (set! called? #t)) '()) #f)
        (benchmark-test-config))
      #f))

  ;; Error: malformed result lists must reject before reporter callbacks run.
  (let ([called? #f])
    (call-with-port (open-output-string)
      (lambda (out)
        (guard (raised [else (not called?)])
          (benchmark-report '(#f)
            (make-benchmark-reporter
              (lambda (results out) (set! called? #t))
              (lambda (result out) (void))
              (lambda (results out) (void))) out)
          #f))))

  ;; Error: suite members must be descriptors, even if the suite is never run.
  (guard (raised [else #t])
    (eval '(let () (define-benchmark-suite invalid () #f) invalid)
          (environment '(cspp)))
    #f))

(mat benchmark-json-preserves-colliding-key-associations
  ;; Error: distinct Scheme keys must not disappear behind duplicate JSON member names.
  (let* ([payload '((foo . 1) ("foo" . 2))]
         [benchmark (make-benchmark 'colliding-json (lambda (state value) (void))
                      `((args (value) (,payload))))]
         [results (benchmark-run benchmark (benchmark-test-config))]
         [text (call-with-string-output-port
                 (lambda (out) (benchmark-report results (benchmark-json-reporter) out)))])
    (and (json5-document? (parse-json5 text))
         (benchmark-string-contains? text "((foo . 1) (\\\"foo\\\" . 2))"))))

(mat benchmark-negative-metric-regressions
  (let* ([baseline (benchmark-results-from-data
                     '(((name . negative) (summary . ((score . ((mean . -100))))))))]
         [current (benchmark-results-from-data
                    '(((name . negative) (summary . ((score . ((mean . -50))))))))]
         [comparison (car (benchmark-compare-results baseline current '((metric . score))))])
    (and (= (benchmark-comparison-percent-difference comparison) -50)
         (benchmark-comparison-regression? comparison)
         (not (benchmark-comparison-improvement? comparison)))))

(mat benchmark-rich-options-reject-at-construction
  ;; Error: unknown, duplicate, malformed and incorrectly typed options must reject early.
  (for-all
    (lambda (options)
      (guard (raised [else #t]) (benchmark-rich-reporter options) #f))
    '(((titel . "typo")) ((padding . -1)) ((show-header? . yes))
      ((title . 42)) ((box . unknown)) ((columns . ((unknown . "Unknown"))))
      ((columns . ((name . 42)))) ((title . "first") (title . "second")) (invalid))))

(mat benchmark-baseline-rejects-trailing-content
  ;; Error: a baseline file must contain exactly one complete document.
  (let ([path (benchmark-temp-file)])
    (dynamic-wind
      (lambda ()
        (call-with-output-file path
          (lambda (out)
            (display "((format . cspp-benchmark-baseline) (version . 1) (results . ()))\n(" out))
          'replace))
      (lambda () (guard (raised [else #t]) (benchmark-load-baseline path) #f))
      (lambda () (when (file-exists? path) (delete-file path))))))

(mat benchmark-current-registry-rejects-invalid-values
  ;; Error: invalid parameter values must reject before any registration can occur.
  (guard (raised [else #t]) (parameterize ([current-benchmark-registry 42]) #f)))

(mat benchmark-empty-descriptor-report-precedence
  (call-with-port (open-output-string)
    (lambda (actual)
      (call-with-port (open-output-string)
        (lambda (fallback)
          (let* ([benchmark (make-benchmark 'empty-json (lambda (state value) (void))
                              `((args (value))
                                (reporter . ,(benchmark-json-reporter)) (output . ,actual)))]
                 [config (parameterize ([current-output-port fallback])
                           (default-benchmark-config))])
            (and (null? (benchmark-run benchmark config))
                 (string=? (get-output-string actual) "{\"results\":[]}\n")
                 (string=? (get-output-string fallback) "")))))))

  ;; Explicitly disabled descriptor reporting must not fall back to a default reporter.
  (call-with-port (open-output-string)
    (lambda (fallback)
      (let* ([benchmark (make-benchmark 'empty-disabled (lambda (state value) (void))
                          '((args (value)) (reporter . #f) (output . #f)))]
             [config (parameterize ([current-output-port fallback]) (default-benchmark-config))])
        (and (null? (benchmark-run benchmark config))
             (string=? (get-output-string fallback) ""))))))

(mat benchmark-cartesian-construction-and-stop-are-bounded
  ;; A failing first case must not allocate states for the remaining Cartesian product.
  (let* ([rows (map list (iota 200))]
         [options (list (cons 'args (cons '(left) rows))
                        (cons 'args (cons '(right) rows)))]
         [before (statistics)]
         [benchmark (make-benchmark 'large-product
                      (lambda (state left right) (error 'large-product "stop")) options)]
         [config (benchmark-config-with (benchmark-test-config)
                   '((samples . 1) (stop-on-error? . #t)))]
         [results (benchmark-run benchmark config)]
         [bytes (sstats-bytes (sstats-difference (statistics) before))])
    (and (= (length results) 1)
         (benchmark-result-error (car results))
         (equal? (benchmark-result-args (car results)) '((left . 0) (right . 0)))
         (< bytes 1000000))))

(mat benchmark-argument-axes-snapshot-input-rows
  (let* ([names (list 'value)] [row (list 7)]
         [benchmark (make-benchmark 'snapshot (lambda (state value) (void))
                      (list (cons 'args (list names row))))])
    (set-car! names 'changed)
    (set-car! row 99)
    (equal? (map (lambda (expanded) (benchmark-state-args (car expanded)))
                 (benchmark-expand benchmark (benchmark-test-config)))
            '(((value . 7))))))

(mat benchmark-empty-later-axis-short-circuits-prefix-products
  (let* ([options (append
                    (map (lambda (n)
                           (cons 'args
                             (list (list (string->symbol (format "axis-~a" n))) '(0) '(1))))
                         (iota 15))
                    '((args (empty))))]
         [benchmark (make-benchmark 'empty-product (lambda arguments (void)) options)]
         [before (statistics)]
         [expanded (benchmark-expand benchmark (benchmark-test-config))]
         [bytes (sstats-bytes (sstats-difference (statistics) before))])
    (and (null? expanded) (< bytes 1000000))))

(mat benchmark-baseline-closes-ports-on-reader-errors
  ;; Error: reader failures and trailing data must close the file before propagating.
  (if (file-directory? "/proc/self/fd")
      (let ([path (benchmark-temp-file)])
        (dynamic-wind
          (lambda () (void))
          (lambda ()
            (for-all
              (lambda (text)
                (call-with-output-file path (lambda (out) (display text out)) 'replace)
                (collect)
                (parameterize ([collect-trip-bytes (* 1024 1024 1024)])
                  (let ([before (length (directory-list "/proc/self/fd"))])
                    (and (for-all
                           (lambda (_)
                             (guard (raised [else #t]) (benchmark-load-baseline path) #f))
                           (iota 8))
                         (= before (length (directory-list "/proc/self/fd")))))))
              '("(" "((format . cspp-benchmark-baseline) (version . 1) (results))\n42")))
          (lambda () (when (file-exists? path) (delete-file path)))))
      #t))

(mat benchmark-lifecycle-callback-arities
  ;; Error: invalid hooks must reject descriptors, fixtures, and suites before acquiring resources.
  (for-all
    (lambda (options)
      (guard (raised [else #t])
        (make-benchmark 'invalid-hook (lambda arguments (void)) options)
        #f))
    (list `((setup . ,(lambda () #f)))
          `((teardown . ,(lambda (state) (void))))))

  ;; Error: fixture and suite macros must validate their callback signatures at definition time.
  (for-all
    (lambda (form)
      (guard (raised [else #t]) (eval form (environment '(cspp))) #f))
    '((let () (define-benchmark-fixture bad (:setup (lambda () #f))) bad)
      (let () (define-benchmark-fixture bad (:teardown (lambda (state) #f))) bad)
      (let () (define-benchmark-suite bad (:suite-setup (lambda () #f))) bad)
      (let () (define-benchmark-suite bad (:suite-teardown (lambda (state) #f))) bad)))

  ;; Optional and rest-argument hooks accepting the required arguments remain supported.
  (let* ([calls '()]
         [descriptor
          (make-benchmark 'valid-hook
            (lambda (state value) (set! calls (cons value calls)))
            `((setup . ,(case-lambda [() #f] [(state) 'ready]))
              (teardown . ,(lambda arguments (set! calls (cons 'closed calls))))))]
         [result (car (benchmark-run descriptor (benchmark-test-config)))])
    (and (not (benchmark-result-error result))
         (equal? (reverse calls) '(ready closed ready closed)))))

(mat benchmark-state-enforces-model-invariants
  ;; Error: public states must enforce the same suite and argument domains as descriptors/baselines.
  (for-all
    (lambda (fields)
      (guard (raised [else #t])
        (apply benchmark-state 'invalid (append fields '(0 1 0)))
        #f))
    '((42 () ()) (#f (invalid) ()) (#f () (invalid))
      (#f ((42 . value)) ()) (#f () ((42 . value)))
      (#f ((value . 1) (value . 2)) ())
      (#f () ((value . 1) (value . 2)))))

  (let* ([state (benchmark-state 'custom 'suite '((value . 7)) '((type . fixnum)) 0 1 0)]
         [descriptor (make-benchmark 'custom (lambda (state value) (void))
                       '((args (value) (7))))]
         [result (benchmark-run-one (cons state descriptor) (benchmark-test-config))]
         [path (benchmark-temp-file)])
    (dynamic-wind
      (lambda () (void))
      (lambda ()
        (benchmark-save-baseline (list result) path)
        (let ([loaded (car (benchmark-load-baseline path))])
          (and (not (benchmark-result-error result))
               (equal? (benchmark-result-args loaded) '((value . 7)))
               (equal? (benchmark-result-template-args loaded) '((type . fixnum))))))
      (lambda () (when (file-exists? path) (delete-file path))))))

(mat benchmark-built-in-reporter-output-contract
  ;; Error: all built-in reporters require textual ports before setup or body side effects.
  (for-all
    (lambda (reporter)
      (call-with-values open-bytevector-output-port
        (lambda (out extract)
          (dynamic-wind
            (lambda () (void))
            (lambda ()
              (let ([ran? #f])
                (guard (raised [else (not ran?)])
                  (benchmark-run
                    (make-benchmark 'invalid-output (lambda (state) (set! ran? #t)) '())
                    (make-benchmark-config 0 1 0 1 out #f reporter #f #f))
                  #f)))
            (lambda () (close-port out))))))
    (list (benchmark-text-reporter) (benchmark-datum-reporter) (benchmark-csv-reporter)
          (benchmark-json-reporter) (benchmark-rich-reporter)))

  ;; Error: a port closed after configuration must reject before sample setup.
  (let* ([out (open-output-string)]
         [config (make-benchmark-config 0 1 0 1 out #f (benchmark-text-reporter) #f #f)]
         [ran? #f]
         [descriptor (make-benchmark 'closed-output (lambda (state value) (void))
                       `((setup . ,(lambda (state) (set! ran? #t) #f))))])
    (close-port out)
    (guard (raised [else (not ran?)]) (benchmark-run descriptor config) #f))

  ;; Error: suite setup must also be protected when a previously valid port is closed.
  (let* ([out (open-output-string)]
         [config (make-benchmark-config 0 1 0 1 out #f (benchmark-text-reporter) #f #f)]
         [ran? #f])
    (define-benchmark-suite closed-output-suite
      (:suite-setup (lambda (state) (set! ran? #t))))
    (close-port out)
    (guard (raised [else (not ran?)]) (benchmark-run-suite closed-output-suite config) #f)))

(mat benchmark-custom-binary-reporters
  (call-with-values open-bytevector-output-port
    (lambda (out extract)
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let* ([reporter (make-benchmark-reporter
                            (lambda (results out) (put-u8 out 1))
                            (lambda (result out) (put-u8 out 2))
                            (lambda (results out) (put-u8 out 3)))]
                 [config (benchmark-config-with (default-benchmark-config)
                           `((output . ,out) (reporter . ,reporter)))])
            (benchmark-run '() config)
            (equal? (extract) #vu8(1 3))))
        (lambda () (close-port out))))))

(mat benchmark-binary-descriptor-output-with-runtime-reporter
  ;; Descriptor defaults must compose with a custom binary reporter supplied by the run.
  (call-with-values open-bytevector-output-port
    (lambda (out extract)
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let* ([descriptor (make-benchmark 'binary-default (lambda (state) (void))
                               `((output . ,out)))]
                 [reporter (make-benchmark-reporter
                             (lambda (results out) (put-u8 out 1))
                             (lambda (result out) (put-u8 out 2))
                             (lambda (results out) (put-u8 out 3)))]
                 [config (benchmark-config-with (default-benchmark-config)
                           `((warmup . 0) (samples . 0) (reporter . ,reporter)))])
            (benchmark-run descriptor config)
            (equal? (extract) #vu8(1 2 3))))
        (lambda () (close-port out))))))

(mat benchmark-direct-report-validates-empty-output
  ;; Error: a datum reporter otherwise only flushes an empty report and can hide an invalid port.
  (call-with-values open-bytevector-output-port
    (lambda (out extract)
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (guard (raised [else #t]) (benchmark-report '() (benchmark-datum-reporter) out) #f))
        (lambda () (close-port out))))))

(mat benchmark-inexact-iterations-use-exact-arithmetic
  (let* ([result (car (benchmark-results-from-data
                       '(((name . fractional)
                          (samples . (((iterations . 3.0) (cpu-ns . 1)
                                       (real-ns . 1) (bytes . 1))))))))]
         [samples (benchmark-result-samples result)]
         [summary (benchmark-summarize samples)])
    (and (eqv? (benchmark-sample-iterations (car samples)) 3.0)
         (for-all
           (lambda (metric)
             (let ([mean (cdr (assq 'mean (cdr (assq metric summary))))])
               (and (exact? mean) (= mean 1/3))))
           '(cpu-ns real-ns bytes))))

  ;; Error: inexact iteration representation must not overflow an otherwise finite deviation.
  (let* ([huge (expt 10 308)]
         [result (car (benchmark-results-from-data
                        `(((name . finite-deviation)
                           (samples . (((iterations . 1.0) (bytes . 0))
                                       ((iterations . 1.0) (bytes . ,huge))))))))]
         [summary (cdr (assq 'bytes (benchmark-summarize (benchmark-result-samples result))))])
    (and (= (cdr (assq 'mean summary)) (/ huge 2))
         (finite? (cdr (assq 'stddev summary)))
         (= (cdr (assq 'stddev summary)) (/ huge 2)))))

(mat benchmark-filtered-descriptor-output-is-unused
  ;; An excluded descriptor's incompatible default output must not prevent the run's empty report.
  (call-with-values open-bytevector-output-port
    (lambda (unused extract)
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (call-with-port (open-output-string)
            (lambda (fallback)
              (let* ([ran? #f]
                     [descriptor (make-benchmark 'excluded-output
                                   (lambda (state) (set! ran? #t)) `((output . ,unused)))]
                     [config (benchmark-config-with
                               (parameterize ([current-output-port fallback])
                                 (default-benchmark-config))
                               `((filter . ,(lambda (benchmark) #f))))]
                     [results (benchmark-run descriptor config)])
                (and (null? results) (not ran?)
                     (not (string=? (get-output-string fallback) "")))))))
        (lambda () (close-port unused))))))

(mat benchmark-run-one-does-not-validate-unused-reporting
  ;; The low-level runner returns one result without invoking a reporter or using its output port.
  (call-with-values open-bytevector-output-port
    (lambda (unused extract)
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let* ([descriptor (make-benchmark 'no-report (lambda (state) (void)) '())]
                 [config (make-benchmark-config 0 1 0 1 unused #f (benchmark-text-reporter) #f #f)]
                 [expanded (car (benchmark-expand descriptor config))]
                 [result (benchmark-run-one expanded config)])
            (and (not (benchmark-result-error result))
                 (= (length (benchmark-result-samples result)) 1)
                 (= (bytevector-length (extract)) 0))))
        (lambda () (close-port unused))))))

(mat benchmark-definition-names-are-identifiers
  ;; Error: a list name must not expand into Scheme's procedure-definition shorthand.
  (for-all
    (lambda (form)
      (guard (raised [else (syntax-violation? raised)])
        (expand form (environment '(cspp)))
        #f))
    '((let () (define-benchmark (bad x) () (lambda (state) #t)) bad)
      (let () (define-benchmark-fixture (bad x) ()) bad)
      (let () (define-fixture-benchmark (bad x) fixture () (lambda arguments #t)) bad)
      (let () (define-benchmark-suite (bad x) ()) bad)
      (let () (define-benchmark-template (bad x) (kind) () (lambda (state) kind)) bad)
      (let () (define-benchmark :bad () (lambda (state) #t)) :bad)))

  ;; All definition forms still produce runnable descriptors with ordinary names.
  (let ()
    (define-benchmark valid-name () (lambda (state) #t))
    (define-benchmark-fixture valid-fixture ())
    (define-fixture-benchmark valid-fixture-name valid-fixture () (lambda (state value) #t))
    (define-benchmark-template valid-template (kind) () (lambda (state) kind))
    (define-benchmark-suite valid-suite () valid-name valid-fixture-name (valid-template 'kind))
    (let ([results (benchmark-run-suite valid-suite (benchmark-test-config))])
      (and (equal? (map benchmark-result-name results)
                   '(valid-name valid-fixture-name valid-template))
           (for-all (lambda (result) (not (benchmark-result-error result))) results)))))

(mat benchmark-body-signatures-match-inputs
  ;; Error: reject impossible body calls before setup, including empty argument domains.
  (let ([setup? #f])
    (and (for-all
           (lambda (entry)
             (guard (raised [else #t])
               (make-benchmark 'invalid-body (car entry) (cdr entry))
               #f))
           (list (cons (lambda () #t) '())
                 (cons (lambda (state) #t) '((args (value) (1))))
                 (cons (lambda (state) #t) '((args (value))))
                 (cons (lambda (state value) #t) '((args (x y) (1 2))))
                 (cons (lambda (state) #t)
                       `((setup . ,(lambda (state) (set! setup? #t) 'resource))))))
         (not setup?)))

  ;; Rest bodies and case-lambda bodies remain valid when they accept the effective call.
  (for-all
    (lambda (body)
      (let* ([benchmark (make-benchmark 'valid-body body
                          '((args (left) (3)) (args (right) (4))
                            (setup . #f)))]
             [results (benchmark-run benchmark (benchmark-test-config))])
        (and (= (length results) 1) (not (benchmark-result-error (car results))))))
    (list (lambda (state left right) (test-= (+ left right) 7))
          (lambda (state . values) (test-equal values '(3 4)))
          (case-lambda [() (error 'body "wrong arity chosen")]
                       [(state left right) (test-= (+ left right) 7)])))

  ;; A fixture always adds its setup value, including the default fixture's #f value.
  (let ()
    (define-benchmark-fixture body-signature-fixture ())
    (and (guard (raised [else #t])
           (make-benchmark 'invalid-fixture-body (lambda (state) #t)
             `((fixture . ,body-signature-fixture)))
           #f)
         (let* ([benchmark (make-benchmark 'valid-setup-body
                             (lambda (state value) (test-= value 7))
                             `((setup . ,(lambda (state) 7))))]
                [results (benchmark-run benchmark (benchmark-test-config))])
           (not (benchmark-result-error (car results)))))))

(mat benchmark-suite-retains-results-before-selector-failure
  ;; Error: a later selector failure must preserve completed children and their reports.
  (for-all
    (lambda (reporting?)
      (call-with-port (open-output-string)
        (lambda (out)
          (let* ([primary (condition (make-error) (make-message-condition "selector failed"))]
                 [visited '()] [ran? #f] [cleaned? #f] [reported '()]
                 [reporter (make-benchmark-reporter
                             (lambda (results out)
                               (unless cleaned? (error 'reporter "teardown has not run"))
                               (set! reported (map benchmark-result-name results)))
                             (lambda (result out) (void))
                             (lambda (results out) (void)))]
                 [first (make-benchmark 'completed (lambda (state) (set! ran? #t)) '())]
                 [second (make-benchmark 'unselected (lambda (state) (void)) '())])
            (define-benchmark-suite partial-suite
              (:suite-teardown (lambda (state setup)
                                 (set! cleaned? #t)
                                 (error 'teardown "secondary failure")))
              first second)
            (let ([results
                   (benchmark-run-suite partial-suite
                     (benchmark-config-with (benchmark-test-config)
                       `((reporter . ,(and reporting? reporter))
                         (output . ,(and reporting? out))
                         (filter . ,(lambda (benchmark)
                                      (set! visited (cons (benchmark-name benchmark) visited))
                                      (if (eq? (benchmark-name benchmark) 'unselected)
                                          (raise primary)
                                          #t))))))])
              (and ran? cleaned?
                   (equal? (reverse visited) '(completed unselected))
                   (equal? (map benchmark-result-name results) '(completed partial-suite))
                   (not (benchmark-result-error (car results)))
                   (eq? (benchmark-result-error (cadr results)) primary)
                   (equal? reported (if reporting? '(completed partial-suite) '()))))))))
    '(#f #t)))

(mat benchmark-summary-preserves-extreme-exact-range
  ;; Error: finite exact variance and confidence limits must not overflow or underflow.
  (for-all
    (lambda (scale)
      (for-all
        (lambda (row)
          (let* ([samples (map (lambda (value)
                                 `((iterations . ,(if (< scale 1) (denominator scale) 1))
                                   (bytes . ,(* value (if (< scale 1) 1 scale)))))
                               (car row))]
                 [result (car (benchmark-results-from-data
                                `(((name . extreme-summary) (samples . ,samples)))))]
                 [summary (cdr (assq 'bytes
                                  (benchmark-summarize (benchmark-result-samples result))))]
                 [deviation (cdr (assq 'stddev summary))]
                 [bounds (cdr (assq 'confidence-interval summary))]
                 [normalized (map (lambda (value) (/ value scale)) bounds)])
            (and (= (cdr (assq 'mean summary)) scale)
                 (finite? deviation) (> deviation 0)
                 (< (abs (- (/ deviation scale) (cadr row))) 1e-14)
                 (for-all finite? bounds)
                 (for-all (lambda (actual expected) (< (abs (- actual expected)) 1e-14))
                          normalized (caddr row)))))
        '(((0 2) 1 (-0.3859292911256331 2.385929291125633))
          ((0 1 2) 0.816496580927726 (0.07604713924957796 1.923952860750422)))))
    (list (expt 10 400) (/ 1 (expt 10 400))
          (expt 10 160) (/ 1 (expt 10 160)))))

(mat benchmark-extreme-summaries-survive-persistence-and-reporting
  (let* ([huge (expt 10 400)]
         [sample-data `(((iterations . 1) (bytes . 0))
                        ((iterations . 1) (bytes . ,huge))
                        ((iterations . 1) (bytes . ,(* huge 2))))]
         [original (car (benchmark-results-from-data
                          `(((name . extreme) (samples . ,sample-data)))))]
         [summary (benchmark-summarize (benchmark-result-samples original))]
         [results (benchmark-results-from-data
                    `(((name . extreme) (samples . ,sample-data) (summary . ,summary))))]
         [path (benchmark-temp-file)])
    (dynamic-wind
      (lambda () (void))
      (lambda ()
        (benchmark-save-baseline results path)
        (let* ([loaded (benchmark-load-baseline path)]
               [text (call-with-string-output-port
                       (lambda (out) (benchmark-report loaded (benchmark-json-reporter) out)))])
          (and (equal? (benchmark-result-summary (car loaded)) summary)
               (for-all finite?
                 (cdr (assq 'confidence-interval (cdr (assq 'bytes summary)))))
               (json5-document? (parse-json5 text))
               (not (benchmark-string-contains? text "inf.0")))))
      (lambda () (when (file-exists? path) (delete-file path))))))

(mat benchmark-user-data-checks-state
  ;; Error: invalid state arguments must raise an ordinary condition before raw record access.
  (guard (raised [else (and (condition? raised) (who-condition? raised)
                           (eq? (condition-who raised) 'pcheck))])
    (if (benchmark-state-user-data #f) #f #f))

  ;; Error: mutation must also validate its state argument before writing a record field.
  (guard (raised [else (and (condition? raised) (who-condition? raised)
                           (eq? (condition-who raised) 'pcheck))])
    (benchmark-state-set-user-data! #f 'resource)
    #f)

  (let ([state (benchmark-state 'user-data #f '() '() 0 1 0)]
        [payload (vector 'resource)])
    (benchmark-state-set-user-data! state payload)
    (eq? (benchmark-state-user-data state) payload)))

(mat benchmark-template-validates-boundaries
  ;; Error: invalid labels or template expressions must be rejected during expansion.
  (for-all
    (lambda (form)
      (guard (raised [else (syntax-violation? raised)])
        (expand form (environment '(cspp)))
        #f))
    '((let ()
        (define-benchmark-template sized (size) () (lambda (state) size))
        (instantiate-benchmark-template sized ([64])))
      (let ([factory (lambda (label) #f)])
        (instantiate-benchmark-template factory ([(nested label)])))
      (instantiate-benchmark-template (lambda (label) #f) ([kind]))))

  ;; Error: a user-supplied template must return a benchmark before record fields are read.
  (let ()
    (benchmark-clear-registry! (current-benchmark-registry))
    (let ([calls 0])
      (let ([factory (lambda (label) (set! calls (+ calls 1)) #f)])
        (and (guard (raised [else (and (condition? raised) (who-condition? raised)
                                      (eq? (condition-who raised)
                                           'instantiate-benchmark-template))])
               (instantiate-benchmark-template factory ([kind]))
               #f)
             (= calls 1)
             (null? (benchmark-registry-benchmarks (current-benchmark-registry)))))))

  (let ()
    (benchmark-clear-registry! (current-benchmark-registry))
    (let ()
      (define-benchmark-template instance-template (kind size) ()
        (lambda (state) (test-true (memq kind '(small large))) (test-true (> size 0))))
      (instantiate-benchmark-template instance-template ([small 1] [large 2]))
      (let ([results (benchmark-run (benchmark-test-config))])
        (and (equal? (map benchmark-result-name results)
                     '(instance-template/small instance-template/large))
             (for-all (lambda (result) (not (benchmark-result-error result))) results))))))

(mat benchmark-fixture-macro-requires-fixture
  ;; Error: fixture-specific definitions must reject #f before registering a benchmark.
  (let* ([registry (current-benchmark-registry)]
         [before (benchmark-registry-benchmarks registry)]
         [evaluations 0])
    (and (guard (raised [else #t])
           (let ()
             (define-fixture-benchmark invalid-fixture
               (begin (set! evaluations (+ evaluations 1)) #f)
               () (lambda arguments #t))
             invalid-fixture)
           #f)
         (= evaluations 1)
         (equal? (benchmark-registry-benchmarks registry) before)))

  (let ([evaluations 0] [seen '()])
    (let ()
      (define-benchmark-fixture required-fixture (:setup (lambda (state) 'resource)))
      (define-fixture-benchmark required-fixture-benchmark
        (begin (set! evaluations (+ evaluations 1)) required-fixture)
        () (lambda (state resource) (set! seen (cons resource seen))))
      (let ([results (benchmark-run required-fixture-benchmark (benchmark-test-config))])
        (and (= evaluations 1) (equal? seen '(resource resource))
             (for-all (lambda (result) (not (benchmark-result-error result))) results))))))

(mat benchmark-selectors-require-unary-callbacks
  ;; Error: invalid selector arity must reject even when there are no descriptors to visit.
  (let ([invalid (lambda () #t)])
    (benchmark-clear-registry! (current-benchmark-registry))
    (and (guard (raised [else #t])
           (benchmark-select (current-benchmark-registry) invalid)
           #f)
         (guard (raised [else #t])
           (make-benchmark-config 0 1 0 1 #f invalid #f #f #f)
           #f)
         (guard (raised [else #t])
           (benchmark-config-with (benchmark-test-config) `((filter . ,invalid)))
           #f)
         (guard (raised [else #t])
           (make-benchmark 'invalid-selector (lambda (state) #t) `((filter . ,invalid)))
           #f)))

  ;; Rest and case-lambda selectors are valid when they accept a descriptor argument.
  (for-all
    (lambda (selector)
      (let ()
        (benchmark-clear-registry! (current-benchmark-registry))
        (let ()
          (define-benchmark selected-by-unary () (lambda (state) #t))
          (let ([config (benchmark-config-with (benchmark-test-config) `((filter . ,selector)))])
            (and (equal? (benchmark-select (current-benchmark-registry) selector)
                         (list selected-by-unary))
                 (= (length (benchmark-run config)) 1))))))
    (list (lambda descriptors (= (length descriptors) 1))
          (case-lambda [() #f] [(descriptor) (benchmark? descriptor)]))))

(mat benchmark-run-one-validates-expanded-inputs
  ;; Error: a pair alone does not establish that either member is the required record.
  (for-all
    (lambda (expanded)
      (guard (raised [else (and (condition? raised) (who-condition? raised)
                               (eq? (condition-who raised) 'pcheck))])
        (benchmark-run-one expanded (benchmark-test-config))
        #f))
    (let ([descriptor (make-benchmark 'pair-check (lambda (state) #t) '())]
          [state (benchmark-state 'pair-check #f '() '() 0 1 0)])
      (list (cons #f #f) (cons #f descriptor) (cons state #f))))

  ;; Error: missing, extra, renamed, or reordered arguments must reject before setup.
  (let* ([setups 0]
         [descriptor
          (make-benchmark 'argument-check (lambda (state left right resource) #t)
            `((args (left) (1)) (args (right) (2))
              (setup . ,(lambda (state) (set! setups (+ setups 1)) 'resource))))])
    (and (for-all
           (lambda (args)
             (guard (raised [else (and (condition? raised) (who-condition? raised)
                                      (eq? (condition-who raised) 'benchmark-run-one))])
               (benchmark-run-one
                 (cons (benchmark-state 'argument-check #f args '() 0 1 0) descriptor)
                 (benchmark-test-config))
               #f))
           '(() ((left . 1)) ((left . 1) (right . 2) (extra . 3))
             ((left . 1) (other . 2)) ((right . 2) (left . 1))))
         (= setups 0)))

  ;; Custom states may retain their own name/template metadata and argument object identity.
  (let* ([payload (vector 'resource)]
         [descriptor (make-benchmark 'declared
                       (lambda (state value) (test-eq value payload))
                       '((args (value) (#f))))]
         [state (benchmark-state 'custom #f (list (cons 'value payload))
                  '((kind . custom)) 0 1 0)]
         [result (benchmark-run-one (cons state descriptor) (benchmark-test-config))])
    (and (not (benchmark-result-error result))
         (eq? (benchmark-result-name result) 'custom)
         (equal? (benchmark-result-template-args result) '((kind . custom)))
         (eq? (cdar (benchmark-result-args result)) payload))))

(mat benchmark-reporters-render-error-diagnostics
  ;; Error: text and rich reports must show condition origins and actual irritants.
  (for-all
    (lambda (row)
      (let* ([descriptor (make-benchmark 'reported-error (lambda (state) ((car row))) '())]
             [results (benchmark-run descriptor (benchmark-test-config))]
             [path (benchmark-temp-file)])
        (dynamic-wind
          (lambda () (void))
          (lambda ()
            (benchmark-save-baseline results path)
            (for-all
              (lambda (reported)
                (for-all
                  (lambda (reporter)
                    (let ([text (call-with-string-output-port
                                  (lambda (out) (benchmark-report reported reporter out)))])
                      (for-all (lambda (needle) (benchmark-string-contains? text needle))
                               (cdr row))))
                  (list (benchmark-text-reporter)
                        (benchmark-rich-reporter '((columns . ((error . "error"))))))))
              (list results (benchmark-load-baseline path))))
          (lambda () (when (file-exists? path) (delete-file path))))))
    (list (list (lambda () (errorf 'open-resource "cannot open resource ~s" "database.db"))
                "open-resource" "cannot open resource \"database.db\"")
          (list (lambda () (error 'literal-message "literal ~s" 'unavailable))
                "literal-message" "literal ~s" "unavailable")
          (list (lambda () (raise '(application-error unavailable)))
                "application-error unavailable"))))

(mat benchmark-baseline-validates-error-format-marker
  ;; Error: an optional error format marker must be Boolean before reconstructing conditions.
  (let ([path (benchmark-temp-file)])
    (dynamic-wind
      (lambda () (void))
      (lambda ()
        (benchmark-write-datum-file path
          '((format . cspp-benchmark-baseline) (version . 1)
            (results . (((name . malformed-format)
                         (error . ((message . "failure") (format? . invalid))))))))
        (guard (raised [else (and (who-condition? raised)
                                 (eq? (condition-who raised) 'benchmark-load-baseline))])
          (benchmark-load-baseline path)
          #f))
      (lambda () (when (file-exists? path) (delete-file path))))))

(mat benchmark-template-preserves-argument-snapshots
  ;; Error: renaming must retain validated rows instead of rereading caller-owned option lists.
  (let* ([payload (vector 'resource)]
         [names (list 'number 'payload)]
         [row (list 7 payload)]
         [descriptor (make-benchmark 'snapshot (lambda (state number payload) (void))
                       (list (cons 'args (list names row))
                             '(template-args . ((kind . original)))))]
         [calls 0]
         [factory (lambda (label) (set! calls (+ calls 1)) descriptor)]
         [changed (begin (set-car! names 'changed) (set-car! row 99))])
    (instantiate-benchmark-template factory ([snapshot]))
    (let* ([renamed (car (reverse (benchmark-registry-benchmarks
                                   (current-benchmark-registry))))]
           [original-state (caar (benchmark-expand descriptor (benchmark-test-config)))]
           [renamed-state (caar (benchmark-expand renamed (benchmark-test-config)))])
      (and (= calls 1)
           (eq? (benchmark-name renamed) 'factory/snapshot)
           (equal? (benchmark-state-args original-state) (benchmark-state-args renamed-state))
           (eq? (benchmark-state-arg renamed-state 'payload) payload)
           (= (benchmark-state-arg renamed-state 'number) 7)
           (equal? (benchmark-state-template-args renamed-state) '((kind . original))))))

  ;; Error: a previously validated range must not be regenerated from a mutated source spec.
  (for-all
    (lambda (key)
      (let* ([spec (list 'number 1 4 2)]
             [descriptor (make-benchmark 'range-snapshot (lambda (state number) (void))
                           (list (cons key spec)))]
             [factory (lambda (label) descriptor)]
             [changed (set-car! (cdddr spec) 0)])
        (instantiate-benchmark-template factory ([range]))
        (let ([renamed (car (reverse (benchmark-registry-benchmarks
                                      (current-benchmark-registry))))])
          (equal? (map (lambda (expanded) (benchmark-state-args (car expanded)))
                       (benchmark-expand descriptor (benchmark-test-config)))
                  (map (lambda (expanded) (benchmark-state-args (car expanded)))
                       (benchmark-expand renamed (benchmark-test-config)))))))
    '(arg-range dense-arg-range)))

(mat benchmark-template-preserves-validated-hooks
  ;; Error: changes to source hook options must not replace the descriptor's validated hooks.
  (let* ([events '()]
         [setup (cons 'setup (lambda (state) 'original-resource))]
         [teardown (cons 'teardown
                     (lambda (state resource)
                       (set! events (cons (list 'cleanup resource) events))))]
         [descriptor
          (make-benchmark 'hook-snapshot
            (lambda (state resource)
              (set! events (cons (list (benchmark-state-suite state) resource) events)))
            (list setup teardown '(suite . original-suite) '(samples . 1)))]
         [factory (lambda (label) descriptor)]
         [changed (begin (set-cdr! setup (lambda (state) 'changed-resource))
                         (set-cdr! teardown (lambda (state resource)
                                             (set! events (cons 'changed-cleanup events)))))])
    (instantiate-benchmark-template factory ([hooks]))
    (let* ([renamed (car (reverse (benchmark-registry-benchmarks
                                   (current-benchmark-registry))))]
           [config (benchmark-config-with (default-benchmark-config)
                     '((warmup . 0) (min-time . 0) (max-iterations . 1)
                       (reporter . #f) (output . #f)))]
           [results (benchmark-run (list descriptor renamed) config)])
      (and (for-all (lambda (result) (not (benchmark-result-error result))) results)
           (equal? events '((cleanup original-resource) (original-suite original-resource)
                            (cleanup original-resource) (original-suite original-resource)))))))

(mat benchmark-template-instantiation-is-an-expression
  ;; Error: helper expressions must not expand into an incidental definition.
  (guard (raised [else #f])
    (eval
      '(let* ([before (length (benchmark-registry-benchmarks (current-benchmark-registry)))]
              [calls '()]
              [factory (lambda (label value)
                         (set! calls (cons (list label value) calls))
                         (make-benchmark label (lambda (state) value) '()))]
              [initialize (lambda (enabled?)
                            (when enabled?
                              (instantiate-benchmark-template factory ([small 1] [large 2]))))])
         (initialize #f)
         (and (null? calls)
              (begin
                (initialize #t)
                (and (equal? (reverse calls) '((small 1) (large 2)))
                     (equal? (map benchmark-name
                                  (list-tail (benchmark-registry-benchmarks
                                               (current-benchmark-registry)) before))
                             '(factory/small factory/large))))))
      (environment '(cspp))))

  ;; Error: an empty batch must work as the sole expression in a lexical body.
  (guard (raised [else #f])
    (let ([before (benchmark-registry-benchmarks (current-benchmark-registry))])
      (and (eq? (eval
                  '(let ([factory (lambda (label) (error 'factory "must not be called"))])
                     (instantiate-benchmark-template factory ()))
                  (environment '(cspp)))
                (void))
           (eq? before (benchmark-registry-benchmarks (current-benchmark-registry)))))))

(mat benchmark-config-exposes-stopping-policy
  (guard (raised [else #f])
    (eval
      '(let* ([defaults (default-benchmark-config)]
              [stopping (benchmark-config-with defaults '((stop-on-error? . #t)))]
              [running (benchmark-config-with stopping
                         '((stop-on-error? . #f) (cost-center? . #t)))]
              [constructed (make-benchmark-config 0 1 0 1 #f #f #f #f #t)])
         (and (not (benchmark-config-stop-on-error? defaults))
              (benchmark-config-stop-on-error? stopping)
              (not (benchmark-config-stop-on-error? running))
              (benchmark-config-stop-on-error? constructed)))
      (environment '(cspp))))

  ;; Error: the public reader must reject non-config arguments before accessing record fields.
  (guard (raised [else (and (condition? raised) (who-condition? raised)
                           (eq? (condition-who raised) 'pcheck))])
    (eval '(benchmark-config-stop-on-error? #f) (environment '(cspp)))
    #f))
