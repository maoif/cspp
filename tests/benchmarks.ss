(import (cspp)
        (cspp benchmark))
(load "../benchmarks/common.ss")

(define benchmark-summary-has?
  (lambda (summary key)
    (and (assq key summary) #t)))

(mat benchmark-example-report-summary
     (let* ([benchmark
             (make-benchmark
              'example-summary
              (lambda (state) (benchmark-do-not-optimize 42))
              '((warmup . 0) (samples . 2) (min-time . 0) (max-iterations . 1)))]
            [config (benchmark-config-with
                     (default-benchmark-config)
                     '((warmup . 0) (samples . 2) (min-time . 0)
                       (max-iterations . 1) (output . #f) (reporter . #f)))]
            [results (benchmark-run benchmark config)]
            [result (car results)]
            [summary (benchmark-result-summary result)]
            [cpu (cdr (assq 'cpu-ns summary))]
            [real (cdr (assq 'real-ns summary))]
            [bytes (cdr (assq 'bytes summary))])
       (and (eq? (benchmark-result-name result) 'example-summary)
            (= (length (benchmark-result-samples result)) 2)
            (benchmark-summary-has? cpu 'median)
            (benchmark-summary-has? cpu 'mean)
            (benchmark-summary-has? cpu 'min)
            (benchmark-summary-has? cpu 'max)
            (benchmark-summary-has? cpu 'stddev)
            (benchmark-summary-has? cpu 'confidence-interval)
            (benchmark-summary-has? real 'median)
            (benchmark-summary-has? bytes 'median)
            (if (benchmark-sample-sstats
                 (car (benchmark-result-samples result)))
                #t
                #f))))

(mat benchmark-shared-table-rendering

     (let ([text
             (with-output-to-string
               (lambda ()
                 (benchmark-print-table! "Counts" '(("Count" . right)) '(("42")))))])
       (and (string-contains? text "Count") (string-contains? text "42"))))
