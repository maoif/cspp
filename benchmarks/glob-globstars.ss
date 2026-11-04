#!chezscheme
(import (cspp))
(load "benchmarks/common.ss")

;; Run from the root: ./cspp --script=benchmarks/glob-globstars.ss
;; Three warmups, five samples, collect before each sample; setup is excluded.
;; Report median elapsed milliseconds and allocated bytes for 40 expansions.
(define measure
  (lambda (name thunk)
    (do ([i 0 (fx1+ i)]) ((fx= i 3)) (benchmark-consume (thunk)))
    (let ([samples
           (map (lambda (sample)
                  (collect)
                  (let ([before (statistics)])
                    (do ([i 0 (fx1+ i)]) ((fx= i 40)) (benchmark-consume (thunk)))
                    (let ([delta (sstats-difference (statistics) before)])
                      (cons (/ (benchmark-time-ns (sstats-real delta)) 1000000.0)
                            (sstats-bytes delta)))))
                (iota 5))])
      (printf "~a reps=40 median-real-ms=~,3f median-bytes=~a\n" name
              (list-ref (sort < (map car samples)) 2)
              (list-ref (sort < (map cdr samples)) 2)))))

(printf "~a ~a; release build; five samples\n" (scheme-version) (machine-type))
(let* ([root (format "glob-globstar-perf-~a" (get-process-id))]
       [leaf root])
  (dynamic-wind
    (lambda ()
      (mkdir root)
      (do ([i 0 (fx1+ i)]) ((fx= i 9))
        (set! leaf (string-append leaf "/d"))
        (mkdir leaf))
      (file-touch (string-append leaf "/x")))
    (lambda ()
      (for-each
       (lambda (count)
         (let ([pattern (string-append root "/"
                                       (apply string-append (make-list count "**/")) "x")])
           (unless (equal? (glob pattern) (list (string-append leaf "/x")))
             (error 'glob-globstars "unexpected expansion" pattern))
           (measure (format "globstars=~a" count) (lambda () (glob pattern)))))
       '(1 3 5)))
    (lambda () (file-removetree root))))
