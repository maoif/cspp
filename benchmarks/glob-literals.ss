#!chezscheme
(import (cspp))
(load "benchmarks/common.ss")

;; Run from the root: ./cspp --script=benchmarks/glob-literals.ss
;; Three warmups, five samples, collect before each sample; setup is excluded.
;; Report median total elapsed milliseconds and allocated bytes for 300 expansions.
(define measure
  (lambda (name thunk)
    (do ([i 0 (fx1+ i)]) ((fx= i 3)) (benchmark-consume (thunk)))
    (let ([samples
           (map (lambda (sample)
                  (collect)
                  (let ([before (statistics)])
                    (do ([i 0 (fx1+ i)]) ((fx= i 300)) (benchmark-consume (thunk)))
                    (let ([delta (sstats-difference (statistics) before)])
                      (cons (/ (benchmark-time-ns (sstats-real delta)) 1000000.0)
                            (sstats-bytes delta)))))
                (iota 5))])
      (printf "~a reps=300 median-real-ms=~,3f median-bytes=~a\n" name
              (list-ref (sort < (map car samples)) 2)
              (list-ref (sort < (map cdr samples)) 2)))))

(printf "~a ~a; release build; five samples\n" (scheme-version) (machine-type))
(let* ([root (format "glob-literal-perf-~a" (get-process-id))]
       [directory (string-append root "/many")]
       [wanted (string-append directory "/wanted.txt")]
       [missing (string-append directory "/missing.txt")]
       [pattern (string-append directory "/wanted.*")])
  (dynamic-wind
    (lambda ()
      (mkdir root)
      (mkdir directory)
      (do ([i 0 (fx1+ i)]) ((fx= i 512))
        (file-touch (string-append directory "/sibling-" (number->string i))))
      (file-touch wanted))
    (lambda ()
      (measure 'literal-hit (lambda () (glob wanted)))
      (measure 'literal-miss (lambda () (glob missing)))
      (measure 'literal-prefix-wildcard (lambda () (glob pattern)))
      ;; Windows flavor still needs case-insensitive directory lookup on Unix hosts.
      (measure 'windows-literal-hit (lambda () (glob 'windows wanted))))
    (lambda () (file-removetree root))))
