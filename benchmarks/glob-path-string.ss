#!chezscheme
(import (cspp) (cspp glob))
(load "benchmarks/common.ss")

;; Run from the repository root: ./cspp --script=benchmarks/glob-path-string.ss
;; Five samples after three warmups; collect before each sample. Report total elapsed
;; milliseconds and allocated bytes for the stated repetition count, excluding setup.
(define measure
  (lambda (name repetitions thunk)
    (do ([i 0 (fx1+ i)]) ((fx= i 3)) (benchmark-consume (thunk)))
    (let ([samples
           (map (lambda (sample)
                  (collect)
                  (let ([before (statistics)])
                    (do ([i 0 (fx1+ i)])
                        ((fx= i repetitions))
                      (benchmark-consume (thunk)))
                    (let ([delta (sstats-difference (statistics) before)])
                      (cons (/ (benchmark-time-ns (sstats-real delta)) 1000000.0)
                            (sstats-bytes delta)))))
                (iota 5))])
      (printf "~a reps=~a median-real-ms=~,3f median-bytes=~a\n"
              name repetitions
              (list-ref (sort < (map car samples)) 2)
              (list-ref (sort < (map cdr samples)) 2)))))

(define base-path
  (path 'unix "one" "two" "three" "four" "five" "six" "seven" "eight"
        "nine" "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen"
        "sixteen" "seventeen" "eighteen" "nineteen" "twenty"))
(define text (make-string 8192 #\a))

(printf "~a ~a; release build; five samples\n" (scheme-version) (machine-type))
(measure 'path-add-component 5000
         (lambda () (path-add-component base-path "tail")))
(measure 'string-map 300 (lambda () (string-map char-upcase text)))
(measure 'string-for-each 300 (lambda () (string-for-each char-alphabetic? text)))

;; Fixed-arity callbacks keep their own argument handling out of traversal allocation.
;; Include four inputs to track the generic fallback alongside the specialized cases.
(let ([plain (case-lambda
               [(a) a]
               [(a b) (if (char=? a b) a b)]
               [(a b c) (if (char=? a b) a c)]
               [(a b c d) (if (char=? a b) c d)])]
      [indexed (case-lambda
                 [(index a) a]
                 [(index a b) (if (char=? a b) a b)]
                 [(index a b c) (if (char=? a b) a c)]
                 [(index a b c d) (if (char=? a b) c d)])])
  (for-each
   (lambda (name proc callback)
     (for-each
      (lambda (count)
        (let ([strings (make-list count text)])
          (measure (format "~a/~a" name count) 300
                   (lambda () (apply proc callback strings)))))
      '(1 2 3 4)))
   '(string-map string-map/i string-for-each string-for-each/i)
   (list string-map string-map/i string-for-each string-for-each/i)
   (list plain indexed plain indexed)))

(measure 'make-glob-unix 300
         (lambda () (make-glob 'unix "src/one/two/three/**/*.ss")))
(measure 'make-glob-windows 300
         (lambda () (make-glob 'windows "src\\one\\two\\three\\**\\*.ss")))
(measure 'glob-sort 30 (lambda () (glob "src/cspp/*.ss")))

;; Compare the original three-query identity with access through one stat snapshot.
(measure 'file-identity-three-queries 10000
         (lambda () (list (file-dev-major ".") (file-dev-minor ".") (file-inode "."))))
(measure 'file-identity-one-snapshot 10000
         (lambda ()
           (let ([stat (file-stat ".")])
             (list (file-stat-dev-major stat) (file-stat-dev-minor stat)
                   (file-stat-inode stat)))))

;; Include the link-following identity checks without traversing the repository's
;; varying build products. The fixture belongs to this process and is always removed.
(let ([root (format "glob-perf-~a" (get-process-id))])
  (dynamic-wind
    (lambda ()
      (mkdir root)
      (do ([i 0 (fx1+ i)])
          ((fx= i 40))
        (let ([directory (string-append root "/d" (number->string i))])
          (mkdir directory)
          (file-touch (string-append directory "/file.txt")))))
    (lambda ()
      (measure 'glob-follow 30
               (lambda () (glob* (string-append root "/**/*.txt") #t #f 'empty))))
    (lambda () (file-removetree root))))
