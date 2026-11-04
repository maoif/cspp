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
(measure 'make-glob-unix 300
         (lambda () (make-glob 'unix "src/one/two/three/**/*.ss")))
(measure 'make-glob-windows 300
         (lambda () (make-glob 'windows "src\\one\\two\\three\\**\\*.ss")))
(measure 'glob-sort 30 (lambda () (glob "src/cspp/*.ss")))

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
