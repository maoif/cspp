(import (cspp))
(load "benchmarks/common.ss")

;; Run from the repository root: ./cspp --script=benchmarks/parser-xml.ss
;; Build the input before measuring; compare total allocation against the baseline library.
(define measure-xml-text
  (lambda (count)
    (let ([text (string-append "<a>" (apply string-append (make-list count "&amp;")) "</a>")])
      (collect)
      (let* ([before (statistics)]
             [started (real-time)]
             [document (parse-xml text)]
             [elapsed (- (real-time) started)]
             [allocated (sstats-bytes (sstats-difference (statistics) before))]
             [children (xml-element-children (xml-document-root document))])
        (unless (and (= 1 (vector-length children))
                     (string=? (make-string count #\&)
                               (xml-text-value (vector-ref children 0))))
          (error 'parser-xml-benchmark "incorrect merged text"))
        (map number->string (list count (string-length text) allocated elapsed))))))

(printf "~a ~a\n" (scheme-version) (machine-type))
(benchmark-print-table!
 "XML adjacent entity references"
 '(("references" . right) ("input chars" . right)
   ("allocated bytes" . right) ("elapsed ms" . right))
 (map measure-xml-text '(2000 4000 8000 16000)))
