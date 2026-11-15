(import (cspp)
        (cspp format scheme))
(load "examples/common.ss")

(example-run!
 'scheme-example
 "Parse Scheme datums from a file and print the resulting list."
 '((file . "Required: path to the Scheme datum file to parse."))
 (lambda (ref)
   (let ([path (example-required 'scheme-example ref 'file)])
     ;; Parse all Scheme datums in this file.
     (example-print (parse-scheme-file path)))
   0)
 (command-line-arguments))
