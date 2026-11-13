(import (cspp) (cspp format json5))
(load "examples/common.ss")

(example-run!
 'json-example
 "Parse a JSON5 file and print its document value."
 '((file . "Required: path to the JSON or JSON5 file to parse."))
 (lambda (ref)
   (let ([path (example-required 'json-example ref 'file)])
     ;; Parse this JSON5 file into a document value.
     (example-print (parse-json5-file path)))
   0)
 (command-line-arguments))
