(import (cspp) (cspp format toml))
(load "examples/common.ss")

(example-run!
 'toml-example
 "Parse a TOML file and print its document value."
 '((file . "Required: path to the TOML file to parse."))
 (lambda (ref)
   (let ([path (example-required 'toml-example ref 'file)])
     ;; Parse the selected TOML file into a document value.
     (example-print (parse-toml-file path)))
   0)
 (command-line-arguments))
