(import (cspp)
        (cspp format elf))
(load "examples/common.ss")

(example-run!
 'elf-example
 "Parse an ELF file and print its record value."
 '((file . "Required: path to the ELF binary file to parse."))
 (lambda (ref)
   (let ([path (example-required 'elf-example ref 'file)])
     ;; Parse ELF headers, sections, and symbols from this binary file.
     (example-print (parse-elf-file path)))
   0)
 (command-line-arguments))
