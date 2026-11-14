(import (cspp)
        (cspp glob))
(load "examples/common.ss")

(example-run!
 'glob-example
 "Match a path, expand files, and consume a lazy glob iterator."
 '((pattern . "Glob pattern relative to the current directory (default: examples/**/*.ss).")
   (limit . "Maximum lazy iterator results, a nonnegative integer (default: 10); eager expansion is also printed."))
 (lambda (ref)
   (let* ([pattern (or (ref 'pattern) "examples/**/*.ss")]
          [limit (example-option-natural 'glob-example ref 'limit 10)]
          [compiled (make-glob pattern)])
     ;; Check a candidate path against a compiled glob.
     (let ([matches? (glob-match? compiled "examples/common.ss")])
       ;; Expand the pattern with directory matches enabled.
       (let ([paths (glob* pattern #f #t 'empty)])
         ;; Iterate lazily and finalize streams after the requested prefix.
         (let ([iterator (glob->iter pattern)])
           (dynamic-wind
             void
             (lambda ()
               (let loop ([count limit] [out '()])
                 (if (zero? count)
                     (example-print (list matches? paths (reverse out)))
                     (let ([path (iter-next! iterator)])
                       (if (iter-end? path)
                           (example-print (list matches? paths (reverse out)))
                           (loop (- count 1) (cons path out)))))))
             (lambda () (iter-finalize! iterator)))))))
   0)
 (command-line-arguments))
