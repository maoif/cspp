(import (cspp)
        (cspp format scheme)
        (cspp navigator))
(load "examples/common.ss")

(example-run!
 'navigator-example
 "Select and transform values in a nested Scheme datum."
 '((value . "Scheme datum with a scores key containing numbers (default: ((name . Ada) (scores . #(2 4 6))))."))
 (lambda (ref)
   (let* ([text (or (ref 'value) "((name . Ada) (scores . #(2 4 6)))")]
          [value (parse-scheme-datum text)]
          ;; Compose a key lookup with traversal of every score.
          [path (nav/path (nav/key 'scores) nav/all)])
     ;; Select all values under the scores key.
     (let ([selected (nav-select path value)])
       ;; Transform selected scores by doubling each numeric focus.
       (example-print
        (list selected
              ;; Transform all selected values without mutating the input.
              (nav-transform path (lambda (score) (* 2 score)) value)
              ;; Describe the navigation path for interactive discovery.
              (nav-explain path)))))
   0)
 (command-line-arguments))
