(import (cspp) (cspp dlist))
(load "examples/common.ss")

(example-run!
 'dlist-example
 "Build and mutate a doubly-linked list, then print its values."
 '((values . "Comma-separated integers (default: 1,2,3)."))
 (lambda (ref)
   (let ([values (example-option-integer-list 'dlist-example ref 'values '(1 2 3))])
     ;; Construct a doubly-linked list from the selected values.
     (let ([items (apply dlist values)])
       ;; Add an item at the front and another at the back.
       (dlist-push! items 0)
       (dlist-push-back! items 4)
       ;; Replace the first item through the indexed mutation API.
       (dlist-set! items 0 -1)
       ;; Reverse the list in place to demonstrate bidirectional mutation.
       (dlist-reverse! items)
       ;; Convert the final dlist to a Scheme list for printing.
       (example-print (dlist->list items))))
   0)
 (command-line-arguments))
