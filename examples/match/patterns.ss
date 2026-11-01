(import (cspp))
(load "examples/common.ss")

(example-run!
 'match-example
 "Explore structural patterns, extensions, recursive matching, and pattern bindings."
 '((value . "Scheme command datum to match (default: (move 3 4))."))
 (lambda (ref)
   (let ([command (parse-scheme-datum (or (ref 'value) "(move 3 4)"))])
;;;;===----------------------------------------------------------------------===
;;;; Structural patterns and literal data
;;;;===----------------------------------------------------------------------===
     ;; Bare symbols are literals, comma introduces captures, and _ ignores a value.
     ;; A condition sits between the pattern and its single result expression.
     (example-print
      (list 'command
            (match command
              [(move ,x ,y) (and (number? x) (number? y)) (list 'destination x y)]
              [(say ,message) (string? message) message]
              [(ignore _) 'ignored]
              [else 'unknown-command])))

     ;; A dotted pattern captures the remaining tail without requiring a proper list.
     (example-print
      (list 'dotted-tail
            (match '(name . Ada) [(,key . ,value) (list key value)])))

     ;; Numeric, boolean, character, string, and symbol literals match directly.
     ;; A predicate condition handles a whole family of values, such as bytevectors.
     (example-print
      (list 'literal-types
            (map (mlambda
                   [42 'number] [#t 'boolean] [#\A 'character]
                   ["Ada" 'string] [ready 'symbol]
                   [,bytes (bytevector? bytes) (bytevector->u8-list bytes)]
                   [else 'other])
                 (list 42 #t #\A "Ada" 'ready #vu8(1 2 3)))))

     ;; Typed vectors use predicate conditions and their own access/conversion procedures.
     (example-print
      (list 'typed-vectors
            (map (mlambda
                   [,items (fxvector? items) (list 'fixnums (fxvector->list items))]
                   [,items (flvector? items) (list 'flonums (flvector->list items))])
                 (list (fxvector 1 2 3) (flvector 1.5 2.5)))))

     ;; Repetition collects each capture into a list, including inside vectors.
     (example-print
      (list 'repeated-pairs
            (match '((Ada . 10) (Grace . 20))
              [((,name . ,score) ...) (list name score)])))
     (example-print
      (list 'vector-with-suffix
            (match '#(scores 10 20 30 end)
              [#(scores ,score ... end) score])))

     ;; A backreference compares against an earlier capture. #` also captures the whole value.
     (example-print
      (list 'backreference
            (match '(Ada Ada)
              [(#`whole (,name `name)) (list 'same name whole)]
              [else 'different])))

;;;;===----------------------------------------------------------------------===
;;;; Records and datatype variants
;;;;===----------------------------------------------------------------------===

     ;; Records can select named fields or destructure all fields in declaration order.
     (let ()
       (record Point (x y))
       (let ([point (Point 3 4)])
         (example-print
          (list 'record-fields
                (match point [,(record Point :fields (y ,height)) height])))
         (example-print
          (list 'record-values
                (match point [,(record Point :values ,x ,y) (list x y)])))))

     ;; Datatype patterns select a particular variant, including singleton variants.
     (let ()
       (datatype Reply [Ok value] [Missing])
       (example-print
        (list 'datatype
              (map (mlambda
                     [,(datatype Reply Ok :values ,value) (list 'found value)]
                     [,(datatype Reply Missing) 'not-found])
                   (list (Ok 42) Missing)))))

     ;; Named datatype fields may be selected in any order and nested inside other patterns.
     ;; A field predicate validates construction; the clause condition selects a value range.
     (let ()
       (datatype Message
         [Text (sender :predicate string?) (body :predicate string?)]
         [Reading (sensor :predicate symbol?) (value :predicate real?)]
         [End])
       (let ([message* (list (Text "Ada" "hello") (Reading 'room 24) End)])
         (example-print
          (list 'datatype-named-fields
                (match (car message*)
                  [,(datatype Message Text :fields (body ,body) (sender ,sender))
                   (list sender body)])))
         (example-print
          (list 'datatype-nested-vector
                (match (list->vector message*)
                  [#(,(datatype Message Text :fields (sender ,sender))
                     ,(datatype Message Reading :values ,sensor ,value)
                     ,(datatype Message End))
                   (> value 20)
                   (list sender sensor 'warm)])))
         ;; Repetition collects captures from a variant pattern, just like list patterns.
         (example-print
          (list 'datatype-repetition
                (match (list (Reading 'room 24) (Reading 'outside 12))
                  [(,(datatype Message Reading :values ,sensor ,value) ...)
                   (list sensor value)])))
         ;; An empty :fields pattern tests the variant without extracting its fields.
         (example-print
          (list 'datatype-variant-check
                (match (car message*)
                  [,(datatype Message Text :fields) 'text]
                  [else 'other])))))

     ;; Native Chez records also work, using their declared type name in the pattern.
     (let ()
       (define-record-type packet (fields kind payload))
       (example-print
        (list 'native-record
              (match (make-packet 'samples '#(10 20 30))
                [,(record packet :fields (payload #(,sample ...)) (kind samples))
                 sample]))))

;;;;===----------------------------------------------------------------------===
;;;; Hash, box, and regular expression extensions
;;;;===----------------------------------------------------------------------===

     ;; Hash keys are literal data; box patterns unwrap the value stored in a box.
     (let ([settings (make-eq-hashtable)])
       (hashtable-set! settings 'name "Ada")
       (hashtable-set! settings 'retries (box 3))
       (example-print
        (list 'hash-and-box
              (match settings
                [,(hash (name ,name) (retries ,(box ,retries)))
                 (list name retries)]))))

     ;; Regex group zero captures the full match; numbered groups capture subexpressions.
     (example-print
      (list 'regex
            (match "Ada:42"
              [,(regex "^([A-Za-z]+):([0-9]+)$" (1 ,name) (2 ,score))
               (list name (string->number score))])))

;;;;===----------------------------------------------------------------------===
;;;; Catamorphisms: one result, multiple results, explicit procedures, and effects
;;;;===----------------------------------------------------------------------===

     ;; Catamorphism: ,@ recursively applies the same clauses and binds the returned result.
     ;; Here each child expression is evaluated before the parent adds or multiplies it.
     (example-print
      (list 'recursive-expression
            (match '(add (multiply 2 3) (add 4 5))
              [(add ,@left ,@right) (+ left right)]
              [(multiply ,@left ,@right) (* left right)]
              [,value (number? value) value])))

     ;; Catamorphisms can recurse through datatype fields, with several returned values.
     ;; Each subtree returns its sum and leaf count; no explicit recursive call is needed.
     (let ()
       (datatype Tree
         [Empty]
         [Leaf (value :predicate number?)]
         [Branch (left :predicate Tree?) (right :predicate Tree?)])
       (let ([tree (Branch (Leaf 10) (Branch Empty (Leaf 20)))])
         (example-print
          (list 'datatype-catamorphism
                (call-with-values
                  (lambda ()
                    (match tree
                      [,(datatype Tree Empty) (values 0 0)]
                      [,(datatype Tree Leaf :values ,value) (values value 1)]
                      [,(datatype Tree Branch
                          :fields (left ,@[,left-sum ,left-count])
                                  (right ,@[,right-sum ,right-count]))
                       (values (+ left-sum right-sum) (+ left-count right-count))]))
                  list)))))

     ;; Repeated catamorphisms collect each returned value into its own list.
     (example-print
      (list 'repeated-catamorphism
            (call-with-values
              (lambda ()
                (match '(1 (2 3) (4 (5)))
                  [() (values 0 1)]
                  [(,@[,sum ,product] ...) (values (apply + sum) (apply * product))]
                  [,value (number? value) (values value value)]))
              list)))

     ;; An explicit procedure replaces recursive matching for the selected subvalue.
     ;; Here the procedure returns two values for each string in the input vector.
     (let ([parse-score (lambda (text) (values text (string->number text)))])
       (example-print
        (list 'procedure-catamorphism
              (match '#("10" "20" "30")
                [#(,@[parse-score -> ,text ,score] ...) (list text score)]))))

     ;; ,@[] recursively visits children and discards their return values.
     ;; The builder records leaves from left to right, demonstrating effects-only traversal.
     (let ([leaves (make-list-builder)])
       (match '(branch (leaf Ada) (branch (leaf Grace) (leaf Linus)))
         [(branch ,@[] ,@[]) (void)]
         [(leaf ,name) (leaves name)])
       (example-print (list 'effect-catamorphism (leaves))))

;;;;===----------------------------------------------------------------------===
;;;; Matching procedures and pattern bindings
;;;;===----------------------------------------------------------------------===

     ;; mlambda matches one input; mlambda+ matches its entire argument list.
     (example-print
      (list 'matching-procedures
            (map (mlambda [(,name . ,score) (list name (+ score 1))])
                 '((Ada . 10) (Grace . 20)))
            ((mlambda+ [(,first ,rest ...) (cons first rest)]) 'Ada 10 20)))

     ;; mlet evaluates independent inputs in parallel and destructures each one.
     (example-print
      (list 'parallel-bindings
            (mlet ([(,name . ,score) '(Ada . 10)]
                   [#(,bonus ,multiplier) '#(2 3)])
              (list name (* (+ score bonus) multiplier)))))

     ;; mlet* lets later input expressions refer to captures from earlier bindings.
     (example-print
      (list 'sequential-bindings
            (mlet* ([(,name . ,scores) '(Ada 10 20 30)]
                    [(,first . ,remaining) scores])
              (list name first remaining)))))
   0)
 (command-line-arguments))
