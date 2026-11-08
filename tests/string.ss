(import (cspp))

;; Procedure, indexed?, mapping?, reverse?. Reuse the same families across the matrices.
(define $string-traversals
  (list (list string-map #f #t #f) (list string-map/i #t #t #f)
        (list string-for-each #f #f #f) (list string-for-each/i #t #f #f)
        (list string-map-rev #f #t #t) (list string-map/i-rev #t #t #t)
        (list string-for-each-rev #f #f #t) (list string-for-each/i-rev #t #f #t)))

(define $string-traversal-procedures (map car $string-traversals))

(mat string-result-list-ownership
     ;; Mutating one result must not affect a later call, including fast paths.
     (let ([indexes (string-search-all "abc" "abc")])
       (set-car! indexes 99)
       (equal? '(0) (string-search-all "abc" "abc")))

     (let ([indexes (string-search-all "" "")])
       (set-car! indexes 99)
       (equal? '(0) (string-search-all "abc" "")))

     (let ([fields (string-split "" #\,)])
       (set-car! fields "changed")
       (and (equal? '("") (string-split "" #\,))
            (equal? '("") (string-split "" "::")))))

(mat string-edit-distance
     (for-all
      (lambda (fixture)
        (let ([left (car fixture)] [right (cadr fixture)] [expected (caddr fixture)])
          (and (= expected (edit-distance left right))
               (= expected (edit-distance right left)))))
      '(("" "" 0) ("" "aλ🚀" 3) ("kitten" "sitting" 3) ("flaw" "lawn" 2)
        ("abc" "abc" 0) ("abXcd" "abYcd" 1) ("abcd" "abXYZcd" 3)
        ("prefixabcTAIL" "prefixxyzTAIL" 3) ("same" "sameextra" 5)))

     ;; Error cases: reject non-strings, including when the other operand is empty.
     (error? (edit-distance 'kitten "sitting"))

     (error? (edit-distance "" 'sitting))

     (let ([left (make-string 4096 #\a)] [right (make-string 4096 #\a)])
       (collect)
       (let* ([before (statistics)] [distance (edit-distance left right)]
              [allocated (sstats-bytes (sstats-difference (statistics) before))])
         ;; Equal strings need no dynamic-programming row, even when separately allocated.
         (and (= 0 distance) (< allocated 1024)))))

(mat string-traversal-arities
     ;; Check character order, index placement, results, and the four-string fallback.
     (for-all
      (lambda (fixture)
        (let ([strings (car fixture)] [chars (cadr fixture)] [indexed (caddr fixture)])
          (for-all
           (lambda (entry)
             (let* ([proc (car entry)] [indexed? (cadr entry)] [mapping? (caddr entry)]
                    [reverse? (cadddr entry)]
                    [seen (make-list-builder)]
                    [callback (if indexed?
                                  (lambda (index . chars)
                                    (seen (cons index chars))
                                    (char-upcase (car chars)))
                                  (lambda chars
                                    (seen chars)
                                    (char-upcase (car chars))))]
                    [result (apply proc callback strings)])
               (and (equal? (seen)
                            (let ([expected (if indexed? indexed chars)])
                              (if reverse? (reverse expected) expected)))
                    (if mapping?
                        (string=? result (if reverse? "BA" "AB"))
                        (eq? result (void))))))
           $string-traversals)))
      '((("ab") ((#\a) (#\b)) ((0 #\a) (1 #\b)))
        (("ab" "cd") ((#\a #\c) (#\b #\d)) ((0 #\a #\c) (1 #\b #\d)))
        (("ab" "cd" "ef") ((#\a #\c #\e) (#\b #\d #\f))
         ((0 #\a #\c #\e) (1 #\b #\d #\f)))
        (("ab" "cd" "ef" "gh") ((#\a #\c #\e #\g) (#\b #\d #\f #\h))
         ((0 #\a #\c #\e #\g) (1 #\b #\d #\f #\h))))))

(mat string-traversal-empty-and-validation
     (for-all
      (lambda (entry)
        (let ([proc (car entry)] [mapping? (caddr entry)])
          (for-all
           (lambda (strings)
             (let* ([called? #f]
                    [result (apply proc (lambda args (set! called? #t) #\a) strings)])
               (and (not called?)
                    (if mapping? (string=? result "") (eq? result (void))))))
           '(("") ("" "") ("" "" "") ("" "" "" "")))))
      $string-traversals)

     ;; Each dispatch clause must reject invalid callbacks, including empty inputs.
     (for-all
      (lambda (proc)
        (for-all (lambda (strings)
                   (guard (condition [(or (error? condition) (violation? condition)) #t])
                     (apply proc 'bad strings)
                     #f))
                 '(("") ("" "") ("" "" "") ("" "" "" ""))))
      $string-traversal-procedures)

     ;; Validate every string argument before reading lengths or calling the callback.
     (for-all
      (lambda (proc)
        (for-all (lambda (strings)
                   (guard (condition [(or (error? condition) (violation? condition)) #t])
                     (apply proc values strings)
                     #f))
                 '((bad) (bad "") ("" bad) (bad "" "") ("" bad "") ("" "" bad)
                   ("" "" "" bad))))
      $string-traversal-procedures)

     ;; Mismatched lengths must fail before the callback has any side effects.
     (for-all
      (lambda (proc)
        (for-all
         (lambda (strings)
           (let ([called? #f])
             (and (guard (condition [(or (error? condition) (violation? condition)) #t])
                    (apply proc (lambda args (set! called? #t) #\a) strings)
                    #f)
                  (not called?))))
         '(("a" "") ("" "a") ("a" "a" "") ("a" "" "a")
           ("" "a" "a") ("a" "a" "a" ""))))
      $string-traversal-procedures)

     ;; Mapping must reject non-character callback results at every supported arity.
     (for-all
      (lambda (proc)
        (for-all (lambda (strings)
                   (guard (condition [(or (error? condition) (violation? condition)) #t])
                     (apply proc (lambda args #f) strings)
                     #f))
                 '(("a") ("a" "b") ("a" "b" "c") ("a" "b" "c" "d"))))
      (list string-map string-map/i string-map-rev string-map/i-rev)))

(mat string-small-arity-traversal-allocation
     ;; Common arities must not allocate temporary character lists per iteration.
     (let ([source (make-string 8192 #\a)]
           [callback (case-lambda
                       [(a) #\a]
                       [(a b) #\a]
                       [(a b c) #\a]
                       [(a b c d) #\a])])
       (for-all
        (lambda (proc)
          (for-all
           (lambda (strings)
             (let* ([before (statistics)]
                    [result (apply proc callback strings)]
                    [allocated (sstats-bytes (sstats-difference (statistics) before))])
               (and (or (eq? result (void)) (string=? result source))
                    (< allocated 100000))))
           (list (list source) (list source source) (list source source source))))
        $string-traversal-procedures)))

(mat string-reverse-mapping
     ;; Indexed reverse mapping passes source indexes and writes in traversal order.
     (string=? "CaA"
               (string-map/i-rev
                (lambda (index char) (if (even? index) (char-upcase char) char))
                "aac"))

     ;; A single character must be visited exactly once, with its original index.
     (let ([seen (make-list-builder)])
       (string-for-each/i-rev (lambda (index char) (seen (cons index char))) "λ")
       (equal? (seen) '((0 . #\λ))))

     ;; Reverse mapping returns independent mutable storage and preserves the input.
     (let* ([source (string-copy "aλ🚀")]
            [result (string-map-rev values source)])
       (string-set! result 0 #\x)
       (and (string=? source "aλ🚀") (string=? result "xλa")))

     ;; Read each source character at callback time, without making reversed input copies.
     (let ([source (string-copy "abc")])
       (string=? "cbz"
                 (string-map-rev
                  (lambda (char)
                    (when (char=? char #\c) (string-set! source 0 #\z))
                    char)
                  source))))

(mat string-mapping-callback-results
     (string=? "AbCd"
               (string-map/i
                (lambda (index char) (if (even? index) (char-upcase char) char))
                "abcd"))

     (string=? "axby"
               (string-map (lambda (left right) (if (char=? left right) left right))
                           "aabb" "axby")))

(mat string-slice
     ;; Extreme steps must terminate before advancing an index beyond the fixnum range.
     (string=? "b" (string-slice "abc" 1 3 (most-positive-fixnum)))

     (string=? "c" (string-slice "abc" 2 -4 (most-negative-fixnum)))

     (string=? "b" (string-slice "abc" 1 3 (fx1- (most-positive-fixnum))))

     (string=? "01234" (string-slice "0123456789" 5))
     (string=? "864" (string-slice "0123456789" 8 2 -2))
     (string=? "" (string-slice "0123456789" 2 9 -1))
     (string=? "89" (string-slice "0123456789" -2 20))
     (string=? "" (string-slice "abc" 9 10))
     (string=? "cba" (string-slice "abc" 3 -4 -1))
     (string=? "" (string-slice "abc" -9 -4 -1))
     (error? (string-slice 123 1))
     (error? (string-slice "abc" 0 2 0)))

(mat string-search

     (= 2 (string-search (string-append (make-string 19 #\a) "b")
                          (string-append (make-string 17 #\a) "b")))

     (not (string-search (make-string 65536 #\a)
                          (string-append (make-string 1024 #\a) "b")))

     (= 6 (string-search "abababababababababababac" "ababababababababac"))

     (= 1 (string-search "λababababababababac" "ababababababababac"))

     (= 0 (string-search "" ""))
     (not (string-search "" "1"))
     (= 0 (string-search "1" "1"))
     (= 1 (string-search "21" "1")))

(mat string-search-all

     (equal? (iota 48) (string-search-all (make-string 64 #\a) (make-string 17 #\a)))

     (equal? '(0 2 4) (string-search-all "abababababababababababac" "ababababababababab"))

     (not (string-search-all "" "1"))
     (equal? '(0) (string-search-all "" ""))
     (equal? '(0 1 2 3) (string-search-all "1111" "1"))
     (equal? '(0 1 2) (string-search-all "1111" "11"))
     (equal? '(0 2) (string-search-all "1212" "12")))

(mat string-contains?

     (string-contains? "" "")
     (string-contains? "hfjdhfkjsdh" "")
     (not (string-contains? "1" "2"))
     (not (string-contains? "" "2")))

(mat string-startswith?

     (string-startswith? "" "")
     (string-startswith? "qwe" "")
     (string-startswith? "qwe" "q")
     (string-startswith? "qwe" "qw")
     (string-startswith? "qwe" "qwe")
     (not (string-startswith? "qwe" "Q"))
     (not (string-startswith? "qwe" "qW")))

(mat string-endswith?

     (string-endswith? "" "")
     (string-endswith? "qwe" "")
     (string-endswith? "qwe" "e")
     (string-endswith? "qwe" "we")
     (string-endswith? "qwe" "qwe")
     (not (string-endswith? "qwe" "E"))
     (not (string-endswith? "qwe" "wE")))

(mat string-split

     (let ([needle (make-string 17 #\a)])
       (equal? '("" "b" "c")
               (string-split (string-append needle "b" needle "c") needle)))

     ;; type error
     (error? (string-split #\a #\a))
     (error? (string-split "string" 1))
     ;; empty string delimiters are invalid
     (error? (string-split "string" ""))
     (error? (string-split "" ""))

     ;; empty string
     (equal? '("") (string-split "" "foo"))
     (equal? '("") (string-split "" #\?))

     ;; delimiter on the side of the string
     (equal? '("" "quick" "fat" "fox" "") (string-split "@quick@fat@fox@" #\@))
     (equal? '("quick" "fat" "fox" "") (string-split "quick@fat@fox@" #\@))
     (equal? '("" "quick" "fat" "fox") (string-split "@quick@fat@fox" #\@))

     (equal? '("" "quick" "fat" "fox" "") (string-split "@@quick@@fat@@fox@@" "@@"))
     (equal? '("quick" "fat" "fox" "") (string-split "quick@@fat@@fox@@" "@@"))
     (equal? '("" "quick" "fat" "fox") (string-split "@@quick@@fat@@fox" "@@"))
     (equal? '("" "a") (string-split "aaa" "aa"))
     (equal? '("" "" "") (string-split "aaaa" "aa"))

     (equal? '("Lorem" "Ipsum" "is" "simply" "dummy" "text" "of" "the" "printing" "and" "typesetting" "industry")
             (string-split "Lorem Ipsum is simply dummy text of the printing and typesetting industry" #\space)))

(mat string-trim

     (error? (string-trim 123))
     (error? (string-trim "  123  " 123))

     (equal? "" (string-trim ""))
     (equal? "" (string-trim "   "))

     (equal? "xyz" (string-trim "xyz"))
     (equal? "xyz" (string-trim "  xyz"))
     (equal? "xyz" (string-trim "xyz  ")))

(mat string-empty?

     (error? (string-empty? 123))

     (string-empty? "")
     (not (string-empty? "123")))
