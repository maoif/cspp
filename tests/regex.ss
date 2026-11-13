(import (cspp) (cspp regex))

(mat regex-basic
     (let* ([rx (string->regex "(ab)+")]
            [m (regex-search rx "xxababyy")])
       (and (regex? rx)
            (regex-match? m)
            (= 2 (regex-match-start-index m 0))
            (= 6 (regex-match-end-index m 0))
            (string=? "ab" (regex-match-substring m 1)))))

;; Error case: regex APIs reject values of the wrong type.
(mat regex-type-errors
     (guard (c [(error? c) #t] [else #f])
       (string->regex 1)
       #f)
     (guard (c [(error? c) #t] [else #f])
       (regex-search 1 "x")
       #f))

(mat regex-flags-and-names
     (let* ([pattern (sre->regex '(seq (submatch-named word (+ alphabetic))))]
            [match (regex-search pattern "abc")])
       (and (regex-matches? (string->regex "abc" '(i)) "ABC")
            (eq? pattern (regex-match-pattern match))
            (= 1 (regex-num-submatches pattern))
            (regex-match-valid-index? match 'word)
            (string=? "abc" (regex-match-substring match 'word)))))

(mat regex-bounds-and-unmatched
     (let ([match (regex-match (string->regex "(a)?b") "xxbyy" 2 3)])
       (and match
            (= 2 (regex-match-start-index match))
            (= 3 (regex-match-end-index match))
            (not (regex-match-substring match 1)))))

(mat regex-utilities
     (and (equal? '("12" "34") (regex-extract (string->regex "[0-9]+") "x12y34"))
          (equal? '("a" "b") (regex-split (string->regex ",") ",a,,b,"))
          (string=? "aXbX"
            (regex-replace/all (string->regex "[0-9]") "a1b2"
              (lambda (match)
                (if (regex-match? match) "X" "bad"))))
          (string=? "aXb2" (regex-replace (string->regex "[0-9]") "a1b2" "X"))
          (regex-matches? (string->regex (regex-quote "a.b")) "a.b")))

(mat regex-fold-stable-records
     (let* ([pattern (string->regex "[0-9]")]
            [matches (regex-fold pattern
                       (lambda (previous match acc) (cons match acc)) '() "a1b2")])
       (equal? '("2" "1") (map regex-match-substring matches))))

(mat regex-reusable-match
     (let* ([pattern (string->regex "[0-9]")]
            [match (regex-new-match pattern)])
       (and (eq? match (regex-search/matches pattern match "a2"))
            (string=? "2" (regex-match-substring match))
            (not (regex-search/matches pattern match "abc"))
            (not (regex-match-start-index match)))))

(mat regex-chunked
     (let* ([chunker (make-regex-chunker
                       (lambda (chunk) (and (pair? (cdr chunk)) (cdr chunk))) car)]
            [pattern (string->regex "abcd")]
            [match (regex-search/chunked pattern chunker '("xa" "bc" "dy"))])
       (and (regex-chunker? chunker)
            (string=? "abcd" (regex-match-substring match))
            (regex-match? (regex-match/chunked pattern chunker '("ab" "cd"))))))

;; Error cases: bad patterns, flags, bounds, capture indices, and callback results.
(mat regex-invalid-inputs
     (error? (string->regex "["))

     (error? (string->regex "a" '(unknown)))

     (error? (regex-search (string->regex "a") "a" -1 1))

     (error? (regex-match-substring (regex-match (string->regex "a") "a") 2))

     (error? (regex-replace/all (string->regex "a") "a" (lambda (match) 3))))
