(import (cspp))


(mat string-edit-distance
     (= 0 (edit-distance "" ""))
     (= 3 (edit-distance "kitten" "sitting"))
     (= 2 (edit-distance "flaw" "lawn"))
     (error? (edit-distance 'kitten "sitting")))

(mat string-sequence-procedures
     (let ([seen '()])
       (string-for-each (lambda (ch) (set! seen (cons ch seen))) "ab")
       (equal? '(#\b #\a) seen))
     (let ([seen '()])
       (string-for-each/i
        (lambda (i ch) (set! seen (cons (cons i ch) seen)))
        "ab")
       (equal? '((1 . #\b) (0 . #\a)) seen))
     (string=? "AB" (string-map char-upcase "ab"))
     (string=? "AbCd"
               (string-map/i
                (lambda (i ch) (if (even? i) (char-upcase ch) ch))
                "abcd"))
     (string=? "axby"
               (string-map (lambda (a b) (if (char=? a b) a b)) "aabb" "axby"))
     (error? (string-map (lambda (ch) 1) "a"))
     (error? (string-map char-upcase "a" "ab"))
     (error? (string-for-each values "a" 1)))

(mat string-slice
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

     (= 0 (string-search "" ""))
     (not (string-search "" "1"))
     (not (string-search "" "13"))
     (= 0 (string-search "1" "1"))
     (= 1 (string-search "21" "1"))

     )


(mat string-search-all

     (not (string-search-all "" "1"))
     (equal? '(0) (string-search-all "" ""))
     (equal? '(0 1 2 3) (string-search-all "1111" "1"))
     (equal? '(0 1 2) (string-search-all "1111" "11"))
     (equal? '(0 2) (string-search-all "1212" "12"))

     )


(mat string-contains?

     (string-contains? "" "")
     (string-contains? "1" "")
     (string-contains? "hfjdhfkjsdh" "")
     (not (string-contains? "1" "2"))
     (not (string-contains? "" "2"))

     )


(mat string-startswith?

     (string-startswith? "" "")
     (string-startswith? "qwe" "")
     (string-startswith? "qwe" "q")
     (string-startswith? "qwe" "qw")
     (string-startswith? "qwe" "qwe")
     (not (string-startswith? "qwe" "Q"))
     (not (string-startswith? "qwe" "qW"))

     )


(mat string-endswith?

     (string-endswith? "" "")
     (string-endswith? "qwe" "")
     (string-endswith? "qwe" "e")
     (string-endswith? "qwe" "we")
     (string-endswith? "qwe" "qwe")
     (not (string-endswith? "qwe" "E"))
     (not (string-endswith? "qwe" "wE"))

     )


(mat string-split

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
             (string-split "Lorem Ipsum is simply dummy text of the printing and typesetting industry" #\space))

     )


(mat string-trim

     (error? (string-trim 123))
     (error? (string-trim "  123  " 123))

     (equal? "" (string-trim ""))
     (equal? "" (string-trim "   "))

     (equal? "x" (string-trim "x"))
     (equal? "x" (string-trim "  x"))
     (equal? "x" (string-trim "x  "))

     (equal? "xyz" (string-trim "xyz"))
     (equal? "xyz" (string-trim "  xyz"))
     (equal? "xyz" (string-trim "xyz  "))

     )


(mat string-empty?

     (error? (string-empty? 123))

     (string-empty? "")
     (not (string-empty? "123"))

     )
