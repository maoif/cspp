;; Included by the registered scheme parser suite; format-specific regressions.

(mat parser-scheme-regressions

     (equal? '(a . #\b) (parse-scheme-datum "(a . #\\b)"))

     (equal? '(a b . #\)) (parse-scheme-datum "[a b . #\\)]"))

     (equal? '((a . #f) (a . ()) (a b) #(a #\b))
             (parse-scheme "(a . #f) [a . ()] (a b) #(a #\\b)"))

     (string=? (string #\page) (parse-scheme-datum "\"\\f\""))

     ;; Error case: a dotted list must have at least one element before the dot.
     (parser-error? (parser-error-from (lambda () (parse-scheme-datum "(. a)"))))

     ;; Error case: a boolean cannot run into a following identifier.
     (parser-error? (parser-error-from (lambda () (parse-scheme "#tfoo"))))

     ;; Error case: a character name cannot run into a following identifier.
     (parser-error? (parser-error-from (lambda () (parse-scheme "#\\spacefoo"))))

     ;; Error case: an integer cannot run into a following identifier.
     (parser-error? (parser-error-from (lambda () (parse-scheme "123abc"))))

     ;; Error case: a radix integer must end before another radix prefix.
     (parser-error? (parser-error-from (lambda () (parse-scheme "#x1#x2"))))

     (equal? '(#t (x) #\space (quote a) -12 + #f)
             (parse-scheme "#t(x)#\\space'a -12 + #f"))

     (equal? '(#t #f #\a #\b #\space #t)
             (parse-scheme "#t#f #\\a#\\b #\\ #t"))

     )

(mat parser-scheme-ordinary-string-cost

     (let* ([body (make-string 100000 #\a)] [source (string-append "\"" body "\"")]
            [before (statistics)] [value (parse-scheme-datum source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (string=? body value) (< allocated 30000000)))

     (for-all
      (lambda (newline)
        (string=? "a\nb" (parse-scheme-datum (string-append "\"a" newline "b\""))))
      '("\r" "\n" "\r\n" "\r\x85;" "\x85;" "\x2028;"))

     (string=? (string #\return #\newline #\x2029)
               (parse-scheme-datum "\"\\r\\n\x2029;\""))

     ;; Error cases: ordinary characters must not hide invalid escapes or a missing quote.
     (for-all (lambda (source)
                (and (parser-error-from (lambda () (parse-scheme-datum source))) #t))
              '("\"abc\\q\"" "\"abc")))

(mat parser-scheme-vector-prefix-cost

     (let* ([source (string-append "#(" (make-string 64000 #\space) "a)")]
            [before (statistics)] [value (parse-scheme-datum source)]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (equal? '#(a) value) (< allocated 6000000)))

     (equal? '(#() #(a) #(#(b) c))
             (parse-scheme "#( ) #(a) #(#(b) c)")))
