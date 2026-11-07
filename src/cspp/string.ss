(library (cspp string)
  (export edit-distance
          string-for-each string-for-each/i string-map string-map/i
          string-for-each-rev string-for-each/i-rev string-map-rev string-map/i-rev
          string-slice string-startswith? string-endswith?
          string-search string-search-all string-contains? string-empty?
          string-split string-trim string-trim-left string-trim-right)
  (import (except (chezscheme) string-for-each)
          (cspp internal)
          (cspp utils)
          (cspp list))

  (define check-length
    (case-lambda
      [(who str0 str1)
       (unless (fx= (string-length str0) (string-length str1))
         (errorf who "strings are not of the same length"))]
      [(who str0 str1 str2)
       (unless (fx= (string-length str0) (string-length str1) (string-length str2))
         (errorf who "strings are not of the same length"))]
      [(who str0 . strings)
       (let ([len (string-length str0)])
         (unless (andmap (lambda (str) (fx= len (string-length str))) strings)
           (errorf who "strings are not of the same length")))]))

  (define all-strings?
    (lambda (values)
      (and (list? values) (andmap string? values))))

  (define string-chars-at
    (lambda (index strings)
      (map (lambda (str) (string-ref str index)) strings)))

  ;; Expand the traversal choices at compile time: small arities never build
  ;; character argument lists, and index/result positions remain loop bindings.
  (define-syntax $define-string-traversal
    (lambda (stx)
      (syntax-case stx ()
        [(_ name mapping? indexed? reverse?)
         (let ([mapping? (syntax->datum #'mapping?)]
               [indexed? (syntax->datum #'indexed?)]
               [reverse? (syntax->datum #'reverse?)])
           (define clause
             (lambda (sources rest?)
               (let* ([arguments (if indexed? (list #'index) '())]
                      [invoke (if rest?
                                  #`(apply proc #,@arguments
                                           (string-chars-at index all-strings))
                                  #`(proc #,@arguments
                                          #,@(map (lambda (source) #`(string-ref #,source index))
                                                  sources)))]
                      [done (if reverse? #'(fx= index -1) #'(fx= index len))]
                      [next (if reverse? #'(fx1- index) #'(fx1+ index))]
                      [result-index (if reverse? #'result-index #'index)]
                      [advance (if (and mapping? reverse?)
                                   #`(loop #,next (fx1+ result-index)) #`(loop #,next))]
                      [body (if mapping?
                                #`(if #,done result
                                      (let ([char #,invoke])
                                        (pcheck ([char? char])
                                                (string-set! result #,result-index char)
                                                #,advance)))
                                #`(unless #,done #,invoke #,advance))])
                 #`[#,(if rest? #'(proc str0 . strings) #`(proc #,@sources))
                    (pcheck ([procedure? proc] [string? #,@sources]
                             #,@(if rest? (list #'[all-strings? strings]) '()))
                            #,@(cond [rest? (list #'(apply check-length who str0 strings))]
                                     [(pair? (cdr sources))
                                      (list #`(check-length who #,@sources))]
                                     [else '()])
                            (let* (#,@(if rest? (list #'[all-strings (cons str0 strings)]) '())
                                   [len (string-length #,(car sources))]
                                   #,@(if mapping? (list #'[result (make-string len)]) '()))
                              (let loop ([index #,(if reverse? #'(fx1- len) #'0)]
                                         #,@(if (and mapping? reverse?)
                                                (list #'[result-index 0]) '()))
                                #,body))
                            #,@(if mapping? '() (list #'(void))))])))
           #`(define-who name
               (case-lambda
                 #,(clause (list #'str0) #f)
                 #,(clause (list #'str0 #'str1) #f)
                 #,(clause (list #'str0 #'str1 #'str2) #f)
                 #,(clause (list #'str0) #t))))])))

  #|proc:string-for-each
  Call `(proc char0 char1 ...)` with corresponding characters from input strings `str0`, ...
  Visit characters from left to right. Ignore callback results.
  All strings must have equal length. Return the result of `(void)`.
  |#
  ($define-string-traversal string-for-each #f #f #f)

  #|proc:string-for-each/i
  Call `(proc index char0 char1 ...)` with the zero-based index and characters from `str0`, ...
  Visit characters from left to right. Ignore callback results.
  All strings must have equal length. Return the result of `(void)`.
  |#
  ($define-string-traversal string-for-each/i #f #t #f)

  #|proc:string-map
  Call `(proc char0 char1 ...)` on corresponding characters from input strings `str0`, ...
  All strings must have equal length. Visit characters from left to right.
  Each callback result must be a character. Return a newly allocated string.
  |#
  ($define-string-traversal string-map #t #f #f)

  #|proc:string-map/i
  Call `(proc index char0 char1 ...)` with the zero-based index and characters from `str0`, ...
  All strings must have equal length. Visit characters from left to right.
  Each callback result must be a character. Return a newly allocated string.
  |#
  ($define-string-traversal string-map/i #t #t #f)

  #|proc:string-for-each-rev
  Call `(proc char0 char1 ...)` with corresponding characters from input strings `str0`, ...
  Visit characters from right to left. Ignore callback results.
  All strings must have equal length. Return the result of `(void)`.
  |#
  ($define-string-traversal string-for-each-rev #f #f #t)

  #|proc:string-for-each/i-rev
  Call `(proc index char0 char1 ...)` with the source index and characters from `str0`, ...
  Visit characters from right to left; `index` decreases from length minus one to zero.
  Ignore callback results. All strings must have equal length. Return the result of `(void)`.
  |#
  ($define-string-traversal string-for-each/i-rev #f #t #t)

  #|proc:string-map-rev
  Call `(proc char0 char1 ...)` on corresponding characters from input strings `str0`, ...
  All strings must have equal length. Visit characters from right to left.
  Each callback result must be a character. Return a new string in traversal order.
  For example, `(string-map-rev char-upcase "abc")` returns `"CBA"`.
  |#
  ($define-string-traversal string-map-rev #t #f #t)

  #|proc:string-map/i-rev
  Call `(proc index char0 char1 ...)` with the source index and characters from `str0`, ...
  Visit characters from right to left; `index` decreases from length minus one to zero.
  All strings must have equal length. Each callback result must be a character.
  Return a new string in traversal order, matching `string-map-rev`.
  |#
  ($define-string-traversal string-map/i-rev #t #t #t)

  #|proc:string-slice
  Return a string slice selected by `start`, exclusive `end`, and nonzero `step`.
  Negative indexes count from the end. Out-of-range combinations return an empty string.
  |#
  (define-who string-slice
    (case-lambda
      [(str end) (string-slice str 0 end 1)]
      [(str start end) (string-slice str start end 1)]
      [(str start end step)
       (pcheck ([string? str] [fixnum? start end step])
               (when (fx= step 0) (errorf who "step cannot be 0"))
               (let* ([len (string-length str)]
                      [raw-start (if (fx>= start 0) start (fx+ len start))]
                      [normalized-start
                       (if (fx> step 0)
                           (cond [(fx< raw-start 0) 0]
                                 [(fx> raw-start len) len]
                                 [else raw-start])
                           (cond [(fx< raw-start 0) -1]
                                 [(fx>= raw-start len) (fx1- len)]
                                 [else raw-start]))]
                      [normalized-end
                       (let ([index (if (fx>= end 0) end (fx+ len end))])
                         (cond [(fx<= index -1) -1]
                               [(fx>= index len) len]
                               [else index]))])
                 (if (and (fx> len 0)
                          (if (fx> step 0)
                              (fx< normalized-start normalized-end)
                              (fx> normalized-start normalized-end)))
                       (let ([result
                              (make-string
                               (ceiling (/ (fx- normalized-end normalized-start) step)))])
                         (let loop ([source-index normalized-start] [result-index 0])
                           (string-set! result result-index (string-ref str source-index))
                           ;; Do not add an arbitrary fixnum step after the final character.
                           (if (fx= result-index (fx1- (string-length result)))
                               result
                               (loop (fx+ source-index step) (fx1+ result-index)))))
                     "")))]))

  #|proc:string-split
  Split a string into a list of substrings, using `delim` as delimiter.
  `delim` can be either a character or a non-empty string.
  Return substrings in their original order, including empty edge fields.
  |#
  (define-who string-split
    (lambda (str delim)
      (pcheck ([string? str])
       (unless (or (char? delim) (string? delim))
         (errorf who "invalid delimiter: ~a" delim))
       (when (and (string? delim) (fx= 0 (string-length delim)))
         (errorf who "empty delimiter"))
       (cond [(equal? str "") (list "")]
             [(string? delim)
              (let ([dlen (string-length delim)] [len (string-length str)])
                (if (fx= dlen 1)
                    (string-split str (string-ref delim 0))
                    (if (fx> dlen len) (list (string-copy str))
                        (let ([next ($make-string-search str delim 0 #f)]
                              [fields (make-list-builder)])
                          (let loop ([lefti 0])
                            (let ([i (next)])
                              (if i
                                  (begin (fields (substring str lefti i))
                                         (loop (fx+ i dlen)))
                                  (begin (fields (substring str lefti len))
                                         (fields)))))))))]
             [(char? delim)
              (let ([lb (make-list-builder)] [len (string-length str)])
                (let loop-next ([leftcur 0])
                  (let loop ([i leftcur])
                    (if (fx= i len)
                        (begin (lb (substring str leftcur i))
                               (lb))
                        (if (char=? (string-ref str i) delim)
                            (begin (lb (substring str leftcur i))
                                   (loop-next (add1 i)))
                            (loop (add1 i)))))))]
             [else (errorf who "invalid delimiter: ~a" delim)]))))


  (define $string-trim
    (lambda (who str c left? right?)
      (pcheck ([string? str] [char? c])
              (let* ([len (string-length str)]
                     [lefti (if left?
                                (let lp ([i 0])
                                  ;; in case `str` consists entirely of `c`
                                  (if (fx< i len)
                                      (if (char=? c (string-ref str i))
                                          (lp (add1 i))
                                          i)
                                      len))
                                0)]
                     [righti (if right?
                                 (let lp ([i (sub1 len)])
                                   ;; ditto
                                   (if (fx>= i 0)
                                       (if (char=? c (string-ref str i))
                                           (lp (sub1 i))
                                           (add1 i))
                                       0))
                                 len)])
                (if (fx<= lefti righti)
                    (substring str lefti righti)
                    "")))))


  #|proc:string-trim
  Return `str` without leading or trailing `c` characters.
  When omitted, `c` defaults to `#\space`.
  |#
  (define-who string-trim
    (case-lambda
      [(str) (string-trim str #\space)]
      [(str c) ($string-trim who str c #t #t)]))


  #|proc:string-trim-left
  Return `str` without leading `c` characters. When omitted, `c` is `#\space`.
  |#
  (define-who string-trim-left
    (case-lambda
      [(str) (string-trim-left str #\space)]
      [(str c) ($string-trim who str c #t #f)]))


  #|proc:string-trim-right
  Return `str` without trailing `c` characters. When omitted, `c` is `#\space`.
  |#
  (define-who string-trim-right
    (case-lambda
      [(str) (string-trim-right str #\space)]
      [(str c) ($string-trim who str c #f #t)]))


  ;; Direct scanning avoids preprocessing and allocation for short needles.
  (define $string-search/direct
    (lambda (str target start)
      (let ([slen (string-length str)] [tlen (string-length target)])
        (define str=?
          (lambda (i)
            (let loop ([i i] [j 0])
              (or (fx= j tlen)
                  (and (char=? (string-ref str i) (string-ref target j))
                       (loop (add1 i) (add1 j)))))))
        (cond [(fx> (fx+ start tlen) slen) #f]
              [(fx= (fx+ start tlen) slen) (and (str=? start) start)]
              [else (let ([end (fx- slen tlen)])
                      (let loop ([i start])
                        (if (fx> i end)
                            #f
                            (if (str=? i)
                                i
                                (loop (add1 i))))))]))))

  (define $string-prefix-table
    (lambda (target)
      (let* ([length (string-length target)] [prefix (make-vector length 0)])
        (do ([index 1 (fx1+ index)]) ((fx= index length) prefix)
          (let loop ([matched (vector-ref prefix (fx1- index))])
            (cond [(char=? (string-ref target index) (string-ref target matched))
                   (vector-set! prefix index (fx1+ matched))]
                  [(fx> matched 0) (loop (vector-ref prefix (fx1- matched)))]))))))

  ;; Return a private next-match procedure for a nonempty target. Long needles use
  ;; KMP; retain the scan position and prefix table across all matches or split fields.
  (define $make-string-search
    (lambda (str target start overlap?)
      (let ([slen (string-length str)] [tlen (string-length target)])
        (if (fx<= tlen 16)
            (let ([cursor start])
              (lambda ()
                (and cursor
                     (let ([match ($string-search/direct str target cursor)])
                       (set! cursor (and match (fx+ match (if overlap? 1 tlen))))
                       match))))
            (let ([prefix #f] [cursor start] [previous 0])
              (define fallback
                (lambda (matched)
                  ;; Exact, nonoverlapping matches need no prefix preprocessing.
                  (unless prefix (set! prefix ($string-prefix-table target)))
                  (vector-ref prefix (fx1- matched))))
              (lambda ()
                (and (fx< cursor slen)
                     (let loop ([index cursor]
                                [matched (if (fx= previous tlen)
                                             (if overlap? (fallback previous) 0) previous)])
                       (cond [(fx= index slen) (set! cursor slen) #f]
                             [(char=? (string-ref str index) (string-ref target matched))
                              (if (fx= (fx1+ matched) tlen)
                                  (begin
                                    (set! cursor (fx1+ index))
                                    (set! previous tlen)
                                    (fx- cursor tlen))
                                  (loop (fx1+ index) (fx1+ matched)))]
                             [(fx> matched 0) (loop index (fallback matched))]
                             [else (loop (fx1+ index) 0)])))))))))

  (define $string-search
    (lambda (str target start)
      (let ([tlen (string-length target)])
        (if (or (fx<= tlen 16) (fx>= tlen (fx- (string-length str) start)))
            ($string-search/direct str target start)
            (($make-string-search str target start #t))))))


  #|proc:string-search
  Return the first index of `target` in `str`, or `#f` when no match exists.
  `target` is a character or string. An empty target matches at index zero.
  |#
  (define string-search
    (lambda (str target)
      (pcheck ([string? str])
              (let ([target (pcase target
                                   [string? target]
                                   [char? (string target)])])
                ($string-search str target 0)))))

  #|proc:string-search-all
  Return all indexes of `target` in `str`, including overlapping matches.
  Return `#f` when no match exists. `target` is a character or string.
  |#
  (define string-search-all
    (lambda (str target)
      (pcheck ([string? str])
              (let* ([target (pcase target
                                    [string? target]
                                    [char? (string target)])]
                     [end (- (string-length str)
                             (string-length target))])
                (cond [(string=? "" target) (list 0)]
                      [(< end 0) #f]
                      [(= end 0) (and (string=? str target) (list 0))]
                      [else (let ([next ($make-string-search str target 0 #t)]
                                  [matches (make-list-builder)])
                              (let loop ([index (next)])
                                (if index
                                    (begin (matches index) (loop (next)))
                                    (let ([result (matches)])
                                      (and (pair? result) result)))))])))))

  #|proc:string-empty?
  Return whether `str` has length zero.
  |#
  (define string-empty?
    (lambda (str)
      (pcheck ([string? str])
              (fx= 0 (string-length str)))))

  #|proc:string-contains?
  Return whether `str` contains every character or string in `patterns`.
  With no patterns, return `#t`.
  |#
  (define string-contains?
    (lambda (str . patterns)
      (pcheck ([string? str])
              (if (null? patterns)
                  #t
                  (let* ([ss (map (lambda (pattern)
                                    (pcase pattern
                                           [string? pattern]
                                           [char? (string pattern)]))
                                  patterns)]
                         [slen (string-length str)]
                         [contains1? (lambda (s)
                                       (let ([patlen (string-length s)])
                                         (cond
                                          [(> patlen slen) #f]
                                          [(= patlen slen) (string=? s str)]
                                          [else (and ($string-search str s 0) #t)])))])
                    (andmap contains1? ss))))))

  #|proc:string-startswith?
  Return whether `str` begins with character or string `prefix`.
  |#
  (define string-startswith?
    (lambda (str prefix)
      (pcheck ([string? str])
              (let ([prefix (pcase prefix
                                   [string? prefix]
                                   [char? (string prefix)])])
                (if (equal? "" prefix)
                    #t
                    (let ([slen (string-length str)]
                          [preflen (string-length prefix)])
                      (cond
                       [(= preflen slen) (equal? str prefix)]
                       [(< preflen slen) (let loop ([i 0])
                                           (if (fx= i preflen)
                                               #t
                                               (and (char=? (string-ref str i) (string-ref prefix i))
                                                    (loop (add1 i)))))]
                       [else #f])))))))

  #|proc:string-endswith?
  Return whether `str` ends with character or string `suffix`.
  |#
  (define string-endswith?
    (lambda (str suffix)
      (pcheck ([string? str])
              (let ([suffix (pcase suffix
                                   [string? suffix]
                                   [char? (string suffix)])])
                (if (equal? "" suffix)
                    #t
                    (let ([slen (string-length str)]
                          [suflen (string-length suffix)])
                      (cond
                       [(= suflen slen) (equal? str suffix)]
                       [(< suflen slen) (let loop ([i (fx- slen suflen)] [j 0])
                                          (if (fx= i slen)
                                              #t
                                              (and (char=? (string-ref str i) (string-ref suffix j))
                                                   (loop (add1 i) (add1 j)))))]
                       [else #f])))))))


  #|proc:edit-distance
  Return the Levenshtein distance between strings `left` and `right`.
  The distance counts single-character insertion, deletion, and replacement operations.
  If either operand is empty, return the other string's length without scanning its characters.
  Common prefixes and suffixes are excluded from the dynamic-programming work.
  |#
  (define edit-distance
    (lambda (left right)
      (pcheck ([string? left right])
        (if (eq? left right) 0
        (let* ([left-length (string-length left)] [right-length (string-length right)]
               [limit (fxmin left-length right-length)]
               [start (let loop ([index 0])
                        (if (and (fx< index limit)
                                 (char=? (string-ref left index) (string-ref right index)))
                            (loop (fx1+ index)) index))])
          (let-values ([(left-end right-end)
                        (let loop ([left-end left-length] [right-end right-length])
                          (if (and (fx> left-end start) (fx> right-end start)
                                   (char=? (string-ref left (fx1- left-end))
                                           (string-ref right (fx1- right-end))))
                              (loop (fx1- left-end) (fx1- right-end))
                              (values left-end right-end)))])
          (let-values ([(rows columns row-count column-count)
                        (if (fx>= left-end right-end)
                            (values left right (fx- left-end start) (fx- right-end start))
                            (values right left (fx- right-end start) (fx- left-end start)))])
            (if (fxzero? column-count)
                row-count
                (let ([distances (make-fxvector (fx1+ column-count))])
                  (let initialize ([column 0])
                    (unless (fx> column column-count)
                      (fxvector-set! distances column column)
                      (initialize (fx1+ column))))
                  (let row-loop ([row 1])
                    (if (fx> row row-count)
                        (fxvector-ref distances column-count)
                        (let ([row-char (string-ref rows (fx+ start (fx1- row)))]
                              [previous-diagonal (fx1- row)])
                          (fxvector-set! distances 0 row)
                          (let column-loop ([column 1]
                                            [previous-diagonal previous-diagonal])
                            (if (fx> column column-count)
                                (row-loop (fx1+ row))
                                (let* ([above (fxvector-ref distances column)]
                                       [left-distance
                                        (fxvector-ref distances (fx1- column))]
                                       [replacement
                                        (fx+ previous-diagonal
                                             (if (char=? row-char
                                                         (string-ref columns (fx+ start (fx1- column))))
                                                 0
                                                 1))]
                                       [distance (min (fx1+ above)
                                                      (fx1+ left-distance)
                                                      replacement)])
                                  (fxvector-set! distances column distance)
                                  (column-loop (fx1+ column) above))))))))))))))))

  )
