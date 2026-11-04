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

  #|proc:string-for-each
  Call `(proc char0 char1 ...)` with corresponding characters from input strings `str0`, ...
  Visit characters from left to right. Ignore callback results.
  All strings must have equal length. Return the result of `(void)`.
  |#
  (define-who string-for-each
    (case-lambda
      [(proc str0)
       (pcheck ([procedure? proc] [string? str0])
               (let ([len (string-length str0)])
                 (let loop ([index 0])
                   (unless (fx= index len)
                     (proc (string-ref str0 index))
                     (loop (fx1+ index)))))
               (void))]
      [(proc str0 str1)
       (pcheck ([procedure? proc] [string? str0 str1])
               (check-length who str0 str1)
               (let ([len (string-length str0)])
                 (let loop ([index 0])
                   (unless (fx= index len)
                     (proc (string-ref str0 index) (string-ref str1 index))
                     (loop (fx1+ index)))))
               (void))]
      [(proc str0 str1 str2)
       (pcheck ([procedure? proc] [string? str0 str1 str2])
               (check-length who str0 str1 str2)
               (let ([len (string-length str0)])
                 (let loop ([index 0])
                   (unless (fx= index len)
                     (proc (string-ref str0 index) (string-ref str1 index)
                           (string-ref str2 index))
                     (loop (fx1+ index)))))
               (void))]
      [(proc str0 . strings)
       (pcheck ([procedure? proc] [string? str0] [all-strings? strings])
               (apply check-length who str0 strings)
               (let ([all-strings (cons str0 strings)] [len (string-length str0)])
                 (let loop ([index 0])
                   (unless (fx= index len)
                     (apply proc (string-chars-at index all-strings))
                     (loop (fx1+ index)))))
               (void))]))

  #|proc:string-for-each/i
  Call `(proc index char0 char1 ...)` with the zero-based index and characters from `str0`, ...
  Visit characters from left to right. Ignore callback results.
  All strings must have equal length. Return the result of `(void)`.
  |#
  (define-who string-for-each/i
    (case-lambda
      [(proc str0)
       (pcheck ([procedure? proc] [string? str0])
               (let ([len (string-length str0)])
                 (let loop ([index 0])
                   (unless (fx= index len)
                     (proc index (string-ref str0 index))
                     (loop (fx1+ index)))))
               (void))]
      [(proc str0 str1)
       (pcheck ([procedure? proc] [string? str0 str1])
               (check-length who str0 str1)
               (let ([len (string-length str0)])
                 (let loop ([index 0])
                   (unless (fx= index len)
                     (proc index (string-ref str0 index) (string-ref str1 index))
                     (loop (fx1+ index)))))
               (void))]
      [(proc str0 str1 str2)
       (pcheck ([procedure? proc] [string? str0 str1 str2])
               (check-length who str0 str1 str2)
               (let ([len (string-length str0)])
                 (let loop ([index 0])
                   (unless (fx= index len)
                     (proc index (string-ref str0 index) (string-ref str1 index)
                           (string-ref str2 index))
                     (loop (fx1+ index)))))
               (void))]
      [(proc str0 . strings)
       (pcheck ([procedure? proc] [string? str0] [all-strings? strings])
               (apply check-length who str0 strings)
               (let ([all-strings (cons str0 strings)] [len (string-length str0)])
                 (let loop ([index 0])
                   (unless (fx= index len)
                     (apply proc index (string-chars-at index all-strings))
                     (loop (fx1+ index)))))
               (void))]))

  #|proc:string-map
  Call `(proc char0 char1 ...)` on corresponding characters from input strings `str0`, ...
  All strings must have equal length. Visit characters from left to right.
  Each callback result must be a character. Return a newly allocated string.
  |#
  (define-who string-map
    (case-lambda
      [(proc str0)
       (pcheck ([procedure? proc] [string? str0])
               (let* ([len (string-length str0)] [result (make-string len)])
                 (let loop ([index 0])
                   (if (fx= index len)
                       result
                       (let ([char (proc (string-ref str0 index))])
                         (pcheck ([char? char])
                                 (string-set! result index char)
                                 (loop (fx1+ index))))))))]
      [(proc str0 str1)
       (pcheck ([procedure? proc] [string? str0 str1])
               (check-length who str0 str1)
               (let* ([len (string-length str0)] [result (make-string len)])
                 (let loop ([index 0])
                   (if (fx= index len)
                       result
                       (let ([char (proc (string-ref str0 index) (string-ref str1 index))])
                         (pcheck ([char? char])
                                 (string-set! result index char)
                                 (loop (fx1+ index))))))))]
      [(proc str0 str1 str2)
       (pcheck ([procedure? proc] [string? str0 str1 str2])
               (check-length who str0 str1 str2)
               (let* ([len (string-length str0)] [result (make-string len)])
                 (let loop ([index 0])
                   (if (fx= index len)
                       result
                       (let ([char (proc (string-ref str0 index) (string-ref str1 index)
                                         (string-ref str2 index))])
                         (pcheck ([char? char])
                                 (string-set! result index char)
                                 (loop (fx1+ index))))))))]
      [(proc str0 . strings)
       (pcheck ([procedure? proc] [string? str0] [all-strings? strings])
               (apply check-length who str0 strings)
               (let* ([all-strings (cons str0 strings)]
                      [len (string-length str0)] [result (make-string len)])
                 (let loop ([index 0])
                   (if (fx= index len)
                       result
                       (let ([char (apply proc (string-chars-at index all-strings))])
                         (pcheck ([char? char])
                                 (string-set! result index char)
                                 (loop (fx1+ index))))))))]))

  #|proc:string-map/i
  Call `(proc index char0 char1 ...)` with the zero-based index and characters from `str0`, ...
  All strings must have equal length. Visit characters from left to right.
  Each callback result must be a character. Return a newly allocated string.
  |#
  (define-who string-map/i
    (case-lambda
      [(proc str0)
       (pcheck ([procedure? proc] [string? str0])
               (let* ([len (string-length str0)] [result (make-string len)])
                 (let loop ([index 0])
                   (if (fx= index len)
                       result
                       (let ([char (proc index (string-ref str0 index))])
                         (pcheck ([char? char])
                                 (string-set! result index char)
                                 (loop (fx1+ index))))))))]
      [(proc str0 str1)
       (pcheck ([procedure? proc] [string? str0 str1])
               (check-length who str0 str1)
               (let* ([len (string-length str0)] [result (make-string len)])
                 (let loop ([index 0])
                   (if (fx= index len)
                       result
                       (let ([char (proc index (string-ref str0 index) (string-ref str1 index))])
                         (pcheck ([char? char])
                                 (string-set! result index char)
                                 (loop (fx1+ index))))))))]
      [(proc str0 str1 str2)
       (pcheck ([procedure? proc] [string? str0 str1 str2])
               (check-length who str0 str1 str2)
               (let* ([len (string-length str0)] [result (make-string len)])
                 (let loop ([index 0])
                   (if (fx= index len)
                       result
                       (let ([char (proc index (string-ref str0 index) (string-ref str1 index)
                                         (string-ref str2 index))])
                         (pcheck ([char? char])
                                 (string-set! result index char)
                                 (loop (fx1+ index))))))))]
      [(proc str0 . strings)
       (pcheck ([procedure? proc] [string? str0] [all-strings? strings])
               (apply check-length who str0 strings)
               (let* ([all-strings (cons str0 strings)]
                      [len (string-length str0)] [result (make-string len)])
                 (let loop ([index 0])
                   (if (fx= index len)
                       result
                       (let ([char (apply proc index (string-chars-at index all-strings))])
                         (pcheck ([char? char])
                                 (string-set! result index char)
                                 (loop (fx1+ index))))))))]))

  #|proc:string-for-each-rev
  Call `(proc char0 char1 ...)` with corresponding characters from input strings `str0`, ...
  Visit characters from right to left. Ignore callback results.
  All strings must have equal length. Return the result of `(void)`.
  |#
  (define-who string-for-each-rev
    (case-lambda
      [(proc str0)
       (pcheck ([procedure? proc] [string? str0])
               (let loop ([index (fx1- (string-length str0))])
                 (unless (fx= index -1)
                   (proc (string-ref str0 index))
                   (loop (fx1- index))))
               (void))]
      [(proc str0 str1)
       (pcheck ([procedure? proc] [string? str0 str1])
               (check-length who str0 str1)
               (let loop ([index (fx1- (string-length str0))])
                 (unless (fx= index -1)
                   (proc (string-ref str0 index) (string-ref str1 index))
                   (loop (fx1- index))))
               (void))]
      [(proc str0 str1 str2)
       (pcheck ([procedure? proc] [string? str0 str1 str2])
               (check-length who str0 str1 str2)
               (let loop ([index (fx1- (string-length str0))])
                 (unless (fx= index -1)
                   (proc (string-ref str0 index) (string-ref str1 index)
                         (string-ref str2 index))
                   (loop (fx1- index))))
               (void))]
      [(proc str0 . strings)
       (pcheck ([procedure? proc] [string? str0] [all-strings? strings])
               (apply check-length who str0 strings)
               (let ([all-strings (cons str0 strings)])
                 (let loop ([index (fx1- (string-length str0))])
                   (unless (fx= index -1)
                     (apply proc (string-chars-at index all-strings))
                     (loop (fx1- index)))))
               (void))]))

  #|proc:string-for-each/i-rev
  Call `(proc index char0 char1 ...)` with the source index and characters from `str0`, ...
  Visit characters from right to left; `index` decreases from length minus one to zero.
  Ignore callback results. All strings must have equal length. Return the result of `(void)`.
  |#
  (define-who string-for-each/i-rev
    (case-lambda
      [(proc str0)
       (pcheck ([procedure? proc] [string? str0])
               (let loop ([index (fx1- (string-length str0))])
                 (unless (fx= index -1)
                   (proc index (string-ref str0 index))
                   (loop (fx1- index))))
               (void))]
      [(proc str0 str1)
       (pcheck ([procedure? proc] [string? str0 str1])
               (check-length who str0 str1)
               (let loop ([index (fx1- (string-length str0))])
                 (unless (fx= index -1)
                   (proc index (string-ref str0 index) (string-ref str1 index))
                   (loop (fx1- index))))
               (void))]
      [(proc str0 str1 str2)
       (pcheck ([procedure? proc] [string? str0 str1 str2])
               (check-length who str0 str1 str2)
               (let loop ([index (fx1- (string-length str0))])
                 (unless (fx= index -1)
                   (proc index (string-ref str0 index) (string-ref str1 index)
                         (string-ref str2 index))
                   (loop (fx1- index))))
               (void))]
      [(proc str0 . strings)
       (pcheck ([procedure? proc] [string? str0] [all-strings? strings])
               (apply check-length who str0 strings)
               (let ([all-strings (cons str0 strings)])
                 (let loop ([index (fx1- (string-length str0))])
                   (unless (fx= index -1)
                     (apply proc index (string-chars-at index all-strings))
                     (loop (fx1- index)))))
               (void))]))

  #|proc:string-map-rev
  Call `(proc char0 char1 ...)` on corresponding characters from input strings `str0`, ...
  All strings must have equal length. Visit characters from right to left.
  Each callback result must be a character. Return a new string in traversal order.
  For example, `(string-map-rev char-upcase "abc")` returns `"CBA"`.
  |#
  (define-who string-map-rev
    (case-lambda
      [(proc str0)
       (pcheck ([procedure? proc] [string? str0])
               (let* ([len (string-length str0)] [result (make-string len)])
                 (let loop ([index (fx1- len)] [result-index 0])
                   (if (fx= index -1)
                       result
                       (let ([char (proc (string-ref str0 index))])
                         (pcheck ([char? char])
                                 (string-set! result result-index char)
                                 (loop (fx1- index) (fx1+ result-index))))))))]
      [(proc str0 str1)
       (pcheck ([procedure? proc] [string? str0 str1])
               (check-length who str0 str1)
               (let* ([len (string-length str0)] [result (make-string len)])
                 (let loop ([index (fx1- len)] [result-index 0])
                   (if (fx= index -1)
                       result
                       (let ([char (proc (string-ref str0 index) (string-ref str1 index))])
                         (pcheck ([char? char])
                                 (string-set! result result-index char)
                                 (loop (fx1- index) (fx1+ result-index))))))))]
      [(proc str0 str1 str2)
       (pcheck ([procedure? proc] [string? str0 str1 str2])
               (check-length who str0 str1 str2)
               (let* ([len (string-length str0)] [result (make-string len)])
                 (let loop ([index (fx1- len)] [result-index 0])
                   (if (fx= index -1)
                       result
                       (let ([char (proc (string-ref str0 index) (string-ref str1 index)
                                         (string-ref str2 index))])
                         (pcheck ([char? char])
                                 (string-set! result result-index char)
                                 (loop (fx1- index) (fx1+ result-index))))))))]
      [(proc str0 . strings)
       (pcheck ([procedure? proc] [string? str0] [all-strings? strings])
               (apply check-length who str0 strings)
               (let* ([all-strings (cons str0 strings)]
                      [len (string-length str0)] [result (make-string len)])
                 (let loop ([index (fx1- len)] [result-index 0])
                   (if (fx= index -1)
                       result
                       (let ([char (apply proc (string-chars-at index all-strings))])
                         (pcheck ([char? char])
                                 (string-set! result result-index char)
                                 (loop (fx1- index) (fx1+ result-index))))))))]))

  #|proc:string-map/i-rev
  Call `(proc index char0 char1 ...)` with the source index and characters from `str0`, ...
  Visit characters from right to left; `index` decreases from length minus one to zero.
  All strings must have equal length. Each callback result must be a character.
  Return a new string in traversal order, matching `string-map-rev`.
  |#
  (define-who string-map/i-rev
    (case-lambda
      [(proc str0)
       (pcheck ([procedure? proc] [string? str0])
               (let* ([len (string-length str0)] [result (make-string len)])
                 (let loop ([index (fx1- len)] [result-index 0])
                   (if (fx= index -1)
                       result
                       (let ([char (proc index (string-ref str0 index))])
                         (pcheck ([char? char])
                                 (string-set! result result-index char)
                                 (loop (fx1- index) (fx1+ result-index))))))))]
      [(proc str0 str1)
       (pcheck ([procedure? proc] [string? str0 str1])
               (check-length who str0 str1)
               (let* ([len (string-length str0)] [result (make-string len)])
                 (let loop ([index (fx1- len)] [result-index 0])
                   (if (fx= index -1)
                       result
                       (let ([char (proc index (string-ref str0 index) (string-ref str1 index))])
                         (pcheck ([char? char])
                                 (string-set! result result-index char)
                                 (loop (fx1- index) (fx1+ result-index))))))))]
      [(proc str0 str1 str2)
       (pcheck ([procedure? proc] [string? str0 str1 str2])
               (check-length who str0 str1 str2)
               (let* ([len (string-length str0)] [result (make-string len)])
                 (let loop ([index (fx1- len)] [result-index 0])
                   (if (fx= index -1)
                       result
                       (let ([char (proc index (string-ref str0 index) (string-ref str1 index)
                                         (string-ref str2 index))])
                         (pcheck ([char? char])
                                 (string-set! result result-index char)
                                 (loop (fx1- index) (fx1+ result-index))))))))]
      [(proc str0 . strings)
       (pcheck ([procedure? proc] [string? str0] [all-strings? strings])
               (apply check-length who str0 strings)
               (let* ([all-strings (cons str0 strings)]
                      [len (string-length str0)] [result (make-string len)])
                 (let loop ([index (fx1- len)] [result-index 0])
                   (if (fx= index -1)
                       result
                       (let ([char (apply proc index (string-chars-at index all-strings))])
                         (pcheck ([char? char])
                                 (string-set! result result-index char)
                                 (loop (fx1- index) (fx1+ result-index))))))))]))

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
                 (if (fx= len 0)
                     ""
                     (cond
                      [(and (fx< normalized-start normalized-end) (fx> step 0))
                       (let ([result
                              (make-string
                               (ceiling (/ (fx- normalized-end normalized-start) step)))])
                         (let loop ([source-index normalized-start] [result-index 0])
                           (string-set! result result-index (string-ref str source-index))
                           ;; Do not add an arbitrary fixnum step after the final character.
                           (if (fx= result-index (fx1- (string-length result)))
                               result
                               (loop (fx+ source-index step) (fx1+ result-index)))))]
                      [(and (fx> normalized-start normalized-end) (fx< step 0))
                       (let ([result
                              (make-string
                               (ceiling (/ (fx- normalized-end normalized-start) step)))])
                         (let loop ([source-index normalized-start] [result-index 0])
                           (string-set! result result-index (string-ref str source-index))
                           (if (fx= result-index (fx1- (string-length result)))
                               result
                               (loop (fx+ source-index step) (fx1+ result-index)))))]
                      [else ""]))))]))

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
       (cond [(equal? str "") '("")]
             [(string? delim)
              (let ([dlen (string-length delim)] [len (string-length str)])
                (if (fx= dlen 1)
                    (string-split str (string-ref delim 0))
                    (let loop ([lefti 0] [res '()])
                      (let ([i ($string-search str delim lefti)])
                        (if i
                            (loop (fx+ i dlen) (cons (substring str lefti i) res))
                            (reverse (cons (substring str lefti len) res)))))))]
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


  ;; currently use brute force
  ;; TODO: Knuth-Morris-Pratt or Boyer-Moore?
  ;; Search for `target` from position `start` in `str`,
  ;; return the index if there's a match, or #f.
  ;; No error checking here.
  (define $string-search
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
                (cond [(string=? "" target) '(0)]
                      [(< end 0) #f]
                      [(= end 0) (and (string=? str target) '(0))]
                      [else (let loop ([i 0] [res '()])
                              (if (< end i)
                                  (and (not (null? res)) (reverse res))
                                  (let ([j ($string-search str target i)])
                                    (if j
                                        (loop (add1 j) (cons j res))
                                        (and (not (null? res)) (reverse res))))))])))))

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
  |#
  (define edit-distance
    (lambda (left right)
      (pcheck ([string? left right])
              (let-values ([(rows columns)
                            (if (fx>= (string-length left) (string-length right))
                                (values left right)
                                (values right left))])
                (let* ([row-count (string-length rows)]
                       [column-count (string-length columns)]
                       [distances (make-fxvector (fx1+ column-count))])
                  (let initialize ([column 0])
                    (unless (fx> column column-count)
                      (fxvector-set! distances column column)
                      (initialize (fx1+ column))))
                  (let row-loop ([row 1])
                    (if (fx> row row-count)
                        (fxvector-ref distances column-count)
                        (let ([row-char (string-ref rows (fx1- row))]
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
                                                         (string-ref columns
                                                                     (fx1- column)))
                                                 0
                                                 1))]
                                       [distance
                                        (min (fx1+ above)
                                             (fx1+ left-distance)
                                             replacement)])
                                  (fxvector-set! distances column distance)
                                  (column-loop (fx1+ column) above))))))))))))

  )
