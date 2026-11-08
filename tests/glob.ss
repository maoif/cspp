(import (cspp) (cspp glob))

(mat glob-windows-literal-roots
     (for-all
      (lambda (root)
        (let ([pattern (string-append root "/file{1..2}.[t][x][t]")])
          (and (glob-match? 'windows pattern (string-append root "/file1.txt"))
               (glob-match? 'windows pattern (string-append root "/file2.txt"))
               (not (glob-match? 'windows pattern (string-append root "/file3.txt")))
               (not (glob-match? 'windows pattern (string-append root "x/file1.txt")))
               (glob-match? 'windows root root)
               (glob-match? 'windows (string-append root "/") (string-append root "/"))
               (glob-match? 'windows
                            (string-map (lambda (char) (if (char=? char #\/) #\\ char)) pattern)
                            (string-append root "/file1.txt")))))
      '("//?/Volume{01234567-89ab-cdef-0123-456789abcdef}"
        "//server/share{one}" "//server/share{one,two}" "//server/share{1..3}"
        "//server/share[" "//server/share]" "//server/share[1]"
        "//ser[ver]/share{" "//server/share}" "//server/share,one"
        "//./Device{one,two}" "//?/UNC/ser{ver}/share[")))

(mat glob-windows-root-unions
     (glob-match? 'windows "{//server/shareA,//server/shareB}/*.txt"
                  "//server/shareA/file.txt")

     (glob-match? 'windows "{//server/shareA,//server/shareB}/*.txt"
                  "//server/shareB/file.txt")

     (not (glob-match? 'windows "{//server/shareA,//server/shareB}/*.txt"
                       "//server/shareC/file.txt"))

     (glob-match? 'windows
                  "{{//server/share{one},//server/share[},//?/Volume{abc}}/file{1..2}.txt"
                  "//server/share[/file2.txt")

     (glob-match? 'windows
                  "{{//server/share{one},//server/share[},//?/Volume{abc}}/file{1..2}.txt"
                  "//?/Volume{abc}/file1.txt")

     (glob-match? 'windows "//server/{shareA,shareB}/*.txt"
                  "//server/{shareA,shareB}/file.txt")

     (not (glob-match? 'windows "//server/{shareA,shareB}/*.txt"
                       "//server/shareA/file.txt"))

     (glob-match? 'windows "{C:/a,//?/Volume{abc}/a}/*.txt" "C:/a/file.txt")

     (glob-match? 'windows "{C:/a,//?/Volume{abc}/a}/*.txt"
                  "//?/Volume{abc}/a/file.txt"))

(mat glob-windows-literal-root-expansion
     (let ([root (format "//tmp/cspp-glob-root-~a-{one,two}[" (get-process-id))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (write-string (path-build root "file1.txt") "one")
           (write-string (path-build root "file2.txt") "two"))
         (lambda ()
           (let* ([pattern (string-append root "/file{1..2}.txt")]
                  [expected (list (path-build root "file1.txt") (path-build root "file2.txt"))])
             (and (equal? expected (glob 'windows pattern))
                  (equal? expected (sort string<? (iter->list (glob->iter 'windows pattern))))
                  (equal? '("one" "two") (map read-string (glob 'windows pattern))))))
         (lambda () (file-removetree root #t)))))

(mat glob-windows-root-result-markers
     (let ([root (format "//tmp/cspp-glob-marker-~a-{one}[" (get-process-id))])
       (dynamic-wind
         (lambda () (mkdir root))
         (lambda ()
           (for-all
            (lambda (separator)
              (let ([spelled (string-map (lambda (char) (if (char=? char #\/) separator char))
                                        root)]
                    [slash (string separator)])
                (for-all
                 (lambda (suffix result-suffix)
                   (let* ([pattern (string-append spelled suffix)]
                          [results (glob* 'windows pattern #f #t 'empty)])
                     (and (equal? results (list (string-append spelled result-suffix)))
                          (glob-match? 'windows pattern (car results)))))
                 (list "" slash (string-append slash "**") (string-append slash "**" slash))
                 (list "" slash "" slash))))
            '(#\/ #\\)))
         (lambda () (file-removetree root #t)))))

(mat glob-unmatched-result-ownership
     (let* ([pattern (string-copy "__cspp_absent_owned_glob__")]
            [result (glob* pattern #f #f 'literal)])
       (string-set! (car result) 0 #\X)
       (string=? pattern "__cspp_absent_owned_glob__")))

(mat glob-zero-padded-ranges
     (for-all
      (lambda (fixture)
        (for-all (lambda (candidate) (glob-match? 'unix (car fixture) candidate)) (cdr fixture)))
      '(("file{01..3}" "file01" "file02" "file03")
        ("{03..01..-2}" "03" "01")
        ("{-02..02}" "-02" "-01" "000" "001" "002")
        ("{-2..02}" "-2" "-1" "00" "01" "02")
        ("{+01..03}" "001" "002" "003")
        ("{-00..02}" "000" "001" "002")
        ("{009..011..2}" "009" "011")))

     (not (glob-match? 'unix "file{01..03}" "file1"))

     (not (glob-match? 'unix "{009..011..2}" "010")))

(mat glob-padded-and-named-class-expansion
     (let ([root (format "glob-padded-classes-~a" (get-process-id))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (for-each (lambda (name) (file-touch (path-build root name)))
                     '("file01" "file02" "file1" "a.txt" "Z.txt" "7.txt")))
         (lambda ()
           (and (equal? (glob (string-append root "/file{01..02}"))
                        (map (lambda (name) (path-build root name)) '("file01" "file02")))
                (equal? (glob (string-append root "/[[:alpha:]].txt"))
                        (map (lambda (name) (path-build root name)) '("Z.txt" "a.txt")))))
         (lambda () (file-removetree root #t)))))

(mat glob-posix-named-classes
     (for-all
      (lambda (fixture)
        (let ([pattern (car fixture)] [yes (cadr fixture)] [no (caddr fixture)])
          (and (glob-match? 'unix pattern yes)
               (not (glob-match? 'unix pattern no)))))
      '(("[[:alnum:]]" "7" "_") ("[[:alpha:]]" "a" "λ")
        ("[[:blank:]]" "\t" "\n") ("[[:cntrl:]]" "\x7f;" " ")
        ("[[:digit:]]" "5" "٥") ("[[:graph:]]" "!" " ")
        ("[[:lower:]]" "z" "Z") ("[[:print:]]" " " "\n")
        ("[[:punct:]]" "$" "A") ("[[:space:]]" "\n" "x")
        ("[[:upper:]]" "A" "a") ("[[:xdigit:]]" "F" "g")
        ("[![:digit:]]" "a" "5") ("[a[:digit:]z]" "5" "b")
        ("[[:alpha:][:digit:]]" "9" "_") ("[[:alpha:]0-9]" "9" "_")))

     (glob-match? 'windows "[[:upper:]]" "a")

     (glob-match? 'windows "[[:alpha:]]" "K")

     (glob-match? 'unix "{[[:alpha:]],x}" "a")

     (glob-match? 'unix "[[:punct:]]" "{")

     ;; Error cases: unknown and unterminated named classes are malformed patterns.
     (error? (make-glob "[[:unknown:]]"))

     (error? (make-glob "[[:alpha]]"))

     ;; Error case: a multi-character class cannot be a range endpoint.
     (error? (make-glob "[a-[:digit:]]")))

(mat glob-missing-prefix-allocation
     ;; A missing first component must not require quadratic root-extraction allocation.
     (let ([allocated
            (lambda (depth)
              (let* ([pattern (string-append "glob-absent-prefix-" (number->string (get-process-id))
                                             (apply string-append (make-list depth "/x")))]
                     ;; Exclude collection work triggered by unrelated earlier test cases.
                     [collected (collect)]
                     [before (statistics)]
                     [results (glob 'unix pattern)])
                (and (null? results)
                     (sstats-bytes (sstats-difference (statistics) before)))))])
       (let ([short (allocated 32)] [long (allocated 256)])
         (and short long (< long (* 12 short))))))

(mat glob-shares-path-root-equality
     (for-all
      (lambda (pattern candidate)
        (and (path-same-root? (path-parse 'windows pattern) (path-parse 'windows candidate))
             (glob-match? 'windows pattern candidate)))
      '("C:x" "C:/x" "//server/share/x" "//?/C:/x" "//?/UNC/server/share/x" "//./COM1/x")
      '("c:X" "c:/X" "//SERVER/SHARE/X" "//?/c:/X" "//?/unc/SERVER/SHARE/X" "//./com1/X")))

(mat glob-windows-skips-unrepresentable-entries
     (let ([root (format "glob-separator-entry-~a" (get-process-id))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (file-touch (string-append root "/normal.txt"))
           (file-touch (string-append root "/odd\\name.txt"))
           (mkdir (string-append root "/odd\\dir"))
           (file-touch (string-append root "/odd\\dir/child.txt")))
         (lambda ()
           (let ([normal (string-append root "/normal.txt")])
             (and (equal? (glob 'windows (string-append root "/*.txt")) (list normal))
                  (equal? (glob 'windows (string-append root "/**/*.txt")) (list normal))
                  (equal? (iter->list (glob->iter 'windows (string-append root "/*.txt")))
                          (list normal))
                  (= 3 (length (glob 'unix (string-append root "/**/*.txt")))))))
         (lambda () (file-removetree root #t)))))

(mat glob-windows-simple-unicode-case
     (for-all
      (lambda (left right equal?)
        (and (eq? equal? (glob-match? 'windows left right))
             (eq? equal? (glob-match? 'windows right left))
             (eq? equal? (glob-match? 'windows (string-append "[" left "]") right))
             (eq? (not equal?)
                  (glob-match? 'windows (string-append "[!" left "]") right))
             (eq? equal? (path-same-root?
                          (path-unc left "share") (path-unc right "SHARE")))
             (eq? equal? (glob-match? 'windows
                          (string-append "//" left "/share/*.txt")
                          (string-append "//" right "/SHARE/x.txt")))))
      '("ſ" "ς" "ß" "İ" "ı" "σ")
      '("s" "σ" "ẞ" "i" "I" "Σ")
      '(#t #t #t #f #f #t))

     (glob-match? 'windows "[A-Z]" "ſ")

     (glob-match? 'windows "[ſ-ſ]" "s")

     (not (glob-match? 'windows "straße" "STRASSE"))

     (not (glob-match? 'unix "ſ" "s")))

(mat glob-windows-broad-unicode-range
     (let ([pattern (string #\[ (integer->char 1) #\- (integer->char #x10ffff) #\])])
       (and (glob-match? 'windows pattern "ÿ")
            (not (glob-match? 'windows
                   (string-append "[!" (substring pattern 1 (string-length pattern))) "ÿ")))))

(mat glob-windows-simple-unicode-filesystem
     (let ([root (format "glob-simple-fold-~a" (get-process-id))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (file-touch (path-build root "ſ.txt"))
           (file-touch (path-build root "ẞ.txt"))
           (file-touch (path-build root "İ.txt")))
         (lambda ()
           (and (equal? (list (string-append root "/ſ.txt"))
                        (glob 'windows (string-append root "/s.txt")))
                (equal? (list (string-append root "/ſ.txt"))
                        (glob 'windows (string-append root "/[S].txt")))
                (equal? (list (string-append root "/ẞ.txt"))
                        (glob 'windows (string-append root "/ß.txt")))
                (null? (glob 'windows (string-append root "/i.txt")))))
         (lambda () (file-removetree root)))))

(mat glob-finite-parent-traversal
     (let ([root (format "glob-finite-parent-~a" (get-process-id))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir (string-append root "/sub"))
           (file-touch (string-append root "/one.txt")))
         (lambda ()
           (for-all
            (lambda (follow?)
              (equal? (list (string-append root "/sub/../one.txt"))
                      (glob* (string-append root "/*/../*.txt") follow? #f 'empty)))
            '(#f #t)))
         (lambda () (file-removetree root)))))

(mat glob-adjacent-globstars
     (let* ([root (format "glob-adjacent-stars-~a" (get-process-id))]
            [leaf root]
            [allocated
             (lambda (pattern)
               (collect)
               (let* ([before (statistics)]
                      [result (glob pattern)]
                      [bytes (sstats-bytes (sstats-difference (statistics) before))])
                 (cons result bytes)))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (do ([i 0 (fx1+ i)]) ((fx= i 9))
             (set! leaf (string-append leaf "/d"))
             (mkdir leaf))
           (file-touch (string-append leaf "/x")))
         (lambda ()
           (let ([single (allocated (string-append root "/**/x"))]
                 [repeated (allocated (string-append root "/**/**/**/**/**/x"))])
             (and (equal? (car single) (list (string-append leaf "/x")))
                  (equal? (car single) (car repeated))
                  ;; Equivalent recursive patterns must not multiply tree traversal.
                  (< (cdr repeated) (* 5 (cdr single)))
                  (glob-match? 'unix "**/**/**/x" "x")
                  (glob-match? 'unix "**/**/**/x" "a/b/x"))))
         (lambda () (file-removetree root)))))

(mat glob-separated-globstars
     (let* ([root (format "glob-separated-stars-~a" (get-process-id))]
            [leaf root]
            [allocated
             (lambda (pattern follow?)
               (collect)
               (let* ([before (statistics)] [result (glob* pattern follow? #f 'empty)]
                      [bytes (sstats-bytes (sstats-difference (statistics) before))])
                 (cons result bytes)))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (do ([i 0 (fx1+ i)]) ((fx= i 16))
             (set! leaf (string-append leaf "/a"))
             (mkdir leaf))
           (file-touch (string-append leaf "/leaf.txt")))
         (lambda ()
           (let* ([single (allocated (string-append root "/**/leaf.txt") #f)]
                  [pattern (string-append root "/**/a/**/a/**/a/**/leaf.txt")]
                  [separated (allocated pattern #f)]
                  [single-follow (allocated (string-append root "/**/leaf.txt") #t)]
                  [separated-follow (allocated pattern #t)]
                  [iterator (glob->iter pattern)]
                  [collect-pass
                   (lambda (iterator)
                     (let ([values (make-list-builder)])
                       (let loop ([value (iter-next! iterator)])
                         (if (eq? value iter-end) (values)
                             (begin (values value) (loop (iter-next! iterator)))))))])
             (dynamic-wind
               void
               (lambda ()
                 (and (equal? (list (string-append leaf "/leaf.txt")) (car separated))
                      ;; Repeated pattern states must not multiply directory traversal.
                      (< (cdr separated) (* 20 (cdr single)))
                      (equal? (car single) (car separated-follow))
                      (< (cdr separated-follow) (* 20 (cdr single-follow)))
                      (equal? (car single) (collect-pass iterator))
                      (begin (iter-reset! iterator)
                             (equal? (car single) (collect-pass iterator)))))
               (lambda () (iter-finalize! iterator)))))
         (lambda () (file-removetree root)))))

(mat glob-dot-components-respect-symlink-policy
     (let ([root (format "glob-dot-policy-~a" (get-process-id))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir (string-append root "/dir"))
           (file-touch (string-append root "/dir/x"))
           (file-symlink "dir" (string-append root "/link")))
         (lambda ()
           (and
            (for-all
             (lambda (suffix)
               (not (exists (lambda (result) (string-search result "/link/"))
                            (glob (string-append root suffix)))))
             '("/*/./x" "/*/../dir/x" "/**/./x" "/*/." "/*/.."))
            ;; Explicit following still permits dot traversal through a directory link.
            (and (member (string-append root "/link/./x")
                         (glob* (string-append root "/*/./x") #t #f 'empty)) #t)))
         (lambda () (file-removetree root)))))

(mat glob-windows-consistent-case-rules
     (let ([root (format "glob-case-policy-~a" (get-process-id))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (file-touch (string-append root "/STRASSE.TXT"))
           (file-touch (string-append root "/Σ.txt")))
         (lambda ()
           (and (null? (glob 'windows (string-append root "/straße.txt")))
                (not (glob-match? 'windows "straße.txt" "STRASSE.TXT"))
                (equal? (list (string-append root "/STRASSE.TXT"))
                        (glob 'windows (string-append root "/strasse.txt")))
                (glob-match? 'windows "σ.txt" "Σ.txt")
                (equal? (list (string-append root "/Σ.txt"))
                        (glob 'windows (string-append root "/σ.txt")))))
         (lambda () (file-removetree root)))))

(mat glob-iterator-owns-deduplication-keys
     (let ([root (format "glob-mutable-result-~a" (get-process-id))])
       (dynamic-wind
         (lambda () (mkdir root) (file-touch (string-append root "/a")))
         (lambda ()
           (let ([iterator (glob->iter (string-append root "/{a,a}"))])
             (dynamic-wind
               void
               (lambda ()
                 (let ([result (iter-next! iterator)])
                   (string-set! result (fx1- (string-length result)) #\b)
                   (eq? iter-end (iter-next! iterator))))
               (lambda () (iter-finalize! iterator)))))
         (lambda () (file-removetree root)))))

(mat glob-unix-backslash-parity
     ;; Pairs of backslashes encode literal backslashes before the path separator.
     (for-all
      (lambda (count)
        (let ([slashes (make-string count #\\)])
          (glob-match? 'unix (string-append "a" (make-string (fx* 2 count) #\\) "/b")
                       (string-append "a" slashes "/b"))))
      '(1 2 3 4))

     ;; An unpaired backslash may escape the separator after literal backslash pairs.
     (glob-match? 'unix "a\\\\\\/b" "a\\/b"))

(mat glob-negated-class-leading-bracket
     (glob-match? 'unix "[!]{]" "a")

     (not (glob-match? 'unix "[!]{]" "{"))

     (not (glob-match? 'unix "[^]{]" "]"))

     ;; Class scanning must agree inside braces and while separating alternatives.
     (glob-match? 'unix "{[!],{],b}" "a")

     (glob-match? 'unix "{b,[^],{]}" "a"))

(mat glob-home-is-literal
     ;; Expanded home components are literal, even when they contain glob syntax.
     (let ([old-home (getenv "HOME")] [old-profile (getenv "USERPROFILE")])
       (dynamic-wind
         (lambda ()
           (putenv "HOME" "/tmp/home[abc]*?{x}\\suffix")
           (putenv "USERPROFILE" "C:/home[abc]*?{x}"))
         (lambda ()
           (and (glob-match? 'unix "~/file" "/tmp/home[abc]*?{x}\\suffix/file")
                (not (glob-match? 'unix "~/file" "/tmp/homeax{x}suffix/file"))
                (glob-match? 'unix "~/{a,b}" "/tmp/home[abc]*?{x}\\suffix/a")
                (glob-match? 'windows "~\\file" "C:/home[abc]*?{x}/file")))
         (lambda ()
           (if old-home (putenv "HOME" old-home)
               ((foreign-procedure "unsetenv" (string) int) "HOME"))
           (if old-profile (putenv "USERPROFILE" old-profile)
               ((foreign-procedure "unsetenv" (string) int) "USERPROFILE"))))))

(mat glob-home-preserves-parent-components
     (let ([old-home (getenv "HOME")] [old-profile (getenv "USERPROFILE")])
       (dynamic-wind
         (lambda ()
           (putenv "HOME" "/tmp/home[abc]/../.")
           (putenv "USERPROFILE" "C:/home[abc]/../."))
         (lambda ()
           (and (glob-match? 'unix "~/*.txt" "/tmp/home[abc]/.././file.txt")
                (not (glob-match? 'unix "~/*.txt" "/tmp/file.txt"))
                (glob-match? 'windows "~\\*.txt" "C:/home[abc]/.././file.txt")
                (not (glob-match? 'windows "~\\*.txt" "C:/file.txt"))))
         (lambda ()
           (if old-home (putenv "HOME" old-home)
               ((foreign-procedure "unsetenv" (string) int) "HOME"))
           (if old-profile (putenv "USERPROFILE" old-profile)
               ((foreign-procedure "unsetenv" (string) int) "USERPROFILE"))))))

(mat glob-literal-descent
     (let ([root (format "glob-literal-descent-~a" (get-process-id))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (write-string (string-append root "/file") "x")
           (mkdir (string-append root "/dir"))
           (write-string (string-append root "/dir/child") "x")
           (file-symlink "file" (string-append root "/file-link"))
           (file-symlink "absent" (string-append root "/broken-link"))
           (file-symlink "dir" (string-append root "/dir-link")))
         (lambda ()
           (and (equal? '() (glob* (string-append root "/file-link/*") #t #f 'empty))
                (equal? '() (glob* (string-append root "/broken-link/*") #t #f 'empty))
                (equal? '() (glob* (string-append root "/file/*") #t #f 'empty))
                (equal? (list (string-append root "/dir-link/child"))
                        (glob* (string-append root "/dir-link/*") #t #f 'empty))
                (equal? '() (glob (string-append root "/dir-link/*")))))
         (lambda () (file-removetree root)))))

(mat glob-searchable-literal-directory
     (let ([root (format "glob-searchable-~a" (get-process-id))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (write-string (string-append root "/known.txt") "x")
           (file-chmod root #o111))
         (lambda ()
           (equal? (list (string-append root "/known.txt"))
                   (glob (string-append root "/known.txt"))))
         (lambda () (file-chmod root #o700) (file-removetree root)))))

(mat glob-default-flavor
     ;; The one-argument API follows the host OS path syntax.
     (if (eq? (system-platform) 'windows)
         (glob-match? (make-glob "src\\*.ss") "src\\file.ss")
         (glob-match? (make-glob "src/cspp/*.ss") "src/cspp/file.ss")))

(mat glob-literals-and-wildcards
     (glob-match? (make-glob "foo.txt") "foo.txt")
     (not (glob-match? (make-glob "foo.txt") "bar.txt"))
     (glob-match? (make-glob "*.txt") "foo.txt")
     (not (glob-match? (make-glob "*.txt") "foo.ss"))
     (glob-match? (make-glob "file?.txt") "file1.txt")
     (not (glob-match? (make-glob "file?.txt") "file10.txt")))

(mat glob-classes-and-escapes
     (glob-match? (make-glob "file[0-9].txt") "file7.txt")
     (not (glob-match? (make-glob "file[0-9].txt") "filex.txt"))
     (glob-match? (make-glob "file[!0-9].txt") "filex.txt")
     (glob-match? (make-glob "literal\\*.txt") "literal*.txt")
     (glob-match? (make-glob "src\\/cspp") "src/cspp")
     ;; Regex metacharacters in literal glob text must remain literal.
     (glob-match? (make-glob "a.+(b).txt") "a.+(b).txt"))

(mat glob-recursive-and-path-components
     (glob-match? (make-glob "src/cspp/**/test?.ss") "src/cspp/test1.ss")
     (glob-match? (make-glob "src/cspp/**/test?.ss") "src/cspp/a/b/test2.ss")
     (not (glob-match? (make-glob "src/cspp/*/test?.ss") "src/cspp/a/b/test2.ss")))

(mat glob-braces-and-ranges
     (glob-match? (make-glob "*.{ss,sls}") "file.ss")
     (glob-match? (make-glob "*.{ss,sls}") "file.sls")
     (not (glob-match? (make-glob "*.{ss,sls}") "file.txt"))
     (glob-match? (make-glob "file{1..3}.txt") "file2.txt")
     (not (glob-match? (make-glob "file{1..3}.txt") "file4.txt"))
     (glob-match? (make-glob "file{3..1}.txt") "file2.txt")
     (glob-match? (make-glob "file{0..6..2}.txt") "file4.txt"))

(mat glob-tilde
     (let* ([home (path-expand-user (path-parse 'unix "~"))]
            [pattern (path-render (path-add-component home "src"))])
       (glob-match? (make-glob "~/src") pattern)))

(mat glob-windows-flavor
     (glob-match? (make-glob 'windows "src\\*.ss") "src\\file.ss")
     (glob-match? (make-glob 'windows "src/[*].ss") "src/*.ss")
     (glob-match? (make-glob 'windows "C:\\src\\*.ss") "C:\\src\\file.ss")
     (not (glob-match? (make-glob 'windows "src\\*.ss") "src\\nested\\file.ss")))

(mat glob-invalid-patterns
     ;; Unterminated bracket class is rejected during compilation.
     (error? (make-glob "[abc"))
     ;; Trailing escape is rejected during compilation.
     (error? (make-glob "abc\\"))
     ;; Empty brace alternatives are rejected during expansion.
     (error? (make-glob "{a,}"))
     ;; Unsupported named-user tilde expansion is rejected.
     (error? (make-glob "~other/src")))

(mat glob-filesystem-expansion
     (pair? (glob "*.ss"))
     (equal? '() (glob "__cspp_missing_glob_file__"))
     (equal? '("__cspp_missing_glob_file__")
             (glob* "__cspp_missing_glob_file__" #f #f 'literal)))

(mat glob-tilde-preserves-dot-components
     (let* ([home (path-render (path-expand-user (path-parse 'unix "~")))])
       (and (glob-match? 'unix "~/a/../x" (string-append home "/a/../x"))
            (not (glob-match? 'unix "~/a/../x" (string-append home "/x"))))))

(mat glob-lexical-regressions
     (glob-match? (make-glob "?") "\n")
     (glob-match? (make-glob "*") "a\nb")
     (glob-match? (make-glob "[]a]") "]")
     (glob-match? (make-glob "[{]") "{")
     (glob-match? (make-glob "[}]") "}")
     (glob-match? (make-glob "[{a,b}]") ",")
     (glob-match? (make-glob "[^a]") "b")
     (glob-match? (make-glob "a{b,{c,d}}") "ad")
     ;; These paths are lexical fixtures, independent of the repository layout.
     (glob-match? (make-glob 'windows "C:/SRC/*.SS") "c:/src/file.ss")

     ;; A component wildcard must not match a nested directory, even on Windows.
     (not (glob-match? (make-glob 'windows "C:/SRC/*.SS") "c:/src/nested/file.ss"))

     (glob-match? (make-glob "\\~user") "~user")
     (glob-match? (make-glob "a/~/b") "a/~/b")
     ;; Descending classes and malformed ranges must fail during compilation.
     (error? (make-glob "[z-a]"))
     (error? (make-glob "{1..x}"))
     (error? (make-glob "{1..3..-1}"))

     ;; A zero range step must fail rather than loop forever.
     (error? (make-glob "{1..3..0}")))

(define $with-glob-tree
  (lambda (proc)
    (let ([root (format "glob-tree-~a-~a" (get-process-id)
                        (time-nanosecond (current-time)))])
      (dynamic-wind
        (lambda ()
          (mkdir root)
          (mkdir (string-append root "/a"))
          (mkdir (string-append root "/a/deep"))
          (mkdir (string-append root "/unrelated"))
          (for-each (lambda (name) (write-string (string-append root "/" name) "x"))
                    '("z.txt" "a.txt" ".hidden" "a/one.txt" "a/deep/two.txt" "literal*.txt")))
        (lambda () (proc root))
        (lambda () (file-removetree root))))))

(mat glob-dot-components-and-file-pruning
     ($with-glob-tree
       (lambda (root)
         (and (equal? (glob (string-append root "/./a/one.txt"))
                      (list (string-append root "/./a/one.txt")))
              (equal? (glob (string-append root "/a/../a/one.txt"))
                      (list (string-append root "/a/../a/one.txt")))
              (equal? (glob (string-append root "/*/one.txt"))
                      (list (string-append root "/a/one.txt")))))))

(mat glob-trailing-directory-marker
     ($with-glob-tree
       (lambda (root)
         (and (equal? (glob* (string-append root "/a/") #f #t 'empty)
                      (list (string-append root "/a/")))
              (null? (glob* (string-append root "/z.txt/") #f #t 'empty))
              (glob-match? 'unix "a/" "a/")
              (not (glob-match? 'unix "a/" "a"))
              (not (glob-match? 'unix "a" "a/"))))))

(mat glob-current-directory-marker
     ($with-glob-tree
      (lambda (root)
        (let ([original (current-directory)])
          (dynamic-wind
            (lambda () (current-directory root))
            (lambda ()
              (for-all
               (lambda (flavor pattern expected)
                 (let ([matches (glob* flavor pattern #f #t 'empty)])
                   (and (equal? matches expected)
                        (for-all (lambda (name) (glob-match? flavor pattern name)) matches))))
               '(unix windows windows)
               '("**/" "**/" "**\\")
               '(("./" "a/" "a/deep/" "unrelated/")
                 ("./" "a/" "a/deep/" "unrelated/")
                 (".\\" "a\\" "a\\deep\\" "unrelated\\"))))
            (lambda () (current-directory original)))))))

(mat glob-empty-expansion
     (for-all
      (lambda (flavor)
        (and (null? (glob flavor ""))
             (null? (glob* flavor "" #f #t 'empty))
             (null? (glob* flavor "" #t #t 'empty))
             (null? (iter->list (glob->iter flavor "")))
             (equal? '("") (glob* flavor "" #f #t 'literal))
             (glob-match? flavor "" "")))
      '(unix windows)))

(mat glob-windows-root-case-matches-components
     (not (glob-match? 'windows "\\\\straße\\share\\x"
                       "\\\\STRASSE\\share\\x")))

(mat glob-windows-directory-markers
     (glob-match? 'windows "a/" "A/")

     (glob-match? 'windows "a\\" "A\\")

     (glob-match? 'windows "C:/" "c:\\")

     (glob-match? 'windows "//server/share/" "\\\\SERVER\\SHARE\\")

     (not (glob-match? 'windows "a/" "a"))

     (not (glob-match? 'windows "a" "a/")))

(mat glob-precompiled-root-allocation
     ;; Reusing a compiled glob must also reuse its root matchers.
     (for-all
      (lambda (fixture)
        (let* ([compiled (make-glob 'windows (car fixture))]
               [candidate (cadr fixture)]
               [before (statistics)])
          (let loop ([remaining 1000])
            (and (glob-match? compiled candidate)
                 (if (fx= remaining 1)
                     (< (sstats-bytes (sstats-difference (statistics) before)) 5000000)
                     (loop (fx1- remaining)))))))
      '(("C:/dir/*.ss" "c:/dir/file.ss")
        ("//server/share/dir/*.ss" "//SERVER/SHARE/dir/file.ss"))))

(mat glob-rooted-and-pruned-expansion
     ($with-glob-tree
       (lambda (root)
         (let ([prefix (string-append root "/")])
           (and (equal? (glob (string-append prefix "{a,z,a}.txt"))
                        (list (string-append prefix "a.txt") (string-append prefix "z.txt")))
                (equal? (glob (string-append prefix "a/**/*.txt"))
                        (list (string-append prefix "a/deep/two.txt")
                              (string-append prefix "a/one.txt")))
                (equal? (glob (string-append prefix "literal\\*.txt"))
                        (list (string-append prefix "literal*.txt")))
                (equal? (glob* (string-append prefix "a") #f #t 'empty)
                        (list (string-append prefix "a")))
                (null? (glob (string-append prefix "a"))))))))

(mat glob-lazy-reset-and-finalize
     ($with-glob-tree
       (lambda (root)
         (let* ([pattern (string-append root "/a/**/*.txt")]
                [iterator (glob->iter pattern)]
                [first (iter-next! iterator)])
           (and (string? first)
                (begin (iter-reset! iterator)
                       (equal? (sort string<?
                                     (let ([out (make-list-builder)])
                                       (let loop ([x (iter-next! iterator)])
                                         (if (eq? x iter-end) (out)
                                             (begin (out x) (loop (iter-next! iterator)))))))
                               (glob pattern)))
                (eq? iter-end (iter-next! iterator))
                (begin (iter-finalize! iterator) #t)))))
     ($with-glob-tree
       (lambda (root)
         (let ([iterator (glob->iter (string-append root "/unrelated/*"))])
           ;; Creation must not open a stream or collect entries.
           (delete-directory (string-append root "/unrelated"))
           (and (eq? iter-end (iter-next! iterator))
                (begin (iter-finalize! iterator) #t))))))

;; Advancing a finalized iterator must raise an error.
(mat glob-finalized-errors
     (error? (let ([iterator (glob->iter "*.ss")])
               (iter-finalize! iterator)
               (iter-next! iterator))))

(mat glob-expanded-parser-regressions
     (glob-match? (make-glob 'windows "src\\{a,b}.ss") "SRC\\B.SS")
     (glob-match? (make-glob "{a,{1..2}}.txt") "2.txt")
     (glob-match? (make-glob "[a\\-z]") "-")
     (not (glob-match? (make-glob "[a\\-z]") "m")))

(mat glob-recursive-cycle-literal-suffix
     (let ([root (format "glob-cycle-suffix-~a" (get-process-id))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir (path-build root "a"))
           (file-touch (path-build root "x"))
           (file-symlink ".." (path-build root "a/back")))
         (lambda ()
           (for-all
            (lambda (flavor)
              (and (for-all
                    (lambda (suffix)
                      (null? (glob* flavor (string-append root "/a/**/" suffix)
                                    #t #f 'empty)))
                    '("x" "[x]" "?"))
                   ;; Explicit finite traversal through a link remains allowed.
                   (equal? (list (string-append root "/a/back/x"))
                           (glob* flavor (string-append root "/a/back/x") #t #f 'empty))))
            '(unix windows)))
         (lambda () (file-removetree root #t)))))

(mat glob-terminal-globstar-literal-equivalence
     ($with-glob-tree
      (lambda (root)
        (file-symlink "missing" (path-build root "broken"))
        (file-symlink "a" (path-build root "link"))
        (for-all
         (lambda (flavor)
           (for-all
            (lambda (follow?)
              (for-all
               (lambda (directories?)
                 (for-all
                  (lambda (name quoted)
                    (let ([literal (path-build root (string-append name "/**"))]
                          [bracket (path-build root (string-append quoted "/**"))])
                      (and (equal? (glob* flavor literal follow? directories? 'empty)
                                   (glob* flavor bracket follow? directories? 'empty))
                           (member (path-build root name)
                                   (glob* flavor literal follow? directories? 'empty))
                           ;; A marker still requires a directory with the selected link policy.
                           (equal? (glob* flavor (string-append literal "/")
                                          follow? directories? 'empty)
                                   (glob* flavor (string-append bracket "/")
                                          follow? directories? 'empty)))))
                  '("z.txt" "broken" "link")
                  '("[z].txt" "[b]roken" "[l]ink")))
               '(#f #t)))
            '(#f #t)))
         '(unix windows)))))

(mat glob-terminal-globstar-cycle-links
     (let* ([root (format "glob-terminal-cycle-~a" (get-process-id))]
            [directory (path-build root "a")]
            [link (path-build directory "back")])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir directory)
           (file-symlink ".." link))
         (lambda ()
           (for-all
            (lambda (flavor)
              (for-all
               (lambda (directories?)
                 (for-all
                  (lambda (marker)
                    (let ([expected
                           (map (lambda (name) (string-append name marker))
                                (if directories? (list root directory link) (list link)))])
                      (and (equal? expected
                                   (glob* flavor (string-append root "/**" marker)
                                          #t directories? 'empty))
                           (equal? (list (string-append link marker))
                                   (glob* flavor (string-append link "/**" marker)
                                          #t directories? 'empty)))))
                  '("" "/")))
               '(#f #t)))
            '(unix windows)))
         (lambda () (file-removetree root #t)))))

(mat glob-traversal-symlinks
     ($with-glob-tree
       (lambda (root)
         (file-symlink "a" (string-append root "/link"))
         (file-symlink ".." (string-append root "/a/back"))
         (file-symlink "missing" (string-append root "/broken"))
         (let* ([pattern (string-append root "/**/*.txt")]
                [ordinary (glob pattern)]
                [followed (glob* pattern #t #f 'empty)])
           (and (not (member (string-append root "/link/one.txt") ordinary))
                (member (string-append root "/link/one.txt") followed)
                (not (exists (lambda (name)
                               (glob-match? (make-glob "**/back/**/*.txt") name)) followed))
                ;; Literal prefixes also establish ancestors for cycle detection.
                (not (exists (lambda (name) (string-search name "/back/"))
                             (glob* (string-append root "/a/**/*.txt") #t #f 'empty)))
                (equal? (glob (string-append root "/broken"))
                        (list (string-append root "/broken")))
                (let ([lazy (iter->list (glob->iter pattern))])
                  (and (= (length ordinary) (length lazy))
                       (for-all (lambda (name) (and (member name ordinary) #t)) lazy))))))))

(mat glob-stream-lifecycle
     ($with-glob-tree
       (lambda (root)
         (let* ([fd-count (lambda () (length (directory-list "/proc/self/fd")))]
                [before (fd-count)]
                [iterator (glob->iter (string-append root "/a/**/*.txt"))])
           (and (iter? iterator)
                (= before (fd-count))
                (string? (iter-next! iterator))
                (> (fd-count) before)
                (begin (iter-reset! iterator) (= before (fd-count)))
                (string? (iter-next! iterator))
                (begin (iter-finalize! iterator) (= before (fd-count))))))))

(mat glob-hidden-and-component-order
     ($with-glob-tree
       (lambda (root)
         (and (member (string-append root "/.hidden") (glob (string-append root "/*")))
              (equal? (glob (string-append root "/{a/one.txt,a.txt}"))
                      (list (string-append root "/a/one.txt")
                            (string-append root "/a.txt")))))))

(mat glob-pruning-and-error-cleanup
     ($with-glob-tree
       (lambda (root)
         (let ([unrelated (string-append root "/unrelated")])
           (dynamic-wind
             (lambda () (file-chmod unrelated #o000))
             (lambda ()
               (equal? (glob (string-append root "/a/*.txt"))
                       (list (string-append root "/a/one.txt"))))
             (lambda () (file-chmod unrelated #o700))))))
     ;; Root can open mode-000 directories; permission failure is checked as a user.
     (or (= (geteuid) 0)
         ($with-glob-tree
           (lambda (root)
             (let* ([deep (string-append root "/a/deep")]
                    [before (length (directory-list "/proc/self/fd"))]
                    [iterator (glob->iter (string-append root "/a/**/deep/*"))])
               (dynamic-wind
                 (lambda () (file-chmod deep #o000))
                 (lambda ()
                   (and (guard (condition [else #t]) (iter-next! iterator) #f)
                        (= before (length (directory-list "/proc/self/fd")))
                        (eq? iter-end (iter-next! iterator))))
                 (lambda () (iter-finalize! iterator) (file-chmod deep #o700))))))))

(mat glob-wildcard-directory-inclusion
     ($with-glob-tree
       (lambda (root)
         (let ([pattern (string-append root "/a/*")])
           (and (equal? (glob pattern) (list (string-append root "/a/one.txt")))
                (equal? (glob* pattern #f #t 'empty)
                        (list (string-append root "/a/deep")
                              (string-append root "/a/one.txt"))))))))

(mat glob-filesystem-home-physical-parents
     ($with-glob-tree
      (lambda (root)
        (let* ([parent (path-build (current-directory) root)]
               [saved-home (getenv "HOME")]
               [home (path-build parent "home-alias/..")]
               [expected (list (path-build home "expected.txt"))])
          (mkdirs (path-build parent "physical/sub"))
          (file-symlink "physical/sub" (path-build parent "home-alias"))
          (write-string (path-build parent "physical/expected.txt") "expected")
          (write-string (path-build parent "wrong.txt") "wrong")
          (dynamic-wind
            (lambda () (putenv "HOME" home))
            (lambda ()
              (let ([names (glob* "~/*.txt" #t #f 'empty)])
                (and (equal? names expected)
                     (equal? (map read-string names) '("expected")))))
            (lambda ()
              (if saved-home (putenv "HOME" saved-home)
                  ((foreign-procedure "unsetenv" (string) int) "HOME"))))))))

(mat glob-filesystem-tilde
     (let ([original-directory (current-directory)]
           [home (path-render (path-expand-user (path-parse 'unix "~")))])
       (dynamic-wind
         (lambda () (current-directory home))
         (lambda ()
           ($with-glob-tree
             (lambda (root)
               (let* ([pattern (string-append "~/" root "/a/*.txt")]
                      [expected (list (string-append home "/" root "/a/one.txt"))])
                 (and (equal? (glob pattern) expected)
                      (equal? (iter->list (glob->iter pattern)) expected))))))
         (lambda () (current-directory original-directory)))))

;; Escaped leading tilde is a literal directory component at filesystem time.
(mat glob-filesystem-escaped-tilde
     (let ([original (current-directory)]
           [root (format "glob-tilde-literal-~a" (get-process-id))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir (string-append root "/~"))
           (write-string (string-append root "/~/item.txt") "x")
           (current-directory root))
         (lambda ()
           (let ([names (glob "\\~/*")])
             (and (equal? names '("./~/item.txt"))
                  (equal? '("x") (map read-string names))
                  (equal? names (iter->list (glob->iter "[~]/*"))))))
         (lambda () (current-directory original) (file-removetree root)))))

;; Windows literal components resolve actual entries case-insensitively.
(mat glob-filesystem-windows-case
     ($with-glob-tree
       (lambda (root)
         (let ([actual (string-append root "/CaseDir")])
           (mkdir actual)
           (write-string (string-append actual "/MiXeD.TXT") "x")
           (equal? (glob* 'windows
                         (string-append root "/casedir/mixed.txt")
                         #f #f 'empty)
                   (list (string-append root "/CaseDir/MiXeD.TXT")))))))

(mat glob-windows-case-collisions
     (let ([root (format "glob-case-collisions-~a" (get-process-id))])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (for-each
            (lambda (directory file)
              (mkdir (path-build root directory))
              (file-touch (path-build (path-build root directory) file))
              (file-touch (path-build (path-build root directory) "shared.bin")))
            '("K" "k" "Σ" "σ") '("upper.txt" "lower.txt" "upper.txt" "lower.txt")))
         (lambda ()
           (and
            (for-all
             (lambda (literal bracket expected)
               (let* ([prefix (string-append (string-upcase root) "/")]
                      [pattern (string-append prefix literal "/*.txt")]
                      [matches (glob 'windows pattern)])
                 (and (equal? matches (glob 'windows (string-append prefix bracket "/*.txt")))
                      (equal? (sort string<? matches) (sort string<? expected))
                      (for-all file-exists? matches)
                      (for-all (lambda (path) (glob-match? 'windows pattern path)) matches))))
             '("k" "σ") '("[k]" "[σ]")
             (list (list (string-append root "/K/upper.txt")
                         (string-append root "/k/lower.txt"))
                   (list (string-append root "/Σ/upper.txt")
                         (string-append root "/σ/lower.txt"))))
            ;; Different alternatives that resolve to one entry yield its actual spelling once.
            (equal? (glob 'windows (string-append root "/{K,k}/upper.txt"))
                    (list (string-append root "/K/upper.txt")))
            (equal? (glob 'windows (string-append root "/[k]/shared.bin"))
                    (list (string-append root "/K/shared.bin")
                          (string-append root "/k/shared.bin")))))
         (lambda () (file-removetree root #t)))))

;; A literal prefix that exists but cannot be opened is an expansion error.
(mat glob-filesystem-inaccessible-literal
     (or (= (geteuid) 0)
         ($with-glob-tree
           (lambda (root)
             (let ([blocked (string-append root "/blocked")])
               (mkdir blocked)
               (write-string (string-append blocked "/child.txt") "x")
               (dynamic-wind
                 (lambda () (file-chmod blocked #o000))
                 (lambda ()
                   (guard (condition [else #t])
                     (glob (string-append root "/blocked/child.txt"))
                     #f))
                 (lambda () (file-chmod blocked #o700))))))))

;; Recursive directory matching includes a rooted directory when requested.
(mat glob-recursive-directory-root
     ($with-glob-tree
      (lambda (root)
        (equal? (glob* (string-append root "/**") #f #t 'empty)
                (list root
                      (string-append root "/.hidden")
                      (string-append root "/a")
                      (string-append root "/a/deep")
                      (string-append root "/a/deep/two.txt")
                      (string-append root "/a/one.txt")
                      (string-append root "/a.txt")
                      (string-append root "/literal*.txt")
                      (string-append root "/unrelated")
                      (string-append root "/z.txt")))))
)
