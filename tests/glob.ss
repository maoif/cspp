(import (cspp) (cspp glob))

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
                (equal? (list (string-append root "/strasse.txt"))
                        (glob 'windows (string-append root "/strasse.txt")))
                (glob-match? 'windows "σ.txt" "Σ.txt")
                (equal? (list (string-append root "/σ.txt"))
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

(mat glob-filesystem-iterator
     (let ([iter (glob->iter "*.ss")])
       (and (string? (iter-next! iter))
            (iter? iter)
            (begin (iter-finalize! iter) #t))))

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
     (error? (make-glob "{1..3..-1}")))

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
              (null? (glob* (string-append root "/z.txt/") #f #t 'empty))))))

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

;; A zero range step must fail rather than loop forever.
(mat glob-zero-step
     (error? (make-glob "{1..3..0}")))

(mat glob-expanded-parser-regressions
     (glob-match? (make-glob 'windows "src\\{a,b}.ss") "SRC\\B.SS")
     (glob-match? (make-glob "{a,{1..2}}.txt") "2.txt")
     (glob-match? (make-glob "[a\\-z]") "-")
     (not (glob-match? (make-glob "[a\\-z]") "m")))

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
           (and (= before (fd-count))
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
         (lambda () (equal? (glob "\\~/*") '("~/item.txt")))
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
                   (list (string-append root "/casedir/mixed.txt")))))))

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
