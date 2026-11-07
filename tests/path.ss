(import (cspp))


(define same-path?
  (lambda (p s)
    (string=? (path-render p) s)))

(mat path-incomplete-windows-namespaces
     ;; Error cases: recognized device namespaces require a root, and UNC requires a share.
     (for-all
      (lambda (text)
        (guard (failure [else #t]) (path-parse 'windows text) #f))
      '("//?/" "//./" "//?/UNC" "//?/UNC/server" "//./UNC/server/"))

     (for-all
      (lambda (text)
        (let* ([path (path-add-component (path-parse 'windows text) "leaf")]
               [again (path-parse 'windows (path-render path))])
          (and (equal? (path-root-info path) (path-root-info again))
               (equal? '("leaf") (path-components again)))))
      '("//?/UNC/server/share" "//?/C:/" "//./COM1")))

(mat path-bulk-construction
     (let ([small (make-list 128 "x")] [large (make-list 1024 "x")])
       (for-all
        (lambda (build)
          (let ([allocation
                 (lambda (parts)
                   (collect)
                   (let* ([before (statistics)] [result (build parts)]
                          [bytes (sstats-bytes (sstats-difference (statistics) before))])
                     (and (= (length parts) (length (path-components result))) bytes)))])
            (let ([short (allocation small)] [long (allocation large)])
              ;; Eight times as many parts should stay well below quadratic allocation.
              (and short long (< long (* 12 short))))))
        (list (lambda (parts) (apply path-join (path 'unix) parts))
              (lambda (parts)
                (apply path-append (path 'unix)
                       (map (lambda (part) (path 'unix part)) parts))))))

     (let ([base (path-parse 'windows "//?/C:/base")])
       (and (same-path? (path-join base "old" "D:/new" "d:child") "D:\\new\\child")
            (same-path? (path-join base "old" "/new" "c:child") "\\\\?\\C:\\new\\child")
            (same-path? (path-join base (path-parse 'windows "leaf/"))
                        "\\\\?\\C:\\base\\leaf\\"))))

(mat path-join-string-component-contract
     (for-all
      (lambda (base)
        (and (string=? "leaf" (path-basename (path-join base "leaf")))
             ;; Error cases: relative strings must not become path fragments or empty components.
             (for-all
              (lambda (part)
                (guard (condition [else #t])
                  (path-join base part)
                  #f))
              '("" "a/b" "leaf/"))))
      (list (path-rooted 'unix "base") (path-drive-absolute #\C "base")))

     ;; A backslash is a separator only in Windows string components.
     (guard (condition [else #t])
       (path-join (path-drive-absolute #\C "base") "a\\b")
       #f)

     (string=? "a\\b" (path-basename (path-join (path-rooted 'unix "base") "a\\b")))

     (same-path? (path-join (path-rooted 'unix "base") (path 'unix "a" "b")) "/base/a/b"))

(mat path-windows-simple-case-comparison
     (same-path? (path-relative-to (path-parse 'windows "C:/straße")
                                   (path-parse 'windows "c:/STRASSE")) "..\\STRASSE")

     (not (path-same-root? (path-parse 'windows "//server/straße/a")
                           (path-parse 'windows "//SERVER/STRASSE/a")))

     (not (path-same-root? (path-parse 'windows "//straße/share/a")
                           (path-parse 'windows "//STRASSE/share/a")))

     (path-same-root? (path-parse 'windows "//σ/share/a")
                      (path-parse 'windows "//Σ/SHARE/a"))

     (same-path? (path-relative-to (path-parse 'windows "C:/σ/child")
                                   (path-parse 'windows "c:/Σ/CHILD/leaf")) "leaf"))

(mat path-windows-relative-rendering
     (for-all
      (lambda (p)
        (for-all
         (lambda (separator)
           (let ([parsed (path-parse 'windows (path-render p separator))])
             (and (equal? (path-root-info p) (path-root-info parsed))
                  (equal? (path-components (path-normalize p))
                          (path-components (path-normalize parsed))))))
         '(#\/ #\\)))
      (list (path 'windows "C:foo")
            (path 'windows "1:foo" "child")
            (path-normalize (path-parse 'windows ".\\C:foo"))))

     ;; Reserved UNC server names select Windows device namespaces instead.
     (error? (path-unc "." "COM1" "child"))

     ;; The question-mark server prefix selects the extended namespace.
     (error? (path-unc "?" "C:" "child")))

(mat path-plain-input-needs-no-home
     (let* ([names '("HOME" "USERPROFILE" "HOMEDRIVE" "HOMEPATH")]
            [saved (map getenv names)]
            [unset (foreign-procedure "unsetenv" (string) int)])
       (dynamic-wind
         (lambda () (for-each unset names))
         (lambda ()
           (and (same-path? (path-expand-user (path 'unix "file")) "file")
                (same-path? (path-expand-user (path 'windows "file")) "file")))
         (lambda ()
           (for-each (lambda (name value) (if value (putenv name value) (unset name)))
                     names saved)))))

(mat path-special-render-owns-result
     ;; Rendered device roots must not expose storage shared by derived paths.
     (let* ([original (path-parse 'windows "\\\\.\\COM1")]
            [derived (path-add-component original "child")]
            [rendered (path-render original)])
       (string-set! rendered 4 #\X)
       (and (same-path? original "\\\\.\\COM1")
            (same-path? derived "\\\\.\\COM1\\child"))))

(mat path-drive-relative-relativization
     ;; Internal parents are cancellable even though path-normalize preserves their spelling.
     (same-path? (path-relative-to (path-parse 'windows "C:a/../b")
                                   (path-parse 'windows "C:b")) "")

     (same-path? (path-relative-to (path-parse 'windows "C:a/../b")
                                   (path-parse 'windows "C:b/c/../d")) "d")

     (same-path? (path-relative-to (path-parse 'windows "C:../a/../b")
                                   (path-parse 'windows "C:../c")) "..\\c")

     ;; An unmatched leading parent in the base cannot be inverted without an absolute base.
     (error? (path-relative-to (path-parse 'windows "C:a/../../b")
                               (path-parse 'windows "C:b"))))

(mat path-user-home-validation
     ;; Validate explicit home types even when the input needs no expansion.
     (error? (path-expand-user (path 'unix "file") 123))

     (same-path? (path-expand-user (path 'unix "file") "/home/me") "file"))

(mat path-rooted-tilde-is-literal
     (for-all
      (lambda (p)
        (and (not (path-starts-with-user-home? p))
             (eq? p (path-expand-user p))
             (eq? p (path-expand-user p
                        (if (eq? (path-flavor p) 'unix) "/home/me" "C:/Users/me")))))
      (list (path-parse 'unix "/~/x")
            (path-parse 'unix "/~/../x")
            (path-parse 'windows "D:/~/x")
            (path-parse 'windows "D:~/x")
            (path-parse 'windows "/~/x")
            (path-parse 'windows "//server/share/~/x")
            (path-parse 'windows "//?/D:/~/x")))

     (same-path? (path-expand-user (path 'unix "~" "x") "/home/me") "/home/me/x")

     (same-path? (path-expand-user (path 'windows "~" "x") "C:/Users/me")
                 "C:\\Users\\me\\x"))

(mat path-private-components-do-not-need-recopying
     ;; A short append should not recopy a large, already-owned component each time.
     (let* ([base (path 'unix (make-string 4096 #\x))]
            [before (statistics)]
            [last (let loop ([i 0] [last base])
                    (if (fx= i 200) last
                        (loop (fx1+ i) (path-add-component base "tail"))))]
            [allocated (sstats-bytes (sstats-difference (statistics) before))])
       (and (string=? "tail" (path-basename last))
            (< allocated 500000))))

(mat path-edits-snapshot-caller-storage
     (let* ([component (string-copy "child")]
            [components (list component)]
            [base (path-parse 'unix "/parent")]
            [added (path-add-component base component)]
            [appended (path-add-components base components)]
            [replaced (path-replace-basename base component)])
       (set-car! components "different")
       (string-set! component 0 #\/)
       (and (same-path? base "/parent")
            (same-path? added "/parent/child")
            (same-path? appended "/parent/child")
            (same-path? replaced "/child"))))

(mat path-derived-values-keep-private-storage
     (let* ([original (path-parse 'windows "C:/parent/child")]
            [derived (path-with-trailing-directory (path-normalize original) #t)]
            [root (path-root-info derived)]
            [components (path-components derived)]
            [basename (path-basename derived)]
            [stem (path-stem derived)])
       (string-set! (caddr root) 0 #\D)
       (string-set! (car components) 0 #\X)
       (set-car! components "different")
       (string-set! basename 0 #\X)
       (string-set! stem 0 #\X)
       (and (same-path? original "C:\\parent\\child")
            (same-path? derived "C:\\parent\\child\\"))))


(mat path-parse/render

     (path? (path-parse 'unix ""))
     (path-flavor? 'unix)
     (path-flavor? 'windows)
     (not (path-flavor? 'posix))

     (equal? '() (path-components (path-parse 'unix "")))
     (same-path? (path-parse 'unix "") "")
     (same-path? (path-parse 'unix "/") "/")
     (same-path? (path-parse 'unix "///usr//local///bin/") "/usr/local/bin/")
     (equal? '(unix absolute #f) (path-root-info (path-parse 'unix "/usr/local")))
     (equal? '("usr" "local") (path-components (path-parse 'unix "/usr/local")))
     (not (eq? (path-components (path-parse 'unix "/usr/local"))
               (path-components (path-parse 'unix "/usr/local"))))

     (let* ([component (string-copy "usr")]
            [p (path 'unix component)])
       (string-set! component 0 #\/)
       (string=? "usr" (path-render p)))
     (let* ([p (path-parse 'unix "usr/local")]
            [basename (path-basename p)])
       (string-set! basename 0 #\/)
       (string=? "usr/local" (path-render p)))
     (let* ([p (path-parse 'windows "C:/usr")]
            [info (path-root-info p)])
       (string-set! (caddr info) 0 #\D)
       (string=? "C:\\usr" (path-render p)))
     (same-path? (path-join (path-parse 'windows "C:/base")
                            (path-parse 'windows "/foo"))
                 "C:\\foo")
     (same-path? (path-join (path-parse 'windows "C:/base") "/foo")
                 "C:\\foo")
     ;; A drive-relative path on another drive depends on ambient per-drive CWD.
     (error? (path-join (path-parse 'windows "C:/base")
                        (path-parse 'windows "D:foo")))

     (same-path? (path-parse 'windows "foo/bar\\baz") "foo\\bar\\baz")
     (same-path? (path-parse 'windows "\\foo\\bar") "\\foo\\bar")
     (same-path? (path-parse 'windows "c:foo\\bar") "C:foo\\bar")
     (same-path? (path-parse 'windows "c:/foo/bar") "C:\\foo\\bar")
     (same-path? (path-parse 'windows "\\\\server\\share\\dir") "\\\\server\\share\\dir")
     (same-path? (path-parse 'windows "\\\\?\\C:\\") "\\\\?\\C:\\")
     (same-path? (path-parse 'windows "\\\\?\\C:\\x") "\\\\?\\C:\\x")
     (path-same-root? (path-parse 'windows "\\\\?\\C:\\")
                      (path-parse 'windows "\\\\?\\C:\\x"))
     (same-path? (path-add-component (path-parse 'windows "\\\\?\\C:\\") "x")
                 "\\\\?\\C:\\x")
     (same-path? (path-parse 'windows "\\\\.\\COM1") "\\\\.\\COM1")
     (string=? (path-render (path-parse 'windows "c:/foo/bar") #\/) "C:/foo/bar")
     (same-path? (path-parse 'windows "\\\\?\\Volume{abc}\\")
                 "\\\\?\\Volume{abc}\\")
     (equal? '(windows drive-relative "C") (path-root-info (path-parse 'windows "c:foo")))
     (equal? '(windows drive-absolute "C") (path-root-info (path-parse 'windows "c:/foo")))
     (equal? '(windows unc "server" "share") (path-root-info (path-parse 'windows "\\\\server\\share\\dir")))

     ;; invalid path flavor
     (error? (path-parse 'posix "x"))
     ;; UNC path missing share
     (error? (path-parse 'windows "\\\\server"))
     ;; invalid Windows drive syntax
     (error? (path-drive "CD" "x"))
     )


(mat path-construct/join

     (for-all
      (lambda (prefix)
        (let ([base (path-parse 'windows (string-append prefix "C:\\base"))])
          (and (same-path? (path-join base "\\child")
                           (string-append prefix "C:\\child"))
               (same-path? (path-join base (path-parse 'windows "\\child"))
                           (string-append prefix "C:\\child"))
               (same-path? (path-join base "c:child")
                           (string-append prefix "C:\\base\\child"))
               (same-path? (path-absolute base (path-parse 'windows "\\child"))
                           (string-append prefix "C:\\child"))
               (same-path? (path-absolute base (path-parse 'windows "c:child"))
                           (string-append prefix "C:\\base\\child")))))
      '("\\\\?\\" "\\\\.\\"))

     ;; Error case: extended drive bases cannot resolve another drive's relative path.
     (error? (path-join (path-parse 'windows "\\\\?\\C:\\base") "D:child"))

     (same-path? (path 'unix "a" "b") "a/b")
     (same-path? (path-rooted 'unix "a" "b") "/a/b")
     (same-path? (path 'windows "a" "b") "a\\b")
     (same-path? (path-rooted 'windows "a" "b") "\\a\\b")
     (same-path? (path-drive "c" "tmp") "C:tmp")
     (same-path? (path-drive-absolute #\c "tmp") "C:\\tmp")
     (same-path? (path-unc "srv" "share" "tmp") "\\\\srv\\share\\tmp")

     (same-path? (path-join (path-parse 'unix "/usr") "local" "bin") "/usr/local/bin")
     (same-path? (path-join (path-parse 'unix "/usr") (path-parse 'unix "/opt") "bin") "/opt/bin")
     (same-path? (path-join (path-parse 'windows "C:\\Users") "maoif" "src") "C:\\Users\\maoif\\src")
     (same-path? (path-join (path-parse 'windows "C:\\Users") (path-parse 'windows "D:\\tmp")) "D:\\tmp")

     (same-path? (path-append (path-parse 'unix "/usr") (path-parse 'unix "local/bin")) "/usr/local/bin")

     ;; component contains a separator
     (error? (path 'unix "a/b"))
     ;; component contains a Windows separator
     (error? (path 'windows "a\\b"))
     ;; strict append rejects absolute replacement
     (error? (path-append (path-parse 'unix "/usr") (path-parse 'unix "/opt")))
     ;; strict append rejects incompatible flavors
     (error? (path-append (path-parse 'unix "/usr") (path-parse 'windows "tmp")))
     )


(mat path-edit/select

     (same-path? (path-add-component (path-parse 'unix "/usr/local") "bin") "/usr/local/bin")
     (same-path? (path-add-components (path-parse 'unix "/usr") '("local" "bin")) "/usr/local/bin")
     (same-path? (path-replace-basename (path-parse 'unix "/usr/local/bin") "sbin") "/usr/local/sbin")
     (same-path? (path-drop-basename (path-parse 'unix "/usr/local/bin")) "/usr/local")
     (same-path? (path-with-trailing-directory (path-parse 'unix "/usr/local") #t) "/usr/local/")
     (same-path? (path-with-trailing-directory (path-parse 'unix "/usr/local/") #f) "/usr/local")

     (same-path? (path-normalize (path-parse 'unix "/a/./b/../c")) "/a/c")
     (same-path? (path-normalize (path-parse 'unix "../a/..")) "..")
     (same-path? (path-normalize (path-parse 'windows "C:a\\..\\b")) "C:a\\..\\b")
     (same-path? (path-normalize (path-parse 'windows "C:\\a\\.\\b\\..\\c")) "C:\\a\\c")

     (path-absolute-path? (path-parse 'unix "/usr"))
     (not (path-absolute-path? (path-parse 'windows "C:usr")))
     (path-relative-path? (path-parse 'unix "usr"))
     (path-rooted-path? (path-parse 'windows "\\usr"))
     (not (path-rooted-path? (path-parse 'windows "usr")))

     (equal? "file.txt" (path-basename (path-parse 'unix "/tmp/file.txt")))
     (not (path-basename (path-parse 'unix "/")))
     (same-path? (path-dirname (path-parse 'unix "/tmp/file.txt")) "/tmp")
     (same-path? (path-dirname (path-parse 'unix "/")) "/")
     (same-path? (path-dirname (path-parse 'unix "foo")) "")
     (equal? "gz" (path-extname (path-parse 'unix "archive.tar.gz")))
     (equal? "archive.tar" (path-stem (path-parse 'unix "archive.tar.gz")))
     (not (path-extname (path-parse 'unix ".profile")))
     (equal? ".profile" (path-stem (path-parse 'unix ".profile")))
     (not (path-extname (path-parse 'unix "foo.")))
     (equal? "foo." (path-stem (path-parse 'unix "foo.")))

     ;; cannot replace basename of an empty path
     (error? (path-replace-basename (path-parse 'unix "") "x"))
     )


(mat path-absolute/relative/home

     (same-path? (path-absolute (path-parse 'unix "/home/me") (path-parse 'unix "src/cspp/../cspp")) "/home/me/src/cspp")
     (same-path? (path-absolute (path-parse 'windows "C:\\Users\\me") (path-parse 'windows "src")) "C:\\Users\\me\\src")
     (same-path? (path-absolute (path-parse 'windows "C:\\Users\\me") (path-parse 'windows "C:src")) "C:\\Users\\me\\src")

     (path-same-root? (path-parse 'unix "/a") (path-parse 'unix "/b"))
     (path-same-root? (path-parse 'windows "C:\\a") (path-parse 'windows "c:\\b"))
     (path-same-root? (path-parse 'windows "\\\\srv\\share\\a") (path-parse 'windows "\\\\SRV\\SHARE\\b"))
     (not (path-same-root? (path-parse 'windows "C:\\a") (path-parse 'windows "D:\\a")))

     (same-path? (path-relative-to (path-parse 'unix "/home/me") (path-parse 'unix "/home/me/src/cspp/lib")) "src/cspp/lib")
     (same-path? (path-relative-to (path-parse 'unix "/home/me/src") (path-parse 'unix "/home/me/doc")) "../doc")
     (same-path? (path-relative-to (path-parse 'windows "C:/Users/Me")
                                   (path-parse 'windows "c:/users/me/src"))
                 "src")
     (same-path? (path-relative-to (path-parse 'windows "C:\\Users\\me") (path-parse 'windows "C:\\Users\\me\\src")) "src")
     (same-path? (path-relative-to (path-parse 'windows "\\\\srv\\share\\a") (path-parse 'windows "\\\\srv\\share\\a\\b")) "b")

     (path-starts-with-user-home? (path-parse 'unix "~/src"))
     (not (path-starts-with-user-home? (path-parse 'unix "~user/src")))
     (same-path? (path-expand-user (path-parse 'unix "~/cspp") (path-parse 'unix "/home/me")) "/home/me/cspp")
     (same-path? (path-expand-user (path-parse 'windows "~\\src") (path-parse 'windows "C:\\Users\\me")) "C:\\Users\\me\\src")

     ;; absolute conversion requires an absolute base
     (error? (path-absolute (path-parse 'unix "home/me") (path-parse 'unix "src")))
     ;; relative conversion rejects incompatible roots
     (error? (path-relative-to (path-parse 'windows "C:\\a") (path-parse 'windows "D:\\a")))
     ;; relative conversion cannot reconstruct a target from divergent unresolved parents
     (error? (path-relative-to (path-parse 'unix "../a") (path-parse 'unix "b")))
     ;; user expansion requires an absolute explicit home
     (error? (path-expand-user (path-parse 'unix "~/src") (path-parse 'unix "home/me")))
     )
