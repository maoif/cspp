(import (cspp))


(define same-path?
  (lambda (p s)
    (string=? (path-render p) s)))


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
