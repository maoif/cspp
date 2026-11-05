#!chezscheme

(library (cspp path)
  (export path? path-flavor?
          path-parse path-render path-flavor path-components path-root-info
          path path-rooted path-drive path-drive-absolute path-unc
          path-join path-append
          path-add-component path-add-components path-replace-basename path-drop-basename
          path-normalize path-with-trailing-directory
          path-absolute-path? path-relative-path? path-rooted-path?
          path-basename path-dirname path-extname path-stem
          path-absolute path-relative-to path-same-root?
          path-expand-user path-starts-with-user-home?)
  (import (chezscheme)
          (cspp utils))

  #|proc:path?
  The `path?` procedure returns `#t` when `x` is a path object.
  |#
  (define-record-type ($path-record mk-path path?)
    (nongenerative)
    (fields (immutable flavor $path-flavor)
            (immutable root-kind $path-root-kind)
            (immutable root $path-root)
            (immutable components $path-components)
            (immutable trailing? $path-trailing?)))

  ;; Adopt private storage: parsers create fresh strings; public constructors and edits
  ;; copy caller input before reaching here. Derived paths may share these private values
  ;; because no operation mutates them and public selectors return defensive copies.
  (define $make-path-object
    (lambda (flavor root-kind root components trailing?)
      (mk-path flavor root-kind root components
               (and trailing? (not (null? components))))))

  (define $err
    (lambda (who msg . args)
      (apply errorf who msg args)))

  (define $path-flavor?
    (lambda (x)
      (or (eq? x 'unix) (eq? x 'windows))))

  #|proc:path-flavor?
  The `path-flavor?` procedure returns `#t` for supported path flavors.
  Supported flavors are `'unix` and `'windows`.
  |#
  (define path-flavor?
    (lambda (x)
      ($path-flavor? x)))

  (define $check-flavor
    (lambda (who flavor)
      (unless ($path-flavor? flavor)
        ($err who "invalid path flavor: ~s" flavor))))

  (define $sep?
    (lambda (flavor c)
      (if (eq? flavor 'unix)
          (char=? c #\/)
          (or (char=? c #\\) (char=? c #\/)))))

  (define $nul?
    (lambda (c)
      (fx= (char->integer c) 0)))

  (define $string-has-nul?
    (lambda (s)
      (let ([n (string-length s)])
        (let loop ([i 0])
          (and (fx< i n)
               (or ($nul? (string-ref s i))
                   (loop (fx+ i 1))))))))

  (define $string-has-sep?
    (lambda (flavor s)
      (let ([n (string-length s)])
        (let loop ([i 0])
          (and (fx< i n)
               (or ($sep? flavor (string-ref s i))
                   (loop (fx+ i 1))))))))

  (define $valid-component?
    (lambda (flavor x)
      (and (string? x)
           (not (string=? x ""))
           (not ($string-has-nul? x))
           (not ($string-has-sep? flavor x)))))

  (define $check-component
    (lambda (who flavor component)
      (unless ($valid-component? flavor component)
        ($err who "invalid path component for ~s path: ~s" flavor component))))

  (define $check-components
    (lambda (who flavor components)
      (for-each (lambda (component) ($check-component who flavor component)) components)))

  (define $component-list?
    (lambda (components)
      (and (list? components) (andmap string? components))))

  (define $letter?
    (lambda (c)
      (or (and (char>=? c #\a) (char<=? c #\z))
          (and (char>=? c #\A) (char<=? c #\Z)))))

  (define $drive->string
    (lambda (who drive)
      (cond [(char? drive)
             (if ($letter? drive)
                 (string (char-upcase drive))
                 ($err who "invalid Windows drive: ~s" drive))]
            [(and (string? drive)
                  (fx= (string-length drive) 1)
                  ($letter? (string-ref drive 0)))
             (string (char-upcase (string-ref drive 0)))]
            [else ($err who "invalid Windows drive: ~s" drive)])))

  (define $copy-components
    (lambda (components)
      (map string-copy components)))

  (define $string-split-path
    (lambda (who flavor s start)
      (let ([n (string-length s)])
        (let loop ([i start] [left start] [acc '()])
          (cond [(fx= i n)
                 (let ([acc (if (fx= left i)
                                acc
                                (cons (substring s left i) acc))])
                   (let ([components (reverse acc)])
                     ($check-components who flavor components)
                     components))]
                [($sep? flavor (string-ref s i))
                 (loop (fx+ i 1) (fx+ i 1)
                       (if (fx= left i)
                           acc
                           (cons (substring s left i) acc)))]
                [else (loop (fx+ i 1) left acc)])))))

  (define $trailing-sep?
    (lambda (flavor s)
      (let ([n (string-length s)])
        (and (fx> n 0)
             ($sep? flavor (string-ref s (fx- n 1)))))))

  (define $two-leading-seps?
    (lambda (s)
      (and (fx>= (string-length s) 2)
           ($sep? 'windows (string-ref s 0))
           ($sep? 'windows (string-ref s 1)))))

  (define $windows-special-prefix?
    (lambda (s)
      (and (fx>= (string-length s) 4)
           ($two-leading-seps? s)
           (let ([c (string-ref s 2)])
             (and (or (char=? c #\?) (char=? c #\.))
                  ($sep? 'windows (string-ref s 3)))))))

  (define $prefix-special-unc?
    (lambda (root)
      (let ([prefix "\\\\?\\UNC\\"])
        (and (fx>= (string-length root) (string-length prefix))
             (string-ci=? prefix (substring root 0 (string-length prefix)))))))

  (define $special-volume-root?
    (lambda (root)
      (let ([prefix "\\\\?\\Volume{"] [n (string-length root)])
        (and (fx> n (string-length prefix))
             (string-ci=? prefix (substring root 0 (string-length prefix)))
             (char=? (string-ref root (fx1- n)) #\})))))

  (define $windows-special-root
    (lambda (who s)
      (let* ([head (string-append "\\\\" (string (string-ref s 2)) "\\")]
             [parts ($string-split-path who 'windows s 4)]
             [trailing? ($trailing-sep? 'windows s)])
        (cond [(null? parts)
               ($make-path-object 'windows 'special head '() #f)]
              [(and (string-ci=? (car parts) "UNC")
                    (pair? (cdr parts))
                    (pair? (cddr parts)))
               (let ([components (cdddr parts)])
                 ($make-path-object 'windows
                        'special
                        (let ([root (string-append head "UNC\\" (cadr parts) "\\" (caddr parts))])
                          root)
                        components
                        trailing?))]
              [else
               (let ([components (cdr parts)])
                 ($make-path-object 'windows
                        'special
                        (let ([root (string-append head (car parts))])
                          root)
                        components
                        trailing?))]))))

  (define $parse-unix
    (lambda (who s)
      (let* ([n (string-length s)]
             [absolute? (and (fx> n 0) (char=? (string-ref s 0) #\/))]
             [components ($string-split-path who 'unix s (if absolute? 1 0))])
        ($make-path-object 'unix
               (if absolute? 'absolute 'relative)
               #f
               components
               (and ($trailing-sep? 'unix s) (not (null? components)))))))

  (define $parse-windows
    (lambda (who s)
      (let ([n (string-length s)])
        (cond [(fx= n 0) ($make-path-object 'windows 'relative #f '() #f)]
              [($windows-special-prefix? s) ($windows-special-root who s)]
              [($two-leading-seps? s)
               (let ([parts ($string-split-path who 'windows s 2)])
                 (if (and (pair? parts) (pair? (cdr parts)))
                     ($make-path-object 'windows
                            'unc
                            (list (car parts) (cadr parts))
                            (cddr parts)
                            (and ($trailing-sep? 'windows s) (not (null? (cddr parts)))))
                     ($err who "UNC path requires server and share: ~s" s)))]
              [(and (fx>= n 2) (char=? (string-ref s 1) #\:))
               (let ([drive ($drive->string who (string-ref s 0))])
                 (if (and (fx> n 2) ($sep? 'windows (string-ref s 2)))
                     ($make-path-object 'windows 'drive-absolute drive
                            ($string-split-path who 'windows s 3)
                            ($trailing-sep? 'windows s))
                     ($make-path-object 'windows 'drive-relative drive
                            ($string-split-path who 'windows s 2)
                            ($trailing-sep? 'windows s))))]
              [($sep? 'windows (string-ref s 0))
               ($make-path-object 'windows 'rooted #f
                      ($string-split-path who 'windows s 1)
                      ($trailing-sep? 'windows s))]
              [else
               ($make-path-object 'windows 'relative #f
                      ($string-split-path who 'windows s 0)
                      ($trailing-sep? 'windows s))]))))

  #|proc:path-parse
  Parse string `str` as a lexical path using `flavor`, either `'unix` or `'windows`.
  Return a path object without checking whether the path exists.
  |#
  (define path-parse
    (lambda (flavor str)
      (pcheck ([string? str])
              ($check-flavor 'path-parse flavor)
              (when ($string-has-nul? str)
                ($err 'path-parse "path contains NUL: ~s" str))
              (if (eq? flavor 'unix)
                  ($parse-unix 'path-parse str)
                  ($parse-windows 'path-parse str)))))

  (define $join-components
    (lambda (components sep)
      (if (null? components)
          ""
          (let ([op (open-output-string)])
            (dynamic-wind
              void
              (lambda ()
                (let loop ([components components] [first? #t])
                  (unless (null? components)
                    (unless first? (write-char sep op))
                    (put-string op (car components))
                    (loop (cdr components) #f)))
                (get-output-string op))
              (lambda () (close-output-port op)))))))

  (define $render-unix
    (lambda (p sep)
      (let* ([components ($path-components p)]
             [body ($join-components components sep)]
             [rooted? (eq? ($path-root-kind p) 'absolute)]
             [base (cond [(and rooted? (null? components)) (string sep)]
                         [rooted? (string-append (string sep) body)]
                         [else body])])
        (if (and ($path-trailing? p) (not (string=? base "")))
            (string-append base (string sep))
            base))))

  (define $render-windows
    (lambda (p sep)
      (let* ([components ($path-components p)]
             [body ($join-components components sep)]
             [root-kind ($path-root-kind p)]
             [root ($path-root p)]
             [base
              (case root-kind
                [(relative)
                 ;; Keep a colon in the first component from becoming a drive prefix.
                 (if (and (fx>= (string-length body) 2)
                          (char=? (string-ref body 1) #\:))
                     (string-append "." (string sep) body)
                     body)]
                [(rooted)
                 (if (null? components) (string sep) (string-append (string sep) body))]
                [(drive-relative)
                 (string-append root ":" body)]
                [(drive-absolute)
                 (if (null? components)
                     (string-append root ":" (string sep))
                     (string-append root ":" (string sep) body))]
                [(unc)
                 (let ([prefix (string-append (string sep) (string sep)
                                              (car root) (string sep) (cadr root))])
                   (if (null? components)
                       (string-append prefix (string sep))
                       (string-append prefix (string sep) body)))]
                [(special)
                 (if (null? components)
                     (if (or (and (fx= (string-length root) 6)
                                  (char=? (string-ref root 5) #\:))
                             ($prefix-special-unc? root)
                             ($special-volume-root? root))
                         (string-append root (string sep))
                         (string-copy root))
                     (string-append root (string sep) body))]
                [else ($err 'path-render "invalid root kind: ~s" root-kind)])])
        (if (and ($path-trailing? p) (not (null? components)))
            (string-append base (string sep))
            base))))

  #|proc:path-render
  The `path-render` procedure renders a path object as a string.
  With one argument it uses the flavor's default separator; with two arguments
  it uses the given separator character. Windows relative paths whose first component
  looks like a drive prefix receive a leading dot component to preserve their root kind.
  |#
  (define path-render
    (case-lambda
      [(p)
       (pcheck ([path? p])
               (path-render p (if (eq? ($path-flavor p) 'unix) #\/ #\\)))]
      [(p separator)
       (pcheck ([path? p] [char? separator])
               (if (eq? ($path-flavor p) 'unix)
                   ($render-unix p separator)
                   ($render-windows p separator)))]))

  #|proc:path-flavor
  The `path-flavor` procedure returns the flavor of `p`.
  |#
  (define path-flavor
    (lambda (p)
      (pcheck ([path? p])
              ($path-flavor p))))

  #|proc:path-components
  Return a fresh list of fresh strings containing path `p`'s components.
  |#
  (define path-components
    (lambda (p)
      (pcheck ([path? p])
              ($copy-components ($path-components p)))))

  #|proc:path-root-info
  Return a fresh list describing path `p`'s root. It starts with the flavor and root kind,
  then includes a drive, special root, or UNC server and share when present.
  |#
  (define path-root-info
    (lambda (p)
      (pcheck ([path? p])
              (let ([flavor ($path-flavor p)]
                    [root-kind ($path-root-kind p)]
                    [root ($path-root p)])
                (case root-kind
                  [(relative absolute rooted) (list flavor root-kind #f)]
                  [(drive-relative drive-absolute special)
                   (list flavor root-kind (string-copy root))]
                  [(unc) (list flavor root-kind (string-copy (car root))
                               (string-copy (cadr root)))]
                  [else ($err 'path-root-info "invalid root kind: ~s" root-kind)])))))

  #|proc:path
  Create a relative path using flavor `flavor` (`'unix` or `'windows`) and component
  strings `components`. Return the new path object.
  |#
  (define path
    (lambda (flavor . components)
      (pcheck ([path-flavor? flavor] [$component-list? components])
              ($check-components 'path flavor components)
              ($make-path-object flavor 'relative #f ($copy-components components) #f))))

  #|proc:path-rooted
  The `path-rooted` procedure creates an absolute Unix path or a Windows path
  rooted relative to the current drive.
  |#
  (define path-rooted
    (lambda (flavor . components)
      (pcheck ([path-flavor? flavor] [$component-list? components])
              ($check-components 'path-rooted flavor components)
              ($make-path-object flavor (if (eq? flavor 'unix) 'absolute 'rooted)
                                 #f ($copy-components components) #f))))

  #|proc:path-drive
  Create a Windows drive-relative path using drive letter `drive` and component strings
  `components`. Return the new path object.
  |#
  (define path-drive
    (lambda (drive . components)
      (pcheck ([(lambda (value) (or (char? value) (string? value))) drive]
               [$component-list? components])
              (let ([drive ($drive->string 'path-drive drive)])
                ($check-components 'path-drive 'windows components)
                ($make-path-object 'windows 'drive-relative drive
                                   ($copy-components components) #f)))))

  #|proc:path-drive-absolute
  Create a Windows drive-absolute path using drive letter `drive` and component strings
  `components`. Return the new path object.
  |#
  (define path-drive-absolute
    (lambda (drive . components)
      (pcheck ([(lambda (value) (or (char? value) (string? value))) drive]
               [$component-list? components])
              (let ([drive ($drive->string 'path-drive-absolute drive)])
                ($check-components 'path-drive-absolute 'windows components)
                ($make-path-object 'windows 'drive-absolute drive
                                   ($copy-components components) #f)))))

  #|proc:path-unc
  Create a Windows UNC path using string names `server` and `share`, and optional
  component strings `components`. Return the new path object. Reserved server names
  `.` and `?` are rejected because they select Windows device namespaces.
  |#
  (define path-unc
    (lambda (server share . components)
      (pcheck ([string? server share] [$component-list? components])
              ($check-component 'path-unc 'windows server)
              ($check-component 'path-unc 'windows share)
              (when (member server '("." "?"))
                ($err 'path-unc "reserved UNC server name: ~s" server))
              ($check-components 'path-unc 'windows components)
              ($make-path-object 'windows 'unc
                                 (list (string-copy server) (string-copy share))
                                 ($copy-components components) #f))))

  (define $same-flavor
    (lambda (who a b)
      (unless (eq? ($path-flavor a) ($path-flavor b))
        ($err who "incompatible path flavors: ~s and ~s" ($path-flavor a) ($path-flavor b)))))

  #|proc:path-absolute-path?
  The `path-absolute-path?` procedure returns whether `p` has an absolute root.
  |#
  (define path-absolute-path?
    (lambda (p)
      (pcheck ([path? p])
              (case ($path-root-kind p)
                [(absolute drive-absolute unc special) #t]
                [else #f]))))

  #|proc:path-rooted-path?
  The `path-rooted-path?` procedure returns whether `p` has any root marker.
  |#
  (define path-rooted-path?
    (lambda (p)
      (pcheck ([path? p])
              (not (eq? ($path-root-kind p) 'relative)))))

  #|proc:path-relative-path?
  The `path-relative-path?` procedure returns whether `p` is relative.
  |#
  (define path-relative-path?
    (lambda (p)
      (pcheck ([path? p])
              (eq? ($path-root-kind p) 'relative))))

  (define $path-append-components
    (lambda (p components trailing?)
      ($make-path-object ($path-flavor p)
             ($path-root-kind p)
             ($path-root p)
             (append ($path-components p) components)
             trailing?)))

  (define $windows-base-drive
    (lambda (base)
      (case ($path-root-kind base)
        [(drive-absolute) (string-ref ($path-root base) 0)]
        [(special)
         (let ([root ($path-root base)])
           (and (fx= (string-length root) 6)
                (char=? (string-ref root 5) #\:)
                ($letter? (string-ref root 4))
                (string-ref root 4)))]
        [else #f])))

  (define $path-rebase
    (lambda (who base part)
      (let ([flavor ($path-flavor base)]
            [kind ($path-root-kind part)])
        (cond [(path-absolute-path? part) part]
              [(and (eq? flavor 'windows) (eq? kind 'rooted))
               (if ($windows-base-drive base)
                   ($make-path-object flavor ($path-root-kind base) ($path-root base)
                                     ($path-components part) ($path-trailing? part))
                   ($err who "rooted Windows path requires a drive-absolute base"))]
              [(and (eq? flavor 'windows) (eq? kind 'drive-relative))
               (let ([drive ($windows-base-drive base)])
                 (if (and drive (char-ci=? drive (string-ref ($path-root part) 0)))
                     ($path-append-components base ($path-components part) ($path-trailing? part))
                     ($err who "drive-relative path requires the matching drive-absolute base")))]
              [else ($path-append-components base ($path-components part)
                                             ($path-trailing? part))]))))

  #|proc:path-join
  The `path-join` procedure appends path parts. Absolute path parts of the same
  flavor replace the accumulated base. A Windows root-relative part uses the current
  drive from a drive-absolute base; unresolved or different drive-relative parts error.
  Extended drive bases retain their device namespace prefix.
  |#
  (define path-join
    (lambda (base . parts)
      (pcheck ([path? base])
              (let loop ([acc base] [parts parts])
                (if (null? parts)
                    acc
                    (let ([part (car parts)])
                      (cond [(path? part)
                             ($same-flavor 'path-join acc part)
                             (loop ($path-rebase 'path-join acc part) (cdr parts))]
                            [(string? part)
                             (let ([parsed (path-parse ($path-flavor acc) part)])
                               (if (path-absolute-path? parsed)
                                   (loop parsed (cdr parts))
                                   (if (and (eq? ($path-flavor acc) 'windows)
                                            (path-rooted-path? parsed))
                                       (loop ($path-rebase 'path-join acc parsed) (cdr parts))
                                       (begin
                                     ($check-component 'path-join ($path-flavor acc) part)
                                     (loop (path-add-component acc part) (cdr parts))))))]
                            [else ($err 'path-join "invalid path part: ~s" part)])))))))

  #|proc:path-append
  The `path-append` procedure strictly appends path objects with compatible
  flavors. Later rooted paths are rejected.
  |#
  (define path-append
    (case-lambda
      [() ($err 'path-append "expected at least one path")]
      [(p)
       (pcheck ([path? p]) p)]
      [(p . paths)
       (pcheck ([path? p])
               (let loop ([acc p] [paths paths])
                 (if (null? paths)
                     acc
                     (let ([part (car paths)])
                       (pcheck ([path? part])
                               ($same-flavor 'path-append acc part)
                               (when (path-rooted-path? part)
                                 ($err 'path-append "cannot append rooted path: ~a" (path-render part)))
                               (loop ($path-append-components acc ($path-components part) ($path-trailing? part))
                                     (cdr paths)))))))]))

  #|proc:path-add-component
  The `path-add-component` procedure returns a new path with `component` added
  at the end.
  |#
  (define path-add-component
    (lambda (p component)
      (pcheck ([path? p])
              ($check-component 'path-add-component ($path-flavor p) component)
              ($make-path-object ($path-flavor p)
                     ($path-root-kind p)
                     ($path-root p)
                     (append ($path-components p) (list (string-copy component)))
                     #f))))

  #|proc:path-add-components
  The `path-add-components` procedure returns a new path with each component in
  `components` added at the end.
  |#
  (define path-add-components
    (lambda (p components)
      (pcheck ([path? p] [list? components])
              ($check-components 'path-add-components ($path-flavor p) components)
              ($path-append-components p ($copy-components components) #f))))

  #|proc:path-replace-basename
  The `path-replace-basename` procedure replaces the final component.
  |#
  (define path-replace-basename
    (lambda (p basename)
      (pcheck ([path? p])
              ($check-component 'path-replace-basename ($path-flavor p) basename)
              (let ([components ($path-components p)])
                (if (null? components)
                    ($err 'path-replace-basename "path has no basename")
                    ($make-path-object ($path-flavor p)
                           ($path-root-kind p)
                           ($path-root p)
                           (append (reverse (cdr (reverse components)))
                                   (list (string-copy basename)))
                           #f))))))

  #|proc:path-drop-basename
  The `path-drop-basename` procedure returns the lexical parent path.
  |#
  (define path-drop-basename
    (lambda (p)
      (pcheck ([path? p])
              (let ([components ($path-components p)])
                (if (null? components)
                    p
                    ($make-path-object ($path-flavor p)
                           ($path-root-kind p)
                           ($path-root p)
                           (reverse (cdr (reverse components)))
                           #f))))))

  (define $normalize-components
    (lambda (root-kind components)
      (let ([drive-relative? (eq? root-kind 'drive-relative)]
            [rooted? (not (eq? root-kind 'relative))])
        (let loop ([components components] [acc '()])
          (cond [(null? components) (reverse acc)]
                [(string=? (car components) ".")
                 (loop (cdr components) acc)]
                [(and (string=? (car components) "..") drive-relative?)
                 (loop (cdr components) (cons (car components) acc))]
                [(string=? (car components) "..")
                 (cond [(and (pair? acc) (not (string=? (car acc) "..")))
                        (loop (cdr components) (cdr acc))]
                       [rooted? (loop (cdr components) acc)]
                       [else (loop (cdr components) (cons (car components) acc))])]
                [else (loop (cdr components) (cons (car components) acc))])))))

  #|proc:path-normalize
  The `path-normalize` procedure performs lexical normalization by removing `.`
  components and resolving safe `..` components.
  |#
  (define path-normalize
    (lambda (p)
      (pcheck ([path? p])
              (let ([components ($normalize-components ($path-root-kind p) ($path-components p))])
                ($make-path-object ($path-flavor p)
                       ($path-root-kind p)
                       ($path-root p)
                       components
                       (and ($path-trailing? p) (not (null? components))))))))

  #|proc:path-with-trailing-directory
  The `path-with-trailing-directory` procedure sets or clears the trailing
  directory marker.
  |#
  (define path-with-trailing-directory
    (lambda (p trailing?)
      (pcheck ([path? p] [boolean? trailing?])
              ($make-path-object ($path-flavor p)
                     ($path-root-kind p)
                     ($path-root p)
                     ($path-components p)
                     trailing?))))

  #|proc:path-basename
  Return a fresh string containing the final component of path `p`, or `#f` for roots
  and empty paths.
  |#
  (define path-basename
    (lambda (p)
      (pcheck ([path? p])
              (let ([components ($path-components p)])
                (and (pair? components)
                     (string-copy (car (reverse components))))))))

  #|proc:path-dirname
  The `path-dirname` procedure returns the lexical parent path.
  |#
  (define path-dirname
    (lambda (p)
      (pcheck ([path? p])
              (path-drop-basename p))))

  (define $last-dot-index
    (lambda (s)
      (let ([n (string-length s)])
        (let loop ([i (fx- n 1)])
          (cond [(fx< i 0) #f]
                [(char=? (string-ref s i) #\.) i]
                [else (loop (fx- i 1))])))))

  #|proc:path-extname
  The `path-extname` procedure returns the extension of the basename without
  the leading `.`, or `#f` when there is no extension.
  |#
  (define path-extname
    (lambda (p)
      (pcheck ([path? p])
              (let ([base (path-basename p)])
                (and base
                     (let ([i ($last-dot-index base)]
                           [n (string-length base)])
                       (and i
                            (fx> i 0)
                            (fx< i (fx- n 1))
                            (substring base (fx+ i 1) n))))))))

  #|proc:path-stem
  The `path-stem` procedure returns the basename without its final extension,
  or `#f` when the path has no basename.
  |#
  (define path-stem
    (lambda (p)
      (pcheck ([path? p])
              (let ([base (path-basename p)])
                (and base
                     (let ([i ($last-dot-index base)])
                       (if (and i
                                (fx> i 0)
                                (fx< i (fx- (string-length base) 1)))
                           (substring base 0 i)
                           base)))))))

  (define $windows-name=?
    (lambda (left right)
      ;; Chez char-ci=? uses Unicode simple folding, shared with Windows glob.
      (let ([length (string-length left)])
        (and (fx= length (string-length right))
             (let loop ([index 0])
               (or (fx= index length)
                   (and (char-ci=? (string-ref left index) (string-ref right index))
                        (loop (fx1+ index)))))))))

  (define $root=?
    (lambda (a b)
      (and (eq? ($path-flavor a) ($path-flavor b))
           (let ([ak ($path-root-kind a)] [bk ($path-root-kind b)])
             (cond [(and (eq? ($path-flavor a) 'unix)
                         (eq? ak bk))
                    #t]
                   [(and (eq? ak 'relative) (eq? bk 'relative)) #t]
                   [(and (eq? ak 'rooted) (eq? bk 'rooted)) #t]
                   [(and (eq? ak 'drive-relative) (eq? bk 'drive-relative))
                    ($windows-name=? ($path-root a) ($path-root b))]
                   [(and (eq? ak 'drive-absolute) (eq? bk 'drive-absolute))
                    ($windows-name=? ($path-root a) ($path-root b))]
                   [(and (eq? ak 'unc) (eq? bk 'unc))
                    (and ($windows-name=? (car ($path-root a)) (car ($path-root b)))
                         ($windows-name=? (cadr ($path-root a)) (cadr ($path-root b))))]
                   [(and (eq? ak 'special) (eq? bk 'special))
                    ($windows-name=? ($path-root a) ($path-root b))]
                   [else #f])))))

  #|proc:path-same-root?
  Return whether path objects `path1` and `path2` have compatible roots for relativization.
  Windows roots use Unicode simple case folding, without multi-character string folding.
  |#
  (define path-same-root?
    (lambda (path1 path2)
      (pcheck ([path? path1 path2])
              ($root=? path1 path2))))

  (define $make-relative-like
    (lambda (flavor components trailing?)
      ($make-path-object flavor 'relative #f components trailing?)))

  #|proc:path-absolute
  The `path-absolute` procedure makes `p` absolute by joining it to absolute
  `base`, then normalizing lexically.
  Windows root-relative and matching drive-relative paths use the drive and namespace of `base`.
  |#
  (define path-absolute
    (lambda (base p)
      (pcheck ([path? base p])
              ($same-flavor 'path-absolute base p)
              (unless (path-absolute-path? base)
                ($err 'path-absolute "base path is not absolute: ~a" (path-render base)))
              (path-normalize ($path-rebase 'path-absolute base p)))))

  (define $drop-common-prefix
    (lambda (flavor a b)
      (if (and (pair? a) (pair? b)
               ((if (eq? flavor 'windows) $windows-name=? string=?) (car a) (car b)))
          ($drop-common-prefix flavor (cdr a) (cdr b))
          (values a b))))

  (define $make-ups
    (lambda (components)
      (map (lambda (x) "..") components)))

  (define $relative-components
    (lambda (p)
      ;; Drive-relative normalization preserves spelling, but relativization needs
      ;; cancellable internal parents removed before counting directory levels.
      ($normalize-components (if (eq? ($path-root-kind p) 'drive-relative)
                                 'relative ($path-root-kind p))
                             ($path-components p))))

  #|proc:path-relative-to
  Return a lexical relative path from `base` to `p`. Windows components compare
  character by character case-insensitively, as in glob. Incompatible roots and
  unresolved divergent parent paths error.
  |#
  (define path-relative-to
    (lambda (base p)
      (pcheck ([path? base p])
              ($same-flavor 'path-relative-to base p)
              (unless (path-same-root? base p)
                ($err 'path-relative-to "incompatible path roots"))
              (call-with-values
                  (lambda () ($drop-common-prefix ($path-flavor p)
                                                   ($relative-components base)
                                                   ($relative-components p)))
                  (lambda (base-rest p-rest)
                    (when (and (pair? base-rest) (string=? (car base-rest) ".."))
                      ($err 'path-relative-to "cannot relativize from an unresolved parent path"))
                    ($make-relative-like ($path-flavor p)
                                         (append ($make-ups base-rest) p-rest)
                                         ($path-trailing? p)))))))

  #|proc:path-starts-with-user-home?
  The `path-starts-with-user-home?` procedure returns whether the first
  component of `p` is exactly `~`.
  |#
  (define path-starts-with-user-home?
    (lambda (p)
      (pcheck ([path? p])
              (let ([components ($path-components p)])
                (and (pair? components)
                     (string=? (car components) "~"))))))

  (define $home-from-environment
    (lambda (flavor)
      (if (eq? flavor 'unix)
          (getenv "HOME")
          (or (getenv "USERPROFILE")
              (let ([drive (getenv "HOMEDRIVE")]
                    [path (getenv "HOMEPATH")])
                (and drive path (string-append drive path)))))))

  (define $coerce-home
    (lambda (who flavor home)
      (cond [(path? home)
             (unless (eq? flavor ($path-flavor home))
               ($err who "home path flavor differs from input path flavor"))
             home]
            [(string? home) (path-parse flavor home)]
            [else ($err who "invalid home path: ~s" home)])))

  #|proc:path-expand-user
  The `path-expand-user` procedure expands a leading `~` component using either
  the host current user's home directory or explicit string/path object `home`.
  Return a path object; an input without a leading `~` is returned unchanged.
  |#
  (define path-expand-user
    (case-lambda
      [(p)
       (pcheck ([path? p])
               (if (not (path-starts-with-user-home? p))
                   p
                   (let ([home ($home-from-environment ($path-flavor p))])
                     (if home
                         (path-expand-user p home)
                         ($err 'path-expand-user "no usable home directory in environment")))))]
      [(p home)
       (pcheck ([path? p] [(lambda (value) (or (string? value) (path? value))) home])
               (if (not (path-starts-with-user-home? p))
                   p
                   (let ([home ($coerce-home 'path-expand-user ($path-flavor p) home)])
                     (unless (path-absolute-path? home)
                       ($err 'path-expand-user "home path is not absolute: ~a" (path-render home)))
                     (path-normalize
                      ($make-path-object ($path-flavor home)
                             ($path-root-kind home)
                             ($path-root home)
                             (append ($path-components home) (cdr ($path-components p)))
                             ($path-trailing? p))))))]))
  )
