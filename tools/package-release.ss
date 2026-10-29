(import (cspp))

;; Native Linux packaging. All process arguments are quoted individually; package paths
;; and filenames are data, including when they contain spaces or shell metacharacters.
(define pkg-join
  (lambda (strings separator)
    (if (null? strings) ""
        (apply string-append
               (car strings) (map (lambda (text) (string-append separator text)) (cdr strings))))))

(define pkg-quote
  (lambda (text)
    (string-append "'" (pkg-join (string-split text #\') "'\"'\"'") "'")))

(define pkg-run
  (lambda arguments
    (capture-shell-command (pkg-join (map pkg-quote arguments) " ")
                           :stdout capture :stderr capture)))

(define pkg-command
  (lambda arguments
    (let ([result (apply pkg-run arguments)])
      (unless (process-exit-success? (process-result-status result))
        (errorf 'package-release "~a failed: ~a" (car arguments)
                (process-result-stderr result)))
      (process-result-stdout result))))

(define pkg-trim
  (lambda (text)
    (let ([size (string-length text)])
      (let left ([start 0])
        (if (and (fx< start size) (char-whitespace? (string-ref text start)))
            (left (fx1+ start))
            (let right ([end size])
              (if (and (fx> end start) (char-whitespace? (string-ref text (fx1- end))))
                  (right (fx1- end))
                  (substring text start end))))))))

(define pkg-prefix?
  (lambda (prefix text)
    (and (fx<= (string-length prefix) (string-length text))
         (string=? prefix (substring text 0 (string-length prefix))))))

(define pkg-lines
  (lambda (text) (filter (lambda (line) (not (string=? line ""))) (string-split text #\newline))))

(define pkg-realpath
  (lambda (path) (pkg-trim (pkg-command "readlink" "-f" "--" path))))

(define pkg-digest
  (lambda (path)
    (let* ([output (pkg-command "sha256sum" "--" path)]
           ;; GNU sha256sum prefixes a backslash when escaping a filename.
           [start (if (char=? (string-ref output 0) #\\) 1 0)])
      (substring output start (fx+ start 64)))))

(define pkg-object
  (lambda entries
    (let ([table (make-hashtable string-hash string=?)])
      (let loop ([entries entries])
        (unless (null? entries)
          (hashtable-set! table (car entries) (cadr entries))
          (loop (cddr entries))))
      table)))

;;;;===----------------------------------------------------------------------===
;;;; Metadata serialization and host requirements
;;;;===----------------------------------------------------------------------===

;; The parser library reads JSON5 but has no JSON writer. This writer covers the
;; metadata's strings, booleans, vectors, and string-keyed hashtables as strict JSON.
(define pkg-json
  (lambda (value)
    (call-with-string-output-port
      (lambda (port)
        (define emit-string
          (lambda (text)
            (write-char #\" port)
            (string-for-each
             (lambda (character)
               (case character
                 [(#\") (display "\\\"" port)]
                 [(#\\) (display "\\\\" port)]
                 [else
                  (if (fx< (char->integer character) 32)
                      (let ([hex (number->string (char->integer character) 16)])
                        (display "\\u" port)
                        (display (make-string (fx- 4 (string-length hex)) #\0) port)
                        (display hex port))
                      (write-char character port))]))
             text)
            (write-char #\" port)))
        (define emit-separated
          (lambda (items emit)
            (unless (null? items)
              (emit (car items))
              (for-each (lambda (item) (display "," port) (emit item)) (cdr items)))))
        (define emit
          (lambda (value)
            (cond
             [(string? value) (emit-string value)]
             [(boolean? value) (display (if value "true" "false") port)]
             [(vector? value)
              (display "[" port) (emit-separated (vector->list value) emit) (display "]" port)]
             [(hashtable? value)
              (display "{" port)
              (emit-separated
               (list-sort string<? (vector->list (hashtable-keys value)))
               (lambda (key)
                 (emit-string key) (display ":" port) (emit (hashtable-ref value key #f))))
              (display "}" port)]
             [else (error 'package-release "unsupported metadata value" value)])))
        (emit value)))))

(define pkg-build-host-info
  (lambda ()
    (let ([glibc (pkg-trim (pkg-command "getconf" "GNU_LIBC_VERSION"))]
          [baseline (pkg-command "sh" "-c" ". /etc/os-release; printf '%s' \"$PRETTY_NAME\"")])
      (unless (regex-matches? (string->regex "glibc [0-9]+[.][0-9]+") glibc)
        (error 'package-release "packaging requires glibc" glibc))
      (pkg-object "baseline" baseline
                  "host_requirements"
                  (vector "Linux" (string-append "glibc >= " (substring glibc 6 (string-length glibc))))))))

;;;;===----------------------------------------------------------------------===
;;;; Recursive native dependency collection
;;;;===----------------------------------------------------------------------===

(define pkg-host-library-pattern
  (string->regex
   "(lib(c|m|dl|pthread|rt|resolv|util|anl|BrokenLocale)[.]so[.][0-9]+|libnss_[^.]+[.]so[.]2|ld-linux[^/]*[.]so[.][0-9]+|ld64[.]so[.][0-9]+)"))

(define pkg-needed-pattern (string->regex "\\(NEEDED\\).*\\[([^]]+)\\]"))
(define pkg-resolved-pattern
  (string->regex "^[[:space:]]*([^[:space:]]+)[[:space:]]+=>[[:space:]]+(/.*)[[:space:]]+\\(0x[0-9a-f]+\\)"))
(define pkg-loader-pattern
  (string->regex "^[[:space:]]*(/[^[:space:]]+)[[:space:]]+\\(0x[0-9a-f]+\\)"))
(define pkg-missing-pattern
  (string->regex "^[[:space:]]*([^[:space:]]+)[[:space:]]+=>[[:space:]]+not found"))

(define pkg-elf-files
  (lambda (prefix)
    (let ([files (make-hashtable string-hash string=?)]
          [root (pkg-realpath prefix)])
      (file-for-each
       (lambda (path)
         (when (and (file-symbolic-link? path)
                    (let ([resolved (pkg-realpath path)])
                      (not (or (string=? resolved root)
                               (pkg-prefix? (string-append root "/") resolved)))))
           (error 'package-release "installed symlink points outside package" path))
         (when (file-regular? path)
           (call-with-port (open-file-input-port path)
             (lambda (port)
               (when (equal? (get-bytevector-n port 4) #vu8(127 69 76 70))
                 (hashtable-set! files (pkg-realpath path) #t))))))
       prefix)
      (list-sort string<? (vector->list (hashtable-keys files))))))

(define pkg-needed
  (lambda (path)
    (let ([builder (make-list-builder)])
      (for-each
       (lambda (line)
         (let ([match (regex-search pkg-needed-pattern line)])
           (when match (builder (regex-match-substring match 1)))))
       (pkg-lines (pkg-command "readelf" "-d" "--" path)))
      (builder))))

(define pkg-collect-dependencies
  (lambda (prefix extra-libraries)
    (let* ([destination (string-append prefix "/lib/deps")]
           [queue (pkg-elf-files prefix)]
           [scanned (make-hashtable string-hash string=?)]
           [records (make-hashtable string-hash string=?)])
      (define record-library!
        (lambda (name source bundled?)
          (let* ([source (pkg-realpath source)]
                 [checksum (pkg-digest source)]
                 [previous (hashtable-ref records name #f)]
                 [target (string-append destination "/" name)])
            (when (and previous (not (string=? checksum (hashtable-ref previous "sha256" ""))))
              (error 'package-release "conflicting dependency SONAME" name))
            (hashtable-set! records name
                            (pkg-object "name" name "source" source "sha256" checksum
                                        "bundled" bundled?))
            (when bundled?
              (when (and (file-exists? target) (not (string=? checksum (pkg-digest target))))
                (error 'package-release "conflicting packaged dependency" name))
              (pkg-command "cp" "-p" "--" source target)
              (set! queue (cons source queue))))))
      (mkdirs destination)
      ;; Explicit roots cover dlopen libraries, including ChezScheme's X11 clipboard support.
      (for-each (lambda (source) (record-library! (path-last source) source #t)) extra-libraries)
      (let loop ()
        (unless (null? queue)
          (let ([path (car queue)])
            (set! queue (cdr queue))
            (unless (hashtable-ref scanned path #f)
              (hashtable-set! scanned path #t)
              (let ([requirements (pkg-needed path)]
                    [resolved (make-hashtable string-hash string=?)])
                (unless (null? requirements)
                  (for-each
                   (lambda (line)
                     (let ([missing (regex-search pkg-missing-pattern line)]
                           [library (regex-search pkg-resolved-pattern line)]
                           [loader (regex-search pkg-loader-pattern line)])
                       (when missing
                         (error 'package-release "unresolved dependency"
                                (regex-match-substring missing 1) path))
                       (cond
                        [library
                         (hashtable-set! resolved (regex-match-substring library 1)
                                         (pkg-trim (regex-match-substring library 2)))]
                        [loader
                         (let ([source (regex-match-substring loader 1)])
                           (hashtable-set! resolved (path-last source) source))])))
                   (pkg-lines (pkg-command "ldd" path)))
                  (for-each
                   (lambda (name)
                     (when (string-search name "/")
                       (error 'package-release "non-relocatable dependency name" name))
                     (let ([source (hashtable-ref resolved name #f)])
                       (unless source (error 'package-release "unresolved dependency" name path))
                       (record-library! name source (not (regex-matches? pkg-host-library-pattern name)))))
                   requirements)))))
          (loop)))
      (map (lambda (name) (hashtable-ref records name #f))
           (list-sort string<? (vector->list (hashtable-keys records)))))))

(define pkg-patch-runpaths
  (lambda (prefix)
    (for-each
     (lambda (path)
       (unless (null? (pkg-needed path))
         (let* ([relative (pkg-trim
                          (pkg-command "realpath" (string-append "--relative-to=" (path-parent path))
                                       "--" (string-append prefix "/lib/deps")))]
                [runpath (if (string=? relative ".") "$ORIGIN" (string-append "$ORIGIN/" relative))]
                [mode (file-mode path)])
           (dynamic-wind
             (lambda () (file-chmod path (fxior mode #o200)))
             (lambda () (pkg-command "patchelf" "--set-rpath" runpath path))
             (lambda () (file-chmod path mode))))))
     (pkg-elf-files prefix))))

;;;;===----------------------------------------------------------------------===
;;;; Installed launcher, licenses, archive, and command-line entry point
;;;;===----------------------------------------------------------------------===

(define pkg-relocate-launcher
  (lambda (prefix)
    (let* ([launcher (string-append prefix "/bin/cspp")]
           [lines (string-split (read-string launcher) #\newline)]
           [boots (file-find-all (lambda (path) (string=? (path-last path) "scheme.boot"))
                                 (string-append prefix "/lib"))])
      (unless (and (>= (length lines) 3) (pkg-prefix? "#!" (car lines))
                   (pkg-prefix? "(putenv \"LIBCSPP\" " (cadr lines))
                   (pkg-prefix? "(load " (caddr lines)))
        (error 'package-release "unrecognized installed cspp launcher"))
      (unless (= (length boots) 1)
        (error 'package-release "expected exactly one installed ChezScheme boot directory"))
      (write-string! (string-append prefix "/lib/cspp.ss")
                     (string-append
                      "(putenv \"LIBCSPP\" (getenv \"CSPP_PACKAGE_NATIVE\"))\n"
                      "(load (getenv \"CSPP_PACKAGE_LIBRARY\"))\n"
                      (pkg-join (cdddr lines) "\n")))
      (write-string! launcher
                     (string-append
                      "#!/bin/sh\nset -eu\n"
                      "package_root=$(dirname \"$(dirname \"$(readlink -f -- \"$0\")\")\")\n"
                      "export CSPP_PACKAGE_NATIVE=\"$package_root/lib/libcspp.so\"\n"
                      "export CSPP_PACKAGE_LIBRARY=\"$package_root/lib/cspp.lib\"\n"
                      "export LD_LIBRARY_PATH=\"$package_root/lib/deps${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"\n"
                      "export SCHEMEHEAPDIR=\"$package_root/"
                      (pkg-trim (pkg-command "realpath" (string-append "--relative-to=" prefix)
                                             "--" (path-parent (car boots)))) "\"\n"
                      "exec \"$package_root/bin/scheme\" --script \"$package_root/lib/cspp.ss\" \"$@\"\n"))
      (file-chmod launcher #o755))))

(define pkg-command-exists?
  (lambda (name)
    (process-exit-success?
     (process-result-status (pkg-run "sh" "-c" "command -v \"$1\" >/dev/null" "sh" name)))))

(define pkg-copy-licenses
  (lambda (prefix repository dependencies)
    (let ([destination (string-append prefix "/share/licenses")])
      (mkdirs destination)
      (for-each
       (lambda (entry)
         (let ([source (string-append repository "/" (cdr entry))])
           (when (file-regular? source)
             (pkg-command "cp" "-p" "--" source (string-append destination "/" (car entry))))))
       '(("cspp" . "LICENSE") ("ChezScheme" . "vendor/ChezScheme/NOTICE")
         ("ChezScheme-license" . "vendor/ChezScheme/LICENSE")))
      (when (pkg-command-exists? "dpkg-query")
        (for-each
         (lambda (item)
           (when (hashtable-ref item "bundled" #f)
             (let* ([source (hashtable-ref item "source" "")]
                    [candidates (if (pkg-prefix? "/usr/lib/" source)
                                    (list source (substring source 4 (string-length source)))
                                    (list source))])
               (let loop ([candidates candidates])
                 (unless (null? candidates)
                   (let ([result (pkg-run "dpkg-query" "-S" (car candidates))])
                     (if (process-exit-success? (process-result-status result))
                         (let* ([output (process-result-stdout result)]
                                [package (car (string-split (car (string-split output ": ")) ", "))]
                                [copyright (string-append "/usr/share/doc/"
                                                          (car (string-split package #\:)) "/copyright")])
                           (when (file-regular? copyright)
                             (pkg-command "cp" "-p" "--" copyright
                                          (string-append destination "/" package "-copyright"))))
                         (loop (cdr candidates)))))))))
         dependencies))
      (when (file-directory? "/usr/local/share/doc/blake3")
        (pkg-command "cp" "-a" "/usr/local/share/doc/blake3" (string-append destination "/blake3"))))))

(define pkg-create-archive
  (lambda (prefix archive name)
    ;; Rename member and hardlink paths while preserving relative symlink targets.
    (pkg-command "tar" "-czf" archive (string-append "--transform=flags=rh;s,^\\.," name ",")
                 "-C" prefix ".")))

(define pkg-main
  (lambda (arguments)
    (if (equal? arguments '("--help"))
        (display "Usage: package-release.ss PREFIX --output DIR --arch x64|arm64|riscv64|loongarch64 --version VERSION --commit SHA\n")
        (let ([options (make-hashtable string-hash string=?)])
          (unless (pair? arguments) (error 'package-release "missing installed prefix"))
          (let loop ([rest (cdr arguments)])
            (unless (null? rest)
              (unless (and (pair? (cdr rest))
                           (member (car rest) '("--output" "--arch" "--version" "--commit"))
                           (not (hashtable-contains? options (car rest))))
                (error 'package-release "invalid or repeated command-line option" (car rest)))
              (hashtable-set! options (car rest) (cadr rest))
              (loop (cddr rest))))
          (for-each (lambda (name)
                      (unless (hashtable-contains? options name)
                        (error 'package-release "missing required option" name)))
                    '("--output" "--arch" "--version" "--commit"))
          (let* ([prefix (pkg-realpath (car arguments))]
                 [repository (path-parent (path-parent (pkg-realpath (car (command-line)))))]
                 [output (hashtable-ref options "--output" "")]
                 [arch (hashtable-ref options "--arch" "")]
                 [version (hashtable-ref options "--version" "")]
                 [commit (hashtable-ref options "--commit" "")])
            (unless (member arch '("x64" "arm64" "riscv64" "loongarch64"))
              (error 'package-release "unsupported architecture" arch))
            (unless (regex-matches? (string->regex "[0-9]+[.][0-9]+[.][0-9]+@[0-9]+-[0-9]+-[0-9]+") version)
              (error 'package-release "invalid CSPP version" version))
            (unless (regex-matches? (string->regex "[0-9a-f]{40}") commit)
              (error 'package-release "commit must be a full Git SHA" commit))
            (let ([x11 (pkg-trim (pkg-command "cc" "-print-file-name=libX11.so"))])
              (unless (and (pkg-prefix? "/" x11) (file-regular? x11))
                (error 'package-release "cannot locate ChezScheme X11 runtime library"))
              (let* ([dependencies (pkg-collect-dependencies prefix (list x11))]
                     [host (pkg-build-host-info)]
                     [metadata
                      (pkg-object "version" version "commit" commit "arch" arch
                                  "baseline" (hashtable-ref host "baseline" "")
                                  "host_requirements" (hashtable-ref host "host_requirements" '#())
                                  "dependencies" (list->vector dependencies)
                                  "build_options" (pkg-trim (read-string (string-append repository "/.cspp-build-options"))))]
                     [name (string-append "cspp-" version "-linux-" arch)])
                (pkg-patch-runpaths prefix)
                (pkg-relocate-launcher prefix)
                (pkg-copy-licenses prefix repository dependencies)
                (write-string! (string-append prefix "/BUILD-INFO.json") (string-append (pkg-json metadata) "\n"))
                (write-string! (string-append prefix "/README.txt")
                               (string-append
                                "Extract this directory anywhere and run bin/cspp.\nRequires "
                                (pkg-join (vector->list (hashtable-ref host "host_requirements" '#())) ", ")
                                ".\nNon-glibc ELF dependencies are bundled recursively in lib/deps.\n"
                                "Build provenance and enabled features are in BUILD-INFO.json.\n"))
                (mkdirs output)
                (let ([archive (string-append (pkg-realpath output) "/" name ".tar.gz")])
                  (pkg-create-archive prefix archive name)
                  (write-string! (string-append archive ".sha256")
                                 (string-append (pkg-digest archive) "  " name ".tar.gz\n"))
                  (display archive) (newline)))))))))

(putenv "LC_ALL" "C")
;; Loading the script from the regression runner defines helpers without invoking its CLI.
(when (string=? (path-last (car (command-line))) "package-release.ss")
  (pkg-main (command-line-arguments)))
