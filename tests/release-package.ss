(import (cspp) (cspp file) (cspp format json5))

(load "tools/package-release.ss")

(define check
  (lambda (value message)
    (unless value (error 'release-package-test message))))

(define expect-package-error
  (lambda (thunk fragment)
    (guard (condition
            [else
             (check (string-search
                     (call-with-string-output-port
                       (lambda (port) (display-condition condition port)))
                     fragment)
                    (format "expected error containing ~s" fragment))])
      (thunk)
      (error 'release-package-test "expected packaging failure"))))

(define compile-fixture
  (lambda (root external name source libraries)
    (let ([path (string-append root "/" name ".c")])
      (write-string! path source)
      (apply pkg-command "cc" "-shared" "-fPIC" path
             (string-append "-Wl,-soname,lib" name ".so")
             (string-append "-L" external) (string-append "-Wl,-rpath," external)
             "-o" (string-append external "/lib" name ".so") libraries))))

(define with-fixture
  (lambda (procedure)
    (let* ([root (pkg-trim (pkg-command "mktemp" "-d" "/tmp/cspp-package-test.'XXXXXXXX"))]
           [prefix (string-append root "/installed")]
           [external (string-append root "/external")])
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (mkdirs (string-append prefix "/bin"))
          (mkdirs (string-append prefix "/lib"))
          (mkdirs external)
          (compile-fixture root external "leaf" "int leaf(void) { return 42; }" '())
          (compile-fixture root external "middle"
                           "extern int leaf(void); int middle(void) { return leaf(); }"
                           '("-lleaf"))
          (write-string! (string-append root "/main.c")
                         "extern int middle(void); int main(void) { return middle() != 42; }")
          (pkg-command "cc" (string-append root "/main.c") (string-append "-L" external)
                       "-lmiddle" (string-append "-Wl,-rpath," external)
                       "-o" (string-append prefix "/bin/probe"))
          (procedure root prefix external))
        (lambda () (file-removetree root))))))

;; Indirect dependencies must survive removal of their original files and package relocation.
(with-fixture
  (lambda (root prefix external)
    (let ([dependencies (pkg-collect-dependencies prefix '())]
          [probe (string-append prefix "/bin/probe")])
      (check (file-regular? (string-append prefix "/lib/deps/libleaf.so")) "missing leaf")
      (check (file-regular? (string-append prefix "/lib/deps/libmiddle.so")) "missing middle")
      (check (not (file-exists? (string-append prefix "/lib/deps/libc.so.6"))) "bundled glibc")
      (check (= 2 (length (filter (lambda (item) (hashtable-ref item "bundled" #f))
                                  dependencies))) "unexpected bundled dependencies")
      ;; Installed executable permissions must be preserved when patching as an ordinary user.
      (file-chmod probe #o555)
      (pkg-patch-runpaths prefix)
      (check (= #o555 (file-mode probe)) "executable mode changed")
      (file-removetree external)
      (let ([moved (string-append root "/moved package")])
        (rename-file prefix moved)
        (pkg-command "env" "-u" "LD_LIBRARY_PATH" (string-append moved "/bin/probe"))))))

;; Error case: an indirect runtime library is missing despite successful linking.
(with-fixture
  (lambda (root prefix external)
    (delete-file (string-append external "/libleaf.so"))
    (expect-package-error (lambda () (pkg-collect-dependencies prefix '())) "libleaf.so")))

;; Libraries loaded with dlopen need explicit roots and recursive dependency collection.
(with-fixture
  (lambda (root prefix external)
    (delete-file (string-append prefix "/bin/probe"))
    (let ([dependencies
           (pkg-collect-dependencies prefix (list (string-append external "/libmiddle.so")))])
      (check (file-regular? (string-append prefix "/lib/deps/libmiddle.so")) "missing dlopen root")
      (check (file-regular? (string-append prefix "/lib/deps/libleaf.so")) "missing dlopen child")
      (check (= 2 (length (filter (lambda (item) (hashtable-ref item "bundled" #f))
                                  dependencies))) "unexpected dlopen dependency count"))))

;; Error case: two objects resolve the same SONAME to different library contents.
(with-fixture
  (lambda (root prefix external)
    (let ([alternative (string-append root "/alternative")])
      (pkg-command "cp" "-a" external alternative)
      (compile-fixture root alternative "leaf" "int leaf(void) { return 7; }" '())
      (write-string! (string-append root "/other.c")
                     "extern int leaf(void); int other(void) { return leaf(); }")
      (pkg-command "cc" "-shared" "-fPIC" (string-append root "/other.c")
                   (string-append "-L" alternative) "-lleaf"
                   (string-append "-Wl,-rpath," alternative)
                   "-o" (string-append prefix "/lib/other.so"))
      (expect-package-error (lambda () (pkg-collect-dependencies prefix '())) "conflict"))))

;; glibc dependencies can name the loader, whose ldd row has no SONAME => prefix.
(with-fixture
  (lambda (root prefix external)
    (write-string! (string-append root "/cpp.cpp")
                   "#include <iostream>\nint main() { std::cout << \"ok\"; }")
    (pkg-command "c++" (string-append root "/cpp.cpp") "-o" (string-append prefix "/bin/cpp"))
    (let ([loaders (filter (lambda (item)
                             (pkg-prefix? "ld-linux" (hashtable-ref item "name" "")))
                           (pkg-collect-dependencies prefix '()))])
      (check (pair? loaders) "missing loader metadata")
      (check (for-all (lambda (item) (not (hashtable-ref item "bundled" #t))) loaders)
             "loader was bundled"))))

;; Error case: an installed symlink would escape the released archive.
(with-fixture
  (lambda (root prefix external)
    (file-symlink (string-append external "/libleaf.so") (string-append prefix "/bin/escape"))
    (expect-package-error (lambda () (pkg-collect-dependencies prefix '())) "outside")))

;; Archive root renaming must preserve relative launcher symlink targets.
(with-fixture
  (lambda (root prefix external)
    (rename-file (string-append prefix "/bin/probe") (string-append prefix "/lib/probe"))
    (file-symlink "../lib/probe" (string-append prefix "/bin/probe"))
    (let ([archive (string-append root "/archive.tar.gz")]
          [destination (string-append root "/extracted")])
      (pkg-create-archive prefix archive "cspp-fixture")
      (mkdirs destination)
      (pkg-command "tar" "-xzf" archive "-C" destination)
      (let ([probe (string-append destination "/cspp-fixture/bin/probe")])
        (check (string=? "../lib/probe" (readlink probe)) "archive rewrote symlink target")
        (pkg-command probe)))))

;; Host metadata must report the actual distribution and glibc requirement.
(let* ([info (pkg-build-host-info)]
       [baseline (pkg-command "sh" "-c" ". /etc/os-release; printf '%s' \"$PRETTY_NAME\"")]
       [glibc (pkg-trim (pkg-command "getconf" "GNU_LIBC_VERSION"))])
  (check (string=? baseline (hashtable-ref info "baseline" "")) "wrong build distribution")
  (check (member (string-append "glibc >= " (substring glibc 6 (string-length glibc)))
                 (vector->list (hashtable-ref info "host_requirements" '#())))
         "wrong glibc requirement"))

;; Metadata strings must round-trip quotes, backslashes, control characters, and Unicode.
(let* ([text "quote\" slash\\ newline\n tab\t control\x1; 雪"]
       [document (parse-json5 (pkg-json (pkg-object "text" text "ok" #t "items" '#())))])
  (check (json5-document? document) "invalid metadata JSON")
  (check (string=? text (json5-member-value
                        (car (filter (lambda (item) (string=? "text" (json5-member-name item)))
                                     (vector->list
                                      (json5-object-members (json5-document-value document)))))))
         "metadata string did not round-trip"))
