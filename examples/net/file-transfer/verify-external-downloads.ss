(import (cspp))
(load (if (file-exists? "examples/net/legacy-example-common.ss")
          "examples/net/legacy-example-common.ss"
          "../examples/net/legacy-example-common.ss"))
(net-example-help! 'verify-external-downloads "Verify optional public HTTPS artifacts. Positional argument: OUTPUT-DIRECTORY.")


(define external-artifacts
  '(("https://ftp.gnu.org/gnu/emacs/windows/emacs-30/emacs-30.2.zip"
     "414d3a1a21147af257ebd98bdd15976fdcb5ed0563f6de89f76d4a4b5dad9c72")
    ("https://mirrors.tuna.tsinghua.edu.cn/archlinux/iso/2026.07.01/archlinux-2026.07.01-x86_64.iso"
     "e86295dc0bdf9b85a5a9256810c553239689d2ae8e80eeec81b4e2e910d8a6c0")))

#|proc:verify-external-download
The `verify-external-download` procedure streams `uri` to `path` and verifies its SHA-256 digest.
The `uri` parameter is the HTTPS resource, `path` is the destination outside the repository, and
`expected-hex` is its lowercase SHA-256 digest. The return value is `path`.
|#
(define verify-external-download
  (lambda (uri path expected-hex)
    (pcheck ([string? uri path expected-hex])
      (let ([response (http-download uri path)])
        (unless (= (http-response-status response) 200)
          (errorf 'verify-external-download
                  "HTTP download failed with status ~a for ~a"
                  (http-response-status response) uri))
        (let ([actual (bytevector->hex (digest-file 'sha256 path))])
          (unless (string-ci=? actual expected-hex)
            (errorf 'verify-external-download
                    "SHA-256 mismatch for ~a: expected ~a, got ~a"
                    uri expected-hex actual)))
        path))))

(let ([destination (and (pair? (command-line-arguments))
                        (car (command-line-arguments)))])
  (unless destination
    (errorf 'verify-external-downloads "expected an output directory argument"))
  (unless (file-directory? destination)
    (mkdir destination))
  (for-each
   (lambda (artifact)
     (let* ([uri (car artifact)]
            [name (path-basename uri)]
            [path (string-append destination "/" name)])
       (verify-external-download uri path (cadr artifact))
       (printf "~a ~a ~a\n" uri path (file-size path))))
   external-artifacts))
