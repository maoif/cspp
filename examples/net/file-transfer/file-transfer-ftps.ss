(import (cspp) (cspp net))

#|proc:ftps-transfer-once
The `ftps-transfer-once` procedure transfers `source` to `destination` in `direction` through an
explicit FTPS endpoint. It uses bounded transfer chunks and returns the local path whose SHA-256
digest is printed by the command-line wrapper.
|#
(define ftps-transfer-once
  (lambda (host port user password direction source destination)
    (pcheck ([string? host user password source destination] [fixnum? port] [symbol? direction])
      (let ([session (ftp-open host port #t 30000)])
        (dynamic-wind
          void
          (lambda ()
            (ftp-login! session user password)
            (case direction
              [(upload) (ftp-upload session source destination default-transfer-policy) source]
              [(download) (ftp-download session source destination default-transfer-policy) destination]
              [else (errorf 'ftps-transfer-once "direction must be upload or download")]))
          (lambda () (ftp-close session)))))))
