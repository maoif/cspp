(import (cspp)
        (cspp net))
(load "examples/net/file-transfer/file-transfer-common.ss")

#|proc:scp-transfer-once
The `scp-transfer-once` procedure transfers `source` to `destination` through an authenticated SSH
SCP session. `direction` is `upload` or `download`; the transfer uses the explicit default policy.
|#
(define scp-transfer-once
  (lambda (host port user private-key direction source destination)
    (pcheck ([string? host user private-key source destination] [fixnum? port] [symbol? direction])
      (let ([ssh (ssh-open-with-policy host port user 30000 'accept-new)])
        (dynamic-wind
          void
          (lambda ()
            (ssh-auth-private-key! ssh user (string-append private-key ".pub") private-key #f)
            (let ([session (scp-open ssh)])
              (dynamic-wind
                void
                (lambda ()
                  (case direction
                    [(upload) (scp-upload session source destination default-transfer-policy) source]
                    [(download) (scp-download session source destination default-transfer-policy) destination]
                    [else (errorf 'scp-transfer-once "direction must be upload or download")]))
                (lambda () (scp-close session)))))
          (lambda () (ssh-close ssh)))))))
