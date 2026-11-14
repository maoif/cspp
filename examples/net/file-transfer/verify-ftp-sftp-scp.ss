(import (cspp)
        (cspp crypto)
        (cspp net))
(load (if (file-exists? "examples/net/legacy-example-common.ss")
          "examples/net/legacy-example-common.ss"
          "../examples/net/legacy-example-common.ss"))
(net-example-help! 'verify-ftp-sftp-scp "Verify loopback transfers. Positional arguments: SOURCE FTP-DOWNLOAD SFTP-DOWNLOAD SCP-DOWNLOAD.")


(load "net-common.ss")
(load "net-ftp-common.ss")
(load "net-ssh-common.ss")
(load "../examples/net/interactive-transfer-common.ss")
(load "../examples/net/ftp-client-common.ss")
(load "../examples/net/sftp-client-common.ss")

(define verify-same-digest!
  (lambda (protocol source-path downloaded-path)
    (unless (equal? (sha256-file source-path) (sha256-file downloaded-path))
      (errorf 'verify-ftp-sftp-scp "~a round-trip digest mismatch" protocol))))

(define run-command-transcript
  (lambda (transcript procedure)
    (call-with-port
     (open-string-input-port transcript)
     (lambda (input)
       (call-with-string-output-port
        (lambda (output)
          (procedure input output)))))))

(define verify-ftp!
  (lambda (source-path downloaded-path)
    (let-values ([(root port stop-server) (start-ftp-test-server)])
      (dynamic-wind
        void
        (lambda ()
          (let ([transcript
                 (format
                  "pwd\nls\nmkdir /verify\nput ~a /verify/source.bin\n~
                   get /verify/source.bin ~a\nrename /verify/source.bin /verify/renamed.bin\n~
                   rm /verify/renamed.bin\nrmdir /verify\nquit\n"
                  source-path downloaded-path)])
            (run-command-transcript
             transcript
             (lambda (input output)
               (run-ftp-client "127.0.0.1" port "user" "pass" input output)))
            (verify-same-digest! 'ftp source-path downloaded-path)))
        stop-server))))

(define verify-sftp!
  (lambda (source-path downloaded-path)
    (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
      (dynamic-wind
        void
        (lambda ()
          (with-env
           "HOME" home
           (lambda ()
             (let* ([remote-directory (string-append remote-root "/verify")]
                    [remote-source (string-append remote-directory "/source.bin")]
                    [remote-renamed (string-append remote-directory "/renamed.bin")]
                    [private-key (string-append home "/.ssh/id_ed25519")]
                    [transcript
                     (format
                      "pwd\nls ~a\nmkdir ~a\nput ~a ~a\nget ~a ~a\nrename ~a ~a\nrm ~a\n~
                       rmdir ~a\nquit\n"
                      remote-root remote-directory source-path remote-source remote-source
                      downloaded-path remote-source remote-renamed remote-renamed
                      remote-directory)])
               (run-command-transcript
                transcript
                (lambda (input output)
                  (run-sftp-client "127.0.0.1" port user private-key input output)))
               (verify-same-digest! 'sftp source-path downloaded-path)))))
        stop-server))))

(define verify-scp!
  (lambda (source-path downloaded-path)
    (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
      (dynamic-wind
        void
        (lambda ()
          (with-env
           "HOME" home
           (lambda ()
             (let ([ssh (ssh-open "127.0.0.1" port user)])
               (dynamic-wind
                 void
                 (lambda ()
                   (ssh-auth-publickey! ssh user)
                   (let ([scp (scp-open ssh)]
                         [remote-path (string-append remote-root "/source.bin")])
                     (dynamic-wind
                       void
                       (lambda ()
                         (scp-upload scp source-path remote-path default-transfer-policy)
                         (scp-download scp remote-path downloaded-path default-transfer-policy)
                         (verify-same-digest! 'scp source-path downloaded-path))
                       (lambda () (scp-close scp)))))
                 (lambda () (ssh-close ssh)))))))
        stop-server))))

(let ([argument* (command-line-arguments)])
  (unless (= (length argument*) 4)
    (errorf 'verify-ftp-sftp-scp
            "usage: verify-ftp-sftp-scp SOURCE FTP-DOWNLOAD SFTP-DOWNLOAD SCP-DOWNLOAD"))
  (let ([source-path (car argument*)]
        [ftp-path (cadr argument*)]
        [sftp-path (caddr argument*)]
        [scp-path (cadddr argument*)])
    (verify-ftp! source-path ftp-path)
    (verify-sftp! source-path sftp-path)
    (verify-scp! source-path scp-path)
    (display (bytevector->hex (sha256-file source-path)))
    (newline)))
