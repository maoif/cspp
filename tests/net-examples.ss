(import (cspp) (cspp net lws ffi))

(define example-entry-point*
  '("examples/net/echo-client-grpc.ss"
    "examples/net/echo-server-grpc.ss"
    "examples/net/http-download.ss"
    "examples/net/ftp-client.ss"
    "examples/net/sftp-client.ss"
    "examples/net/http-file-server.ss"
    "examples/net/http2-file-server.ss"
    "examples/net/https-file-server.ss"
    "examples/net/wget.ss"
    "examples/net/file-transfer/file-transfer-https.ss"
    "examples/net/file-transfer/file-transfer-wss.ss"
    "examples/net/file-transfer/file-transfer-grpc-tls.ss"
    "examples/net/file-transfer/verify-external-downloads.ss"
    "examples/net/file-transfer/verify-ftp-sftp-scp.ss"
    "examples/net/ssh-open-repl.ss"
    "examples/net/ssh-run-cmd.ss"
    "examples/net/ssh-port-forward.ss"
    "examples/net/scp.ss"
    "examples/net/file-transfer/file-transfer-ftp-client.ss"
    "examples/net/file-transfer/file-transfer-ftp-server.ss"
    "examples/net/file-transfer/file-transfer-grpc-client.ss"
    "examples/net/file-transfer/file-transfer-grpc-server.ss"
    "examples/net/file-transfer/file-transfer-http-client.ss"
    "examples/net/file-transfer/file-transfer-http-server.ss"
    "examples/net/file-transfer/file-transfer-sftp-client.ss"
    "examples/net/file-transfer/file-transfer-sftp-server.ss"
    "examples/net/file-transfer/file-transfer-tcp-client.ss"
    "examples/net/file-transfer/file-transfer-tcp-server.ss"
    "examples/net/file-transfer/file-transfer-websocket-client.ss"
    "examples/net/file-transfer/file-transfer-websocket-server.ss"
    "examples/net/file-transfer/transfer-example-client.ss"
    "examples/net/file-transfer/transfer-example-server.ss"))

(define entry-point-load-path*
  (lambda (path)
    (call-with-input-file path
      (lambda (input)
        (let loop ([path* '()])
          (let ([datum (read input)])
            (cond
             [(eof-object? datum) (reverse path*)]
             [(and (pair? datum)
                   (eq? (car datum) 'load)
                   (pair? (cdr datum))
                   (string? (cadr datum)))
              (loop (cons (cadr datum) path*))]
             [else
              (loop path*)])))))))

(define all-load-paths-exist?
  (lambda (path seen)
    (if (member path seen)
        #t
        (and (file-exists? path)
             (andmap
              (lambda (dependency)
                (all-load-paths-exist? dependency (cons path seen)))
              (entry-point-load-path* path))))))

(define file-text
  (lambda (path)
    (call-with-input-file path get-string-all)))

(define contains?
  (lambda (text fragment)
    (and (string-contains? text fragment) #t)))

(mat net-example-load-paths
     (parameterize ([current-directory ".."])
       (andmap
        (lambda (entry-point)
          (or (member entry-point
                      '("examples/net/file-transfer/verify-ftp-sftp-scp.ss"))
              (all-load-paths-exist? entry-point '())))
        example-entry-point*)))

(mat net-example-script-portability
     (let ([common (file-text "../examples/net/file-transfer/file-transfer-script-common.ss")]
           [sftp (file-text "../examples/net/file-transfer/file-transfer-sftp.ss")])
       (and (not (contains? common "/home/maoif/"))
            (not (contains? common "normalize-script-arguments"))
            (not (contains? sftp "/tmp/cspp-example-sftp"))
            (not (contains? sftp "make-uuid"))
            (not (contains? sftp "kill $(cat"))
            (not (contains? sftp " & echo $!"))
            (contains? sftp "CSPP_SFTP_EXAMPLE_STATE")
            (contains? sftp "sftp-ready-path")
            (contains? sftp "process-spawn")
            (contains? sftp "process-running?")
            (contains? sftp "process-terminate")
            (contains? sftp "process-wait"))))

(parameterize
    ([current-directory ".."])
  (load "examples/net/file-transfer/file-transfer-common.ss")
  (load "examples/net/file-transfer/file-transfer-sftp.ss"))

(define upload-path-error?
  (lambda (name)
    (guard (c [else #t])
      ((top-level-value 'validated-upload-path)
       'net-example-upload
       "/tmp/upload-root"
       name)
      #f)))

(mat net-example-upload-name-validation
     (string=? "/tmp/upload-root/file.txt"
               ((top-level-value 'validated-upload-path)
                'net-example-upload
                "/tmp/upload-root"
                "file.txt"))

     ;; Error case: an empty peer-supplied upload name is invalid.
     (upload-path-error? "")

     ;; Error case: an absolute peer-supplied upload name is invalid.
     (upload-path-error? "/tmp/escape")

     ;; Error case: a slash in a peer-supplied upload name is invalid.
     (upload-path-error? "sub/file")

     ;; Error case: a backslash in a peer-supplied upload name is invalid.
     (upload-path-error? "sub\\file")

     ;; Error case: a NUL byte in a peer-supplied upload name is invalid.
     (upload-path-error? (string #\a (integer->char 0) #\b))

     ;; Error case: the current-directory component is not an upload name.
     (upload-path-error? ".")

     ;; Error case: the parent-directory component is not an upload name.
     (upload-path-error? "..")

     ;; Error case: normalization must not let an upload name escape its root.
     (upload-path-error? "dir/../../escape"))

(define required-sftp-state-root/error?
  (lambda ()
    (guard (c [else #t])
      ((top-level-value 'required-sftp-state-root) 'net-example-sftp)
      #f)))

(mat net-example-sftp-state-coordination
     (with-env
      "CSPP_SFTP_EXAMPLE_STATE"
      "/tmp/explicit-cspp-sftp-state"
      (lambda ()
        (let ([state-root
               ((top-level-value 'required-sftp-state-root) 'net-example-sftp)])
          (and (string=? state-root "/tmp/explicit-cspp-sftp-state")
               (string=? ((top-level-value 'sftp-ready-path) state-root)
                         "/tmp/explicit-cspp-sftp-state/ready")))))

     ;; Error case: server and client must not fall back to an undiscoverable state path.
     (with-env
      "CSPP_SFTP_EXAMPLE_STATE"
      ""
      required-sftp-state-root/error?))

(mat net-ssh-example-integration
     (mat-requires (ssh)
       (or (not (= 0 (system
                      "command -v python3 >/dev/null && command -v sshd >/dev/null && command -v ssh-keygen >/dev/null && command -v ssh-agent >/dev/null && command -v ssh-add >/dev/null")))
           (= 0 (system "cd .. && python3 tests/ssh-examples-integration.py")))))

(mat net-http-file-example-integration
     (mat-requires (websockets openssl)
       (let ([status (lws-status)])
         (or (not (and (lws-capability? (vector-ref status 1) lws-cap-http2)
                       (lws-capability? (vector-ref status 1) lws-cap-tls)
                       (= 0 (system "curl --version | grep -q HTTP2"))))
             (= 0 (system "cd .. && python3 tests/integration/examples.py http"))))))

(mat net-example-cli-help
     (= 0 (system "cd .. && python3 tests/net-example-help.py")))
