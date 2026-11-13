(import (cspp) (cspp path) (cspp net))

(load "examples/net/legacy-example-common.ss")
(net-example-help! 'file-transfer-grpc-tls "Transfer a file over TLS gRPC. Positional argument: FILE-OR-DIRECTORY; CSPP_TRANSFER_ROLE=server|client.")

(load "examples/net/file-transfer/file-transfer-common.ss")
(load "examples/net/file-transfer/file-transfer-grpc.ss")

(define read-file-bytevector
  (lambda (path)
    (call-with-port
     (open-file-input-port path (file-options) (buffer-mode block) #f)
     get-bytevector-all)))

(call-with-legacy-example-log
 'file-transfer-grpc-tls
 (lambda ()
   (let ([arg* (command-line-arguments)])
     (unless (= (length arg*) 1)
       (errorf 'file-transfer-grpc-tls "expected one directory or file path"))
     (let ([server? (string=? (getenv "CSPP_TRANSFER_ROLE") "server")]
           [cert (getenv "CSPP_TRANSFER_CERT")]
           [key (getenv "CSPP_TRANSFER_KEY")])
       (unless (and (string? cert) (string? key))
         (errorf 'file-transfer-grpc-tls "TLS certificate and key environment variables are required"))
       (if server?
           (begin
             (set! grpc-file-transfer-server-credentials
                   (make-grpc-server-credentials #f
                                                 (utf8->string (read-file-bytevector cert))
                                                 (utf8->string (read-file-bytevector key))))
             (grpc-file-server (car arg*)))
           (begin
             (set! grpc-file-transfer-client-credentials
                   (make-grpc-channel-credentials
                    (utf8->string (read-file-bytevector cert))
                    #f
                    #f))
             (grpc-file-client (list (car arg*)))))))))
