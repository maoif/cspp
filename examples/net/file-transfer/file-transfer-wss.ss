(import (cspp)
        (cspp net))

(load "examples/net/legacy-example-common.ss")
(net-example-help! 'file-transfer-wss "Transfer a file over WSS. Positional argument: FILE-OR-DIRECTORY; CSPP_TRANSFER_ROLE=server|client.")

(load "examples/net/file-transfer/file-transfer-common.ss")
(load "examples/net/file-transfer/file-transfer-websocket.ss")

(define secure-context
  (lambda (role)
    (let ([ctx (make-tls-context role)])
      (if (eq? role 'server)
          (begin
            (tls-context-load-cert! ctx (getenv "CSPP_TRANSFER_CERT"))
            (tls-context-load-private-key! ctx (getenv "CSPP_TRANSFER_KEY")))
          (begin
            (tls-context-load-ca-file! ctx (getenv "CSPP_TRANSFER_CERT"))
            (tls-context-set-verify! ctx #t)))
      ctx)))

(call-with-legacy-example-log
 'file-transfer-wss
 (lambda ()
   (let ([arg* (command-line-arguments)])
     (unless (= (length arg*) 1)
       (errorf 'file-transfer-wss "expected one directory or file path"))
     (let* ([server? (string=? (getenv "CSPP_TRANSFER_ROLE") "server")]
            [ctx (secure-context (if server? 'server 'client))]
            [options (make-websocket-options ctx '("cspp-file-transfer")
                                             65536 #f 30000)])
       (set! websocket-file-transfer-port 41009)
       (set! websocket-file-transfer-options options)
       (if server?
           (websocket-file-server (car arg*))
           (websocket-file-client (list (car arg*))))
       (close-tls-context ctx)))))
