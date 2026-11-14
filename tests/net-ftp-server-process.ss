(import (cspp)
        (cspp file)
        (cspp path)
        (cspp net))

(load "net-common.ss")

(define main
  (lambda ()
    (let ([root (car (command-line-arguments))])
      (define connection-count-path (string-append root ".control-connections"))
      (define command-log-path (string-append root ".commands"))
      (define accepted-connections 0)
      (let ([listener (open-socket 'inet 'stream)])
        (socket-set-option! listener 'reuse-address #t)
        (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
        (socket-listen! listener 8)
        (let ([port (socket-address-port (socket-local-address listener))])
          (define running? #t)
          (define client-threads '())
          (define client-sockets '())
          (define remove-client-socket!
            (lambda (client)
              (let loop ([rest client-sockets] [out '()])
                (cond
                 [(null? rest)
                  (set! client-sockets (reverse out))]
                 [(eq? (car rest) client)
                  (set! client-sockets (append (reverse out) (cdr rest)))]
                 [else
                  (loop (cdr rest) (cons (car rest) out))]))))
          (define physical-path
            (lambda (virtual-path)
              (string-append root (normalize-absolute-test-path virtual-path))))
          (define log-command!
            (lambda (line)
              (let ([op (open-file-output-port command-log-path
                                               (file-options no-fail no-truncate)
                                               (buffer-mode block)
                                               (native-transcoder))])
                (dynamic-wind
                  (lambda () (file-position op (file-length op)))
                  (lambda () (put-string op line) (newline op))
                  (lambda () (close-port op))))))
          (define open-passive
            (lambda ()
              (let ([sock (open-socket 'inet 'stream)])
                (socket-set-option! sock 'reuse-address #t)
                (socket-bind! sock (make-socket-address 'inet "127.0.0.1" 0))
                (socket-listen! sock 1)
                sock)))
          (define close-passive
            (lambda (sock)
              (when sock
                (guard (c [else #f])
                  (close-socket sock)))))
          (define handle-client
            (lambda (client)
              (let ([ip (open-socket-input-port client)]
                    [op (open-socket-output-port client)])
                (define cwd "/")
                (define rename-from #f)
                (define restart-offset 0)
                (define passive-listener #f)
                (define passive-port #f)
                (define data-accept
                  (lambda ()
                    (unless passive-listener
                      (error 'ftp-test "passive listener missing"))
                    (let-values ([(data peer) (socket-accept passive-listener)])
                      (close-passive passive-listener)
                      (set! passive-listener #f)
                      (set! passive-port #f)
                      data)))
                (define ensure-parent-dir
                  (lambda (path)
                    (mkdirs (path-dirname path))))
                (define list-dir
                  (lambda (path)
                    (map (lambda (name) (string-append name "\r\n"))
                         (directory-list path))))
                (define mlsd-line
                  (lambda (path name)
                    (let ([entry-path (if (string-endswith? path "/")
                                          (string-append path name)
                                          (string-append path "/" name))])
                      (if (file-directory? entry-path)
                          (format "type=dir;modify=20260812000000;perm=elcmfd; ~a\r\n" name)
                          (format "type=file;size=~a;modify=20260812000000;perm=rwafd; ~a\r\n"
                                  (file-size entry-path)
                                  name)))))
                (define mlsd-dir
                  (lambda (path)
                    (map (lambda (name) (mlsd-line path name))
                         (directory-list path))))
                (send-crlf-line op "220 cspp ftp test server")
                (let loop ()
                  (let ([line (read-crlf-line ip)])
                    (when line
                      (log-command! line)
                      (let* ([parts (string-split line #\space)]
                             [cmd (string-upcase (car parts))]
                             [arg (if (> (string-length line) (+ (string-length (car parts)) 1))
                                      (substring line (+ (string-length (car parts)) 1)
                                                 (string-length line))
                                      "")])
                        (cond
                         [(string=? cmd "USER")
                          (send-crlf-line op "331 password required")
                          (loop)]
                         [(string=? cmd "PASS")
                          (send-crlf-line op "230 logged in")
                          (loop)]
                         [(string=? cmd "SYST")
                          (send-crlf-line op "215 UNIX Type: L8")
                          (loop)]
                         [(string=? cmd "FEAT")
                          (send-crlf-line op "211-Features")
                          (send-crlf-line op " EPSV")
                          (send-crlf-line op " MLSD")
                          (send-crlf-line op " MLST type*;size*;modify*;perm*;")
                          (send-crlf-line op " UTF8")
                          (send-crlf-line op "211 End")
                          (loop)]
                         [(string=? cmd "TYPE")
                          (send-crlf-line op "200 type set")
                          (loop)]
                         [(string=? cmd "PWD")
                          (send-crlf-line op (format "257 \"~a\"" cwd))
                          (loop)]
                         [(string=? cmd "CWD")
                          (let* ([target (test-path-join cwd arg)]
                                 [path (physical-path target)])
                            (if (file-directory? path)
                                (begin
                                  (set! cwd target)
                                  (send-crlf-line op "250 directory changed"))
                                (send-crlf-line op "550 not a directory"))
                            (loop))]
                         [(or (string=? cmd "PASV") (string=? cmd "EPSV"))
                          (close-passive passive-listener)
                          (set! passive-listener (open-passive))
                          (set! passive-port
                                (socket-address-port (socket-local-address passive-listener)))
                          (if (string=? cmd "PASV")
                              (let ([p1 (quotient passive-port 256)]
                                    [p2 (mod passive-port 256)])
                                (send-crlf-line op
                                                (format "227 Entering Passive Mode (127,0,0,1,~a,~a)"
                                                        p1
                                                        p2)))
                              (send-crlf-line op
                                              (format "229 Entering Extended Passive Mode (|||~a|)"
                                                      passive-port)))
                          (loop)]
                         [(or (string=? cmd "LIST") (string=? cmd "NLST")
                              (string=? cmd "MLSD"))
                          (let* ([target (if (string=? arg "") cwd (test-path-join cwd arg))]
                                 [path (physical-path target)])
                            (if (file-directory? path)
                                (begin
                                  (when (string=? target "/slow")
                                    (milisleep 200))
                                  (send-crlf-line op "150 opening data connection")
                                  (let ([data (data-accept)])
                                    (let ([dop (open-socket-output-port data)])
                                      (for-each (lambda (entry)
                                                  (put-bytevector dop (string->utf8 entry)))
                                                (if (string=? cmd "MLSD")
                                                    (mlsd-dir path)
                                                    (list-dir path)))
                                      (flush-output-port dop)
                                      (close-port dop))
                                    (close-socket data)
                                    (send-crlf-line op "226 transfer complete")))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "MLST")
                          (let* ([target (test-path-join cwd arg)]
                                 [path (physical-path target)])
                            (if (file-exists? path)
                                (begin
                                  (send-crlf-line op "250-Listing")
                                  (send-crlf-line
                                   op
                                   (string-append " "
                                                  (let ([line (mlsd-line
                                                               (path-dirname path)
                                                               (file-basename path))])
                                                    (substring line 0
                                                               (fx- (string-length line) 2)))))
                                  (send-crlf-line op "250 End"))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "SIZE")
                          (let* ([target (test-path-join cwd arg)]
                                 [path (physical-path target)])
                            (if (file-regular? path)
                                (send-crlf-line op (format "213 ~a" (file-size path)))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "REST")
                          (let ([offset (string->number arg)])
                            (if (and offset (natural? offset))
                                (begin
                                  (set! restart-offset offset)
                                  (send-crlf-line op "350 restart position accepted"))
                                (send-crlf-line op "501 invalid restart position"))
                            (loop))]
                         [(string=? cmd "RETR")
                          (let* ([target (test-path-join cwd arg)]
                                 [path (physical-path target)])
                            (if (file-regular? path)
                                (begin
                                  (send-crlf-line op "150 opening data connection")
                                  (let ([data (data-accept)])
                                    (let* ([dop (open-socket-output-port data)]
                                           [content (read-u8vec path)])
                                      (put-bytevector dop content restart-offset
                                                      (fx- (bytevector-length content)
                                                           restart-offset))
                                      (flush-output-port dop)
                                      (close-port dop))
                                    (set! restart-offset 0)
                                    (close-socket data)
                                    (send-crlf-line op "226 transfer complete")))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "STOR")
                          (let* ([target (test-path-join cwd arg)]
                                 [path (physical-path target)])
                            (ensure-parent-dir path)
                            (send-crlf-line op "150 opening data connection")
                            (let ([data (data-accept)])
                              (let* ([dip (open-socket-input-port data)]
                                     [incoming (read-port->bytevector dip)]
                                     [prefix (if (and (fx> restart-offset 0)
                                                      (file-exists? path))
                                                 (let* ([old (read-u8vec path)]
                                                        [copy (make-bytevector restart-offset)])
                                                   (bytevector-copy! old 0 copy 0 restart-offset)
                                                   copy)
                                                 #vu8())]
                                     [content (make-bytevector
                                               (fx+ (bytevector-length prefix)
                                                    (bytevector-length incoming)))])
                                (bytevector-copy! prefix 0 content 0 (bytevector-length prefix))
                                (bytevector-copy! incoming 0 content (bytevector-length prefix)
                                                  (bytevector-length incoming))
                                (write-bytevector-file path content)
                                (close-port dip))
                              (close-socket data)
                              (set! restart-offset 0)
                              (send-crlf-line op "226 transfer complete"))
                            (loop))]
                         [(string=? cmd "DELE")
                          (let ([path (physical-path (test-path-join cwd arg))])
                            (if (file-regular? path)
                                (begin
                                  (delete-file path)
                                  (send-crlf-line op "250 deleted"))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "MKD")
                          (mkdirs (physical-path (test-path-join cwd arg)))
                          (send-crlf-line op "257 created")
                          (loop)]
                         [(string=? cmd "RMD")
                          (let ([path (physical-path (test-path-join cwd arg))])
                            (if (file-directory? path)
                                (begin
                                  (delete-directory path)
                                  (send-crlf-line op "250 removed"))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "RNFR")
                          (let ([path (test-path-join cwd arg)])
                            (if (file-exists? (physical-path path))
                                (begin
                                  (set! rename-from path)
                                  (send-crlf-line op "350 ready for RNTO"))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "RNTO")
                          (if rename-from
                              (let ([src (physical-path rename-from)]
                                    [dest (physical-path (test-path-join cwd arg))])
                                (ensure-parent-dir dest)
                                (file-move src dest)
                                (set! rename-from #f)
                                (send-crlf-line op "250 renamed"))
                              (send-crlf-line op "503 bad sequence"))
                          (loop)]
                         [(string=? cmd "QUIT")
                          (send-crlf-line op "221 bye")]
                         [else
                          (send-crlf-line op "502 command not implemented")
                          (loop)]))))))))
          (define spawn-client-handler
            (lambda (client)
              (set! client-sockets (cons client client-sockets))
              (let ([th
                     (fork-thread
                      (lambda ()
                        (guard (c [else #f])
                          (handle-client client))
                        (remove-client-socket! client)
                        (guard (c [else #f])
                          (close-socket client))))])
                (set! client-threads (cons th client-threads))
                th)))
          (define accept-thread
            (fork-thread
             (lambda ()
               (let loop ()
                 (when running?
                   (let ([accepted
                          (guard (c [else #f])
                            (call-with-values
                              (lambda ()
                                (socket-accept/nonblocking listener))
                              (case-lambda
                                [(v) v]
                                [(client peer)
                                 (cons client peer)])))])
                     (if (and accepted (not (net-would-block? accepted)))
                         (let ([client (car accepted)]
                               [peer (cdr accepted)])
                           (set! accepted-connections (+ accepted-connections 1))
                           (write-bytevector-file
                            connection-count-path
                            (string->utf8 (number->string accepted-connections)))
                           (spawn-client-handler client)
                           (loop))
                         (begin
                           (milisleep 50)
                           (loop)))))))))
          (define command-thread
            (begin
              ;; Console input and output share a mutex. Announce readiness before reading stdin.
              (write port)
              (newline)
              (flush-output-port)
              (fork-thread
               (lambda ()
                 (let loop ()
                   (let ([line (get-line (current-input-port))])
                     (unless (eof-object? line)
                       (when (string=? line "stop")
                         (set! running? #f)
                         (guard (c [else #f])
                           (close-socket listener))
                         (for-each
                          (lambda (client)
                            (guard (c [else #f]) (close-socket client)))
                          client-sockets)
                         (exit 0))
                       (when running?
                         (loop)))))))))
          (thread-join command-thread)
          (set! running? #f)
          (guard (c [else #f])
            (close-socket listener))
          (for-each (lambda (client)
                      (guard (c [else #f]) (close-socket client)))
                    client-sockets)
          (thread-join accept-thread)
          (for-each (lambda (client)
                      (guard (c [else #f])
                        (close-socket client)))
                    client-sockets))))))

(main)
