(import (cspp) (cspp file)
        (cspp net))

(load "net-common.ss")
(load "net-ftp-common.ss")

(define wait-ftp-nonblocking
  (lambda (proc)
    (let ([answer (net-operation-wait (proc))])
      (if (bytevector? answer)
          (filter (lambda (entry) (not (string=? entry "")))
                  (map (lambda (entry) (string-trim-right entry #\return))
                       (string-split (utf8->string answer) #\newline)))
          answer))))

(define retry-ftp-test-op
  (lambda (proc)
    (let loop ([i 0])
      (guard (c [else
                 (if (and (net-error? c) (< i 4))
                     (begin
                       (milisleep 100)
                     (loop (+ i 1)))
                     (raise c))])
        (proc)))))

(define ftp-entry-names
  (lambda (entry*)
    (map ftp-directory-entry-name entry*)))

(define ftp-test-policy
  (lambda (resume overwrite progress)
    (make-transfer-policy resume overwrite 3 progress)))

(define ftp-command-seen?
  (lambda (root command)
    (let ([path (string-append root ".commands")])
      (and (file-exists? path)
           (string-contains? (utf8->string (read-u8vec path)) command)))))

(define ftp-net-error-timeout?
  (lambda (thunk)
    (guard (c [else
               (and (net-error? c)
                    (or (string-contains? (net-error-message c) "timed out")
                        (string-contains? (net-error-message c) "Timeout")))])
      (thunk)
      #f)))

(define ftp-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                      (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(define start-stalled-ftp-control-server
  (lambda (delay-ms)
    (let ([listener (open-socket 'inet 'stream)])
      (socket-set-option! listener 'reuse-address #t)
      (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
      (socket-listen! listener 4)
      (let ([port (socket-address-port (socket-local-address listener))])
        (values listener
                port
                (fork-thread
                 (lambda ()
                   (let-values ([(client peer) (socket-accept listener)])
                     (milisleep delay-ms)
                     (close-socket client)
                     (close-socket listener)))))))))

(mat net-ftp-session
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))])
           (dynamic-wind
             void
             (lambda ()
               (and (ftp-session? session)
                    (ftp-login! session "user" "pass")
                    (equal? (ftp-pwd session) "/")
                    (not (ftp-active-mode! session))
                    (ftp-passive-mode! session)
                    (equal? (ftp-quit! session) session)))
             (lambda ()
               (stop-server)))))))
     (mat optional-net-ftp-operation
       (mat-requires (curl)
         (let-values ([(root port stop-server) (start-ftp-test-server)])
           (let ([session (ftp-open "127.0.0.1" port #f 2000)])
             (dynamic-wind
               void
               (lambda ()
                 (and (ftp-session? session)
                      (ftp-login! session "user" "pass")
                      (equal? (ftp-pwd session) "/")))
               (lambda ()
                 (ftp-close session)
                 (stop-server))))
         (let-values ([(root port stop-server) (start-ftp-test-server)])
           (dynamic-wind
             void
             (lambda ()
               (call-with-ftp-session
                (format "ftp://127.0.0.1:~a/" port)
                2000
                ftp-session?))
             (lambda ()
               (stop-server))))
         (let-values ([(root port stop-server) (start-ftp-test-server)])
           (dynamic-wind
             void
             (lambda ()
               (call-with-ftp-session
                "127.0.0.1"
                port
                2000
                ftp-session?))
             (lambda ()
               (stop-server))))
         (let-values ([(listener port th)
                       (start-stalled-ftp-control-server 200)])
           (dynamic-wind
             void
             (lambda ()
               (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port) 50)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (ftp-net-error-timeout?
                      (lambda ()
                        (ftp-list session))))
                   (lambda ()
                     (ftp-close session)))))
             (lambda ()
               (thread-join th)
               (guard (c [else #f])
                 (close-socket listener))))))))

(mat net-ftp-tls-verification-api
     (mat-requires (curl)
       (let ([plain (ftp-open "ftp://127.0.0.1:21/")]
             [explicit (ftp-open "ftp://127.0.0.1:21/" 'explicit)]
             [secure (ftp-open "ftps://127.0.0.1:21/")])
         (dynamic-wind
           void
           (lambda ()
             (and
              (eq? 'plain (ftp-mode plain))
              (eq? 'explicit (ftp-mode explicit))
              (eq? 'implicit (ftp-mode secure))
              (not (ftp-verify-peer? plain))
              (not (ftp-verify-host? plain))
              (ftp-verify-peer? secure)
              (ftp-verify-host? secure)
              (eq? (ftp-set-tls-verification! secure #f #f) secure)
              (not (ftp-verify-peer? secure))
              (not (ftp-verify-host? secure))
              (eq? (ftp-set-tls-verification! secure #t #t) secure)
              (ftp-verify-peer? secure)
              (ftp-verify-host? secure)))
           (lambda ()
             (ftp-close plain)
             (ftp-close explicit)
             (ftp-close secure)))))

     ;; An unknown FTPS mode is invalid.
     (mat-requires (curl)
       (ftp-error-message-contains?
        "FTP mode must be"
        (lambda () (ftp-open "ftp://127.0.0.1:21/" 'automatic)))))

(mat net-ftp-timeout-validation
     (mat-requires (curl)
       (let ([endpoint "ftp://127.0.0.1:21/"])
         (and
          (ftp-error-message-contains?
           "timeout must be non-negative"
           (lambda ()
             (ftp-open endpoint -1)))
          (ftp-error-message-contains?
           "timeout must be non-negative"
           (lambda ()
             (ftp-open "127.0.0.1" 21 #f -1)))
          (ftp-error-message-contains?
           "timeout must be non-negative"
           (lambda ()
             (call-with-ftp-session endpoint -1 ftp-session?)))
          (ftp-error-message-contains?
           "timeout must be non-negative"
           (lambda ()
             (call-with-ftp-session "127.0.0.1" 21 #f -1 ftp-session?)))))))

(mat net-ftp-port-validation
     (mat-requires (curl)
       (and
        (ftp-error-message-contains?
         "port must be between 0 and 65535"
         (lambda ()
           (ftp-open "127.0.0.1" -1 #f 1000)))
        (ftp-error-message-contains?
         "port must be between 0 and 65535"
         (lambda ()
           (call-with-ftp-session "127.0.0.1" 70000 #f 1000 ftp-session?))))))

(mat net-ftp-list
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))])
           (dynamic-wind
             void
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (let ([entries (begin
                                     (milisleep 50)
                                     (ftp-list session))])
                      (let ([name* (ftp-entry-names entries)])
                        (and (not (not (member "docs" name*)))
                             (not (not (member "hello.txt" name*))))))))
             (lambda ()
               (stop-server)))))))

(mat net-ftp-directory-entry
     (let ([entry (ftp-parse-mlsd-line
                   "type=file;size=12;modify=20260801123456;perm=rw; sample.txt")])
       (and (ftp-directory-entry? entry)
            (string=? "sample.txt" (ftp-directory-entry-name entry))
            (eq? 'file (ftp-directory-entry-type entry))
            (= 12 (ftp-directory-entry-size entry))
            (string=? "20260801123456" (ftp-directory-entry-modify entry))
            (equal? '(read write) (ftp-directory-entry-permissions entry))))

     ;; An MLSD line without the fact/name delimiter is invalid.
     (ftp-error-message-contains?
      "name delimiter"
      (lambda () (ftp-parse-mlsd-line "type=file;size=1;missing.txt")))

     ;; A nonnumeric MLSD size is invalid.
     (ftp-error-message-contains?
      "size is invalid"
      (lambda () (ftp-parse-mlsd-line "type=file;size=nope; bad.txt")))

     ;; A duplicate MLSD fact is invalid.
     (ftp-error-message-contains?
      "duplicate fact"
      (lambda () (ftp-parse-mlsd-line "type=file;type=dir; duplicate")))

     ;; An unknown MLSD fact is retained for forward compatibility.
     (let ([entry (ftp-parse-mlsd-line "type=file;x-vendor=yes; unknown.txt")])
       (equal? '("x-vendor" . "yes")
               (assoc "x-vendor" (ftp-directory-entry-facts entry))))

     (mat-requires (curl)
       (with-test-ftp-session
        (lambda (session)
          (let ([entry (ftp-stat session "/hello.txt")])
            (and (ftp-directory-entry? entry)
                 (eq? 'file (ftp-directory-entry-type entry))
                 (= 9 (ftp-directory-entry-size entry))
                 (bytevector? (ftp-list/raw session))
                 (not (ftp-stat session "/missing.txt"))))))))

(mat net-ftp-cwd
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [docs-path (string-append root "/docs")])
           (dynamic-wind
             void
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (equal? (ftp-cwd! session "/docs") session)
                    (equal? (ftp-pwd session) "/docs")
                    (equal? (directory-list docs-path)
                            '("readme.txt"))
                    (equal? (begin
                              (milisleep 50)
                              (ftp-entry-names (ftp-list session)))
                            '("readme.txt"))))
             (lambda ()
               (stop-server)))))))

(mat net-ftp-download
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [download-path "/tmp/cspp-net-ftp-download.txt"])
           (dynamic-wind
             (lambda ()
               (when (file-exists? download-path)
                 (delete-file download-path #f)))
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (equal? (ftp-download session "/hello.txt" download-path)
                            download-path)
                    (equal? (read-u8vec download-path)
                            (string->utf8 "hello ftp"))))
             (lambda ()
               (when (file-exists? download-path)
                 (delete-file download-path #f))
               (stop-server)))))))

(mat net-ftp-upload
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [upload-path "/tmp/cspp-net-ftp-upload.txt"]
               [uploaded-path (string-append root "/uploaded.txt")])
           (dynamic-wind
             (lambda ()
               (when (file-exists? upload-path)
                 (delete-file upload-path #f)))
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (begin
                      (write-bytevector-file upload-path (string->utf8 "upload ftp"))
                      #t)
                    (equal? (ftp-upload session upload-path "/uploaded.txt")
                            "/uploaded.txt")
                    (file-regular? uploaded-path)
                    (equal? (read-u8vec uploaded-path)
                            (string->utf8 "upload ftp"))))
             (lambda ()
               (when (file-exists? upload-path)
                 (delete-file upload-path #f))
               (stop-server)))))))

(mat net-ftp-rename
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [uploaded-path (string-append root "/uploaded.txt")]
               [renamed-path (string-append root "/renamed.txt")])
           (dynamic-wind
             (lambda ()
               (write-bytevector-file uploaded-path (string->utf8 "rename me")))
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (eq? (ftp-rename! session "/uploaded.txt" "/renamed.txt")
                         session)
                    (not (file-exists? uploaded-path))
                    (file-regular? renamed-path)
                    (equal? (read-u8vec renamed-path)
                            (string->utf8 "rename me"))))
             (lambda ()
               (stop-server)))))))

(mat net-ftp-mkdir
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [tmpdir-path (string-append root "/tmpdir")])
           (dynamic-wind
             void
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (eq? (ftp-mkdir! session "/tmpdir") session)
                    (file-directory? tmpdir-path)))
             (lambda ()
               (stop-server)))))))

(mat net-ftp-rmdir
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [tmpdir-path (string-append root "/tmpdir")])
           (dynamic-wind
             (lambda ()
               (mkdirs tmpdir-path))
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (eq? (begin
                           (milisleep 100)
                           (ftp-rmdir! session "/tmpdir"))
                         session)
                    (not (file-exists? tmpdir-path))))
             (lambda ()
               (stop-server)))))))

(mat net-ftp-delete
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [victim-path (string-append root "/victim.txt")])
           (dynamic-wind
             (lambda ()
               (write-bytevector-file victim-path (string->utf8 "delete me")))
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (eq? (retry-ftp-test-op
                          (lambda ()
                            (ftp-delete! session "/victim.txt")))
                         session)
                    (not (file-exists? victim-path))))
             (lambda ()
               (stop-server)))))))

(mat net-ftp-input-port
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))])
           (dynamic-wind
             void
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (call-with-port
                     (open-ftp-input-port session "/hello.txt")
                     (lambda (ip)
                       (equal? (read-port->bytevector ip)
                               (string->utf8 "hello ftp"))))))
             (lambda ()
               (ftp-close session)
               (stop-server)))))))

(mat net-ftp-output-port
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [ported-path (string-append root "/ported.txt")])
           (dynamic-wind
             void
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (call-with-port
                     (open-ftp-output-port session "/ported.txt")
                     (lambda (op)
                       (put-bytevector op (string->utf8 "through port"))))
                    (file-regular? ported-path)
                    (equal? (read-u8vec ported-path)
                            (string->utf8 "through port"))))
             (lambda ()
               (ftp-close session)
               (stop-server)))))))

(mat net-ftp-input-port-closed-session
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))])
           (dynamic-wind
             void
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (begin
                      (ftp-close session)
                      #t)
                    (ftp-error-message-contains?
                     "FTP session is closed"
                     (lambda ()
                       (open-ftp-input-port session "/hello.txt")))))
             (lambda ()
               (stop-server)))))))

(mat net-ftp-output-port-closed-session
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))])
           (dynamic-wind
             void
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (begin
                      (ftp-close session)
                      #t)
                    (ftp-error-message-contains?
                     "FTP session is closed"
                     (lambda ()
                       (open-ftp-output-port session "/ported.txt")))))
             (lambda ()
               (stop-server)))))))

(mat net-ftp-output-port-close-closed-session
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [ported-path (string-append root "/ported-after-close.txt")])
           (dynamic-wind
             void
             (lambda ()
               (and
                (ftp-login! session "user" "pass")
                (let ([op (open-ftp-output-port session "/ported-after-close.txt")])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (begin
                         (put-bytevector op (string->utf8 "through close"))
                         (ftp-close session)
                         #t)
                       (ftp-error-message-contains?
                        "FTP file is closed"
                        (lambda () (close-port op)))))
                    (lambda ()
                      (unless (port-closed? op)
                        (guard (c [else #f])
                          (close-port op))))))))
             (lambda ()
               (stop-server)))))))

(mat net-ftp-list-nonblocking
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))])
           (dynamic-wind
             void
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (let ([entries (wait-ftp-nonblocking
                                    (lambda ()
                                      (ftp-list/nonblocking session)))])
                      (let ([name* (map (lambda (line)
                                         (ftp-directory-entry-name
                                          (ftp-parse-mlsd-line line)))
                                       entries)])
                        (and (list? entries)
                             (not (not (member "docs" name*)))
                             (not (not (member "hello.txt" name*))))))))
             (lambda ()
               (stop-server)))))))

(mat net-ftp-readiness-operation
     (mat-requires (curl)
       (with-test-ftp-session
        (lambda (session)
          (let ([operation (ftp-list/nonblocking session ".")])
            (and (net-operation? operation)
                 (let loop ([pending-cycles 0])
                   (net-operation-step! operation)
                   (case (net-operation-state operation)
                     [(completed)
                      (and (fx>= pending-cycles 2)
                           (bytevector? (net-operation-result operation)))]
                     [(pending)
                      (poll (net-operation-poll-targets operation)
                            (net-operation-remaining-timeout-ms operation))
                      (loop (fx1+ pending-cycles))]
                     [else #f]))))))))

(mat net-ftp-cancel-pending
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))])
           (dynamic-wind
             void
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (eq? (ftp-cancel-pending! session) session)
                    (let ([operation (ftp-list/nonblocking session "/slow")])
                      (and (net-operation? operation)
                           (eq? (ftp-cancel-pending! session) session)
                           (eq? (net-operation-state operation) 'cancelled)))
                    (let* ([entries (ftp-list session)]
                           [name* (ftp-entry-names entries)])
                      (and (list? entries)
                           (not (not (member "docs" name*)))
                           (not (not (member "hello.txt" name*)))))))
             (lambda ()
               (stop-server)))))))

(mat net-ftp-download-nonblocking
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [download-path "/tmp/cspp-net-ftp-download-nb.txt"])
           (dynamic-wind
             (lambda ()
               (when (file-exists? download-path)
                 (delete-file download-path #f)))
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (equal? (wait-ftp-nonblocking
                             (lambda ()
                               (ftp-download/nonblocking session
                                                         "/hello.txt"
                                                         download-path)))
                            download-path)
                    (equal? (read-u8vec download-path)
                            (string->utf8 "hello ftp"))))
             (lambda ()
               (when (file-exists? download-path)
                 (delete-file download-path #f))
               (stop-server)))))))

(mat net-ftp-upload-nonblocking
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [upload-path "/tmp/cspp-net-ftp-upload-nb.txt"]
               [uploaded-path (string-append root "/uploaded-nb.txt")])
           (dynamic-wind
             (lambda ()
               (when (file-exists? upload-path)
                 (delete-file upload-path #f)))
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (begin
                      (write-bytevector-file upload-path (string->utf8 "upload nonblocking"))
                      #t)
                    (equal? (wait-ftp-nonblocking
                             (lambda ()
                               (ftp-upload/nonblocking session
                                                       upload-path
                                                       "/uploaded-nb.txt")))
                            "/uploaded-nb.txt")
                    (file-regular? uploaded-path)
                    (equal? (read-u8vec uploaded-path)
                            (string->utf8 "upload nonblocking"))))
             (lambda ()
               (when (file-exists? upload-path)
                 (delete-file upload-path #f))
               (stop-server)))))))

(mat net-ftp-download-cancellation-policy
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [download-path "/tmp/cspp-net-ftp-cancelled-download.bin"])
           (dynamic-wind
             (lambda ()
               (when (file-exists? download-path)
                 (delete-file download-path #f)))
             (lambda ()
               (and (ftp-login! session "user" "pass")
                    (let ([operation
                           (ftp-download/nonblocking
                            session "/hello.txt" download-path)])
                      (and (file-exists? download-path)
                           (eq? (ftp-cancel-pending! session) session)
                           (eq? (net-operation-state operation) 'cancelled)
                           (not (file-exists? download-path))))
                    (let ([entries (ftp-list session)])
                      (and (member "hello.txt" (ftp-entry-names entries)) #t))))
             (lambda ()
               (ftp-close session)
               (when (file-exists? download-path)
                 (delete-file download-path #f))
               (stop-server)))))))

(mat net-ftp-file
     (mat-requires (curl)
       (with-test-ftp-session
        (lambda (session)
          (let ([file (ftp-open-file session "/data.bin" 'write
                                     default-transfer-policy)])
            (dynamic-wind
              void
              (lambda ()
                (and (ftp-file? file)
                     (= 4 (ftp-write file #vu8(1 2 3 4)))
                     (eq? 'write (ftp-file-direction file))
                     (string=? "/data.bin" (ftp-file-path file))))
              (lambda () (ftp-close-file file)))))))

     (mat-requires (curl)
       (with-test-ftp-session
        (lambda (session)
          (call-with-ftp-file
           session "/reuse.bin" 'write
           (lambda (file) (ftp-write-all file #vu8(9 8 7))))
          (call-with-ftp-file
           session "/reuse.bin" 'read
           (lambda (file) (equal? #vu8(9 8 7) (ftp-read-all file)))))))

     (mat-requires (curl)
       (with-test-ftp-session
        (lambda (session)
          (call-with-ftp-file
           session "/hello.txt" 'read default-transfer-policy
           (lambda (file)
             (equal? (string->utf8 "hello ftp") (ftp-read-all file)))))))

     ;; A closed transfer cannot be read.
     (mat-requires (curl)
       (with-test-ftp-session
        (lambda (session)
          (let ([file (ftp-open-file session "/hello.txt" 'read)])
            (ftp-close-file file)
            (error? (guard (failure [else failure]) (ftp-read file 1) #f))))))

     ;; A readable transfer cannot be written.
     (mat-requires (curl)
       (with-test-ftp-session
        (lambda (session)
          (let ([file (ftp-open-file session "/hello.txt" 'read)])
            (dynamic-wind
              void
              (lambda ()
                (error? (guard (failure [else failure])
                          (ftp-write file #vu8(1))
                          #f)))
              (lambda () (ftp-close-file file))))))))

(mat net-ftp-file-connection-reuse
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [connection-count-path (string-append root ".control-connections")])
           (dynamic-wind
             void
             (lambda ()
               (ftp-login! session "user" "pass")
               (call-with-ftp-file
                session "/reuse-count.bin" 'write
                (lambda (file) (ftp-write-all file #vu8(4 5 6))))
               (and (call-with-ftp-file
                     session "/reuse-count.bin" 'read
                     (lambda (file) (equal? #vu8(4 5 6) (ftp-read-all file))))
                    (= 1 (string->number
                          (utf8->string (read-u8vec connection-count-path))))))
             (lambda ()
               (ftp-close session)
               (stop-server)))))))

(mat net-ftp-transfer-policy
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [download-path "/tmp/cspp-net-ftp-resume-download.bin"]
               [upload-path "/tmp/cspp-net-ftp-resume-upload.bin"]
               [progress '()])
           (dynamic-wind
             (lambda ()
               (write-bytevector-file download-path (string->utf8 "hello"))
               (write-bytevector-file upload-path (string->utf8 "upload resumed"))
               (write-bytevector-file (string-append root "/resume-upload.bin")
                                      (string->utf8 "upload")))
             (lambda ()
               (ftp-login! session "user" "pass")
               (ftp-download
                session "/hello.txt" download-path
                (ftp-test-policy
                 'resume 'replace
                 (lambda (protocol direction path completed total)
                   (set! progress (cons (list protocol direction path completed total)
                                        progress)))))
               (ftp-upload
                session upload-path "/resume-upload.bin"
                (ftp-test-policy
                 'resume 'replace
                 (lambda (protocol direction path completed total)
                   (set! progress (cons (list protocol direction path completed total)
                                        progress)))))
               (and (equal? (read-u8vec download-path) (string->utf8 "hello ftp"))
                    (equal? (read-u8vec (string-append root "/resume-upload.bin"))
                            (string->utf8 "upload resumed"))
                    (ftp-command-seen? root "REST 5")
                    (ftp-command-seen? root "REST 6")
                    (exists (lambda (event) (eq? (cadr event) 'download)) progress)
                    (exists (lambda (event) (eq? (cadr event) 'upload)) progress)))
             (lambda ()
               (ftp-close session)
               (when (file-exists? download-path) (delete-file download-path #f))
               (when (file-exists? upload-path) (delete-file upload-path #f))
               (stop-server))))))

     (mat-requires (curl)
       (with-test-ftp-session
        (lambda (session)
          (let ([path "/tmp/cspp-net-ftp-policy.bin"])
            (dynamic-wind
              (lambda () (write-bytevector-file path (string->utf8 "keep")))
              (lambda ()
                (and
                 ;; Overwrite mode `error` rejects an existing local destination.
                 (ftp-error-message-contains?
                  "local destination exists"
                  (lambda ()
                    (ftp-download session "/hello.txt" path
                                  (ftp-test-policy 'never 'error #f))))
                 (equal? (ftp-download session "/hello.txt" path
                                       (ftp-test-policy 'never 'skip #f))
                         path)
                 (equal? (read-u8vec path) (string->utf8 "keep"))
                 (equal? (ftp-download session "/hello.txt" path
                                       (ftp-test-policy 'never 'replace #f))
                         path)
                 (equal? (read-u8vec path) (string->utf8 "hello ftp"))
                 (begin
                   (write-bytevector-file path (string->utf8 "hello"))
                   (ftp-download session "/hello.txt" path
                                 (ftp-test-policy 5 'replace #f))
                   (equal? (read-u8vec path) (string->utf8 "hello ftp")))
                 (begin
                   (write-bytevector-file path (string->utf8 "partial"))
                   ;; A failed non-resume download removes its partial destination.
                   (guard (c [else #t])
                     (ftp-download session "/missing.txt" path
                                   (ftp-test-policy 'never 'replace #f))
                     #f)
                   (not (file-exists? path)))))
              (lambda () (when (file-exists? path) (delete-file path #f)))))))))

(mat net-ftp-recursive-transfer
     (mat-requires (curl)
       (let-values ([(root port stop-server) (start-ftp-test-server)])
         (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
               [source "/tmp/cspp-net-ftp-tree-source"]
               [dest "/tmp/cspp-net-ftp-tree-dest"])
           (dynamic-wind
             (lambda ()
               (when (file-exists? source) (file-removetree source #f))
               (when (file-exists? dest) (file-removetree dest #f))
               (mkdirs (string-append source "/nested/deep"))
               (write-bytevector-file (string-append source "/top.txt") (string->utf8 "top"))
               (write-bytevector-file (string-append source "/nested/deep/data.txt")
                                      (string->utf8 "nested")))
             (lambda ()
               (ftp-login! session "user" "pass")
               (ftp-upload-directory session source "/tree"
                                     (ftp-test-policy 'never 'replace #f))
               (ftp-download-directory session "/tree" dest
                                       (ftp-test-policy 'never 'replace #f))
               (and (equal? (read-u8vec (string-append dest "/top.txt"))
                            (string->utf8 "top"))
                    (equal? (read-u8vec (string-append dest "/nested/deep/data.txt"))
                            (string->utf8 "nested"))))
             (lambda ()
               (ftp-close session)
               (when (file-exists? source) (file-removetree source #f))
               (when (file-exists? dest) (file-removetree dest #f))
               (stop-server))))))

     ;; Recursive upload rejects symbolic links instead of following them.
     (mat-requires (curl)
       (with-test-ftp-session
        (lambda (session)
          (let ([source "/tmp/cspp-net-ftp-link-source"])
            (dynamic-wind
              (lambda ()
                (when (file-exists? source) (file-removetree source #f))
                (mkdirs source)
                (file-symlink "/tmp" (string-append source "/link")))
              (lambda ()
                (ftp-error-message-contains?
                 "rejects symbolic links"
                 (lambda () (ftp-upload-directory session source "/links"))))
              (lambda () (when (file-exists? source) (file-removetree source #f)))))))))

(mat net-ftp-helper-startup-before-command-reader
     ;; Error case: reading stdin before announcing readiness deadlocks the shared console port.
     (let-values ([(input output errors pid)
                   (open-process-ports
                    "timeout 5s ../cspp --script=net-ftp-startup-process.ss /tmp/cspp-ftp-startup"
                    (buffer-mode block) (native-transcoder))])
       (dynamic-wind
         void
         (lambda ()
           (let ([port (read output)])
             (and (integer? port) (<= 1 port 65535)
                  (begin
                    (display "stop\n" input)
                    (flush-output-port input)
                    (and (eof-object? (read output))
                         (eof-object? (get-string-all errors)))))))
         (lambda ()
           (close-port input)
           (close-port output)
           (close-port errors)))))
