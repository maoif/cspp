(import (cspp) (cspp file)
        (cspp net))

(load "net-common.ss")
(load "net-ssh-common.ss")

(define sftp-net-error-timeout?
  (lambda (thunk)
    (guard (c [else
               (and (net-error? c)
                    (or (string-contains? (net-error-message c) "timed out")
                        (string-contains? (net-error-message c) "Timeout")))])
      (thunk)
      #f)))

(define sftp-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                      (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(mat net-sftp
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (run-net-sftp-test remote-root home port user))
           (lambda ()
             (stop-server))))))

(mat net-sftp-attributes
     (mat-requires (ssh)
       (with-test-sftp-session
        (lambda (session remote-root)
          (let ([attributes (sftp-stat session remote-root)])
            (and (sftp-attributes? attributes)
                 (eq? 'directory (sftp-attributes-type attributes))
                 (natural? (sftp-attributes-permissions attributes)))))))

     (mat-requires (ssh)
       (with-test-sftp-session
        (lambda (session remote-root)
          (call-with-sftp-directory
           session remote-root
           (lambda (directory)
             (let loop ([count 0])
               (let ([entry (sftp-read-directory/nonblocking directory)])
                 (cond [(net-would-block? entry) (loop count)]
                       [(eof-object? entry) (> count 0)]
                       [else
                        (and (sftp-attributes? entry)
                             (loop (fx1+ count)))])))))))))

(mat net-sftp-path-and-metadata
     (mat-requires (ssh)
       (with-test-sftp-session
        (lambda (session remote-root)
          (sftp-cwd! session remote-root)
          (sftp-chmod! session "hello.txt" #o600)
          (sftp-utime! session "hello.txt" 1000000000 1000000001)
          (sftp-symlink! session "hello.txt" "hello.link")
          (let ([observed
                 (list (sftp-pwd session)
                       (sftp-normalize-path session "nested/../hello.txt")
                       (sftp-attributes-size (sftp-stat session "hello.txt"))
                       (fxlogand #o777
                                 (sftp-attributes-permissions (sftp-stat session "hello.txt")))
                       (sftp-attributes-modification-time (sftp-stat session "hello.txt"))
                       (sftp-readlink session "hello.link"))])
            (unless (equal? observed
                            (list remote-root (string-append remote-root "/hello.txt")
                                  10 #o600 1000000001 "hello.txt"))
              (errorf 'net-sftp-path-and-metadata "unexpected observations: ~s" observed))
            #t)))))

(mat net-sftp-recursive-transfer
     (mat-requires (ssh)
       (with-test-sftp-session
        (lambda (session remote-root)
          (let ([source "/tmp/cspp-net-sftp-tree-source"]
                [dest "/tmp/cspp-net-sftp-tree-dest"]
                [remote (string-append remote-root "/tree")])
            (dynamic-wind
              (lambda ()
                (when (file-exists? source) (file-removetree source #f))
                (when (file-exists? dest) (file-removetree dest #f))
                (mkdirs (string-append source "/nested"))
                (write-bytevector-file (string-append source "/top.txt")
                                       (string->utf8 "top"))
                (write-bytevector-file (string-append source "/nested/data.txt")
                                       (string->utf8 "nested")))
              (lambda ()
                (sftp-upload-directory session source remote default-transfer-policy #t)
                (sftp-download-directory session remote dest default-transfer-policy #t)
                (and (equal? (read-u8vec (string-append dest "/top.txt"))
                             (string->utf8 "top"))
                     (equal? (read-u8vec (string-append dest "/nested/data.txt"))
                             (string->utf8 "nested"))))
              (lambda ()
                (when (file-exists? source) (file-removetree source #f))
                (when (file-exists? dest) (file-removetree dest #f))))))))

     ;; Recursive SFTP upload rejects local symbolic links.
     (mat-requires (ssh)
       (with-test-sftp-session
        (lambda (session remote-root)
          (let ([source "/tmp/cspp-net-sftp-link-source"])
            (dynamic-wind
              (lambda ()
                (when (file-exists? source) (file-removetree source #f))
                (mkdirs source)
                (file-symlink "/tmp" (string-append source "/link")))
              (lambda ()
                (sftp-error-message-contains?
                 "rejects symbolic links"
                 (lambda ()
                   (sftp-upload-directory session source
                                          (string-append remote-root "/links")))))
              (lambda ()
                (when (file-exists? source) (file-removetree source #f)))))))))

(mat net-sftp-nonblocking
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (run-net-sftp-nonblocking-test remote-root home port user))
           (lambda ()
             (stop-server))))))

(mat net-sftp-timeout
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (run-net-sftp-timeout-test remote-root home port user sftp-net-error-timeout?))
           (lambda ()
             (stop-server))))))

(mat net-sftp-timeout-validation
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (let ([sftp (sftp-open session)])
                         (dynamic-wind
                           void
                           (lambda ()
                             (and
                              (sftp-session? sftp)
                              (let ([read-file (sftp-open-file sftp
                                                               (string-append remote-root "/hello.txt")
                                                               'read)]
                                    [write-file (sftp-open-file sftp
                                                                (string-append remote-root "/timeout-validation.txt")
                                                                '(write create truncate))]
                                    [buf (make-bytevector 4 0)]
                                    [bv (string->utf8 "x")])
                                (dynamic-wind
                                  void
                                  (lambda ()
                                    (and
                                     (sftp-error-message-contains?
                                      "timeout must be non-negative"
                                      (lambda ()
                                        (sftp-read read-file 1 -1)))
                                     (sftp-error-message-contains?
                                      "timeout must be non-negative"
                                      (lambda ()
                                        (sftp-read! read-file buf 0 1 -1)))
                                     (sftp-error-message-contains?
                                      "timeout must be non-negative"
                                      (lambda ()
                                        (sftp-write write-file bv 0 1 -1)))
                                     (sftp-error-message-contains?
                                      "timeout must be non-negative"
                                      (lambda ()
                                        (sftp-write-all write-file bv 0 1 -1)))))
                                  (lambda ()
                                    (sftp-close-file write-file)
                                    (sftp-close-file read-file))))))
                           (lambda ()
                             (sftp-close sftp))))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))

(mat net-sftp-read-size-validation
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (let ([sftp (sftp-open session)])
                         (dynamic-wind
                           void
                           (lambda ()
                             (let ([file (sftp-open-file sftp
                                                         (string-append remote-root "/hello.txt")
                                                         'read)])
                               (dynamic-wind
                                 void
                                 (lambda ()
                                   (and
                                    (sftp-error-message-contains?
                                     "size must be non-negative"
                                     (lambda ()
                                       (sftp-read file -1)))
                                    (sftp-error-message-contains?
                                     "size must be non-negative"
                                     (lambda ()
                                       (sftp-read/nonblocking file -1)))))
                                 (lambda ()
                                   (sftp-close-file file)))))
                           (lambda ()
                             (sftp-close sftp))))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))

(mat net-sftp-open-closed-ssh-session
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (begin
                         (ssh-close session)
                         #t)
                       (sftp-error-message-contains?
                        "SSH session is closed"
                        (lambda ()
                          (sftp-open session)))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))

(mat net-sftp-ops-closed-ssh-session
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (let ([sftp (sftp-open session)])
                         (dynamic-wind
                           void
                           (lambda ()
                             (and
                              (begin
                                (ssh-close session)
                                #t)
                              (sftp-error-message-contains?
                               "SSH session is closed"
                               (lambda ()
                                 (sftp-list sftp)))))
                           (lambda ()
                             (sftp-close sftp))))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))

(mat net-sftp-close-file-closed-ssh-session
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (let ([sftp (sftp-open session)])
                         (dynamic-wind
                           void
                           (lambda ()
                             (let ([file (sftp-open-file sftp
                                                         (string-append remote-root "/hello.txt")
                                                         'read)])
                               (dynamic-wind
                                 void
                                 (lambda ()
                                   (and
                                    (begin
                                      (ssh-close session)
                                      #t)
                                    (eq? (sftp-close-file file) file)
                                   (eq? (sftp-close-file file) file)))
                                 (lambda ()
                                   (sftp-close-file file)))))
                           (lambda ()
                             (sftp-close sftp))))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))

(mat net-sftp-input-port-read-closed-ssh-session
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (let ([sftp (sftp-open session)])
                         (dynamic-wind
                           void
                           (lambda ()
                             (let ([file (sftp-open-file sftp
                                                         (string-append remote-root "/hello.txt")
                                                         'read)])
                               (dynamic-wind
                                 void
                                 (lambda ()
                                   (call-with-port
                                    (open-sftp-input-port file)
                                    (lambda (ip)
                                      (and
                                       (begin
                                         (ssh-close session)
                                         #t)
                                       (sftp-error-message-contains?
                                        "SSH session is closed"
                                        (lambda ()
                                          (get-bytevector-n ip 1)))))))
                                 (lambda ()
                                   (sftp-close-file file)))))
                           (lambda ()
                             (sftp-close sftp))))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))

(mat net-sftp-output-port-write-closed-ssh-session
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (let ([sftp (sftp-open session)])
                         (dynamic-wind
                           void
                           (lambda ()
                             (let ([file (sftp-open-file sftp
                                                         (string-append remote-root "/port-write.txt")
                                                         '(write create truncate))])
                               (dynamic-wind
                                 void
                                 (lambda ()
                                   (let ([op (open-sftp-output-port file)])
                                     (dynamic-wind
                                       void
                                       (lambda ()
                                         (and
                                          (begin
                                            (ssh-close session)
                                            #t)
                                          (sftp-error-message-contains?
                                           "SSH session is closed"
                                           (lambda ()
                                             (put-bytevector op (string->utf8 "x"))
                                             (flush-output-port op)
                                             (close-port op)))))
                                       (lambda ()
                                         (unless (port-closed? op)
                                           (guard (c [else #f])
                                             (close-port op)))))))
                                 (lambda ()
                                   (sftp-close-file file)))))
                           (lambda ()
                             (sftp-close sftp))))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))

(mat net-sftp-port-ops-closed-file
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (with-env
              "HOME"
              home
              (lambda ()
                (let ([session (ssh-open "127.0.0.1" port user)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (and
                       (eq? (ssh-auth-publickey! session user) session)
                       (let ([sftp (sftp-open session)])
                         (dynamic-wind
                           void
                           (lambda ()
                             (and
                              (let ([file (sftp-open-file sftp
                                                          (string-append remote-root "/hello.txt")
                                                          'read)])
                                (dynamic-wind
                                  void
                                  (lambda ()
                                    (call-with-port
                                     (open-sftp-input-port file)
                                     (lambda (ip)
                                       (and
                                        (begin
                                          (sftp-close-file file)
                                          #t)
                                        (sftp-error-message-contains?
                                         "SFTP file is closed"
                                         (lambda ()
                                           (get-bytevector-n ip 1)))))))
                                  (lambda ()
                                    (sftp-close-file file))))
                              (let ([file (sftp-open-file sftp
                                                          (string-append remote-root "/port-write-closed-file.txt")
                                                          '(write create truncate))])
                                (dynamic-wind
                                  void
                                  (lambda ()
                                    (let ([op (open-sftp-output-port file)])
                                      (dynamic-wind
                                        void
                                        (lambda ()
                                          (and
                                           (begin
                                             (sftp-close-file file)
                                             #t)
                                           (sftp-error-message-contains?
                                            "SFTP file is closed"
                                            (lambda ()
                                              (put-bytevector op (string->utf8 "x"))
                                              (flush-output-port op)
                                              (close-port op)))))
                                        (lambda ()
                                          (unless (port-closed? op)
                                            (guard (c [else #f])
                                              (close-port op)))))))
                                  (lambda ()
                                    (sftp-close-file file))))))
                           (lambda ()
                             (sftp-close sftp))))))
                    (lambda ()
                      (ssh-close session)))))))
           (lambda ()
             (stop-server))))))
