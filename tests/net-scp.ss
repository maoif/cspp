(import (cspp)
        (cspp file)
        (cspp crypto)
        (cspp net))

(load "net-common.ss")
(load "net-ssh-common.ss")

(define scp-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                      (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(define wait-for-scp-operation
  (lambda (thunk)
    (let ([operation (thunk)])
      (and (net-operation? operation)
           (net-operation-wait operation)))))

(define run-net-scp-basic-test
  (lambda (remote-root home port user)
    (let ([local-root (format "/tmp/cspp-net-scp-basic-~a" port)])
      (when (file-exists? local-root)
        (file-removetree local-root #f))
      (mkdirs local-root)
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
                    (let ([scp (scp-open session)])
                      (dynamic-wind
                        void
                        (lambda ()
                          (let ([upload-path (path-build local-root "upload.txt")]
                                [download-path (path-build local-root "download.txt")]
                                [tree-path (path-build local-root "tree")]
                                [tree-out-path (path-build local-root "tree-out")])
                            (write-u8vec! upload-path (string->utf8 "scp-upload"))
                            (mkdirs (path-build tree-path "nested"))
                            (write-u8vec! (path-build tree-path "root.txt")
                                          (string->utf8 "root-data"))
                            (write-u8vec! (path-build (path-build tree-path "nested") "leaf.txt")
                                          (string->utf8 "leaf-data"))
                            (and
                             (equal? (scp-upload scp upload-path
                                                 (string-append remote-root "/uploaded.txt"))
                                     (string-append remote-root "/uploaded.txt"))
                             (equal? (scp-download scp
                                                   (string-append remote-root "/uploaded.txt")
                                                   download-path)
                                     download-path)
                             (equal? (read-u8vec download-path)
                                     (string->utf8 "scp-upload"))
                             (equal? (scp-copy-directory scp
                                                         'upload
                                                         tree-path
                                                         (string-append remote-root "/tree-remote"))
                                     (string-append remote-root "/tree-remote"))
                             (equal? (scp-copy-directory scp
                                                         'download
                                                         (string-append remote-root "/tree-remote")
                                                         tree-out-path)
                                     tree-out-path)
                             (equal? (read-u8vec (path-build tree-out-path "root.txt"))
                                     (string->utf8 "root-data"))
                             (equal? (read-u8vec (path-build (path-build tree-out-path "nested") "leaf.txt"))
                                     (string->utf8 "leaf-data")))))
                        (lambda ()
                          (scp-close scp))))))
                 (lambda ()
                   (ssh-close session)))))))
        (lambda ()
          (when (file-exists? local-root)
            (file-removetree local-root #f)))))))

(define run-net-scp-nonblocking-test
  (lambda (remote-root home port user)
    (let ([local-root (format "/tmp/cspp-net-scp-nonblocking-~a" port)])
      (when (file-exists? local-root)
        (file-removetree local-root #f))
      (mkdirs local-root)
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
                    (let ([scp (scp-open session)])
                      (dynamic-wind
                        void
                        (lambda ()
                          (let ([upload-path (path-build local-root "upload-nb.txt")]
                                [download-path (path-build local-root "download-nb.txt")]
                                [cancel-path (path-build local-root "cancel-nb.txt")]
                                [tree-path (path-build local-root "tree-nb")]
                                [tree-out-path (path-build local-root "tree-nb-out")]
                                [large-payload (make-bytevector 200000 77)])
                            (write-u8vec! upload-path large-payload)
                            (mkdirs (path-build tree-path "nested"))
                            (write-u8vec! (path-build tree-path "a.txt")
                                          (string->utf8 "A"))
                            (write-u8vec! (path-build (path-build tree-path "nested") "b.txt")
                                          (string->utf8 "B"))
                            (and
                             (equal? (wait-for-scp-operation
                                      (lambda ()
                                        (scp-upload/nonblocking
                                         scp
                                         upload-path
                                         (string-append remote-root "/uploaded-nb.txt"))))
                                     (string-append remote-root "/uploaded-nb.txt"))
                             (let ([operation
                                    (scp-download/nonblocking
                                     scp
                                     (string-append remote-root "/uploaded-nb.txt")
                                     cancel-path)])
                               (and (file-exists? cancel-path)
                                    (eq? (scp-cancel-pending! scp) scp)
                                    (eq? (net-operation-state operation) 'cancelled)
                                    (not (file-exists? cancel-path))))
                             (equal? (wait-for-scp-operation
                                      (lambda ()
                                        (scp-download/nonblocking
                                         scp
                                         (string-append remote-root "/uploaded-nb.txt")
                                         download-path)))
                                     download-path)
                             (equal? (read-u8vec download-path) large-payload)
                             (equal? (sha256-file upload-path)
                                     (sha256-file download-path))
                             (equal? (wait-for-scp-operation
                                      (lambda ()
                                        (scp-copy-directory/nonblocking
                                         scp
                                         'upload
                                         tree-path
                                         (string-append remote-root "/tree-nb-remote"))))
                                     (string-append remote-root "/tree-nb-remote"))
                             (equal? (wait-for-scp-operation
                                      (lambda ()
                                        (scp-copy-directory/nonblocking
                                         scp
                                         'download
                                         (string-append remote-root "/tree-nb-remote")
                                         tree-out-path)))
                                     tree-out-path)
                             (equal? (read-u8vec (path-build tree-out-path "a.txt"))
                                     (string->utf8 "A"))
                             (equal? (read-u8vec (path-build (path-build tree-out-path "nested") "b.txt"))
                                     (string->utf8 "B")))))
                        (lambda ()
                          (scp-close scp))))))
                 (lambda ()
                   (ssh-close session)))))))
        (lambda ()
          (when (file-exists? local-root)
            (file-removetree local-root #f)))))))

(define run-net-scp-convenience-open-test
  (lambda (remote-root home port user)
    (let ([local-root (format "/tmp/cspp-net-scp-open-~a" port)])
      (when (file-exists? local-root)
        (file-removetree local-root #f))
      (mkdirs local-root)
      (dynamic-wind
        void
        (lambda ()
          (with-env
           "HOME"
           home
           (lambda ()
             (let ([scp (scp-open "127.0.0.1" port user 'publickey #f 30000)])
               (dynamic-wind
                 void
                 (lambda ()
                   (let ([upload-path (path-build local-root "upload-open.txt")]
                         [download-path (path-build local-root "download-open.txt")])
                     (write-u8vec! upload-path (string->utf8 "scp-open"))
                     (and
                      (equal? (scp-upload scp upload-path
                                          (string-append remote-root "/uploaded-open.txt"))
                              (string-append remote-root "/uploaded-open.txt"))
                      (equal? (scp-download scp
                                            (string-append remote-root "/uploaded-open.txt")
                                            download-path)
                              download-path)
                      (equal? (read-u8vec download-path)
                              (string->utf8 "scp-open")))))
                 (lambda ()
                   (scp-close scp)))))))
        (lambda ()
          (when (file-exists? local-root)
            (file-removetree local-root #f)))))))

(define run-net-scp-closed-session-test
  (lambda (home port user)
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
              (let ([scp (scp-open session)])
                (dynamic-wind
                  void
                  (lambda ()
                    (and
                     (begin
                       (ssh-close session)
                       #t)
                     (scp-error-message-contains?
                      "SSH session is closed"
                      (lambda ()
                        (scp-download scp "/tmp/nope" "/tmp/nope")))))
                  (lambda ()
                    (scp-close scp))))))
           (lambda ()
             (ssh-close session))))))))

(mat net-scp
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (run-net-scp-basic-test remote-root home port user))
           (lambda ()
             (stop-server))))))

(mat net-scp-metadata-and-policy
     (mat-requires (ssh)
       (with-test-scp-session
        (lambda (session remote-root)
          (let ([local-path "/tmp/cspp-net-scp-policy.txt"])
            (dynamic-wind
              (lambda () (write-u8vec! local-path (string->utf8 "keep")))
              (lambda ()
                (let ([attributes (scp-stat session (string-append remote-root "/hello.txt"))])
                  (and (scp-attributes? attributes)
                       (eq? 'regular (scp-attributes-type attributes))
                       (= 10 (scp-attributes-size attributes))
                       (not (scp-stat session (string-append remote-root "/missing")))
                       ;; Overwrite mode `error` rejects an existing local target.
                       (scp-error-message-contains?
                        "local destination exists"
                        (lambda ()
                          (scp-download session (string-append remote-root "/hello.txt") local-path
                                        (make-transfer-policy 'never 'error 4096 #f))))
                       (equal? local-path
                               (scp-download session (string-append remote-root "/hello.txt")
                                             local-path
                                             (make-transfer-policy 'never 'skip 4096 #f)))
                       (equal? (read-u8vec local-path) (string->utf8 "keep"))
                       (equal? local-path
                               (scp-download session (string-append remote-root "/hello.txt")
                                             local-path
                                             (make-transfer-policy 'never 'replace 4096 #f)))
                       (equal? (read-u8vec local-path) (string->utf8 "hello sftp"))
                       ;; SCP resume is rejected when no verified restart helper exists.
                       (scp-error-message-contains?
                        "restart helper"
                        (lambda ()
                          (scp-download session (string-append remote-root "/hello.txt") local-path
                                        (make-transfer-policy 'resume 'replace 4096 #f)))))))
              (lambda ()
                (when (file-exists? local-path) (delete-file local-path #f)))))))))

(mat net-scp-nonblocking
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (run-net-scp-nonblocking-test remote-root home port user))
           (lambda ()
             (stop-server))))))

(mat net-scp-convenience-open
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (run-net-scp-convenience-open-test remote-root home port user))
           (lambda ()
             (stop-server))))))

(mat net-scp-closed-session
     (mat-requires (ssh)
       (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
         (dynamic-wind
           void
           (lambda ()
             (run-net-scp-closed-session-test home port user))
           (lambda ()
             (stop-server))))))

(mat net-scp-policy-progress
     (mat-requires (ssh)
       (with-test-scp-session
        (lambda (session remote-root)
          (let* ([local (format "/tmp/cspp-scp-progress-~a" (get-process-id))]
                 [remote (string-append remote-root "/progress")]
                 [events '()]
                 [policy (make-transfer-policy
                          'never 'replace 65536
                          (lambda (protocol direction path completed total)
                            (set! events (cons (list protocol direction path completed total)
                                               events))))])
            (dynamic-wind
              (lambda () (write-u8vec! local (make-bytevector 262144 42)))
              (lambda ()
                (scp-upload session local remote policy)
                (scp-download session remote local policy)
                (andmap
                 (lambda (direction)
                   (let ([selected (filter (lambda (event) (eq? direction (cadr event)))
                                           (reverse events))])
                     (and (pair? selected)
                          (andmap (lambda (event)
                                    (and (eq? 'scp (car event))
                                         (string=? remote (caddr event)))) selected)
                          (exists (lambda (event) (< 0 (cadddr event) 262144)) selected)
                          (equal? '(262144 262144) (cdddr (car (reverse selected)))))))
                 '(upload download)))
              (lambda () (when (file-exists? local) (delete-file local)))))))))

(mat net-scp-batch-progress-failure-keeps-completed-files
     ;; Error case: a progress callback fails during a later file on a shared channel.
     ;; Keep the completed file, remove the partial current file, and release the session.
     (mat-requires (ssh)
       (with-test-scp-session
        (lambda (session remote-root)
          (let* ([local (format "/tmp/cspp-scp-batch-cleanup-~a" (get-process-id))]
                 [source (string-append local "/source")]
                 [output (string-append local "/output")]
                 [remote (string-append remote-root "/batch-cleanup")]
                 [payload (make-bytevector 524288 53)]
                 [completed-name #f] [partial-name #f]
                 [policy
                  (make-transfer-policy
                   'never 'replace 65536
                   (lambda (protocol direction path completed total)
                     (cond
                      [(and completed-name (not (string=? completed-name (path-last path)))
                            (positive? completed) total (< completed total))
                       (set! partial-name (path-last path))
                       (error 'batch-progress "stop during a later file")]
                      [(and total (= completed total))
                       (set! completed-name (path-last path))])))])
            (dynamic-wind
              (lambda ()
                (mkdirs source)
                (for-each (lambda (name) (write-u8vec! (string-append source "/" name) payload))
                          '("one" "two" "three")))
              (lambda ()
                (scp-copy-directory session 'upload source remote)
                (and (scp-error-message-contains?
                      "stop during a later file"
                      (lambda ()
                        (scp-copy-directory session 'download remote output policy #f 'reject)))
                     completed-name partial-name
                     (equal? payload (read-u8vec (string-append output "/" completed-name)))
                     (not (file-exists? (string-append output "/" partial-name)))
                     (scp-attributes? (scp-stat session (string-append remote "/one")))))
              (lambda () (when (file-exists? local) (file-removetree local #f)))))))))

(mat net-scp-progress-can-stat-same-session
     ;; Progress hooks can inspect metadata without changing the transfer's native mode.
     (mat-requires (ssh)
       (with-test-scp-session
        (lambda (session remote-root)
          (let* ([local (format "/tmp/cspp-scp-progress-stat-~a" (get-process-id))]
                 [source (string-append local "/source")]
                 [output (string-append local "/output")]
                 [remote (string-append remote-root "/progress-stat")]
                 [known (string-append remote-root "/hello.txt")]
                 [payload (make-bytevector 524288 71)]
                 [observed '()]
                 [policy
                  (make-transfer-policy
                   'never 'replace 65536
                   (lambda (protocol direction path completed total)
                     (let ([key (list direction path (and total (= completed total)))])
                       (unless (member key observed)
                         (let ([attributes (scp-stat session known)])
                           (unless (and (scp-attributes? attributes)
                                        (eq? 'regular (scp-attributes-type attributes))
                                        (= 10 (scp-attributes-size attributes)))
                             (error 'progress-stat "metadata lookup failed")))
                         (set! observed (cons key observed))))))])
            (dynamic-wind
              (lambda ()
                (mkdirs source)
                (for-each (lambda (name)
                            (write-u8vec (string-append source "/" name) payload))
                          '("one" "two")))
              (lambda ()
                ;; Cover both individual transfers and shared-channel directory batches.
                (scp-upload session (string-append source "/one")
                            (string-append remote-root "/single") policy)
                (scp-download session (string-append remote-root "/single")
                              (string-append local "/single") policy)
                (scp-copy-directory session 'upload source remote policy #f 'reject)
                (scp-copy-directory session 'download remote output policy #f 'reject)
                (and (= 12 (length observed))
                     (equal? payload (read-u8vec (string-append local "/single")))
                     (andmap (lambda (name)
                               (equal? payload (read-u8vec (string-append output "/" name))))
                             '("one" "two"))))
              (lambda () (when (file-exists? local) (file-removetree local #f)))))))))
