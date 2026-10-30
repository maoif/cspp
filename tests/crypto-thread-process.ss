(import (chezscheme) (cspp crypto) (cspp optional-library))

(define run-workers
  (lambda (workers proc)
    (let ([mutex (make-mutex)] [ready (make-condition)] [done (make-condition)]
          [started 0] [finished 0] [go? #f] [errors '()])
      (do ([index 0 (fx1+ index)]) [(fx= index workers)]
        (fork-thread
          (lambda ()
            (with-mutex mutex
              (set! started (fx1+ started))
              (condition-broadcast ready)
              (let loop () (unless go? (condition-wait ready mutex) (loop))))
            (guard (condition [else (with-mutex mutex (set! errors (cons condition errors)))])
              (proc))
            (with-mutex mutex
              (set! finished (fx1+ finished))
              (condition-broadcast done)))))
      (with-mutex mutex
        (let loop () (unless (fx= started workers) (condition-wait ready mutex) (loop)))
        (set! go? #t)
        (condition-broadcast ready)
        (let loop () (unless (fx= finished workers) (condition-wait done mutex) (loop))))
      (unless (null? errors) (error 'crypto-threads "worker raised" errors)))))

(define repeat-update
  (lambda (update! state bytes count)
    (do ([index 0 (fx1+ index)]) [(fx= index count)] (update! state bytes))))

(define run-actions
  (lambda (actions)
    (let ([mutex (make-mutex)] [remaining actions])
      (run-workers (length actions)
        (lambda ()
          (let ([action (with-mutex mutex
                          (let ([action (car remaining)])
                            (set! remaining (cdr remaining))
                            action))])
            (action)))))))

(define allow-terminal-error
  (lambda (proc)
    (guard (condition
             [(and (message-condition? condition)
                   (member (condition-message condition)
                     '("digester is destroyed" "digester is finalized"
                       "HMAC state is destroyed" "HMAC state is finalized"
                       "cipher state is destroyed" "cipher state is finalized"
                       "private key is destroyed" "public key is destroyed" "key is destroyed"
                       "certificate is destroyed" "certificate store is destroyed")))
              (void)])
      (proc))))

(define check-state-lifetime-races
  (lambda (make-state update! get reset! finalize! destroy! bytes expected)
    (let ([state (make-state)] [results-mutex (make-mutex)] [winners 0])
      (dynamic-wind
        void
        (lambda ()
          (update! state bytes)
          ;; Error case: exactly one racing finalizer may complete and release the state.
          (run-workers 4
            (lambda ()
              (allow-terminal-error
                (lambda ()
                  (unless (equal? expected (finalize! state))
                    (error 'crypto-threads "racing finalizer returned a wrong result"))
                  (with-mutex results-mutex (set! winners (fx1+ winners)))))))
          (unless (fx= winners 1) (error 'crypto-threads "finalization was not exclusive")))
        (lambda () (destroy! state))))
    ;; Error case: use and reset racing with destruction must finish or reject the state.
    (do ([round 0 (fx1+ round)]) [(fx= round 30)]
      (let ([state (make-state)])
        (dynamic-wind
          void
          (lambda ()
            (run-actions
              (list
                (lambda () (allow-terminal-error
                             (lambda () (repeat-update update! state bytes 16))))
                (lambda () (allow-terminal-error
                             (lambda ()
                               (do ([index 0 (fx1+ index)]) [(fx= index 16)] (get state)))))
                (lambda () (allow-terminal-error (lambda () (reset! state))))
                (lambda () (allow-terminal-error (lambda () (finalize! state))))
                (lambda () (destroy! state)))))
          (lambda () (destroy! state)))))))

(define check-shared-state
  (lambda (make-state update! get destroy! bytes)
    (let ([serial (make-state)] [parallel (make-state)])
      (dynamic-wind
        void
        (lambda ()
          (repeat-update update! serial bytes 512)
          (run-workers 4 (lambda () (repeat-update update! parallel bytes 128)))
          ;; Error case: identical concurrent chunks must not be lost or corrupt native state.
          (unless (equal? (get serial) (get parallel))
            (error 'crypto-threads "concurrent updates changed the result")))
        (lambda () (destroy! serial) (destroy! parallel))))))

(define check-shared-destroy
  (lambda (make-state destroy!)
    (let ([states (map (lambda (index) (make-state)) (iota 2000))])
      ;; Error case: concurrent idempotent destruction must never free a native handle twice.
      (run-workers 4 (lambda () (for-each destroy! states))))))

(let ([mode (car (command-line-arguments))] [bytes (make-bytevector 65536 97)])
  (cond
    [(string=? mode "sha256")
     (when (optional-library-available? (optional-library-info 'openssl))
       (check-shared-state make-digester digester-update-bytevector! digester-get
                           digester-destroy! bytes)
       (check-shared-destroy make-digester digester-destroy!))]
    [(string=? mode "blake3")
     (when (optional-library-available? (optional-library-info 'blake3))
       (let ([make-state (lambda () (make-digester 'blake3))])
         (check-shared-state make-state digester-update-bytevector! digester-get
                             digester-destroy! bytes)
         (check-shared-destroy make-state digester-destroy!)))]
    [(string=? mode "hmac")
     (when (optional-library-available? (optional-library-info 'openssl))
       (let ([make-state (lambda () (make-hmac-state 'sha256 #vu8(1 2 3)))])
         (check-shared-state make-state hmac-update-bytevector! hmac-get hmac-destroy! bytes)
         (check-shared-destroy make-state hmac-destroy!)))]
    [(string=? mode "buffered")
     (when (optional-library-available? (optional-library-info 'openssl))
       (let* ([key (make-bytevector 32 7)] [nonce (make-bytevector 12 9)]
              [chunk (make-bytevector 1024 97)] [message (make-bytevector (* 512 1024) 97)])
         (call-with-aead-encrypt-state 'chacha20-poly1305 key nonce
           (lambda (state)
             (run-workers 4
               (lambda () (repeat-update aead-update-plaintext-bytevector! state chunk 128)))
             (let-values ([(ciphertext tag) (aead-finalize-encrypt! state)])
               (unless (equal? message
                         (aead-decrypt 'chacha20-poly1305 key nonce ciphertext tag))
                 (error 'crypto-threads "buffered AEAD lost concurrent input")))))
         (let* ([private (generate-private-key 'ed25519)]
                [public (private-key->public-key private)])
           (dynamic-wind
             void
             (lambda ()
               (call-with-sign-state 'ed25519 private
                 (lambda (state)
                   (run-workers 4
                     (lambda () (repeat-update sign-update-bytevector! state chunk 128)))
                   (unless (verify-signature? 'ed25519 public message (sign-finalize! state))
                     (error 'crypto-threads "buffered signing lost concurrent input")))))
             (lambda () (destroy-public-key! public) (destroy-private-key! private))))))]
    [(string=? mode "cipher")
     (when (optional-library-available? (optional-library-info 'openssl))
       (let* ([make-state (lambda ()
                            (make-cipher-state 'encrypt 'aes-256-ctr
                                               (make-bytevector 32 7) (make-bytevector 16 9)))]
              [serial (make-state)] [parallel (make-state)]
              [mutex (make-mutex)] [chunks '()] [input (make-bytevector 1024 0)])
         (dynamic-wind
           void
           (lambda ()
             (let ([expected (map (lambda (index)
                                    (bytevector->hex (cipher-update-bytevector! serial input)))
                                  (iota 512))])
               (run-workers 4
                 (lambda ()
                   (do ([index 0 (fx1+ index)]) [(fx= index 128)]
                     (let ([chunk (bytevector->hex (cipher-update-bytevector! parallel input))])
                       (with-mutex mutex (set! chunks (cons chunk chunks)))))))
               (unless (equal? (list-sort string<? expected) (list-sort string<? chunks))
                 (error 'crypto-threads "cipher chunks changed under concurrent updates"))))
           (lambda () (cipher-destroy! serial) (cipher-destroy! parallel)))
         (check-shared-destroy make-state cipher-destroy!)))]
    [(string=? mode "resources")
     (when (optional-library-available? (optional-library-info 'openssl))
       (check-shared-destroy (lambda () (generate-private-key 'x25519)) destroy-private-key!)
       (let ([private (generate-private-key 'ed25519)])
         (dynamic-wind
           void
           (lambda ()
             (check-shared-destroy (lambda () (private-key->public-key private))
                                   destroy-public-key!))
           (lambda () (destroy-private-key! private))))
       (check-shared-destroy make-cert-store destroy-cert-store!)
       (let* ([port (open-file-input-port "tests/data/crypto/leaf.cert.pem")]
              [pem (dynamic-wind void (lambda () (get-bytevector-all port))
                                 (lambda () (close-port port)))])
         (check-shared-destroy (lambda () (load-certificate pem)) destroy-certificate!))
       (let* ([left (generate-private-key 'ed25519)]
              [right (pem->private-key (store-private-key left))])
         (dynamic-wind
           void
           (lambda ()
             ;; Reversed argument order and duplicate resources must not deadlock.
             (run-workers 4
               (lambda ()
                 (do ([index 0 (fx1+ index)]) [(fx= index 100)]
                   (unless (and (private-key=? left right)
                                (private-key=? right left)
                                (private-key=? left left))
                     (error 'crypto-threads "concurrent key equality changed"))))))
           (lambda () (destroy-private-key! left) (destroy-private-key! right)))))]
    [(string=? mode "rsa-parameters")
     (when (optional-library-available? (optional-library-info 'openssl))
       (for-each
         (lambda (bits)
           (unless (guard (condition [else #t])
                     (destroy-private-key! (generate-private-key 'rsa bits))
                     #f)
             (error 'crypto-parameters "invalid RSA count accepted" bits)))
         '(4294967808 512.0 0 -1)))]
    [(string=? mode "state-lifetimes")
     (for-each
       (lambda (entry)
         (when (optional-library-available? (optional-library-info (car entry)))
           (let* ([algorithm (cadr entry)]
                  [make-state (lambda () (make-digester algorithm))])
             (check-state-lifetime-races make-state digester-update-bytevector! digester-get
               digester-reset! digester-finalize! digester-destroy! bytes
               (digest-bytevector algorithm bytes)))))
       '((openssl sha256) (blake3 blake3)))
     (when (optional-library-available? (optional-library-info 'openssl))
       (check-state-lifetime-races (lambda () (make-hmac-state 'sha256 #vu8(1 2 3)))
         hmac-update-bytevector! hmac-get hmac-reset! hmac-finalize! hmac-destroy! bytes
         (hmac 'sha256 #vu8(1 2 3) bytes)))]
    [(string=? mode "resource-lifetimes")
     (when (optional-library-available? (optional-library-info 'openssl))
       (do ([round 0 (fx1+ round)]) [(fx= round 20)]
         (let* ([private (generate-private-key 'ed25519)]
                [public (private-key->public-key private)])
           (dynamic-wind
             void
             (lambda ()
               ;; Error case: native key use racing with destruction must not use freed memory.
               (run-actions
                 (list
                   (lambda () (allow-terminal-error
                                (lambda ()
                                  (let ([signature (sign-message 'ed25519 private bytes)])
                                    (unless (verify-signature? 'ed25519 public bytes signature)
                                      (error 'crypto-threads "racing signer returned bad signature"))))))
                   (lambda () (allow-terminal-error
                                (lambda () (store-private-key private))))
                   (lambda () (allow-terminal-error
                                (lambda () (destroy-public-key!
                                             (private-key->public-key private)))))
                   (lambda () (destroy-private-key! private)))))
             (lambda () (destroy-public-key! public) (destroy-private-key! private)))))
       (let ([read-cert (lambda (name)
                          (let ([port (open-file-input-port
                                        (string-append "tests/data/crypto/" name))])
                            (dynamic-wind
                              void
                              (lambda () (load-certificate (get-bytevector-all port)))
                              (lambda () (close-port port)))))])
         (do ([round 0 (fx1+ round)]) [(fx= round 20)]
           (let* ([leaf (read-cert "leaf.cert.pem")]
                  [intermediate (read-cert "intermediate.cert.pem")]
                  [root (read-cert "root.cert.pem")] [store (make-cert-store)])
             (dynamic-wind
               (lambda () (cert-store-add! store root))
               (lambda ()
                 ;; Error case: verification must retain chain/store resources while in use.
                 (run-actions
                   (list
                     (lambda () (allow-terminal-error
                                  (lambda ()
                                    (unless (verify-certificate-chain
                                              leaf (list intermediate) store "example.com")
                                      (error 'crypto-threads "racing verification failed")))))
                     (lambda () (allow-terminal-error (lambda () (cert-store-add! store root))))
                     (lambda () (allow-terminal-error (lambda () (certificate-subject leaf))))
                     (lambda ()
                       (destroy-certificate! intermediate)
                       (destroy-certificate! leaf)
                       (destroy-cert-store! store)))))
               (lambda ()
                 (destroy-cert-store! store)
                 (for-each destroy-certificate! (list leaf root intermediate))))))))]
    [else (error 'crypto-threads "unknown fixture mode" mode)]))
