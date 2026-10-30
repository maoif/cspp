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
    [else (error 'crypto-threads "unknown fixture mode" mode)]))
