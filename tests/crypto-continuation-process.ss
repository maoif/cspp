(import (chezscheme) (cspp crypto) (cspp optional-library))

(define openssl? (optional-library-available? (optional-library-info 'openssl)))
(define blake3? (optional-library-available? (optional-library-info 'blake3)))

(define allow-destroyed-error
  (lambda (proc)
    (guard (condition
             [(and (message-condition? condition)
                   (member (condition-message condition)
                     '("digester is destroyed" "HMAC state is destroyed"
                       "cipher state is destroyed" "private key is destroyed"
                       "public key is destroyed" "key is destroyed")))
              (void)])
      (proc))))

(define resume-engine
  (lambda (engine complete)
    (engine 1000000 complete
      (lambda (next) (error 'crypto-continuations "resumption exhausted fuel")))))

(define check-state-resumption
  (lambda (create destroy! operations unchanged?)
    (for-each
      (lambda (operate)
        ;; Error case: a suspended operation must not retain a pointer to freed state,
        ;; mutate a replacement allocation, or free the original allocation twice.
        (do ([ticks 1 (fx1+ ticks)]) [(fx= ticks 129)]
          (let ([state (create)] [replacement #f])
            (dynamic-wind
              void
              (lambda ()
                (allow-destroyed-error
                  (lambda ()
                    ((make-engine (lambda () (operate state))) ticks
                     (lambda results (void))
                     (lambda (engine)
                       (destroy! state)
                       (set! replacement (create))
                       (resume-engine engine (lambda results (void)))))))
                (when (and replacement (not (unchanged? replacement)))
                  (error 'crypto-continuations "operation modified replacement state")))
              (lambda ()
                (destroy! state)
                (when replacement (destroy! replacement)))))))
      operations)))

(define read-fixture
  (lambda (name)
    (let ([port (open-file-input-port (string-append "tests/data/crypto/" name))])
      (dynamic-wind void (lambda () (get-bytevector-all port))
                    (lambda () (close-port port))))))

(define touch-state
  (lambda (state)
    (cond
      [(digester? state) (digester-get state)]
      [(hmac-state? state) (hmac-get state)]
      [(cipher-state? state) (cipher-update-bytevector! state #vu8())]
      [(aead-state? state) (aead-update-aad-bytevector! state #vu8())]
      [(sign-state? state) (sign-update-bytevector! state #vu8())]
      [(verify-state? state) (verify-update-bytevector! state #vu8())])))

(let ([mode (car (command-line-arguments))])
  (cond
    [(string=? mode "digests")
     (for-each
       (lambda (entry)
         (when (cdr entry)
           (let ([algorithm (car entry)]
                 [empty (hex->bytevector
                          (if (eq? (car entry) 'sha256)
                              "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                              "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262"))])
             (check-state-resumption
               (lambda () (make-digester algorithm)) digester-destroy!
               (list (lambda (state) (digester-update-bytevector! state #vu8(1 2 3)))
                     digester-get digester-reset! digester-finalize! digester-destroy!)
               (lambda (state) (equal? empty (digester-get state)))))))
       (list (cons 'sha256 openssl?) (cons 'blake3 blake3?)))]
    [(string=? mode "mac-cipher")
     (when openssl?
       (check-state-resumption
         (lambda () (make-hmac-state 'sha256 #vu8(1 2 3))) hmac-destroy!
         (list (lambda (state) (hmac-update-bytevector! state #vu8(1 2 3)))
               hmac-get hmac-reset! hmac-finalize! hmac-destroy!)
         (lambda (state)
           (equal? (hex->bytevector
                     "368ac05cd81d972174a46fd54daa74ed0e808c09b0cc86f0c3fc342bef66f405")
                   (hmac-get state))))
       (check-state-resumption
         (lambda () (make-cipher-state 'encrypt 'aes-128-ctr
                      (make-bytevector 16 0) (make-bytevector 16 0)))
         cipher-destroy!
         (list (lambda (state) (cipher-update-bytevector! state #vu8(1 2 3)))
               cipher-reset! cipher-finalize! cipher-destroy!)
         (lambda (state)
           (equal? (hex->bytevector "66e94bd4ef8a2c3b884cfa59ca342b2e")
                   (cipher-update-bytevector! state (make-bytevector 16 0))))))]
    [(string=? mode "certificates")
     (when openssl?
       (let* ([certificate (load-certificate (read-fixture "root.cert.pem"))]
              [store (make-cert-store)])
         (dynamic-wind
           (lambda () (cert-store-add! store certificate))
           (lambda ()
             ;; Error case: timer suspension must not free the temporary verification
             ;; context and then resume verification or cleanup using that pointer.
             (do ([ticks 1 (fx1+ ticks)]) [(fx= ticks 161)]
               ((make-engine (lambda () (verify-certificate-chain certificate store))) ticks
                (lambda (fuel result)
                  (unless result (error 'crypto-continuations "trusted certificate rejected")))
                (lambda (engine)
                  (resume-engine engine
                    (lambda (fuel result)
                      (unless result
                        (error 'crypto-continuations "resumed verification failed"))))))))
           (lambda () (destroy-cert-store! store) (destroy-certificate! certificate)))))]
    [(string=? mode "callbacks")
     (when openssl?
       (let* ([private (generate-private-key 'ed25519)]
              [public (private-key->public-key private)]
              [scopes
               (list
                 (lambda (proc) (call-with-digester 'sha256 proc))
                 (lambda (proc) (call-with-hmac-state 'sha256 #vu8(1) proc))
                 (lambda (proc)
                   (call-with-cipher-state 'encrypt 'aes-128-ctr
                     (make-bytevector 16 0) (make-bytevector 16 0) proc))
                 (lambda (proc)
                   (call-with-aead-encrypt-state 'aes-128-gcm
                     (make-bytevector 16 0) (make-bytevector 12 0) proc))
                 (lambda (proc)
                   (call-with-aead-decrypt-state 'aes-128-gcm
                     (make-bytevector 16 0) (make-bytevector 12 0) proc))
                 (lambda (proc) (call-with-sign-state 'ed25519 private proc))
                 (lambda (proc) (call-with-verify-state 'ed25519 public proc)))])
         (dynamic-wind
           void
           (lambda ()
             (for-each
               (lambda (with-state)
                 (let ([resume #f] [attempt 0] [resumed? #f])
                   ;; Error case: callback escape destroys its state. Reentry must
                   ;; either reject the scope or reject subsequent state operations.
                   (let ([result
                          (guard (condition [else 'blocked])
                            ;; Capture both exit and resumption to test resource winders.
                            (call/cc
                              (lambda (escape)
                                (with-state
                                  (lambda (state)
                                    (unless (= 1 (disable-interrupts))
                                      (error 'crypto-continuations "callback interrupts disabled"))
                                    (enable-interrupts)
                                    ;; A multi-shot callback continuation outlives its scope.
                                    (call/cc (lambda (next) (set! resume next) (escape 'escaped)))
                                    (set! resumed? #t)
                                    (touch-state state))))))])
                     (cond
                       [(fx= attempt 0)
                        (unless (eq? result 'escaped)
                          (error 'crypto-continuations "callback did not escape"))
                        (set! attempt 1)
                        (resume (void))]
                       [else
                        (unless (and (eq? result 'blocked)
                                     (or (not (eq? with-state (car scopes))) (not resumed?)))
                          (error 'crypto-continuations "escaped state remained usable"))]))))
               scopes)
             (for-each
               (lambda (with-state)
                 (unless (equal? '(one two)
                           (call-with-values
                             (lambda () (with-state (lambda (state) (values 'one 'two))))
                             list))
                   (error 'crypto-continuations "callback values changed")))
               (cdr scopes))
             (unless (eq? 'resumed
                       (with-exception-handler (lambda (condition) 'resumed)
                         (lambda ()
                           (call-with-hmac-state 'sha256 #vu8(1)
                             (lambda (state)
                               (let ([result (raise-continuable 'callback-condition)])
                                 (hmac-update-bytevector! state #vu8(2))
                                 result))))))
               (error 'crypto-continuations "continuable callback exception changed")))
           (lambda () (destroy-public-key! public) (destroy-private-key! private)))))]
    [(string=? mode "error-continuations")
     (when openssl?
       (let ([state (make-hmac-state 'sha256 #vu8(1))] [resume #f] [attempt 0])
         ;; Error case: Chez error conditions expose a continuation past validation;
         ;; invoking it after cleanup must not resume a native operation.
         (let ([result
                (guard (condition
                         [(and (who-condition? condition)
                               (eq? (condition-who condition) 'with-crypto-operation))
                          'blocked]
                         [else
                          (let ([count (disable-interrupts)])
                            (enable-interrupts)
                            (unless (fx= count 1)
                              (error 'crypto-continuations "error handler interrupts disabled")))
                          (when (continuation-condition? condition)
                            (set! resume (condition-continuation condition)))
                          'initial-error])
                  (hmac-update-bytevector! state #vu8(1 2 3) 3 1)
                  'resumed)])
           (cond
             [(fx= attempt 0)
              (set! attempt 1)
              (unless resume (error 'crypto-continuations "error lacks continuation"))
              (hmac-destroy! state)
              (resume (void))]
             [else
              (unless (eq? result 'blocked)
                (error 'crypto-continuations "reentered failed native operation"))]))))]
    [else (error 'crypto-continuations "unknown fixture mode" mode)]))
