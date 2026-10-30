(import (chezscheme))

;; The hooks must precede all CSPP imports so OpenSSL has not allocated memory yet.
(load-shared-object (car (command-line-arguments)))
(unless (= 1 ((foreign-procedure "crypto_test_track_memory" () int)))
  (error 'envelope-memory "OpenSSL memory tracking initialized too late"))
(load "cspp.lib")
(import (cspp))

(define live-allocations
  (foreign-procedure "crypto_test_live_allocations" () unsigned-64))
(define clear-native-errors
  (foreign-procedure "ERR_clear_error" () void))

(define check-no-native-leak
  (lambda (name thunk)
    ;; Warm provider caches before measuring the allocations owned by each operation.
    (do ([index 0 (fx1+ index)]) [(fx= index 4)]
      (thunk)
      (clear-native-errors))
    (let ([before (live-allocations)])
      (do ([index 0 (fx1+ index)]) [(fx= index 5)]
        (thunk)
        (clear-native-errors))
      (unless (= before (live-allocations))
        (error name "native allocation count changed" before (live-allocations))))))

(define expect-derivation-error
  (lambda (thunk)
    (unless (guard (condition
                    [else (and (who-condition? condition)
                               (eq? (condition-who condition) 'derive-shared-secret))])
              (thunk)
              #f)
      (error 'envelope-memory "expected failure from a low-order X25519 public key"))))

(let* ([port (open-file-input-port "tests/data/crypto/leaf.cert.pem")]
       [valid (dynamic-wind void (lambda () (get-bytevector-all port))
                           (lambda () (close-port port)))]
       [invalid (string->utf8
                  "\n-----BEGIN CERTIFICATE-----\ninvalid\n-----END CERTIFICATE-----\n")]
       [chain (make-bytevector (fx+ (bytevector-length valid) (bytevector-length invalid)))])
  (bytevector-copy! valid 0 chain 0 (bytevector-length valid))
  (bytevector-copy! invalid 0 chain (bytevector-length valid) (bytevector-length invalid))
  ;; Error case: failing on the second PEM certificate must release the valid first one.
  (check-no-native-leak 'certificate-chain-load-error
    (lambda ()
      (unless (guard (condition [else (and (who-condition? condition)
                                           (eq? (condition-who condition) 'load-cert-chain))])
                (for-each destroy-certificate! (load-cert-chain chain))
                #f)
        (error 'certificate-chain-load-error "malformed second certificate accepted")))))

(let* ([private (generate-private-key 'x25519)]
       [public (private-key->public-key private)]
       ;; RFC 8410 X25519 SubjectPublicKeyInfo containing the low-order all-zero key.
       [zero-public (der->public-key
                      (hex->bytevector
                        (string-append "302a300506032b656e032100" (make-string 64 #\0))))]
       [envelope (seal-envelope public #vu8(1 2 3))]
       [invalid-envelope (seal-envelope public #vu8(1 2 3))])
  (dynamic-wind
    void
    (lambda ()
      (let ([encoded-zero (store-public-key zero-public 'pem)]
            [ephemeral (envelope-ephemeral-key invalid-envelope)])
        (unless (= (bytevector-length encoded-zero) (bytevector-length ephemeral))
          (error 'envelope-memory "public key fixture length differs"))
        (bytevector-copy! encoded-zero 0 ephemeral 0 (bytevector-length ephemeral)))
      (check-no-native-leak 'envelope-roundtrip
        (lambda ()
          (unless (equal? #vu8(1 2 3) (open-envelope private
                                      (seal-envelope public #vu8(1 2 3))))
            (error 'envelope-memory "roundtrip failed"))))
      ;; Error case: authentication failure must release the imported ephemeral key.
      (check-no-native-leak 'envelope-authentication-failure
        (lambda ()
          (when (open-envelope/aad private #vu8(9) envelope)
            (error 'envelope-memory "invalid associated data authenticated"))))
      ;; Error case: derivation fails after sealing has allocated its temporary key.
      (check-no-native-leak 'envelope-seal-error
        (lambda ()
          (expect-derivation-error (lambda () (seal-envelope zero-public #vu8(1 2 3))))))
      ;; Error case: derivation fails after opening has imported the ephemeral key.
      (check-no-native-leak 'envelope-open-error
        (lambda ()
          (expect-derivation-error (lambda () (open-envelope private invalid-envelope))))))
    (lambda ()
      (destroy-public-key! zero-public)
      (destroy-public-key! public)
      (destroy-private-key! private))))

;; Error case: an engine may suspend after construction but before a scope installs
;; its cleanup. Dropping that engine must release the native state without resumption.
(for-each
  (lambda (entry)
    (let ([name (car entry)] [with-state (cdr entry)])
      (check-no-native-leak name
        (lambda ()
          (do ([ticks 1 (fx1+ ticks)]) [(fx= ticks 161)]
            ((make-engine (lambda () (with-state (lambda (state) (void))))) ticks
             (lambda results (void))
             (lambda (abandoned) (void))))))))
  (list
    (cons 'abandoned-digester (lambda (proc) (call-with-digester 'sha256 proc)))
    (cons 'abandoned-hmac
      (lambda (proc) (call-with-hmac-state 'sha256 #vu8(1) proc)))
    (cons 'abandoned-cipher
      (lambda (proc)
        (call-with-cipher-state 'encrypt 'aes-128-ctr
          (make-bytevector 16 0) (make-bytevector 16 0) proc)))))
