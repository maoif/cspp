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
