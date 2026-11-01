(import (cspp)
        (cspp crypto)
        (cspp crypto encoding))

(define resolve-test-path
  (lambda (repo-root-path tests-dir-path)
    (if (file-exists? repo-root-path)
        repo-root-path
        tests-dir-path)))

(define crypto-test-file
  (resolve-test-path "tests/crypto.ss" "crypto.ss"))
(define crypto-root-cert-file
  (resolve-test-path "tests/data/crypto/root.cert.pem" "data/crypto/root.cert.pem"))
(define crypto-intermediate-cert-file
  (resolve-test-path "tests/data/crypto/intermediate.cert.pem" "data/crypto/intermediate.cert.pem"))
(define crypto-leaf-cert-file
  (resolve-test-path "tests/data/crypto/leaf.cert.pem" "data/crypto/leaf.cert.pem"))
(define crypto-leaf-der-hex-file
  (resolve-test-path "tests/data/crypto/leaf.der.hex" "data/crypto/leaf.der.hex"))

(define read-file-bytevector
  (lambda (path)
    (let ([p (open-file-input-port path)])
      (dynamic-wind
        void
        (lambda ()
          (get-bytevector-all p))
        (lambda ()
          (close-port p))))))

(define password-salt
  (bytevector #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17
              #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f))

(define bv-append
  (lambda (a b)
    (let ([out (make-bytevector (+ (bytevector-length a) (bytevector-length b)) 0)])
      (bytevector-copy! a 0 out 0 (bytevector-length a))
      (bytevector-copy! b 0 out (bytevector-length a) (bytevector-length b))
      out)))

(define bv-concat*
  (lambda (bvs)
    (if (null? bvs)
        (make-bytevector 0 0)
        (fold-left bv-append (car bvs) (cdr bvs)))))

(define string-strip-whitespace
  (lambda (str)
    (list->string
     (reverse
      (let loop ([i 0] [out '()])
        (if (= i (string-length str))
            out
            (let ([ch (string-ref str i)])
              (loop (+ i 1)
                    (if (char-whitespace? ch)
                        out
                        (cons ch out))))))))))

(define string-contains?
  (lambda (haystack needle)
    (let ([n (string-length haystack)]
          [m (string-length needle)])
      (let loop ([i 0])
        (cond
         [(> (+ i m) n) #f]
         [(let check ([j 0])
            (or (= j m)
                (and (char=? (string-ref haystack (+ i j))
                             (string-ref needle j))
                     (check (+ j 1)))))
          #t]
         [else (loop (+ i 1))])))))

(define with-sparse-byte-file
  (lambda (size proc)
    (let ([path (format "crypto-sparse-~a-~a.tmp"
                        (random 9999)
                        (time-nanosecond (current-time)))])
      (dynamic-wind
        (lambda ()
          (when (file-exists? path)
            (delete-file path))
          (let ([p (open-file-output-port path
                                          (file-options replace)
                                          (buffer-mode block)
                                          #f)])
            (dynamic-wind
              void
              (lambda ()
                (set-port-position! p (fx- size 1))
                (put-u8 p 0))
              (lambda ()
                (close-port p)))))
        (lambda ()
          (proc path))
        (lambda ()
          (when (file-exists? path)
            (delete-file path)))))))

(mat crypto-encoding
     (equal? "000102ff" (bytevector->hex (bytevector 0 1 2 255)))
     (equal? (bytevector 0 1 2 255) (hex->bytevector "000102ff"))
     (error? (hex->bytevector "0"))
     (error? (hex->bytevector "zz")))

(mat crypto-random
     (mat-requires (openssl)
       (= 32 (bytevector-length (random-bytes 32))))
     (mat-requires (openssl)
       (let ([bv (make-bytevector 24 0)])
         (random-bytes! bv)
         (= 24 (bytevector-length bv))))
     (mat-requires (openssl)
       (let ([bv (make-bytevector 8 0)])
         (random-bytes! bv 2 6)
         (= 8 (bytevector-length bv))))
     (mat-requires (openssl)
       (boolean? (random-status))))

(mat crypto-constant-time
     (mat-requires (openssl)
       (constant-time-bytevector=? (bytevector 1 2 3) (bytevector 1 2 3)))
     (mat-requires (openssl)
       (not (constant-time-bytevector=? (bytevector 1 2 3) (bytevector 1 2 4))))
     (mat-requires (openssl)
       (not (constant-time-bytevector=? (bytevector 1 2 3) (bytevector 1 2))))
     (mat-requires (openssl)
       (constant-time-subbytevector=?
        (bytevector 9 1 2 3 8) 1 4
        (bytevector 1 2 3) 0 3))
     (mat-requires (openssl)
       (not (constant-time-subbytevector=?
             (bytevector 9 1 2 3 8) 1 4
             (bytevector 1 2 4) 0 3))))

(mat crypto-digest-one-shot
     (mat-requires (openssl)
       (equal? (bytevector->hex (digest 'sha256 (string->utf8 "abc")))
               "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
     (mat-requires (openssl)
       (equal? (bytevector->hex (digest-bytevector 'sha256 (string->utf8 "abc") 1 3))
               "1e0bbd6c686ba050b8eb03ffeedc64fdc9d80947fce821abbe5d6dc8d252c5ac"))
     (mat-requires (openssl)
       (equal? (bytevector->hex (digest-string 'sha3-256 "abc"))
               "43377aca8e6c7de136624d10167e4fd347a984b24fd56745894111004103da77"))
     (mat-requires (openssl)
       (let ([bv (read-file-bytevector crypto-test-file)])
         (equal? (digest-file 'sha256 crypto-test-file)
                 (digest-bytevector 'sha256 bv))))
     (mat-requires (openssl)
       (let ([bv (read-file-bytevector crypto-test-file)])
         (equal? (digest-file 'sha256 crypto-test-file 3 17)
                 (digest-bytevector 'sha256 bv 3 17))))
     (mat-requires (openssl)
       (= 32 (digest-output-size 'sha256)))
     (mat-requires (openssl)
       (= 128 (digest-block-size 'sha512)))
     ;; Error case: the bytevector slice cannot run backwards.
     (error? (digest-bytevector 'sha256 (bytevector 1 2 3) 3 1))

     ;; Error case: the file slice cannot run backwards.
     (error? (digest-file 'sha256 crypto-test-file 99 1)))

(mat crypto-digest-state
     (mat-requires (openssl)
       (= 64 (bytevector-length
               (call-with-digester 'blake2b-512
                 (lambda (state)
                   (digester-update-string! state "ab")
                   (digester-update-string! state "c"))))))

     (mat-requires (openssl)
       (string=? "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
                 (bytevector->hex
                   (call-with-digester 'sha256
                     (lambda (state)
                       (digester-update-bytevector! state (string->utf8 "abc")))))))

     (mat-requires (openssl)
       (let ([state (make-digester 'sha256)])
         (dynamic-wind
           void
           (lambda ()
             (digester-update-bytevector! state (string->utf8 "abc"))
             (and (equal? (digester-get state)
                          (digest-bytevector 'sha256 (string->utf8 "abc")))
                  (begin
                    (digester-reset! state)
                    (digester-update-bytevector! state (string->utf8 "abc"))
                    (equal? (digester-finalize! state)
                            (digest-bytevector 'sha256 (string->utf8 "abc"))))))
           (lambda () (digester-destroy! state)))))

     (mat-requires (openssl)
       (let ([state (make-digester 'sha256)])
         (digester-destroy! state)
         ;; Error case: snapshots of destroyed digesters must be rejected.
         (guard (c [else #t])
           (digester-get state)
           #f))))

(mat crypto-uint64-slices
     (mat-requires (openssl)
       (with-sparse-byte-file
        (fx+ #x80000000 1)
        (lambda (path)
          (equal? (digest-file 'sha256 path #x80000000 #x80000000)
                  (digest-bytevector 'sha256 (bytevector))))))
     (mat-requires (openssl)
       (let* ([key (bytevector 9 1 2 3 4 9)]
              [data (string->utf8 "xxpayloadyy")]
              [st (make-hmac-state 'sha256 key 1 5)])
         (hmac-update-bytevector! st data 2 9)
         (equal? (hmac-finalize! st)
                 (hmac 'sha256 (bytevector 1 2 3 4) (string->utf8 "payload")))))
     (mat-requires (openssl)
       (let* ([alg 'aes-128-ctr]
              [key (make-bytevector (cipher-key-size alg) 1)]
              [iv (make-bytevector (cipher-iv-size alg) 2)]
              [wrapped (string->utf8 "xxplainyy")]
              [st (make-cipher-state 'encrypt alg key iv)]
              [ct (bv-concat* (list (cipher-update-bytevector! st wrapped 2 7)
                                    (cipher-finalize! st)))]
              [dst (make-cipher-state 'decrypt alg key iv)]
              [pt0 (cipher-update-bytevector! dst ct)]
              [tail1 (cipher-finalize! dst)])
         (equal? (bv-concat* (list pt0 tail1))
                 (string->utf8 "plain"))))
     (mat-requires (openssl)
       (let* ([key (generate-private-key 'ed25519)]
              [pem (store-private-key key)]
              [wrapped (bv-concat* (list (bytevector 0) pem (bytevector 0)))]
              [loaded (load-private-key wrapped 'pem 1 (fx+ 1 (bytevector-length pem)))])
         (private-key=? key loaded)))
     (mat-requires (openssl)
       (let* ([pem (read-file-bytevector crypto-leaf-cert-file)]
              [wrapped (bv-concat* (list (bytevector 0) pem (bytevector 0)))]
              [cert (load-certificate wrapped 'pem 1 (fx+ 1 (bytevector-length pem)))])
         (certificate? cert))))

(mat crypto-mac
     (mat-requires (openssl)
       (equal? (bytevector->hex
                (hmac 'sha256 (bytevector 1 2 3 4) (string->utf8 "hello")))
               "b2fed3cd8fb06026f3f1b243372ec5b2aa94cf9ea64a04d3ad71ae503926749f"))
     (mat-requires (openssl)
       (hmac-verify? 'sha256 (bytevector 1 2 3 4) (string->utf8 "hello")
                     (hmac 'sha256 (bytevector 1 2 3 4) (string->utf8 "hello"))))
     (mat-requires (openssl)
       (not (hmac-verify? 'sha256 (bytevector 1 2 3 4) (string->utf8 "hello")
                          (bytevector 0 1 2 3))))
     (mat-requires (openssl)
       (let ([st (make-hmac-state 'sha256 (bytevector 1 2 3 4))])
         (hmac-update-string! st "hello")
         (and (equal? (hmac-get st)
                      (hmac 'sha256 (bytevector 1 2 3 4) "hello"))
              (begin (hmac-reset! st)
                     (hmac-update-string! st "hello")
                     (equal? (hmac-finalize! st)
                             (hmac 'sha256 (bytevector 1 2 3 4) "hello"))))))
     (mat-requires (openssl)
       (eq? 'ok
            (call-with-hmac-state 'sha256 (bytevector 1 2 3 4)
              (lambda (st)
                (hmac-update-string! st "hello")
                'ok)))))

(mat crypto-kdf
     (mat-requires (openssl)
       (equal? (bytevector->hex
                (hkdf 'sha256
                      (bytevector #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b)
                      (bytevector #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c)
                      (bytevector #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 #xf8 #xf9)
                      42))
               "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865"))
     (mat-requires (openssl)
       (= 32 (bytevector-length (hkdf-extract 'sha256 "password" "salt"))))
     (mat-requires (openssl)
       (= 17 (bytevector-length (hkdf-expand 'sha256 (hkdf-extract 'sha256 "password" "salt") "info" 17))))
     (mat-requires (openssl)
       (= 32 (bytevector-length (pbkdf2 'sha256 "password" "salt" 1000 32))))
     (mat-requires (openssl)
       (= 32 (bytevector-length (scrypt "password" "salt" 16384 8 1 32))))
     (mat-requires (openssl)
       (equal? (scrypt "password" "salt" 1024 8 16 32)
               (scrypt "password" "salt" 1024 8 16 32)))
     (mat-requires (openssl)
       (not (equal? (scrypt "password" "salt" 1024 8 16 32)
                    (scrypt "password!" "salt" 1024 8 16 32)))))

(mat crypto-aead-one-shot
     (mat-requires (openssl)
       (let ([key (make-bytevector 32 7)]
             [nonce (make-bytevector 12 9)]
             [aad (bytevector 1 2 3)]
             [pt (string->utf8 "secret payload")])
         (let-values ([(ct tag) (aead-encrypt/aad 'chacha20-poly1305 key nonce aad pt)])
           (and (bytevector? ct)
                (bytevector? tag)
                (equal? pt (aead-decrypt/aad 'chacha20-poly1305 key nonce aad ct tag))
                (not (aead-decrypt/aad 'chacha20-poly1305 key nonce aad ct (bytevector 0 1 2))))
                (= 32 (aead-key-size 'chacha20-poly1305))
                (= 12 (aead-default-nonce-size 'chacha20-poly1305))
                (= 16 (aead-tag-size 'chacha20-poly1305))))))

(mat crypto-aead-state
     (mat-requires (openssl)
       (let ([key (make-bytevector 32 7)]
             [nonce (make-bytevector 12 9)]
             [aad0 (bytevector 1 2)]
             [aad1 (bytevector 3 4)]
             [pt0 (string->utf8 "secret ")]
             [pt1 (string->utf8 "payload")])
         (let ([est (make-aead-encrypt-state 'chacha20-poly1305 key nonce)])
           (aead-update-aad-bytevector! est aad0)
           (aead-update-aad-bytevector! est aad1)
           (aead-update-plaintext-bytevector! est pt0)
           (aead-update-plaintext-bytevector! est pt1)
           (let-values ([(ct tag) (aead-finalize-encrypt! est)])
             (let ([dst (make-aead-decrypt-state 'chacha20-poly1305 key nonce)])
               (aead-update-aad-bytevector! dst aad0)
               (aead-update-aad-bytevector! dst aad1)
               (aead-update-ciphertext-bytevector! dst ct)
               (equal? (bv-append pt0 pt1)
                       (aead-finalize-decrypt! dst tag)))))))
     (eq? 'ok
          (call-with-aead-encrypt-state
           'chacha20-poly1305 (make-bytevector 32 1) (make-bytevector 12 2)
           (lambda (st)
             (aead-update-plaintext-bytevector! st (string->utf8 "x"))
             'ok))))

(mat crypto-cipher
     (mat-requires (openssl)
       (let* ([alg 'aes-256-ctr]
              [key (make-bytevector (cipher-key-size alg) 7)]
              [iv (make-bytevector (cipher-iv-size alg) 9)]
              [pt (string->utf8 "plain text through cipher")]
              [est (make-cipher-state 'encrypt alg key iv)]
              [ct (bv-concat*
                   (list (cipher-update-bytevector! est pt 0 5)
                         (cipher-update-bytevector! est pt 5 (bytevector-length pt))
                         (cipher-finalize! est)))])
         (let ([dst (make-cipher-state 'decrypt alg key iv)])
           (and (= 32 (cipher-key-size alg))
                (= 16 (cipher-iv-size alg))
                (equal? pt
                        (bv-concat*
                         (list (cipher-update-bytevector! dst ct)
                               (cipher-finalize! dst))))))))
     (mat-requires (openssl)
       (let* ([alg 'chacha20]
              [key (make-bytevector (cipher-key-size alg) 3)]
              [iv (make-bytevector (cipher-iv-size alg) 4)]
              [pt (string->utf8 "reset-me")]
              [st (make-cipher-state 'encrypt alg key iv)]
              [ct0 (bv-concat* (list (cipher-update-bytevector! st pt)
                                     (cipher-finalize! st)))])
         (cipher-reset! st)
         (equal? ct0
                 (bv-concat* (list (cipher-update-bytevector! st pt)
                                   (cipher-finalize! st))))))
     (mat-requires (openssl)
       (eq? 'ok
            (call-with-cipher-state
             'encrypt 'aes-128-ctr (make-bytevector 16 1) (make-bytevector 16 2)
             (lambda (st)
               (cipher-update-bytevector! st (string->utf8 "x"))
               'ok)))))

(mat crypto-pkey
     (mat-requires (openssl)
       (let ([k (generate-private-key 'ed25519)])
         (let* ([pub (private-key->public-key k)]
                [pem (store-private-key k 'pem)]
                [der (store-public-key pub 'der)]
                [k1 (pem->private-key pem)]
                [p1 (der->public-key der)])
           (and (private-key? k)
                (public-key? pub)
                (eq? 'ed25519 (private-key-algorithm k))
                (eq? 'ed25519 (public-key-algorithm pub))
                (private-key=? k k1)
                (public-key=? pub p1)))))
     (mat-requires (openssl)
       (let ([k (generate-private-key 'rsa 2048)])
         (and (private-key? k)
              (>= (private-key-bits k) 2048))))
     (mat-requires (openssl)
       (let ([k (generate-private-key 'ecdsa 'p-256)])
         (private-key? k))))

(mat crypto-sign
     (mat-requires (openssl)
       (let* ([priv (generate-private-key 'ed25519)]
              [pub (private-key->public-key priv)]
              [msg (string->utf8 "signed payload")]
              [sig (sign-message 'ed25519 priv msg)])
         (and (bytevector? sig)
              (verify-signature? 'ed25519 pub msg sig)
              (not (verify-signature? 'ed25519 pub (string->utf8 "other") sig)))))
     (mat-requires (openssl)
       (let* ([priv (generate-private-key 'rsa 2048)]
              [pub (private-key->public-key priv)]
              [msg (string->utf8 "rsa pss payload")]
              [sig (sign-message 'rsa-pss 'sha256 priv msg)])
         (and (verify-signature? 'rsa-pss 'sha256 pub msg sig)
              (not (verify-signature? 'rsa-pss 'sha256 pub (string->utf8 "bad") sig)))))
     (mat-requires (openssl)
       (let* ([priv (generate-private-key 'ecdsa 'p-256)]
              [pub (private-key->public-key priv)]
              [sig (sign-message 'ecdsa 'sha256 priv (string->utf8 "ecdsa payload"))])
         (verify-signature? 'ecdsa 'sha256 pub (string->utf8 "ecdsa payload") sig)))
     (mat-requires (openssl)
       (let* ([priv (generate-private-key 'ed25519)]
              [pub (private-key->public-key priv)]
              [msg0 (string->utf8 "sig-")]
              [msg1 (string->utf8 "state")]
              [sig (let ([st (make-sign-state 'ed25519 priv)])
                     (sign-update-bytevector! st msg0)
                     (sign-update-bytevector! st msg1)
                     (sign-finalize! st))])
         (let ([st (make-verify-state 'ed25519 pub)])
           (verify-update-bytevector! st msg0)
           (verify-update-bytevector! st msg1)
           (verify-finalize? st sig)))))

(mat crypto-key-agreement
     (mat-requires (openssl)
       (let* ([a (generate-private-key 'x25519)]
              [b (generate-private-key 'x25519)]
              [ap (private-key->public-key a)]
              [bp (private-key->public-key b)]
              [sa (derive-shared-secret 'x25519 a bp)]
              [sb (derive-shared-secret 'x25519 b ap)])
         (equal? sa sb)))
     (mat-requires (openssl)
       (let* ([a (generate-private-key 'ecdh 'p-256)]
              [b (generate-private-key 'ecdh 'p-256)]
              [ap (private-key->public-key a)]
              [bp (private-key->public-key b)])
         (equal? (derive-shared-secret 'ecdh a bp)
                 (derive-shared-secret 'ecdh b ap)))))

(mat crypto-password
     (mat-requires (openssl)
       (let ([encoded (password-hash "passw0rd!")])
         (and (password-hash? encoded)
              (password-verify? "passw0rd!" encoded)
              (not (password-verify? "passw0rd?" encoded)))))
     (mat-requires (openssl)
       (let ([encoded (password-hash/scrypt "passw0rd!" password-salt 1024 8 16 32)])
         (and (password-hash? encoded)
              (password-verify? "passw0rd!" encoded)
              (not (password-verify? "passw0rd?" encoded)))))
     (mat-requires (openssl)
       (let ([encoded (password-hash/pbkdf2 "passw0rd!" password-salt 1000 32)])
         (and (password-hash? encoded)
              (password-verify? "passw0rd!" encoded)
              (not (password-verify? "passw0rd?" encoded)))))
     (not (password-hash? (bytevector 1 2 3 4))))

(mat crypto-envelope
     (mat-requires (openssl)
       (let* ([recipient (generate-private-key 'x25519)]
              [recipient-pub (private-key->public-key recipient)]
              [aad (bytevector 1 2 3 4)]
              [pt (string->utf8 "enveloped payload")]
              [env (seal-envelope/aad recipient-pub aad pt)])
         (and (envelope? env)
              (equal? pt (open-envelope/aad recipient aad env))
              (not (open-envelope/aad recipient (bytevector 9 9 9) env))))))

(mat crypto-cert
     (mat-requires (openssl)
       (let* ([leaf-pem (read-file-bytevector crypto-leaf-cert-file)]
              [leaf (load-certificate leaf-pem)]
              [leaf-der (hex->bytevector
                         (string-strip-whitespace (read-string crypto-leaf-der-hex-file)))]
              [leaf-der-cert (load-certificate leaf-der 'der)]
              [sans (certificate-subject-alt-names leaf)])
         (and (certificate? leaf)
              (certificate? leaf-der-cert)
              (string-contains? (certificate-subject leaf) "CN=example.com")
              (string-contains? (certificate-issuer leaf) "ChezPP Test Intermediate CA")
              (string-contains? (certificate-not-before leaf) "T")
              (string-contains? (certificate-not-after leaf) "T")
              (equal? sans
                      '((dns . "example.com")
                        (dns . "www.example.com")
                        (ip . "127.0.0.1")))
              (certificate-hostname-matches? leaf "example.com")
              (certificate-hostname-matches? leaf "127.0.0.1")
              (not (certificate-hostname-matches? leaf "bad.example.com"))
              (public-key? (certificate-public-key leaf))
              (eq? 'rsa (public-key-algorithm (certificate-public-key leaf)))
              (< 0 (bytevector-length (certificate-serial-number leaf)))
              (= 32 (bytevector-length (certificate-fingerprint leaf))))))
     (mat-requires (openssl)
       (let* ([leaf-pem (read-file-bytevector crypto-leaf-cert-file)]
              [intermediate-pem (read-file-bytevector crypto-intermediate-cert-file)]
              [root-pem (read-file-bytevector crypto-root-cert-file)]
              [chain (load-cert-chain (bv-append leaf-pem intermediate-pem))]
              [leaf (car chain)]
              [intermediate (cadr chain)]
              [root (load-certificate root-pem)]
              [store (make-cert-store)])
         (cert-store-add! store root)
         (and (= 2 (length chain))
              (not (verify-certificate-chain leaf store))
              (verify-certificate-chain leaf (list intermediate) store)
              (verify-certificate-chain leaf (list intermediate) store "example.com")
              (not (verify-certificate-chain leaf (list intermediate) store "bad.example.com")))))
     (mat-requires (openssl)
       (let ([store (make-cert-store 'system)])
         (cert-store? store))))

(mat crypto-unsafe
     (mat-requires (openssl)
       (equal? (bytevector->hex (unsafe-hash 'md5 (string->utf8 "abc")))
               "900150983cd24fb0d6963f7d28e17f72"))
     (mat-requires (openssl)
       (equal? (bytevector->hex (unsafe-hash-bytevector 'sha1 (string->utf8 "abc")))
               "a9993e364706816aba3e25717850c26c9cd0d89d")))
