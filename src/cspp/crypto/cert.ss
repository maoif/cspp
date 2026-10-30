(library (cspp crypto cert)
  (export load-certificate
          load-cert-chain
          certificate?
          destroy-certificate!
          certificate-subject
          certificate-issuer
          certificate-not-before
          certificate-not-after
          certificate-subject-alt-names
          certificate-hostname-matches?
          certificate-public-key
          certificate-serial-number
          certificate-fingerprint
          make-cert-store
          cert-store?
          destroy-cert-store!
          cert-store-add!
          cert-store-load-system-defaults!
          verify-certificate-chain)
  (import (cspp chez)
          (cspp utils)
          (cspp crypto ffi)
          (cspp crypto private)
          (cspp crypto pkey))

  (define cert-formats '(pem der))

  (define check-cert-format
    (lambda (who fmt)
      (unless (memq fmt cert-formats)
        (errorf who "valid certificate format is one of ~a" cert-formats))))

  (define certificate? crypto-certificate?)
  (define cert-store? crypto-cert-store?)

  (define check-certificate-live
    (lambda (who cert)
      (when (crypto-certificate-destroyed? cert)
        (errorf who "certificate is destroyed"))))

  (define check-cert-store-live
    (lambda (who store)
      (when (crypto-cert-store-destroyed? store)
        (errorf who "certificate store is destroyed"))))

  (define check-hostname
    (lambda (who hostname)
      (when (fx= (string-length hostname) 0)
        (errorf who "hostname must not be empty"))
      (let loop ([index 0])
        (unless (fx= index (string-length hostname))
          (when (char=? (string-ref hostname index) #\nul)
            (errorf who "hostname must not contain NUL characters"))
          (loop (fx1+ index))))))

  (define raw->certificate
    (lambda (who handle)
      (when (eq? handle 0)
        (errorf who "failed to load certificate"))
      (make-crypto-certificate handle #f)))

  (define load-certificate/raw
    (lambda (who bv start stop fmt)
      (check-cert-format who fmt)
      (let ([handle (case fmt
                      [(pem) (ffi-cert-load-pem bv start stop)]
                      [(der) (ffi-cert-load-der bv start stop)]
                      [else (unreachable!)])])
        (raw->certificate who handle))))

  (define string-find
    (lambda (str needle start)
      (let ([n (string-length str)]
            [m (string-length needle)])
        (let loop ([i start])
          (cond
           [(fx> (fx+ i m) n) #f]
           [(let check ([j 0])
              (or (fx= j m)
                  (and (char=? (string-ref str (fx+ i j))
                               (string-ref needle j))
                       (check (fx1+ j)))))
            i]
           [else (loop (fx1+ i))])))))

  (define split-pem-certificates
    (lambda (bv)
      (define begin-marker "-----BEGIN CERTIFICATE-----")
      (define end-marker "-----END CERTIFICATE-----")
      (let ([text (utf8->string bv)])
        (let loop ([start 0] [out '()])
          (let ([begin (string-find text begin-marker start)])
            (if (not begin)
                (reverse out)
                (let ([end (string-find text end-marker begin)])
                  (unless end
                    (errorf 'load-cert-chain "unterminated PEM certificate block"))
                  (let* ([block-end (fx+ end (string-length end-marker))]
                         [next (let scan ([i block-end])
                                 (if (and (fx< i (string-length text))
                                          (or (char=? (string-ref text i) #\newline)
                                              (char=? (string-ref text i) #\return)))
                                     (scan (fx1+ i))
                                     i))]
                         [block (substring text begin next)])
                    (loop next (cons (string->utf8 block) out))))))))))

  (define check-certificate-chain
    (lambda (who chain)
      (unless (list? chain)
        (errorf who "certificate chain must be a list, given ~s" chain))
      (for-each
       (lambda (cert)
         (unless (certificate? cert)
           (errorf who "certificate chain element is not a certificate: ~s" cert)))
       chain)))

  #|proc:load-certificate
The `load-certificate` procedure loads a single certificate from a PEM or DER bytevector slice.
|#
  (define-who load-certificate
    (case-lambda
      [(bv) (load-certificate bv 'pem 0 (bytevector-length bv))]
      [(bv fmt)
       (pcheck ([bytevector? bv])
               (load-certificate bv fmt 0 (bytevector-length bv)))]
      [(bv fmt start)
       (pcheck ([bytevector? bv])
               (load-certificate bv fmt start (bytevector-length bv)))]
      [(bv fmt start stop)
       (pcheck ([bytevector? bv])
               (check-slice who (bytevector-length bv) start stop)
               (load-certificate/raw who bv start stop fmt))]))

  #|proc:load-cert-chain
The `load-cert-chain` procedure loads a list of certificates from PEM or DER data.
|#
  (define-who load-cert-chain
    (case-lambda
      [(bv) (load-cert-chain bv 'pem 0 (bytevector-length bv))]
      [(bv fmt)
       (pcheck ([bytevector? bv])
               (load-cert-chain bv fmt 0 (bytevector-length bv)))]
      [(bv fmt start)
       (pcheck ([bytevector? bv])
               (load-cert-chain bv fmt start (bytevector-length bv)))]
      [(bv fmt start stop)
       (pcheck ([bytevector? bv])
               (check-slice who (bytevector-length bv) start stop)
               (check-cert-format who fmt)
               (let ([slice (subbytevector bv start stop)])
                 (case fmt
                   [(der) (list (load-certificate/raw who slice 0 (bytevector-length slice) 'der))]
                   [(pem)
                    (let ([blocks (split-pem-certificates slice)])
                      (when (null? blocks)
                        (errorf who "no PEM certificates found"))
                      (map (lambda (block)
                             (load-certificate/raw who block 0 (bytevector-length block) 'pem))
                           blocks))]
                   [else (unreachable!)])))]))

  #|proc:destroy-certificate!
The `destroy-certificate!` procedure releases foreign resources owned by `cert`.
|#
  (define-who destroy-certificate!
    (lambda (cert)
      (pcheck ([certificate? cert])
              (unless (crypto-certificate-destroyed? cert)
                (ffi-cert-free (crypto-certificate-handle cert))
                (crypto-certificate-handle-set! cert 0)
                (crypto-certificate-destroyed?-set! cert #t))
              cert)))

  #|proc:certificate-subject
The `certificate-subject` procedure returns the subject distinguished name as a string.
|#
  (define-who certificate-subject
    (lambda (cert)
      (pcheck ([certificate? cert])
              (check-certificate-live who cert)
              (let ([ans (ffi-cert-subject (crypto-certificate-handle cert))])
                (when (eq? ans #f)
                  (errorf who "failed to read certificate subject"))
                ans))))

  #|proc:certificate-issuer
The `certificate-issuer` procedure returns the issuer distinguished name as a string.
|#
  (define-who certificate-issuer
    (lambda (cert)
      (pcheck ([certificate? cert])
              (check-certificate-live who cert)
              (let ([ans (ffi-cert-issuer (crypto-certificate-handle cert))])
                (when (eq? ans #f)
                  (errorf who "failed to read certificate issuer"))
                ans))))

  #|proc:certificate-not-before
The `certificate-not-before` procedure returns the UTC start time as an ISO-8601 string.
|#
  (define-who certificate-not-before
    (lambda (cert)
      (pcheck ([certificate? cert])
              (check-certificate-live who cert)
              (let ([ans (ffi-cert-not-before (crypto-certificate-handle cert))])
                (when (eq? ans #f)
                  (errorf who "failed to read certificate not-before time"))
                ans))))

  #|proc:certificate-not-after
The `certificate-not-after` procedure returns the UTC expiry time as an ISO-8601 string.
|#
  (define-who certificate-not-after
    (lambda (cert)
      (pcheck ([certificate? cert])
              (check-certificate-live who cert)
              (let ([ans (ffi-cert-not-after (crypto-certificate-handle cert))])
                (when (eq? ans #f)
                  (errorf who "failed to read certificate not-after time"))
                ans))))

  #|proc:certificate-subject-alt-names
The `certificate-subject-alt-names` procedure returns SAN entries as typed pairs such as `(dns . "example.com")`.
|#
  (define-who certificate-subject-alt-names
    (lambda (cert)
      (pcheck ([certificate? cert])
              (check-certificate-live who cert)
              (let ([ans (ffi-cert-subject-alt-names (crypto-certificate-handle cert))])
                (if (eq? ans #f) '() ans)))))

  #|proc:certificate-hostname-matches?
  The `certificate-hostname-matches?` procedure returns `#t` if string `hostname` matches
  the live certificate `cert` under SAN/CN rules, or `#f` for a mismatch. Hostnames must
  be nonempty and contain no NUL characters; malformed hostnames raise an error.
  |#
  (define-who certificate-hostname-matches?
    (lambda (cert hostname)
      (pcheck ([certificate? cert] [string? hostname])
              (check-certificate-live who cert)
              (check-hostname who hostname)
              (not (fx= 0 (ffi-cert-hostname-matches (crypto-certificate-handle cert)
                                                     (string->utf8 hostname)))))))

  #|proc:certificate-public-key
The `certificate-public-key` procedure extracts the certificate public key as a `(crypto pkey)` object.
|#
  (define-who certificate-public-key
    (lambda (cert)
      (pcheck ([certificate? cert])
              (check-certificate-live who cert)
              (let ([ans (ffi-cert-public-key-der (crypto-certificate-handle cert))])
                (when (eq? ans #f)
                  (errorf who "failed to extract certificate public key"))
                (der->public-key ans)))))

  #|proc:certificate-serial-number
The `certificate-serial-number` procedure returns the certificate serial number bytes.
|#
  (define-who certificate-serial-number
    (lambda (cert)
      (pcheck ([certificate? cert])
              (check-certificate-live who cert)
              (let ([ans (ffi-cert-serial-number (crypto-certificate-handle cert))])
                (when (eq? ans #f)
                  (errorf who "failed to read certificate serial number"))
                ans))))

  #|proc:certificate-fingerprint
The `certificate-fingerprint` procedure returns a certificate digest, defaulting to SHA-256.
|#
  (define-who certificate-fingerprint
    (case-lambda
      [(cert) (certificate-fingerprint cert 'sha256)]
      [(cert which)
       (pcheck ([certificate? cert])
               (check-certificate-live who cert)
               (let ([ans (ffi-cert-fingerprint (crypto-certificate-handle cert) which)])
                 (when (eq? ans #f)
                   (errorf who "failed to compute certificate fingerprint using ~s" which))
                 ans))]))

  #|proc:make-cert-store
The `make-cert-store` procedure creates an empty or system-backed certificate store.
|#
  (define-who make-cert-store
    (case-lambda
      [() (let ([handle (ffi-cert-store-create 0)])
            (when (eq? handle 0)
              (errorf who "failed to create certificate store"))
            (make-crypto-cert-store handle #f))]
      [(source)
       (unless (eq? source 'system)
         (errorf who "valid certificate store source is 'system, given ~s" source))
       (let ([handle (ffi-cert-store-create 1)])
         (when (eq? handle 0)
           (errorf who "failed to create system certificate store"))
         (make-crypto-cert-store handle #f))]))

  #|proc:destroy-cert-store!
The `destroy-cert-store!` procedure releases foreign resources owned by `store`.
|#
  (define-who destroy-cert-store!
    (lambda (store)
      (pcheck ([cert-store? store])
              (unless (crypto-cert-store-destroyed? store)
                (ffi-cert-store-destroy (crypto-cert-store-handle store))
                (crypto-cert-store-handle-set! store 0)
                (crypto-cert-store-destroyed?-set! store #t))
              store)))

  #|proc:cert-store-add!
The `cert-store-add!` procedure adds a trusted certificate to `store`.
|#
  (define-who cert-store-add!
    (lambda (store cert)
      (pcheck ([cert-store? store] [certificate? cert])
              (check-cert-store-live who store)
              (check-certificate-live who cert)
              (unless (fx= 1 (ffi-cert-store-add (crypto-cert-store-handle store)
                                                 (crypto-certificate-handle cert)))
                (errorf who "failed to add certificate to store"))
              store)))

  #|proc:cert-store-load-system-defaults!
The `cert-store-load-system-defaults!` procedure appends system trust anchors to `store`.
|#
  (define-who cert-store-load-system-defaults!
    (lambda (store)
      (pcheck ([cert-store? store])
              (check-cert-store-live who store)
              (unless (fx= 1 (ffi-cert-store-load-defaults (crypto-cert-store-handle store)))
                (errorf who "failed to load system certificate defaults"))
              store)))

  (define verify-certificate-chain*
    (lambda (who cert chain store hostname)
      (check-certificate-chain who chain)
      (when hostname (check-hostname who hostname))
      (let ([state (ffi-cert-verify-state-create (crypto-certificate-handle cert)
                                                 (crypto-cert-store-handle store)
                                                 (if hostname (string->utf8 hostname) #f))])
        (when (eq? state 0)
          (errorf who "failed to create certificate verification state"))
        (dynamic-wind
          void
          (lambda ()
            (for-each
             (lambda (chain-cert)
               (check-certificate-live who chain-cert)
               (unless (fx= 1 (ffi-cert-verify-state-add-chain-cert state
                                                                     (crypto-certificate-handle chain-cert)))
                 (errorf who "failed to add certificate chain element for verification")))
             chain)
            (not (fx= 0 (ffi-cert-verify-state-verify state))))
          (lambda ()
            (ffi-cert-verify-state-destroy state))))))

  #|proc:verify-certificate-chain
  The `verify-certificate-chain` procedure returns `#t` if the live certificate `cert`
  verifies against the live trust `store`, and `#f` on verification failure. Optional
  `chain` is a list of live intermediate certificates; it defaults to the empty list.
  An optional string `hostname` enables peer identity checking. It must be nonempty and
  contain no NUL characters; malformed names raise an error. Omitting the hostname
  verifies the chain without checking a peer identity.
  |#
  (define-who verify-certificate-chain
    (case-lambda
      [(cert store)
       (pcheck ([certificate? cert] [cert-store? store])
               (check-certificate-live who cert)
               (check-cert-store-live who store)
               (verify-certificate-chain* who cert '() store #f))]
      [(cert x y)
       (pcheck ([certificate? cert])
               (cond
                [(and (cert-store? x) (string? y))
                 (check-certificate-live who cert)
                 (check-cert-store-live who x)
                 (verify-certificate-chain* who cert '() x y)]
                [(and (list? x) (cert-store? y))
                 (check-certificate-live who cert)
                 (check-cert-store-live who y)
                 (verify-certificate-chain* who cert x y #f)]
                [else
                 (errorf who "expected (certificate cert-store string) or (certificate list cert-store)")]))]
      [(cert chain store hostname)
       (pcheck ([certificate? cert] [cert-store? store] [string? hostname])
               (check-certificate-live who cert)
               (check-cert-store-live who store)
               (verify-certificate-chain* who cert chain store hostname))]))
  )
