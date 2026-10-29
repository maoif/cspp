(import (chezscheme)
        (cspp crypto digest)
        (cspp crypto encoding)
        (cspp optional-library))

(define digest-abc
  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
(define digest-empty
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
(define blake3-abc
  "6437b3ac38465133ffb63b75273a8db548c558465d79db03fd359c6cd5bd9d85")
(define blake3-empty
  "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262")

(define with-digest-file
  (lambda (bytes proc)
    (let ([path (format "digest-~a.tmp" (get-process-id))])
      (dynamic-wind
        (lambda ()
          (call-with-port
            (open-file-output-port path (file-options no-fail))
            (lambda (port) (put-bytevector port bytes))))
        (lambda () (proc path))
        (lambda () (when (file-exists? path) (delete-file path)))))))

(define check-digester-lifecycle
  (lambda (algorithm expected-abc expected-empty)
    (let ([state (make-digester algorithm)])
      (dynamic-wind
        void
        (lambda ()
          (digester-update-bytevector! state #vu8(97))
          (digester-update-bytevector! state #vu8(98 99))
          (and (string=? expected-abc (bytevector->hex (digester-get state)))
               (string=? expected-abc (bytevector->hex (digester-get state)))
               (begin
                 (digester-update-bytevector! state #vu8())
                 (digester-reset! state)
                 (and (string=? expected-empty (bytevector->hex (digester-get state)))
                      (begin
                        (digester-update-bytevector! state #vu8(0 97 98 99 0) 1 4)
                        (string=? expected-abc
                                  (bytevector->hex (digester-finalize! state))))))))
        (lambda () (digester-destroy! state))))))

(mat digest-known-vectors
  (mat-requires (openssl)
    (for-all
      (lambda (entry)
        (let ([algorithm (car entry)] [expected (cadr entry)])
          (string=? expected (bytevector->hex (digest-bytevector algorithm #vu8(97 98 99))))))
      '((sha224 "23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7")
        (sha256 "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
        (sha384 "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7")
        (sha512 "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f")
        (sha512-224 "4634270f707b6a54daae7530460842e20e37ed265ceee9a43e8924aa")
        (sha512-256 "53048e2681941ef99b2e29b76b4c7dabe4c2d0c634fc6d46e0e2f13107e7af23")
        (sha3-224 "e642824c3f8cf24ad09234ee7d3c766fc9a3a5168d0c94ad73b46fdf")
        (sha3-256 "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532")
        (sha3-384 "ec01498288516fc926459f58e2c6ad8df9b473cb0fc08c2596da7cf0e49be4b298d88cea927ac7f539f1edf228376d25")
        (sha3-512 "b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0")
        (blake2b-512 "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923")
        (blake2s-256 "508c5e8c327c14e2e1a72ba34eeb452f37458b209ed63a294d999b4c86675982"))))

  (mat-requires (blake3)
    (string=? blake3-abc (bytevector->hex (blake3-bytevector #vu8(0 97 98 99 0) 1 4)))))

(mat digest-utf32
  (mat-requires (openssl)
    (let ([expected
           (if (eq? (native-endianness) (endianness little))
               "91788497d5e167ecf9ae7e177b17262de238f2cd3be4c2beb7f0e6001db6b86c"
               "371caf4a5efb69bb85edb2c37f01adad060705c0670e25603b82e515b66ecb30")])
      (and (string=? expected (bytevector->hex (digest 'sha256 "Aλ😀")))
           (string=? expected (bytevector->hex (sha256-string "_Aλ😀_" 1 4)))
           (string=? expected
                     (bytevector->hex
                       (call-with-digester 'sha256
                         (lambda (state)
                           (digester-update-string! state "_Aλ😀_" 1 2)
                           (digester-update-string! state "_Aλ😀_" 2 4))))))))

  (mat-requires (blake3)
    (let ([bytes (make-bytevector 12)])
      (bytevector-u32-native-set! bytes 0 #x41)
      (bytevector-u32-native-set! bytes 4 #x3bb)
      (bytevector-u32-native-set! bytes 8 #x1f600)
      (equal? (digest-bytevector 'blake3 bytes)
              (call-with-digester 'blake3
                (lambda (state) (digester-update-string! state "Aλ😀")))))))

(mat digest-incremental
  (mat-requires (openssl)
    (check-digester-lifecycle 'sha256 digest-abc digest-empty))

  (mat-requires (blake3)
    (check-digester-lifecycle 'blake3 blake3-abc blake3-empty))

  (mat-requires (openssl)
    (let ([state (make-digester)])
      (dynamic-wind
        void
        (lambda ()
          (digester-update-bytevector! state #vu8(97))
          (digester-get state)
          (digester-update-bytevector! state #vu8(98 99))
          (string=? digest-abc (bytevector->hex (digester-finalize! state))))
        (lambda () (digester-destroy! state)))))

  (mat-requires (openssl)
    (let ([state (make-digester)])
      (digester-finalize! state)
      ;; Error case: terminal states reject reads, updates, resets, and repeated finalization.
      (and (guard (c [else #t]) (digester-get state) #f)
           (guard (c [else #t]) (digester-update-bytevector! state #vu8()) #f)
           (guard (c [else #t]) (digester-update-string! state "") #f)
           (guard (c [else #t]) (digester-reset! state) #f)
           (guard (c [else #t]) (digester-finalize! state) #f)
           (eq? state (digester-destroy! state))
           (eq? state (digester-destroy! state)))))

  (mat-requires (blake3)
    (let ([state (make-digester 'blake3)])
      (digester-destroy! state)
      ;; Error case: explicitly destroyed BLAKE3 states must never reach their native context.
      (and (guard (c [else #t]) (digester-get state) #f)
           (guard (c [else #t]) (digester-update-bytevector! state #vu8()) #f)
           (guard (c [else #t]) (digester-reset! state) #f)
           (guard (c [else #t]) (digester-finalize! state) #f)
           (eq? state (digester-destroy! state))))))

(mat digest-callback-cleanup
  (mat-requires (openssl)
    (let ([escaped-state #f] [reason (list 'callback-error)])
      ;; Error case: cleanup must retain the callback's original exception and destroy its state.
      (and (guard (c [else (eq? c reason)])
             (call-with-digester
               (lambda (state) (set! escaped-state state) (raise reason)))
             #f)
           (guard (c [else #t]) (digester-get escaped-state) #f))))

  (mat-requires (openssl)
    (string=? digest-abc
              (bytevector->hex
                (call-with-digester
                  (lambda (state)
                    (digester-update-bytevector! state #vu8(97 98 99))
                    (values 'ignored 'callback-values)))))))

(mat digest-files
  (mat-requires (openssl)
    (with-digest-file #vu8(0 97 98 99 0)
      (lambda (path)
        (and (string=? digest-abc (bytevector->hex (digest-file 'sha256 path 1 4)))
             (string=? digest-empty (bytevector->hex (sha256-file path 5 5)))))))

  (mat-requires (blake3)
    (with-digest-file #vu8(97 98 99)
      (lambda (path)
        (and (string=? blake3-abc (bytevector->hex (blake3-file path)))
             (string=? blake3-abc (bytevector->hex (digest-file 'blake3 path)))))))

  (mat-requires (openssl)
    (with-digest-file (make-bytevector 8193 97)
      (lambda (path)
        (equal? (sha256-bytevector (make-bytevector 8193 97)) (sha256-file path))))))

(mat digest-metadata
  (mat-requires (openssl)
    (and (= 32 (digest-output-size 'sha256)) (= 128 (digest-block-size 'sha512))))

  (mat-requires (blake3)
    (and (= 32 (digest-output-size 'blake3)) (= 64 (digest-block-size 'blake3)))))

(mat digest-validation
  ;; Error case: algorithm names must be known symbols.
  (error? (make-digester 'unsupported))

  ;; Error case: malformed states must be checked before reading record fields.
  (error? (digester-update-bytevector! #f #vu8() 0 0))

  ;; Error case: bytevector slices cannot run backwards or exceed the data.
  (error? (digest-bytevector 'sha256 #vu8(1) 1 0))

  (error? (digest-bytevector 'sha256 #vu8(1) 0 2))

  ;; Error case: inexact or out-of-fixnum indices must not truncate at the FFI boundary.
  (error? (digest-string 'sha256 "a" 0.0 1))

  (error? (digest-bytevector 'sha256 #vu8(1) 0 (expt 2 100)))

  ;; Error case: generic digest input must be a string or bytevector.
  (error? (digest 'sha256 42)))

(mat digest-optional-backends
  ;; Error case: disabled providers raise rather than exposing sentinels as digests.
  (for-all
    (lambda (entry)
      (let ([library (car entry)] [algorithm (cadr entry)])
        (or (optional-library-available? (optional-library-info library))
            (and (guard (c [else #t]) (digest-bytevector algorithm #vu8(1)) #f)
                 (guard (c [else #t]) (digest-string algorithm "a") #f)
                 (guard (c [else #t]) (make-digester algorithm) #f)
                 (guard (c [else #t]) (digest-output-size algorithm) #f)
                 (guard (c [else #t]) (digest-block-size algorithm) #f)))))
    '((openssl sha256) (blake3 blake3))))
