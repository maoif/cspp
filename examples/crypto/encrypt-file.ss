(import (cspp)
        (cspp file)
        (cspp crypto aead)
        (cspp crypto kdf)
        (cspp crypto random)
        (cspp crypto encoding))
(load "examples/common.ss")

(define aead-file-header
  (lambda (algorithm salt nonce)
    (string->utf8
     (format "CSPP-AEAD-1\n~a\n~a\n~a\n\n"
             algorithm (bytevector->hex salt) (bytevector->hex nonce)))))

(define bytevector-copy-range
  (lambda (source start stop)
    (let ([target (make-bytevector (- stop start) 0)])
      (bytevector-copy! source start target 0 (- stop start))
      target)))

(define bytevector-concatenate
  (lambda (bytevector*)
    (let* ([size (fold-left (lambda (total bv) (+ total (bytevector-length bv)))
                            0 bytevector*)]
           [output (make-bytevector size 0)])
      (let loop ([rest bytevector*] [offset 0])
        (unless (null? rest)
          (let ([bv (car rest)])
            (bytevector-copy! bv 0 output offset (bytevector-length bv))
            (loop (cdr rest) (+ offset (bytevector-length bv)))))
        output))))

(define split-aead-header
  (lambda (data)
    (let ([length (bytevector-length data)])
      (let loop ([index 0] [line* '()] [start 0])
        (cond
         [(= index length) (errorf 'encrypt-file "truncated envelope header")]
         [(= (bytevector-u8-ref data index) 10)
          (let ([line (utf8->string (bytevector-copy-range data start index))])
            (if (string=? line "")
                (values (reverse line*) (+ index 1))
                (loop (+ index 1) (cons line line*) (+ index 1))))]
         [else (loop (+ index 1) line* start)])))))

(define encrypt-file!
  (lambda (input output method password)
    (when (or (string=? input output) (file-exists? output))
      (errorf 'encrypt-file "output must be a new path distinct from input"))
    ;; Read the input file as bytes before encrypting it.
    (let* ([plaintext (read-u8vec input)]
           [salt (random-bytes 16)]
           [nonce (random-nonce (aead-default-nonce-size method))]
           [key (pbkdf2 'sha256 password salt 200000 (aead-key-size method))])
      ;; Encrypt the file bytes with authenticated associated metadata.
      (let-values ([(ciphertext tag)
                    (aead-encrypt/aad method key nonce
                                      (aead-file-header method salt nonce) plaintext)])
        ;; Store algorithm, salt, nonce, ciphertext, and authentication tag.
        ;; Create the destination exclusively so a concurrent creator cannot be overwritten.
        (write-u8vec output
                     (bytevector-concatenate
                      (list (aead-file-header method salt nonce)
                            ciphertext tag)))
        (bytevector-fill! key 0)
        output))))

(define decrypt-file!
  (lambda (input output password)
    (when (or (string=? input output) (file-exists? output))
      (errorf 'encrypt-file "output must be a new path distinct from input"))
    (let ([data (read-u8vec input)])
      (let-values ([(line* body-start) (split-aead-header data)])
        (unless (and (= (length line*) 4)
                     (string=? (car line*) "CSPP-AEAD-1"))
          (errorf 'encrypt-file "unsupported or malformed envelope"))
        (let* ([method (string->symbol (cadr line*))]
               [salt (hex->bytevector (caddr line*))]
               [nonce (hex->bytevector (cadddr line*))]
               [body-length (- (bytevector-length data) body-start)]
               [tag-length (aead-tag-size method)])
          (when (< body-length tag-length)
            (errorf 'encrypt-file "truncated ciphertext or tag"))
          (let* ([key (pbkdf2 'sha256 password salt 200000 (aead-key-size method))]
                 [ciphertext (bytevector-copy-range
                              data body-start
                              (- (bytevector-length data) tag-length))]
                 [tag (bytevector-copy-range
                       data (- (bytevector-length data) tag-length)
                       (bytevector-length data))])
            ;; Authenticate the ciphertext and metadata before writing plaintext.
            (let ([plaintext
                   (aead-decrypt/aad method key nonce
                                     (bytevector-copy-range data 0 body-start) ciphertext tag)])
              (bytevector-fill! key 0)
              (unless plaintext
                (errorf 'encrypt-file "authentication failed; wrong password or corrupt file"))
              ;; Write plaintext only after AEAD authentication has succeeded.
              ;; Create the destination exclusively so a concurrent creator cannot be overwritten.
              (write-u8vec output plaintext)
              output)))))))

(example-run!
 'encrypt-file
 "Encrypt or decrypt a file using authenticated AES-GCM or ChaCha20-Poly1305."
 '((mode . "Operation: encrypt or decrypt (default: encrypt).")
   (input . "Required: path to the plaintext or encrypted input file.")
   (output . "Required: new output path, distinct from input; existing files are rejected.")
   (method . "Encryption algorithm: aes-256-gcm or chacha20-poly1305 (default: aes-256-gcm); decryption reads it from the envelope.")
   (password-env . "Environment variable containing a nonempty password (default: CSPP_EXAMPLE_PASSWORD)."))
 (lambda (ref)
   (let* ([mode (string->symbol (or (ref 'mode) "encrypt"))]
          [input (example-required 'encrypt-file ref 'input)]
          [output (example-required 'encrypt-file ref 'output)]
          [method (string->symbol (or (ref 'method) "aes-256-gcm"))]
          [password-name (or (ref 'password-env) "CSPP_EXAMPLE_PASSWORD")]
          [password (getenv password-name)])
     (unless (memq method '(aes-256-gcm chacha20-poly1305))
       (errorf 'encrypt-file "method must be aes-256-gcm or chacha20-poly1305"))
     (unless (and password (not (string=? password "")))
       (errorf 'encrypt-file "set password in environment variable ~a" password-name))
     (case mode
       [(encrypt)
        ;; Encrypt input to a new output file with the requested AEAD method.
        (encrypt-file! input output method password)]
       [(decrypt)
        ;; Decrypt input and authenticate before creating the output file.
        (decrypt-file! input output password)]
       [else (errorf 'encrypt-file "mode must be encrypt or decrypt")])
     0)
   )
 (command-line-arguments))
