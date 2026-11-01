(import (cspp) (cspp crypto digest))
(load "examples/common.ss")

(define file-hash
  (lambda (method path)
    (case method
      [(md5 sha224 sha256 sha384 sha512 sha512-224 sha512-256
        sha3-224 sha3-256 sha3-384 sha3-512 blake2b-512 blake2s-256 blake3)
       ;; Stream cryptographic digests through the unified digest API.
       (digest-file method path)]
      [(xxhash32)
       ;; Compute the non-cryptographic 32-bit checksum.
       (xxhash32-file path)]
      [(xxhash64)
       ;; Compute the non-cryptographic 64-bit checksum.
       (xxhash64-file path)]
      [else
       (errorf 'hash-file "unsupported method ~s" method)])))

(example-run!
 'hash-file
 "Print a file hash or checksum using a selected digest method."
 '((file . "Required: path to the file to hash.")
   (method . "Digest: sha224, sha256 (default), sha384, sha512, sha3-224/256/384/512, blake2b-512, blake2s-256, md5, sha512-224, sha512-256, blake3, xxhash32, or xxhash64. Use full names such as sha3-256; some require optional libraries."))
 (lambda (ref)
   (let* ([path (example-required 'hash-file ref 'file)]
          [method (string->symbol (or (ref 'method) "sha256"))]
          [digest (file-hash method path)])
     (example-print
      (list method path
            (if (bytevector? digest)
                (bytevector->hex digest)
                (number->string digest 16))))
     0))
 (command-line-arguments))
