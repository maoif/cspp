(import (cspp))

(mat crypto-password-empty-derived-key
  ;; Error case: empty PBKDF2 output contains no information about the password.
  (mat-requires (openssl)
    (error? (password-hash/pbkdf2 "correct" #vu8(1 2 3 4) 1 0)))

  ;; Error case: empty scrypt output also permits every password to match.
  (mat-requires (openssl)
    (error? (password-hash/scrypt "correct" #vu8(1 2 3 4) 16 1 1 0)))

  ;; Error case: externally supplied records must not bypass constructor validation.
  (let ([encoded (hex->bytevector
                   "4350573101020000000100000000000000000000000000000000")])
    (and (not (password-hash? encoded))
         (not (password-verify? "wrong" encoded))))

  ;; Error case: zero-length scrypt records must be rejected before deriving a candidate.
  (let ([encoded (hex->bytevector
                   "4350573101010000001000000001000000010000000000000000")])
    (and (not (password-hash? encoded))
         (not (password-verify? "wrong" encoded)))))
