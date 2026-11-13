(import (cspp) (cspp hash) (cspp crypto))

;; Exercise the installed library and optional native dependencies after archive relocation.
(unless (and (= 32 (bytevector-length (sha256-string "abc")))
             (= 32 (bytevector-length (blake3-string "abc")))
             (integer? (xxhash32-string "abc")))
  (error 'package-smoke "bundled native APIs failed"))
