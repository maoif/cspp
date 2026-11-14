(library (cspp crypto constant-time)
  (export constant-time-bytevector=?
          constant-time-subbytevector=?)
  (import (cspp chez)
          (only (cspp utils) define-who pcheck)
          (cspp crypto private ffi)
          (cspp crypto private common))

  #|proc:constant-time-subbytevector=?
The `constant-time-subbytevector=?` procedure compares two bytevector slices in constant time when the lengths match.
|#
  (define-who constant-time-subbytevector=?
    (lambda (bv1 start1 stop1 bv2 start2 stop2)
      (pcheck ([bytevector? bv1 bv2])
              (check-slice who (bytevector-length bv1) start1 stop1)
              (check-slice who (bytevector-length bv2) start2 stop2)
              (ensure-openssl who)
              (not (fx= 0 (ffi-constant-time-eq bv1 start1 stop1 bv2 start2 stop2))))))

  #|proc:constant-time-bytevector=?
The `constant-time-bytevector=?` procedure compares two whole bytevectors in constant time when their lengths match.
|#
  (define constant-time-bytevector=?
    (lambda (bv1 bv2)
      (pcheck ([bytevector? bv1 bv2])
              (constant-time-subbytevector=?
               bv1 0 (bytevector-length bv1)
               bv2 0 (bytevector-length bv2)))))
  )
