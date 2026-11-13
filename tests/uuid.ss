(import (cspp) (cspp uuid))

(mat uuid-bytevector
     (mat-requires (uuid)
       (let* ([uuid (string->uuid "1b4e28ba-2fa1-11d2-883f-0016d3cca427")]
              [bv (uuid->bytevector uuid)]
              [uuid* (bytevector->uuid bv)])
         (and (uuid? uuid*)
              (uuid=? uuid uuid*)
              (equal? (uuid->string uuid*)
                      "1b4e28ba-2fa1-11d2-883f-0016d3cca427"))))
     (mat-requires (uuid)
       (let* ([uuid (string->uuid "1b4e28ba-2fa1-11d2-883f-0016d3cca427")]
              [bv (uuid->bytevector uuid)]
              [uuid* (bytevector->uuid bv)])
         (bytevector-u8-set! bv 0 #x00)
         (uuid=? uuid uuid*)))
     (guard (c [else #t])
       (bytevector->uuid (make-bytevector 15 0))
       #f)
     (guard (c [else #t])
       (bytevector->uuid (make-bytevector 17 0))
       #f))

(mat uuid-time
     (mat-requires (uuid)
       (let-values ([(safe? uuid) (make-uuid-from-time)])
         (let ([t (uuid-time uuid)])
           (and (boolean? safe?)
                (uuid? uuid)
                (time? t)
                (eq? (time-type t) 'time-utc)
                (integer? (time-second t))
                (integer? (time-nanosecond t)))))))
