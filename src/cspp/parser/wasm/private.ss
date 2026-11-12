(library (cspp parser wasm private)
  (export list->immutable-vector concatenate-bytevectors)
  (import (cspp chez)
          (cspp utils))

  #|proc:list->immutable-vector
  Converts list `value*` to an immutable vector and returns that vector.
  |#
  (define list->immutable-vector
    (lambda (value*)
      (pcheck ([list? value*])
              (vector->immutable-vector (list->vector value*)))))

  #|proc:concatenate-bytevectors
  Returns a bytevector containing the bytevectors in proper list `fragments`, in list order.
  The result copies all fragment bytes into one allocation and does not share nonempty storage.
  An empty list or fragments containing no bytes produce an empty bytevector.
  |#
  (define concatenate-bytevectors
    (lambda (fragments)
      (pcheck ([list? fragments]
               [(lambda (values) (for-all bytevector? values)) fragments])
              (let* ([count (fold-left (lambda (count fragment)
                                        (+ count (bytevector-length fragment)))
                                      0 fragments)]
                     [bytes (make-bytevector count)])
                (let loop ([offset 0] [fragments fragments])
                  (unless (null? fragments)
                    (let* ([fragment (car fragments)]
                           [length (bytevector-length fragment)])
                      (bytevector-copy! fragment 0 bytes offset length)
                      (loop (fx+ offset length) (cdr fragments)))))
                bytes))))
  )
