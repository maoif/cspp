(library (cspp parser wasm private)
  (export list->immutable-vector concatenate-bytevectors expression-uses-data-index?)
  (import (cspp chez)
          (only (cspp vector) vexists)
          (only (cspp parser wasm types)
                wasm-instruction-mnemonic wasm-instruction-body wasm-instruction-alternate)
          (cspp utils))

  #|proc:expression-uses-data-index?
  Returns whether instruction vector `instructions` references a data segment, including nested
  bodies and both arms of `if`. Catch clauses in other structured alternates are not instructions.
  |#
  (define expression-uses-data-index?
    (lambda (instructions)
      (pcheck ([vector? instructions])
              (vexists
               (lambda (instruction)
                 (let ([mnemonic (wasm-instruction-mnemonic instruction)])
                   (or (memq mnemonic '(memory.init data.drop array.new-data array.init-data))
                       (expression-uses-data-index? (wasm-instruction-body instruction))
                       (and (eq? mnemonic 'if)
                            (expression-uses-data-index?
                             (wasm-instruction-alternate instruction))))))
               instructions))))

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
