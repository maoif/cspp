(import (cspp) (cspp format protobuf))

(define protobuf-error?
  (lambda (thunk)
    (guard (condition [else #t]) (thunk) #f)))

(mat protobuf-wire
     (equal? #vu8(150 1) (protobuf-encode-varint 150))
     (= 150 (protobuf-decode-varint #vu8(150 1)))
     (equal? #vu8(10 3 97 98 99) (protobuf-encode-field 1 'string "abc"))
     (equal? #vu8(8 1 18 3 102 111 111)
             (protobuf-encode-message '((1 bool #t) (2 string "foo"))))
     (= -1 (protobuf-decode-zigzag (protobuf-encode-zigzag -1)))
     (= -42 (protobuf-decode-signed-varint (protobuf-encode-signed-varint -42)))
     (= -1234 (protobuf-decode-sfixed32 (protobuf-encode-sfixed32 -1234)))
     (= -5678 (protobuf-decode-sfixed64 (protobuf-encode-sfixed64 -5678)))
     (= 1.5 (protobuf-decode-float (protobuf-encode-float 1.5)))
     (= -2.25 (protobuf-decode-double (protobuf-encode-double -2.25)))

     ;; Error case: a varint cannot end while its continuation bit is set.
     (protobuf-error? (lambda () (protobuf-decode-varint #vu8(128))))

     ;; Error case: protobuf varints may not exceed ten bytes.
     (protobuf-error? (lambda () (protobuf-decode-varint #vu8(128 128 128 128 128 128 128 128 128 128))))

     ;; Error case: protobuf field number zero is reserved.
     (protobuf-error? (lambda () (protobuf-encode-field 0 'bool #t)))

     ;; Error case: unsupported field types have no protobuf wire representation.
     (protobuf-error? (lambda () (protobuf-encode-field 1 'invalid #t)))

     ;; Error case: fixed-width decoding requires the complete field width.
     (protobuf-error? (lambda () (protobuf-decode-fixed32 #vu8(1 2 3))))

     ;; Error case: an iterator cannot consume a truncated fixed-width field.
     (protobuf-error?
      (lambda () (protobuf-decoder-next-field (make-protobuf-decoder #vu8(13 1 2 3)))))

     ;; Error case: a length-delimited field length cannot exceed the remaining input.
     (protobuf-error?
      (lambda () (protobuf-decoder-next-field (make-protobuf-decoder #vu8(10 4 1 2 3)))))

     (let* ([decoder (make-protobuf-decoder #vu8(8 150 1 18 3 97 98 99))]
            [first (protobuf-decoder-next-field decoder)]
            [second (protobuf-decoder-next-field decoder)])
       (and (= 1 (protobuf-wire-field-number first))
            (= 0 (protobuf-wire-field-wire-type first))
            (= 150 (protobuf-wire-field-value first))
            (= 2 (protobuf-wire-field-number second))
            (equal? #vu8(97 98 99) (protobuf-wire-field-value second))
            (protobuf-decoder-eof? decoder)))

     (let* ([decoder (make-protobuf-decoder #vu8(40 7))]
            [field (protobuf-decoder-next-field decoder)])
       (protobuf-decoder-preserve-field! decoder field)
       (equal? '#(#vu8(40 7)) (protobuf-decoder-unknown-fields decoder))))
