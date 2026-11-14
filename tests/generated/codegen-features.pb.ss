(library (cspp tests codegen codegen-features protobuf)
  (export
    envelope?
    make-envelope
    envelope-id
    envelope-id-present?
    envelope-tags
    envelope-state
    envelope-state-present?
    envelope-payload
    envelope-counters
    envelope-content
    envelope-unknown-fields
    envelope-encoded-size
    envelope-encode
    bytevector->envelope
    envelope-payload?
    make-envelope-payload
    envelope-payload-delta
    envelope-payload-unknown-fields
    envelope-payload-encoded-size
    envelope-payload-encode
    bytevector->envelope-payload
    envelope-state-state-unknown
    envelope-state-state-ready
    shapes-unary-method
    shapes-unary
    shapes-server-method
    shapes-server
    shapes-client-method
    shapes-client
    shapes-bidi-method
    shapes-bidi
    register-shapes-service!
    protobuf-file-descriptor-bytes
    register-protobuf-file-reflection!
  )
  (import (cspp chez)
          (only (cspp utils) pcheck)
          (cspp format protobuf)
          (cspp net grpc)
          (cspp net grpc reflection))

  (define protobuf-file-descriptor-bytes #vu8(10 22 99 111 100 101 103 101 110 45 102 101 97 116 117 114 101 115 46 112 114 111 116 111 18 18 99 115 112 112 46 116 101 115 116 115 46 99 111 100 101 103 101 110 34 176 3 10 8 69 110 118 101 108 111 112 101 18 14 10 2 105 100 24 1 32 1 40 5 82 2 105 100 18 18 10 4 116 97 103 115 24 2 32 3 40 9 82 4 116 97 103 115 18 56 10 5 115 116 97 116 101 24 3 32 1 40 14 50 34 46 99 115 112 112 46 116 101 115 116 115 46 99 111 100 101 103 101 110 46 69 110 118 101 108 111 112 101 46 83 116 97 116 101 82 5 115 116 97 116 101 18 62 10 7 112 97 121 108 111 97 100 24 4 32 1 40 11 50 36 46 99 115 112 112 46 116 101 115 116 115 46 99 111 100 101 103 101 110 46 69 110 118 101 108 111 112 101 46 80 97 121 108 111 97 100 82 7 112 97 121 108 111 97 100 18 70 10 8 99 111 117 110 116 101 114 115 24 5 32 3 40 11 50 42 46 99 115 112 112 46 116 101 115 116 115 46 99 111 100 101 103 101 110 46 69 110 118 101 108 111 112 101 46 67 111 117 110 116 101 114 115 69 110 116 114 121 82 8 99 111 117 110 116 101 114 115 18 20 10 4 116 101 120 116 24 6 32 1 40 9 72 0 82 4 116 101 120 116 18 18 10 3 114 97 119 24 7 32 1 40 12 72 0 82 3 114 97 119 26 31 10 7 80 97 121 108 111 97 100 18 20 10 5 100 101 108 116 97 24 1 32 2 40 18 82 5 100 101 108 116 97 26 59 10 13 67 111 117 110 116 101 114 115 69 110 116 114 121 18 16 10 3 107 101 121 24 1 32 1 40 9 82 3 107 101 121 18 20 10 5 118 97 108 117 101 24 2 32 1 40 4 82 5 118 97 108 117 101 58 2 56 1 34 43 10 5 83 116 97 116 101 18 17 10 13 83 84 65 84 69 95 85 78 75 78 79 87 78 16 0 18 15 10 11 83 84 65 84 69 95 82 69 65 68 89 16 1 66 9 10 7 99 111 110 116 101 110 116 50 165 2 10 6 83 104 97 112 101 115 18 67 10 5 85 110 97 114 121 18 28 46 99 115 112 112 46 116 101 115 116 115 46 99 111 100 101 103 101 110 46 69 110 118 101 108 111 112 101 26 28 46 99 115 112 112 46 116 101 115 116 115 46 99 111 100 101 103 101 110 46 69 110 118 101 108 111 112 101 18 70 10 6 83 101 114 118 101 114 18 28 46 99 115 112 112 46 116 101 115 116 115 46 99 111 100 101 103 101 110 46 69 110 118 101 108 111 112 101 26 28 46 99 115 112 112 46 116 101 115 116 115 46 99 111 100 101 103 101 110 46 69 110 118 101 108 111 112 101 48 1 18 70 10 6 67 108 105 101 110 116 18 28 46 99 115 112 112 46 116 101 115 116 115 46 99 111 100 101 103 101 110 46 69 110 118 101 108 111 112 101 26 28 46 99 115 112 112 46 116 101 115 116 115 46 99 111 100 101 103 101 110 46 69 110 118 101 108 111 112 101 40 1 18 70 10 4 66 105 100 105 18 28 46 99 115 112 112 46 116 101 115 116 115 46 99 111 100 101 103 101 110 46 69 110 118 101 108 111 112 101 26 28 46 99 115 112 112 46 116 101 115 116 115 46 99 111 100 101 103 101 110 46 69 110 118 101 108 111 112 101 40 1 48 1 74 154 8 10 6 18 4 0 0 31 1 10 8 10 1 12 18 3 0 0 18 10 8 10 1 2 18 3 2 0 27 10 10 10 2 4 0 18 4 4 0 24 1 10 10 10 3 4 0 1 18 3 4 8 16 10 12 10 4 4 0 4 0 18 4 5 2 8 3 10 12 10 5 4 0 4 0 1 18 3 5 7 12 10 13 10 6 4 0 4 0 2 0 18 3 6 4 22 10 14 10 7 4 0 4 0 2 0 1 18 3 6 4 17 10 14 10 7 4 0 4 0 2 0 2 18 3 6 20 21 10 13 10 6 4 0 4 0 2 1 18 3 7 4 20 10 14 10 7 4 0 4 0 2 1 1 18 3 7 4 15 10 14 10 7 4 0 4 0 2 1 2 18 3 7 18 19 10 12 10 4 4 0 3 0 18 4 10 2 12 3 10 12 10 5 4 0 3 0 1 18 3 10 10 17 10 13 10 6 4 0 3 0 2 0 18 3 11 4 30 10 14 10 7 4 0 3 0 2 0 4 18 3 11 4 12 10 14 10 7 4 0 3 0 2 0 5 18 3 11 13 19 10 14 10 7 4 0 3 0 2 0 1 18 3 11 20 25 10 14 10 7 4 0 3 0 2 0 3 18 3 11 28 29 10 11 10 4 4 0 2 0 18 3 14 2 24 10 12 10 5 4 0 2 0 4 18 3 14 2 10 10 12 10 5 4 0 2 0 5 18 3 14 11 16 10 12 10 5 4 0 2 0 1 18 3 14 17 19 10 12 10 5 4 0 2 0 3 18 3 14 22 23 10 11 10 4 4 0 2 1 18 3 15 2 27 10 12 10 5 4 0 2 1 4 18 3 15 2 10 10 12 10 5 4 0 2 1 5 18 3 15 11 17 10 12 10 5 4 0 2 1 1 18 3 15 18 22 10 12 10 5 4 0 2 1 3 18 3 15 25 26 10 11 10 4 4 0 2 2 18 3 16 2 27 10 12 10 5 4 0 2 2 4 18 3 16 2 10 10 12 10 5 4 0 2 2 6 18 3 16 11 16 10 12 10 5 4 0 2 2 1 18 3 16 17 22 10 12 10 5 4 0 2 2 3 18 3 16 25 26 10 11 10 4 4 0 2 3 18 3 17 2 31 10 12 10 5 4 0 2 3 4 18 3 17 2 10 10 12 10 5 4 0 2 3 6 18 3 17 11 18 10 12 10 5 4 0 2 3 1 18 3 17 19 26 10 12 10 5 4 0 2 3 3 18 3 17 29 30 10 11 10 4 4 0 2 4 18 3 18 2 35 10 12 10 5 4 0 2 4 6 18 3 18 2 21 10 12 10 5 4 0 2 4 1 18 3 18 22 30 10 12 10 5 4 0 2 4 3 18 3 18 33 34 10 12 10 4 4 0 8 0 18 4 20 2 23 3 10 12 10 5 4 0 8 0 1 18 3 20 8 15 10 11 10 4 4 0 2 5 18 3 21 4 20 10 12 10 5 4 0 2 5 5 18 3 21 4 10 10 12 10 5 4 0 2 5 1 18 3 21 11 15 10 12 10 5 4 0 2 5 3 18 3 21 18 19 10 11 10 4 4 0 2 6 18 3 22 4 18 10 12 10 5 4 0 2 6 5 18 3 22 4 9 10 12 10 5 4 0 2 6 1 18 3 22 10 13 10 12 10 5 4 0 2 6 3 18 3 22 16 17 10 10 10 2 6 0 18 4 26 0 31 1 10 10 10 3 6 0 1 18 3 26 8 14 10 11 10 4 6 0 2 0 18 3 27 2 41 10 12 10 5 6 0 2 0 1 18 3 27 6 11 10 12 10 5 6 0 2 0 2 18 3 27 12 20 10 12 10 5 6 0 2 0 3 18 3 27 31 39 10 11 10 4 6 0 2 1 18 3 28 2 49 10 12 10 5 6 0 2 1 1 18 3 28 6 12 10 12 10 5 6 0 2 1 2 18 3 28 13 21 10 12 10 5 6 0 2 1 6 18 3 28 32 38 10 12 10 5 6 0 2 1 3 18 3 28 39 47 10 11 10 4 6 0 2 2 18 3 29 2 49 10 12 10 5 6 0 2 2 1 18 3 29 6 12 10 12 10 5 6 0 2 2 5 18 3 29 13 19 10 12 10 5 6 0 2 2 2 18 3 29 20 28 10 12 10 5 6 0 2 2 3 18 3 29 39 47 10 11 10 4 6 0 2 3 18 3 30 2 54 10 12 10 5 6 0 2 3 1 18 3 30 6 10 10 12 10 5 6 0 2 3 5 18 3 30 11 17 10 12 10 5 6 0 2 3 2 18 3 30 18 26 10 12 10 5 6 0 2 3 6 18 3 30 37 43 10 12 10 5 6 0 2 3 3 18 3 30 44 52))

#|proc:register-protobuf-file-reflection!
The `register-protobuf-file-reflection!` procedure adds this file to `registry`.
The return value is the supplied gRPC reflection registry.
|#
(define register-protobuf-file-reflection!
  (lambda (registry)
    (pcheck
      ((grpc-reflection-registry? registry))
      (grpc-reflection-register-file! registry "codegen-features.proto"
        protobuf-file-descriptor-bytes
        '("cspp.tests.codegen.Envelope"
           "cspp.tests.codegen.Envelope.Payload")
        '("cspp.tests.codegen.Shapes")))))

(define %protobuf-encode-with-unknown
  (lambda (field* unknown*)
    (let-values ([(port get) (open-bytevector-output-port)])
      (put-bytevector port (protobuf-encode-message field*))
      (vector-for-each
        (lambda (raw) (put-bytevector port raw))
        unknown*)
      (get))))

(define %protobuf-signed
  (lambda (value bits)
    (if (bitwise-bit-set? value (- bits 1))
        (- value (bitwise-arithmetic-shift 1 bits))
        value)))

(define %protobuf-zigzag
  (lambda (value)
    (bitwise-xor
      (bitwise-arithmetic-shift value -1)
      (- (bitwise-and value 1)))))

(define %protobuf-u32->signed
  (lambda (value) (%protobuf-signed value 32)))

(define %protobuf-u64->signed
  (lambda (value) (%protobuf-signed value 64)))

(define %protobuf-u32->float
  (lambda (value)
    (let ([bytes (make-bytevector 4)])
      (bytevector-u32-set! bytes 0 value (endianness little))
      (protobuf-decode-float bytes))))

(define %protobuf-u64->double
  (lambda (value)
    (let ([bytes (make-bytevector 8)])
      (bytevector-u64-set! bytes 0 value (endianness little))
      (protobuf-decode-double bytes))))

(define %protobuf-map-fields
  (lambda (number table encode-entry)
    (let-values ([(key* value*) (hashtable-entries table)])
      (let loop ([index 0] [answer '()])
        (if (= index (vector-length key*))
            (reverse answer)
            (loop
              (+ index 1)
              (cons
                (list
                  number
                  'message
                  (encode-entry
                    (vector-ref key* index)
                    (vector-ref value* index)))
                answer)))))))

(define envelope-state-state-unknown 0)
(define envelope-state-state-ready 1)

(define %envelope-counters-entry-encode
  (lambda (key value)
    (protobuf-encode-message
      (list (list 1 'string key) (list 2 'uint64 value)))))
(define %bytevector->envelope-counters-entry
  (lambda (bytes)
    (let ([decoder (make-protobuf-decoder bytes)]
          [key ""]
          [value 0])
      (let loop ()
        (let ([field (protobuf-decoder-next-field decoder)])
          (when field
            (case (protobuf-wire-field-number field)
              [(1)
               (set! key
                 (protobuf-decode-string (protobuf-wire-field-value field)))]
              [(2) (set! value (protobuf-wire-field-value field))])
            (loop))))
      (cons key value))))

(define-record-type (%envelope-record-type
                      %make-envelope
                      envelope?)
  (sealed #t)
  (opaque #f)
  (fields (immutable id envelope-id)
    (immutable id-present? envelope-id-present?)
    (immutable tags envelope-tags)
    (immutable state envelope-state)
    (immutable state-present? envelope-state-present?)
    (immutable payload envelope-payload)
    (immutable counters envelope-counters)
    (immutable content envelope-content)
    (immutable unknown-fields envelope-unknown-fields)))

#|proc:make-envelope
The `make-envelope` procedure creates a protobuf `Envelope` message.
Its parameters supply fields in schema order; presence flags mark optional values.
The return value is a new message record with no unknown fields.
|#
(define make-envelope
  (lambda (id id-present? tags state state-present? payload
           counters content)
    (pcheck
      ((integer? id) (boolean? id-present?) (vector? tags) (integer? state)
        (boolean? state-present?)
        ((lambda (value) (or (not value) (envelope-payload? value)))
          payload)
        (hashtable? counters)
        ((lambda (value) (or (not value) (pair? value))) content))
      (%make-envelope id id-present? tags state state-present?
        payload counters content '#()))))

#|proc:envelope-encode
The `envelope-encode` procedure encodes protobuf record `message`.
The return value is a newly allocated wire-format bytevector.
|#
(define envelope-encode
  (lambda (message)
    (pcheck
      ((envelope? message))
      (%protobuf-encode-with-unknown
        (append
          (if (envelope-id-present? message)
              (list (list 1 'int32 (envelope-id message)))
              '())
          (map (lambda (value) (list 2 'string value))
               (vector->list (envelope-tags message)))
          (if (envelope-state-present? message)
              (list (list 3 'enum (envelope-state message)))
              '())
          (if (envelope-payload message)
              (list
                (list
                  4
                  'message
                  (envelope-payload-encode (envelope-payload message))))
              '())
          (%protobuf-map-fields
            5
            (envelope-counters message)
            %envelope-counters-entry-encode)
          (let ([choice (envelope-content message)])
            (if (not choice)
                '()
                (case (car choice)
                  [(text) (list (list 6 'string (cdr choice)))]
                  [(raw) (list (list 7 'bytes (cdr choice)))]
                  [else
                   (errorf 'envelope-content
                     "invalid oneof value: ~s"
                     choice)]))))
        (envelope-unknown-fields message)))))

#|proc:envelope-encoded-size
The `envelope-encoded-size` procedure measures protobuf record `message`.
The return value is the encoded byte length.
|#
(define envelope-encoded-size
  (lambda (message)
    (pcheck
      ((envelope? message))
      (bytevector-length (envelope-encode message)))))

#|proc:bytevector->envelope
The `bytevector->envelope` procedure decodes protobuf bytevector `bytes`.
The return value is a message record that retains unknown fields.
|#
(define bytevector->envelope
  (lambda (bytes)
    (pcheck
      ((bytevector? bytes))
      (let ([decoder (make-protobuf-decoder bytes)]
            [id 0]
            [id-present? #f]
            [tags '()]
            [state 0]
            [state-present? #f]
            [payload #f]
            [counters (make-hashtable equal-hash equal?)]
            [content #f])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(1)
                 (begin
                   (set! id
                     (%protobuf-signed (protobuf-wire-field-value field) 32))
                   (set! id-present? #t))]
                [(2)
                 (set! tags
                   (cons
                     (protobuf-decode-string (protobuf-wire-field-value field))
                     tags))]
                [(3)
                 (begin
                   (set! state
                     (%protobuf-signed (protobuf-wire-field-value field) 32))
                   (set! state-present? #t))]
                [(4)
                 (set! payload
                   (bytevector->envelope-payload
                     (protobuf-wire-field-value field)))]
                [(5)
                 (let ([entry (%bytevector->envelope-counters-entry
                                (protobuf-wire-field-value field))])
                   (hashtable-set! counters (car entry) (cdr entry)))]
                [(6)
                 (set! content
                   (cons
                     'text
                     (protobuf-decode-string
                       (protobuf-wire-field-value field))))]
                [(7)
                 (set! content
                   (cons 'raw (protobuf-wire-field-value field)))]
                [else (protobuf-decoder-preserve-field! decoder field)])
              (loop))))
        (%make-envelope id id-present? (list->vector (reverse tags)) state
          state-present? payload counters content
          (protobuf-decoder-unknown-fields decoder))))))

(define-record-type (%envelope-payload-record-type
                      %make-envelope-payload
                      envelope-payload?)
  (sealed #t)
  (opaque #f)
  (fields
    (immutable delta envelope-payload-delta)
    (immutable unknown-fields envelope-payload-unknown-fields)))

#|proc:make-envelope-payload
The `make-envelope-payload` procedure creates a protobuf `Payload` message.
Its parameters supply fields in schema order; presence flags mark optional values.
The return value is a new message record with no unknown fields.
|#
(define make-envelope-payload
  (lambda (delta)
    (pcheck
      ((integer? delta))
      (%make-envelope-payload delta '#()))))

#|proc:envelope-payload-encode
The `envelope-payload-encode` procedure encodes protobuf record `message`.
The return value is a newly allocated wire-format bytevector.
|#
(define envelope-payload-encode
  (lambda (message)
    (pcheck
      ((envelope-payload? message))
      (%protobuf-encode-with-unknown
        (append
          (list (list 1 'sint64 (envelope-payload-delta message))))
        (envelope-payload-unknown-fields message)))))

#|proc:envelope-payload-encoded-size
The `envelope-payload-encoded-size` procedure measures protobuf record `message`.
The return value is the encoded byte length.
|#
(define envelope-payload-encoded-size
  (lambda (message)
    (pcheck
      ((envelope-payload? message))
      (bytevector-length (envelope-payload-encode message)))))

#|proc:bytevector->envelope-payload
The `bytevector->envelope-payload` procedure decodes protobuf bytevector `bytes`.
The return value is a message record that retains unknown fields.
|#
(define bytevector->envelope-payload
  (lambda (bytes)
    (pcheck
      ((bytevector? bytes))
      (let ([decoder (make-protobuf-decoder bytes)] [delta 0])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(1)
                 (set! delta
                   (%protobuf-zigzag (protobuf-wire-field-value field)))]
                [else (protobuf-decoder-preserve-field! decoder field)])
              (loop))))
        (%make-envelope-payload
          delta
          (protobuf-decoder-unknown-fields decoder))))))

(define shapes-unary-method
  "/cspp.tests.codegen.Shapes/Unary")

#|proc:shapes-unary
The `shapes-unary` procedure starts the generated `Unary` RPC.
The `channel` parameter is a client gRPC channel; request values are encoded.
The return value is a decoded response record or a gRPC stream.
|#
(define shapes-unary
  (lambda (channel request)
    (pcheck
      ((grpc-channel? channel) (envelope? request))
      (bytevector->envelope
        (grpc-response-payload
          (grpc-call
            channel
            shapes-unary-method
            (envelope-encode request)))))))

(define shapes-server-method
  "/cspp.tests.codegen.Shapes/Server")

#|proc:shapes-server
The `shapes-server` procedure starts the generated `Server` RPC.
The `channel` parameter is a client gRPC channel; request values are encoded.
The return value is a decoded response record or a gRPC stream.
|#
(define shapes-server
  (lambda (channel request)
    (pcheck
      ((grpc-channel? channel) (envelope? request))
      (grpc-call/server-stream
        channel
        shapes-server-method
        (envelope-encode request)))))

(define shapes-client-method
  "/cspp.tests.codegen.Shapes/Client")

#|proc:shapes-client
The `shapes-client` procedure starts the generated `Client` RPC.
The `channel` parameter is a client gRPC channel; request values are encoded.
The return value is a decoded response record or a gRPC stream.
|#
(define shapes-client
  (lambda (channel message*)
    (pcheck
      ((grpc-channel? channel) (vector? message*))
      (let ([stream (grpc-call/client-stream
                      channel
                      shapes-client-method)])
        (dynamic-wind
          void
          (lambda ()
            (vector-for-each
              (lambda (message)
                (grpc-stream-send stream (envelope-encode message)))
              message*)
            (grpc-stream-close-send stream)
            (bytevector->envelope (grpc-stream-recv stream)))
          (lambda () (grpc-stream-close stream)))))))

(define shapes-bidi-method
  "/cspp.tests.codegen.Shapes/Bidi")

#|proc:shapes-bidi
The `shapes-bidi` procedure starts the generated `Bidi` RPC.
The `channel` parameter is a client gRPC channel; request values are encoded.
The return value is a decoded response record or a gRPC stream.
|#
(define shapes-bidi
  (lambda (channel)
    (pcheck
      ((grpc-channel? channel))
      (grpc-call/bidi-stream channel shapes-bidi-method))))

#|proc:register-shapes-service!
The `register-shapes-service!` procedure registers generated handlers on `server`.
Each handler corresponds to one schema method and must follow its RPC shape.
The return value is `server`.
|#
(define register-shapes-service!
  (lambda (server unary-handler server-handler client-handler
           bidi-handler)
    (pcheck
      ((grpc-channel? server)
        (procedure?
          unary-handler
          server-handler
          client-handler
          bidi-handler))
      (grpc-register-service!
        server
        shapes-unary-method
        'unary
        unary-handler)
      (grpc-register-service!
        server
        shapes-server-method
        'server
        server-handler)
      (grpc-register-service!
        server
        shapes-client-method
        'client
        client-handler)
      (grpc-register-service!
        server
        shapes-bidi-method
        'bidi
        bidi-handler)
      server)))

)
