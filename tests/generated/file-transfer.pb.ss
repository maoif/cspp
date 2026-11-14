(library (cspp examples transfer file-transfer protobuf)
  (export
    file-chunk?
    make-file-chunk
    file-chunk-name
    file-chunk-offset
    file-chunk-data
    file-chunk-sha256
    file-chunk-done?
    file-chunk-unknown-fields
    file-chunk-encoded-size
    file-chunk-encode
    bytevector->file-chunk
    transfer-result?
    make-transfer-result
    transfer-result-size
    transfer-result-sha256
    transfer-result-unknown-fields
    transfer-result-encoded-size
    transfer-result-encode
    bytevector->transfer-result
    file-transfer-upload-method
    file-transfer-upload
    file-transfer-download-method
    file-transfer-download
    register-file-transfer-service!
    protobuf-file-descriptor-bytes
    register-protobuf-file-reflection!
  )
  (import (cspp chez)
          (cspp utils)
          (cspp format protobuf)
          (cspp net grpc)
          (cspp net grpc reflection))

  (define protobuf-file-descriptor-bytes #vu8(10 19 102 105 108 101 45 116 114 97 110 115 102 101 114 46 112 114 111 116 111 18 22 99 115 112 112 46 101 120 97 109 112 108 101 115 46 116 114 97 110 115 102 101 114 34 119 10 9 70 105 108 101 67 104 117 110 107 18 18 10 4 110 97 109 101 24 1 32 1 40 9 82 4 110 97 109 101 18 22 10 6 111 102 102 115 101 116 24 2 32 1 40 4 82 6 111 102 102 115 101 116 18 18 10 4 100 97 116 97 24 3 32 1 40 12 82 4 100 97 116 97 18 22 10 6 115 104 97 50 53 54 24 4 32 1 40 12 82 6 115 104 97 50 53 54 18 18 10 4 100 111 110 101 24 5 32 1 40 8 82 4 100 111 110 101 34 60 10 14 84 114 97 110 115 102 101 114 82 101 115 117 108 116 18 18 10 4 115 105 122 101 24 1 32 1 40 4 82 4 115 105 122 101 18 22 10 6 115 104 97 50 53 54 24 2 32 1 40 12 82 6 115 104 97 50 53 54 50 185 1 10 12 70 105 108 101 84 114 97 110 115 102 101 114 18 85 10 6 85 112 108 111 97 100 18 33 46 99 115 112 112 46 101 120 97 109 112 108 101 115 46 116 114 97 110 115 102 101 114 46 70 105 108 101 67 104 117 110 107 26 38 46 99 115 112 112 46 101 120 97 109 112 108 101 115 46 116 114 97 110 115 102 101 114 46 84 114 97 110 115 102 101 114 82 101 115 117 108 116 40 1 18 82 10 8 68 111 119 110 108 111 97 100 18 33 46 99 115 112 112 46 101 120 97 109 112 108 101 115 46 116 114 97 110 115 102 101 114 46 70 105 108 101 67 104 117 110 107 26 33 46 99 115 112 112 46 101 120 97 109 112 108 101 115 46 116 114 97 110 115 102 101 114 46 70 105 108 101 67 104 117 110 107 48 1 74 239 4 10 6 18 4 0 0 20 1 10 8 10 1 12 18 3 0 0 18 10 8 10 1 2 18 3 2 0 31 10 10 10 2 4 0 18 4 4 0 10 1 10 10 10 3 4 0 1 18 3 4 8 17 10 11 10 4 4 0 2 0 18 3 5 2 18 10 12 10 5 4 0 2 0 5 18 3 5 2 8 10 12 10 5 4 0 2 0 1 18 3 5 9 13 10 12 10 5 4 0 2 0 3 18 3 5 16 17 10 11 10 4 4 0 2 1 18 3 6 2 20 10 12 10 5 4 0 2 1 5 18 3 6 2 8 10 12 10 5 4 0 2 1 1 18 3 6 9 15 10 12 10 5 4 0 2 1 3 18 3 6 18 19 10 11 10 4 4 0 2 2 18 3 7 2 17 10 12 10 5 4 0 2 2 5 18 3 7 2 7 10 12 10 5 4 0 2 2 1 18 3 7 8 12 10 12 10 5 4 0 2 2 3 18 3 7 15 16 10 11 10 4 4 0 2 3 18 3 8 2 19 10 12 10 5 4 0 2 3 5 18 3 8 2 7 10 12 10 5 4 0 2 3 1 18 3 8 8 14 10 12 10 5 4 0 2 3 3 18 3 8 17 18 10 11 10 4 4 0 2 4 18 3 9 2 16 10 12 10 5 4 0 2 4 5 18 3 9 2 6 10 12 10 5 4 0 2 4 1 18 3 9 7 11 10 12 10 5 4 0 2 4 3 18 3 9 14 15 10 10 10 2 4 1 18 4 12 0 15 1 10 10 10 3 4 1 1 18 3 12 8 22 10 11 10 4 4 1 2 0 18 3 13 2 18 10 12 10 5 4 1 2 0 5 18 3 13 2 8 10 12 10 5 4 1 2 0 1 18 3 13 9 13 10 12 10 5 4 1 2 0 3 18 3 13 16 17 10 11 10 4 4 1 2 1 18 3 14 2 19 10 12 10 5 4 1 2 1 5 18 3 14 2 7 10 12 10 5 4 1 2 1 1 18 3 14 8 14 10 12 10 5 4 1 2 1 3 18 3 14 17 18 10 10 10 2 6 0 18 4 17 0 20 1 10 10 10 3 6 0 1 18 3 17 8 20 10 11 10 4 6 0 2 0 18 3 18 2 56 10 12 10 5 6 0 2 0 1 18 3 18 6 12 10 12 10 5 6 0 2 0 5 18 3 18 13 19 10 12 10 5 6 0 2 0 2 18 3 18 20 29 10 12 10 5 6 0 2 0 3 18 3 18 40 54 10 11 10 4 6 0 2 1 18 3 19 2 53 10 12 10 5 6 0 2 1 1 18 3 19 6 14 10 12 10 5 6 0 2 1 2 18 3 19 15 24 10 12 10 5 6 0 2 1 6 18 3 19 35 41 10 12 10 5 6 0 2 1 3 18 3 19 42 51 98 6 112 114 111 116 111 51))

#|proc:register-protobuf-file-reflection!
The `register-protobuf-file-reflection!` procedure adds this file to `registry`.
The return value is the supplied gRPC reflection registry.
|#
(define register-protobuf-file-reflection!
  (lambda (registry)
    (pcheck
      ((grpc-reflection-registry? registry))
      (grpc-reflection-register-file! registry "file-transfer.proto"
        protobuf-file-descriptor-bytes
        '("cspp.examples.transfer.FileChunk"
           "cspp.examples.transfer.TransferResult")
        '("cspp.examples.transfer.FileTransfer")))))

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

(define-record-type (%file-chunk-record-type
                      %make-file-chunk
                      file-chunk?)
  (sealed #t)
  (opaque #f)
  (fields (immutable name file-chunk-name)
    (immutable offset file-chunk-offset)
    (immutable data file-chunk-data)
    (immutable sha256 file-chunk-sha256)
    (immutable done? file-chunk-done?)
    (immutable unknown-fields file-chunk-unknown-fields)))

#|proc:make-file-chunk
The `make-file-chunk` procedure creates a protobuf `FileChunk` message.
Its parameters supply fields in schema order; presence flags mark optional values.
The return value is a new message record with no unknown fields.
|#
(define make-file-chunk
  (lambda (name offset data sha256 done?)
    (pcheck
      ((string? name)
        (natural? offset)
        (bytevector? data)
        (bytevector? sha256)
        (boolean? done?))
      (%make-file-chunk name offset data sha256 done? '#()))))

#|proc:file-chunk-encode
The `file-chunk-encode` procedure encodes protobuf record `message`.
The return value is a newly allocated wire-format bytevector.
|#
(define file-chunk-encode
  (lambda (message)
    (pcheck
      ((file-chunk? message))
      (%protobuf-encode-with-unknown
        (append
          (if (not (string=? (file-chunk-name message) ""))
              (list (list 1 'string (file-chunk-name message)))
              '())
          (if (not (zero? (file-chunk-offset message)))
              (list (list 2 'uint64 (file-chunk-offset message)))
              '())
          (if (positive?
                (bytevector-length (file-chunk-data message)))
              (list (list 3 'bytes (file-chunk-data message)))
              '())
          (if (positive?
                (bytevector-length (file-chunk-sha256 message)))
              (list (list 4 'bytes (file-chunk-sha256 message)))
              '())
          (if (file-chunk-done? message)
              (list (list 5 'bool (file-chunk-done? message)))
              '()))
        (file-chunk-unknown-fields message)))))

#|proc:file-chunk-encoded-size
The `file-chunk-encoded-size` procedure measures protobuf record `message`.
The return value is the encoded byte length.
|#
(define file-chunk-encoded-size
  (lambda (message)
    (pcheck
      ((file-chunk? message))
      (bytevector-length (file-chunk-encode message)))))

#|proc:bytevector->file-chunk
The `bytevector->file-chunk` procedure decodes protobuf bytevector `bytes`.
The return value is a message record that retains unknown fields.
|#
(define bytevector->file-chunk
  (lambda (bytes)
    (pcheck
      ((bytevector? bytes))
      (let ([decoder (make-protobuf-decoder bytes)]
            [name ""]
            [offset 0]
            [data #vu8()]
            [sha256 #vu8()]
            [done? #f])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(1)
                 (set! name
                   (protobuf-decode-string (protobuf-wire-field-value field)))]
                [(2) (set! offset (protobuf-wire-field-value field))]
                [(3) (set! data (protobuf-wire-field-value field))]
                [(4) (set! sha256 (protobuf-wire-field-value field))]
                [(5)
                 (set! done?
                   (not (zero? (protobuf-wire-field-value field))))]
                [else (protobuf-decoder-preserve-field! decoder field)])
              (loop))))
        (%make-file-chunk name offset data sha256 done?
          (protobuf-decoder-unknown-fields decoder))))))

(define-record-type (%transfer-result-record-type
                      %make-transfer-result
                      transfer-result?)
  (sealed #t)
  (opaque #f)
  (fields
    (immutable size transfer-result-size)
    (immutable sha256 transfer-result-sha256)
    (immutable unknown-fields transfer-result-unknown-fields)))

#|proc:make-transfer-result
The `make-transfer-result` procedure creates a protobuf `TransferResult` message.
Its parameters supply fields in schema order; presence flags mark optional values.
The return value is a new message record with no unknown fields.
|#
(define make-transfer-result
  (lambda (size sha256)
    (pcheck
      ((natural? size) (bytevector? sha256))
      (%make-transfer-result size sha256 '#()))))

#|proc:transfer-result-encode
The `transfer-result-encode` procedure encodes protobuf record `message`.
The return value is a newly allocated wire-format bytevector.
|#
(define transfer-result-encode
  (lambda (message)
    (pcheck
      ((transfer-result? message))
      (%protobuf-encode-with-unknown
        (append
          (if (not (zero? (transfer-result-size message)))
              (list (list 1 'uint64 (transfer-result-size message)))
              '())
          (if (positive?
                (bytevector-length (transfer-result-sha256 message)))
              (list (list 2 'bytes (transfer-result-sha256 message)))
              '()))
        (transfer-result-unknown-fields message)))))

#|proc:transfer-result-encoded-size
The `transfer-result-encoded-size` procedure measures protobuf record `message`.
The return value is the encoded byte length.
|#
(define transfer-result-encoded-size
  (lambda (message)
    (pcheck
      ((transfer-result? message))
      (bytevector-length (transfer-result-encode message)))))

#|proc:bytevector->transfer-result
The `bytevector->transfer-result` procedure decodes protobuf bytevector `bytes`.
The return value is a message record that retains unknown fields.
|#
(define bytevector->transfer-result
  (lambda (bytes)
    (pcheck
      ((bytevector? bytes))
      (let ([decoder (make-protobuf-decoder bytes)]
            [size 0]
            [sha256 #vu8()])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(1) (set! size (protobuf-wire-field-value field))]
                [(2) (set! sha256 (protobuf-wire-field-value field))]
                [else (protobuf-decoder-preserve-field! decoder field)])
              (loop))))
        (%make-transfer-result
          size
          sha256
          (protobuf-decoder-unknown-fields decoder))))))

(define file-transfer-upload-method
  "/cspp.examples.transfer.FileTransfer/Upload")

#|proc:file-transfer-upload
The `file-transfer-upload` procedure starts the generated `Upload` RPC.
The `channel` parameter is a client gRPC channel; request values are encoded.
The return value is a decoded response record or a gRPC stream.
|#
(define file-transfer-upload
  (lambda (channel message*)
    (pcheck
      ((grpc-channel? channel) (vector? message*))
      (let ([stream (grpc-call/client-stream
                      channel
                      file-transfer-upload-method)])
        (dynamic-wind
          void
          (lambda ()
            (vector-for-each
              (lambda (message)
                (grpc-stream-send stream (file-chunk-encode message)))
              message*)
            (grpc-stream-close-send stream)
            (bytevector->transfer-result (grpc-stream-recv stream)))
          (lambda () (grpc-stream-close stream)))))))

(define file-transfer-download-method
  "/cspp.examples.transfer.FileTransfer/Download")

#|proc:file-transfer-download
The `file-transfer-download` procedure starts the generated `Download` RPC.
The `channel` parameter is a client gRPC channel; request values are encoded.
The return value is a decoded response record or a gRPC stream.
|#
(define file-transfer-download
  (lambda (channel request)
    (pcheck
      ((grpc-channel? channel) (file-chunk? request))
      (grpc-call/server-stream
        channel
        file-transfer-download-method
        (file-chunk-encode request)))))

#|proc:register-file-transfer-service!
The `register-file-transfer-service!` procedure registers generated handlers on `server`.
Each handler corresponds to one schema method and must follow its RPC shape.
The return value is `server`.
|#
(define register-file-transfer-service!
  (lambda (server upload-handler download-handler)
    (pcheck
      ((grpc-channel? server)
        (procedure? upload-handler download-handler))
      (grpc-register-service!
        server
        file-transfer-upload-method
        'client
        upload-handler)
      (grpc-register-service!
        server
        file-transfer-download-method
        'server
        download-handler)
      server)))

)
