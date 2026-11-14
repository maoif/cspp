(library (cspp format protobuf wire)
  (export protobuf-encode-varint protobuf-decode-varint
          protobuf-encode-signed-varint protobuf-decode-signed-varint
          protobuf-encode-zigzag protobuf-decode-zigzag
          protobuf-encode-fixed32 protobuf-decode-fixed32
          protobuf-encode-fixed64 protobuf-decode-fixed64
          protobuf-encode-sfixed32 protobuf-decode-sfixed32
          protobuf-encode-sfixed64 protobuf-decode-sfixed64
          protobuf-encode-float protobuf-decode-float
          protobuf-encode-double protobuf-decode-double
          protobuf-encode-bool protobuf-decode-bool
          protobuf-encode-enum protobuf-decode-enum
          protobuf-encode-bytes protobuf-decode-bytes
          protobuf-encode-string protobuf-decode-string
          protobuf-encode-embedded-message protobuf-decode-embedded-message
          protobuf-encode-tag protobuf-decode-tag
          protobuf-encode-field protobuf-encode-message
          protobuf-decoder? make-protobuf-decoder
          protobuf-decoder-eof? protobuf-decoder-index protobuf-decoder-limit
          protobuf-decoder-recursion-depth protobuf-decoder-recursion-limit
          protobuf-decoder-unknown-fields protobuf-decoder-next-field
          protobuf-decoder-preserve-field!
          protobuf-wire-field? protobuf-wire-field-number
          protobuf-wire-field-wire-type protobuf-wire-field-value
          protobuf-wire-field-raw)
  (import (cspp chez)
          (only (cspp utils) natural? pcheck))

  (define protobuf-max-field-number 536870911)
  (define protobuf-max-u64 #xffffffffffffffff)
  (define protobuf-min-s64 (- (bitwise-arithmetic-shift 1 63)))
  (define protobuf-max-s64 (- (bitwise-arithmetic-shift 1 63) 1))

  (define protobuf-u64?
    (lambda (value)
      (and (integer? value) (exact? value) (<= 0 value protobuf-max-u64))))

  (define protobuf-bytevector-append
    (lambda (part*)
      (let ([size (fold-left (lambda (total part) (+ total (bytevector-length part))) 0 part*)])
        (let ([out (make-bytevector size 0)])
          (let loop ([rest part*] [index 0])
            (unless (null? rest)
              (let ([part (car rest)])
                (bytevector-copy! part 0 out index (bytevector-length part))
                (loop (cdr rest) (+ index (bytevector-length part))))))
          out))))

  (define protobuf-bytevector-slice
    (lambda (bytes start stop)
      (let ([out (make-bytevector (- stop start) 0)])
        (bytevector-copy! bytes start out 0 (- stop start))
        out)))

  (define protobuf-unsigned-varint
    (lambda (value)
      (let ([size (let loop ([n value] [count 1])
                    (if (< n 128) count (loop (bitwise-arithmetic-shift n -7) (+ count 1))))])
        (let ([out (make-bytevector size 0)])
          (let loop ([n value] [index 0])
            (if (< n 128)
                (begin (bytevector-u8-set! out index n) out)
                (begin
                  (bytevector-u8-set! out index (bitwise-ior #x80 (bitwise-and n #x7f)))
                  (loop (bitwise-arithmetic-shift n -7) (+ index 1)))))))))

  (define protobuf-decode-varint/at
    (lambda (who bytes start limit)
      (let loop ([index start] [shift 0] [value 0] [count 0])
        (when (= index limit) (errorf who "truncated varint"))
        (when (= count 10) (errorf who "varint exceeds 64 bits"))
        (let ([octet (bytevector-u8-ref bytes index)])
          (when (and (= count 9) (> octet 1)) (errorf who "varint exceeds 64 bits"))
          (let ([next (bitwise-ior value
                                   (bitwise-arithmetic-shift (bitwise-and octet #x7f) shift))])
            (if (zero? (bitwise-and octet #x80))
                (values next (+ index 1))
                (loop (+ index 1) (+ shift 7) next (+ count 1))))))))

  #|proc:protobuf-encode-varint
The `protobuf-encode-varint` procedure encodes unsigned 64-bit `value` as a protobuf varint.
The return value is a newly allocated bytevector.
|#
  (define protobuf-encode-varint
    (lambda (value)
      (pcheck ([protobuf-u64? value]) (protobuf-unsigned-varint value))))

  #|proc:protobuf-decode-varint
The `protobuf-decode-varint` procedure decodes the complete protobuf-varint bytevector `bytes`.
The return value is its unsigned 64-bit integer value.
|#
  (define protobuf-decode-varint
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (let-values ([(value index) (protobuf-decode-varint/at 'protobuf-decode-varint
                                                                 bytes 0 (bytevector-length bytes))])
          (unless (= index (bytevector-length bytes))
            (errorf 'protobuf-decode-varint "trailing bytes after varint"))
          value))))

  #|proc:protobuf-encode-signed-varint
The `protobuf-encode-signed-varint` procedure encodes signed 64-bit integer `value` as a varint.
Negative values use the protobuf two's-complement ten-byte representation.
The return value is a newly allocated bytevector.
|#
  (define protobuf-encode-signed-varint
    (lambda (value)
      (pcheck ([integer? value])
        (unless (<= protobuf-min-s64 value protobuf-max-s64)
          (errorf 'protobuf-encode-signed-varint "signed 64-bit integer required: ~s" value))
        (protobuf-encode-varint (bitwise-and value protobuf-max-u64)))))

  #|proc:protobuf-decode-signed-varint
The `protobuf-decode-signed-varint` procedure decodes complete varint bytevector `bytes`.
The return value is the corresponding signed 64-bit integer.
|#
  (define protobuf-decode-signed-varint
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (let ([value (protobuf-decode-varint bytes)])
          (if (bitwise-bit-set? value 63)
              (- value (+ protobuf-max-u64 1))
              value)))))

  #|proc:protobuf-encode-zigzag
The `protobuf-encode-zigzag` procedure encodes signed 64-bit `value` with protobuf zigzag.
The return value is a newly allocated bytevector.
|#
  (define protobuf-encode-zigzag
    (lambda (value)
      (pcheck ([integer? value])
        (protobuf-encode-varint
         (bitwise-and (bitwise-xor (bitwise-arithmetic-shift value 1)
                                   (bitwise-arithmetic-shift value -63))
                      protobuf-max-u64)))))

  #|proc:protobuf-decode-zigzag
The `protobuf-decode-zigzag` procedure decodes zigzag protobuf-varint bytevector `bytes`.
The return value is its signed integer value.
|#
  (define protobuf-decode-zigzag
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (let ([value (protobuf-decode-varint bytes)])
          (bitwise-xor (bitwise-arithmetic-shift value -1)
                       (- (bitwise-and value 1)))))))

  (define protobuf-fixed?
    (lambda (bits value)
      (and (integer? value) (exact? value)
           (<= 0 value (- (bitwise-arithmetic-shift 1 bits) 1)))))

  #|proc:protobuf-encode-fixed32
The `protobuf-encode-fixed32` procedure encodes unsigned 32-bit `value` in little-endian order.
The return value is a four-byte bytevector.
|#
  (define protobuf-encode-fixed32
    (lambda (value)
      (pcheck ([integer? value])
        (unless (protobuf-fixed? 32 value) (errorf 'protobuf-encode-fixed32 "invalid value: ~s" value))
        (let ([out (make-bytevector 4 0)])
          (bytevector-u32-set! out 0 value (endianness little)) out))))

  #|proc:protobuf-decode-fixed32
The `protobuf-decode-fixed32` procedure decodes four little-endian bytes in `bytes`.
The return value is an unsigned 32-bit integer.
|#
  (define protobuf-decode-fixed32
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (unless (= (bytevector-length bytes) 4) (errorf 'protobuf-decode-fixed32 "four bytes required"))
        (bytevector-u32-ref bytes 0 (endianness little)))))

  #|proc:protobuf-encode-fixed64
The `protobuf-encode-fixed64` procedure encodes unsigned 64-bit `value` in little-endian order.
The return value is an eight-byte bytevector.
|#
  (define protobuf-encode-fixed64
    (lambda (value)
      (pcheck ([protobuf-u64? value])
        (let ([out (make-bytevector 8 0)])
          (bytevector-u64-set! out 0 value (endianness little)) out))))

  #|proc:protobuf-decode-fixed64
The `protobuf-decode-fixed64` procedure decodes eight little-endian bytes in `bytes`.
The return value is an unsigned 64-bit integer.
|#
  (define protobuf-decode-fixed64
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (unless (= (bytevector-length bytes) 8) (errorf 'protobuf-decode-fixed64 "eight bytes required"))
        (bytevector-u64-ref bytes 0 (endianness little)))))

  #|proc:protobuf-encode-sfixed32
The `protobuf-encode-sfixed32` procedure encodes signed 32-bit `value` in little-endian order.
The return value is a four-byte bytevector.
|#
  (define protobuf-encode-sfixed32
    (lambda (value)
      (pcheck ([integer? value])
        (unless (<= -2147483648 value 2147483647)
          (errorf 'protobuf-encode-sfixed32 "signed 32-bit integer required: ~s" value))
        (let ([out (make-bytevector 4 0)])
          (bytevector-s32-set! out 0 value (endianness little))
          out))))

  #|proc:protobuf-decode-sfixed32
The `protobuf-decode-sfixed32` procedure decodes four little-endian bytes in `bytes`.
The return value is a signed 32-bit integer.
|#
  (define protobuf-decode-sfixed32
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (unless (= (bytevector-length bytes) 4)
          (errorf 'protobuf-decode-sfixed32 "four bytes required"))
        (bytevector-s32-ref bytes 0 (endianness little)))))

  #|proc:protobuf-encode-sfixed64
The `protobuf-encode-sfixed64` procedure encodes signed 64-bit `value` in little-endian order.
The return value is an eight-byte bytevector.
|#
  (define protobuf-encode-sfixed64
    (lambda (value)
      (pcheck ([integer? value])
        (unless (<= protobuf-min-s64 value protobuf-max-s64)
          (errorf 'protobuf-encode-sfixed64 "signed 64-bit integer required: ~s" value))
        (let ([out (make-bytevector 8 0)])
          (bytevector-s64-set! out 0 value (endianness little))
          out))))

  #|proc:protobuf-decode-sfixed64
The `protobuf-decode-sfixed64` procedure decodes eight little-endian bytes in `bytes`.
The return value is a signed 64-bit integer.
|#
  (define protobuf-decode-sfixed64
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (unless (= (bytevector-length bytes) 8)
          (errorf 'protobuf-decode-sfixed64 "eight bytes required"))
        (bytevector-s64-ref bytes 0 (endianness little)))))

  #|proc:protobuf-encode-float
The `protobuf-encode-float` procedure encodes real number `value` as IEEE-754 binary32.
The return value is a four-byte little-endian bytevector.
|#
  (define protobuf-encode-float
    (lambda (value)
      (pcheck ([real? value])
        (let ([out (make-bytevector 4 0)])
          (bytevector-ieee-single-set! out 0 value (endianness little))
          out))))

  #|proc:protobuf-decode-float
The `protobuf-decode-float` procedure decodes four-byte little-endian bytevector `bytes`.
The return value is the represented IEEE-754 binary32 real number.
|#
  (define protobuf-decode-float
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (unless (= (bytevector-length bytes) 4)
          (errorf 'protobuf-decode-float "four bytes required"))
        (bytevector-ieee-single-ref bytes 0 (endianness little)))))

  #|proc:protobuf-encode-double
The `protobuf-encode-double` procedure encodes real number `value` as IEEE-754 binary64.
The return value is an eight-byte little-endian bytevector.
|#
  (define protobuf-encode-double
    (lambda (value)
      (pcheck ([real? value])
        (let ([out (make-bytevector 8 0)])
          (bytevector-ieee-double-set! out 0 value (endianness little))
          out))))

  #|proc:protobuf-decode-double
The `protobuf-decode-double` procedure decodes eight-byte little-endian bytevector `bytes`.
The return value is the represented IEEE-754 binary64 real number.
|#
  (define protobuf-decode-double
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (unless (= (bytevector-length bytes) 8)
          (errorf 'protobuf-decode-double "eight bytes required"))
        (bytevector-ieee-double-ref bytes 0 (endianness little)))))

  #|proc:protobuf-encode-bool
The `protobuf-encode-bool` procedure encodes boolean `value` as a protobuf varint.
The return value is a one-byte bytevector.
|#
  (define protobuf-encode-bool
    (lambda (value)
      (pcheck ([boolean? value])
        (protobuf-encode-varint (if value 1 0)))))

  #|proc:protobuf-decode-bool
The `protobuf-decode-bool` procedure decodes complete varint bytevector `bytes` as a boolean.
The return value is `#f` for zero and `#t` for every nonzero value.
|#
  (define protobuf-decode-bool
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (not (zero? (protobuf-decode-varint bytes))))))

  #|proc:protobuf-encode-enum
The `protobuf-encode-enum` procedure encodes signed 32-bit enum number `value` as a varint.
The return value is a newly allocated bytevector.
|#
  (define protobuf-encode-enum
    (lambda (value)
      (pcheck ([integer? value])
        (unless (<= -2147483648 value 2147483647)
          (errorf 'protobuf-encode-enum "signed 32-bit integer required: ~s" value))
        (protobuf-encode-signed-varint value))))

  #|proc:protobuf-decode-enum
The `protobuf-decode-enum` procedure decodes complete varint bytevector `bytes` as an enum number.
The return value is a signed 32-bit integer.
|#
  (define protobuf-decode-enum
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (let ([value (protobuf-decode-signed-varint bytes)])
          (unless (<= -2147483648 value 2147483647)
            (errorf 'protobuf-decode-enum "enum number exceeds 32 bits: ~s" value))
          value))))

  #|proc:protobuf-encode-bytes
The `protobuf-encode-bytes` procedure copies bytevector `value` for a length-delimited payload.
The return value is a newly allocated bytevector without its tag or length prefix.
|#
  (define protobuf-encode-bytes
    (lambda (value)
      (pcheck ([bytevector? value])
        (bytevector-copy value))))

  #|proc:protobuf-decode-bytes
The `protobuf-decode-bytes` procedure copies length-delimited payload bytevector `bytes`.
The return value is a newly allocated bytevector.
|#
  (define protobuf-decode-bytes
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (bytevector-copy bytes))))

  #|proc:protobuf-encode-string
The `protobuf-encode-string` procedure UTF-8 encodes string `value` as a field payload.
The return value is a newly allocated bytevector without its tag or length prefix.
|#
  (define protobuf-encode-string
    (lambda (value)
      (pcheck ([string? value])
        (string->utf8 value))))

  #|proc:protobuf-decode-string
The `protobuf-decode-string` procedure UTF-8 decodes payload bytevector `bytes`.
The return value is the decoded string; malformed UTF-8 raises an exception.
|#
  (define protobuf-decode-string
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (utf8->string bytes))))

  #|proc:protobuf-encode-embedded-message
The `protobuf-encode-embedded-message` procedure copies encoded message bytevector `message`.
The return value is a newly allocated payload bytevector without its tag or length prefix.
|#
  (define protobuf-encode-embedded-message
    (lambda (message)
      (pcheck ([bytevector? message])
        (bytevector-copy message))))

  #|proc:protobuf-decode-embedded-message
The `protobuf-decode-embedded-message` procedure creates a child decoder for payload `bytes`.
`parent` is the decoder whose recursion limit the child inherits, or `#f` for a root decoder.
The return value is a bounded protobuf decoder.
|#
  (define protobuf-decode-embedded-message
    (lambda (bytes parent)
      (pcheck ([bytevector? bytes] [(lambda (x) (or (not x) (protobuf-decoder? x))) parent])
        (if parent
            (let ([depth (+ (protobuf-decoder-recursion-depth parent) 1)])
              (when (> depth (protobuf-decoder-recursion-limit parent))
                (errorf 'protobuf-decode-embedded-message "recursion limit exceeded"))
              (%make-protobuf-decoder bytes 0 (bytevector-length bytes) depth
                                      (protobuf-decoder-recursion-limit parent) '()))
            (make-protobuf-decoder bytes)))))

  #|proc:protobuf-encode-tag
The `protobuf-encode-tag` procedure encodes positive field `number` and numeric `wire-type`.
The return value is a newly allocated protobuf tag bytevector.
|#
  (define protobuf-encode-tag
    (lambda (number wire-type)
      (pcheck ([integer? number wire-type])
        (unless (and (positive? number) (<= number protobuf-max-field-number))
          (errorf 'protobuf-encode-tag "invalid field number: ~s" number))
        (unless (memv wire-type '(0 1 2 5))
          (errorf 'protobuf-encode-tag "invalid wire type: ~s" wire-type))
        (protobuf-encode-varint
         (bitwise-ior (bitwise-arithmetic-shift number 3) wire-type)))))

  #|proc:protobuf-decode-tag
The `protobuf-decode-tag` procedure decodes complete tag bytevector `bytes`.
It returns the field number and wire type as two values.
|#
  (define protobuf-decode-tag
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (let* ([tag (protobuf-decode-varint bytes)]
               [number (bitwise-arithmetic-shift tag -3)]
               [wire-type (bitwise-and tag 7)])
          (unless (and (positive? number) (<= number protobuf-max-field-number))
            (errorf 'protobuf-decode-tag "invalid field number: ~s" number))
          (unless (memv wire-type '(0 1 2 5))
            (errorf 'protobuf-decode-tag "invalid wire type: ~s" wire-type))
          (values number wire-type)))))

  #|record:protobuf-wire-field
The `protobuf-wire-field` record is an immutable decoded protobuf field occurrence.
Number is the positive field number, wire-type is `0`, `1`, `2`, or `5`, value is the decoded
scalar or payload bytes, and raw is the complete copied encoded field bytes.
|#
  (define-record-type (protobuf-wire-field %make-protobuf-wire-field protobuf-wire-field?)
    (sealed #t)
    (opaque #f)
    (fields (immutable number protobuf-wire-field-number)
            (immutable wire-type protobuf-wire-field-wire-type)
            (immutable value protobuf-wire-field-value)
            (immutable raw protobuf-wire-field-raw)))

  #|record:protobuf-decoder
The `protobuf-decoder` record is a mutable cursor over a retained source bytevector.
It tracks current index, bounded limit, recursion depth and limit, and copied unknown fields.
Decoder operations advance the index; the source remains owned by the caller and must not change.
|#
  (define-record-type (protobuf-decoder %make-protobuf-decoder protobuf-decoder?)
    (sealed #t)
    (opaque #f)
    (fields (immutable source protobuf-decoder-source)
            (mutable index protobuf-decoder-index protobuf-decoder-index-set!)
            (immutable limit protobuf-decoder-limit)
            (immutable recursion-depth protobuf-decoder-recursion-depth)
            (immutable recursion-limit protobuf-decoder-recursion-limit)
            (mutable unknown-fields protobuf-decoder-unknown-field-list
                     protobuf-decoder-unknown-field-list-set!)))

  #|proc:make-protobuf-decoder
The `make-protobuf-decoder` procedure creates a bounded decoder over bytevector `bytes`.
With `recursion-limit`, nested message depth is limited to that nonnegative integer; it defaults
to 100. The return value is a new decoder positioned at the beginning of `bytes`.
|#
  (define make-protobuf-decoder
    (case-lambda
      [(bytes) (make-protobuf-decoder bytes 100)]
      [(bytes recursion-limit)
       (pcheck ([bytevector? bytes] [natural? recursion-limit])
         (%make-protobuf-decoder bytes 0 (bytevector-length bytes) 0 recursion-limit '()))]))

  #|proc:protobuf-decoder-eof?
The `protobuf-decoder-eof?` procedure tests whether `decoder` has consumed its bounded input.
The return value is a boolean.
|#
  (define protobuf-decoder-eof?
    (lambda (decoder)
      (pcheck ([protobuf-decoder? decoder])
        (= (protobuf-decoder-index decoder) (protobuf-decoder-limit decoder)))))

  #|proc:protobuf-decoder-unknown-fields
The `protobuf-decoder-unknown-fields` procedure returns fields preserved in `decoder`.
The return value is a vector of exact raw wire-format bytevectors in encounter order.
|#
  (define protobuf-decoder-unknown-fields
    (lambda (decoder)
      (pcheck ([protobuf-decoder? decoder])
        (list->vector (reverse (protobuf-decoder-unknown-field-list decoder))))))

  #|proc:protobuf-decoder-preserve-field!
The `protobuf-decoder-preserve-field!` procedure retains unknown wire `field` in `decoder`.
The return value is unspecified.
|#
  (define protobuf-decoder-preserve-field!
    (lambda (decoder field)
      (pcheck ([protobuf-decoder? decoder] [protobuf-wire-field? field])
        (protobuf-decoder-unknown-field-list-set!
         decoder
         (cons (protobuf-wire-field-raw field)
               (protobuf-decoder-unknown-field-list decoder))))))

  #|proc:protobuf-decoder-next-field
The `protobuf-decoder-next-field` procedure consumes the next field from bounded `decoder`.
The return value is a wire-field record, or `#f` when no input remains. Length-delimited values
are returned as copied bytevectors, and malformed or truncated fields raise an exception.
|#
  (define protobuf-decoder-next-field
    (lambda (decoder)
      (pcheck ([protobuf-decoder? decoder])
        (if (protobuf-decoder-eof? decoder)
            #f
            (let* ([bytes (protobuf-decoder-source decoder)]
                   [start (protobuf-decoder-index decoder)]
                   [limit (protobuf-decoder-limit decoder)])
              (let-values ([(tag payload-start)
                            (protobuf-decode-varint/at
                             'protobuf-decoder-next-field bytes start limit)])
                (let ([number (bitwise-arithmetic-shift tag -3)]
                      [wire-type (bitwise-and tag 7)])
                  (unless (and (positive? number) (<= number protobuf-max-field-number))
                    (errorf 'protobuf-decoder-next-field "invalid field number: ~s" number))
                  (let-values ([(value stop)
                                (case wire-type
                                  [(0)
                                   (protobuf-decode-varint/at
                                    'protobuf-decoder-next-field bytes payload-start limit)]
                                  [(1)
                                   (let ([stop (+ payload-start 8)])
                                     (when (> stop limit)
                                       (errorf 'protobuf-decoder-next-field
                                               "truncated fixed64 field"))
                                     (values (bytevector-u64-ref bytes payload-start
                                                                 (endianness little))
                                             stop))]
                                  [(2)
                                   (let-values ([(length data-start)
                                                 (protobuf-decode-varint/at
                                                  'protobuf-decoder-next-field
                                                  bytes payload-start limit)])
                                     (let ([stop (+ data-start length)])
                                       (when (> stop limit)
                                         (errorf 'protobuf-decoder-next-field
                                                 "length exceeds remaining input"))
                                       (values (protobuf-bytevector-slice bytes data-start stop)
                                               stop)))]
                                  [(5)
                                   (let ([stop (+ payload-start 4)])
                                     (when (> stop limit)
                                       (errorf 'protobuf-decoder-next-field
                                               "truncated fixed32 field"))
                                     (values (bytevector-u32-ref bytes payload-start
                                                                 (endianness little))
                                             stop))]
                                  [else
                                   (errorf 'protobuf-decoder-next-field
                                           "invalid wire type: ~s" wire-type)])])
                    (protobuf-decoder-index-set! decoder stop)
                    (%make-protobuf-wire-field
                     number wire-type value (protobuf-bytevector-slice bytes start stop))))))))))

  (define protobuf-field-payload
    (lambda (type value)
      (case type
       [(bool) (protobuf-encode-varint (if value 1 0))]
       [(enum uint32 uint64) (protobuf-encode-varint value)]
       [(int32 int64) (protobuf-encode-signed-varint value)]
       [(sint32 sint64) (protobuf-encode-zigzag value)]
       [(fixed32) (protobuf-encode-fixed32 value)]
       [(fixed64) (protobuf-encode-fixed64 value)]
       [(sfixed32) (protobuf-encode-sfixed32 value)]
       [(sfixed64) (protobuf-encode-sfixed64 value)]
       [(float) (protobuf-encode-float value)]
       [(double) (protobuf-encode-double value)]
       [(bytes) value]
       [(string) (string->utf8 value)]
       [(message) value]
       [else (errorf 'protobuf-encode-field "unsupported field type: ~s" type)])))

  #|proc:protobuf-encode-field
The `protobuf-encode-field` procedure encodes field number `number`, protobuf `type`, and `value`.
The return value is a newly allocated wire-format bytevector.
|#
  (define protobuf-encode-field
    (lambda (number type value)
      (pcheck ([integer? number] [symbol? type])
        (unless (and (positive? number) (<= number protobuf-max-field-number))
          (errorf 'protobuf-encode-field "invalid field number: ~s" number))
        (let* ([wire-type (case type
                            [(bool enum int32 int64 uint32 uint64 sint32 sint64) 0]
                            [(double fixed64 sfixed64) 1]
                            [(bytes string message) 2]
                            [(float fixed32 sfixed32) 5]
                            [else (errorf 'protobuf-encode-field "unsupported field type: ~s" type)])]
               [payload (protobuf-field-payload type value)]
               [tag (protobuf-encode-varint (bitwise-ior (bitwise-arithmetic-shift number 3) wire-type))])
          (if (= wire-type 2)
              (protobuf-bytevector-append (list tag (protobuf-encode-varint (bytevector-length payload)) payload))
              (protobuf-bytevector-append (list tag payload)))))))

  #|proc:protobuf-encode-message
The `protobuf-encode-message` procedure encodes `field*`, a list of `(number type value)` fields.
The return value is one newly allocated protobuf message bytevector.
|#
  (define protobuf-encode-message
    (lambda (field*)
      (pcheck ([list? field*])
        (protobuf-bytevector-append
         (map (lambda (field)
                (unless (and (list? field) (= (length field) 3))
                  (errorf 'protobuf-encode-message "field must be (number type value): ~s" field))
                (protobuf-encode-field (car field) (cadr field) (caddr field)))
              field*)))))
)
