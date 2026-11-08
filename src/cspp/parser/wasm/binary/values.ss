(library (cspp parser wasm binary values)
  (export <wasm-u32> <wasm-u64> <wasm-s32> <wasm-s33> <wasm-s64>
          <wasm-f32> <wasm-f64> <wasm-byte-vector> <wasm-name> <wasm-vector>)
  (import (cspp chez)
          (cspp parser combinator)
          (cspp parser wasm types)
          (cspp utils))

  (define wasm-unsigned-parser
    (lambda (width)
      (let ([maximum-bytes (quotient (+ width 6) 7)])
        (letrec ([next
                  (lambda (index shift value)
                    (<bind>
                     <u8>
                     (lambda (byte)
                       (let* ([payload (logand byte #x7f)]
                              [remaining (- width shift)]
                              [limit (expt 2 (min 7 remaining))]
                              [continued? (not (zero? (logand byte #x80)))])
                         (cond [(>= payload limit)
                                (<fail-with> "integer has nonzero unused bits")]
                               [(and continued? (= index (fx1- maximum-bytes)))
                                (<fail-with> "integer encoding is too long")]
                               [continued?
                                (next (fx1+ index) (+ shift 7)
                                      (+ value (ash payload shift)))]
                               [else
                                (<result> (+ value (ash payload shift)))])))))])
          (next 0 0 0)))))

  (define wasm-signed-parser
    (lambda (width)
      (let ([maximum-bytes (quotient (+ width 6) 7)])
        (letrec ([next
                  (lambda (index shift value)
                    (<bind>
                     <u8>
                     (lambda (byte)
                       (let* ([payload (logand byte #x7f)]
                              [remaining (- width shift)]
                              [used-bits (min 7 remaining)]
                              [value-mask (fx1- (expt 2 used-bits))]
                              [unused-mask (logxor #x7f value-mask)]
                              [negative?
                               (not (zero? (logand payload
                                                   (ash 1 (fx1- used-bits)))))]
                              [unused (logand payload unused-mask)]
                              [continued? (not (zero? (logand byte #x80)))]
                              [next-value
                               (+ value (ash (logand payload value-mask) shift))])
                         (cond [(not (= unused (if negative? unused-mask 0)))
                                (<fail-with> "integer has inconsistent unused bits")]
                               [(and continued? (= index (fx1- maximum-bytes)))
                                (<fail-with> "integer encoding is too long")]
                               [continued?
                                (next (fx1+ index) (+ shift 7) next-value)]
                               [negative?
                                (<result>
                                 (- next-value (ash 1 (+ shift used-bits))))]
                               [else (<result> next-value)])))))])
          (next 0 0 0)))))

  ;; Direct slices avoid an intermediate list and still belong to each invocation.
  (define float-parser
    (lambda (width size ref)
      (<map> (lambda (bytes)
               (make-wasm-float width (ref bytes 0 (endianness little))))
             (<u8vec> size))))

  (define strict-utf8->string
    (lambda (bytes)
      (bytevector->string
       bytes
       (make-transcoder (utf-8-codec)
                        (eol-style none)
                        (error-handling-mode raise)))))

  #|proc:<wasm-u32>
  The `<wasm-u32>` parser reads a bounded unsigned 32-bit LEB128 integer.
  |#
  (define <wasm-u32> (wasm-unsigned-parser 32))

  #|proc:<wasm-u64>
  The `<wasm-u64>` parser reads a bounded unsigned 64-bit LEB128 integer.
  |#
  (define <wasm-u64> (wasm-unsigned-parser 64))

  #|proc:<wasm-s32>
  The `<wasm-s32>` parser reads a bounded signed 32-bit LEB128 integer.
  |#
  (define <wasm-s32> (wasm-signed-parser 32))

  #|proc:<wasm-s33>
  The `<wasm-s33>` parser reads a bounded signed 33-bit LEB128 integer.
  |#
  (define <wasm-s33> (wasm-signed-parser 33))

  #|proc:<wasm-s64>
  The `<wasm-s64>` parser reads a bounded signed 64-bit LEB128 integer.
  |#
  (define <wasm-s64> (wasm-signed-parser 64))

  #|proc:<wasm-f32>
  The `<wasm-f32>` parser reads four little-endian bytes and preserves their exact bits.
  |#
  (define <wasm-f32> (float-parser 32 4 bytevector-u32-ref))

  #|proc:<wasm-f64>
  The `<wasm-f64>` parser reads eight little-endian bytes and preserves their exact bits.
  |#
  (define <wasm-f64> (float-parser 64 8 bytevector-u64-ref))

  #|proc:<wasm-byte-vector>
  The `<wasm-byte-vector>` parser reads a u32 length and that many bytes.
  |#
  (define <wasm-byte-vector>
    (<bind> <wasm-u32>
            (lambda (length) (<u8vec> length))))

  #|proc:<wasm-name>
  The `<wasm-name>` parser reads a length-prefixed, strictly valid UTF-8 string.
  |#
  (define <wasm-name>
    (<bind> <wasm-byte-vector>
            (lambda (bytes)
              (guard (condition [else (<fail-with> "invalid UTF-8 WebAssembly name")])
                (<result> (strict-utf8->string bytes))))))

  #|proc:<wasm-vector>
  The `<wasm-vector>` procedure returns a count-prefixed vector parser. `element-parser`
  has parser behavior `(BinaryInput -> Any)` and parses one vector element.
  |#
  (define-who (<wasm-vector> element-parser)
    (pcheck ([parser? element-parser])
            (<bind> <wasm-u32>
                    (lambda (count)
                      (<map> list->vector
                             (<rep> element-parser count))))))
  )
