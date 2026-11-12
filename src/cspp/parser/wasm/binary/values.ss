(library (cspp parser wasm binary values)
  (export <wasm-u32> <wasm-u64> <wasm-s32> <wasm-s33> <wasm-s64>
          <wasm-f32> <wasm-f64> <wasm-byte-vector> <wasm-name> <wasm-vector>)
  (import (cspp chez)
          (cspp parser combinator)
          (cspp parser private)
          (cspp parser wasm types)
          (cspp utils))

  (define wasm-unsigned-parser
    (lambda (width)
      (define-parser unsigned-parser
        (let loop ([shift 0] [number 0] [current inp])
          (let-values ([(status byte next) (parser-call <u8> current state lvl)])
            (if (not status)
                (values #f #f next)
                (let* ([payload (fxlogand byte #x7f)]
                       [remaining (- width shift)]
                       [continued? (fx>= byte #x80)])
                  (cond
                    [(>= payload (ash 1 (min 7 remaining)))
                     (parser-call (<fail-with> "integer has nonzero unused bits")
                                  next state lvl)]
                    [(and continued? (<= remaining 7))
                     (parser-call (<fail-with> "integer encoding is too long") next state lvl)]
                    [continued?
                     (loop (fx+ shift 7) (+ number (ash payload shift)) next)]
                    [else (values #t (+ number (ash payload shift)) next)]))))))
      unsigned-parser))

  (define float-parser
    (lambda (width size integer-parser)
      (let ([truncated-parser (<u8vec> size)])
        (define-parser bits-parser
          ;; Retain the byte-slice diagnostic on truncation without copying successful reads.
          (if (> (+ (input-pos inp) size) (input-len inp))
              (parser-call truncated-parser inp state lvl)
              (parser-call integer-parser inp state lvl)))
        (<map> (lambda (bits) (make-wasm-float width bits)) bits-parser))))

  (define strict-utf8->string
    (lambda (bytes) (parser-decode-bytes bytes 'utf-8)))

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
  (define <wasm-s32> (<sleb128> 32))

  #|proc:<wasm-s33>
  The `<wasm-s33>` parser reads a bounded signed 33-bit LEB128 integer.
  |#
  (define <wasm-s33> (<sleb128> 33))

  #|proc:<wasm-s64>
  The `<wasm-s64>` parser reads a bounded signed 64-bit LEB128 integer.
  |#
  (define <wasm-s64> (<sleb128> 64))

  #|proc:<wasm-f32>
  The `<wasm-f32>` parser reads four little-endian bytes and preserves their exact bits.
  |#
  (define <wasm-f32> (float-parser 32 4 <u32le>))

  #|proc:<wasm-f64>
  The `<wasm-f64>` parser reads eight little-endian bytes and preserves their exact bits.
  |#
  (define <wasm-f64> (float-parser 64 8 <u64le>))

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
              (guard (condition
                      [(i/o-decoding-error? condition) (<fail-with> "invalid UTF-8 WebAssembly name")])
                (<result> (strict-utf8->string bytes))))))

  #|proc:<wasm-vector>
  The `<wasm-vector>` procedure returns a count-prefixed vector parser. `element-parser`
  has parser behavior `(BinaryInput -> Any)` and parses one vector element.
  |#
  (define-who (<wasm-vector> element-parser)
    (pcheck ([parser? element-parser])
            ;; Normalize custom failures through the same mapper used by the list path.
            (let ([checked-element (<map> values element-parser)])
              (<bind> <wasm-u32>
                      (lambda (count)
                        (define-parser vector-parser
                          ;; Do not preallocate from a count larger than the input. Retain
                          ;; the list path for this case, including zero-width elements.
                          (if (> count (- (input-len inp) (input-pos inp)))
                              (parser-call (<map> list->vector (<rep> element-parser count))
                                           inp state lvl)
                              (let ([result (make-vector count)])
                                (let loop ([index 0] [current inp])
                                  (if (fx= index count)
                                      (values #t result current)
                                      (let-values ([(status value next)
                                                    (parser-call checked-element
                                                                 current state lvl)])
                                        (if status
                                            (begin
                                              (vector-set! result index value)
                                              (loop (fx1+ index) next))
                                            (values #f #f next))))))))
                        vector-parser)))))
  )
