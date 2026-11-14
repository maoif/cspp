(library (cspp crypto encoding)
  (export bytevector->hex
          hex->bytevector)
  (import (cspp chez)
          (cspp utils))

  #|proc:bytevector->hex
The `bytevector->hex` procedure returns the lowercase hexadecimal encoding of `bv`.
|#
  (define bytevector->hex
    (lambda (bv)
      (pcheck ([bytevector? bv])
              (let ([str (make-string (fx* 2 (bytevector-length bv)) #\space)])
                (let loop ([i 0] [j 0])
                  (if (fx= i (bytevector-length bv))
                      str
                      (let* ([x (bytevector-u8-ref bv i)]
                             [a (number->string (fxlogand x #xf) 16)]
                             [b (number->string (fxsrl x 4) 16)])
                        (string-set! str j (char-downcase (string-ref b 0)))
                        (string-set! str (fx1+ j) (char-downcase (string-ref a 0)))
                        (loop (fx1+ i) (fx+ j 2)))))))))

  (define hex-char->nibble
    (lambda (c)
      (cond
       [(char<=? #\0 c #\9) (fx- (char->integer c) (char->integer #\0))]
       [(char<=? #\a c #\f) (fx+ 10 (fx- (char->integer c) (char->integer #\a)))]
       [(char<=? #\A c #\F) (fx+ 10 (fx- (char->integer c) (char->integer #\A)))]
       [else #f])))

  #|proc:hex->bytevector
The `hex->bytevector` procedure decodes a hexadecimal string into a bytevector.
|#
  (define hex->bytevector
    (lambda (str)
      (pcheck ([string? str])
              (unless (fx= 0 (fxlogand (string-length str) 1))
                (errorf 'hex->bytevector "hex string length must be even, given ~a"
                        (string-length str)))
              (let ([bv (make-bytevector (fxsrl (string-length str) 1) 0)])
                (let loop ([i 0] [j 0])
                  (if (fx= i (string-length str))
                      bv
                      (let ([hi (hex-char->nibble (string-ref str i))]
                            [lo (hex-char->nibble (string-ref str (fx1+ i)))])
                        (unless (and hi lo)
                          (errorf 'hex->bytevector "invalid hex string ~s" str))
                        (bytevector-u8-set! bv j (fxlogor (fxsll hi 4) lo))
                        (loop (fx+ i 2) (fx1+ j)))))))))
  )
