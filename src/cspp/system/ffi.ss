(library (cspp system ffi)
  (export ffi-result-ref)
  (import (cspp chez) (cspp utils) (cspp system errors))

  (define $context-or-empty
    (lambda (x)
      (if (list? x) x '())))

  (define $operation->symbol
    (lambda (x)
      (cond [(symbol? x) x]
            [(string? x) (string->symbol x)]
            [else #f])))

  (define $errno-condition
    (lambda (operation code message context)
      (if (memv code '(1 13))
          (raise (make-system-permission-error operation code message context))
          (raise (make-system-error operation code message context)))))

  #|proc:ffi-result-ref
  The `ffi-result-ref` procedure decodes a tagged C FFI result vector.
  The `result` parameter is a vector tagged with a symbol from a C helper.
  The `ok` tag returns a value; error tags raise system conditions.
  |#
  (define-who ffi-result-ref
    (lambda (result)
      (pcheck-who who ([vector? result])
              (let ([tag (and (fx< 0 (vector-length result)) (vector-ref result 0))])
                (cond
                  [(and (eq? tag 'ok) (fx= 2 (vector-length result)))
                   (vector-ref result 1)]
                  [(and (eq? tag 'errno) (fx= 5 (vector-length result)))
                   ($errno-condition ($operation->symbol (vector-ref result 1))
                                     (vector-ref result 2)
                                     (vector-ref result 3)
                                     ($context-or-empty (vector-ref result 4)))]
                  [(and (eq? tag 'not-found) (fx= 3 (vector-length result)))
                   (raise (make-system-not-found-error ($operation->symbol (vector-ref result 1)) "not found"
                                                       ($context-or-empty (vector-ref result 2))))]
                  [(and (eq? tag 'unsupported) (fx= 2 (vector-length result)))
                   (raise (make-system-unsupported-error ($operation->symbol (vector-ref result 1)) "unsupported"))]
                  [(and (eq? tag 'timeout) (fx= 3 (vector-length result)))
                   (raise (make-system-timeout-error ($operation->symbol (vector-ref result 1)) "timeout"
                                                     ($context-or-empty (vector-ref result 2))))]
                  [else (errorf 'ffi-result-ref "invalid FFI result: ~a" result)])))))
  )
