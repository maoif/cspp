(library (cspp net private lws-transport)
  (export lws-transport-encode-headers lws-transport-decode-headers
          lws-transport-redirect-response?
          lws-transport-start! lws-transport-submit-body! lws-transport-consume-body!)
  (import (cspp chez)
          (only (cspp utils) define-who natural? pcheck)
          (cspp net errors)
          (cspp net private http)
          (cspp net private lws-reactor))

  #|proc:lws-transport-redirect-response?
Returns whether `request` policy follows the redirect described by integer `status` and header
alist `headers`. Reducers discard intermediate redirect bodies before invoking a user sink.
|#
  (define lws-transport-redirect-response?
    (lambda (request status headers)
      (pcheck ([normalized-http-request? request] [integer? status] [list? headers])
        (and (http-request-policy-follow-redirects? (normalized-http-request-policy request))
             (memv status '(301 302 303 307 308))
             (exists (lambda (entry) (string-ci=? "location" (car entry))) headers)
             #t))))

  (define header-list?
    (lambda (headers)
      (and (list? headers)
           (for-all (lambda (entry)
                      (and (pair? entry) (string? (car entry)) (string? (cdr entry))))
                    headers))))

  (define contains-nul?
    (lambda (text)
      (let ([size (string-length text)])
        (let loop ([index 0])
          (and (fx< index size)
               (or (char=? (string-ref text index) #\nul)
                   (loop (fx1+ index))))))))

  #|proc:lws-transport-encode-headers
  The `lws-transport-encode-headers` procedure encodes string pairs in `headers` as native
  NUL-delimited metadata. It returns a fresh bytevector and rejects embedded NULs and empty names.
  |#
  (define-who lws-transport-encode-headers
    (lambda (headers)
      (pcheck ([header-list? headers])
        (let* ([parts
                (map (lambda (entry)
                       (when (or (zero? (string-length (car entry)))
                                 (contains-nul? (car entry))
                                 (contains-nul? (cdr entry)))
                         (raise-net-error who 'http "invalid header metadata" entry))
                       (cons (string->utf8 (car entry)) (string->utf8 (cdr entry))))
                     headers)]
               [size (fold-left (lambda (size entry)
                                  (+ size 2 (bytevector-length (car entry))
                                     (bytevector-length (cdr entry))))
                                0 parts)]
               [bytes (make-bytevector size 0)])
          (let loop ([parts parts] [offset 0])
            (unless (null? parts)
              (let* ([name (caar parts)] [value (cdar parts)]
                     [value-start (fx+ offset (bytevector-length name) 1)])
                (bytevector-copy! name 0 bytes offset (bytevector-length name))
                (bytevector-copy! value 0 bytes value-start (bytevector-length value))
                (loop (cdr parts) (fx+ value-start (bytevector-length value) 1)))))
          bytes))))

  #|proc:lws-transport-decode-headers
  The `lws-transport-decode-headers` procedure decodes native NUL-delimited `bytes` to an alist
  of string pairs, preserving order and repeated headers. Truncation or empty names raise a network
  error; a valid prefix is never returned for malformed metadata.
  |#
  (define-who lws-transport-decode-headers
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (let ([size (bytevector-length bytes)])
          (define terminator
            (lambda (start)
              (let loop ([index start])
                (cond
                 [(fx= index size)
                  (raise-net-error who 'http "truncated header metadata" bytes)]
                 [(fxzero? (bytevector-u8-ref bytes index)) index]
                 [else (loop (fx1+ index))]))))
          (define text
            (lambda (start end)
              (let ([part (make-bytevector (fx- end start) 0)])
                (bytevector-copy! bytes start part 0 (bytevector-length part))
                (utf8->string part))))
          (let loop ([offset 0] [headers '()])
            (if (fx= offset size)
                (reverse headers)
                (let* ([name-end (terminator offset)]
                       [value-start (fx1+ name-end)]
                       [value-end (terminator value-start)])
                  (when (fx= offset name-end)
                    (raise-net-error who 'http "empty header name" bytes))
                  (loop (fx1+ value-end)
                        (cons (cons (text offset name-end) (text value-start value-end))
                              headers)))))))))

  #|proc:lws-transport-start!
  The `lws-transport-start!` procedure submits normalized `request` to `reactor`, using `alpn`
  as the protocol offer. `connection-id`, `stream-id`, and `generation` route the logical operation.
  It returns command acceptance; execution failures are delivered through the reactor operation.
  |#
  (define lws-transport-start!
    (lambda (reactor connection-id stream-id generation request alpn)
      (pcheck ([lws-reactor? reactor] [natural? connection-id stream-id generation]
               [normalized-http-request? request] [string? alpn])
        (lws-reactor-client-start!
         reactor connection-id stream-id generation
         (normalized-http-request-host request) (normalized-http-request-port request)
         (normalized-http-request-tls? request) (normalized-http-request-method request)
         (normalized-http-request-host request) (normalized-http-request-path request)
         (lws-transport-encode-headers (normalized-http-request-headers request))
         #vu8() (and (normalized-http-request-body-factory request) #t) alpn))))

  #|proc:lws-transport-submit-body!
  The `lws-transport-submit-body!` procedure queues `payload` on `reactor` for the logical stream
  selected by `connection-id`, `stream-id`, and `generation`. `final?` marks the last chunk.
  It returns true on acceptance and raises a network error on command rejection.
  |#
  (define-who lws-transport-submit-body!
    (lambda (reactor connection-id stream-id generation payload final?)
      (pcheck ([lws-reactor? reactor] [natural? connection-id stream-id generation]
               [bytevector? payload] [boolean? final?])
        (unless (lws-reactor-submit-body!
                 reactor connection-id stream-id generation payload final?)
          (raise-net-error who 'http "request body command rejected" stream-id))
        #t)))

  #|proc:lws-transport-consume-body!
  The `lws-transport-consume-body!` procedure acknowledges `byte-count` bytes on `reactor` for
  the logical stream selected by `connection-id`, `stream-id`, and `generation`, after consumption.
  It returns true on acceptance and raises a network error on command rejection.
  |#
  (define-who lws-transport-consume-body!
    (lambda (reactor connection-id stream-id generation byte-count)
      (pcheck ([lws-reactor? reactor] [natural? connection-id stream-id generation byte-count])
        (unless (lws-reactor-consume-body!
                 reactor connection-id stream-id generation byte-count)
          (raise-net-error who 'http "response consumption command rejected" stream-id))
        #t))))
