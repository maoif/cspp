(library (cspp net grpc)
  (export grpc-open-channel
          grpc-channel-credentials? make-grpc-channel-credentials
          grpc-server-credentials? make-grpc-server-credentials
          grpc-call-options? make-grpc-call-options
          grpc-call-options-metadata grpc-call-options-timeout-ms
          grpc-call-options-compression
          grpc-capabilities grpc-capabilities?
          grpc-capabilities-tls? grpc-capabilities-compression?
          grpc-capabilities-compression-algorithms
          grpc-capabilities-deadlines? grpc-capabilities-cancellation?
          grpc-capabilities-status-details? grpc-capabilities-reflection?
          grpc-close-channel
          grpc-cancel-pending!
          grpc-channel?
          grpc-stream?
          grpc-stream-send
          grpc-stream-recv
          grpc-stream-close-send
          grpc-stream-close
          grpc-register-service!
          grpc-serve
          grpc-call
          grpc-call/nonblocking
          grpc-call/server-stream
          grpc-call/client-stream
          grpc-call/bidi-stream
          grpc-call/server-stream/nonblocking
          grpc-call/client-stream/nonblocking
          grpc-call/bidi-stream/nonblocking
          grpc-request
          grpc-request?
          grpc-request-method
          grpc-request-payload
          grpc-request-metadata
          grpc-response
          grpc-response?
          grpc-response-payload
          grpc-response-metadata
          grpc-response-status
          grpc-status? make-grpc-status
          grpc-status-code
          grpc-status-message
          grpc-status-details
          grpc-metadata-ref)
  (import (cspp chez)
          (only (cspp utils) define-who pcheck)
          (cspp net errors)
          (cspp net address)
          (cspp net socket)
          (cspp net poll)
          (cspp net operation)
          (cspp net private ffi)
          (cspp net private common))

  (define grpc-status-ok 0)
  (define grpc-status-internal 13)
  (define grpc-status-unimplemented 12)
  (define grpc-default-timeout-ms 30000)

  #|record:grpc-request-record
The `grpc-request-record` record is an immutable server-side gRPC request view.
Its method is the RPC path, payload is copied message bytes, and metadata is the received alist.
The native handle remains owned by the serving channel and is not exposed to callers.
|#
  (define-record-type (grpc-request-record %make-grpc-request-record grpc-request-record?)
    (sealed #t)
    (opaque #f)
    (fields (immutable handle grpc-request-handle)
            (immutable method grpc-request-method)
            (immutable payload grpc-request-payload)
            (immutable metadata grpc-request-metadata)))

  (define-record-type (grpc-status-record %make-grpc-status-record grpc-status-record?)
    (sealed #t)
    (opaque #f)
    (fields (immutable code grpc-status-record-code)
            (immutable message grpc-status-record-message)
            (immutable details grpc-status-record-details)))

  #|record:grpc-response-record
The `grpc-response-record` record is an immutable completed gRPC response.
Its payload is copied message bytes, metadata is the response metadata alist, and status is an
immutable gRPC status. The record remains valid after the call or stream closes.
|#
  (define-record-type (grpc-response-record %make-grpc-response-record grpc-response?)
    (sealed #t)
    (opaque #f)
    (fields (immutable payload grpc-response-payload)
            (immutable metadata grpc-response-metadata)
            (immutable status grpc-response-status)))

  (define grpc-request? grpc-request-record?)

  #|record:grpc-stream
The `grpc-stream` record owns one native streaming RPC with a fixed side and call shape.
Send and receive closure are tracked independently. `grpc-stream-close` releases the handle and
marks both directions closed; subsequent stream operations raise an error.
|#
  (define-record-type (grpc-stream %make-grpc-stream grpc-stream?)
    (sealed #t)
    (opaque #f)
    (fields (immutable handle grpc-stream-handle)
            (immutable side grpc-stream-side)
            (immutable shape grpc-stream-shape)
            (mutable send-closed? grpc-stream-send-closed? grpc-stream-send-closed?-set!)
            (mutable recv-closed? grpc-stream-recv-closed? grpc-stream-recv-closed?-set!)
            (mutable closed? grpc-stream-closed? grpc-stream-closed?-set!)))

  #|record:grpc-channel
The `grpc-channel` record owns a client channel or server listener for an immutable endpoint.
Server channels retain registered handlers. Closing releases the native handle, cancels pending
work, and causes later channel operations to raise an error.
|#
  (define-record-type (grpc-channel %make-grpc-channel grpc-channel?)
    (sealed #t)
    (opaque #f)
    (fields (immutable role grpc-channel-role)
            (immutable endpoint grpc-channel-endpoint)
            (mutable handle grpc-channel-handle grpc-channel-handle-set!)
            (immutable handlers grpc-channel-handlers)
            (mutable pending grpc-channel-pending grpc-channel-pending-set!)
            (mutable closed? grpc-channel-closed? grpc-channel-closed?-set!)))

  #|record:grpc-channel-credentials
The `grpc-channel-credentials` record is immutable client TLS configuration.
Root certificates are PEM trust bytes. The optional certificate chain and private key are PEM
bytes used together for mutual TLS. Credential bytevectors are retained by the record.
|#
  (define-record-type (grpc-channel-credentials %make-grpc-channel-credentials
                                                 grpc-channel-credentials?)
    (sealed #t)
    (opaque #f)
    (fields (immutable root-certs grpc-channel-credentials-root-certs)
            (immutable certificate-chain grpc-channel-credentials-certificate-chain)
            (immutable private-key grpc-channel-credentials-private-key)))

  #|record:grpc-server-credentials
The `grpc-server-credentials` record is immutable server TLS configuration.
The PEM certificate chain and private key identify the server. Optional PEM roots enable and
authenticate mutual TLS clients. Credential bytevectors are retained by the record.
|#
  (define-record-type (grpc-server-credentials %make-grpc-server-credentials
                                                grpc-server-credentials?)
    (sealed #t)
    (opaque #f)
    (fields (immutable root-certs grpc-server-credentials-root-certs)
            (immutable certificate-chain grpc-server-credentials-certificate-chain)
            (immutable private-key grpc-server-credentials-private-key)))

  #|record:grpc-call-options
The `grpc-call-options` record is immutable policy shared by every gRPC call shape.
Metadata is an alist of request headers, timeout-ms is a positive deadline interval, and
compression is one of `identity`, `deflate`, or `gzip`.
|#
  (define-record-type (grpc-call-options %make-grpc-call-options grpc-call-options?)
    (sealed #t)
    (opaque #f)
    (fields (immutable metadata grpc-call-options-metadata)
            (immutable timeout-ms grpc-call-options-timeout-ms)
            (immutable compression grpc-call-options-compression)))

  #|record:grpc-capabilities-record
The `grpc-capabilities-record` record is an immutable runtime feature snapshot.
Its boolean fields report TLS, compression, deadline, cancellation, status-detail, and reflection
support. Compression-algorithms lists the accepted compression symbols.
|#
  (define-record-type (grpc-capabilities-record %make-grpc-capabilities
                                                grpc-capabilities?)
    (sealed #t)
    (opaque #f)
    (fields (immutable tls? grpc-capabilities-tls?)
            (immutable compression? grpc-capabilities-compression?)
            (immutable compression-algorithms grpc-capabilities-compression-algorithms)
            (immutable deadlines? grpc-capabilities-deadlines?)
            (immutable cancellation? grpc-capabilities-cancellation?)
            (immutable status-details? grpc-capabilities-status-details?)
            (immutable reflection? grpc-capabilities-reflection?)))

  (define grpc-status? grpc-status-record?)

  #|proc:make-grpc-status
The `make-grpc-status` procedure constructs a gRPC status. `code` is the integer status code,
`message` is its text, and `details` is the copied binary detail payload. It returns a status.
|#
  (define make-grpc-status
    (lambda (code message details)
      (pcheck ([fixnum? code] [string? message] [bytevector? details])
        (%make-grpc-status-record code message (bytevector-copy details)))))

  #|proc:grpc-status-code
The `grpc-status-code` procedure accepts a gRPC status or response and returns its integer code.
|#
  (define grpc-status-code
    (lambda (status-or-response)
      (pcheck ([(lambda (value) (or (grpc-status? value) (grpc-response? value)))
                status-or-response])
        (grpc-status-record-code
         (if (grpc-response? status-or-response)
             (grpc-response-status status-or-response)
             status-or-response)))))

  #|proc:grpc-status-message
The `grpc-status-message` procedure accepts a gRPC status or response and returns its message.
|#
  (define grpc-status-message
    (lambda (status-or-response)
      (pcheck ([(lambda (value) (or (grpc-status? value) (grpc-response? value)))
                status-or-response])
        (grpc-status-record-message
         (if (grpc-response? status-or-response)
             (grpc-response-status status-or-response)
             status-or-response)))))

  #|proc:grpc-status-details
The `grpc-status-details` procedure accepts a gRPC status or response and returns detail bytes.
|#
  (define grpc-status-details
    (lambda (status-or-response)
      (pcheck ([(lambda (value) (or (grpc-status? value) (grpc-response? value)))
                status-or-response])
        (grpc-status-record-details
         (if (grpc-response? status-or-response)
             (grpc-response-status status-or-response)
             status-or-response)))))

  #|proc:make-grpc-call-options
The `make-grpc-call-options` procedure constructs call options. `metadata` is an ordered alist,
`timeout-ms` is the non-negative call timeout, and `compression` is `identity`, `deflate`, or
`gzip`. The return value is an immutable call-options record.
|#
  (define make-grpc-call-options
    (case-lambda
      [() (make-grpc-call-options '() grpc-default-timeout-ms 'identity)]
      [(metadata) (make-grpc-call-options metadata grpc-default-timeout-ms 'identity)]
      [(metadata timeout-ms) (make-grpc-call-options metadata timeout-ms 'identity)]
      [(metadata timeout-ms compression)
       (pcheck ([fixnum? timeout-ms] [symbol? compression])
         (check-timeout-ms 'make-grpc-call-options timeout-ms)
         (unless (memq compression '(identity deflate gzip))
           (errorf 'make-grpc-call-options "unsupported compression algorithm ~s" compression))
         (%make-grpc-call-options metadata timeout-ms compression))]))

  #|proc:grpc-capabilities
The `grpc-capabilities` procedure reports the features supported by the loaded gRPC runtime.
The return value is an immutable gRPC capabilities record.
|#
  (define grpc-capabilities
    (lambda ()
      (let* ([bits (ffi-net-grpc-capabilities)]
             [compression? (not (zero? (bitwise-and bits 2)))])
        (%make-grpc-capabilities
         (not (zero? (bitwise-and bits 1)))
         compression?
         (if compression? '(identity deflate gzip) '(identity))
         #t #t #t #t))))

  #|proc:make-grpc-channel-credentials
The `make-grpc-channel-credentials` procedure copies optional PEM root, certificate,
and private-key strings for a TLS client channel. The return value is credentials.
|#
  (define make-grpc-channel-credentials
    (lambda (root-certs certificate-chain private-key)
      (pcheck ([(lambda (x) (or (not x) (string? x))) root-certs certificate-chain private-key])
        (%make-grpc-channel-credentials root-certs certificate-chain private-key))))

  #|proc:make-grpc-server-credentials
The `make-grpc-server-credentials` procedure copies optional PEM roots and required
certificate/private-key strings for a TLS server. The return value is credentials.
|#
  (define make-grpc-server-credentials
    (lambda (root-certs certificate-chain private-key)
      (pcheck ([(lambda (x) (or (not x) (string? x))) root-certs certificate-chain private-key])
        (unless (and certificate-chain private-key)
          (errorf 'make-grpc-server-credentials "certificate and private key are required"))
        (%make-grpc-server-credentials root-certs certificate-chain private-key))))

  (define credential-string
    (lambda (value) (if value value "")))

  (define ensure-success
    (lambda (who x)
      (when (ffi-error? x)
        (raise-net-error who 'grpc (ffi-error-message x) x))
      x))

  (define ensure-channel-open
    (lambda (who channel)
      (when (grpc-channel-closed? channel)
        (raise-net-error who 'grpc "gRPC channel is closed" channel))))

  (define ensure-stream-open
    (lambda (who stream)
      (when (grpc-stream-closed? stream)
        (raise-net-error who 'grpc "gRPC stream is closed" stream))))

  (define ensure-role
    (lambda (who channel role)
      (unless (eq? (grpc-channel-role channel) role)
        (raise-net-error who 'grpc
                         (format "gRPC channel role mismatch, expected ~a" role)
                         channel))))

  (define make-handler-table
    (lambda ()
      (make-hashtable string-hash string=?)))

  (define check-slice
    (lambda (who len start stop)
      (unless (and (fixnum? start) (fixnum? stop) (fx<= 0 start stop len))
        (errorf who "invalid slice [~a, ~a) for length ~a" start stop len))))

  (define check-timeout-ms
    (lambda (who timeout-ms)
      (unless (fixnum? timeout-ms)
        (errorf who "expected timeout fixnum, given ~s" timeout-ms))
      (when (fx< timeout-ms 0)
        (errorf who "timeout must be non-negative, given ~s" timeout-ms))
      timeout-ms))

  (define current-time-ms
    (lambda ()
      (let ([time (current-time 'time-monotonic)])
        (+ (* (time-second time) 1000)
           (quotient (time-nanosecond time) 1000000)))))

  (define endpoint-string
    (case-lambda
      [(who endpoint)
       (unless (string? endpoint)
         (errorf who "expected endpoint string, given ~s" endpoint))
       endpoint]
      [(who host port)
       (unless (string? host)
         (errorf who "expected host string, given ~s" host))
       (unless (fixnum? port)
         (errorf who "expected port fixnum, given ~s" port))
       (check-port who port)
       (format "~a:~a" host port)]))

  (define normalize-payload
    (lambda (who payload)
      (cond
       [(bytevector? payload) payload]
       [(string? payload) (string->utf8 payload)]
       [(eq? payload #f) #f]
       [else
        (errorf who "expected bytevector, string, or #f payload, given ~s" payload)])))

  (define string-suffix?
    (lambda (suffix s)
      (let ([sn (string-length suffix)]
            [n (string-length s)])
        (and (fx<= sn n)
             (string=? suffix (substring s (fx- n sn) n))))))

  (define grpc-metadata-key-character?
    (lambda (character)
      (or (char<=? #\a character #\z)
          (char<=? #\0 character #\9)
          (memv character '(#\- #\_ #\.)))))

  (define normalize-metadata-key
    (lambda (who raw-key)
      (let ([key (cond
                  [(string? raw-key) raw-key]
                  [(symbol? raw-key) (symbol->string raw-key)]
                  [else
                   (errorf who "expected string or symbol metadata key, given ~s" raw-key)])])
        (unless (and (positive? (string-length key))
                     (andmap grpc-metadata-key-character? (string->list key)))
          (errorf who "invalid gRPC metadata key ~s" key))
        (when (and (>= (string-length key) 5)
                   (string=? "grpc-" (substring key 0 5)))
          (errorf who "caller metadata key uses reserved grpc- prefix: ~s" key))
        key)))

  (define normalize-metadata
    (lambda (who metadata)
      (cond
       [(eq? metadata #f) '()]
       [(null? metadata) '()]
       [(list? metadata)
        (map (lambda (entry)
               (unless (pair? entry)
                 (errorf who "expected metadata pair, given ~s" entry))
               (let* ([raw-key (car entry)]
                      [raw-value (cdr entry)]
                      [key (normalize-metadata-key who raw-key)]
                      [value (cond
                              [(bytevector? raw-value)
                               (unless (string-suffix? "-bin" key)
                                 (errorf who
                                         "bytevector metadata requires a -bin key: ~s"
                                         key))
                               raw-value]
                              [(string? raw-value)
                               (when (string-suffix? "-bin" key)
                                 (errorf who "binary metadata value must be a bytevector: ~s" key))
                               (string->utf8 raw-value)]
                              [else (errorf who "expected string or bytevector metadata value, given ~s"
                                            raw-value)])])
                 (cons key value)))
             metadata)]
       [else
        (errorf who "expected metadata alist or #f, given ~s" metadata)])))

  (define normalize-call-metadata
    (lambda (who metadata compression)
      (let ([metadata* (normalize-metadata who metadata)])
        (case compression
          [(identity) metadata*]
          [(deflate gzip)
           (unless (grpc-capabilities-compression? (grpc-capabilities))
             (raise-net-error who 'unsupported
                              "the loaded gRPC runtime does not support compression"
                              compression))
           (cons (cons "grpc-internal-encoding-request"
                       (string->utf8 (symbol->string compression)))
                 metadata*)]
          [else (errorf who "unsupported compression algorithm ~s" compression)]))))

  (define response-with-compression
    (lambda (response compression)
      (if (or (not response) (eq? compression 'identity))
          response
          (%make-grpc-response-record
           (grpc-response-payload response)
           (cons (cons "grpc-encoding" (string->utf8 (symbol->string compression)))
                 (grpc-response-metadata response))
           (grpc-response-status response)))))

  (define metadata-value->scheme
    (lambda (key value)
      (cond
       [(not value) #f]
       [(string-suffix? "-bin" key) value]
       [else (utf8->string value)])))

  (define metadata-ref*
    (lambda (metadata key default)
      (let ([target (if (symbol? key) (symbol->string key) key)])
        (unless (string? target)
          (errorf 'grpc-metadata-ref "expected metadata key string or symbol, given ~s" key))
        (let loop ([rest metadata])
          (if (null? rest)
              default
              (let* ([entry (car rest)]
                     [entry-key (car entry)]
                     [entry-value (cdr entry)])
                (if (string-ci=? entry-key target)
                    (metadata-value->scheme entry-key entry-value)
                    (loop (cdr rest)))))))))

  (define maybe-response-from-ffi
    (lambda (who x)
      (cond
       [(eq? x #f) #f]
       [(and (vector? x) (= (vector-length x) 5))
        (%make-grpc-response-record
         (vector-ref x 0)
         (vector-ref x 1)
         (%make-grpc-status-record
          (vector-ref x 2)
          (vector-ref x 3)
          (vector-ref x 4)))]
       [else
        (errorf who "unexpected gRPC response payload ~s" x)])))

  (define request-from-ffi
    (lambda (who x)
      (unless (and (vector? x) (= (vector-length x) 4))
        (errorf who "unexpected gRPC request payload ~s" x))
      (%make-grpc-request-record
       (vector-ref x 0)
       (vector-ref x 1)
       (vector-ref x 2)
       (vector-ref x 3))))

  (define normalize-response
    (lambda (value)
      (cond
       [(grpc-response? value) value]
       [(or (bytevector? value) (string? value) (eq? value #f))
        (%make-grpc-response-record
         (normalize-payload 'grpc-response value) '()
         (%make-grpc-status-record grpc-status-ok "" #vu8()))]
       [else
        (errorf 'grpc-serve "handler must return a gRPC response, string, bytevector, or #f: ~s" value)])))

  (define method-name
    (lambda (who method)
      (cond
       [(string? method) method]
       [(symbol? method) (symbol->string method)]
       [else (errorf who "expected gRPC method string or symbol, given ~s" method)])))

  (define normalize-stream-shape
    (lambda (who shape)
      (case shape
        [(unary server client bidi) shape]
        [else
         (errorf who "invalid gRPC stream shape ~s" shape)])))

  (define stream-shape->int
    (lambda (who shape)
      (case shape
        [(server) 1]
        [(client) 2]
        [(bidi) 3]
        [else
         (errorf who "invalid gRPC stream shape ~s" shape)])))

  (define stream-send-allowed?
    (lambda (stream)
      (or (eq? (grpc-stream-side stream) 'server)
          (memq (grpc-stream-shape stream) '(client bidi)))))

  (define stream-recv-allowed?
    (lambda (stream)
      (or (eq? (grpc-stream-side stream) 'server)
          (memq (grpc-stream-shape stream) '(server client bidi)))))

  (define normalize-stream-response
    (lambda (value)
      (cond
       [(grpc-response? value) value]
       [(or (bytevector? value) (string? value) (eq? value #f))
        (grpc-response value '() grpc-status-ok "")]
       [else
        (errorf 'grpc-serve
                "streaming handler must return a gRPC response, string, bytevector, or #f: ~s"
                value)])))

  (define make-client-stream
    (lambda (handle shape)
      (%make-grpc-stream handle 'client shape #f #f #f)))

  (define make-server-stream
    (lambda (handle shape)
      (%make-grpc-stream handle 'server shape #f #f #f)))

  (define close-stream-handle!
    (lambda (who stream)
      (unless (grpc-stream-closed? stream)
        (ensure-success who (ffi-net-grpc-stream-close (grpc-stream-handle stream)))
        (grpc-stream-closed?-set! stream #t))))

  (define finish-server-stream!
    (lambda (who stream response)
      (ensure-success who
                      (ffi-net-grpc-stream-finish
                       (grpc-stream-handle stream)
                       (grpc-response-payload response)
                       0
                       (if (grpc-response-payload response)
                           (bytevector-length (grpc-response-payload response))
                           0)
                       (grpc-status-code response)
                       (grpc-status-message response)
                       (grpc-response-metadata response)))
      (grpc-stream-send-closed?-set! stream #t)
      stream))

  (define open-stream
    (lambda (who channel method shape payload metadata timeout-ms)
      (ensure-channel-open who channel)
      (ensure-role who channel 'client)
      (let* ([method* (method-name who method)]
             [payload* (normalize-payload who payload)]
             [metadata* (normalize-metadata who metadata)]
             [handle (ensure-success who
                                     (ffi-net-grpc-stream-open
                                      (grpc-channel-handle channel)
                                      method*
                                      (stream-shape->int who shape)
                                      payload*
                                      0
                                      (if payload*
                                          (bytevector-length payload*)
                                          0)
                                      metadata*
                                      timeout-ms))])
        (make-client-stream handle shape))))

  (define stream-recv-result
    (lambda (who x)
      (cond
       [(or (bytevector? x) (eof-object? x)) x]
       [else (ensure-success who x)])))

  (define grpc-would-block?
    (lambda (value)
      (and (vector? value)
           (fx>= (vector-length value) 1)
           (eq? (vector-ref value 0) 'would-block))))

  (define await-stream-attempt
    (lambda (who kind thunk)
      (net-operation-wait
       (make-net-operation
        kind
        (lambda ()
          (guard (failure [else (net-operation-failed failure)])
            (let ([answer (thunk)])
              (if (grpc-would-block? answer)
                  (let ([fd (ffi-net-grpc-driver-fd)])
                    (net-operation-pending
                     (if (fx>= fd 0)
                         (list (make-poll-target fd '(read)))
                         '())
                     #f))
                  (net-operation-completed answer)))))
        void))))

  (define accept-stream-request
    (lambda (who channel)
      (let ([ans (ensure-success who
                                 (ffi-net-grpc-server-request-stream
                                  (grpc-channel-handle channel)))])
        (unless (and (vector? ans) (= (vector-length ans) 3))
          (errorf who "unexpected gRPC stream request payload ~s" ans))
        (values (vector-ref ans 0)
                (vector-ref ans 1)
                (vector-ref ans 2)))))

  (define cancel-pending!
    (lambda (who channel)
      (for-each net-operation-cancel! (grpc-channel-pending channel))
      (grpc-channel-pending-set! channel '())
      channel))

  (define remove-pending-operation!
    (lambda (channel operation)
      (grpc-channel-pending-set! channel
                                 (remq operation (grpc-channel-pending channel)))))

  #|proc:grpc-request
The `grpc-request` procedure constructs a gRPC request record.
|#
  (define-who grpc-request
    (case-lambda
      [(method payload)
       (grpc-request method payload '())]
      [(method payload metadata)
       (%make-grpc-request-record
        #f
        (method-name who method)
        (normalize-payload who payload)
        (normalize-metadata who metadata))]))

  #|proc:grpc-response
The `grpc-response` procedure constructs a gRPC response record. `payload` is a bytevector,
string, or `#f`, and `metadata` is an ordered metadata alist. `status-code` and
`status-message` describe completion. Optional `status-details` contains binary detail bytes.
The return value is an immutable gRPC response.
|#
  (define-who grpc-response
    (case-lambda
      [(payload)
       (grpc-response payload '() grpc-status-ok "")]
      [(payload metadata)
       (grpc-response payload metadata grpc-status-ok "")]
      [(payload metadata status-code status-message)
       (grpc-response payload metadata status-code status-message #vu8())]
      [(payload metadata status-code status-message status-details)
       (pcheck ([fixnum? status-code] [string? status-message])
               (%make-grpc-response-record
                (normalize-payload who payload)
                (normalize-metadata who metadata)
                (make-grpc-status status-code status-message status-details)))]))

  #|proc:grpc-metadata-ref
The `grpc-metadata-ref` procedure looks up `key` in metadata alist or request/response `x`.
The optional `default` is returned when the key is absent. The return value is the first value.
|#
  (define-who grpc-metadata-ref
    (case-lambda
      [(x key)
       (grpc-metadata-ref x key #f)]
      [(x key default)
       (let ([metadata (cond
                        [(grpc-request? x) (grpc-request-metadata x)]
                        [(grpc-response? x) (grpc-response-metadata x)]
                        [else x])])
         (unless (list? metadata)
           (errorf who "expected gRPC metadata list or request/response, given ~s" x))
         (metadata-ref* metadata key default))]))

  #|proc:grpc-open-channel
The `grpc-open-channel` procedure opens a client channel or server listener at an endpoint.
The parameters select an endpoint string, host and port, role, and optional TLS credentials.
The return value is an open gRPC channel.
|#
  (define-who grpc-open-channel
    (case-lambda
      [(endpoint)
       (let ([ep (endpoint-string who endpoint)])
         (%make-grpc-channel
          'client
          ep
          (ensure-success who (ffi-net-grpc-channel-open ep))
          (make-handler-table)
          '()
          #f))]
      [(host port)
       (let ([ep (endpoint-string who host port)])
         (%make-grpc-channel
          'client
          ep
          (ensure-success who (ffi-net-grpc-channel-open ep))
          (make-handler-table)
          '()
          #f))]
      [(first second third)
       (if (grpc-channel-credentials? first)
           (pcheck ([string? second] [fixnum? third])
             (check-port who third)
             (let ([ep (endpoint-string who second third)])
               (%make-grpc-channel
                'client ep
                (ensure-success
                 who
                 (ffi-net-grpc-channel-open-tls
                  ep
                  (credential-string (grpc-channel-credentials-root-certs first))
                  (credential-string
                   (grpc-channel-credentials-certificate-chain first))
                  (credential-string (grpc-channel-credentials-private-key first))))
                (make-handler-table) '() #f)))
           (pcheck ([symbol? first] [string? second] [fixnum? third])
             (check-port who third)
             (case first
               [(server)
                (let ([ans (ensure-success who (ffi-net-grpc-server-open second third))])
                  (unless (and (vector? ans) (= (vector-length ans) 2))
                    (errorf who "unexpected gRPC server open result ~s" ans))
                  (%make-grpc-channel
                   'server
                   (format "~a:~a" second (vector-ref ans 1))
                   (vector-ref ans 0)
                   (make-handler-table)
                   '()
                   #f))]
               [else
                (errorf who "invalid gRPC role ~s" first)])))]
      [(role credentials host port)
       (pcheck ([symbol? role] [grpc-server-credentials? credentials]
                [string? host] [fixnum? port])
         (unless (eq? role 'server)
           (errorf who "TLS credentials are only valid for server channels"))
         (check-port who port)
         (let ([ans (ensure-success
                     who
                     (ffi-net-grpc-server-open-tls
                      host port
                      (credential-string (grpc-server-credentials-root-certs credentials))
                      (credential-string
                       (grpc-server-credentials-certificate-chain credentials))
                      (credential-string (grpc-server-credentials-private-key credentials))))])
           (unless (and (vector? ans) (= (vector-length ans) 2))
             (errorf who "unexpected gRPC TLS server open result ~s" ans))
           (%make-grpc-channel
            'server (format "~a:~a" host (vector-ref ans 1))
            (vector-ref ans 0) (make-handler-table) '() #f))) ]))

  #|proc:grpc-close-channel
The `grpc-close-channel` procedure closes a gRPC client channel or server listener.
|#
  (define-who grpc-close-channel
    (lambda (channel)
      (pcheck ([grpc-channel? channel])
              (unless (grpc-channel-closed? channel)
                (cancel-pending! who channel)
                (let ([handle (grpc-channel-handle channel)])
                  (when handle
                    (ensure-success who
                                    (if (eq? (grpc-channel-role channel) 'server)
                                        (ffi-net-grpc-server-close handle)
                                        (ffi-net-grpc-channel-close handle)))
                    (grpc-channel-handle-set! channel 0)))
                (grpc-channel-closed?-set! channel #t))
              channel)))

  #|proc:grpc-cancel-pending!
The `grpc-cancel-pending!` procedure cancels all pending operations owned by `channel`.
The `channel` parameter is a gRPC client channel.
The return value is `channel`.
|#
  (define-who grpc-cancel-pending!
    (lambda (channel)
      (pcheck ([grpc-channel? channel])
              (cancel-pending! who channel)
              channel)))

  #|proc:grpc-register-service!
The `grpc-register-service!` procedure registers a gRPC handler on a server channel.
|#
  (define-who grpc-register-service!
    (case-lambda
      [(channel method proc)
       (grpc-register-service! channel method 'unary proc)]
      [(channel method shape proc)
       (pcheck ([grpc-channel? channel] [procedure? proc])
               (ensure-channel-open who channel)
               (ensure-role who channel 'server)
               (hashtable-set! (grpc-channel-handlers channel)
                               (method-name who method)
                               (vector (normalize-stream-shape who shape) proc))
               channel)]))

  (define call-unary
    (lambda (who channel method payload metadata timeout-ms compression)
      (ensure-channel-open who channel)
      (ensure-role who channel 'client)
      (let* ([method* (method-name who method)]
             [payload* (normalize-payload who payload)]
             [metadata* (normalize-call-metadata who metadata compression)]
             [ans (ensure-success who
                                  (ffi-net-grpc-unary-call (grpc-channel-handle channel)
                                                           method*
                                                           payload*
                                                           0
                                                           (if payload*
                                                               (bytevector-length payload*)
                                                               0)
                                                           metadata*
                                                           timeout-ms))])
        (response-with-compression (maybe-response-from-ffi who ans) compression))))

  #|proc:grpc-call
The `grpc-call` procedure performs a blocking unary gRPC call and returns a gRPC response record.
|#
  (define-who grpc-call
    (case-lambda
      [(channel method payload)
       (grpc-call channel method payload '() grpc-default-timeout-ms)]
      [(channel method payload metadata)
       (if (grpc-call-options? metadata)
           (grpc-call channel method payload
                      (grpc-call-options-metadata metadata)
                      (grpc-call-options-timeout-ms metadata)
                      (grpc-call-options-compression metadata))
           (grpc-call channel method payload metadata grpc-default-timeout-ms))]
      [(channel method payload metadata timeout-ms)
       (grpc-call channel method payload metadata timeout-ms 'identity)]
      [(channel method payload metadata timeout-ms compression)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (call-unary who channel method payload metadata timeout-ms compression))]))

  (define start-pending-unary!
    (lambda (who channel method payload metadata timeout-ms compression)
      (let* ([method* (method-name who method)]
             [payload* (normalize-payload who payload)]
             [metadata* (normalize-call-metadata who metadata compression)]
             [args (list method* payload* metadata* timeout-ms)]
             [deadline-ms (+ (current-time-ms) timeout-ms)]
             [handle #f]
             [operation #f])
        (set! operation
              (make-net-operation
               'grpc-unary
               (lambda ()
                 (guard (failure [else (net-operation-failed failure)])
                   (unless handle
                     (let ([started (ffi-net-grpc-unary-start
                                     (grpc-channel-handle channel) method* payload* 0
                                     (if payload* (bytevector-length payload*) 0)
                                     metadata* timeout-ms)])
                       (if (ffi-error? started)
                           (raise-net-error who 'grpc (ffi-error-message started) started)
                           (set! handle started))))
                   (let ([answer (ffi-net-grpc-unary-poll handle)])
                     (if (not answer)
                         (net-operation-pending
                          (let ([fd (ffi-net-grpc-driver-fd)])
                            (if (fx>= fd 0)
                                (list (make-poll-target fd '(read)))
                                '()))
                          deadline-ms)
                         (begin
                           (set! handle #f)
                           (net-operation-completed
                            (response-with-compression
                             (maybe-response-from-ffi who answer) compression)))))))
               (lambda ()
                 (when handle
                   (ffi-net-grpc-unary-close handle)
                   (set! handle #f)))
               (lambda ()
                 (when handle
                   (ffi-net-grpc-unary-close handle)
                   (set! handle #f))
                 (remove-pending-operation! channel operation))))
        (grpc-channel-pending-set! channel
                                   (cons operation (grpc-channel-pending channel)))
        operation)))

  #|proc:grpc-call/nonblocking
The `grpc-call/nonblocking` procedure starts a unary call on `channel` for `method`.
The `payload` parameter is a bytevector, string, or `#f` request body.
The optional `metadata` parameter is an alist, and `timeout-ms` is the call timeout.
The return value is a network operation whose result is a gRPC response.
|#
  (define-who grpc-call/nonblocking
    (case-lambda
      [(channel method payload)
       (grpc-call/nonblocking channel method payload '() grpc-default-timeout-ms)]
      [(channel method payload metadata)
       (if (grpc-call-options? metadata)
           (grpc-call/nonblocking channel method payload
                                  (grpc-call-options-metadata metadata)
                                  (grpc-call-options-timeout-ms metadata)
                                  (grpc-call-options-compression metadata))
           (grpc-call/nonblocking channel method payload metadata grpc-default-timeout-ms))]
      [(channel method payload metadata timeout-ms)
       (grpc-call/nonblocking channel method payload metadata timeout-ms 'identity)]
      [(channel method payload metadata timeout-ms compression)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (ensure-role who channel 'client)
               (start-pending-unary!
                who channel method payload metadata timeout-ms compression))]))

  (define open-stream/nonblocking
    (lambda (who channel kind method shape payload metadata timeout-ms compression)
      (ensure-channel-open who channel)
      (ensure-role who channel 'client)
      (let* ([method* (method-name who method)]
             [payload* (normalize-payload who payload)]
             [metadata* (normalize-call-metadata who metadata compression)]
             [args (list kind method* payload* metadata* timeout-ms)]
             [deadline-ms (+ (current-time-ms) timeout-ms)])
        (let ([handle #f]
              [operation #f])
              (set! operation
                   (make-net-operation
                    kind
                    (lambda ()
                      (guard (failure [else (net-operation-failed failure)])
                        (unless handle
                          (let ([started
                                 (ffi-net-grpc-stream-open-start
                                  (grpc-channel-handle channel) method*
                                  (stream-shape->int who shape) payload* 0
                                  (if payload* (bytevector-length payload*) 0)
                                  metadata* timeout-ms)])
                            (if (ffi-error? started)
                                (raise-net-error who 'grpc (ffi-error-message started) started)
                                (set! handle started))))
                        (let ([answer (ffi-net-grpc-stream-open-poll handle)])
                          (cond
                           [(not answer)
                            (let ([fd (ffi-net-grpc-driver-fd)])
                              (net-operation-pending
                               (if (fx>= fd 0)
                                   (list (make-poll-target fd '(read)))
                                   '())
                               deadline-ms))]
                           [(ffi-error? answer)
                            (net-operation-failed
                             (make-net-error who 'grpc (ffi-error-message answer) answer))]
                           [else
                            (let ([stream (make-client-stream handle shape)])
                              (set! handle #f)
                              (net-operation-completed stream))]))))
                    (lambda ()
                      (when handle
                        (ffi-net-grpc-stream-close handle)
                        (set! handle #f)))
                    (lambda ()
                      (when handle
                        (ffi-net-grpc-stream-close handle)
                        (set! handle #f))
                      (remove-pending-operation! channel operation))))
          (grpc-channel-pending-set! channel
                                     (cons operation (grpc-channel-pending channel)))
          operation))))

  (define respond-to-request
    (lambda (who request response)
      (let ([payload (grpc-response-payload response)])
        (ensure-success
         who
         (ffi-net-grpc-server-respond (grpc-request-handle request)
                                      payload
                                      0
                                      (if payload (bytevector-length payload) 0)
                                      (grpc-status-code response)
                                      (grpc-status-message response)
                                      (grpc-response-metadata response))))))

  #|proc:grpc-serve
The `grpc-serve` procedure accepts and processes one gRPC request on a server channel.
|#
  (define-who grpc-serve
    (lambda (channel)
      (pcheck ([grpc-channel? channel])
              (ensure-channel-open who channel)
              (ensure-role who channel 'server)
              (let-values ([(handle method metadata)
                            (accept-stream-request who channel)])
                (let* ([entry (hashtable-ref (grpc-channel-handlers channel) method #f)]
                       [shape (and entry (vector-ref entry 0))]
                       [proc (and entry (vector-ref entry 1))]
                       [stream (make-server-stream handle (or shape 'bidi))]
                       [request (%make-grpc-request-record handle method #f metadata)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (cond
                       [(not entry)
                        (finish-server-stream!
                         who
                         stream
                         (grpc-response #f '() grpc-status-unimplemented "unimplemented"))
                        request]
                       [(eq? shape 'unary)
                        (let* ([payload (stream-recv-result who
                                                           (ffi-net-grpc-stream-recv
                                                            (grpc-stream-handle stream)))]
                               [request* (%make-grpc-request-record handle method payload metadata)]
                               [response
                                (guard (c [else
                                           (grpc-response #f
                                                          '()
                                                          grpc-status-internal
                                                          (if (condition? c)
                                                              (format "~a" c)
                                                              (format "~s" c)))])
                                  (normalize-response (proc request*)))])
                          (when (eof-object? payload)
                            (raise-net-error who 'grpc "unexpected EOF in unary gRPC request" request*))
                          (grpc-stream-recv-closed?-set! stream #t)
                          (finish-server-stream! who stream response)
                          request*)]
                       [else
                        (let ([result
                               (guard (c [else
                                          (finish-server-stream!
                                           who
                                           stream
                                           (grpc-response #f
                                                          '()
                                                          grpc-status-internal
                                                          (if (condition? c)
                                                              (format "~a" c)
                                                              (format "~s" c))))
                                          #f])
                                 (proc stream))])
                          (unless (or (not result) (grpc-stream-send-closed? stream))
                            (finish-server-stream!
                             who
                             stream
                             (normalize-stream-response result)))
                          (unless (grpc-stream-send-closed? stream)
                            (finish-server-stream! who stream (grpc-response #f '() grpc-status-ok "")))
                          request)]))
                    (lambda ()
                      (guard (c [else #f])
                        (grpc-stream-close stream)))))))))

  #|proc:grpc-stream-send
The `grpc-stream-send` procedure sends one message on a gRPC streaming call.
|#
  (define-who grpc-stream-send
    (lambda (stream payload)
      (pcheck ([grpc-stream? stream])
              (ensure-stream-open who stream)
              (unless (stream-send-allowed? stream)
                (raise-net-error who 'grpc "gRPC stream does not support sending" stream))
              (when (grpc-stream-send-closed? stream)
                (raise-net-error who 'grpc "gRPC stream send side is closed" stream))
              (let ([payload* (normalize-payload who payload)])
                (ensure-success
                 who
                 (await-stream-attempt
                  who
                  'grpc-stream-send
                  (lambda ()
                    (ffi-net-grpc-stream-send
                     (grpc-stream-handle stream)
                     payload*
                     0
                     (if payload* (bytevector-length payload*) 0)))))
                stream))))

  #|proc:grpc-stream-recv
The `grpc-stream-recv` procedure receives one message from `stream`.
It returns a payload bytevector or an EOF object when the peer finishes sending.
|#
  (define-who grpc-stream-recv
    (lambda (stream)
      (pcheck ([grpc-stream? stream])
              (ensure-stream-open who stream)
              (unless (stream-recv-allowed? stream)
                (raise-net-error who 'grpc "gRPC stream does not support receiving" stream))
              (let ([ans
                     (stream-recv-result
                      who
                      (await-stream-attempt
                       who
                       'grpc-stream-recv
                       (lambda ()
                         (ffi-net-grpc-stream-recv
                          (grpc-stream-handle stream)))))])
                (when (eof-object? ans)
                  (grpc-stream-recv-closed?-set! stream #t))
                ans))))

  #|proc:grpc-stream-close-send
The `grpc-stream-close-send` procedure closes the local send side of a gRPC streaming call.
|#
  (define-who grpc-stream-close-send
    (lambda (stream)
      (pcheck ([grpc-stream? stream])
              (ensure-stream-open who stream)
              (unless (stream-send-allowed? stream)
                (raise-net-error who 'grpc "gRPC stream does not support sending" stream))
              (unless (grpc-stream-send-closed? stream)
                (ensure-success
                 who
                 (await-stream-attempt
                  who
                  'grpc-stream-close-send
                  (lambda ()
                    (ffi-net-grpc-stream-close-send
                     (grpc-stream-handle stream)))))
                (grpc-stream-send-closed?-set! stream #t))
              stream)))

  #|proc:grpc-stream-close
The `grpc-stream-close` procedure closes a gRPC streaming call and releases its resources.
|#
  (define-who grpc-stream-close
    (lambda (stream)
      (pcheck ([grpc-stream? stream])
              (unless (grpc-stream-closed? stream)
                (guard (c [else #f])
                  (when (and (eq? (grpc-stream-side stream) 'server)
                             (not (grpc-stream-send-closed? stream)))
                    (grpc-stream-close-send stream)))
                (close-stream-handle! who stream)
                (grpc-stream-send-closed?-set! stream #t)
                (grpc-stream-recv-closed?-set! stream #t))
              stream)))

  #|proc:grpc-call/server-stream
The `grpc-call/server-stream` procedure opens a blocking server-streaming call on `channel`.
`method` identifies the RPC, `payload` is its request, and metadata, options, or timeout may follow.
The return value is an open gRPC stream.
|#
  (define-who grpc-call/server-stream
    (case-lambda
      [(channel method payload)
       (grpc-call/server-stream channel method payload '() grpc-default-timeout-ms)]
      [(channel method payload metadata-or-timeout)
       (cond
        [(fixnum? metadata-or-timeout)
         (grpc-call/server-stream channel method payload '() metadata-or-timeout)]
        [(grpc-call-options? metadata-or-timeout)
         (net-operation-wait
          (grpc-call/server-stream/nonblocking channel method payload metadata-or-timeout))]
        [else
         (grpc-call/server-stream
          channel method payload metadata-or-timeout grpc-default-timeout-ms)])]
      [(channel method payload metadata timeout-ms)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (net-operation-wait
                (grpc-call/server-stream/nonblocking
                 channel method payload metadata timeout-ms)))]))

  #|proc:grpc-call/client-stream
The `grpc-call/client-stream` procedure opens a blocking client-streaming call on `channel`.
`method` identifies the RPC, and metadata, options, or timeout may follow.
The return value is an open gRPC stream.
|#
  (define-who grpc-call/client-stream
    (case-lambda
      [(channel method)
       (grpc-call/client-stream channel method '() grpc-default-timeout-ms)]
      [(channel method metadata-or-timeout)
       (cond
        [(fixnum? metadata-or-timeout)
         (grpc-call/client-stream channel method '() metadata-or-timeout)]
        [(grpc-call-options? metadata-or-timeout)
         (net-operation-wait
          (grpc-call/client-stream/nonblocking channel method metadata-or-timeout))]
        [else
         (grpc-call/client-stream
          channel method metadata-or-timeout grpc-default-timeout-ms)])]
      [(channel method metadata timeout-ms)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (net-operation-wait
                (grpc-call/client-stream/nonblocking channel method metadata timeout-ms)))]))

  #|proc:grpc-call/bidi-stream
The `grpc-call/bidi-stream` procedure opens a blocking bidirectional call on `channel`.
`method` identifies the RPC, and metadata, options, or timeout may follow.
The return value is an open gRPC stream.
|#
  (define-who grpc-call/bidi-stream
    (case-lambda
      [(channel method)
       (grpc-call/bidi-stream channel method '() grpc-default-timeout-ms)]
      [(channel method metadata-or-timeout)
       (cond
        [(fixnum? metadata-or-timeout)
         (grpc-call/bidi-stream channel method '() metadata-or-timeout)]
        [(grpc-call-options? metadata-or-timeout)
         (net-operation-wait
          (grpc-call/bidi-stream/nonblocking channel method metadata-or-timeout))]
        [else
         (grpc-call/bidi-stream
          channel method metadata-or-timeout grpc-default-timeout-ms)])]
      [(channel method metadata timeout-ms)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (net-operation-wait
                (grpc-call/bidi-stream/nonblocking channel method metadata timeout-ms)))]))

  #|proc:grpc-call/server-stream/nonblocking
The `grpc-call/server-stream/nonblocking` procedure starts a server-streaming call on `channel`.
The `method`, `payload`, optional `metadata`, and `timeout-ms` parameters describe the call.
The return value is a network operation whose result is a gRPC stream.
|#
  (define-who grpc-call/server-stream/nonblocking
    (case-lambda
      [(channel method payload)
       (grpc-call/server-stream/nonblocking channel method payload '() grpc-default-timeout-ms)]
      [(channel method payload metadata-or-timeout)
       (cond
        [(fixnum? metadata-or-timeout)
         (grpc-call/server-stream/nonblocking channel method payload '() metadata-or-timeout)]
        [(grpc-call-options? metadata-or-timeout)
         (grpc-call/server-stream/nonblocking
          channel method payload
          (grpc-call-options-metadata metadata-or-timeout)
          (grpc-call-options-timeout-ms metadata-or-timeout)
          (grpc-call-options-compression metadata-or-timeout))]
        [else
         (grpc-call/server-stream/nonblocking
          channel method payload metadata-or-timeout grpc-default-timeout-ms)])]
      [(channel method payload metadata timeout-ms)
       (grpc-call/server-stream/nonblocking
        channel method payload metadata timeout-ms 'identity)]
      [(channel method payload metadata timeout-ms compression)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (open-stream/nonblocking
                who
                channel
                'server-stream
                method
                'server
                payload
                metadata
                timeout-ms
                compression))]))

  #|proc:grpc-call/client-stream/nonblocking
The `grpc-call/client-stream/nonblocking` procedure starts a client-streaming call on `channel`.
The `method`, optional `metadata`, and `timeout-ms` parameters describe the call.
The return value is a network operation whose result is a gRPC stream.
|#
  (define-who grpc-call/client-stream/nonblocking
    (case-lambda
      [(channel method)
       (grpc-call/client-stream/nonblocking channel method '() grpc-default-timeout-ms)]
      [(channel method metadata-or-timeout)
       (cond
        [(fixnum? metadata-or-timeout)
         (grpc-call/client-stream/nonblocking channel method '() metadata-or-timeout)]
        [(grpc-call-options? metadata-or-timeout)
         (grpc-call/client-stream/nonblocking
          channel method
          (grpc-call-options-metadata metadata-or-timeout)
          (grpc-call-options-timeout-ms metadata-or-timeout)
          (grpc-call-options-compression metadata-or-timeout))]
        [else
         (grpc-call/client-stream/nonblocking
          channel method metadata-or-timeout grpc-default-timeout-ms)])]
      [(channel method metadata timeout-ms)
       (grpc-call/client-stream/nonblocking channel method metadata timeout-ms 'identity)]
      [(channel method metadata timeout-ms compression)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (open-stream/nonblocking
                who
                channel
                'client-stream
                method
                'client
                #f
                metadata
                timeout-ms
                compression))]))

  #|proc:grpc-call/bidi-stream/nonblocking
The `grpc-call/bidi-stream/nonblocking` procedure starts a bidirectional call on `channel`.
The `method`, optional `metadata`, and `timeout-ms` parameters describe the call.
The return value is a network operation whose result is a gRPC stream.
|#
  (define-who grpc-call/bidi-stream/nonblocking
    (case-lambda
      [(channel method)
       (grpc-call/bidi-stream/nonblocking channel method '() grpc-default-timeout-ms)]
      [(channel method metadata-or-timeout)
       (cond
        [(fixnum? metadata-or-timeout)
         (grpc-call/bidi-stream/nonblocking channel method '() metadata-or-timeout)]
        [(grpc-call-options? metadata-or-timeout)
         (grpc-call/bidi-stream/nonblocking
          channel method
          (grpc-call-options-metadata metadata-or-timeout)
          (grpc-call-options-timeout-ms metadata-or-timeout)
          (grpc-call-options-compression metadata-or-timeout))]
        [else
         (grpc-call/bidi-stream/nonblocking
          channel method metadata-or-timeout grpc-default-timeout-ms)])]
      [(channel method metadata timeout-ms)
       (grpc-call/bidi-stream/nonblocking channel method metadata timeout-ms 'identity)]
      [(channel method metadata timeout-ms compression)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (open-stream/nonblocking
                who
                channel
                'bidi-stream
                method
                'bidi
                #f
                metadata
                timeout-ms
                compression))])))
