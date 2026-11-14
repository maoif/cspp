(library (cspp net tls)
  (export make-tls-context
          tls-context?
          tls-context-native-handle
          close-tls-context
          tls-context-load-ca-file!
          tls-context-load-ca-path!
          tls-context-load-default-ca!
          tls-context-load-cert!
          tls-context-load-private-key!
          tls-context-set-verify!
          tls-context-set-alpn!
          make-tls-policy
          tls-policy?
          tls-policy-minimum-version
          tls-policy-maximum-version
          tls-policy-cipher-list
          tls-policy-ciphersuites
          tls-policy-alpn-protocols
          tls-policy-ocsp-policy
          tls-context-policy-set!
          tls-context-sni-selector-set!
          tls-session-ticket?
          tls-session-ticket-data
          tls-context-session-ticket-set!
          tls-certificate?
          tls-certificate-der
          tls-certificate-subject
          tls-certificate-issuer
          tls-certificate-serial
          tls-certificate-not-before
          tls-certificate-not-after
          tls-certificate-digest
          tls-certificate-public-key-summary
          tls-certificate-chain-position
          tls-certificate-verified?
          tls-ocsp-result?
          tls-ocsp-result-status
          tls-ocsp-result-signature-valid?
          tls-ocsp-result-time-valid?
          tls-ocsp-result-response
          tls-connect
          tls-connect/nonblocking
          tls-accept
          tls-accept/nonblocking
          tls-session?
          close-tls-session
          tls-read
          tls-read!
          tls-write
          tls-write-all
          tls-read/nonblocking
          tls-read!/nonblocking
          tls-write/nonblocking
          tls-write-all/nonblocking
          tls-flush
          tls-shutdown!
          tls-peer-certificate
          tls-peer-certificate-chain
          tls-peer-certificate-chain-records
          tls-protocol-version
          tls-negotiated-alpn
          tls-cipher-name
          tls-verified?
          tls-session-export-ticket
          tls-session-reused?
          tls-stapled-ocsp-response
          tls-session-ocsp-result
          tls-capabilities
          call-with-tls-client
          call-with-tls-server
          open-tls-port
          open-tls-input-port
          open-tls-output-port
          open-tls-text-input-port
          open-tls-text-output-port
          call-with-tls-ports)
  (import (cspp chez)
          (only (cspp utils) define-who pcheck unreachable!)
          (cspp crypto cert)
          (cspp crypto pkey)
          (cspp crypto private common)
          (cspp net errors)
          (cspp net private ffi)
          (cspp net private common)
          (cspp net poll)
          (cspp net socket)
          (cspp net operation))

  (define tls-formats '(pem der))

  #|record:tls-context
The `tls-context` record owns mutable OpenSSL client or server configuration.
Its policy and SNI selector may change before handshakes. `close-tls-context` releases the native
handle; existing sessions retain their selected context, while new operations raise an error.
|#
  (define-record-type (tls-context %make-tls-context tls-context?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle tls-context-handle tls-context-handle-set!)
            (immutable mode tls-context-mode)
            (mutable policy tls-context-policy tls-context-policy-set-internal!)
            (mutable sni-selector tls-context-sni-selector
                     tls-context-sni-selector-set-internal!)
            (mutable closed? tls-context-closed? tls-context-closed?-set!)))

  #|record:tls-session
The `tls-session` record owns one established or handshaking TLS connection and its socket.
It retains the configured and SNI-selected contexts. `close-tls-session` releases its native
handle and marks it closed; later session I/O raises an error.
|#
  (define-record-type (tls-session %make-tls-session tls-session?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle tls-session-handle tls-session-handle-set!)
            (immutable context tls-session-context)
            (immutable selected-context tls-session-selected-context)
            (immutable socket tls-session-socket)
            (mutable closed? tls-session-closed? tls-session-closed?-set!)))

  #|record:tls-policy
The `tls-policy` record describes protocol, cipher, ALPN, and OCSP requirements for a context.
`make-tls-policy` accepts minimum and maximum TLS version symbols, optional OpenSSL cipher strings,
a list of ALPN protocol strings, and an OCSP policy of `disabled`, `optional`, or `required`.
The record is immutable; its accessors return the corresponding policy fields.
|#
  (define-record-type (tls-policy %make-tls-policy tls-policy?)
    (sealed #t)
    (opaque #f)
    (fields (immutable minimum-version tls-policy-minimum-version)
            (immutable maximum-version tls-policy-maximum-version)
            (immutable cipher-list tls-policy-cipher-list)
            (immutable ciphersuites tls-policy-ciphersuites)
            (immutable alpn-protocols tls-policy-alpn-protocols)
            (immutable ocsp-policy tls-policy-ocsp-policy)))

  #|record:tls-session-ticket
The `tls-session-ticket` record owns a copy of serialized OpenSSL session data.
`tls-session-ticket-data` returns a fresh bytevector copy suitable for persistent storage.
The record is immutable and contains no private key material.
|#
  (define-record-type (tls-session-ticket %make-tls-session-ticket tls-session-ticket?)
    (sealed #t)
    (opaque #f)
    (fields (immutable data %tls-session-ticket-data)))

  #|record:tls-certificate
The `tls-certificate` record is an immutable inspection snapshot of one peer-chain certificate.
Its fields contain DER bytes, names, serial bytes, validity strings, SHA-256 digest, public-key
algorithm and size, zero-based chain position, and whether the TLS chain was verified.
The record owns its bytevectors and contains no private key or native certificate handle.
|#
  (define-record-type (tls-certificate %make-tls-certificate tls-certificate?)
    (sealed #t)
    (opaque #f)
    (fields (immutable der %tls-certificate-der)
            (immutable subject tls-certificate-subject)
            (immutable issuer tls-certificate-issuer)
            (immutable serial %tls-certificate-serial)
            (immutable not-before tls-certificate-not-before)
            (immutable not-after tls-certificate-not-after)
            (immutable digest %tls-certificate-digest)
            (immutable public-key-summary tls-certificate-public-key-summary)
            (immutable chain-position tls-certificate-chain-position)
            (immutable verified? tls-certificate-verified?)))

  #|record:tls-ocsp-result
The `tls-ocsp-result` record describes validation of a stapled OCSP response.
Its immutable fields contain certificate status, signature and time-validity flags, and copied
response bytes. A record is returned only after issuer matching and cryptographic verification.
|#
  (define-record-type (tls-ocsp-result %make-tls-ocsp-result tls-ocsp-result?)
    (sealed #t)
    (opaque #f)
    (fields (immutable status tls-ocsp-result-status)
            (immutable signature-valid? tls-ocsp-result-signature-valid?)
            (immutable time-valid? tls-ocsp-result-time-valid?)
            (immutable response %tls-ocsp-result-response)))

  #|proc:tls-context-native-handle
The `tls-context-native-handle` procedure returns the opaque native handle owned by `ctx`.
The handle is intended for CSPP transport integrations and remains owned by `ctx`.
|#
  (define tls-context-native-handle
    (lambda (ctx)
      (pcheck ([tls-context? ctx])
        (when (tls-context-closed? ctx)
          (errorf 'tls-context-native-handle "TLS context is closed"))
        (tls-context-handle ctx))))

  (define check-format
    (lambda (who fmt)
      (unless (memq fmt tls-formats)
        (errorf who "TLS format must be one of ~s" tls-formats))))

  (define format->int
    (lambda (fmt)
      (case fmt
        [(pem) 0]
        [(der) 1]
        [else (unreachable!)])))

  (define mode->int
    (lambda (who mode)
      (case mode
        [(client) 0]
        [(server) 1]
        [else (errorf who "invalid TLS mode ~s" mode)])))

  (define ensure-context-open
    (lambda (who ctx)
      (when (tls-context-closed? ctx)
        (raise-net-error who 'tls "TLS context is closed" ctx))))

  (define ensure-session-open
    (lambda (who session)
      (when (tls-session-closed? session)
        (raise-net-error who 'tls "TLS session is closed" session))))

  (define ensure-success
    (lambda (who value)
      (when (ffi-error? value)
        (raise-net-error who 'tls (ffi-error-message value) value))
      value))

  (define tls-no-timeout -1)

  (define current-monotonic-ms
    (lambda ()
      (let ([time (current-time 'time-monotonic)])
        (+ (* (time-second time) 1000)
           (quotient (time-nanosecond time) 1000000)))))

  (define timeout->deadline-ms
    (lambda (timeout-ms)
      (and (fx>= timeout-ms 0)
           (+ (current-monotonic-ms) timeout-ms))))

  (define check-timeout-ms
    (lambda (who timeout-ms)
      (unless (fixnum? timeout-ms)
        (errorf who "timeout must be a fixnum, given ~s" timeout-ms))
      (when (fx< timeout-ms 0)
        (errorf who "timeout must be non-negative, given ~s" timeout-ms))
      timeout-ms))

  (define check-size
    (lambda (who size)
      (when (fx< size 0)
        (errorf who "size must be non-negative, given ~s" size))
      size))

  (define make-tls-handshake-operation
    (lambda (who kind ctx sock server-name timeout-ms)
      (ensure-context-open who ctx)
      (let* ([answer (if (eq? kind 'tls-connect)
                         (ffi-net-tls-connect (tls-context-handle ctx)
                                              (socket-fd sock)
                                              (or server-name "")
                                              timeout-ms)
                         (ffi-net-tls-accept (tls-context-handle ctx)
                                             (socket-fd sock)
                                             timeout-ms))]
             [handle (if (ffi-error? answer)
                         (raise-net-error who 'tls (ffi-error-message answer) answer)
                         answer)]
             [deadline-ms (timeout->deadline-ms timeout-ms)]
             [selected-context ctx])
        (make-net-operation
         kind
         (lambda ()
           (when (and deadline-ms (<= deadline-ms (current-monotonic-ms)))
             (raise-net-error who 'tls
                              (if (eq? kind 'tls-connect)
                                  "TLS client handshake timed out"
                                  "TLS server handshake timed out")))
           (let ([step (ffi-net-tls-handshake-step handle)])
             (cond
              [(eq? step #t)
               (let ([session (%make-tls-session handle ctx selected-context sock #f)])
                 (set! handle 0)
                 (net-operation-completed session))]
              [(and (vector? step)
                    (= (vector-length step) 2)
                    (eq? (vector-ref step 0) 'sni)
                    (string? (vector-ref step 1)))
               (let* ([selector (tls-context-sni-selector ctx)]
                      [choice (and selector (selector (vector-ref step 1)))])
                 (unless (or (not choice) (tls-context? choice))
                   (errorf who "SNI selector must return a TLS context or #f, given ~s" choice))
                 (set! selected-context (or choice ctx))
                 (ensure-context-open who selected-context)
                 (unless (eq? (tls-context-mode selected-context) 'server)
                   (errorf who "SNI selector returned a non-server TLS context"))
                 (ensure-success
                  who
                  (ffi-net-tls-session-select-context
                   handle (tls-context-handle selected-context)))
                 (net-operation-pending
                  (list (make-poll-target sock '(read write))) deadline-ms))]
              [(ffi-would-block? step)
               (net-operation-pending
                (list (make-poll-target sock (ffi-would-block-events step)))
                deadline-ms)]
              [else
               (ensure-success who step)
               (assert-unreachable)])))
         (lambda ()
           (when (not (zero? handle))
             (ensure-success who (ffi-net-tls-close handle))
             (set! handle 0)))
         (lambda ()
           (when (not (zero? handle))
             (ensure-success who (ffi-net-tls-close handle))
             (set! handle 0)))))))

  (define tls-connect*
    (lambda (who ctx sock server-name timeout-ms)
      (net-operation-wait
       (make-tls-handshake-operation who 'tls-connect ctx sock server-name timeout-ms))))

  (define tls-accept*
    (lambda (who ctx sock timeout-ms)
      (net-operation-wait
       (make-tls-handshake-operation who 'tls-accept ctx sock #f timeout-ms))))

  (define tls-read*
    (lambda (who session size timeout-ms nonblocking?)
      (ensure-session-open who session)
      (read-result who session
                   (ffi-net-tls-read (tls-session-handle session)
                                     size
                                     timeout-ms
                                     (if nonblocking? 1 0)))))

  (define tls-read-into*
    (lambda (who session bv start stop timeout-ms nonblocking?)
      (ensure-session-open who session)
      (check-slice who (bytevector-length bv) start stop)
      (read-into-result
       who session
       (ffi-net-tls-read-into (tls-session-handle session)
                              bv
                              start
                              stop
                              timeout-ms
                              (if nonblocking? 1 0)))))

  (define tls-write*
    (lambda (who session bv start stop timeout-ms nonblocking?)
      (ensure-session-open who session)
      (check-slice who (bytevector-length bv) start stop)
      (write-result who session
                    (ffi-net-tls-write (tls-session-handle session)
                                       bv
                                       start
                                       stop
                                       timeout-ms
                                       (if nonblocking? 1 0)))))

  (define encode-alpn
    (lambda (who proto*)
      (unless (list? proto*)
        (errorf who "ALPN protocols must be a list, given ~s" proto*))
      (let ([chunks
             (map (lambda (proto)
                    (unless (string? proto)
                      (errorf who "ALPN protocol must be a string, given ~s" proto))
                    (let ([bv (string->utf8 proto)])
                      (when (fx> (bytevector-length bv) 255)
                        (errorf who "ALPN protocol is too long: ~s" proto))
                      (let ([out (make-bytevector (fx1+ (bytevector-length bv)) 0)])
                        (bytevector-u8-set! out 0 (bytevector-length bv))
                        (bytevector-copy! bv 0 out 1 (bytevector-length bv))
                        out)))
                  proto*)])
        (apply bytevector-append chunks))))

  (define maybe-derive-peer-certificate
    (lambda (der)
      (and der (load-certificate der 'der))))

  (define maybe-derive-peer-certificate-chain
    (lambda (der*)
      (map (lambda (der) (load-certificate der 'der)) der*)))

  (define certificate-der->tls-certificate
    (lambda (der position verified?)
      (let ([certificate (load-certificate der 'der)]
            [public-key #f])
        (dynamic-wind
          void
          (lambda ()
            (set! public-key (certificate-public-key certificate))
            (%make-tls-certificate
             (bytevector-copy der)
             (certificate-subject certificate)
             (certificate-issuer certificate)
             (bytevector-copy (certificate-serial-number certificate))
             (certificate-not-before certificate)
             (certificate-not-after certificate)
             (bytevector-copy (certificate-fingerprint certificate 'sha256))
             (cons (public-key-algorithm public-key) (public-key-bits public-key))
             position
             verified?))
          (lambda ()
            (when public-key (destroy-public-key! public-key))
            (destroy-certificate! certificate))))))

  (define read-result
    (lambda (who session answer)
      (cond
       [(or (bytevector? answer) (eof-object? answer)) answer]
       [(ffi-would-block? answer)
        (make-net-would-block
         (tls-session-socket session)
         (list (ffi-would-block-event answer)))]
       [else (ensure-success who answer)])))

  (define read-into-result
    (lambda (who session answer)
      (cond
       [(or (fixnum? answer) (eof-object? answer)) answer]
       [(ffi-would-block? answer)
        (make-net-would-block
         (tls-session-socket session)
         (list (ffi-would-block-event answer)))]
       [else (ensure-success who answer)])))

  (define write-result
    (lambda (who session answer)
      (cond
       [(fixnum? answer) answer]
       [(ffi-would-block? answer)
        (make-net-would-block
         (tls-session-socket session)
         (list (ffi-would-block-event answer)))]
       [else (ensure-success who answer)])))

  (define make-binary-input-port
    (lambda (session)
      (make-custom-binary-input-port
       "cspp-tls-input"
       (lambda (bv start count)
         (let ([n (tls-read! session bv start (fx+ start count))])
           (cond
            [(fixnum? n) n]
            [(eof-object? n) 0]
            [else (errorf 'call-with-tls-ports
                          "unexpected nonblocking result from blocking TLS port read")])))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define make-binary-output-port
    (lambda (session)
      (make-custom-binary-output-port
       "cspp-tls-output"
       (lambda (bv start count)
         (tls-write-all session bv start (fx+ start count)))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define tls-versions '(tls1.2 tls1.3))

  (define tls-version->integer
    (lambda (who version allow-false?)
      (case version
        [(tls1.2) #x0303]
        [(tls1.3) #x0304]
        [else
         (if (and allow-false? (not version))
             0
             (errorf who "TLS version must be `tls1.2`, `tls1.3`, or #f, given ~s"
                     version))])))

  (define ocsp-policy->integer
    (lambda (policy)
      (case policy
        [(disabled) 0]
        [(optional) 1]
        [(required) 2]
        [else (assert-unreachable)])))

  (define validate-tls-policy
    (lambda (who minimum-version maximum-version cipher-list ciphersuites
                 alpn-protocols ocsp-policy)
      (tls-version->integer who minimum-version #f)
      (tls-version->integer who maximum-version #t)
      (unless (or (not cipher-list) (string? cipher-list))
        (errorf who "TLS 1.2 cipher list must be a string or #f, given ~s" cipher-list))
      (unless (or (not ciphersuites) (string? ciphersuites))
        (errorf who "TLS 1.3 ciphersuites must be a string or #f, given ~s" ciphersuites))
      (encode-alpn who alpn-protocols)
      (unless (memq ocsp-policy '(disabled optional required))
        (errorf who "OCSP policy must be `disabled`, `optional`, or `required`, given ~s"
                ocsp-policy))
      (when (and maximum-version
                 (> (tls-version->integer who minimum-version #f)
                    (tls-version->integer who maximum-version #t)))
        (errorf who "minimum TLS version exceeds maximum TLS version"))))

  #|proc:make-tls-policy
The `make-tls-policy` procedure constructs an immutable TLS policy.
`minimum-version` and `maximum-version` are `tls1.2`, `tls1.3`, or `#f` for no maximum.
`cipher-list` and `ciphersuites` are OpenSSL policy strings or `#f` for defaults.
`alpn-protocols` is a list of protocol strings. `ocsp-policy` controls stapling requirements.
The return value is a validated `tls-policy` record.
|#
  (define make-tls-policy
    (case-lambda
      [() (make-tls-policy 'tls1.2 #f #f #f '() 'disabled)]
      [(minimum-version maximum-version)
       (make-tls-policy minimum-version maximum-version #f #f '() 'disabled)]
      [(minimum-version maximum-version cipher-list ciphersuites alpn-protocols ocsp-policy)
       (validate-tls-policy 'make-tls-policy minimum-version maximum-version cipher-list
                            ciphersuites alpn-protocols ocsp-policy)
       (%make-tls-policy minimum-version maximum-version cipher-list ciphersuites
                         (map string-copy alpn-protocols) ocsp-policy)]))

  #|proc:tls-session-ticket-data
The `tls-session-ticket-data` procedure copies the serialized bytes owned by `ticket`.
The return value is a fresh bytevector that the caller may mutate.
|#
  (define tls-session-ticket-data
    (lambda (ticket)
      (pcheck ([tls-session-ticket? ticket])
        (bytevector-copy (%tls-session-ticket-data ticket)))))

  #|proc:tls-certificate-der
The `tls-certificate-der` procedure copies the encoded certificate stored in `certificate`.
The return value is a fresh DER bytevector that the caller may mutate.
|#
  (define tls-certificate-der
    (lambda (certificate)
      (pcheck ([tls-certificate? certificate])
        (bytevector-copy (%tls-certificate-der certificate)))))

  #|proc:tls-certificate-serial
The `tls-certificate-serial` procedure copies the serial number stored in `certificate`.
The return value is a fresh unsigned big-endian bytevector.
|#
  (define tls-certificate-serial
    (lambda (certificate)
      (pcheck ([tls-certificate? certificate])
        (bytevector-copy (%tls-certificate-serial certificate)))))

  #|proc:tls-certificate-digest
The `tls-certificate-digest` procedure copies the SHA-256 digest stored in `certificate`.
The return value is a fresh bytevector.
|#
  (define tls-certificate-digest
    (lambda (certificate)
      (pcheck ([tls-certificate? certificate])
        (bytevector-copy (%tls-certificate-digest certificate)))))

  #|proc:tls-ocsp-result-response
The `tls-ocsp-result-response` procedure copies the DER response stored in `result`.
The return value is a fresh bytevector that the caller may mutate.
|#
  (define tls-ocsp-result-response
    (lambda (result)
      (pcheck ([tls-ocsp-result? result])
        (bytevector-copy (%tls-ocsp-result-response result)))))

  #|proc:make-tls-context
The `make-tls-context` procedure constructs a client or server TLS context.
|#
  (define-who make-tls-context
    (lambda (mode)
      (let ([message (ffi-net-tls-load-error)])
        (when message
          (raise-net-error who 'tls message)))
      (let ([handle (ffi-net-tls-context-create (mode->int who mode))])
        (when (= handle 0)
          (raise-net-error who 'tls "failed to create TLS context"))
        (%make-tls-context handle mode (make-tls-policy) #f #f))))

  #|proc:close-tls-context
The `close-tls-context` procedure releases foreign resources owned by a TLS context.
|#
  (define-who close-tls-context
    (lambda (ctx)
      (pcheck ([tls-context? ctx])
              (unless (tls-context-closed? ctx)
                (ffi-net-tls-context-free (tls-context-handle ctx))
                (tls-context-handle-set! ctx 0)
                (tls-context-closed?-set! ctx #t))
              ctx)))

  #|proc:tls-context-load-ca-file!
The `tls-context-load-ca-file!` procedure loads trusted CA certificates from a PEM file.
|#
  (define-who tls-context-load-ca-file!
    (lambda (ctx path)
      (pcheck ([tls-context? ctx] [string? path])
              (ensure-context-open who ctx)
              (ensure-success who (ffi-net-tls-context-load-ca-file (tls-context-handle ctx) path)))))

  #|proc:tls-context-load-ca-path!
The `tls-context-load-ca-path!` procedure loads trusted CA certificates from a directory path.
|#
  (define-who tls-context-load-ca-path!
    (lambda (ctx path)
      (pcheck ([tls-context? ctx] [string? path])
              (ensure-context-open who ctx)
              (ensure-success who (ffi-net-tls-context-load-ca-path (tls-context-handle ctx) path)))))

  #|proc:tls-context-load-default-ca!
The `tls-context-load-default-ca!` procedure loads the platform default trusted certificate
locations into a TLS context.
|#
  (define-who tls-context-load-default-ca!
    (lambda (ctx)
      (pcheck ([tls-context? ctx])
              (ensure-context-open who ctx)
              (ensure-success who
                              (ffi-net-tls-context-load-default-ca
                               (tls-context-handle ctx))))))

  #|proc:tls-context-load-cert!
The `tls-context-load-cert!` procedure loads a TLS certificate from a pathname string or
bytevector data.
|#
  (define-who tls-context-load-cert!
    (case-lambda
      [(ctx source) (tls-context-load-cert! ctx source 'pem)]
      [(ctx source fmt)
       (pcheck ([tls-context? ctx])
               (ensure-context-open who ctx)
               (check-format who fmt)
               (cond
                [(string? source)
                 (ensure-success
                  who
                  (ffi-net-tls-context-load-cert-file (tls-context-handle ctx)
                                                      source
                                                      (format->int fmt)))]
                [(bytevector? source)
                 (ensure-success
                  who
                  (ffi-net-tls-context-load-cert-bytes (tls-context-handle ctx)
                                                       source
                                                       0
                                                       (bytevector-length source)
                                                       (format->int fmt)))]
                [else
                 (errorf who "expected pathname string or bytevector certificate source, given ~s"
                         source)]))]))

  #|proc:tls-context-load-private-key!
The `tls-context-load-private-key!` procedure loads a TLS private key from a pathname string or
bytevector data.
|#
  (define-who tls-context-load-private-key!
    (case-lambda
      [(ctx source) (tls-context-load-private-key! ctx source 'pem)]
      [(ctx source fmt)
       (pcheck ([tls-context? ctx])
               (ensure-context-open who ctx)
               (check-format who fmt)
               (cond
                [(string? source)
                 (ensure-success
                  who
                  (ffi-net-tls-context-load-key-file (tls-context-handle ctx)
                                                     source
                                                     (format->int fmt)))]
                [(bytevector? source)
                 (ensure-success
                  who
                  (ffi-net-tls-context-load-key-bytes (tls-context-handle ctx)
                                                      source
                                                      0
                                                      (bytevector-length source)
                                                      (format->int fmt)))]
                [else
                 (errorf who "expected pathname string or bytevector private-key source, given ~s"
                         source)])
               (ensure-success who (ffi-net-tls-context-check-key (tls-context-handle ctx))))]))

  #|proc:tls-context-set-verify!
The `tls-context-set-verify!` procedure enables or disables peer verification on a TLS context.
|#
  (define-who tls-context-set-verify!
    (lambda (ctx verify?)
      (pcheck ([tls-context? ctx] [boolean? verify?])
              (ensure-context-open who ctx)
              (ensure-success who (ffi-net-tls-context-set-verify (tls-context-handle ctx)
                                                                  (if verify? 1 0))))))

  #|proc:tls-context-set-alpn!
The `tls-context-set-alpn!` procedure configures ALPN protocol strings on a TLS context.
|#
  (define-who tls-context-set-alpn!
    (lambda (ctx proto*)
      (pcheck ([tls-context? ctx])
              (ensure-context-open who ctx)
              (let ([wire (encode-alpn who proto*)])
                (ensure-success
                 who
                 (ffi-net-tls-context-set-alpn (tls-context-handle ctx)
                                               wire
                                               0
                                               (bytevector-length wire)))))))

  #|proc:tls-context-policy-set!
The `tls-context-policy-set!` procedure applies `policy` to the open TLS context `ctx`.
It configures protocol bounds, TLS 1.2 ciphers, TLS 1.3 ciphersuites, and ALPN protocols.
The return value is `policy` after all native settings have succeeded.
|#
  (define-who tls-context-policy-set!
    (lambda (ctx policy)
      (pcheck ([tls-context? ctx] [tls-policy? policy])
        (ensure-context-open who ctx)
        (ensure-success
         who
         (ffi-net-tls-context-set-policy
          (tls-context-handle ctx)
          (tls-version->integer who (tls-policy-minimum-version policy) #f)
          (tls-version->integer who (tls-policy-maximum-version policy) #t)
          (or (tls-policy-cipher-list policy) "")
          (or (tls-policy-ciphersuites policy) "")
          (ocsp-policy->integer (tls-policy-ocsp-policy policy))))
        (tls-context-set-alpn! ctx (tls-policy-alpn-protocols policy))
        (tls-context-policy-set-internal! ctx policy)
        policy)))

  #|proc:tls-context-sni-selector-set!
The `tls-context-sni-selector-set!` procedure installs a server-name selector on `ctx`.
`selector` has signature `(server-name) -> tls-context-or-#f` and runs in a Scheme handshake step.
Returning `#f` keeps `ctx`; returning a server context selects its certificate and policy.
The return value is `selector` after native ClientHello pausing is enabled.
|#
  (define-who tls-context-sni-selector-set!
    (lambda (ctx selector)
      (pcheck ([tls-context? ctx] [procedure? selector])
        (ensure-context-open who ctx)
        (unless (eq? (tls-context-mode ctx) 'server)
          (errorf who "SNI selection requires a server TLS context"))
        (ensure-success who (ffi-net-tls-context-enable-sni (tls-context-handle ctx)))
        (tls-context-sni-selector-set-internal! ctx selector)
        selector)))

  #|proc:tls-context-session-ticket-set!
The `tls-context-session-ticket-set!` procedure installs `ticket` for the next client handshake.
`ctx` is an open client TLS context and `ticket` contains serialized session state.
The return value is `ticket` after OpenSSL accepts the serialized session.
|#
  (define-who tls-context-session-ticket-set!
    (lambda (ctx ticket)
      (pcheck ([tls-context? ctx] [tls-session-ticket? ticket])
        (ensure-context-open who ctx)
        (unless (eq? (tls-context-mode ctx) 'client)
          (errorf who "session tickets can only be installed on client contexts"))
        (let ([data (%tls-session-ticket-data ticket)])
          (ensure-success
           who
           (ffi-net-tls-context-import-session
            (tls-context-handle ctx) data 0 (bytevector-length data))))
        ticket)))

  #|proc:tls-connect
The `tls-connect` procedure performs a client-side TLS handshake over an existing socket.
|#
  (define-who tls-connect
    (case-lambda
      [(ctx sock)
       (pcheck ([tls-context? ctx] [socket? sock])
               (tls-connect* who ctx sock #f tls-no-timeout))]
      [(ctx sock server-name)
       (pcheck ([tls-context? ctx] [socket? sock])
               (unless (or (not server-name) (string? server-name))
                 (errorf who "server name must be a string or #f, given ~s" server-name))
               (tls-connect* who ctx sock server-name tls-no-timeout))]
      [(ctx sock server-name timeout-ms)
       (pcheck ([tls-context? ctx] [socket? sock])
               (unless (or (not server-name) (string? server-name))
                 (errorf who "server name must be a string or #f, given ~s" server-name))
               (check-timeout-ms who timeout-ms)
               (tls-connect* who ctx sock server-name timeout-ms))]))

  #|proc:tls-connect/nonblocking
The `tls-connect/nonblocking` procedure creates a client TLS handshake operation.
The `ctx` parameter is an open client TLS context.
The `sock` parameter is a connected socket used by the handshake and resulting session.
The optional `server-name` parameter is a hostname for SNI and certificate verification, or `#f`.
The optional `timeout-ms` parameter is the nonnegative handshake timeout in milliseconds.
The return value is a network operation whose result is a TLS session.
|#
  (define-who tls-connect/nonblocking
    (case-lambda
      [(ctx sock)
       (pcheck ([tls-context? ctx] [socket? sock])
               (make-tls-handshake-operation
                who 'tls-connect ctx sock #f tls-no-timeout))]
      [(ctx sock server-name)
       (pcheck ([tls-context? ctx] [socket? sock])
               (unless (or (not server-name) (string? server-name))
                 (errorf who "server name must be a string or #f, given ~s" server-name))
               (make-tls-handshake-operation
                who 'tls-connect ctx sock server-name tls-no-timeout))]
      [(ctx sock server-name timeout-ms)
       (pcheck ([tls-context? ctx] [socket? sock] [fixnum? timeout-ms])
               (unless (or (not server-name) (string? server-name))
                 (errorf who "server name must be a string or #f, given ~s" server-name))
               (check-timeout-ms who timeout-ms)
               (make-tls-handshake-operation
                who 'tls-connect ctx sock server-name timeout-ms))]))

  #|proc:tls-accept
The `tls-accept` procedure performs a server-side TLS handshake over an existing socket.
|#
  (define-who tls-accept
    (case-lambda
      [(ctx sock)
       (pcheck ([tls-context? ctx] [socket? sock])
               (tls-accept* who ctx sock tls-no-timeout))]
      [(ctx sock timeout-ms)
       (pcheck ([tls-context? ctx] [socket? sock])
               (check-timeout-ms who timeout-ms)
               (tls-accept* who ctx sock timeout-ms))]))

  #|proc:tls-accept/nonblocking
The `tls-accept/nonblocking` procedure creates a server TLS handshake operation.
The `ctx` parameter is an open server TLS context.
The `sock` parameter is a connected socket used by the handshake and resulting session.
The optional `timeout-ms` parameter is the nonnegative handshake timeout in milliseconds.
The return value is a network operation whose result is a TLS session.
|#
  (define-who tls-accept/nonblocking
    (case-lambda
      [(ctx sock)
       (pcheck ([tls-context? ctx] [socket? sock])
               (make-tls-handshake-operation
                who 'tls-accept ctx sock #f tls-no-timeout))]
      [(ctx sock timeout-ms)
       (pcheck ([tls-context? ctx] [socket? sock] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (make-tls-handshake-operation who 'tls-accept ctx sock #f timeout-ms))]))

  #|proc:close-tls-session
The `close-tls-session` procedure releases foreign resources owned by a TLS session.
|#
  (define-who close-tls-session
    (lambda (session)
      (pcheck ([tls-session? session])
              (unless (tls-session-closed? session)
                (ensure-success who (ffi-net-tls-close (tls-session-handle session)))
                (tls-session-handle-set! session 0)
                (tls-session-closed?-set! session #t))
              session)))

  #|proc:tls-read
The `tls-read` procedure reads up to `size` bytes from a TLS session.
|#
  (define-who tls-read
    (case-lambda
      [(session size)
       (pcheck ([tls-session? session] [fixnum? size])
               (check-size who size)
               (tls-read* who session size tls-no-timeout #f))]
      [(session size timeout-ms)
       (pcheck ([tls-session? session] [fixnum? size])
               (check-size who size)
               (check-timeout-ms who timeout-ms)
               (tls-read* who session size timeout-ms #f))]))

  #|proc:tls-read/nonblocking
The `tls-read/nonblocking` procedure attempts one TLS read from `session` for up to `size` bytes.
The `session` parameter is an open TLS session. The `size` parameter is the maximum byte count.
The return value is a bytevector, EOF, or a would-block value naming the session socket.
|#
  (define-who tls-read/nonblocking
    (lambda (session size)
      (pcheck ([tls-session? session] [fixnum? size])
              (check-size who size)
              (tls-read* who session size tls-no-timeout #t))))

  #|proc:tls-read!
The `tls-read!` procedure reads into a bytevector slice and returns a byte count or EOF object.
|#
  (define-who tls-read!
    (case-lambda
      [(session bv) (tls-read! session bv 0 (bytevector-length bv))]
      [(session bv start) (tls-read! session bv start (bytevector-length bv))]
      [(session bv start stop)
       (pcheck ([tls-session? session] [bytevector? bv])
               (tls-read-into* who session bv start stop tls-no-timeout #f))]
      [(session bv start stop timeout-ms)
       (pcheck ([tls-session? session] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (tls-read-into* who session bv start stop timeout-ms #f))]))

  #|proc:tls-read!/nonblocking
The `tls-read!/nonblocking` procedure attempts one TLS read into a bytevector slice.
The `session` parameter is an open TLS session. The `bv` parameter receives the bytes.
The optional `start` and `stop` parameters delimit the half-open destination slice.
The return value is a byte count, EOF, or a would-block value naming the session socket.
|#
  (define-who tls-read!/nonblocking
    (case-lambda
      [(session bv) (tls-read!/nonblocking session bv 0 (bytevector-length bv))]
      [(session bv start) (tls-read!/nonblocking session bv start (bytevector-length bv))]
      [(session bv start stop)
       (pcheck ([tls-session? session] [bytevector? bv])
               (tls-read-into* who session bv start stop tls-no-timeout #t))]))

  #|proc:tls-write
The `tls-write` procedure writes a bytevector slice to a TLS session and returns the number of
bytes written.
|#
  (define-who tls-write
    (case-lambda
      [(session bv) (tls-write session bv 0 (bytevector-length bv))]
      [(session bv start) (tls-write session bv start (bytevector-length bv))]
      [(session bv start stop)
       (pcheck ([tls-session? session] [bytevector? bv])
               (tls-write* who session bv start stop tls-no-timeout #f))]
      [(session bv start stop timeout-ms)
       (pcheck ([tls-session? session] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (tls-write* who session bv start stop timeout-ms #f))]))

  #|proc:tls-write/nonblocking
The `tls-write/nonblocking` procedure attempts one TLS write from a bytevector slice.
The `session` parameter is an open TLS session. The `bv` parameter contains the bytes to write.
The optional `start` and `stop` parameters delimit the half-open source slice.
The return value is a byte count or a would-block value naming the session socket.
|#
  (define-who tls-write/nonblocking
    (case-lambda
      [(session bv) (tls-write/nonblocking session bv 0 (bytevector-length bv))]
      [(session bv start) (tls-write/nonblocking session bv start (bytevector-length bv))]
      [(session bv start stop)
       (pcheck ([tls-session? session] [bytevector? bv])
               (tls-write* who session bv start stop tls-no-timeout #t))]))

  #|proc:tls-write-all
The `tls-write-all` procedure writes an entire bytevector slice to a TLS session before returning.
|#
  (define-who tls-write-all
    (case-lambda
      [(session bv) (tls-write-all session bv 0 (bytevector-length bv))]
      [(session bv start) (tls-write-all session bv start (bytevector-length bv))]
      [(session bv start stop)
       (pcheck ([tls-session? session] [bytevector? bv])
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (loop (fx+ i (tls-write* who session bv i stop tls-no-timeout #f))))))]
      [(session bv start stop timeout-ms)
       (pcheck ([tls-session? session] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (loop (fx+ i (tls-write* who session bv i stop timeout-ms #f))))))]))

  #|proc:tls-write-all/nonblocking
The `tls-write-all/nonblocking` procedure writes as much of a bytevector slice as possible.
The `session` parameter is an open TLS session. The `bv` parameter contains the bytes to write.
The optional `start` and `stop` parameters delimit the half-open source slice.
The return value is a byte count or a would-block value when no bytes were written.
|#
  (define-who tls-write-all/nonblocking
    (case-lambda
      [(session bv) (tls-write-all/nonblocking session bv 0 (bytevector-length bv))]
      [(session bv start) (tls-write-all/nonblocking session bv start (bytevector-length bv))]
      [(session bv start stop)
       (pcheck ([tls-session? session] [bytevector? bv])
               (ensure-session-open who session)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (let ([n (tls-write/nonblocking session bv i stop)])
                       (cond
                        [(net-would-block? n)
                         (if (fx> i start) (fx- i start) n)]
                        [(fx= n 0) (fx- i start)]
                        [else (loop (fx+ i n))])))))]))

  #|proc:tls-flush
The `tls-flush` procedure flushes buffered TLS writes and currently acts as a no-op marker.
|#
  (define-who tls-flush
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              #t)))

  #|proc:tls-shutdown!
The `tls-shutdown!` procedure performs an orderly TLS shutdown.
|#
  (define-who tls-shutdown!
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (ensure-success who (ffi-net-tls-shutdown (tls-session-handle session))))))

  #|proc:tls-peer-certificate
The `tls-peer-certificate` procedure returns the peer certificate as a `(cspp crypto cert)`
certificate object, or `#f`.
|#
  (define-who tls-peer-certificate
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (let ([ans (ensure-success who (ffi-net-tls-peer-certificate-der (tls-session-handle session)))])
                (maybe-derive-peer-certificate ans)))))

  #|proc:tls-peer-certificate-chain
The `tls-peer-certificate-chain` procedure returns the presented certificate chain as a list of
certificate objects.
|#
  (define-who tls-peer-certificate-chain
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (let ([ans (ensure-success who (ffi-net-tls-peer-certificate-chain-der (tls-session-handle session)))])
                (maybe-derive-peer-certificate-chain ans)))))

  #|proc:tls-peer-certificate-chain-records
The `tls-peer-certificate-chain-records` procedure snapshots the peer chain from `session`.
The return value is a list of `tls-certificate` records ordered from leaf toward the trust anchor.
Each record owns copied metadata, and its position starts at zero.
|#
  (define-who tls-peer-certificate-chain-records
    (lambda (session)
      (pcheck ([tls-session? session])
        (ensure-session-open who session)
        (let ([der* (ensure-success
                     who
                     (ffi-net-tls-peer-certificate-chain-der
                      (tls-session-handle session)))]
              [verified? (tls-verified? session)])
          (let loop ([rest der*] [position 0])
            (if (null? rest)
                '()
                (cons (certificate-der->tls-certificate
                       (car rest) position verified?)
                      (loop (cdr rest) (fx1+ position)))))))))

  #|proc:tls-protocol-version
The `tls-protocol-version` procedure returns the negotiated TLS protocol version string.
|#
  (define-who tls-protocol-version
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (ensure-success who (ffi-net-tls-protocol-version (tls-session-handle session))))))

  #|proc:tls-negotiated-alpn
The `tls-negotiated-alpn` procedure returns the protocol selected during the `session` handshake.
The return value is a protocol string, or `#f` when the peers did not negotiate ALPN.
|#
  (define-who tls-negotiated-alpn
    (lambda (session)
      (pcheck ([tls-session? session])
        (ensure-session-open who session)
        (ensure-success who (ffi-net-tls-negotiated-alpn (tls-session-handle session))))))

  #|proc:tls-cipher-name
The `tls-cipher-name` procedure returns the negotiated cipher-suite name.
|#
  (define-who tls-cipher-name
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (ensure-success who (ffi-net-tls-cipher-name (tls-session-handle session))))))

  #|proc:tls-verified?
The `tls-verified?` procedure returns `#t` when peer verification succeeded.
|#
  (define-who tls-verified?
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (ensure-success who (ffi-net-tls-verified (tls-session-handle session))))))

  #|proc:tls-session-export-ticket
The `tls-session-export-ticket` procedure serializes resumable state from `session`.
The return value is an immutable `tls-session-ticket` record containing copied session bytes.
|#
  (define-who tls-session-export-ticket
    (lambda (session)
      (pcheck ([tls-session? session])
        (ensure-session-open who session)
        (%make-tls-session-ticket
         (ensure-success who (ffi-net-tls-session-export (tls-session-handle session)))))))

  #|proc:tls-session-reused?
The `tls-session-reused?` procedure reports whether `session` resumed an earlier TLS session.
The return value is `#t` for a resumed handshake and `#f` for a full handshake.
|#
  (define-who tls-session-reused?
    (lambda (session)
      (pcheck ([tls-session? session])
        (ensure-session-open who session)
        (ensure-success who (ffi-net-tls-session-reused (tls-session-handle session))))))

  #|proc:tls-stapled-ocsp-response
The `tls-stapled-ocsp-response` procedure copies the peer's stapled OCSP bytes from `session`.
The return value is a DER bytevector, or `#f` when the peer supplied no staple.
|#
  (define-who tls-stapled-ocsp-response
    (lambda (session)
      (pcheck ([tls-session? session])
        (ensure-session-open who session)
        (ensure-success who (ffi-net-tls-stapled-ocsp (tls-session-handle session))))))

  #|proc:tls-session-ocsp-result
The `tls-session-ocsp-result` procedure validates the stapled OCSP response on `session`.
The return value is a verified `tls-ocsp-result`, or `#f` when no response was stapled.
Malformed, mismatched, expired, revoked, unknown, or invalidly signed responses raise a TLS error.
|#
  (define-who tls-session-ocsp-result
    (lambda (session)
      (pcheck ([tls-session? session])
        (ensure-session-open who session)
        (let ([answer (ensure-success
                       who
                       (ffi-net-tls-ocsp-result (tls-session-handle session)))])
          (if (not answer)
              #f
              (let ([response (tls-stapled-ocsp-response session)])
                (unless (and (vector? answer)
                             (= (vector-length answer) 3)
                             (eq? (vector-ref answer 0) 'good)
                             (boolean? (vector-ref answer 1))
                             (boolean? (vector-ref answer 2))
                             (bytevector? response))
                  (raise-net-error who 'tls "malformed OCSP validation result" answer))
                (%make-tls-ocsp-result
                 (vector-ref answer 0)
                 (vector-ref answer 1)
                 (vector-ref answer 2)
                 response)))))))

  #|proc:tls-capabilities
The `tls-capabilities` procedure reports optional TLS features available in this OpenSSL build.
The return value is an association list with Boolean session, SNI, and OCSP capability entries.
|#
  (define tls-capabilities
    (lambda ()
      '((session-serialization . #t)
        (sni-selection . #t)
        (ocsp-stapling . #t))))

  #|proc:call-with-tls-client
The `call-with-tls-client` procedure performs a client TLS handshake, passes the session to a
procedure, and closes the session afterwards.
|#
  (define-who call-with-tls-client
    (case-lambda
      [(ctx sock proc) (call-with-tls-client ctx sock #f proc)]
      [(ctx sock timeout-ms proc)
       (pcheck ([tls-context? ctx] [socket? sock] [fixnum? timeout-ms] [procedure? proc])
               (check-timeout-ms who timeout-ms)
               (let ([session (tls-connect ctx sock #f timeout-ms)])
                 (dynamic-wind
                   void
                   (lambda () (proc session))
                   (lambda () (when session (close-tls-session session))))))]
      [(ctx sock server-name proc)
       (pcheck ([tls-context? ctx] [socket? sock] [procedure? proc])
               (let ([session (tls-connect ctx sock server-name)])
                 (dynamic-wind
                   void
                   (lambda () (proc session))
                   (lambda () (when session (close-tls-session session))))))]
      [(ctx sock server-name timeout-ms proc)
       (pcheck ([tls-context? ctx] [socket? sock] [fixnum? timeout-ms] [procedure? proc])
               (check-timeout-ms who timeout-ms)
               (unless (or (not server-name) (string? server-name))
                 (errorf who "server name must be a string or #f, given ~s" server-name))
               (let ([session (tls-connect ctx sock server-name timeout-ms)])
                 (dynamic-wind
                   void
                   (lambda () (proc session))
                   (lambda () (when session (close-tls-session session))))))]))

  #|proc:call-with-tls-server
The `call-with-tls-server` procedure performs a server TLS handshake, passes the session to a
procedure, and closes the session afterwards.
|#
  (define-who call-with-tls-server
    (case-lambda
      [(ctx sock proc)
       (pcheck ([tls-context? ctx] [socket? sock] [procedure? proc])
               (let ([session (tls-accept ctx sock)])
                 (dynamic-wind
                   void
                   (lambda () (proc session))
                   (lambda () (when session (close-tls-session session))))))]
      [(ctx sock timeout-ms proc)
       (pcheck ([tls-context? ctx] [socket? sock] [fixnum? timeout-ms] [procedure? proc])
               (check-timeout-ms who timeout-ms)
               (let ([session (tls-accept ctx sock timeout-ms)])
                 (dynamic-wind
                   void
                   (lambda () (proc session))
                   (lambda () (when session (close-tls-session session))))))]))

  #|proc:open-tls-port
The `open-tls-port` procedure opens a bidirectional binary port layered over a TLS session.
|#
  (define-who open-tls-port
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (make-custom-binary-input/output-port
               "cspp-tls-port"
               (lambda (bv start count)
                 (tls-read! session bv start (fx+ start count)))
               (lambda (bv start count)
                 (tls-write-all session bv start (fx+ start count)))
               (lambda () #f)
               (lambda (x) #f)
               (lambda () #t)))))

  #|proc:open-tls-input-port
The `open-tls-input-port` procedure opens a binary input port layered over a TLS session.
|#
  (define-who open-tls-input-port
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (make-binary-input-port session))))

  #|proc:open-tls-output-port
The `open-tls-output-port` procedure opens a binary output port layered over a TLS session.
|#
  (define-who open-tls-output-port
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (make-binary-output-port session))))

  #|proc:open-tls-text-input-port
The `open-tls-text-input-port` procedure opens a text input port layered over a TLS session.
|#
  (define-who open-tls-text-input-port
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (transcoded-port (open-tls-input-port session) (native-transcoder)))))

  #|proc:open-tls-text-output-port
The `open-tls-text-output-port` procedure opens a text output port layered over a TLS session.
|#
  (define-who open-tls-text-output-port
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (transcoded-port (open-tls-output-port session) (native-transcoder)))))

  #|proc:call-with-tls-ports
The `call-with-tls-ports` procedure opens binary TLS ports, passes them to a procedure, and closes
the wrapper ports afterwards.
|#
  (define-who call-with-tls-ports
    (lambda (session proc)
      (pcheck ([tls-session? session] [procedure? proc])
              (ensure-session-open who session)
              (let ([ip (open-tls-input-port session)]
                    [op (open-tls-output-port session)])
                (dynamic-wind
                  void
                  (lambda () (proc ip op))
                  (lambda ()
                    (close-port ip)
                    (close-port op)))))))
  )
