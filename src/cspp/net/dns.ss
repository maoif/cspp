(library (cspp net dns)
  (export dns-options?
          make-dns-options
          dns-options-family
          dns-options-type
          dns-options-timeout-ms
          dns-options-canonical-name?
          default-dns-options
          dns-resolve
          dns-resolve/nonblocking
          dns-resolve/ipv4
          dns-resolve/ipv6
          dns-reverse-resolve
          dns-result?
          dns-result-query-name
          dns-result-canonname
          dns-result-addresses
          dns-result-aliases
          dns-result-record-type
          dns-result-ttls
          dns-result-status
          dns-result-partial-errors)
  (import (cspp chez)
          (only (cspp utils) define-who natural? pcheck)
          (cspp optional-library)
          (cspp net errors)
          (cspp net private ffi)
          (cspp net private common)
          (cspp net address)
          (cspp net poll)
          (cspp net operation))

  #|record:dns-options
The `dns-options` record configures one address lookup.
`family` is `unspecified`, `ipv4`, or `ipv6`; `type` is `address`.
`timeout-ms` is the total timeout, and `canonical-name?` requests canonical-name data.
|#
  (define-record-type (dns-options %make-dns-options dns-options?)
    (sealed #t)
    (opaque #f)
    (fields (immutable family dns-options-family)
            (immutable type dns-options-type)
            (immutable timeout-ms dns-options-timeout-ms)
            (immutable canonical-name? dns-options-canonical-name?)))

  #|proc:make-dns-options
The `make-dns-options` procedure constructs DNS lookup options.
The `family`, `type`, `timeout-ms`, and `canonical-name?` parameters configure the lookup.
The return value is an immutable DNS options record.
|#
  (define-who make-dns-options
    (lambda (family type timeout-ms canonical-name?)
      (pcheck ([symbol? family type] [fixnum? timeout-ms]
               [boolean? canonical-name?])
        (unless (memq family '(unspecified ipv4 ipv6))
          (errorf who "DNS family must be unspecified, ipv4, or ipv6, given ~s" family))
        (unless (eq? type 'address)
          (errorf who "DNS record type must be address, given ~s" type))
        (when (fx< timeout-ms 0)
          (errorf who "DNS timeout must be nonnegative, given ~s" timeout-ms))
        (%make-dns-options family type timeout-ms canonical-name?))))

  (define default-dns-options
    (%make-dns-options 'unspecified 'address 5000 #t))

  (define current-monotonic-ms
    (lambda ()
      (let ([time (current-time 'time-monotonic)])
        (+ (* (time-second time) 1000)
           (quotient (time-nanosecond time) 1000000)))))

  (define dns-family->int
    (lambda (family)
      (case family
        [(unspecified) 0]
        [(ipv4) (net-af-inet)]
        [(ipv6) (net-af-inet6)]
        [else (assert-unreachable)])))

  (define dns-status-symbol
    (lambda (status)
      (case status
        [(4) 'not-found]
        [(11 12) 'timeout]
        [(24) 'cancelled]
        [else 'resolver-error])))

  (define dns-status-retryable?
    (lambda (status)
      (memv status '(1 2 3 6 7 8 9 10 11 12 14 15 16 19 20 22))))

  (define raise-dns-status
    (lambda (who host answer)
      (let ([status (vector-ref answer 1)] [message (vector-ref answer 2)])
        (raise-net-error 'dns 'resolve message (dns-status-symbol status) #f host
                         (and (dns-status-retryable? status) #t) #f answer))))

  (define decode-dns-result
    (lambda (who answer)
      ;; Success result: #(dns-ok query-name canonical-name addresses aliases ttls).
      ;; Strings and lists are Scheme-owned; each address uses the private four-field shape.
      (unless (and (vector? answer)
                   (fx= (vector-length answer) 6)
                   (eq? (vector-ref answer 0) 'dns-ok)
                   (string? (vector-ref answer 1))
                   (or (not (vector-ref answer 2)) (string? (vector-ref answer 2)))
                   (list? (vector-ref answer 3))
                   (list? (vector-ref answer 4))
                   (andmap string? (vector-ref answer 4))
                   (list? (vector-ref answer 5))
                   (andmap natural? (vector-ref answer 5)))
        (raise-net-error who 'internal-ffi "malformed c-ares success result" answer))
      (%make-dns-result (vector-ref answer 1)
                        (vector-ref answer 2)
                        (map %socket-address-from-ffi (vector-ref answer 3))
                        (vector-ref answer 4)
                        'address
                        (vector-ref answer 5)
                        'success
                        '())))

  (define pending->targets
    (lambda (who spec*)
      ;; Pending specs are Scheme-owned #(descriptor event-mask) vectors; bit 1 is read and 2 write.
      (unless (and (list? spec*)
                   (andmap (lambda (spec)
                             (and (vector? spec) (fx= (vector-length spec) 2)
                                  (fixnum? (vector-ref spec 0))
                                  (fixnum? (vector-ref spec 1))))
                           spec*))
        (raise-net-error who 'internal-ffi "malformed c-ares poll targets" spec*))
      (map (lambda (spec)
             (let ([mask (vector-ref spec 1)])
               (make-poll-target
                (vector-ref spec 0)
                (append (if (fx= 0 (fxlogand mask 1)) '() '(read))
                        (if (fx= 0 (fxlogand mask 2)) '() '(write))
                        '(error hup invalid)))))
           spec*)))

  #|proc:dns-resolve/nonblocking
  The `dns-resolve/nonblocking` procedure starts a readiness-driven lookup of host string `host`.
  `options` is an optional DNS options record, defaulting to `default-dns-options`.
  It returns a network operation whose successful value is a DNS result record.
  Unavailable c-ares support raises a network error containing the native build diagnostic.
  |#
  (define-who dns-resolve/nonblocking
    (case-lambda
      [(host) (dns-resolve/nonblocking host default-dns-options)]
      [(host options)
       (pcheck ([string? host] [dns-options? options])
         (let ([info (optional-library-info 'cares)])
           (unless (optional-library-available? info)
             (raise-net-error 'dns 'resolve (optional-library-error info)
                              'unsupported #f host #f #f options)))
         (let* ([timeout-ms (dns-options-timeout-ms options)]
                [deadline-ms (+ (current-monotonic-ms) timeout-ms)]
                [handle (ffi-net-dns-start host (dns-family->int (dns-options-family options))
                                           timeout-ms)])
           (define close!
             (lambda ()
               (unless (zero? handle)
                 (ffi-net-dns-close handle)
                 (set! handle 0))))
           (when (zero? handle)
             (raise-net-error 'dns 'resolve "failed to initialize c-ares lookup"
                              'resolver-error #f host #f #f options))
           (make-net-operation
            'dns-resolve
            (lambda ()
              (when (fx>= (current-monotonic-ms) deadline-ms)
                (unless (zero? handle) (ffi-net-dns-cancel handle))
                (raise-net-error 'dns 'resolve "DNS lookup timed out" 'timeout #f host
                                 #t #f options))
              (let ([answer (ffi-net-dns-advance handle)])
                (cond
                 [(and (vector? answer) (fx= (vector-length answer) 6)
                       (eq? (vector-ref answer 0) 'dns-ok))
                  (net-operation-completed (decode-dns-result who answer))]
                 [(and (vector? answer) (fx= (vector-length answer) 3)
                       (eq? (vector-ref answer 0) 'dns-error)
                       (fixnum? (vector-ref answer 1))
                       (string? (vector-ref answer 2)))
                  (raise-dns-status who host answer)]
                 [(and (vector? answer) (fx= (vector-length answer) 3)
                       (eq? (vector-ref answer 0) 'dns-pending)
                       (fixnum? (vector-ref answer 2)))
                  (net-operation-pending
                   (pending->targets who (vector-ref answer 1))
                   (min deadline-ms (+ (current-monotonic-ms) (vector-ref answer 2))))]
                 [(ffi-error? answer)
                  (raise-net-error 'dns 'resolve (ffi-error-message answer)
                                   'resolver-error #f host #f #f answer)]
                 [else
                  (raise-net-error who 'internal-ffi "malformed c-ares operation result"
                                   answer)])))
            (lambda ()
              (unless (zero? handle) (ffi-net-dns-cancel handle)))
            close!)))]))

  #|proc:dns-resolve
The `dns-resolve` procedure resolves `host` using optional DNS `options`.
The return value is a DNS result record after the readiness-driven operation completes.
|#
  (define-who dns-resolve
    (case-lambda
      [(host) (dns-resolve host default-dns-options)]
      [(host options)
       (pcheck ([string? host] [dns-options? options])
         (net-operation-wait (dns-resolve/nonblocking host options)))]))

  #|proc:dns-resolve/ipv4
The `dns-resolve/ipv4` procedure resolves `host` and returns only IPv4 addresses.
|#
  (define-who dns-resolve/ipv4
    (lambda (host)
      (pcheck ([string? host])
        (dns-resolve host (%make-dns-options 'ipv4 'address 5000 #t)))))

  #|proc:dns-resolve/ipv6
The `dns-resolve/ipv6` procedure resolves `host` and returns only IPv6 addresses.
|#
  (define-who dns-resolve/ipv6
    (lambda (host)
      (pcheck ([string? host])
        (dns-resolve host (%make-dns-options 'ipv6 'address 5000 #t)))))

  #|proc:dns-reverse-resolve
The `dns-reverse-resolve` procedure returns the hostname for socket `address`.
|#
  (define-who dns-reverse-resolve
    (lambda (address)
      (pcheck ([socket-address? address])
        (address->name address))))
  )
