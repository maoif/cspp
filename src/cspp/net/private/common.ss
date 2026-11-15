(library (cspp net private common)
  (export socket-address?
          socket?
          socket-fd
          socket-fd-set!
          socket-family
          socket-type
          socket-proto
          socket-blocking?
          socket-blocking-set!
          socket-closed?
          socket-closed-set!
          %make-socket
          socket-address-family
          socket-address-host
          socket-address-port
          socket-address-path
          dns-result?
          dns-result-addresses
          dns-result-canonname
          dns-result-query-name
          dns-result-aliases
          dns-result-record-type
          dns-result-ttls
          dns-result-status
          dns-result-partial-errors
          %make-socket-address
          %make-dns-result
          %socket-address-from-ffi
          %dns-result-from-ffi
          family-symbol->int
          type-symbol->int
          shutdown-symbol->int
          check-port
          ffi-error?
          ffi-would-block?
          ffi-would-block-read?
          ffi-would-block-write?
          ffi-would-block-event
          ffi-would-block-events
          ffi-error-message)
  (import (cspp chez)
          (cspp net private ffi)
          (cspp net errors))

  (define-record-type (socket %make-socket socket?)
    (sealed #t)
    (opaque #f)
    (fields (mutable fd socket-fd socket-fd-set!)
            (immutable family socket-family)
            (immutable type socket-type)
            (immutable proto socket-proto)
            (mutable blocking socket-blocking? socket-blocking-set!)
            (mutable closed socket-closed? socket-closed-set!)))

  (define-record-type (socket-address %make-socket-address socket-address?)
    (sealed #t)
    (opaque #f)
    (fields (immutable family socket-address-family)
            (immutable host socket-address-host)
            (immutable port socket-address-port)
            (immutable path socket-address-path)))

  (define-record-type (dns-result %make-dns-result dns-result?)
    (sealed #t)
    (opaque #f)
    (fields (immutable query-name dns-result-query-name)
            (immutable canonname dns-result-canonname)
            (immutable addresses dns-result-addresses)
            (immutable aliases dns-result-aliases)
            (immutable record-type dns-result-record-type)
            (immutable ttls dns-result-ttls)
            (immutable status dns-result-status)
            (immutable partial-errors dns-result-partial-errors)))

  (define raise-malformed-ffi
    (lambda (who expected value)
      (raise-net-error who 'internal-ffi
                       (format "malformed native result; expected ~a" expected)
                       value)))

  ;; Error result: #(error message), where message is an owned Scheme string.
  (define ffi-error?
    (lambda (value)
      (and (vector? value)
           (fx> (vector-length value) 0)
           (eq? (vector-ref value 0) 'error)
           (if (and (fx= (vector-length value) 2)
                    (string? (vector-ref value 1)))
               #t
               (raise-malformed-ffi 'ffi-error? "#(error string)" value)))))

  ;; Blocking result: #(tag events). The tag names a blocking direction. For the generic tag,
  ;; events is an event symbol or a nonempty list of event symbols. All values are Scheme-owned.
  (define ffi-would-block?
    (lambda (value)
      (and (vector? value)
           (fx> (vector-length value) 0)
           (memq (vector-ref value 0) '(would-block would-block-read would-block-write))
           (begin
             (unless (fx= (vector-length value) 2)
               (raise-malformed-ffi 'ffi-would-block? "#(would-block-tag events)" value))
             #t))))

  (define ffi-would-block-read?
    (lambda (value)
      (and (ffi-would-block? value)
           (eq? (vector-ref value 0) 'would-block-read))))

  (define ffi-would-block-write?
    (lambda (value)
      (and (ffi-would-block? value)
           (eq? (vector-ref value 0) 'would-block-write))))

  (define ffi-would-block-event
    (lambda (answer)
      (car (ffi-would-block-events answer))))

  (define ffi-would-block-events
    (lambda (answer)
      (unless (and (vector? answer) (fx= (vector-length answer) 2))
        (raise-malformed-ffi 'ffi-would-block-events "#(would-block-tag events)" answer))
      (case (vector-ref answer 0)
        [(would-block-read)
         (unless (memq (vector-ref answer 1) '(#f read))
           (raise-malformed-ffi 'ffi-would-block-events
                                "#(would-block-read #f-or-read)" answer))
         '(read)]
        [(would-block-write)
         (unless (memq (vector-ref answer 1) '(#f write))
           (raise-malformed-ffi 'ffi-would-block-events
                                "#(would-block-write #f-or-write)" answer))
         '(write)]
        [(would-block)
         (let ([event* (vector-ref answer 1)])
           (let ([events (if (pair? event*) event* (list event*))])
             (unless (and (pair? events)
                          (andmap (lambda (event) (memq event '(read write))) events))
               (raise-malformed-ffi 'ffi-would-block-events
                                    "#(would-block read-or-write-events)" answer))
             events))]
        [else
         (raise-malformed-ffi 'ffi-would-block-events "#(would-block-tag events)" answer)])))

  (define ffi-error-message
    (lambda (value)
      (and (ffi-error? value)
           (vector-ref value 1))))

  ;; Socket address result: #(family host port path). Family is inet, inet6, or unix. Host and
  ;; path are strings or #f; port is an integer in [0, 65535] or #f. Strings are Scheme-owned.
  (define %socket-address-from-ffi
    (lambda (value)
      (unless (and (vector? value)
                   (fx= (vector-length value) 4)
                   (memq (vector-ref value 0) '(inet inet6 unix))
                   (or (string? (vector-ref value 1)) (not (vector-ref value 1)))
                   (or (and (fixnum? (vector-ref value 2))
                            (fx<= 0 (vector-ref value 2) 65535))
                       (not (vector-ref value 2)))
                   (or (string? (vector-ref value 3)) (not (vector-ref value 3))))
        (raise-malformed-ffi '%socket-address-from-ffi
                             "#(family host-or-#f port-or-#f path-or-#f)" value))
      (%make-socket-address (vector-ref value 0)
                            (vector-ref value 1)
                            (vector-ref value 2)
                            (vector-ref value 3))))

  ;; DNS result: #(canonical-name addresses). Canonical-name is a string or #f and addresses is a
  ;; proper list of socket address vectors. The list and all nested vectors are Scheme-owned.
  (define %dns-result-from-ffi
    (lambda (value)
      (unless (and (vector? value)
                   (fx= (vector-length value) 2)
                   (or (string? (vector-ref value 0)) (not (vector-ref value 0)))
                   (list? (vector-ref value 1)))
        (raise-malformed-ffi '%dns-result-from-ffi
                             "#(canonical-name-or-#f socket-address-list)" value))
      (%make-dns-result #f
                        (vector-ref value 0)
                        (map %socket-address-from-ffi (vector-ref value 1))
                        '() 'address '() 'success '())))

  (define family-symbol->int
    (lambda (who family)
      (case family
        [(#f) 0]
        [(inet) (net-af-inet)]
        [(inet6) (net-af-inet6)]
        [(unix) (net-af-unix)]
        [else (errorf who "invalid socket family ~s" family)])))

  (define type-symbol->int
    (lambda (who type)
      (case type
        [(#f) 0]
        [(stream) (net-sock-stream)]
        [(datagram) (net-sock-datagram)]
        [(seqpacket) (net-sock-seqpacket)]
        [else (errorf who "invalid socket type ~s" type)])))

  (define shutdown-symbol->int
    (lambda (who how)
      (case how
        [(read) (net-shut-read)]
        [(write) (net-shut-write)]
        [(read/write) (net-shut-read/write)]
        [else (errorf who "invalid shutdown mode ~s" how)])))

  (define check-port
    (lambda (who port)
      (when (or (fx< port 0) (fx> port 65535))
        (errorf who "port must be between 0 and 65535, given ~s" port))
      port))
  )
