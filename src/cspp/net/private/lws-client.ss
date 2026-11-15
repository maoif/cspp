(library (cspp net private lws-client)
  (export make-lws-client-transport
          lws-client-request/nonblocking
          lws-client-close!
          lws-client-release-operation!
          lws-client-pool-metrics)
  (import (cspp chez)
          (cspp utils)
          (cspp net operation)
          (cspp net private http)
          (cspp net private lws-http1)
          (cspp net private lws-http2))

  #|record:lws-client-transport
An internal shared transport selecting the LWS HTTP/1 or HTTP/2 reducer.
|#
  (define-record-type (lws-client-transport %make-lws-client-transport lws-client-transport?)
    (sealed #t) (opaque #t)
    (fields (immutable version lws-client-transport-version)
            (immutable implementation lws-client-transport-implementation)))

  #|proc:make-lws-client-transport
Creates an LWS transport for protocol `version` and bounded pool parameters.
Returns a shared transport boundary used by both HTTP reducers.
|#
  (define make-lws-client-transport
    (lambda (version event-capacity payload-capacity command-capacity tls-context
             proxy-address proxy-port max-active max-idle idle-timeout-ms)
      (pcheck ([symbol? version]
               [positive-natural? event-capacity payload-capacity command-capacity]
               [natural? tls-context proxy-port max-idle idle-timeout-ms]
               [string? proxy-address] [positive-natural? max-active])
        (%make-lws-client-transport
         version
         (if (eq? version 'h2)
             (make-lws-http2-client event-capacity payload-capacity command-capacity tls-context
                                    max-active)
             (make-lws-http1-client event-capacity payload-capacity command-capacity tls-context
                                     proxy-address proxy-port max-active max-idle idle-timeout-ms))))))

  #|proc:lws-client-request/nonblocking
Starts normalized `request` on shared `transport`, optionally writing to `sink`.
Returns the protocol reducer's network operation.
|#
  (define lws-client-request/nonblocking
    (lambda (transport request sink)
      (pcheck ([lws-client-transport? transport] [normalized-http-request? request]
               [(lambda (value) (or (not value) (vector? value))) sink])
        (if (eq? (lws-client-transport-version transport) 'h2)
            (lws-http2-request/nonblocking
             (lws-client-transport-implementation transport) request sink)
            (lws-http1-request/nonblocking
             (lws-client-transport-implementation transport) request sink)))))

  #|proc:lws-client-release-operation!
Releases terminal `operation` from shared `transport` and returns the transport.
|#
  (define lws-client-release-operation!
    (lambda (transport operation)
      (pcheck ([lws-client-transport? transport] [net-operation? operation])
        (if (eq? (lws-client-transport-version transport) 'h2)
            (lws-http2-release-operation! (lws-client-transport-implementation transport) operation)
            (lws-http1-release-operation! (lws-client-transport-implementation transport) operation))
        transport)))

  #|proc:lws-client-close!
Closes shared `transport` and returns it. Closing is idempotent.
|#
  (define lws-client-close!
    (lambda (transport)
      (pcheck ([lws-client-transport? transport])
        (if (eq? (lws-client-transport-version transport) 'h2)
            (lws-http2-client-close! (lws-client-transport-implementation transport))
            (lws-http1-client-close! (lws-client-transport-implementation transport))))))

  #|proc:lws-client-pool-metrics
Returns implementation-specific pool metrics for shared `transport`.
|#
  (define lws-client-pool-metrics
    (lambda (transport)
      (pcheck ([lws-client-transport? transport])
        (if (eq? (lws-client-transport-version transport) 'h2)
            (lws-http2-client-pool-metrics (lws-client-transport-implementation transport))
            (lws-http1-client-pool-metrics (lws-client-transport-implementation transport))))))
  )
