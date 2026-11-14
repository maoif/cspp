(library (cspp net private http)
  (export http-request-policy?
          make-http-request-policy
          http-request-policy-headers http-request-policy-auth
          http-request-policy-cookie-jar http-request-policy-proxy
          http-request-policy-tls-context http-request-policy-version
          http-request-policy-pool-policy http-request-policy-follow-redirects?
          http-request-policy-max-redirects http-request-policy-deadline-ms
          normalized-http-request?
          make-normalized-http-request
          normalized-http-request-method normalized-http-request-uri
          normalized-http-request-scheme normalized-http-request-host
          normalized-http-request-port normalized-http-request-tls?
          normalized-http-request-path normalized-http-request-headers
          normalized-http-request-body-factory normalized-http-request-body-length
          normalized-http-request-policy
          transport-response? make-transport-response
          transport-response-status transport-response-reason
          transport-response-headers transport-response-body
          transport-response-trailers transport-response-version
          transport-response-connection-id)
  (import (cspp chez))

  #|record:http-request-policy
An immutable snapshot of client policy used for one HTTP operation.
|#
  (define-record-type (http-request-policy make-http-request-policy http-request-policy?)
    (sealed #t) (opaque #t)
    (fields (immutable headers http-request-policy-headers)
            (immutable auth http-request-policy-auth)
            (immutable cookie-jar http-request-policy-cookie-jar)
            (immutable proxy http-request-policy-proxy)
            (immutable tls-context http-request-policy-tls-context)
            (immutable version http-request-policy-version)
            (immutable pool-policy http-request-policy-pool-policy)
            (immutable follow-redirects? http-request-policy-follow-redirects?)
            (immutable max-redirects http-request-policy-max-redirects)
            (immutable deadline-ms http-request-policy-deadline-ms)))

  #|record:normalized-http-request
An immutable request normalized for a protocol transport.
|#
  (define-record-type
    (normalized-http-request make-normalized-http-request normalized-http-request?)
    (sealed #t) (opaque #t)
    (fields (immutable method normalized-http-request-method)
            (immutable uri normalized-http-request-uri)
            (immutable scheme normalized-http-request-scheme)
            (immutable host normalized-http-request-host)
            (immutable port normalized-http-request-port)
            (immutable tls? normalized-http-request-tls?)
            (immutable path normalized-http-request-path)
            (immutable headers normalized-http-request-headers)
            (immutable body-factory normalized-http-request-body-factory)
            (immutable body-length normalized-http-request-body-length)
            (immutable policy normalized-http-request-policy)))

  #|record:transport-response
An immutable response produced by an HTTP protocol transport.
|#
  (define-record-type (transport-response make-transport-response transport-response?)
    (sealed #t) (opaque #t)
    (fields (immutable status transport-response-status)
            (immutable reason transport-response-reason)
            (immutable headers transport-response-headers)
            (immutable body transport-response-body)
            (immutable trailers transport-response-trailers)
            (immutable version transport-response-version)
            (immutable connection-id transport-response-connection-id))))
