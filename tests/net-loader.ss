(import (cspp) (cspp optional-library) (cspp net)
        (cspp net private lws-ffi))

(define check-unavailable
  (lambda (name expected)
    (let ([info (optional-library-info name)])
      (and (optional-library-info? info)
           (if (optional-library-available? info)
               (or (not (optional-library-version info))
                   (string? (optional-library-version info)))
               (and (string? (optional-library-error info))
                    (or (string-contains? (optional-library-error info) expected)
                        (string-contains? (optional-library-error info)
                                          "disabled at build time"))))))))

(mat optional-library-record
     (let ([info (optional-library-info 'openssl)])
       (and (optional-library-info? info)
            (eq? (optional-library-name info) 'openssl)
            (boolean? (optional-library-available? info))
            (or (not (optional-library-version info))
                (string? (optional-library-version info)))
            (list? (optional-library-capabilities info))
            (or (not (optional-library-error info))
                (string? (optional-library-error info))))))

(mat net-loader-errors
     (check-unavailable 'curl "curl")
     (check-unavailable 'ssh "ssh")
     (check-unavailable 'websockets "websockets")
     (check-unavailable 'grpc "grpc")
     (check-unavailable 'zlib "zlib")
     (check-unavailable 'cares "c-ares")
     (check-unavailable 'idn2 "libidn2"))

(mat net-loader-websocket-capabilities
     (let ([capabilities (optional-library-capabilities (optional-library-info 'websockets))])
       (for-all (lambda (capability) (eq? capability 'tls)) capabilities)))

(mat net-loader-does-not-expose-nghttp2
     ;; Error case: nghttp2 is an implementation detail of the optional LWS runtime.
     (guard (condition [else #t])
       (optional-library-info 'nghttp2)
       #f))

(mat net-lws-http-loader-status
     (let ([status (lws-status)])
       (and (vector? status)
            (= (vector-length status) 4)
            (boolean? (vector-ref status 0))
            (natural? (vector-ref status 1))
            (or (not (vector-ref status 2))
                (string? (vector-ref status 2)))
            (or (not (vector-ref status 3))
                (string? (vector-ref status 3)))
            (if (vector-ref status 0)
                (and (lws-capability? (vector-ref status 1) lws-cap-http1)
                     (lws-capability? (vector-ref status 1) lws-cap-external-poll))
                (string? (vector-ref status 3))))))

(mat net-lws-http-loader-required-capability
     (let ([status (lws-status)])
       (if (vector-ref status 0)
           (begin
             (lws-require-capability! lws-cap-http1 'http1)
             (lws-require-capability! lws-cap-external-poll 'external-poll)
             #t)
           (guard (condition
                   [(net-error? condition)
                    (string-contains? (net-error-message condition) "websockets")]
                   [else #f])
             (lws-require-capability! lws-cap-http1 'http1)
             #f))))
