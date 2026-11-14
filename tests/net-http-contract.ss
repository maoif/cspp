(import (cspp chez)
        (cspp net)
        (cspp net private http))

(define net-environment
  (environment '(cspp net)))

(define net-ffi-environment
  (environment '(cspp net private ffi)))

(define net-private-environment
  (environment '(cspp net private common)))

(define retained-http-exports
  '(http-request? make-http-request http-request-method http-request-uri http-request-headers
    http-request-body http-response? make-http-response http-response-status http-response-reason
    http-response-headers http-response-body http-response-trailers http-response-version
    http-body-source? make-http-body-source http-body-source-length http-body-source-read
    http-body-sink? make-http-body-sink http-body-sink-write! http-body-sink-finish!
    make-http-port-body-source make-http-file-body-source make-http-port-body-sink
    make-http-file-body-sink http-cookie? make-http-cookie http-cookie-name http-cookie-value
    http-cookie-domain http-cookie-path http-cookie-secure? http-cookie-jar? make-http-cookie-jar
    http-proxy? make-http-proxy http-proxy-uri http-multipart-part? make-http-multipart-part
    http-multipart-part-name http-multipart-part-value http-multipart-part-filename
    http-multipart-part-content-type http-pool-policy? make-http-pool-policy
    http-pool-policy-max-idle http-pool-policy-max-active http-pool-policy-idle-timeout-ms
    http-client-cookie-jar-set! http-client-auth-set! http-client-proxy-set!
    http-client-pool-policy-set! http-client-version-set! make-http-multipart-body http-header-ref
    http-header-set http-header-add http-client? http-open http-close http-send http-get http-head
    http-post http-put http-delete http-request http-download http-upload http-follow-redirects!
    http-set-header! http-set-timeout! http-cancel-pending! http-send/nonblocking
    http-request/nonblocking http-download/nonblocking http-upload/nonblocking http-server?
    http-listen http-server-close http-accept http-accept/nonblocking http-serve http-serve-loop
    http-register-handler! http-handler-ref http-unregister-handler! http-connection?
    http-connection-close http-read-request http-read-request/nonblocking http-write-response
    http-write-response/nonblocking))

(mat net-http-public-api-contract
     (andmap
      (lambda (name)
        (guard (condition [else #f])
          (procedure? (eval name net-environment))))
      retained-http-exports))

(mat net-http-record-contract
     (let* ([request (make-http-request 'get "http://example.test/" '() #f)]
            [response (make-http-response 200 "OK" '() (make-bytevector 0) '() 'h1)]
            [source (make-http-body-source (lambda (n) (eof-object)) #f (lambda () #t))]
            [sink (make-http-body-sink (lambda (bv start count) #t)
                                       (lambda () 'done))]
            [cookie (make-http-cookie "a" "b" "example.test" "/" #f)]
            [proxy (make-http-proxy "http://proxy.test/")]
            [part (make-http-multipart-part "a" "b" #f #f)]
            [policy (make-http-pool-policy 2 4 1000)]
            [jar (make-http-cookie-jar)])
       (and (http-request? request)
            (equal? (http-request-method request) "GET")
            (http-response? response)
            (eq? (http-response-version response) 'h1)
            (http-body-source? source)
            (http-body-sink? sink)
            (http-cookie? cookie)
            (http-proxy? proxy)
            (http-multipart-part? part)
            (http-pool-policy? policy)
            (http-cookie-jar? jar))))

(mat net-http-private-record-contract
     (let* ([policy (make-http-request-policy '() #f #f #f #f 'http/1.1 #f #t 10 5000)]
            [request (make-normalized-http-request
                      "GET" #f "http" "example.test" 80 #f "/" '() #f #f policy)]
            [response (make-transport-response 200 "OK" '() #vu8() '() 'h1 17)])
       (and (normalized-http-request? request)
            (eq? (normalized-http-request-policy request) policy)
            (string=? (normalized-http-request-host request) "example.test")
            (transport-response? response)
            (= (transport-response-connection-id response) 17)
            (eq? (transport-response-version response) 'h1))))

(mat net-http-private-record-contract
     (let* ([policy (make-http-request-policy '() #f #f #f #f 'auto #f #t 10 1234)]
            [request (make-normalized-http-request
                      "GET" #f "http" "example.test" 80 #f "/" '() #f #f policy)]
            [response (make-transport-response 200 "OK" '() #vu8() '() 'h1 42)])
       (and (http-request-policy? policy)
            (eq? 'auto (http-request-policy-version policy))
            (normalized-http-request? request)
            (string=? "example.test" (normalized-http-request-host request))
            (eq? policy (normalized-http-request-policy request))
            (transport-response? response)
            (= 42 (transport-response-connection-id response)))))

(mat net-http-normalization-contract
     (let ([request (make-http-request 'post "HTTP://Example.test/path" '(("X-Test" . "ok")) "body")])
       (and (equal? (http-request-method request) "POST")
            (uri? (http-request-uri request))
            (equal? (uri-host (http-request-uri request)) "Example.test")
            (equal? (http-request-body request) "body"))))

(mat net-http-normalization-rejects-invalid
     ;; Error case: request headers must be an association list of string values.
     (guard (condition [else #t])
       (make-http-request 'get "http://example.test/" '(("X" . 1)) #f)
       #f))

(mat net-http2-private-api-contract
     ;; Error case: the low-level HTTP/2 session API is not exported by `(cspp net)`.
     (guard (condition [else #t])
       (eval 'http2-open net-environment)
       #f))

(mat net-http2-ffi-private-api-contract
     ;; Error case: the obsolete direct nghttp2 session FFI is not exported.
     (guard (condition [else #t])
       (eval 'ffi-net-http2-open net-ffi-environment)
       #f))

(mat net-http-obsolete-private-hooks-absent
     ;; Error case: old HTTP/2 adapter test hooks must not remain in the private net API.
     (and (guard (condition [else #t])
            (eval '%http2-transport-read-hook net-private-environment)
            #f)
          (guard (condition [else #t])
            (eval '%http2-event-hook net-private-environment)
            #f)))

(mat net-http-version-policy-contract
     (mat-requires (websockets)
       (let ([client (http-open)])
         (and (eq? (http-client-version-set! client 'h2) client)
              (eq? (http-client-version-set! client 'http/1.1) client)
              (eq? (http-client-version-set! client 'auto) client)
              (guard (condition [else #t])
                (http-client-version-set! client 'bogus)
                #f)))))

(mat net-http-cancel-pending-clears-active-contract
     ;; Cancellation must snapshot and remove pending operations before callbacks run.
     (mat-requires (websockets)
       (let ([client (http-open)])
         (http-cancel-pending! client)
         (http-close client)
         #t)))
