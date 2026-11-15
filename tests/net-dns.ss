(import (cspp)
        (cspp net))

(mat net-dns-options
     (let ([options (make-dns-options 'unspecified 'address 2000 #t)])
       (and (dns-options? options)
            (eq? 'unspecified (dns-options-family options))
            (eq? 'address (dns-options-type options))
            (= 2000 (dns-options-timeout-ms options))
            (dns-options-canonical-name? options)))

     (mat-requires (cares)
       (let ([operation (dns-resolve/nonblocking "localhost" default-dns-options)])
         (and (net-operation? operation)
              (let ([result (net-operation-wait operation)])
                (and (dns-result? result)
                     (equal? "localhost" (dns-result-query-name result))
                     (eq? 'address (dns-result-record-type result))
                     (eq? 'success (dns-result-status result))
                     (pair? (dns-result-addresses result))))))))

(mat net-dns-family-selection
     (mat-requires (cares)
       (let ([result (dns-resolve/ipv4 "localhost")])
         (and (pair? (dns-result-addresses result))
              (andmap (lambda (address)
                      (eq? 'inet (socket-address-family address)))
                      (dns-result-addresses result))))))

(mat net-dns-cancellation
     ;; Cancelling a resolver operation must close its c-ares channel exactly once.
     (mat-requires (cares)
       (let ([operation
              (dns-resolve/nonblocking
               "cancelled-query.invalid"
               (make-dns-options 'unspecified 'address 5000 #t))])
         (net-operation-cancel! operation)
         (and (eq? 'cancelled (net-operation-state operation))
              (eq? operation (net-operation-cancel! operation))))))
