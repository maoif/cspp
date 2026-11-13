(import (cspp) (cspp net))

(mat net-address-service-and-selection
     (let ([address* (resolve-addresses "localhost" "http" #f 'stream)])
       (and (pair? address*)
            (andmap (lambda (address) (= 80 (socket-address-port address))) address*)))

     (let* ([address* (list (make-socket-address 'inet "127.0.0.1" 80)
                            (make-socket-address 'inet6 "::1" 80)
                            (make-socket-address 'inet "127.0.0.2" 80))]
            [selected (address-select
                       address*
                       (lambda (address) (eq? 'inet (socket-address-family address))))]
            [interleaved (address-interleave address*)])
       (and (= 2 (length selected))
            (equal? '(inet6 inet inet)
                    (map socket-address-family interleaved)))))

(mat net-address-staggered-connect
     (let ([listener (open-socket 'inet 'stream)]
           [reserved (open-socket 'inet 'stream)])
       (dynamic-wind
         void
         (lambda ()
           (socket-set-option! listener 'reuse-address #t)
           (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
           (socket-listen! listener 4)
           (socket-bind! reserved (make-socket-address 'inet "127.0.0.1" 0))
           (let ([good-port (socket-address-port (socket-local-address listener))]
                 [bad-port (socket-address-port (socket-local-address reserved))])
             (close-socket reserved)
             (let* ([operation
                     (connect-addresses/nonblocking
                      (list (make-socket-address 'inet "127.0.0.1" bad-port)
                            (make-socket-address 'inet "127.0.0.1" good-port))
                      2000)]
                    [client (net-operation-wait operation)])
               (let-values ([(server peer) (socket-accept listener)])
                 (close-socket server)
                 (let ([connected? (= good-port
                                      (socket-address-port
                                       (socket-peer-address client)))])
                   (close-socket client)
                   connected?)))))
         (lambda ()
           (unless (socket-closed? reserved) (close-socket reserved))
           (close-socket listener)))))
