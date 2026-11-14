(import (cspp)
        (cspp net))

(mat net-socket-datagram
     (let ([receiver (open-socket 'inet 'datagram)]
           [sender (open-socket 'inet 'datagram)])
       (dynamic-wind
         void
         (lambda ()
           (socket-bind! receiver (make-socket-address 'inet "127.0.0.1" 0))
           (let* ([port (socket-address-port (socket-local-address receiver))]
                  [target (make-socket-address 'inet "127.0.0.1" port)]
                  [payload (string->utf8 "datagram")])
             (and (= (bytevector-length payload)
                     (socket-send-to sender payload target))
                  (let-values ([(received source) (socket-recv-from receiver 128)])
                    (and (equal? payload received)
                         (eq? 'inet (socket-address-family source))
                         (string=? "127.0.0.1" (socket-address-host source)))))))
         (lambda ()
           (close-socket sender)
           (close-socket receiver)))))

(mat net-socket-datagram-into-buffer
     (let ([receiver (open-socket 'inet 'datagram)]
           [sender (open-socket 'inet 'datagram)])
       (dynamic-wind
         void
         (lambda ()
           (socket-bind! receiver (make-socket-address 'inet "127.0.0.1" 0))
           (let* ([port (socket-address-port (socket-local-address receiver))]
                  [target (make-socket-address 'inet "127.0.0.1" port)]
                  [payload (string->utf8 "into-buffer")]
                  [buffer (make-bytevector 32 0)])
             (socket-send-to sender payload target)
             (let-values ([(count source) (socket-recv-from! receiver buffer 3 24)])
               (and (= count (bytevector-length payload))
                    (equal? payload
                            (let ([copy (make-bytevector count 0)])
                              (bytevector-copy! buffer 3 copy 0 count)
                              copy))
                    (eq? 'inet (socket-address-family source))))))
         (lambda ()
           (close-socket sender)
           (close-socket receiver)))))

(mat net-socket-datagram-nonblocking
     (let ([receiver (open-socket 'inet 'datagram)])
       (dynamic-wind
         void
         (lambda ()
           (socket-bind! receiver (make-socket-address 'inet "127.0.0.1" 0))
           (let ([answer (socket-recv-from!/nonblocking receiver (make-bytevector 8 0))])
             (and (net-would-block? answer)
                  (equal? '(read) (net-would-block-events answer)))))
         (lambda () (close-socket receiver)))))

(mat net-socket-options
     (let ([datagram (open-socket 'inet 'datagram)]
           [stream (open-socket 'inet 'stream)])
       (dynamic-wind
         void
         (lambda ()
           (socket-set-option! datagram 'broadcast #t)
           (socket-set-option! datagram 'multicast-ttl 7)
           (socket-set-option! stream 'keepalive #t)
           (socket-set-option! stream 'keepalive-idle 30)
           (socket-set-option! stream 'keepalive-interval 5)
           (socket-set-option! stream 'keepalive-count 4)
           (and (socket-get-option datagram 'broadcast)
                (= 7 (socket-get-option datagram 'multicast-ttl))
                (socket-get-option stream 'keepalive)
                (= 30 (socket-get-option stream 'keepalive-idle))
                (= 5 (socket-get-option stream 'keepalive-interval))
                (= 4 (socket-get-option stream 'keepalive-count))))
         (lambda ()
           (close-socket stream)
           (close-socket datagram)))))

(mat net-socket-option-validation
     ;; Unknown socket options are rejected before entering the native boundary.
     (guard (condition [else #t])
       (let ([sock (open-socket 'inet 'stream)])
         (dynamic-wind void
           (lambda () (socket-set-option! sock 'unknown-option #t) #f)
           (lambda () (close-socket sock)))))

     ;; Boolean socket options reject numeric values.
     (guard (condition [else #t])
       (let ([sock (open-socket 'inet 'stream)])
         (dynamic-wind void
           (lambda () (socket-set-option! sock 'keepalive 1) #f)
           (lambda () (close-socket sock))))))
