(import (cspp)
        (cspp net)
        (cspp net http))

;; The Python fixture supplies URI, output, and a first-chunk acknowledgement path.
(let* ([arguments (command-line-arguments)]
       [client (http-open)]
       [output (open-file-output-port (cadr arguments) (file-options)
                                     (buffer-mode block) #f)]
       [received 0] [largest 0] [chunks 0])
  (dynamic-wind
    void
    (lambda ()
      (http-set-timeout! client 5000)
      (let ([response
             (net-operation-wait
              (http-send/nonblocking
               client (make-http-request 'get (car arguments))
               (make-http-body-sink
                (lambda (bytes start count)
                  (put-bytevector output bytes start count)
                  (set! received (+ received count))
                  (set! largest (max largest count))
                  (when (zero? chunks)
                    (call-with-output-file (caddr arguments) (lambda (port) (display "ready" port))))
                  (set! chunks (fx1+ chunks))))))])
        (unless (= 200 (http-response-status response)) (error 'streaming "unexpected status"))
        (write (list received largest chunks))
        (newline)))
    (lambda () (close-port output) (http-close client))))
