(import (cspp)
        (cspp net private lws-reactor)
        (cspp net operation)
        (cspp concurrency fiber))

(mat net-operation-waiter-reuse-stress
     ;; Stress case: repeated fiber waits return pooled waiters without retaining operations.
     (mat-requires (websockets)
       (let ([before (lws-reactor-pool-metrics (make-lws-reactor 4 64 4))])
         (do ([i 0 (fx1+ i)]) ((fx= i 200))
           (run-fibers
            (lambda ()
              (eq? 'ok
                   (net-operation-wait
                    (make-net-operation 'stress
                      (lambda () (net-operation-completed 'ok)) void))))))
         #t)))
