(import (cspp) (cspp system native))

;; Exercise the shared wrapper with real native process state. Only the timer scheduling and
;; disposal counter are injected; the native prepare/step/result/dispose operations are unchanged.
(define create (foreign-procedure "cspp_process_operation_create" () uptr))
(define prepare
  (foreign-procedure "cspp_process_prepare_capture"
    (uptr ptr ptr utf-8 ptr int int int int int int) void))
(define step (foreign-procedure __collect_safe "cspp_process_operation_step" (uptr) int))
(define result (foreign-procedure "cspp_process_operation_result" (uptr) ptr))
(define dispose (foreign-procedure __collect_safe "cspp_process_operation_dispose" (uptr) void))

(define previous (timer-interrupt-handler))
(define sentinel (list 'native-cleanup-interrupt))
(define active? #f)
(define escape #f)
(define interrupt-kind 'exception)
(define interruptions 0)

(dynamic-wind #t
  (lambda ()
    (timer-interrupt-handler
      (lambda ()
        (when active?
          (set! interruptions (+ interruptions 1))
          (if (eq? interrupt-kind 'exception) (raise sentinel) (escape sentinel))))))
  (lambda ()
    (for-each
      (lambda (kind)
        (set! interrupt-kind kind)
        (for-each
          (lambda (program)
            (do ([ticks 1 (+ ticks 1)]) ((= ticks 65))
              (let ([handle 0] [disposals 0])
                ;; Error case: handlers escaping final result handling must never skip disposal,
                ;; including when the native result itself reports a command launch error.
                (guard (failure
                         [(eq? failure sentinel) (void)]
                         [(and (system-error? failure) (not (string=? program "/bin/true")))
                          (unless (= 2 (system-error-code failure)) (raise failure))])
                  (call/1cc
                    (lambda (leave)
                      (set! escape leave)
                      (dynamic-wind #t
                        (lambda () (set! active? #t))
                        (lambda ()
                          ($call-native-operation 'native-cleanup
                            (lambda () (set! handle (create)) handle)
                            (lambda (address)
                              (prepare address (list program) #f "" #f 0 0 0 0 0 -1))
                            step
                            (lambda (address)
                              (let ([value (result address)]) (set-timer ticks) value))
                            (lambda (address)
                              (set! disposals (+ disposals 1))
                              (unless (= disposals 1)
                                (error 'native-cleanup "native state disposed more than once"))
                              (dispose address))
                            (lambda (value) value)))
                        (lambda () (set! active? #f) (set-timer 0))))))
                (when (and (not (= handle 0)) (= disposals 0))
                  ;; Release the diagnostic's state before reporting a failing implementation.
                  (dispose handle)
                  (error 'native-cleanup "native state was not disposed" kind program ticks)))))
          '("/bin/true" "/__cspp_missing_native_cleanup_command__")))
      '(exception continuation))
    (unless (> interruptions 0) (error 'native-cleanup "interrupt sweep did not run a handler")))
  (lambda ()
    (set! active? #f)
    (set-timer 0)
    (timer-interrupt-handler previous)))
