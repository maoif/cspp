(import (cspp))

;; The supervisor uses Chez spawning and nonblocking libc waitpid so a regression
;; in CSPP capture or GC participation cannot disable its deadline or cleanup.
(define native-wait (foreign-procedure "waitpid" (int u8* int) int))
(define native-kill (foreign-procedure "kill" (int int) int))
(define clock-ms
  (lambda ()
    (let ([now (current-time 'time-monotonic)])
      (+ (* (time-second now) 1000) (quotient (time-nanosecond now) 1000000)))))

(define shell-quote
  (lambda (text)
    (string-append "'" (apply string-append
                        (map (lambda (character)
                               (if (char=? character #\') "'\\''" (string character)))
                             (string->list text))) "'")))

(define test-root
  (path-parent (path-parent (path-build (current-directory) (car (command-line))))))

(define run-case
  (lambda (mode)
    (let* ([fixture (cond [(string=? mode "tilde-lookup-gc") "system-lookup-fixture.so"]
                          [(string=? mode "native-allocation-errors")
                           "system-allocation-fixture.so"]
                          [else #f])]
           [command (format "exec ~a setsid ~a --script=~a ~a"
                           (if fixture
                               (string-append "env LD_PRELOAD="
                                 (shell-quote
                                   (path-build test-root (string-append "tests/" fixture))))
                               "")
                           (shell-quote (path-build test-root "cspp"))
                           (shell-quote (path-build test-root "tests/system-runtime.ss"))
                           (shell-quote mode))])
      (let-values ([(input output errors pid) (open-process-ports command 'block #f)])
        (dynamic-wind
          void
          (lambda ()
            (put-bytevector input (string->utf8 "inherited-input"))
            (close-output-port input)
            ;; The loader stress case can approach 15 seconds under user-mode emulation.
            (let ([status (make-bytevector 4 0)]
                  [deadline (+ (clock-ms) (if (string=? mode "loader-list-race") 30000 15000))])
              (let loop ()
                (let ([result (native-wait pid status 1)])
                  (cond
                    [(= result pid)
                     (let ([stdout (get-bytevector-all output)]
                           [stderr (get-bytevector-all errors)])
                       (unless (and (= 0 (bytevector-u32-native-ref status 0))
                                    (or (eof-object? stdout) (zero? (bytevector-length stdout)))
                                    (or (eof-object? stderr) (zero? (bytevector-length stderr))))
                         (error 'system-runtime-runner "child failed" mode status stdout stderr)))]
                    [(negative? result) (error 'system-runtime-runner "waitpid failed" mode)]
                    [(>= (clock-ms) deadline)
                     (error 'system-runtime-runner "child exceeded deadline" mode)]
                    [else (milisleep 5) (loop)])))))
          (lambda ()
            ;; A timeout case may leave descendants after the immediate child exits.
            (native-kill (- pid) 9)
            (native-kill pid 9)
            (let ([status (make-bytevector 4)]) (native-wait pid status 0))
            (for-each (lambda (port) (unless (port-closed? port) (close-port port)))
                      (list input output errors))))))))

(for-each run-case (cdr (command-line)))
