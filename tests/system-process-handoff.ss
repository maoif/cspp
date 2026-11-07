(import (cspp))

;; Run the real expert definitions and native operations, injecting only the exact escape point.
;; Real children and ports verify cleanup without depending on timer scheduling or stack layout.
(define source
  (if (file-exists? "../src/cspp/system/process/expert.ss")
      "../src/cspp/system/process/expert.ss"
      "src/cspp/system/process/expert.ss"))

(define scope
  (copy-environment
    (environment '(cspp chez) '(cspp utils) '(cspp system common) '(cspp system private)
                 '(cspp system process status) '(cspp system process private)
                 '(cspp system native) '(only (cspp system time) milisleep))
    #t))

(define definitions (cddddr (call-with-input-file source read)))

(eval
  `(let ([native-operation $call-process-operation] [escape #f] [resume #f]
         [transferred '()] [replaying? #f] [phase 'consume])
     (let ([$call-process-operation
             (lambda (who prepare consume)
               (let ([value
                       (native-operation who prepare
                         (lambda (result)
                           (let ([value (consume result)])
                             (set! transferred (if (list? value) value (list value)))
                             (when (eq? phase 'consume)
                               ;; call/cc retains the abandoned frame to check replay rejection.
                               (call/cc
                                 (lambda (continuation)
                                   (set! resume continuation)
                                   (escape 'abandoned))))
                             value)))])
                 (when (eq? phase 'return)
                   ;; call/cc also checks replay after the native scope has already returned.
                   (call/cc
                     (lambda (continuation)
                       (set! resume continuation)
                       (escape 'abandoned))))
                 value))])
       ,@definitions
       (let ([fds (lambda () (length (directory-list "/proc/self/fd")))]
             [waitpid (foreign-procedure "waitpid" (int u8* int) int)]
             [kill (foreign-procedure "kill" (int int) int)])
         (for-each
           (lambda (handoff-phase)
             (set! phase handoff-phase)
             (for-each
               (lambda (pipeline?)
                 (let ([baseline (fds)] [failure #f] [rejected? #f])
                   (set! transferred '())
                   (set! replaying? #f)
                   ;; Error case: abandoning an unpublished live result must close and reap it.
                   (let ([result
                           (guard (condition [else condition])
                             (call/1cc
                               (lambda (leave)
                                 (set! escape leave)
                                 (if pipeline?
                                     (process-spawn-pipeline
                                       '(("/bin/sleep" "10") ("/bin/sleep" "10")) '())
                                     (process-spawn "/bin/sleep" '("10")
                                       '((stdin . pipe) (stdout . pipe) (stderr . pipe)))))))])
                     (if replaying?
                         (set! rejected? (error? result))
                         (begin
                           (unless (eq? result 'abandoned)
                             (set! failure (list 'escape-failed result)))
                           (unless
                             (and (= baseline (fds))
                                  (< (waitpid -1 (make-bytevector 4 0) 1) 0))
                             (set! failure 'unreturned-resources))
                           ;; Always clean up a failing implementation before reporting its error.
                           (for-each
                             (lambda (child)
                               (process-close-ports! child)
                               (kill (process-pid child) 9)
                               (waitpid (process-pid child) (make-bytevector 4 0) 0))
                             transferred)
                           (set! replaying? #t)
                           (resume))))
                   (when failure
                     (error 'system-process-handoff "handoff leaked resources" phase pipeline? failure))
                   ;; Error case: replay must not expose process objects whose children were cleaned up.
                   (unless rejected?
                     (error 'system-process-handoff "abandoned handoff accepted replay"))))
               '(#f #t)))
           '(consume return)))))
  scope)
