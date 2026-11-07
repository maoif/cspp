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

;; Error case: continuation escapes while wait entry reenables interrupts must close its descriptor.
;; Deliver SIGINT to this thread after opening a real descriptor, before entry has finished.
(eval
  `(let ([opened -1] [escape #f] [inject? #t] [previous (keyboard-interrupt-handler)]
         [thread ((foreign-procedure "pthread_self" () uptr))]
         [send (foreign-procedure "pthread_kill" (uptr int) int)]
         [close (foreign-procedure "close" (int) int)]
         [duplicate (foreign-procedure "dup" (int) int)]
         [descriptor-valid? (foreign-procedure "fcntl" (int int) int)])
     (let ([$wait-open-ffi
             (lambda (pid)
               (if inject?
                   (begin
                     (set! opened (duplicate 1))
                     (when (< opened 0) (error 'wait-entry "cannot open descriptor"))
                     (send thread 2)
                     opened)
                   ((foreign-procedure "cspp_process_wait_open" (int) int) pid)))])
       ,@(filter
           (lambda (definition)
             (not (and (pair? definition) (eq? (car definition) 'define)
                       (eq? (cadr definition) '$wait-open-ffi))))
           definitions)
       (let ([child (process-spawn "/bin/sleep" '("10") '())])
         (dynamic-wind
           void
           (lambda ()
             (keyboard-interrupt-handler (lambda () (escape 'escaped)))
             (do ([index 0 (+ index 1)]) ((= index 20))
               (let ([result
                       (call/1cc
                         (lambda (leave)
                           (set! escape leave)
                           (if (even? index)
                               (process-wait child)
                               (process-wait/timeout child 1000))))])
                 (unless (eq? result 'escaped) (error 'wait-entry "interrupt did not escape"))
                 (let ([leaked? (>= (descriptor-valid? opened 1) 0)])
                   ;; Close on failure so the diagnostic itself does not retain resources.
                   (when leaked? (close opened))
                   (when leaked? (error 'wait-entry "wait entry leaked a descriptor"))))))
           (lambda ()
             (set! inject? #f)
             (keyboard-interrupt-handler previous)
             (process-kill child 9)
             (process-wait child))))))
  scope)
