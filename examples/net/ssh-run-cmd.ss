(import (cspp)
        (cspp net)
        (cspp cli)
        (cspp logging))
(load "examples/common.ss")
(load "examples/net/ssh-example-common.ss")

(define has-command-words? (and (member "--" (command-line-arguments)) #t))

(define command
  (ssh-example-command
   'ssh-run-cmd "Run --command=text [-- remaining command words] over SSH."
   (append ssh-example-options '((command . "Remote shell command text; required unless command words follow --. Prints stdout/stderr and returns the remote exit status.")))
   (lambda (ref)
     (let* ([words (if has-command-words? (ref 'command-words) '())]
            [text (join-command-arguments
                   (append (if (ref 'command) (list (ref 'command)) '()) words))])
       (when (string=? text "") (errorf 'ssh-run-cmd "provide --command or command words"))
       (exit
        (call-with-example-ssh
         'ssh-run-cmd ref
         (lambda (session log)
           ;; Execute command text in a fresh SSH channel.
           (call-with-ssh-channel
            session
            (lambda (channel)
              ;; Request execution of the assembled remote shell command.
              (ssh-exec channel text)
              ;; Commands receive stdin EOF so programs waiting for input can finish.
              (ssh-send-eof! channel)
              (ssh-example-drain channel void))))))))))

;; The CLI sink is needed only when positional words follow the named options.
(let ([args (command-line-arguments)])
  (when (member "--" args)
    (let ([words (example-make-option 'command-words
                                      "Remaining words joined as remote shell command text after --.")])
      (option-positional?-set! words #t)
      (option-sink?-set! words #t)
      (command-options-add! command words)))
  (run-cli-command! command (ssh-example-arguments args)))
