(define external-tool-available?
  (lambda (name)
    (let ([result
           (process-capture "/bin/sh" "-c" "command -v -- \"$1\" >/dev/null" "sh" (begin name)
             :stdout capture
             :stderr capture
             :timeout 10000)])
      (process-exit-success? (process-result-status result)))))

(define successful-process-output
  (lambda (result)
    (and (process-exit-success? (process-result-status result))
         (string=? "" (process-result-stderr result))
         (process-result-stdout result))))
