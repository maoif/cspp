(import (cspp))

(define check
  (lambda (value)
    (unless value (error 'system-runtime "regression failed" (cdr (command-line))))))

(define rejects?
  (lambda (thunk)
    (guard (condition [else (or (error? condition) (violation? condition))])
      (thunk)
      #f)))

(define run-threads
  (lambda (thunks)
    (let ([mutex (make-mutex)] [ready (make-condition)]
          [remaining (length thunks)] [failures '()])
      (for-each
        (lambda (thunk)
          (fork-thread
            (lambda ()
              (let ([failure (guard (condition [else condition]) (thunk) #f)])
                (with-mutex mutex
                  (when failure (set! failures (cons failure failures)))
                  (set! remaining (- remaining 1))
                  (condition-signal ready))))))
        thunks)
      (with-mutex mutex
        (let loop ()
          (unless (zero? remaining) (condition-wait ready mutex) (loop))))
      (unless (null? failures) (raise (car failures))))))

   (let* ([first (current-user)] [second (user-ref/default "daemon" first)])
     ;; Error case: lookups in other threads must not overwrite any record fields.
     (run-threads
       (map (lambda (expected)
              (lambda ()
                (do ([index 0 (+ index 1)]) ((= index 30000))
                  (let ([actual (user-ref (unix-user-name expected))])
                    (check (and (string=? (unix-user-name expected) (unix-user-name actual))
                                (= (unix-user-uid expected) (unix-user-uid actual))
                                (= (unix-user-gid expected) (unix-user-gid actual))))))))
            (list first second))))
