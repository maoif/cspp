(import (chezscheme))

;; Evaluate the actual private definitions with injected port failures; no test API is exported.
(define source
  (if (file-exists? "../src/cspp/system/process/expert.ss")
      "../src/cspp/system/process/expert.ss"
      "src/cspp/system/process/expert.ss"))

(define definition
  (lambda (name)
    (let ([library-form (call-with-input-file source read)])
      (or (find (lambda (form)
                  (and (pair? form) (eq? (car form) 'define) (eq? (cadr form) name)))
                (cddr library-form))
          (error 'definition "missing private procedure" name)))))

(define scope (copy-environment (environment '(chezscheme)) #t))

;; Error case: a failed close must clear buffered output and retry before returning the first error.
(define close-result
  (eval
    `(let ([attempts 0] [cleared? #f] [closed? #f])
       (let ([port-closed? (lambda (port) closed?)]
             [output-port? (lambda (port) #t)]
             [clear-output-port (lambda (port) (set! cleared? #t))]
             [close-port
               (lambda (port)
                 (set! attempts (+ attempts 1))
                 (if (= attempts 1)
                     (error 'injected "first close failed")
                     (set! closed? #t)))])
         ,(definition '$close-port/cleanup)
         (let ([failure ($close-port/cleanup 'fake-port)])
           (list (condition? failure) attempts cleared? closed?))))
    scope))
(unless (equal? close-result '(#t 2 #t #t))
  (error 'cleanup-probe "close retry failed" close-result))

(define pipe-failure
  (lambda (phase)
    (eval
      `(let ([closed-fds '()] [closed-ports '()])
         (let ([$close-fd-ffi (lambda (fd) (set! closed-fds (cons fd closed-fds)) 0)])
           (let ([ffi-result-ref (lambda (value) value)]
                 [$make-pipe-ffi (lambda () (vector 10 11))]
                 [open-fd-input-port
                   (lambda (fd mode transcoder)
                     (if (eq? ',phase 'input)
                         (error 'injected "input port failed")
                         'input))]
                 [open-fd-output-port
                   (lambda (fd mode transcoder)
                     (error 'injected "output port failed"))]
                 [$close-port/cleanup
                   (lambda (port)
                     (when port (set! closed-ports (cons port closed-ports))))])
             ,(definition '$call-with-process-handoff)
             ,(definition 'make-pipe)
             (guard (failure [else (list closed-ports closed-fds)])
               (make-pipe)
               (error 'pipe-failure "injected failure did not occur")))))
      scope)))

;; Error case: failed input wrapping must close both raw descriptors.
(unless (equal? (pipe-failure 'input) '(() (11 10)))
  (error 'cleanup-probe "input failure leaked descriptors"))

;; Error case: failed output wrapping must close the input port and remaining raw descriptor.
(unless (equal? (pipe-failure 'output) '((input) (11)))
  (error 'cleanup-probe "output failure leaked resources"))
