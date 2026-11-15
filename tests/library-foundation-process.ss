(import (chezscheme))

;; Use source libraries in a fresh process, without preloading the CSPP bundle.
(library-directories '("../src"))
(compile-imported-libraries #f)
(putenv "LIBCSPP" (path-build (path-parent (current-directory)) "libcspp.so"))

(define check
  (lambda (value message)
    (unless value (error 'library-foundation message))))

(define mode (car (command-line-arguments)))

(when (string=? mode "chez-first")
  (check (not (foreign-entry? "cspp_environment_get")) "native runtime was already loaded")
  (let ([foundation (environment '(cspp chez))])
    ;; Error case: the raw sleeper and numeric wrappers are absent from the thin gateway.
    (for-each
      (lambda (name)
        (check (not (memq name (library-exports '(cspp chez))))
               "utility or raw sleep binding leaked from the Chez gateway"))
      '($sleep sleep milisleep nanosleep getenv putenv))
    (check (= 42 (eval '(+ 20 22) foundation)) "Chez primitives are unavailable")
    (check (not (foreign-entry? "cspp_environment_get")) "Chez gateway loaded native code")))

(when (string=? mode "native-first")
  (let ([native (environment '(cspp system private user-credentials))])
    (check (integer? (eval '(getuid) native)) "native library failed to initialize independently")))

(let ([utilities (environment '(cspp chez) '(cspp utils))])
  (eval
    '(begin
       (putenv "CSPP_FOUNDATION_PROCESS_TEST" "ready")
       (unless (string=? "ready" (getenv "CSPP_FOUNDATION_PROCESS_TEST"))
         (error 'library-foundation "synchronized environment access failed"))
       (sleep 0)
       (milisleep 0)
       (nanosleep 0)
       ;; Error case: the public sleeper accepts numeric durations, not Chez time objects.
       (unless (guard (condition [else (and (who-condition? condition)
                                           (eq? 'sleep (condition-who condition)))])
                 (sleep (make-time 'time-duration 0 0))
                 #f)
         (error 'library-foundation "public sleep accepted a Chez time object"))
       ;; Error case: malformed environment names must fail before entering native code.
       (unless (guard (condition [else (and (who-condition? condition)
                                           (eq? 'pcheck (condition-who condition)))])
                 (putenv "invalid=name" "value")
                 #f)
         (error 'library-foundation "invalid environment name was accepted")))
    utilities))

(check (not (top-level-bound? '$sleep (environment '(cspp)))) "raw sleep leaked from CSPP")
