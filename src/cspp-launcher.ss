#!@SCHEME_SCRIPT@ --script
(putenv "LIBCSPP" "@LIBCSPP@")
(load "@CSPP_LIB@")
(import (cspp))

;; Keep launcher dependencies local so applications and the REPL start with (cspp).
(module ()
  (import (cspp io) (cspp system) (cspp rich) (cspp cli))

  (define load-file*
    (lambda (file*)
      (for-each load file*)))

  (define run-script
    (lambda (file arg*)
      (apply (scheme-script) file arg*)))

  (define stdout-terminal?
    (lambda ()
      (= 0 (system "test -t 1 2>/dev/null"))))

  (define cspp-version-string
    (lambda ()
      (let ([version (library-version '(cspp))])
        (format "~a.~a.~a@~a-~a-~a"
                (car version)
                (cadr version)
                (caddr version)
                (cadddr version)
                (car (cddddr version))
                (cadr (cddddr version))))))

  (define make-greeting-logo
    (lambda (content-width)
      (let ([glyph*
             '((" ████" "█    " "█    " "█    " " ████")
               ("█████" "█    " "█████" "    █" "█████")
               ("████ " "█   █" "████ " "█    " "█    ")
               ("████ " "█   █" "████ " "█    " "█    "))]
            [style* (map (lambda (color) (rich-style 'bold color))
                         '(#xb04d97 #x8662bd #x427bb3 #x0c8595))]
            [shadow-style (rich-style #x808080)])
        (make-rich-layout
         'column
         (map (lambda (row-index)
                (let ([row (make-rich-text)])
                  (for-each
                   (lambda (glyph style)
                     (let* ([width (string-length (car glyph))]
                            [front (and (fx< row-index 5) (list-ref glyph row-index))]
                            [back (and (fx> row-index 0)
                                       (list-ref glyph (fx1- row-index)))])
                       ;; Offset the shadow one cell down and right behind the solid lettering.
                       (do ([column 0 (fx1+ column)])
                           ((fx> column width))
                         (cond
                          [(and front (fx< column width)
                                (char=? (string-ref front column) #\█))
                           (rich-text-append! row "█" style)]
                          [(and back (fx> column 0)
                                (char=? (string-ref back (fx1- column)) #\█))
                           (rich-text-append! row "░" shadow-style)]
                          [else (rich-text-append! row " ")]))
                       (rich-text-append! row " ")))
                   glyph* style*)
                  (make-rich-align row content-width 'center)))
              '(0 1 2 3 4 5))))))

  (define print-greeting
    (lambda ()
      (let* ([content-width 48]
             [panel-width (fx+ content-width 4)]
             [console
              (rich-console :output-port (current-output-port)
                            :width panel-width
                            :color-system (if (stdout-terminal?) 'standard 'none))]
             [logo (make-greeting-logo content-width)]
             [label-style (rich-style 'bold 'default)]
             [author (rich-text "Author   " label-style)]
             [version (rich-text "Version  " label-style)]
             [platform (rich-text "Platform " label-style)]
             [github (rich-text "Github   " label-style)])
        (rich-text-append! author "Maoif" (rich-style 'default))
        (rich-text-append! version (cspp-version-string) (rich-style 'default))
        (rich-text-append! platform
                           (format "~a / ~a"
                                   (case (system-platform)
                                     [(linux) "Linux"]
                                     [(darwin) "macOS"]
                                     [(windows) "Windows"]
                                     [else "Unknown"])
                                   (let ([arch (cpu-arch)])
                                     (if (string? arch) arch "unknown")))
                           (rich-style 'default))
        (rich-text-append! github
                           "https://github.com/maoif/cspp"
                           (rich-style 'underline 'default))
        (rich-print console
                    (rich-style #x777777)
                    (rich-panel
                     :body (make-rich-layout
                            'column
                            (list "" logo ""
                                  (make-rich-align
                                   (rich-text "Chez Scheme Programming Platform"
                                              (rich-style 'italic 'default))
                                   content-width 'center)
                                  ""
                                  (make-rich-padding
                                   (make-rich-layout 'column (list author version platform github))
                                   0 0 0 5)
                                  ""))
                     :box 'rounded :width panel-width)
                    (reset-style)
                    "\n\n"))))

  (define configure-repl!
    (lambda (quiet?)
      (when quiet?
        (suppress-greeting #t)
        (waiter-prompt-string ""))))

  (define run-code
    (lambda (code arg*)
      (command-line-arguments arg*)
      (call-with-port (open-string-input-port code)
        (lambda (port)
          (let loop ()
            (let ([form (read port)])
              (unless (eof-object? form)
                (eval form (interaction-environment))
                (loop))))))))

  (define main
    (lambda ()
      (let ([code? #f])
        (run-cli-command!
         (cli-command cspp
           :overview "CSPP REPL and script launcher"
           :version (cspp-version-string)
           :about
           (string-append
            "Modes:\n"
            "  cspp [-q] [FILE ...]              Load FILEs in order, then start the REPL.\n"
            "  cspp [-q] -s=FILE [-- ARG ...]    Run a script and exit.\n"
            "  cspp [-q] -c=CODE [-- ARG ...]    Evaluate Scheme forms and exit.\n"
            "\n"
            "Use -- to pass remaining arguments literally through command-line-arguments.\n"
            "CODE prints only explicit output.\n"
            "Use -- before REPL input filenames that start with '-'.\n"
            "Use -h or --help to show this help and exit.\n"
            "\nExamples:\n"
            "  cspp -q init.ss\n"
            "  cspp --script=app.ss arg1\n"
            "  cspp -c='(display (+ 1 2))'")
           :exec
           (lambda (ref)
             (let ([arg* (ref 'args)])
               (cond
                [(ref 'version) (println "~a" (cspp-version-string))]
                [(ref 'script) (run-script (ref 'script) arg*)]
                [code? (run-code (ref 'code) arg*)]
                [else
                 (command-line-arguments arg*)
                 (load-file* arg*)
                 (configure-repl! (ref 'quiet))
                 (unless (ref 'quiet)
                   (print-greeting))
                 (new-cafe)]))
             (void))
           :options
           [quiet :short #\q :no-long
            :help "Suppress the startup greeting and REPL prompt."]
           [script :short #\s :value-parser string :value-name "FILE" :number ?
            :conflicts (code version)
            :help "Run FILE with the remaining arguments and exit."]
           [code :short #\c :no-long :value-parser string :value-name "CODE" :number ?
            ;; Keep an explicitly empty -c= distinct from an absent code option.
            :value-number ? :default ""
            :callback (lambda (command option value) (set! code? #t))
            :conflicts (script version)
            :help "Evaluate CODE with the remaining arguments and exit."]
           [version :short #\v
            :conflicts (script code)
            :help "Show the CSPP version and exit."]
           [args :positional :sink :number * :default '() :value-name "ARG"
            :help "Files to load, or arguments passed to the script or code."])
         (command-line-arguments)))
      ;; Script and code output can remain buffered when the top-level loader returns.
      (flush-output-port (current-output-port))
      (flush-output-port (current-error-port))))

  (main)
)
