(import (cspp) (cspp cli))

(define capture-output
  (lambda (thunk)
    (let ([v #f])
      (let ([out (call-with-string-output-port
                   (lambda (port)
                     (parameterize ([current-output-port port])
                       (set! v (thunk)))))])
        (cons v out)))))

(define output-contains?
  (lambda (str needle)
    (if (string-search str needle) #t #f)))

(define silent
  (lambda (thunk)
    (let ([v #f])
      (let ([out (call-with-string-output-port
                   (lambda (port)
                     (parameterize ([current-output-port port])
                       (set! v (thunk)))))])
        (and (string=? "" out) v)))))



;;===----------------------------------------------------------------------===
;; procedural interface
;;===----------------------------------------------------------------------===


(mat cli-procedural-p

     (begin
       (define cmd (make-command 'prog))
       (define opt-verbose (make-option 'verbose))
       (option-short-set! opt-verbose "v")
       (option-value-number-set! opt-verbose 0)
       (define opt-out (make-option 'out))
       (option-short-set! opt-out "o")
       (option-default-set! opt-out "a.out")
       (option-value-parser-set! opt-out parser-string)
       (define opt-input (make-option 'input))
       (option-positional?-set! opt-input #t)
       (command-options-add! cmd opt-verbose opt-out opt-input)
       (command-exec-set! cmd
         (lambda (ref)
           (list (ref 'verbose) (ref 'out) (ref 'input))))
       #t)

     (equal? (silent (lambda () (run-cli-command! cmd '("main.ss"))))
             '(#f "a.out" "main.ss"))
     (equal? (silent (lambda () (run-cli-command! cmd '("-v" "--out=prog" "main.ss"))))
             '(#t "prog" "main.ss"))
     (error? (run-cli-command! cmd '("--out" "prog" "main.ss")))
     (error? (run-cli-command! cmd '("--verbose=true" "main.ss")))
     (error? (run-cli-command! cmd '())))


(mat cli-values-p

     (begin
       (define cmd (make-command 'prog))
       (define opt-include (make-option 'include))
       (option-short-set! opt-include "I")
       (option-value-parser-set! opt-include parser-string)
       (option-number-set! opt-include '*)
       (option-value-number-set! opt-include '+)
       (define opt-mode (make-option 'mode))
       (option-value-parser-set! opt-mode parser-string)
       (option-number-set! opt-mode '?)
       (option-default-set! opt-mode "debug")
       (define opt-file (make-option 'file))
       (option-positional?-set! opt-file #t)
       (define opt-args (make-option 'args))
       (option-positional?-set! opt-args #t)
       (option-sink?-set! opt-args #t)
       (command-options-add! cmd opt-include opt-mode opt-file opt-args)
       (command-exec-set! cmd
         (lambda (ref)
           (list (ref 'include) (ref 'mode) (ref 'file) (ref 'args))))
       #t)

     (equal? (silent (lambda () (run-cli-command! cmd '("-I=a,b" "--include=c" "main.ss" "--" "-x" "y"))))
             '(("a" "b" "c") "debug" "main.ss" ("-x" "y")))
     (equal? (silent (lambda () (run-cli-command! cmd '("--mode=release" "main.ss" "x"))))
             '(#f "release" "main.ss" ("x")))
     (error? (run-cli-command! cmd '("--mode" "release" "main.ss")))
     (error? (run-cli-command! cmd '("-I=" "main.ss"))))


(mat cli-sink-p

     (begin
       (define optional
         (cli-command prog
           :exec (lambda (ref) (ref 'args))
           :options [args :positional :sink :number * :default '()]))
       (define required
         (cli-command prog
           :exec (lambda (ref) (ref 'args))
           :options [args :positional :sink]))
       (define fixed-and-sink
         (cli-command prog
           :exec (lambda (ref) (list (ref 'file) (ref 'args)))
           :options [file :positional] [args :positional :sink :number * :default '()]))
       #t)

     (equal? (run-cli-command! optional '()) '())

     (equal? (run-cli-command! optional '("--")) '())

     (equal? (run-cli-command! optional '("--" "--")) '("--"))

     (equal? (run-cli-command! optional '("--" "a" "--" "b")) '("a" "--" "b"))

     (equal? (run-cli-command! optional '("--" "-x" "" "a,b" "--help"))
             '("-x" "" "a,b" "--help"))

     (equal? (run-cli-command! optional '("a" "--" "-x" "--")) '("a" "-x" "--"))

     (equal? (run-cli-command! fixed-and-sink '("main.ss")) '("main.ss" ()))

     (equal? (run-cli-command! fixed-and-sink '("main.ss" "--")) '("main.ss" ()))

     (equal? (run-cli-command! fixed-and-sink '("main.ss" "--" "--"))
             '("main.ss" ("--")))

     ;; Required sinks must reject an invocation with no arguments.
     (error? (run-cli-command! required '()))

     ;; An explicit separator does not satisfy a required sink.
     (error? (run-cli-command! required '("--")))

     ;; An optional sink does not make the preceding fixed argument optional.
     (error? (run-cli-command! fixed-and-sink '("--"))))


(mat cli-parsers-p

     (begin
       (define color-parser (parser-enum '[red :value 1] '[green :value 2] '[blue :value 3]))
       (define cmd (make-command 'prog))
       (define opt-n (make-option 'n))
       (option-value-parser-set! opt-n parser-natural)
       (define opt-i (make-option 'i))
       (option-value-parser-set! opt-i parser-integer)
       (define opt-fx (make-option 'fx))
       (option-value-parser-set! opt-fx parser-fixnum)
       (define opt-fl (make-option 'fl))
       (option-short-set! opt-fl "l")
       (option-value-parser-set! opt-fl parser-flonum)
       (define opt-color (make-option 'color))
       (option-value-parser-set! opt-color color-parser)
       (command-options-add! cmd opt-n opt-i opt-fx opt-fl opt-color)
       (command-exec-set! cmd
         (lambda (ref)
           (list (ref 'n) (ref 'i) (ref 'fx) (ref 'fl) (ref 'color))))
       #t)

     (equal? (silent (lambda () (run-cli-command! cmd '("--n=7" "--i=-2" "--fx=3" "--fl=1.5" "--color=green"))))
             '(7 -2 3 1.5 2))
     (error? (run-cli-command! cmd '("--n=-1" "--i=-2" "--fx=3" "--fl=1.5" "--color=green")))
     (error? (run-cli-command! cmd '("--n=7" "--i=x" "--fx=3" "--fl=1.5" "--color=green")))
     (error? (run-cli-command! cmd '("--n=7" "--i=-2" "--fx=3.2" "--fl=1.5" "--color=green")))
     (error? (run-cli-command! cmd '("--n=7" "--i=-2" "--fx=3" "--fl=x" "--color=green")))
     (error? (run-cli-command! cmd '("--n=7" "--i=-2" "--fx=3" "--fl=1.5" "--color=yellow"))))


(mat cli-relations-p

     (begin
       (define cmd (make-command 'prog))
       (define opt-a (make-option 'a))
       (define opt-b (make-option 'b))
       (define opt-c (make-option 'c))
       (option-value-number-set! opt-a 0)
       (option-value-number-set! opt-b 0)
       (option-value-number-set! opt-c 0)
       (option-requires-set! opt-a '(b))
       (option-conflicts-set! opt-b '(c))
       (option-overrides-set! opt-c '(a b))
       (command-options-add! cmd opt-a opt-b opt-c)
       (command-exec-set! cmd (lambda (ref) (list (ref 'a) (ref 'b) (ref 'c))))
       #t)

     (equal? (silent (lambda () (run-cli-command! cmd '("-a" "-b"))))
             '(#t #t #f))
     (equal? (silent (lambda () (run-cli-command! cmd '("-a" "-b" "-c"))))
             '(#f #f #t))
     (error? (run-cli-command! cmd '("-a")))
     (equal? (silent (lambda () (run-cli-command! cmd '("-b" "-c"))))
             '(#f #f #t)))


(mat cli-subcommands-p

     (begin
       (define seen '())
       (define root (make-command 'prog))
       (define opt-config (make-option 'config))
       (option-value-parser-set! opt-config parser-string)
       (option-default-set! opt-config "default.cfg")
       (command-options-add! root opt-config)
       (command-exec-set! root
         (lambda (ref)
           (set! seen (cons (list 'root (ref 'config)) seen))
           'root-result))

       (define build (make-command 'build))
       (define opt-release (make-option 'release))
       (option-value-number-set! opt-release 0)
       (command-options-add! build opt-release)
       (command-exec-set! build
         (lambda (ref)
           (set! seen (cons (list 'build (ref 'config) (ref 'release)) seen))
           (reverse seen)))

       (command-subcommands-add! root build)
       #t)

     (equal? (silent (lambda () (run-cli-command! root '("--config=app.cfg" "build" "--release"))))
             '((root "app.cfg") (build "app.cfg" #t)))
     (error? (run-cli-command! root '("unknown")))
     (error? (run-cli-command! root '("--release" "build")))

     (begin
       (define seen-complex '())
       (define complex-root (make-command 'tool))
       (command-maybe-no-subcommand?-set! complex-root #t)
       (command-multi-personality?-set! complex-root #t)
       (define opt-profile (make-option 'profile))
       (option-value-parser-set! opt-profile parser-string)
       (option-default-set! opt-profile "dev")
       (define opt-include (make-option 'include))
       (option-short-set! opt-include "I")
       (option-value-parser-set! opt-include parser-string)
       (option-number-set! opt-include '*)
       (option-value-number-set! opt-include '+)
       (command-options-add! complex-root opt-profile opt-include)
       (command-exec-set! complex-root
         (lambda (ref)
           (set! seen-complex
                 (cons (list 'root (ref 'profile) (ref 'include)) seen-complex))
           (reverse seen-complex)))

       (define package (make-command 'package))
       (define opt-jobs (make-option 'jobs))
       (option-value-parser-set! opt-jobs parser-natural)
       (option-default-set! opt-jobs 1)
       (command-options-add! package opt-jobs)
       (command-exec-set! package
         (lambda (ref)
           (set! seen-complex (cons (list 'package (ref 'jobs)) seen-complex))
           (reverse seen-complex)))

       (define publish (make-command 'publish))
       (define opt-tag (make-option 'tag))
       (option-value-parser-set! opt-tag parser-string)
       (option-default-set! opt-tag "latest")
       (define opt-force (make-option 'force))
       (option-value-number-set! opt-force 0)
       (define opt-target (make-option 'target))
       (option-positional?-set! opt-target #t)
       (command-options-add! publish opt-tag opt-force opt-target)
       (command-exec-set! publish
         (lambda (ref)
           (set! seen-complex
                 (cons (list 'publish (ref 'tag) (ref 'force) (ref 'target))
                       seen-complex))
           (reverse seen-complex)))

       (command-subcommands-add! package publish)
       (command-subcommands-add! complex-root package)
       #t)

     (equal? (begin
               (set! seen-complex '())
               (silent (lambda ()
                         (run-cli-command!
                          complex-root
                          '("--profile=prod" "-I=src,lib" "-I=vendor"
                            "package" "--jobs=4" "publish" "--tag=v1" "pkg.tgz")))))
             '((root "prod" ("src" "lib" "vendor"))
               (package 4)
               (publish "v1" #f "pkg.tgz")))
     (equal? (begin
               (set! seen-complex '())
               (silent (lambda () (run-cli-command! complex-root '()))))
             '((root "dev" #f)))
     (error? (run-cli-command! complex-root
                               '("package" "publish" "--force=true" "pkg.tgz")))
     (error? (let ([bad-root (make-command 'bad)]
                   [bad-sub (make-command 'sub)]
                   [opt-root (make-option 'config)]
                   [opt-sub (make-option 'config)])
               (command-options-add! bad-root opt-root)
               (command-options-add! bad-sub opt-sub)
               (command-subcommands-add! bad-root bad-sub)
               (run-cli-command! bad-root '("sub"))))
     (error? (let ([bad-root (make-command 'bad)]
                   [bad-sub (make-command 'sub)]
                   [arg (make-option 'arg)])
               (option-positional?-set! arg #t)
               (command-options-add! bad-root arg)
               (command-subcommands-add! bad-root bad-sub)
               (run-cli-command! bad-root '("sub"))))
     (error? (let ([bad-root (make-command 'bad)]
                   [bad-sub (make-command 'sub)]
                   [opt-a (make-option 'alpha)]
                   [opt-b (make-option 'apple)])
               (command-options-add! bad-root opt-a opt-b)
               (command-subcommands-add! bad-root bad-sub)
               (run-cli-command! bad-root '("sub"))))
     (begin
       (define bad-exec-ran? #f)
       (define bad-exec-root (make-command 'bad))
       (define bad-exec-sub (make-command 'sub))
       (define bad-exec-root-opt (make-option 'config))
       (define bad-exec-sub-opt (make-option 'config))
       (define bad-exec-arg (make-option 'arg))
       (option-positional?-set! bad-exec-arg #t)
       (command-options-add! bad-exec-root bad-exec-root-opt)
       (command-options-add! bad-exec-sub bad-exec-sub-opt bad-exec-arg)
       (command-subcommands-add! bad-exec-root bad-exec-sub)
       (command-exec-set! bad-exec-root (lambda (ref) (set! bad-exec-ran? #t)))
       #t)
     (error? (run-cli-command! bad-exec-root '("sub" "x")))
     (not bad-exec-ran?)
     (error? (let ([bad-root (make-command 'bad)]
                   [bad-sub (make-command 'sub)])
               (command-multi-personality?-set! bad-sub #t)
               (command-subcommands-add! bad-root bad-sub)
               (run-cli-command! bad-root '("sub"))))

     )


(mat cli-help-p

     (begin
       (define ran? #f)
       (define root (make-command 'prog))
       (command-overview-set! root "Program overview")
       (command-about-set! root "Example: prog run input.ss")
       (command-version-set! root "1.0.0")
       (command-author-set! root "Maoif")
       (define opt-hidden (make-option 'secret))
       (option-hidden?-set! opt-hidden #t)
       (define opt-config (make-option 'config))
       (option-help-set! opt-config "Config file")
       (option-category-set! opt-config "GLOBAL")
       (option-value-parser-set! opt-config parser-string)
       (define sub (make-command 'run))
       (command-help-set! sub "Run the program")
       (command-about-set! sub "Example: prog run main.ss")
       (command-category-set! sub "TASKS")
       (define opt-threads (make-option 'threads))
       (option-help-set! opt-threads "Worker count")
       (option-value-parser-set! opt-threads parser-natural)
       (option-default-set! opt-threads 1)
       (define opt-input (make-option 'input))
       (option-help-set! opt-input "Input file")
       (option-value-name-set! opt-input "<INPUT>")
       (option-positional?-set! opt-input #t)
       (command-options-add! sub opt-threads opt-input)
       (define deploy (make-command 'deploy))
       (command-help-set! deploy "Deploy the program")
       (command-category-set! deploy "TASKS")
       (command-options-add! root opt-hidden opt-config)
       (command-subcommands-add! root sub deploy)
       (command-exec-set! root (lambda (ref) (set! ran? #t)))
       #t)

     (let ([ans (capture-output (lambda () (run-cli-command! root '("--help"))))])
       (and (not ran?)
            (output-contains? (cdr ans) "Usage: prog")
            (output-contains? (cdr ans) "GLOBAL")
            (output-contains? (cdr ans) "TASKS")
            (output-contains? (cdr ans) "--config=<CONFIG>")
            (output-contains? (cdr ans) "Example: prog run input.ss")
            (not (output-contains? (cdr ans) "--secret"))))
     (let ([ans (capture-output (lambda () (run-cli-command! root '("--help-all"))))])
       (output-contains? (cdr ans) "--secret"))
     (let ([ans (capture-output (lambda () (run-cli-command! root '("--help-commands"))))])
       (and (output-contains? (cdr ans) "prog")
            (output-contains? (cdr ans) "prog run")
            (output-contains? (cdr ans) "prog deploy")))
     (let ([ans (capture-output (lambda () (run-cli-command! root '("run" "--help"))))])
       (and (output-contains? (cdr ans) "Usage: prog [OPTIONS] run [OPTIONS] <INPUT>")
            (output-contains? (cdr ans) "ARGUMENTS")
            (output-contains? (cdr ans) "<INPUT>")
            (output-contains? (cdr ans) "OPTIONS")
            (output-contains? (cdr ans) "--threads=<THREADS>")
            (output-contains? (cdr ans) "Example: prog run main.ss")
            (not (output-contains? (cdr ans) "Example: prog run input.ss"))
            (not (output-contains? (cdr ans) "COMMANDS"))))
     )

(mat cli-help-alignment-p

     (let* ([cmd (cli-command prog
                   :options
                   [configuration :value-parser string :help "Configuration file"]
                   [verbose :help "Verbose output"])]
            [ans (capture-output (lambda () (run-cli-command! cmd '("--help"))))]
            [lines (string-split (cdr ans) #\newline)]
            [config-line (find (lambda (line) (string-search line "Configuration file")) lines)]
            [verbose-line (find (lambda (line) (string-search line "Verbose output")) lines)])
       (= (string-search config-line "Configuration file")
          (string-search verbose-line "Verbose output"))))



;;===----------------------------------------------------------------------===
;; syntactic interface
;;===----------------------------------------------------------------------===


(mat cli-macro-p

     (begin
       (define color-parser
         (cli-enum [red :value 1] [green :value 2]))
       (define common-options
         (cli-option-group
          [verbose :short #\v]
          [color :value-parser color-parser :default 1]))
       (define cmd
         (cli-command prog
           :overview "Macro program"
           :options common-options
           :subcommands
           [run
            :exec (lambda (ref) (list (ref 'verbose) (ref 'color)))
            :options
            [input :positional]]))
       #t)

     (equal? (silent (lambda () (run-cli-command! cmd '("-v" "--color=green" "run" "file"))))
             '(#t 2))

     (begin
       (define macro-events '())
       (define mode-parser
         (cli-enum [fast :value fast] [safe :value safe]))
       (define output-option
         (cli-option output
           :short #\o
           :alias (out)
           :value-parser string
           :default "a.out"
           :category "GLOBAL"))
       (define macro-common-options
         (cli-option-group
          [verbose :short #\v]
          [mode :value-parser mode-parser :default 'fast]
          output-option))
       (define macro-cmd
         (cli-command tool
           :overview "Tool"
           :multi-personality
           :maybe-no-subcommand
           :exec (lambda (ref)
                   (set! macro-events
                         (cons (list 'tool (ref 'mode) (ref 'output) (ref 'out))
                               macro-events))
                   (reverse macro-events))
           :options macro-common-options
           :subcommands
           [build
            :help "Build artifacts"
            :exec (lambda (ref)
                    (set! macro-events
                          (cons (list 'build (ref 'jobs) (ref 'clean))
                                macro-events))
                    (reverse macro-events))
            :options
            [jobs :value-parser natural :default 1]
            [clean :value-number 0 :conflicts (jobs)]
            :subcommands
            [package
             :exec (lambda (ref)
                     (set! macro-events
                           (cons (list 'package (ref 'target) (ref 'force))
                                 macro-events))
                     (reverse macro-events))
             :options
             [force :short #\f]
             [target :positional]]]))
       #t)

     (equal? (begin
               (set! macro-events '())
               (silent (lambda ()
                         (run-cli-command!
                          macro-cmd
                          '("--mode=safe" "--output=dist" "build" "--jobs=8"
                            "package" "-f" "pkg")))))
             '((tool safe "dist" "dist")
               (build 8 #f)
               (package "pkg" #t)))
     (equal? (begin
               (set! macro-events '())
               (silent (lambda () (run-cli-command! macro-cmd '()))))
             '((tool fast "a.out" "a.out")))
     (error? (run-cli-command! macro-cmd
                               '("build" "--jobs=2" "--clean" "package" "pkg")))
     (error? (run-cli-command! macro-cmd
                               '("build" "package" "--force=true" "pkg")))
     (error? (run-cli-command!
              (cli-command bad
                :subcommands
                [sub :multi-personality])
              '("sub")))
     (error? (cli-option bad :unknown #t))
     (error? (cli-command bad :unknown #t))
     (error? (cli-command bad :options [123]))
     (error? (cli-command bad :subcommands [123]))

)
