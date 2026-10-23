#!chezscheme
(library (cspp rich live)
  (export rich-current-time
          rich-sleep
          make-rich-live
          rich-live
          rich-live?
          rich-live-renderable
          rich-live-renderable-set!
          rich-live-console
          rich-live-console-set!
          rich-live-refresh-rate
          rich-live-refresh-rate-set!
          rich-live-transient?
          rich-live-transient?-set!
          rich-live-start!
          rich-live-refresh!
          rich-live-stop!
          make-rich-status
          rich-status
          rich-status?
          rich-status-message
          rich-status-message-set!
          rich-status-console
          rich-status-console-set!
          rich-status-frames
          rich-status-frames-set!
          rich-status-interval
          rich-status-interval-set!
          rich-status-start!
          rich-status-stop!
          rich-status-render)
  (import (cspp chez)
          (cspp utils)
          (cspp rich private common)
          (cspp rich private cells)
          (cspp rich style)
          (cspp rich segment)
          (cspp rich renderable)
          (cspp rich console))

  (define-record-type (rich-live-record $make-rich-live-record $rich-live?)
    (fields (mutable renderable $rich-live-renderable $rich-live-renderable-set!)
            (mutable console $rich-live-console $rich-live-console-set!)
            (mutable refresh-rate $rich-live-refresh-rate $rich-live-refresh-rate-set!)
            (mutable transient? $rich-live-transient? $rich-live-transient?-set!)
            (mutable started? $rich-live-started? $rich-live-started?-set!)
            (mutable previous-frame $rich-live-previous-frame $rich-live-previous-frame-set!)
            (mutable previous-port $rich-live-previous-port $rich-live-previous-port-set!)
            (mutable thread $rich-live-thread $rich-live-thread-set!)
            (mutable stop? $rich-live-stop? $rich-live-stop?-set!)
            (mutable failure $rich-live-failure $rich-live-failure-set!)
            (immutable wake-lock $rich-live-wake-lock)
            (immutable wake-condition $rich-live-wake-condition)))

  (define-record-type (rich-status-record $make-rich-status-record $rich-status?)
    (fields (mutable message $rich-status-message $rich-status-message-set!)
            (mutable console $rich-status-console $rich-status-console-set!)
            (mutable frames $rich-status-frames $rich-status-frames-set!)
            (mutable interval $rich-status-interval $rich-status-interval-set!)
            (mutable start-time $rich-status-start-time $rich-status-start-time-set!)
            (mutable live $rich-status-live $rich-status-live-set!)))

  (define $default-status-frames
    '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))

  (define $real-nonnegative?
    (lambda (x)
      (and (real? x) (finite? x) (>= x 0))))

  (define $real-positive?
    (lambda (x)
      (and (real? x) (finite? x) (positive? x))))

  (define $status-message?
    (lambda (x)
      (string? x)))

  (define $string-list?
    (lambda (x)
      (and (list? x)
           (not (null? x))
           (rich-list-every? string? x))))

  (define $time->seconds
    (lambda (time)
      (+ (time-second time)
         (/ (time-nanosecond time) 1000000000.0))))

  (define $round->exact
    (lambda (value)
      (let ([rounded (round value)])
        (if (exact? rounded)
            rounded
            (inexact->exact rounded)))))

  (define $sleep-duration
    (lambda (seconds)
      (let* ([milliseconds (max 0 ($round->exact (* seconds 1000)))]
             [sec (quotient milliseconds 1000)]
             [nsec (* 1000000 (remainder milliseconds 1000))])
        (make-time 'time-duration nsec sec))))

  (define $default-sleep
    (lambda (seconds) ($sleep ($sleep-duration seconds))))

  ;; Return true on expiry, false when a setter or shutdown wakes the worker.
  ;; Custom sleep hooks remain responsible for returning promptly themselves.
  (define $wait-live
    (lambda (live)
      (if (eq? (rich-sleep) $default-sleep)
          (with-mutex ($rich-live-wake-lock live)
            (and (not ($rich-live-stop? live))
                 (let ([rate (rich-live-refresh-rate live)])
                   (not (condition-wait ($rich-live-wake-condition live)
                                        ($rich-live-wake-lock live)
                                        (and (positive? rate) ($sleep-duration (/ 1 rate))))))))
          (let ([rate (rich-live-refresh-rate live)])
            ((rich-sleep) (if (positive? rate) (/ 1 rate) 0.05))
            #t))))

  (define $frame-line-count
    (lambda (frame width)
      (if (or (not frame) (string=? frame ""))
          0
          (let ([len (string-length frame)])
            (let loop ([i 0] [column 0] [rows 1])
              ;; column=width denotes the terminal's pending wrap at the right edge.
              (if (= i len)
                  rows
                  (let ([ch (string-ref frame i)])
                    (cond
                     [(char=? ch #\esc)
                      ;; SGR/erase sequences and OSC metadata do not occupy cells.
                      (cond
                       [(and (< (+ i 1) len) (char=? (string-ref frame (+ i 1)) #\[))
                        (let scan ([j (+ i 2)])
                          (cond [(= j len) (loop len column rows)]
                                [(<= #x40 (char->integer (string-ref frame j)) #x7e)
                                 (loop (+ j 1) column rows)]
                                [else (scan (+ j 1))]))]
                       [(and (< (+ i 1) len) (char=? (string-ref frame (+ i 1)) #\]))
                        (let scan ([j (+ i 2)])
                          (cond [(= j len) (loop len column rows)]
                                [(char=? (string-ref frame j) #\bel)
                                 (loop (+ j 1) column rows)]
                                [(and (char=? (string-ref frame j) #\esc)
                                      (< (+ j 1) len)
                                      (char=? (string-ref frame (+ j 1)) #\\))
                                 (loop (+ j 2) column rows)]
                                [else (scan (+ j 1))]))]
                       [else (loop (min len (+ i 2)) column rows)])]
                     [(char=? ch #\newline) (loop (+ i 1) 0 (+ rows 1))]
                     [(char=? ch #\return) (loop (+ i 1) 0 rows)]
                     [(char=? ch #\backspace)
                      (loop (+ i 1) (max 0 (- column (if (= column width) 2 1))) rows)]
                     [(char=? ch #\tab)
                      (loop (+ i 1) (min (- width 1) (* 8 (+ 1 (quotient column 8)))) rows)]
                     [(< (char->integer ch) 32) (loop (+ i 1) column rows)]
                     [else
                      (let-values ([(end cells) (rich-cell-next frame i)])
                        (cond [(zero? cells) (loop end column rows)]
                              [(> (+ column cells) width) (loop end cells (+ rows 1))]
                              [else (loop end (+ column cells) rows)]))]))))))))

  (define $clear-live-output
    (lambda (port line-count)
      (when (positive? line-count)
        (display "\r\033[2K" port)
        (let loop ([n (- line-count 1)])
          (when (positive? n)
            (display "\033[1A\r\033[2K" port)
            (loop (- n 1)))))))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Time Hooks
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-current-time
  The `rich-current-time` parameter returns the current time in seconds for
  live display and status rendering.
  |#
  (define rich-current-time
    (make-parameter
     (lambda ()
       ($time->seconds (current-time)))))

  #|proc:rich-sleep
  The `rich-sleep` parameter returns the procedure used by live refresh threads
  to sleep for a number of seconds.
  |#
  (define rich-sleep
    (make-parameter $default-sleep))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Live
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-live?
  The `rich-live?` procedure returns `#t` when its argument is a rich live
  display object, and `#f` otherwise.
  |#
  (define rich-live?
    (lambda (x)
      ($rich-live? x)))

  #|proc:make-rich-live
  The `make-rich-live` procedure constructs a live display around `renderable`
  using a fresh console, four refreshes per second, and non-transient output.
  |#
  (define make-rich-live
    (case-lambda
      [(renderable)
       ($make-rich-live-record renderable (make-rich-console) 4 #f #f #f #f #f #f #f
                               (make-mutex) (make-condition))]
      [(renderable console refresh-rate transient?)
       (pcheck ([rich-console? console]
                [$real-nonnegative? refresh-rate]
                [boolean? transient?])
               ($make-rich-live-record renderable console refresh-rate
                                       transient? #f #f #f #f #f #f
                                       (make-mutex) (make-condition)))]))

  #|proc:rich-live-renderable
  The `rich-live-renderable` procedure returns the renderable for `live`.
  |#
  (define rich-live-renderable
    (lambda (live)
      (pcheck ([rich-live? live])
              ($rich-live-renderable live))))

  #|proc:rich-live-renderable-set!
  The `rich-live-renderable-set!` procedure sets the renderable for `live`.
  |#
  (define rich-live-renderable-set!
    (lambda (live renderable)
      (pcheck ([rich-live? live])
              ($rich-live-renderable-set! live renderable))))

  #|proc:rich-live-console
  The `rich-live-console` procedure returns the console for `live`.
  |#
  (define rich-live-console
    (lambda (live)
      (pcheck ([rich-live? live])
              ($rich-live-console live))))

  #|proc:rich-live-console-set!
  The `rich-live-console-set!` procedure sets the console for `live`.
  |#
  (define rich-live-console-set!
    (lambda (live console)
      (pcheck ([rich-live? live] [rich-console? console])
              ($rich-live-console-set! live console))))

  #|proc:rich-live-refresh-rate
  The `rich-live-refresh-rate` procedure returns refreshes per second for
  `live`.
  |#
  (define rich-live-refresh-rate
    (lambda (live)
      (pcheck ([rich-live? live])
              ($rich-live-refresh-rate live))))

  #|proc:rich-live-refresh-rate-set!
  The `rich-live-refresh-rate-set!` procedure sets nonnegative refreshes per second
  `refresh-rate` for `live` and returns unspecified. Zero pauses an existing worker;
  a positive rate resumes it. A display started at zero has no automatic worker.
  |#
  (define rich-live-refresh-rate-set!
    (lambda (live refresh-rate)
      (pcheck ([rich-live? live] [$real-nonnegative? refresh-rate])
              (with-mutex ($rich-live-wake-lock live)
                ($rich-live-refresh-rate-set! live refresh-rate)
                (condition-signal ($rich-live-wake-condition live))))))

  #|proc:rich-live-transient?
  The `rich-live-transient?` procedure returns whether `live` clears output on
  stop.
  |#
  (define rich-live-transient?
    (lambda (live)
      (pcheck ([rich-live? live])
              ($rich-live-transient? live))))

  #|proc:rich-live-transient?-set!
  The `rich-live-transient?-set!` procedure sets whether `live` clears output
  on stop.
  |#
  (define rich-live-transient?-set!
    (lambda (live transient?)
      (pcheck ([rich-live? live] [boolean? transient?])
              ($rich-live-transient?-set! live transient?))))

  #|proc:rich-live-refresh!
  The `rich-live-refresh!` procedure clears the previous live output and writes
  the current renderable to the live console and returns `live`. Each frame is rendered
  once at the current available width. Clearing counts wrapped physical terminal rows,
  including after resize, without counting a pending wrap at the right edge twice.
  |#
  (define rich-live-refresh!
    (lambda (live)
      (pcheck ([rich-live? live])
              (let* ([console (rich-live-console live)]
                     [port (rich-console-output-port console)]
                     [frame (rich-console-frame console (rich-live-renderable live))]
                     [width (rich-console-output-width console)])
                (when (eq? port ($rich-live-previous-port live))
                  ($clear-live-output port ($frame-line-count ($rich-live-previous-frame live) width)))
                (display frame port)
                (flush-output-port port)
                ($rich-live-previous-frame-set! live frame)
                ($rich-live-previous-port-set! live port)
                live))))

  #|proc:rich-live-start!
  The `rich-live-start!` procedure starts refreshing `live` and returns `live`.
  If the first refresh fails, the error is raised and `live` remains ready to retry.
  A later worker failure stops refreshing and allows restart after its cause is repaired.
  Stopping a failed display raises its saved failure after cleanup.
  |#
  (define rich-live-start!
    (lambda (live)
      (pcheck ([rich-live? live])
              (unless ($rich-live-started? live)
                (when ($rich-live-thread live)
                  (thread-join ($rich-live-thread live))
                  ($rich-live-thread-set! live #f))
                ($rich-live-failure-set! live #f)
                (guard (condition
                        [else
                         ($rich-live-stop?-set! live #t)
                         ($rich-live-started?-set! live #f)
                         (raise condition)])
                  ($rich-live-started?-set! live #t)
                  ($rich-live-stop?-set! live #f)
                  (rich-live-refresh! live)
                  (when (positive? (rich-live-refresh-rate live))
                    (let ([thread
                           (fork-thread
                            (lambda ()
                              (guard (condition
                                      [else
                                       ($rich-live-failure-set! live condition)
                                       ($rich-live-stop?-set! live #t)
                                       ($rich-live-started?-set! live #f)])
                                (let loop ()
                                  (unless ($rich-live-stop? live)
                                    (let ([expired? ($wait-live live)])
                                      (unless ($rich-live-stop? live)
                                        (when (and expired?
                                                   (positive? (rich-live-refresh-rate live)))
                                          (rich-live-refresh! live))
                                        (loop))))))))])
                      ($rich-live-thread-set! live thread)))))
              live)))

  #|proc:rich-live-stop!
  The `rich-live-stop!` procedure stops refreshing `live` and returns `live`.
  The default wait is interrupted immediately; custom `rich-sleep` hooks must return
  themselves. If its worker failed, cleanup finishes before its condition is raised once.
  |#
  (define rich-live-stop!
    (lambda (live)
      (pcheck ([rich-live? live])
        (with-mutex ($rich-live-wake-lock live)
          ($rich-live-stop?-set! live #t)
          (condition-signal ($rich-live-wake-condition live)))
        (when ($rich-live-thread live)
          (thread-join ($rich-live-thread live))
          ($rich-live-thread-set! live #f))
        (when (rich-live-transient? live)
          (let ([port (rich-console-output-port (rich-live-console live))])
            (when (eq? port ($rich-live-previous-port live))
              ($clear-live-output
               port ($frame-line-count ($rich-live-previous-frame live)
                                       (rich-console-output-width (rich-live-console live)))))
            (flush-output-port port)
            ($rich-live-previous-frame-set! live #f)
            ($rich-live-previous-port-set! live #f)))
        ($rich-live-started?-set! live #f)
        (let ([failure ($rich-live-failure live)])
          ($rich-live-failure-set! live #f)
          (when failure (raise failure)))
        live)))

  #|macro:rich-live
  The `rich-live` macro constructs and returns a live display. The
  `:renderable` field is required.
  |#
  (define-syntax rich-live
    (lambda (stx)
      (define field->setter
        (lambda (field)
          (let ([datum (syntax->datum field)])
            (case datum
              [(:renderable) #'rich-live-renderable-set!]
              [(:console) #'rich-live-console-set!]
              [(:refresh-rate) #'rich-live-refresh-rate-set!]
              [(:transient?) #'rich-live-transient?-set!]
              [else (syntax-error field "invalid rich-live field")]))))
      (define renderable-value
        (lambda (clause*)
          (let loop ([clause* clause*])
            (cond [(null? clause*) #f]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-live form")]
                  [(eq? (syntax->datum (car clause*)) ':renderable)
                   (cadr clause*)]
                  [else
                   (loop (cddr clause*))]))))
      (define build-actions
        (lambda (name clause*)
          (let loop ([clause* clause*] [setter* '()])
            (cond [(null? clause*) (reverse setter*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-live form")]
                  [else
                   (let ([field (car clause*)]
                         [value (cadr clause*)])
                     (if (eq? (syntax->datum field) ':renderable)
                         (loop (cddr clause*) setter*)
                         (with-syntax ([live-name name]
                                       [set-live-field! (field->setter field)]
                                       [field-value value])
                           (loop (cddr clause*)
                                 (cons #'(set-live-field! live-name field-value)
                                       setter*)))))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (let ([renderable (renderable-value #'(clause ...))])
           (unless renderable
             (syntax-error stx "rich-live requires :renderable"))
           (with-syntax ([tmp (car (generate-temporaries #'(rich-live)))]
                         [renderable-value renderable])
             (with-syntax ([(action ...) (build-actions #'tmp #'(clause ...))])
               #'(let ([tmp (make-rich-live renderable-value)])
                   action ...
                   tmp))))]
        [_ (syntax-error stx "invalid rich-live form")])))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Status
  ;;;;===----------------------------------------------------------------------===

  #|proc:rich-status?
  The `rich-status?` procedure returns `#t` when its argument is a rich status
  object, and `#f` otherwise.
  |#
  (define rich-status?
    (lambda (x)
      ($rich-status? x)))

  #|proc:make-rich-status
  The `make-rich-status` procedure constructs a status renderable with
  `message`, spinner frames, and deterministic time hooks. It does not start a
  live refresh loop; use `rich-status-start!` to display a spinning status.
  |#
  (define make-rich-status
    (case-lambda
      [(message)
       (make-rich-status message (make-rich-console) $default-status-frames 0.1)]
      [(message console frames interval)
       (pcheck ([$status-message? message]
                [rich-console? console]
                [$string-list? frames]
                [$real-positive? interval])
               ($make-rich-status-record message console frames interval
                                         ((rich-current-time))
                                         #f))]))

  #|proc:rich-status-message
  The `rich-status-message` procedure returns the message for `status`.
  |#
  (define rich-status-message
    (lambda (status)
      (pcheck ([rich-status? status])
              ($rich-status-message status))))

  #|proc:rich-status-message-set!
  The `rich-status-message-set!` procedure sets the message for `status`.
  |#
  (define rich-status-message-set!
    (lambda (status message)
      (pcheck ([rich-status? status] [$status-message? message])
              ($rich-status-message-set! status message))))

  #|proc:rich-status-console
  The `rich-status-console` procedure returns the console for `status`.
  |#
  (define rich-status-console
    (lambda (status)
      (pcheck ([rich-status? status])
              ($rich-status-console status))))

  #|proc:rich-status-console-set!
  The `rich-status-console-set!` procedure sets `console` for `status` and its active
  live display, if any. Returns unspecified.
  |#
  (define rich-status-console-set!
    (lambda (status console)
      (pcheck ([rich-status? status] [rich-console? console])
              ($rich-status-console-set! status console)
              (when ($rich-status-live status)
                (rich-live-console-set! ($rich-status-live status) console)))))

  #|proc:rich-status-frames
  The `rich-status-frames` procedure returns the spinner frames for `status`.
  |#
  (define rich-status-frames
    (lambda (status)
      (pcheck ([rich-status? status])
              ($rich-status-frames status))))

  #|proc:rich-status-frames-set!
  The `rich-status-frames-set!` procedure sets the spinner frames for `status`.
  |#
  (define rich-status-frames-set!
    (lambda (status frames)
      (pcheck ([rich-status? status] [$string-list? frames])
              ($rich-status-frames-set! status frames))))

  #|proc:rich-status-interval
  The `rich-status-interval` procedure returns the spinner frame interval for
  `status`.
  |#
  (define rich-status-interval
    (lambda (status)
      (pcheck ([rich-status? status])
              ($rich-status-interval status))))

  #|proc:rich-status-interval-set!
  The `rich-status-interval-set!` procedure sets finite positive `interval` in seconds
  for `status`, also changing its active refresh rate. Returns unspecified.
  |#
  (define rich-status-interval-set!
    (lambda (status interval)
      (pcheck ([rich-status? status] [$real-positive? interval])
              ($rich-status-interval-set! status interval)
              (when ($rich-status-live status)
                (rich-live-refresh-rate-set! ($rich-status-live status) (/ 1 interval))))))

  #|proc:rich-status-render
  The `rich-status-render` procedure renders `status` as one segment line.
  |#
  (define rich-status-render
    (lambda (status)
      (pcheck ([rich-status? status])
              (let* ([frames (rich-status-frames status)]
                     [elapsed (max 0 (- ((rich-current-time))
                                        ($rich-status-start-time status)))]
                     [index (modulo (inexact->exact
                                     (floor (/ elapsed
                                               (rich-status-interval status))))
                                    (length frames))]
                     [frame (list-ref frames index)])
                (list (list (rich-segment frame)
                            (rich-segment " ")
                            (rich-segment (rich-status-message status))))))))

  #|proc:rich-status-start!
  The `rich-status-start!` procedure starts a live display for `status` so its
  frame is refreshed at the status interval and returns `status`. If startup fails,
  the error is raised and the status can be started again after its cause is repaired.
  |#
  (define rich-status-start!
    (lambda (status)
      (pcheck ([rich-status? status])
              (unless ($rich-status-live status)
                ($rich-status-start-time-set! status ((rich-current-time)))
                (let ([live (make-rich-live status
                                            (rich-status-console status)
                                            (/ 1 (rich-status-interval status))
                                            #f)])
                  (rich-live-start! live)
                  ($rich-status-live-set! status live)))
              status)))

  #|proc:rich-status-stop!
  The `rich-status-stop!` procedure stops the live display for `status`.
  |#
  (define rich-status-stop!
    (lambda (status)
      (pcheck ([rich-status? status])
              (when ($rich-status-live status)
                (rich-live-stop! ($rich-status-live status))
                ($rich-status-live-set! status #f))
              status)))

  #|macro:rich-status
  The `rich-status` macro constructs and returns a status spinner. The
  `:message` field is required.
  |#
  (define-syntax rich-status
    (lambda (stx)
      (define field->setter
        (lambda (field)
          (let ([datum (syntax->datum field)])
            (case datum
              [(:message) #'rich-status-message-set!]
              [(:console) #'rich-status-console-set!]
              [(:frames) #'rich-status-frames-set!]
              [(:interval) #'rich-status-interval-set!]
              [else (syntax-error field "invalid rich-status field")]))))
      (define message-value
        (lambda (clause*)
          (let loop ([clause* clause*])
            (cond [(null? clause*) #f]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-status form")]
                  [(eq? (syntax->datum (car clause*)) ':message)
                   (cadr clause*)]
                  [else
                   (loop (cddr clause*))]))))
      (define build-actions
        (lambda (name clause*)
          (let loop ([clause* clause*] [setter* '()])
            (cond [(null? clause*) (reverse setter*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-status form")]
                  [else
                   (let ([field (car clause*)]
                         [value (cadr clause*)])
                     (if (eq? (syntax->datum field) ':message)
                         (loop (cddr clause*) setter*)
                         (with-syntax ([status-name name]
                                       [set-status-field! (field->setter field)]
                                       [field-value value])
                           (loop (cddr clause*)
                                 (cons #'(set-status-field! status-name field-value)
                                       setter*)))))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (let ([message (message-value #'(clause ...))])
           (unless message
             (syntax-error stx "rich-status requires :message"))
           (with-syntax ([tmp (car (generate-temporaries #'(rich-status)))]
                         [message-value message])
             (with-syntax ([(action ...) (build-actions #'tmp #'(clause ...))])
               #'(let ([tmp (make-rich-status message-value)])
                   action ...
                   tmp))))]
        [_ (syntax-error stx "invalid rich-status form")])))

  (rich-register-renderer! rich-status? rich-status-render))
