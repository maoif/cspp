(import (cspp) (cspp net http) (cspp logging) (cspp rich))
(load "examples/common.ss")
(load "examples/net/transfer-progress.ss")

;; PROC receives (report path completed-bytes total-bytes-or-#f).
(define call-with-wget-progress
  (lambda (enabled? proc)
    (if (not enabled?)
        (proc (lambda (path completed total) (void)))
        ;; Create the Rich console and progress task used throughout this download.
        (let* ([console (make-rich-console)]
               ;; The recent byte rate is appended below; avoid duplicate timing columns.
               [progress (make-rich-progress 24 '(description bar percentage completed))]
               [task (rich-progress-add-task! progress "download" #f)]
               ;; Refresh synchronously after chunks; no background thread is needed.
               [live (make-rich-live progress console 0 #f)]
               [last-render #f] [previous-count 0] [previous-total #f] [speed 0])
          (define report
            (lambda (path completed total)
              (let* ([time (current-time 'time-monotonic)]
                     [now (+ (time-second time) (/ (time-nanosecond time) 1000000000))]
                     [changed? (or (not (= completed previous-count))
                                   (not (equal? total previous-total)))])
                (when (or (not last-render)
                          (and changed?
                               (or (>= (- now last-render) 1/10)
                                   (and (zero? previous-count) (positive? completed))
                                   (and total (= completed total)))))
                  (when (and last-render (> now last-render) (> completed previous-count))
                    (set! speed (/ (- completed previous-count) (- now last-render))))
                  ;; Set the file label, advertised size, and byte count on the Rich task.
                  (rich-progress-task-description-set! progress task (format "~s" path))
                  (rich-progress-task-total-set! progress task total)
                  (rich-progress-update! progress task completed)
                  (when (and total (= completed total))
                    ;; Mark a finished task, including an empty file, as complete.
                    (rich-progress-complete! progress task))
                  (let ([rate (let loop ([value (inexact speed)]
                                        [units '(B/s KiB/s MiB/s GiB/s)])
                                (if (or (< value 1024) (null? (cdr units)))
                                    (format "~8,1f ~5a" value (car units))
                                    (loop (/ value 1024) (cdr units))))])
                    ;; Render the library's bar and counts, then append the recent byte rate.
                    (rich-live-renderable-set!
                     live (rich-text (string-append (rich-export-text progress)
                                                    " bytes " rate
                                                    (if total "" " (total unknown)")))))
                  ;; Let Rich clear and redraw the live line on stderr.
                  (if last-render (rich-live-refresh! live) (rich-live-start! live))
                  (set! last-render now)
                  (set! previous-count completed)
                  (set! previous-total total)))))
          ;; Progress and logging share stderr; downloaded bytes go only to the file.
          (rich-console-output-port-set! console (current-error-port))
          ;; Preserve the progress fields in redirected output; terminals wrap the live line.
          (rich-console-soft-wrap?-set! console #t)
          (dynamic-wind
            void
            (lambda () (proc report))
            (lambda ()
              (when last-render
                ;; Stop the Rich display and separate the next lifecycle/error log line.
                (rich-live-stop! live)
                (rich-println console)
                (flush-output-port (current-error-port)))))))))

(define download-file!
  (lambda (client uri output report)
    ;; Create a sibling file exclusively so failed requests cannot damage the destination.
    (let* ([temporary (format "~a.cspp-download-~a-~a"
                              output (get-process-id) (random (most-positive-fixnum)))]
           [port (open-file-output-port temporary (file-options) (buffer-mode block) #f)]
           [completed 0]
           [total #f])
      (dynamic-wind
        void
        (lambda ()
          (report output 0 #f)
          ;; Stream each body chunk to disk and report bytes without buffering the whole file.
          (let* ([sink (make-http-body-sink
                        (lambda (bytes start count)
                          (put-bytevector port bytes start count)
                          (set! completed (+ completed count))
                          (report output completed total))
                        (lambda () (flush-output-port port))
                        (lambda (status headers)
                          ;; Read the total from the actual response, after followed redirects.
                          (let ([length (http-header-ref headers "Content-Length" #f)]
                                [encoding (http-header-ref headers "Transfer-Encoding" #f)])
                            (set! total
                                  (and (<= 200 status 299) (not encoding) length
                                       (positive? (string-length length))
                                       (for-all (lambda (char) (char<=? #\0 char #\9))
                                                (string->list length))
                                       (string->number length)))
                            (report output completed total))))]
                 ;; Build a GET request and wait for the streaming HTTP operation.
                 [request (make-http-request 'get uri '() #f)]
                 [response (net-operation-wait (http-send/nonblocking client request sink))]
                 [status (http-response-status response)])
            (unless (<= 200 status 299)
              (errorf 'cspp-wget "download failed: HTTP ~a for ~a" status uri))
            (close-port port)
            ;; Publish only a complete successful download, replacing any existing output.
            (rename-file temporary output)
            (report output completed completed)
            response))
        (lambda ()
          (unless (port-closed? port) (close-port port))
          (when (file-exists? temporary) (delete-file temporary)))))))

(example-run!
 'cspp-wget
 "Download a URI to a file using HTTP or HTTP/2."
 '((uri . "Required: HTTP or HTTPS URI to download.")
   (output . "Required: destination file path.")
   (version . "HTTP policy: auto, http/1.1, or h2 (default: auto).")
   (timeout . "Total request timeout in seconds, a positive integer (default: 3600).")
   (progress . "Show progress and current download speed on stderr: yes or no (default: yes).")
   (user-agent . "User-Agent sent to the server (default: cspp-wget)."))
 (lambda (ref)
   (let* ([uri (example-required 'cspp-wget ref 'uri)]
          [output (example-required 'cspp-wget ref 'output)]
          [version (string->symbol (or (ref 'version) "auto"))]
          [timeout (example-option-natural 'cspp-wget ref 'timeout 3600)]
          [user-agent (or (ref 'user-agent) "cspp-wget")]
          [progress? (example-progress-enabled? 'cspp-wget ref)]
          [client (http-open)]
          [log (make-logger 'cspp-wget)])
     (logger-add-sink! log (make-log-port-sink 'stderr (current-error-port)))
     (dynamic-wind
       void
       (lambda ()
         (unless (memq version '(auto http/1.1 h2))
           (errorf 'cspp-wget "version must be auto, http/1.1, or h2"))
         (unless (<= 1 timeout (fxquotient (most-positive-fixnum) 1000))
           (errorf 'cspp-wget "timeout must be positive and fit in milliseconds as a fixnum"))
         ;; Allow large downloads to finish within the selected total request deadline.
         (http-set-timeout! client (* timeout 1000))
         ;; Identify the downloader; mirrors may reject requests without a User-Agent.
         (http-set-header! client "User-Agent" user-agent)
         ;; Select the requested HTTP protocol policy on the client.
         (http-client-version-set! client version)
         ;; Preserve the destination until the request has completed with a successful status.
         (let ([response (call-with-wget-progress
                           progress?
                           (lambda (report) (download-file! client uri output report)))])
           (with-logger log
             (log-infof "downloaded ~a to ~a with HTTP ~a (status ~a)"
                        uri output (http-response-version response)
                        (http-response-status response)))
           0))
       (lambda ()
         ;; Close both the HTTP client and logger when transfer ends or fails.
         (http-close client)
         (logger-close! log)))))
 (command-line-arguments))
