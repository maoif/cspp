(library (cspp net lws ffi)
  (export lws-cap-http1
          lws-cap-http2
          lws-cap-tls
          lws-cap-socks5
          lws-cap-external-poll
          lws-status
          lws-capability?
          lws-require-capability!
          lws-context-open
          lws-server-context-open
          lws-context-close
          lws-context-wakeup-fd
          lws-context-poll-snapshot
          lws-context-service-fd
          lws-context-service
          lws-context-next-event
          lws-context-timeout-ms
          lws-context-wakeup
          lws-context-pool-metrics
          lws-signal-open
          lws-signal-fd
          lws-signal-notify
          lws-signal-drain
          lws-signal-close
          lws-client-start
          lws-client-acquire
          lws-client-release
          lws-client-body-submit
          lws-client-body-drain
          lws-server-request-dequeue
          lws-server-response-submit
          lws-stream-cancel
          lws-body-consumed
          lws-context-inject-event!
          lws-context-inject-poll!)
  (import (cspp utils) (cspp private optional-library) (cspp chez)
          (cspp net errors)
          )

  (define lws-cap-http1 (fxsll 1 0))
  (define lws-cap-http2 (fxsll 1 1))
  (define lws-cap-tls (fxsll 1 2))
  (define lws-cap-socks5 (fxsll 1 3))
  (define lws-cap-external-poll (fxsll 1 4))

  (define ffi-lws-status
    (foreign-procedure "cspp_lws_status" () scheme-object))

  #|proc:ffi-lws-context-open
  The `ffi-lws-context-open` procedure calls the native websockets operation
  `cspp_lws_http_context_open`.
  Parameters `event-capacity`, `payload-capacity`, `tls-context-handle`, `proxy-address`,
  `proxy-port` are passed to the native operation in that order.
  `event-capacity` is a native handle.
  `payload-capacity` is a native handle.
  `tls-context-handle` is a native handle.
  `proxy-address` is a string.
  `proxy-port` is a number.
  It returns an owned native handle, or zero when the resource could not be created.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-context-open
    (let ([native (foreign-procedure "cspp_lws_http_context_open" (uptr uptr uptr string int) uptr)])
      (lambda (event-capacity payload-capacity tls-context-handle proxy-address proxy-port)
        (pcheck ([natural? event-capacity] [natural? payload-capacity] [natural? tls-context-handle] [string? proxy-address] [integer? proxy-port])
                (require-optional-library 'ffi-lws-context-open 'websockets)
                (native event-capacity payload-capacity tls-context-handle proxy-address proxy-port)))))
  #|proc:ffi-lws-server-context-open
  The `ffi-lws-server-context-open` procedure calls the native websockets operation
  `cspp_lws_http_server_context_open_version`.
  `prior-knowledge?` selects cleartext H2 when true.
  Parameters `event-capacity`, `payload-capacity`, `interface-name`, `port`, `tls-context-handle`
  are passed to the native operation in that order.
  `event-capacity` is a native handle.
  `payload-capacity` is a native handle.
  `interface-name` is a string.
  `port` is a number.
  `tls-context-handle` is a native handle.
  It returns an owned native handle, or zero when the resource could not be created.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-server-context-open
    (let ([native (foreign-procedure "cspp_lws_http_server_context_open_version" (uptr uptr string int uptr int) uptr)])
      (lambda (event-capacity payload-capacity interface-name port tls-context-handle prior-knowledge?)
        (pcheck ([natural? event-capacity] [natural? payload-capacity] [string? interface-name] [integer? port] [natural? tls-context-handle] [boolean? prior-knowledge?])
                (require-optional-library 'ffi-lws-server-context-open 'websockets)
                (native event-capacity payload-capacity interface-name port tls-context-handle
                        (if prior-knowledge? 1 0))))))
  #|proc:ffi-lws-context-close
  The `ffi-lws-context-close` procedure calls the native websockets operation
  `cspp_lws_http_context_close`.
  Parameters `context-handle` are passed to the native operation in that order.
  `context-handle` is the native HTTP context handle.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-context-close
    (let ([native (foreign-procedure "cspp_lws_http_context_close" (uptr) void)])
      (lambda (context-handle)
        (pcheck ([natural? context-handle])
                (require-optional-library 'ffi-lws-context-close 'websockets)
                (native context-handle)))))
  #|proc:ffi-lws-context-wakeup-fd
  The `ffi-lws-context-wakeup-fd` procedure calls the native websockets operation
  `cspp_lws_http_context_wakeup_fd`.
  Parameters `context-handle` are passed to the native operation in that order.
  `context-handle` is the native HTTP context handle.
  It returns the native file descriptor, or -1 when the handle has no descriptor.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-context-wakeup-fd
    (let ([native (foreign-procedure "cspp_lws_http_context_wakeup_fd" (uptr) int)])
      (lambda (context-handle)
        (pcheck ([natural? context-handle])
                (require-optional-library 'ffi-lws-context-wakeup-fd 'websockets)
                (native context-handle)))))
  (define ffi-lws-context-poll-snapshot
    (foreign-procedure "cspp_lws_http_context_poll_snapshot" (uptr) scheme-object))
  #|proc:ffi-lws-context-service-fd
  The `ffi-lws-context-service-fd` procedure calls the native websockets operation
  `cspp_lws_http_context_service_fd`.
  Parameters `context-handle`, `fd`, `revents` are passed to the native operation in that order.
  `context-handle` is the native HTTP context handle.
  `fd` is a number.
  `revents` is a number.
  It returns the libwebsockets service result, zero for the wakeup descriptor, or -1 for an
  invalid context.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-context-service-fd
    (let ([native (foreign-procedure "cspp_lws_http_context_service_fd" (uptr int int) int)])
      (lambda (context-handle fd revents)
        (pcheck ([natural? context-handle] [integer? fd] [integer? revents])
                (require-optional-library 'ffi-lws-context-service-fd 'websockets)
                (native context-handle fd revents)))))
  (define ffi-lws-context-next-event
    (foreign-procedure "cspp_lws_http_context_next_event" (uptr) scheme-object))
  (define ffi-lws-context-service
    (foreign-procedure "cspp_lws_http_context_service" (uptr int) int))
  #|proc:ffi-lws-context-timeout-ms
  The `ffi-lws-context-timeout-ms` procedure calls the native websockets operation
  `cspp_lws_http_context_timeout_ms`.
  Parameters `context-handle`, `maximum-timeout-ms` are passed to the native operation in that
  order.
  `context-handle` is the native HTTP context handle.
  `maximum-timeout-ms` is a number.
  It returns the service timeout in milliseconds, or -1 for an invalid context.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-context-timeout-ms
    (let ([native (foreign-procedure "cspp_lws_http_context_timeout_ms" (uptr int) int)])
      (lambda (context-handle maximum-timeout-ms)
        (pcheck ([natural? context-handle] [integer? maximum-timeout-ms])
                (require-optional-library 'ffi-lws-context-timeout-ms 'websockets)
                (native context-handle maximum-timeout-ms)))))
  #|proc:ffi-lws-context-wakeup
  The `ffi-lws-context-wakeup` procedure calls the native websockets operation
  `cspp_lws_http_context_wakeup`.
  Parameters `context-handle` are passed to the native operation in that order.
  `context-handle` is the native HTTP context handle.
  It returns 1 when the context was notified and 0 when notification failed.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-context-wakeup
    (let ([native (foreign-procedure "cspp_lws_http_context_wakeup" (uptr) int)])
      (lambda (context-handle)
        (pcheck ([natural? context-handle])
                (require-optional-library 'ffi-lws-context-wakeup 'websockets)
                (native context-handle)))))
  (define ffi-lws-context-pool-metrics
    (foreign-procedure "cspp_lws_http_context_pool_metrics" (uptr) scheme-object))
  #|proc:ffi-lws-signal-open
  The `ffi-lws-signal-open` procedure calls the native websockets operation
  `cspp_lws_http_signal_open`.
  Parameters `context-handle` are passed to the native operation in that order.
  `context-handle` is the native HTTP context handle.
  It returns an owned native handle, or zero when the resource could not be created.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-signal-open
    (let ([native (foreign-procedure "cspp_lws_http_signal_open" (uptr) uptr)])
      (lambda (context-handle)
        (pcheck ([natural? context-handle])
                (require-optional-library 'ffi-lws-signal-open 'websockets)
                (native context-handle)))))
  #|proc:ffi-lws-signal-fd
  The `ffi-lws-signal-fd` procedure calls the native websockets operation
  `cspp_lws_http_signal_fd`.
  Parameters `signal-handle` are passed to the native operation in that order.
  `signal-handle` is the native signal handle.
  It returns the native file descriptor, or -1 when the handle has no descriptor.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-signal-fd
    (let ([native (foreign-procedure "cspp_lws_http_signal_fd" (uptr) int)])
      (lambda (signal-handle)
        (pcheck ([natural? signal-handle])
                (require-optional-library 'ffi-lws-signal-fd 'websockets)
                (native signal-handle)))))
  #|proc:ffi-lws-signal-notify
  The `ffi-lws-signal-notify` procedure calls the native websockets operation
  `cspp_lws_http_signal_notify`.
  Parameters `signal-handle` are passed to the native operation in that order.
  `signal-handle` is the native signal handle.
  It returns 1 when a signal is pending or was delivered and 0 on failure.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-signal-notify
    (let ([native (foreign-procedure "cspp_lws_http_signal_notify" (uptr) int)])
      (lambda (signal-handle)
        (pcheck ([natural? signal-handle])
                (require-optional-library 'ffi-lws-signal-notify 'websockets)
                (native signal-handle)))))
  #|proc:ffi-lws-signal-drain
  The `ffi-lws-signal-drain` procedure calls the native websockets operation
  `cspp_lws_http_signal_drain`.
  Parameters `signal-handle` are passed to the native operation in that order.
  `signal-handle` is the native signal handle.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-signal-drain
    (let ([native (foreign-procedure "cspp_lws_http_signal_drain" (uptr) void)])
      (lambda (signal-handle)
        (pcheck ([natural? signal-handle])
                (require-optional-library 'ffi-lws-signal-drain 'websockets)
                (native signal-handle)))))
  #|proc:ffi-lws-signal-close
  The `ffi-lws-signal-close` procedure calls the native websockets operation
  `cspp_lws_http_signal_close`.
  Parameters `signal-handle` are passed to the native operation in that order.
  `signal-handle` is the native signal handle.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-signal-close
    (let ([native (foreign-procedure "cspp_lws_http_signal_close" (uptr) void)])
      (lambda (signal-handle)
        (pcheck ([natural? signal-handle])
                (require-optional-library 'ffi-lws-signal-close 'websockets)
                (native signal-handle)))))
  #|proc:ffi-lws-client-start
  The `ffi-lws-client-start` procedure calls the native websockets operation
  `cspp_lws_http_client_start`.
  Parameters `context-handle`, `connection-id`, `stream-id`, `generation`, `address`, `port`,
  `tls`, `method`, `host`, `path`, `headers`, `initial-body`, `has-body`, `alpn` are passed to the
  native operation in that order.
  `context-handle` is the native HTTP context handle.
  `connection-id` is the HTTP connection identifier.
  `stream-id` is the HTTP stream identifier.
  `generation` is the HTTP stream generation.
  `address` is a string.
  `port` is a number.
  `tls` is a number.
  `method` is a string.
  `host` is a string.
  `path` is a string.
  `headers` is a Scheme object.
  `initial-body` is a Scheme object.
  `has-body` is a number.
  `alpn` is a string.
  It returns 1 when the HTTP command was accepted and 0 when the command was rejected.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-client-start
    (let ([native (foreign-procedure "cspp_lws_http_client_start" (uptr unsigned-64 unsigned-64 unsigned-64 string int int
                             string string string scheme-object scheme-object int string) int)])
      (lambda (context-handle connection-id stream-id generation address port tls method host path headers initial-body has-body alpn)
        (pcheck ([natural? context-handle] [natural? connection-id] [natural? stream-id] [natural? generation] [string? address] [integer? port] [integer? tls] [string? method] [string? host] [string? path] [integer? has-body] [string? alpn])
                (require-optional-library 'ffi-lws-client-start 'websockets)
                (native context-handle connection-id stream-id generation address port tls method host path headers initial-body has-body alpn)))))
  #|proc:ffi-lws-client-acquire
  The `ffi-lws-client-acquire` procedure calls the native websockets operation
  `cspp_lws_http_client_acquire`.
  Parameters `context-handle`, `connection-id`, `stream-id`, `generation` are passed to the native
  operation in that order.
  `context-handle` is the native HTTP context handle.
  `connection-id` is the HTTP connection identifier.
  `stream-id` is the HTTP stream identifier.
  `generation` is the HTTP stream generation.
  It returns 1 when the HTTP command was accepted and 0 when the command was rejected.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-client-acquire
    (let ([native (foreign-procedure "cspp_lws_http_client_acquire" (uptr unsigned-64 unsigned-64 unsigned-64) int)])
      (lambda (context-handle connection-id stream-id generation)
        (pcheck ([natural? context-handle] [natural? connection-id] [natural? stream-id] [natural? generation])
                (require-optional-library 'ffi-lws-client-acquire 'websockets)
                (native context-handle connection-id stream-id generation)))))
  #|proc:ffi-lws-client-release
  The `ffi-lws-client-release` procedure calls the native websockets operation
  `cspp_lws_http_client_release`.
  Parameters `context-handle`, `connection-id`, `stream-id`, `generation` are passed to the native
  operation in that order.
  `context-handle` is the native HTTP context handle.
  `connection-id` is the HTTP connection identifier.
  `stream-id` is the HTTP stream identifier.
  `generation` is the HTTP stream generation.
  It returns 1 when the HTTP command was accepted and 0 when the command was rejected.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-client-release
    (let ([native (foreign-procedure "cspp_lws_http_client_release" (uptr unsigned-64 unsigned-64 unsigned-64) int)])
      (lambda (context-handle connection-id stream-id generation)
        (pcheck ([natural? context-handle] [natural? connection-id] [natural? stream-id] [natural? generation])
                (require-optional-library 'ffi-lws-client-release 'websockets)
                (native context-handle connection-id stream-id generation)))))
  #|proc:ffi-lws-client-body-submit
  The `ffi-lws-client-body-submit` procedure calls the native websockets operation
  `cspp_lws_http_client_body_submit`.
  Parameters `context-handle`, `connection-id`, `stream-id`, `generation`, `payload`,
  `final-chunk` are passed to the native operation in that order.
  `context-handle` is the native HTTP context handle.
  `connection-id` is the HTTP connection identifier.
  `stream-id` is the HTTP stream identifier.
  `generation` is the HTTP stream generation.
  `payload` is a Scheme object.
  `final-chunk` is a number.
  It returns 1 when the HTTP command was accepted and 0 when the command was rejected.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-client-body-submit
    (let ([native (foreign-procedure "cspp_lws_http_client_body_submit" (uptr unsigned-64 unsigned-64 unsigned-64 scheme-object int) int)])
      (lambda (context-handle connection-id stream-id generation payload final-chunk)
        (pcheck ([natural? context-handle] [natural? connection-id] [natural? stream-id] [natural? generation] [integer? final-chunk])
                (require-optional-library 'ffi-lws-client-body-submit 'websockets)
                (native context-handle connection-id stream-id generation payload final-chunk)))))
  #|proc:ffi-lws-client-body-drain
  The `ffi-lws-client-body-drain` procedure calls the native websockets operation
  `cspp_lws_http_client_body_drain`.
  Parameters `context-handle`, `connection-id`, `stream-id`, `generation` are passed to the native
  operation in that order.
  `context-handle` is the native HTTP context handle.
  `connection-id` is the HTTP connection identifier.
  `stream-id` is the HTTP stream identifier.
  `generation` is the HTTP stream generation.
  It returns 1 when the HTTP command was accepted and 0 when the command was rejected.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-client-body-drain
    (let ([native (foreign-procedure "cspp_lws_http_client_body_drain" (uptr unsigned-64 unsigned-64 unsigned-64) int)])
      (lambda (context-handle connection-id stream-id generation)
        (pcheck ([natural? context-handle] [natural? connection-id] [natural? stream-id] [natural? generation])
                (require-optional-library 'ffi-lws-client-body-drain 'websockets)
                (native context-handle connection-id stream-id generation)))))
  (define ffi-lws-server-request-dequeue
    (foreign-procedure "cspp_lws_http_server_request_dequeue" (uptr) scheme-object))
  #|proc:ffi-lws-server-response-submit
  The `ffi-lws-server-response-submit` procedure calls the native websockets operation
  `cspp_lws_http_server_response_submit`.
  Parameters `context-handle`, `connection-id`, `stream-id`, `generation`, `status`, `headers`,
  `payload`, `final-chunk` are passed to the native operation in that order.
  `context-handle` is the native HTTP context handle.
  `connection-id` is the HTTP connection identifier.
  `stream-id` is the HTTP stream identifier.
  `generation` is the HTTP stream generation.
  `status` is a number.
  `headers` is a Scheme object.
  `payload` is a Scheme object.
  `final-chunk` is a number.
  It returns 1 when the HTTP command was accepted and 0 when the command was rejected.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-server-response-submit
    (let ([native (foreign-procedure "cspp_lws_http_server_response_submit" (uptr unsigned-64 unsigned-64 unsigned-64 int scheme-object scheme-object int) int)])
      (lambda (context-handle connection-id stream-id generation status headers payload final-chunk)
        (pcheck ([natural? context-handle] [natural? connection-id] [natural? stream-id] [natural? generation] [integer? status] [integer? final-chunk])
                (require-optional-library 'ffi-lws-server-response-submit 'websockets)
                (native context-handle connection-id stream-id generation status headers payload final-chunk)))))
  #|proc:ffi-lws-stream-cancel
  The `ffi-lws-stream-cancel` procedure calls the native websockets operation
  `cspp_lws_http_stream_cancel`.
  Parameters `context-handle`, `connection-id`, `stream-id`, `generation`, `status` are passed to
  the native operation in that order.
  `context-handle` is the native HTTP context handle.
  `connection-id` is the HTTP connection identifier.
  `stream-id` is the HTTP stream identifier.
  `generation` is the HTTP stream generation.
  `status` is a number.
  It returns 1 when the HTTP command was accepted and 0 when the command was rejected.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-stream-cancel
    (let ([native (foreign-procedure "cspp_lws_http_stream_cancel" (uptr unsigned-64 unsigned-64 unsigned-64 int) int)])
      (lambda (context-handle connection-id stream-id generation status)
        (pcheck ([natural? context-handle] [natural? connection-id] [natural? stream-id] [natural? generation] [integer? status])
                (require-optional-library 'ffi-lws-stream-cancel 'websockets)
                (native context-handle connection-id stream-id generation status)))))
  #|proc:ffi-lws-body-consumed
  The `ffi-lws-body-consumed` procedure calls the native websockets operation
  `cspp_lws_http_body_consumed`.
  Parameters `context-handle`, `connection-id`, `stream-id`, `generation`, `byte-count` are passed
  to the native operation in that order.
  `context-handle` is the native HTTP context handle.
  `connection-id` is the HTTP connection identifier.
  `stream-id` is the HTTP stream identifier.
  `generation` is the HTTP stream generation.
  `byte-count` is a native handle.
  It returns 1 when the HTTP command was accepted and 0 when the command was rejected.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-body-consumed
    (let ([native (foreign-procedure "cspp_lws_http_body_consumed" (uptr unsigned-64 unsigned-64 unsigned-64 uptr) int)])
      (lambda (context-handle connection-id stream-id generation byte-count)
        (pcheck ([natural? context-handle] [natural? connection-id] [natural? stream-id] [natural? generation] [natural? byte-count])
                (require-optional-library 'ffi-lws-body-consumed 'websockets)
                (native context-handle connection-id stream-id generation byte-count)))))
  #|proc:ffi-lws-context-inject-event
  The `ffi-lws-context-inject-event` procedure calls the native websockets operation
  `cspp_lws_http_inject_event`.
  Parameters `context-handle`, `tag`, `connection-id`, `stream-id`, `generation`, `status`,
  `payload`, `protocol`, `reusable`, `peer-h2-capacity`, `peer-h2-capacity-known`,
  `terminal-scope` are passed to the native operation in that order.
  `context-handle` is the native HTTP context handle.
  `tag` is a number.
  `connection-id` is the HTTP connection identifier.
  `stream-id` is the HTTP stream identifier.
  `generation` is the HTTP stream generation.
  `status` is a number.
  `payload` is a Scheme object.
  `protocol` is a number.
  `reusable` is a number.
  `peer-h2-capacity` is a number.
  `peer-h2-capacity-known` is a number.
  `terminal-scope` is a number.
  It returns 1 when the HTTP command was accepted and 0 when the command was rejected.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-context-inject-event
    (let ([native (foreign-procedure "cspp_lws_http_inject_event" (uptr int unsigned-64 unsigned-64 unsigned-64 int scheme-object
                             int int unsigned-32 int int) int)])
      (lambda (context-handle tag connection-id stream-id generation status payload protocol reusable peer-h2-capacity peer-h2-capacity-known terminal-scope)
        (pcheck ([natural? context-handle] [integer? tag] [natural? connection-id] [natural? stream-id] [natural? generation] [integer? status] [integer? protocol] [integer? reusable] [natural? peer-h2-capacity] [integer? peer-h2-capacity-known] [integer? terminal-scope])
                (require-optional-library 'ffi-lws-context-inject-event 'websockets)
                (native context-handle tag connection-id stream-id generation status payload protocol reusable peer-h2-capacity peer-h2-capacity-known terminal-scope)))))
  #|proc:ffi-lws-context-inject-poll
  The `ffi-lws-context-inject-poll` procedure calls the native websockets operation
  `cspp_lws_http_inject_poll`.
  Parameters `context-handle`, `operation`, `fd`, `events` are passed to the native operation in
  that order.
  `context-handle` is the native HTTP context handle.
  `operation` is a number.
  `fd` is a number.
  `events` is a number.
  It returns 1 when the HTTP command was accepted and 0 when the command was rejected.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-lws-context-inject-poll
    (let ([native (foreign-procedure "cspp_lws_http_inject_poll" (uptr int int int) int)])
      (lambda (context-handle operation fd events)
        (pcheck ([natural? context-handle] [integer? operation] [integer? fd] [integer? events])
                (require-optional-library 'ffi-lws-context-inject-poll 'websockets)
                (native context-handle operation fd events)))))

  (define valid-lws-status?
    (lambda (status)
      (and (vector? status)
           (= (vector-length status) 4)
           (boolean? (vector-ref status 0))
           (natural? (vector-ref status 1))
           (or (not (vector-ref status 2))
               (string? (vector-ref status 2)))
           (or (not (vector-ref status 3))
               (string? (vector-ref status 3))))))

  (define capability-name
    (lambda (capability)
      (cond
       [(fx= capability lws-cap-http1) 'http1]
       [(fx= capability lws-cap-http2) 'http2]
       [(fx= capability lws-cap-tls) 'tls]
       [(fx= capability lws-cap-socks5) 'socks5]
       [(fx= capability lws-cap-external-poll) 'external-poll]
       [else 'unknown])))

  (define capability-value?
    (lambda (capability)
      (and (fixnum? capability)
           (memv capability
                 (list lws-cap-http1
                       lws-cap-http2
                       lws-cap-tls
                       lws-cap-socks5
                       lws-cap-external-poll)))))

  (define capability-mask?
    (lambda (capability-mask)
      (and (fixnum? capability-mask)
           (fx>= capability-mask 0))))

  (define lws-context?
    (lambda (context)
      (and (natural? context) (positive? context))))

  (define positive-size?
    (lambda (size)
      (and (fixnum? size) (fxpositive? size))))

  (define file-descriptor?
    (lambda (descriptor)
      (and (fixnum? descriptor) (fx>= descriptor -1))))

  (define event-mask?
    (lambda (mask)
      (and (fixnum? mask) (fx>= mask 0))))

  (define port-number?
    (lambda (port)
      (and (fixnum? port) (fx> port 0) (fx<= port 65535))))

  (define lws-event-tag?
    (lambda (tag)
      (and (symbol? tag)
           (memq tag '(connected headers readable writable complete closed
                       failed reset goaway)))))

  (define lws-event-tag-value
    (lambda (tag)
      (case tag
        [(connected) 4]
        [(headers) 5]
        [(readable) 6]
        [(writable) 7]
        [(complete) 8]
        [(closed) 9]
        [(failed) 10]
        [(reset) 11]
        [(goaway) 12])))

  (define lws-observed-protocol-value
    (lambda (protocol)
      (case protocol
        [(http1) 1]
        [(http2) 2]
        [(unknown) 0]
        [else protocol])))

  (define lws-terminal-scope-value
    (lambda (scope)
      (case scope
        [(none) 0]
        [(stream) 1]
        [(connection) 2]
        [else scope])))

  (define lws-observed-protocol?
    (lambda (value)
      (or (fixnum? value) (memq value '(unknown http1 http2)))))

  (define lws-terminal-scope?
    (lambda (value)
      (or (fixnum? value) (memq value '(none stream connection)))))

  (define lws-poll-operation?
    (lambda (operation)
      (and (symbol? operation) (memq operation '(add change delete)))))

  (define lws-poll-operation-value
    (lambda (operation)
      (case operation
        [(add) 1]
        [(change) 2]
        [(delete) 3])))

  (define ffi-true?
    (lambda (result)
      (not (zero? result))))

  #|proc:lws-status
The `lws-status` procedure returns `#(available? capability-mask version error)` for the optional
libwebsockets HTTP runtime. Version and error are strings or `#f`.
|#
  (define-who lws-status
    (lambda ()
      (let ([status (ffi-lws-status)])
        (unless (valid-lws-status? status)
          (raise-net-error who 'internal-ffi
                           "malformed libwebsockets HTTP loader status" status))
        status)))

  #|proc:lws-capability?
The `lws-capability?` procedure returns whether `capability-mask` contains the single capability
bit `capability`.
|#
  (define-who lws-capability?
    (lambda (capability-mask capability)
      (pcheck ([capability-mask? capability-mask] [capability-value? capability])
        (not (fxzero? (fxand capability-mask capability))))))

  #|proc:lws-require-capability!
The `lws-require-capability!` procedure requires the single capability bit `capability`.
The `name` parameter identifies that capability in an error. It returns an unspecified value or
raises a network error when libwebsockets is unavailable or lacks the capability.
|#
  (define-who lws-require-capability!
    (lambda (capability name)
      (pcheck ([capability-value? capability] [symbol? name])
        (let* ([status (lws-status)]
               [available? (vector-ref status 0)]
               [capabilities (vector-ref status 1)])
          (unless available?
            (raise-net-error who 'unsupported
                             (or (vector-ref status 3)
                                 "libwebsockets HTTP is unavailable")
                             name))
          (unless (lws-capability? capabilities capability)
            (raise-net-error
             who 'unsupported
             (format "libwebsockets HTTP is missing capability ~a"
                     (capability-name capability))
             name))))))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Native nonblocking adapter
  ;;;;===----------------------------------------------------------------------===

  #|proc:lws-context-open
The `lws-context-open` procedure creates a native LWS context. `event-capacity` bounds queued
events, and `payload-capacity` bounds copied bytes per event. `tls-context` is zero or a native TLS
context handle. `proxy-address` and `proxy-port` select an immutable HTTP proxy policy. It returns a
native context handle.
|#
  (define-who lws-context-open
    (case-lambda
      [(event-capacity payload-capacity)
       (lws-context-open event-capacity payload-capacity 0 "" 0)]
      [(event-capacity payload-capacity tls-context)
       (lws-context-open event-capacity payload-capacity tls-context "" 0)]
      [(event-capacity payload-capacity tls-context proxy-address proxy-port)
       (pcheck ([positive-size? event-capacity payload-capacity]
                [natural? tls-context]
                [string? proxy-address]
                [natural? proxy-port])
        (unless (or (string=? proxy-address "") (port-number? proxy-port))
          (errorf who "expected a valid proxy port, given ~s" proxy-port))
        (lws-require-capability! lws-cap-http1 'http1)
        (lws-require-capability! lws-cap-external-poll 'external-poll)
        (let ([context (ffi-lws-context-open event-capacity payload-capacity tls-context
                                             proxy-address proxy-port)])
          (when (zero? context)
            (raise-net-error who 'resource "could not create libwebsockets HTTP context"
                             (vector event-capacity payload-capacity)))
          context))]))

  #|proc:lws-server-context-open
The `lws-server-context-open` procedure creates a listening LWS context. `event-capacity` bounds
events, `payload-capacity` bounds copied bytes, `interface-name` selects the local interface,
`port` is the listening port, and `tls-context` is zero or a native TLS context handle. It returns
a native context handle. Optional `version` is `h1` (the default) or `h2`; cleartext `h2`
uses prior knowledge and a TLS listener negotiates H2 using ALPN.
|#
  (define-who lws-server-context-open
    (case-lambda
      [(event-capacity payload-capacity interface-name port tls-context)
       (lws-server-context-open event-capacity payload-capacity interface-name port tls-context 'h1)]
      [(event-capacity payload-capacity interface-name port tls-context version)
      (pcheck ([positive-size? event-capacity payload-capacity]
               [string? interface-name]
               [port-number? port]
               [natural? tls-context] [symbol? version])
        (unless (memq version '(h1 h2)) (errorf who "version must be h1 or h2"))
        (when (eq? version 'h2) (lws-require-capability! lws-cap-http2 'http2))
        (lws-require-capability! lws-cap-http1 'http1)
        (lws-require-capability! lws-cap-external-poll 'external-poll)
        (let ([context (ffi-lws-server-context-open event-capacity payload-capacity
                                                    interface-name port tls-context
                                                    (and (eq? version 'h2) (zero? tls-context)))])
          (when (zero? context)
            (raise-net-error who 'resource "could not create LWS HTTP server context"
                             (vector interface-name port)))
          context))]))

  #|proc:lws-context-close
The `lws-context-close` procedure closes `context`, its LWS context, and its wakeup descriptors.
It returns an unspecified value.
|#
  (define-who lws-context-close
    (lambda (context)
      (pcheck ([lws-context? context])
        (ffi-lws-context-close context))))

  #|proc:lws-context-wakeup-fd
The `lws-context-wakeup-fd` procedure returns the readable wakeup descriptor owned by `context`.
|#
  (define-who lws-context-wakeup-fd
    (lambda (context)
      (pcheck ([lws-context? context])
        (ffi-lws-context-wakeup-fd context))))

  #|proc:lws-context-poll-snapshot
The `lws-context-poll-snapshot` procedure returns `#(#(descriptor events) ...)` for `context`.
The returned vector is a copy and can be retained by the caller.
|#
  (define-who lws-context-poll-snapshot
    (lambda (context)
      (pcheck ([lws-context? context])
        (ffi-lws-context-poll-snapshot context))))

  #|proc:lws-context-service-fd
The `lws-context-service-fd` procedure services `descriptor` in `context` using `ready-events`.
It returns the libwebsockets service result; servicing the wakeup descriptor returns zero.
|#
  (define-who lws-context-service-fd
    (lambda (context descriptor ready-events)
      (pcheck ([lws-context? context]
               [file-descriptor? descriptor]
               [event-mask? ready-events])
        (ffi-lws-context-service-fd context descriptor ready-events))))

  #|proc:lws-context-service
The `lws-context-service` procedure polls and services `context` once on its owner thread.
`maximum-timeout-ms` bounds the wait in milliseconds; LWS timers can shorten it.
It reuses native descriptor storage and lets other Scheme threads collect during the wait.
It returns the count of ready descriptors, or zero after timer service. Poll failures raise
a network condition. It must not run concurrently with another service call or context close.
|#
  (define-who lws-context-service
    (lambda (context maximum-timeout-ms)
      (pcheck ([lws-context? context] [natural? maximum-timeout-ms])
        (unless (<= maximum-timeout-ms 2147483647)
          (errorf who "timeout does not fit in a native integer: ~s" maximum-timeout-ms))
        (require-optional-library who 'websockets)
        (let ([ready (ffi-lws-context-service context maximum-timeout-ms)])
          (when (negative? ready)
            (raise-net-error who 'poll "native reactor poll failed" context))
          ready))))

  #|proc:lws-context-next-event
The `lws-context-next-event` procedure removes and returns the next copied event from `context`.
It returns `#f` when the bounded native event queue is empty.
|#
  (define-who lws-context-next-event
    (lambda (context)
      (pcheck ([lws-context? context])
        (ffi-lws-context-next-event context))))

  #|proc:lws-context-timeout-ms
The `lws-context-timeout-ms` procedure adjusts `maximum-timeout-ms` for LWS timers in `context`.
It returns the nonnegative timeout selected by libwebsockets.
|#
  (define-who lws-context-timeout-ms
    (lambda (context maximum-timeout-ms)
      (pcheck ([lws-context? context] [natural? maximum-timeout-ms])
        (ffi-lws-context-timeout-ms context maximum-timeout-ms))))

  #|proc:lws-context-wakeup
The `lws-context-wakeup` procedure signals the wakeup descriptor and LWS service for `context`.
It returns `#t` when the signal was accepted and `#f` on an operating-system error.
|#
  (define-who lws-context-wakeup
    (lambda (context)
      (pcheck ([lws-context? context])
        (ffi-true? (ffi-lws-context-wakeup context)))))

  #|proc:lws-context-pool-metrics
The `lws-context-pool-metrics` procedure returns native queue, byte, poll, and handle metrics for
`context`. The vector is intended for internal tests and allocation diagnostics.
|#
  (define-who lws-context-pool-metrics
    (lambda (context)
      (pcheck ([lws-context? context])
        (ffi-lws-context-pool-metrics context))))

  #|proc:lws-signal-open
The `lws-signal-open` procedure reserves a bounded per-operation wakeup signal for `context`.
It returns an opaque signal handle or zero when the context signal pool is exhausted.
|#
  (define-who lws-signal-open
    (lambda (context)
      (pcheck ([lws-context? context])
        (ffi-lws-signal-open context))))

  #|proc:lws-signal-fd
The `lws-signal-fd` procedure returns the readable descriptor for `signal`.
|#
  (define-who lws-signal-fd
    (lambda (signal)
      (pcheck ([natural? signal])
        (ffi-lws-signal-fd signal))))

  #|proc:lws-signal-notify
The `lws-signal-notify` procedure wakes waiters on `signal` and returns a boolean success flag.
|#
  (define lws-signal-notify
    (lambda (signal)
      (pcheck ([natural? signal])
        (ffi-true? (ffi-lws-signal-notify signal)))))

  #|proc:lws-signal-drain
The `lws-signal-drain` procedure consumes pending wakeup bytes from `signal`.
|#
  (define lws-signal-drain
    (lambda (signal)
      (pcheck ([natural? signal])
        (ffi-lws-signal-drain signal))))

  #|proc:lws-signal-close
The `lws-signal-close` procedure returns `signal` to its context-owned pool.
|#
  (define lws-signal-close
    (lambda (signal)
      (pcheck ([natural? signal])
        (ffi-lws-signal-close signal))))

  #|proc:lws-client-start
The `lws-client-start` procedure starts one HTTP stream in `context`. `connection-id`,
`stream-id`, and `generation` identify its lease. `address`, `port`, and `tls?` select the peer.
`method`, `host`, and `path` form the request. `alpn` selects the required HTTP protocol.
It returns whether LWS accepted the start.
|#
  (define-who lws-client-start
    (case-lambda
      [(context connection-id stream-id generation address port tls? method host path)
       (lws-client-start context connection-id stream-id generation address port tls? method host
                         path #vu8() #vu8() #f "http/1.1")]
      [(context connection-id stream-id generation address port tls? method host path headers
                initial-body has-body?)
       (lws-client-start context connection-id stream-id generation address port tls? method host
                         path headers initial-body has-body? "http/1.1")]
      [(context connection-id stream-id generation address port tls? method host path headers
                initial-body has-body? alpn)
       (pcheck ([lws-context? context]
                [natural? connection-id stream-id generation]
                [string? address method host path]
                [port-number? port]
                [boolean? tls?]
                [bytevector? headers initial-body]
                [boolean? has-body?]
                [string? alpn])
         (ffi-true?
          (ffi-lws-client-start context connection-id stream-id generation address port
                                (if tls? 1 0) method host path headers
                                initial-body (if has-body? 1 0) alpn)))]))

  #|proc:lws-client-acquire
The `lws-client-acquire` procedure reserves a logical stream on a physical connection identity.
`connection-id`, `stream-id`, and `generation` identify the lease. It returns acceptance.
|#
  (define-who lws-client-acquire
    (lambda (context connection-id stream-id generation)
      (pcheck ([lws-context? context]
               [natural? connection-id stream-id generation])
        (ffi-true? (ffi-lws-client-acquire context connection-id stream-id generation)))))

  #|proc:lws-client-release
The `lws-client-release` procedure releases a terminal logical stream lease. It returns whether
the matching terminal stream was released; active or stale leases are rejected.
|#
  (define-who lws-client-release
    (lambda (context connection-id stream-id generation)
      (pcheck ([lws-context? context]
               [natural? connection-id stream-id generation])
        (ffi-true? (ffi-lws-client-release context connection-id stream-id generation)))))

  #|proc:lws-client-body-submit
The `lws-client-body-submit` procedure queues copied `payload` bytes for the identified stream.
`final?` says that no later request bytes follow. It returns whether the bounded slot accepted it.
|#
  (define-who lws-client-body-submit
    (lambda (context connection-id stream-id generation payload final?)
      (pcheck ([lws-context? context]
               [natural? connection-id stream-id generation]
               [bytevector? payload]
               [boolean? final?])
        (ffi-true? (ffi-lws-client-body-submit context connection-id stream-id generation payload
                                               (if final? 1 0))))))

  #|proc:lws-client-body-drain
The `lws-client-body-drain` procedure asks LWS to drain available response bytes for the identified
stream. It returns whether draining started; copied readable events carry the resulting bytes.
|#
  (define-who lws-client-body-drain
    (lambda (context connection-id stream-id generation)
      (pcheck ([lws-context? context] [natural? connection-id stream-id generation])
        (ffi-true? (ffi-lws-client-body-drain context connection-id stream-id generation)))))

  #|proc:lws-server-request-dequeue
The `lws-server-request-dequeue` procedure removes the next copied server headers event from
`context`. It returns `#f` when no logical request is ready.
|#
  (define-who lws-server-request-dequeue
    (lambda (context)
      (pcheck ([lws-context? context])
        (ffi-lws-server-request-dequeue context))))

  #|proc:lws-server-response-submit
The `lws-server-response-submit` procedure queues `payload` for an identified server stream.
`status` is the HTTP status, and `final?` identifies the last body chunk. It returns acceptance.
Optional `headers` contains encoded response header metadata.
|#
  (define-who lws-server-response-submit
    (case-lambda
      [(context connection-id stream-id generation status payload final?)
       (lws-server-response-submit context connection-id stream-id generation status
                                   #vu8() payload final?)]
      [(context connection-id stream-id generation status headers payload final?)
      (pcheck ([lws-context? context]
               [natural? connection-id stream-id generation]
               [fixnum? status]
               [bytevector? headers payload]
               [boolean? final?])
        (ffi-true?
         (ffi-lws-server-response-submit context connection-id stream-id generation status
                                         headers payload (if final? 1 0))))]))

  #|proc:lws-stream-cancel
The `lws-stream-cancel` procedure resets the identified native stream and queues a reset event.
`status` supplies its cancellation metadata. It returns whether cancellation won the stream race.
|#
  (define-who lws-stream-cancel
    (lambda (context connection-id stream-id generation status)
      (pcheck ([lws-context? context]
               [natural? connection-id stream-id generation]
               [fixnum? status])
        (ffi-true?
         (ffi-lws-stream-cancel context connection-id stream-id generation status)))))

  #|proc:lws-body-consumed
The `lws-body-consumed` procedure acknowledges `byte-count` copied bytes for the identified stream.
`context` owns the stream selected by `connection-id`, `stream-id`, and `generation`.
Call it on the context owner; it resumes input and publishes deferred completion without waking
that same owner. Reactor clients enqueue acknowledgements with `lws-reactor-consume-body!`.
It returns whether the generation and byte count matched pending body data.
|#
  (define-who lws-body-consumed
    (lambda (context connection-id stream-id generation byte-count)
      (pcheck ([lws-context? context]
               [natural? connection-id stream-id generation byte-count])
        (ffi-true?
         (ffi-lws-body-consumed context connection-id stream-id generation byte-count)))))

  #|proc:lws-context-inject-event!
The `lws-context-inject-event!` procedure copies a fake callback event into `context`. The identity,
generation, status, and payload parameters form the event. Optional protocol, reusability, peer H2
capacity, and terminal scope values model observations made by LWS. It returns acceptance.
|#
  (define-who lws-context-inject-event!
    (case-lambda
      [(context event-tag connection-id stream-id generation status payload)
       (lws-context-inject-event! context event-tag connection-id stream-id generation status
                                   payload 0 #f 0 #f 0)]
      [(context event-tag connection-id stream-id generation status payload protocol reusable?
                peer-h2-capacity peer-h2-capacity-known terminal-scope)
       (pcheck ([lws-context? context]
                [lws-event-tag? event-tag]
                [natural? connection-id stream-id generation]
                [fixnum? status peer-h2-capacity]
                [lws-observed-protocol? protocol]
                [lws-terminal-scope? terminal-scope]
                [bytevector? payload]
                [boolean? reusable? peer-h2-capacity-known])
         (ffi-true?
          (ffi-lws-context-inject-event context (lws-event-tag-value event-tag) connection-id
                                        stream-id generation status payload
                                        (lws-observed-protocol-value protocol)
                                        (if reusable? 1 0) peer-h2-capacity
                                        (if peer-h2-capacity-known 1 0)
                                        (lws-terminal-scope-value terminal-scope))))]))

  #|proc:lws-context-inject-poll!
The `lws-context-inject-poll!` procedure applies fake poll `operation` to `descriptor` in `context`.
`events` is the replacement poll mask. It returns whether the bounded poll/event pools accepted it.
|#
  (define-who lws-context-inject-poll!
    (lambda (context operation descriptor events)
      (pcheck ([lws-context? context]
               [lws-poll-operation? operation]
               [file-descriptor? descriptor]
               [event-mask? events])
        (ffi-true?
         (ffi-lws-context-inject-poll context (lws-poll-operation-value operation)
                                      descriptor events)))))
  )
