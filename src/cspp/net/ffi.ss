(library (cspp net ffi)
  (export net-af-inet
          ffi-optional-library-info
          ffi-zlib-stream-open
          ffi-zlib-stream-process
          ffi-zlib-stream-close
          net-af-inet6
          net-af-unix
          net-pollin
          net-pollout
          net-pollpri
          net-pollerr
          net-pollhup
          net-pollnval
          net-sock-stream
          net-sock-datagram
          net-sock-seqpacket
          net-shut-read
          net-shut-write
          net-shut-read/write
          ffi-net-socket-open
          ffi-net-socket-close
          ffi-net-socket-dup
          ffi-net-socket-bind
          ffi-net-socket-listen
          ffi-net-socket-connect
          ffi-net-socket-connect-status
          ffi-net-socket-accept
          ffi-net-socket-shutdown
          ffi-net-socket-send
          ffi-net-socket-recv
          ffi-net-socket-recv-into
          ffi-net-socket-send-to
          ffi-net-socket-recv-from
          ffi-net-socket-recv-from-into
          ffi-net-socket-local-address
          ffi-net-socket-peer-address
          ffi-net-socket-set-blocking
          ffi-net-socket-get-blocking
          ffi-net-socket-wait
          ffi-net-socket-set-option
          ffi-net-socket-get-option
          ffi-net-poll
          ffi-net-resolve-addresses
          ffi-net-service->port
          ffi-net-dns-start
          ffi-net-dns-advance
          ffi-net-dns-cancel
          ffi-net-dns-close
          ffi-net-idna->ascii
          ffi-net-idna->unicode
          ffi-net-address->name
          ffi-net-ftp-list
          ffi-net-ftp-stat
          ffi-net-ftp-download
          ffi-net-ftp-upload
          ffi-net-ftp-command
          ffi-net-ftp-rename
          ffi-net-ftp-transfer-start
          ffi-net-ftp-transfer-step
          ffi-net-ftp-transfer-cancel
          ffi-net-ftp-transfer-close
          ffi-net-ftp-session-open
          ffi-net-ftp-session-close
          ffi-net-ftp-file-open
          ffi-net-ftp-file-step
          ffi-net-ftp-file-read
          ffi-net-ftp-file-read-into
          ffi-net-ftp-file-write
          ffi-net-ftp-file-finish
          ffi-net-ftp-file-cancel
          ffi-net-ftp-file-close
          ffi-net-ssh-open
          ffi-net-ssh-close
          ffi-net-ssh-session-fd
          ffi-net-ssh-auth-password
          ffi-net-ssh-auth-publickey-auto
          ffi-net-ssh-auth-publickey
          ffi-net-ssh-auth-keyboard-interactive-step
          ffi-net-ssh-auth-keyboard-interactive-answer
          ffi-net-ssh-auth-agent
          ffi-net-ssh-auth-agent-identity
          ffi-net-ssh-known-host-check
          ffi-net-ssh-known-host-update
          ffi-net-ssh-known-host-export
          ffi-net-ssh-channel-open
          ffi-net-ssh-channel-forward-start
          ffi-net-ssh-channel-forward-step
          ffi-net-ssh-channel-forward-discard
          ffi-net-ssh-channel-open-forward
          ffi-net-ssh-remote-forward-listen
          ffi-net-ssh-remote-forward-accept
          ffi-net-ssh-remote-forward-cancel
          ffi-net-ssh-channel-close
          ffi-net-ssh-channel-send-eof
          ffi-net-ssh-channel-request-exec
          ffi-net-ssh-channel-request-shell
          ffi-net-ssh-channel-request-pty
          ffi-net-ssh-channel-request-pty-size
          ffi-ssh-terminal-size
          ffi-ssh-terminal-raw-start
          ffi-ssh-terminal-raw-close
          ffi-net-ssh-channel-request-environment
          ffi-net-ssh-channel-request-subsystem
          ffi-net-ssh-channel-read
          ffi-net-ssh-channel-read-into
          ffi-net-ssh-channel-write
          ffi-net-ssh-channel-exit-status
          ffi-net-sftp-open
          ffi-net-sftp-close
          ffi-net-scp-download-file
          ffi-net-scp-upload-file
          ffi-net-scp-stat
          ffi-net-scp-download-directory
          ffi-net-scp-upload-directory
          ffi-net-scp-transfer-start
          ffi-net-scp-transfer-start-many
          ffi-net-scp-transfer-step
          ffi-net-scp-transfer-cancel
          ffi-net-scp-transfer-close
          ffi-net-sftp-list
          ffi-net-sftp-stat
          ffi-net-sftp-open-directory
          ffi-net-sftp-read-directory
          ffi-net-sftp-close-directory
          ffi-net-sftp-chmod
          ffi-net-sftp-chown
          ffi-net-sftp-utimes
          ffi-net-sftp-symlink
          ffi-net-sftp-readlink
          ffi-net-sftp-seek
          ffi-net-sftp-delete
          ffi-net-sftp-mkdir
          ffi-net-sftp-rmdir
          ffi-net-sftp-rename
          ffi-net-sftp-open-file
          ffi-net-sftp-close-file
          ffi-net-sftp-read
          ffi-net-sftp-read-into
          ffi-net-sftp-write
          ffi-net-websocket-listen
          ffi-net-websocket-server-close
          ffi-net-websocket-accept
          ffi-net-websocket-connect
          ffi-net-websocket-connect-step
          ffi-net-websocket-poll-targets
          ffi-net-websocket-close
          ffi-net-websocket-close-with-reason
          ffi-net-websocket-state
          ffi-net-websocket-cancel-send
          ffi-net-websocket-send
          ffi-net-websocket-send-fragment
          ffi-net-websocket-recv
          ffi-net-grpc-channel-open
          ffi-net-grpc-channel-open-tls
          ffi-net-grpc-channel-close
          ffi-net-grpc-server-open
          ffi-net-grpc-server-open-tls
          ffi-net-grpc-server-close
          ffi-net-grpc-unary-call
          ffi-net-grpc-unary-start
          ffi-net-grpc-unary-poll
          ffi-net-grpc-unary-close
          ffi-net-grpc-stream-open
          ffi-net-grpc-stream-open-start
          ffi-net-grpc-stream-open-poll
          ffi-net-grpc-stream-send
          ffi-net-grpc-stream-recv
          ffi-net-grpc-stream-close-send
          ffi-net-grpc-stream-finish
          ffi-net-grpc-stream-close
          ffi-net-grpc-server-request
          ffi-net-grpc-server-request-stream
          ffi-net-grpc-server-respond
          ffi-net-grpc-capabilities
          ffi-net-grpc-driver-fd
          ffi-net-grpc-driver-drain
          ffi-net-sftp-flag-read
          ffi-net-sftp-flag-write
          ffi-net-sftp-flag-read/write
          ffi-net-sftp-flag-append
          ffi-net-sftp-flag-create
          ffi-net-sftp-flag-truncate
          ffi-net-sftp-flag-exclusive
          ffi-net-sftp-flag-text
          ffi-net-tls-load-error
          ffi-net-tls-context-create
          ffi-net-tls-context-free
          ffi-net-tls-context-load-ca-file
          ffi-net-tls-context-load-ca-path
          ffi-net-tls-context-load-default-ca
          ffi-net-tls-context-load-cert-file
          ffi-net-tls-context-load-cert-bytes
          ffi-net-tls-context-load-key-file
          ffi-net-tls-context-load-key-bytes
          ffi-net-tls-context-check-key
          ffi-net-tls-context-set-verify
          ffi-net-tls-context-set-alpn
          ffi-net-tls-context-set-policy
          ffi-net-tls-context-import-session
          ffi-net-tls-context-enable-sni
          ffi-net-tls-connect
          ffi-net-tls-accept
          ffi-net-tls-handshake-step
          ffi-net-tls-close
          ffi-net-tls-read
          ffi-net-tls-read-into
          ffi-net-tls-write
          ffi-net-tls-shutdown
          ffi-net-tls-protocol-version
          ffi-net-tls-negotiated-alpn
          ffi-net-tls-cipher-name
          ffi-net-tls-verified
          ffi-net-tls-session-export
          ffi-net-tls-session-reused
          ffi-net-tls-session-select-context
          ffi-net-tls-stapled-ocsp
          ffi-net-tls-ocsp-result
          ffi-net-tls-peer-certificate-der
          ffi-net-tls-peer-certificate-chain-der)
  (import (cspp utils) (cspp private optional-library) (cspp chez)
          (cspp private common))

  (define ffi-optional-library-info
    (foreign-procedure "cspp_optional_library_info" (string) scheme-object))
  #|proc:ffi-zlib-stream-open
  The `ffi-zlib-stream-open` procedure calls the native zlib operation `cspp_zlib_stream_open`.
  Parameters `compress`, `gzip` are passed to the native operation in that order.
  `compress` is a number.
  `gzip` is a number.
  It returns an owned native handle, or zero when the resource could not be created.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-zlib-stream-open
    (let ([native (foreign-procedure "cspp_zlib_stream_open" (int int) uptr)])
      (lambda (compress gzip)
        (pcheck ([integer? compress] [integer? gzip])
                (require-optional-library 'ffi-zlib-stream-open 'zlib)
                (native compress gzip)))))
  (define ffi-zlib-stream-process
    (foreign-procedure "cspp_zlib_stream_process"
                       (uptr scheme-object int int int int) scheme-object))
  #|proc:ffi-zlib-stream-close
  The `ffi-zlib-stream-close` procedure calls the native zlib operation
  `cspp_zlib_stream_close`.
  Parameters `handle` are passed to the native operation in that order.
  `handle` is the native resource handle.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-zlib-stream-close
    (let ([native (foreign-procedure "cspp_zlib_stream_close" (uptr) void)])
      (lambda (handle)
        (pcheck ([natural? handle])
                (require-optional-library 'ffi-zlib-stream-close 'zlib)
                (native handle)))))

  (define net-af-inet (foreign-procedure "cspp_net_af_inet" () int))
  (define net-af-inet6 (foreign-procedure "cspp_net_af_inet6" () int))
  (define net-af-unix (foreign-procedure "cspp_net_af_unix" () int))
  (define net-pollin (foreign-procedure "cspp_net_pollin" () int))
  (define net-pollout (foreign-procedure "cspp_net_pollout" () int))
  (define net-pollpri (foreign-procedure "cspp_net_pollpri" () int))
  (define net-pollerr (foreign-procedure "cspp_net_pollerr" () int))
  (define net-pollhup (foreign-procedure "cspp_net_pollhup" () int))
  (define net-pollnval (foreign-procedure "cspp_net_pollnval" () int))

  (define net-sock-stream (foreign-procedure "cspp_net_sock_stream" () int))
  (define net-sock-datagram (foreign-procedure "cspp_net_sock_datagram" () int))
  (define net-sock-seqpacket (foreign-procedure "cspp_net_sock_seqpacket" () int))

  (define net-shut-read (foreign-procedure "cspp_net_shut_rd" () int))
  (define net-shut-write (foreign-procedure "cspp_net_shut_wr" () int))
  (define net-shut-read/write (foreign-procedure "cspp_net_shut_rdwr" () int))

  (define ffi-net-socket-open
    (foreign-procedure "cspp_net_socket_open" (int int int) scheme-object))
  (define ffi-net-socket-close
    (foreign-procedure "cspp_net_socket_close" (int) scheme-object))
  (define ffi-net-socket-dup
    (foreign-procedure "cspp_net_socket_dup" (int) scheme-object))
  (define ffi-net-socket-bind
    (foreign-procedure "cspp_net_socket_bind" (int int string int string) scheme-object))
  (define ffi-net-socket-listen
    (foreign-procedure "cspp_net_socket_listen" (int int) scheme-object))
  (define ffi-net-socket-connect
    (foreign-procedure "cspp_net_socket_connect" (int int string int string) scheme-object))
  (define ffi-net-socket-connect-status
    (foreign-procedure "cspp_net_socket_connect_status" (int) scheme-object))
  (define ffi-net-socket-accept
    (foreign-procedure "cspp_net_socket_accept" (int int) scheme-object))
  (define ffi-net-socket-shutdown
    (foreign-procedure "cspp_net_socket_shutdown" (int int) scheme-object))
  (define ffi-net-socket-send
    (foreign-procedure "cspp_net_socket_send" (int ptr int int int) scheme-object))
  (define ffi-net-socket-recv
    (foreign-procedure "cspp_net_socket_recv" (int int int) scheme-object))
  (define ffi-net-socket-recv-into
    (foreign-procedure "cspp_net_socket_recv_into" (int ptr int int int) scheme-object))
  (define ffi-net-socket-send-to
    (foreign-procedure "cspp_net_socket_send_to"
                       (int ptr int int int string int string int) scheme-object))
  (define ffi-net-socket-recv-from
    (foreign-procedure "cspp_net_socket_recv_from" (int int int) scheme-object))
  (define ffi-net-socket-recv-from-into
    (foreign-procedure "cspp_net_socket_recv_from_into"
                       (int ptr int int int) scheme-object))
  (define ffi-net-socket-local-address
    (foreign-procedure "cspp_net_socket_local_address" (int) scheme-object))
  (define ffi-net-socket-peer-address
    (foreign-procedure "cspp_net_socket_peer_address" (int) scheme-object))
  (define ffi-net-socket-set-blocking
    (foreign-procedure "cspp_net_socket_set_blocking" (int int) scheme-object))
  (define ffi-net-socket-get-blocking
    (foreign-procedure "cspp_net_socket_get_blocking" (int) scheme-object))
  (define ffi-net-socket-wait
    (foreign-procedure "cspp_net_socket_wait" (int int int) scheme-object))
  (define ffi-net-socket-set-option
    (foreign-procedure "cspp_net_socket_set_option" (int string scheme-object) scheme-object))
  (define ffi-net-socket-get-option
    (foreign-procedure "cspp_net_socket_get_option" (int string) scheme-object))
  (define ffi-net-poll
    (foreign-procedure "cspp_net_poll" (scheme-object int) scheme-object))
  (define ffi-net-resolve-addresses
    (foreign-procedure "cspp_net_resolve_addresses" (string int int int) scheme-object))
  (define ffi-net-service->port
    (foreign-procedure "cspp_net_service_to_port" (string int) scheme-object))
  #|proc:ffi-net-dns-start
  The `ffi-net-dns-start` procedure starts asynchronous address resolution for string `name`.
  `family` is zero for either IP family, `net-af-inet` for IPv4, or `net-af-inet6` for IPv6.
  `timeout-ms` is the per-attempt resolver timeout in milliseconds.
  It returns an owned DNS-operation handle, or zero on allocation or initialization failure.
  It raises an error when c-ares is unavailable.
  |#
  (define ffi-net-dns-start
    (let ([native (foreign-procedure "cspp_net_dns_start" (string int int) uptr)])
      (lambda (name family timeout-ms)
        (pcheck ([string? name] [integer? family] [integer? timeout-ms])
                (require-optional-library 'ffi-net-dns-start 'cares)
                (native name family timeout-ms)))))
  (define ffi-net-dns-advance
    (foreign-procedure "cspp_net_dns_advance" (uptr) scheme-object))
  (define ffi-net-dns-cancel
    (foreign-procedure "cspp_net_dns_cancel" (uptr) scheme-object))
  #|proc:ffi-net-dns-close
  The `ffi-net-dns-close` procedure calls the native cares operation `cspp_net_dns_close`.
  Parameters `handle` are passed to the native operation in that order.
  `handle` is the native resource handle.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-net-dns-close
    (let ([native (foreign-procedure "cspp_net_dns_close" (uptr) void)])
      (lambda (handle)
        (pcheck ([natural? handle])
                (require-optional-library 'ffi-net-dns-close 'cares)
                (native handle)))))
  (define ffi-net-idna->ascii
    (foreign-procedure "cspp_net_idna_to_ascii" (string) scheme-object))
  (define ffi-net-idna->unicode
    (foreign-procedure "cspp_net_idna_to_unicode" (string) scheme-object))
  (define ffi-net-address->name
    (foreign-procedure "cspp_net_address_to_name" (int string int string) scheme-object))
  (define ffi-net-ftp-list
    (foreign-procedure "cspp_net_ftp_list"
                       (string string string int int int int int)
                       scheme-object))
  (define ffi-net-ftp-stat
    (foreign-procedure "cspp_net_ftp_stat"
                       (string string string int int int int int string)
                       scheme-object))
  (define ffi-net-ftp-download
    (foreign-procedure "cspp_net_ftp_download"
                       (string string string string int int int int int)
                       scheme-object))
  (define ffi-net-ftp-upload
    (foreign-procedure "cspp_net_ftp_upload"
                       (string string string string int int int int int)
                       scheme-object))
  (define ffi-net-ftp-command
    (foreign-procedure "cspp_net_ftp_command"
                       (string string string int int int int int string)
                       scheme-object))
  (define ffi-net-ftp-rename
    (foreign-procedure "cspp_net_ftp_rename"
                       (string string string int int int int int string string)
                       scheme-object))
  (define ffi-net-ftp-transfer-start
    (foreign-procedure "cspp_net_ftp_transfer_start"
                       (int string string string string int int int int int)
                       scheme-object))
  (define ffi-net-ftp-transfer-step
    (foreign-procedure "cspp_net_ftp_transfer_step" (uptr scheme-object int) scheme-object))
  (define ffi-net-ftp-transfer-cancel
    (foreign-procedure "cspp_net_ftp_transfer_cancel" (uptr) scheme-object))
  #|proc:ffi-net-ftp-transfer-close
  The `ffi-net-ftp-transfer-close` procedure calls the native curl operation
  `cspp_net_ftp_transfer_close`.
  Parameters `handle` are passed to the native operation in that order.
  `handle` is the native resource handle.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-net-ftp-transfer-close
    (let ([native (foreign-procedure "cspp_net_ftp_transfer_close" (uptr) void)])
      (lambda (handle)
        (pcheck ([natural? handle])
                (require-optional-library 'ffi-net-ftp-transfer-close 'curl)
                (native handle)))))
  (define ffi-net-ftp-session-open
    (foreign-procedure "cspp_net_ftp_session_open" () scheme-object))
  (define ffi-net-ftp-session-close
    (foreign-procedure "cspp_net_ftp_session_close" (uptr) scheme-object))
  (define ffi-net-ftp-file-open
    (foreign-procedure "cspp_net_ftp_file_open"
                       (uptr int string string string int int int int int iptr)
                       scheme-object))
  (define ffi-net-ftp-file-step
    (foreign-procedure "cspp_net_ftp_file_step" (uptr scheme-object int) scheme-object))
  (define ffi-net-ftp-file-read
    (foreign-procedure "cspp_net_ftp_file_read" (uptr int) scheme-object))
  (define ffi-net-ftp-file-read-into
    (foreign-procedure "cspp_net_ftp_file_read_into"
                       (uptr scheme-object int int int)
                       scheme-object))
  (define ffi-net-ftp-file-write
    (foreign-procedure "cspp_net_ftp_file_write"
                       (uptr scheme-object int int int)
                       scheme-object))
  (define ffi-net-ftp-file-finish
    (foreign-procedure "cspp_net_ftp_file_finish" (uptr) scheme-object))
  (define ffi-net-ftp-file-cancel
    (foreign-procedure "cspp_net_ftp_file_cancel" (uptr) scheme-object))
  #|proc:ffi-net-ftp-file-close
  The `ffi-net-ftp-file-close` procedure calls the native curl operation
  `cspp_net_ftp_file_close`.
  Parameters `handle` are passed to the native operation in that order.
  `handle` is the native resource handle.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-net-ftp-file-close
    (let ([native (foreign-procedure "cspp_net_ftp_file_close" (uptr) void)])
      (lambda (handle)
        (pcheck ([natural? handle])
                (require-optional-library 'ffi-net-ftp-file-close 'curl)
                (native handle)))))
  (define ffi-net-ssh-open
    (foreign-procedure "cspp_net_ssh_open" (string int string int int) scheme-object))
  (define ffi-net-ssh-close
    (foreign-procedure "cspp_net_ssh_close" (uptr) scheme-object))
  (define ffi-net-ssh-session-fd
    (foreign-procedure "cspp_net_ssh_session_fd" (uptr) scheme-object))
  (define ffi-net-ssh-auth-password
    (foreign-procedure "cspp_net_ssh_auth_password" (uptr string string) scheme-object))
  (define ffi-net-ssh-auth-publickey-auto
    (foreign-procedure "cspp_net_ssh_auth_publickey_auto" (uptr string string) scheme-object))
  (define ffi-net-ssh-auth-publickey
    (foreign-procedure "cspp_net_ssh_auth_publickey"
                       (uptr string string string string) scheme-object))
  (define ffi-net-ssh-auth-keyboard-interactive-step
    (foreign-procedure "cspp_net_ssh_auth_keyboard_interactive_step"
                       (uptr string) scheme-object))
  (define ffi-net-ssh-auth-keyboard-interactive-answer
    (foreign-procedure "cspp_net_ssh_auth_keyboard_interactive_answer"
                       (uptr int string) scheme-object))
  (define ffi-net-ssh-auth-agent
    (foreign-procedure "cspp_net_ssh_auth_agent" (uptr string) scheme-object))
  (define ffi-net-ssh-auth-agent-identity
    (foreign-procedure "cspp_net_ssh_auth_agent_identity"
                       (uptr string string) scheme-object))
  (define ffi-net-ssh-known-host-check
    (foreign-procedure "cspp_net_ssh_known_host_check" (uptr string) scheme-object))
  (define ffi-net-ssh-known-host-update
    (foreign-procedure "cspp_net_ssh_known_host_update" (uptr string) scheme-object))
  (define ffi-net-ssh-known-host-export
    (foreign-procedure "cspp_net_ssh_known_host_export" (uptr) scheme-object))
  (define ffi-net-ssh-channel-open
    (foreign-procedure "cspp_net_ssh_channel_open" (uptr int) scheme-object))
  (define ffi-net-ssh-channel-open-forward
    (foreign-procedure "cspp_net_ssh_channel_open_forward"
                       (uptr string int string int int) scheme-object))
  (define ffi-net-ssh-channel-forward-start
    (foreign-procedure "cspp_net_ssh_channel_forward_start" (uptr) scheme-object))
  (define ffi-net-ssh-channel-forward-step
    (foreign-procedure "cspp_net_ssh_channel_forward_step"
                       (uptr string int string int) scheme-object))
  (define ffi-net-ssh-channel-forward-discard
    (foreign-procedure "cspp_net_ssh_channel_forward_discard" (uptr) void))
  (define ffi-net-ssh-remote-forward-listen
    (foreign-procedure "cspp_net_ssh_remote_forward_listen"
                       (uptr string int) scheme-object))
  (define ffi-net-ssh-remote-forward-accept
    (foreign-procedure "cspp_net_ssh_remote_forward_accept" (uptr) scheme-object))
  (define ffi-net-ssh-remote-forward-cancel
    (foreign-procedure "cspp_net_ssh_remote_forward_cancel"
                       (uptr string int) scheme-object))
  (define ffi-net-ssh-channel-send-eof
    (foreign-procedure "cspp_net_ssh_channel_send_eof" (uptr) scheme-object))
  (define ffi-net-ssh-channel-close
    (foreign-procedure "cspp_net_ssh_channel_close" (uptr) scheme-object))
  (define ffi-net-ssh-channel-request-exec
    (foreign-procedure "cspp_net_ssh_channel_request_exec" (uptr string int) scheme-object))
  (define ffi-net-ssh-channel-request-shell
    (foreign-procedure "cspp_net_ssh_channel_request_shell" (uptr int) scheme-object))
  (define ffi-net-ssh-channel-request-pty
    (foreign-procedure "cspp_net_ssh_channel_request_pty" (uptr int) scheme-object))
  #|proc:ffi-net-ssh-channel-request-pty-size
  Request a PTY, or resize it when `resize?` is true, on native `channel-handle`.
  `terminal` is its TERM value, `columns` and `rows` its size, and `timeout-ms` the wait bound.
  Returns native success or an error status.
  |#
  (define ffi-net-ssh-channel-request-pty-size
    (let ([native (foreign-procedure "cspp_net_ssh_channel_request_pty_size"
                                    (uptr string int int boolean int) scheme-object)])
      (lambda (channel-handle terminal columns rows resize? timeout-ms)
        (pcheck ([natural? channel-handle] [string? terminal] [fixnum? columns rows timeout-ms]
                 [boolean? resize?])
          (require-optional-library 'ffi-net-ssh-channel-request-pty-size 'ssh)
          (native channel-handle terminal columns rows resize? timeout-ms)))))

  #|proc:ffi-ssh-terminal-size
  Return local `descriptor`'s #(columns rows), #f for a nonterminal, or an error status.
  |#
  (define ffi-ssh-terminal-size
    (let ([native (foreign-procedure "cspp_ssh_terminal_size" (int) scheme-object)])
      (lambda (descriptor) (pcheck ([fixnum? descriptor]) (native descriptor)))))

  #|proc:ffi-ssh-terminal-raw-start
  Put local `descriptor` in raw mode, returning an ok status with an owned restoration handle,
  or an error status. Release that handle with `ffi-ssh-terminal-raw-close`.
  |#
  (define ffi-ssh-terminal-raw-start
    (let ([native (foreign-procedure "cspp_ssh_terminal_raw_start" (int) scheme-object)])
      (lambda (descriptor) (pcheck ([fixnum? descriptor]) (native descriptor)))))

  #|proc:ffi-ssh-terminal-raw-close
  Restore and release native `terminal-handle`. Return zero on success or an errno integer.
  |#
  (define ffi-ssh-terminal-raw-close
    (let ([native (foreign-procedure "cspp_ssh_terminal_raw_close" (uptr) int)])
      (lambda (terminal-handle) (pcheck ([natural? terminal-handle]) (native terminal-handle)))))
  (define ffi-net-ssh-channel-request-environment
    (foreign-procedure "cspp_net_ssh_channel_request_environment" (uptr string string)
                       scheme-object))
  (define ffi-net-ssh-channel-request-subsystem
    (foreign-procedure "cspp_net_ssh_channel_request_subsystem" (uptr string) scheme-object))
  (define ffi-net-ssh-channel-read
    (foreign-procedure "cspp_net_ssh_channel_read" (uptr int int int int) scheme-object))
  (define ffi-net-ssh-channel-read-into
    (foreign-procedure "cspp_net_ssh_channel_read_into" (uptr ptr int int int int int) scheme-object))
  (define ffi-net-ssh-channel-write
    (foreign-procedure "cspp_net_ssh_channel_write" (uptr ptr int int int int) scheme-object))
  (define ffi-net-ssh-channel-exit-status
    (foreign-procedure "cspp_net_ssh_channel_exit_status" (uptr) scheme-object))
  (define ffi-net-sftp-open
    (foreign-procedure "cspp_net_sftp_open" (uptr) scheme-object))
  (define ffi-net-sftp-close
    (foreign-procedure "cspp_net_sftp_close" (uptr) scheme-object))
  (define ffi-net-scp-download-file
    (foreign-procedure "cspp_net_scp_download_file" (uptr string string int) scheme-object))
  (define ffi-net-scp-upload-file
    (foreign-procedure "cspp_net_scp_upload_file" (uptr string string int) scheme-object))
  (define ffi-net-scp-stat
    (foreign-procedure "cspp_net_scp_stat" (uptr string) scheme-object))
  (define ffi-net-scp-download-directory
    (foreign-procedure "cspp_net_scp_download_directory" (uptr string string int) scheme-object))
  (define ffi-net-scp-upload-directory
    (foreign-procedure "cspp_net_scp_upload_directory" (uptr string string int) scheme-object))
  (define ffi-net-scp-transfer-start
    (foreign-procedure "cspp_net_scp_transfer_start" (uptr int string string) scheme-object))
  #|proc:ffi-net-scp-transfer-start-many
  The `ffi-net-scp-transfer-start-many` procedure starts a bounded SCP sibling-file batch.
  `session-handle` identifies an authenticated native SSH session. `direction` is zero for
  download or one for upload. `local-directory` and `remote-directory` contain the files.
  `encoded-names` is a bytevector of up to 32 UTF-8 child names, each terminated by NUL.
  It returns an `ok` status containing a native transfer handle, or an error status. Release
  the handle with `ffi-net-scp-transfer-close`; step and cancel use the ordinary transfer APIs.
  |#
  (define ffi-net-scp-transfer-start-many
    (let ([native (foreign-procedure "cspp_net_scp_transfer_start_many"
                                    (uptr int string string scheme-object) scheme-object)])
      (lambda (session-handle direction local-directory remote-directory encoded-names)
        (pcheck ([natural? session-handle] [fixnum? direction]
                 [string? local-directory remote-directory] [bytevector? encoded-names])
          (require-optional-library 'ffi-net-scp-transfer-start-many 'ssh)
          (native session-handle direction local-directory remote-directory encoded-names)))))
  (define ffi-net-scp-transfer-step
    (foreign-procedure "cspp_net_scp_transfer_step" (uptr) scheme-object))
  (define ffi-net-scp-transfer-cancel
    (foreign-procedure "cspp_net_scp_transfer_cancel" (uptr) scheme-object))
  #|proc:ffi-net-scp-transfer-close
  The `ffi-net-scp-transfer-close` procedure calls the native ssh operation
  `cspp_net_scp_transfer_close`.
  Parameters `handle` are passed to the native operation in that order.
  `handle` is the native resource handle.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-net-scp-transfer-close
    (let ([native (foreign-procedure "cspp_net_scp_transfer_close" (uptr) void)])
      (lambda (handle)
        (pcheck ([natural? handle])
                (require-optional-library 'ffi-net-scp-transfer-close 'ssh)
                (native handle)))))
  (define ffi-net-sftp-list
    (foreign-procedure "cspp_net_sftp_list" (uptr string) scheme-object))
  (define ffi-net-sftp-stat
    (foreign-procedure "cspp_net_sftp_stat" (uptr string) scheme-object))
  (define ffi-net-sftp-open-directory
    (foreign-procedure "cspp_net_sftp_open_directory" (uptr string) scheme-object))
  (define ffi-net-sftp-read-directory
    (foreign-procedure "cspp_net_sftp_read_directory" (uptr) scheme-object))
  (define ffi-net-sftp-close-directory
    (foreign-procedure "cspp_net_sftp_close_directory" (uptr) scheme-object))
  (define ffi-net-sftp-chmod
    (foreign-procedure "cspp_net_sftp_chmod" (uptr string unsigned) scheme-object))
  (define ffi-net-sftp-chown
    (foreign-procedure "cspp_net_sftp_chown" (uptr string unsigned unsigned) scheme-object))
  (define ffi-net-sftp-utimes
    (foreign-procedure "cspp_net_sftp_utimes" (uptr string integer-64 integer-64)
                       scheme-object))
  (define ffi-net-sftp-symlink
    (foreign-procedure "cspp_net_sftp_symlink" (uptr string string) scheme-object))
  (define ffi-net-sftp-readlink
    (foreign-procedure "cspp_net_sftp_readlink" (uptr string) scheme-object))
  (define ffi-net-sftp-seek
    (foreign-procedure "cspp_net_sftp_seek" (uptr unsigned-64) scheme-object))
  (define ffi-net-sftp-delete
    (foreign-procedure "cspp_net_sftp_delete" (uptr string) scheme-object))
  (define ffi-net-sftp-mkdir
    (foreign-procedure "cspp_net_sftp_mkdir" (uptr string int) scheme-object))
  (define ffi-net-sftp-rmdir
    (foreign-procedure "cspp_net_sftp_rmdir" (uptr string) scheme-object))
  (define ffi-net-sftp-rename
    (foreign-procedure "cspp_net_sftp_rename" (uptr string string) scheme-object))
  (define ffi-net-sftp-open-file
    (foreign-procedure "cspp_net_sftp_open_file" (uptr string int int) scheme-object))
  (define ffi-net-sftp-close-file
    (foreign-procedure "cspp_net_sftp_close_file" (uptr) scheme-object))
  (define ffi-net-sftp-read
    (foreign-procedure "cspp_net_sftp_read" (uptr int int int) scheme-object))
  (define ffi-net-sftp-read-into
    (foreign-procedure "cspp_net_sftp_read_into" (uptr ptr int int int int) scheme-object))
  (define ffi-net-sftp-write
    (foreign-procedure "cspp_net_sftp_write" (uptr ptr int int int int) scheme-object))
  (define ffi-net-websocket-listen
    (foreign-procedure "cspp_net_websocket_listen"
                       (string int string string uptr) scheme-object))
  (define ffi-net-websocket-server-close
    (foreign-procedure "cspp_net_websocket_server_close" (uptr) scheme-object))
  (define ffi-net-websocket-accept
    (foreign-procedure "cspp_net_websocket_accept" (uptr int int) scheme-object))
  (define ffi-net-websocket-connect
    (foreign-procedure "cspp_net_websocket_connect"
                       (string int string string string int uptr int) scheme-object))
  (define ffi-net-websocket-connect-step
    (foreign-procedure "cspp_net_websocket_connect_step" (uptr) scheme-object))
  (define ffi-net-websocket-poll-targets
    (foreign-procedure "cspp_net_websocket_poll_targets" (uptr int) scheme-object))
  (define ffi-net-websocket-close
    (foreign-procedure "cspp_net_websocket_close" (uptr) scheme-object))
  (define ffi-net-websocket-close-with-reason
    (foreign-procedure "cspp_net_websocket_close_with_reason"
                       (uptr int scheme-object) scheme-object))
  (define ffi-net-websocket-state
    (foreign-procedure "cspp_net_websocket_state" (uptr) scheme-object))
  (define ffi-net-websocket-cancel-send
    (foreign-procedure "cspp_net_websocket_cancel_send" (uptr) scheme-object))
  (define ffi-net-websocket-send
    (foreign-procedure "cspp_net_websocket_send" (uptr int ptr int int int int) scheme-object))
  (define ffi-net-websocket-send-fragment
    (foreign-procedure "cspp_net_websocket_send_fragment"
                       (uptr int ptr int int int int int) scheme-object))
  (define ffi-net-websocket-recv
    (foreign-procedure "cspp_net_websocket_recv" (uptr int int) scheme-object))
  (define ffi-net-grpc-channel-open
    (foreign-procedure "cspp_net_grpc_channel_open" (string) scheme-object))
  (define ffi-net-grpc-channel-open-tls
    (foreign-procedure "cspp_net_grpc_channel_open_tls"
                       (string string string string) scheme-object))
  (define ffi-net-grpc-channel-close
    (foreign-procedure "cspp_net_grpc_channel_close" (uptr) scheme-object))
  (define ffi-net-grpc-server-open
    (foreign-procedure "cspp_net_grpc_server_open" (string int) scheme-object))
  (define ffi-net-grpc-server-open-tls
    (foreign-procedure "cspp_net_grpc_server_open_tls"
                       (string int string string string) scheme-object))
  (define ffi-net-grpc-server-close
    (foreign-procedure __collect_safe
                       "cspp_net_grpc_server_close" (uptr) scheme-object))
  (define ffi-net-grpc-unary-call
    (foreign-procedure "cspp_net_grpc_unary_call"
                       (uptr string ptr int int scheme-object int)
                       scheme-object))
  (define ffi-net-grpc-unary-start
    (foreign-procedure "cspp_net_grpc_unary_start"
                       (uptr string ptr int int scheme-object int)
                       scheme-object))
  (define ffi-net-grpc-unary-poll
    (foreign-procedure "cspp_net_grpc_unary_poll" (uptr) scheme-object))
  (define ffi-net-grpc-unary-close
    (foreign-procedure "cspp_net_grpc_unary_close" (uptr) scheme-object))
  (define ffi-net-grpc-stream-open
    (foreign-procedure "cspp_net_grpc_stream_open"
                       (uptr string int ptr int int scheme-object int)
                       scheme-object))
  (define ffi-net-grpc-stream-open-start
    (foreign-procedure "cspp_net_grpc_stream_open_start"
                       (uptr string int ptr int int scheme-object int)
                       scheme-object))
  (define ffi-net-grpc-stream-open-poll
    (foreign-procedure "cspp_net_grpc_stream_open_poll" (uptr) scheme-object))
  (define ffi-net-grpc-stream-send
    (foreign-procedure "cspp_net_grpc_stream_send" (uptr ptr int int) scheme-object))
  (define ffi-net-grpc-stream-recv
    (foreign-procedure "cspp_net_grpc_stream_recv" (uptr) scheme-object))
  (define ffi-net-grpc-stream-close-send
    (foreign-procedure "cspp_net_grpc_stream_close_send" (uptr) scheme-object))
  (define ffi-net-grpc-stream-finish
    (foreign-procedure "cspp_net_grpc_stream_finish"
                       (uptr ptr int int int string scheme-object)
                       scheme-object))
  (define ffi-net-grpc-stream-close
    (foreign-procedure "cspp_net_grpc_stream_close" (uptr) scheme-object))
  (define ffi-net-grpc-server-request
    (foreign-procedure __collect_safe
                       "cspp_net_grpc_server_request" (uptr) scheme-object))
  (define ffi-net-grpc-server-request-stream
    (foreign-procedure __collect_safe
                       "cspp_net_grpc_server_request_stream" (uptr) scheme-object))
  (define ffi-net-grpc-server-respond
    (foreign-procedure "cspp_net_grpc_server_respond"
                       (uptr ptr int int int string scheme-object)
                       scheme-object))
  #|proc:ffi-net-grpc-capabilities
  The `ffi-net-grpc-capabilities` procedure calls the native grpc operation
  `cspp_net_grpc_capabilities`.
  It returns the capability bitmask: bit 0 means TLS and bit 1 means compression.
  It raises an error when gRPC is unavailable.
  |#
  (define ffi-net-grpc-capabilities
    (let ([native (foreign-procedure "cspp_net_grpc_capabilities" () unsigned-int)])
      (lambda ()
        (pcheck ()
                (require-optional-library 'ffi-net-grpc-capabilities 'grpc)
                (native )))))
  #|proc:ffi-net-grpc-driver-fd
  The `ffi-net-grpc-driver-fd` procedure calls the native grpc operation
  `cspp_net_grpc_driver_fd`.
  It returns the native file descriptor, or -1 when the handle has no descriptor.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-net-grpc-driver-fd
    (let ([native (foreign-procedure "cspp_net_grpc_driver_fd" () int)])
      (lambda ()
        (pcheck ()
                (require-optional-library 'ffi-net-grpc-driver-fd 'grpc)
                (native )))))
  (define ffi-net-grpc-driver-drain
    (foreign-procedure "cspp_net_grpc_driver_drain" () scheme-object))
  (define ffi-net-sftp-flag-read
    (foreign-procedure "cspp_net_sftp_flag_read" () int))
  (define ffi-net-sftp-flag-write
    (foreign-procedure "cspp_net_sftp_flag_write" () int))
  (define ffi-net-sftp-flag-read/write
    (foreign-procedure "cspp_net_sftp_flag_read_write" () int))
  (define ffi-net-sftp-flag-append
    (foreign-procedure "cspp_net_sftp_flag_append" () int))
  (define ffi-net-sftp-flag-create
    (foreign-procedure "cspp_net_sftp_flag_create" () int))
  (define ffi-net-sftp-flag-truncate
    (foreign-procedure "cspp_net_sftp_flag_truncate" () int))
  (define ffi-net-sftp-flag-exclusive
    (foreign-procedure "cspp_net_sftp_flag_exclusive" () int))
  (define ffi-net-sftp-flag-text
    (foreign-procedure "cspp_net_sftp_flag_text" () int))
  (define ffi-net-tls-load-error
    (foreign-procedure "cspp_net_tls_load_error" () ptr))
  #|proc:ffi-net-tls-context-create
  The `ffi-net-tls-context-create` procedure calls the native openssl operation
  `cspp_net_tls_context_create`.
  Parameters `mode` are passed to the native operation in that order.
  `mode` is a number.
  It returns an owned native handle, or zero when the resource could not be created.
  It raises an error when the dependency is unavailable.
  |#
  (define ffi-net-tls-context-create
    (let ([native (foreign-procedure "cspp_net_tls_context_create" (int) uptr)])
      (lambda (mode)
        (pcheck ([integer? mode])
                (require-optional-library 'ffi-net-tls-context-create 'openssl)
                (native mode)))))
  #|proc:ffi-net-tls-context-free
  The `ffi-net-tls-context-free` procedure calls the native openssl operation
  `cspp_net_tls_context_free`.
  Parameters `handle` are passed to the native operation in that order.
  `handle` is the native resource handle.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-net-tls-context-free
    (let ([native (foreign-procedure "cspp_net_tls_context_free" (uptr) void)])
      (lambda (handle)
        (pcheck ([natural? handle])
                (require-optional-library 'ffi-net-tls-context-free 'openssl)
                (native handle)))))
  (define ffi-net-tls-context-load-ca-file
    (foreign-procedure "cspp_net_tls_context_load_ca_file" (uptr string) scheme-object))
  (define ffi-net-tls-context-load-ca-path
    (foreign-procedure "cspp_net_tls_context_load_ca_path" (uptr string) scheme-object))
  (define ffi-net-tls-context-load-default-ca
    (foreign-procedure "cspp_net_tls_context_load_default_ca" (uptr) scheme-object))
  (define ffi-net-tls-context-load-cert-file
    (foreign-procedure "cspp_net_tls_context_load_cert_file" (uptr string int) scheme-object))
  (define ffi-net-tls-context-load-cert-bytes
    (foreign-procedure "cspp_net_tls_context_load_cert_bytes" (uptr ptr int int int) scheme-object))
  (define ffi-net-tls-context-load-key-file
    (foreign-procedure "cspp_net_tls_context_load_key_file" (uptr string int) scheme-object))
  (define ffi-net-tls-context-load-key-bytes
    (foreign-procedure "cspp_net_tls_context_load_key_bytes" (uptr ptr int int int) scheme-object))
  (define ffi-net-tls-context-check-key
    (foreign-procedure "cspp_net_tls_context_check_key" (uptr) scheme-object))
  (define ffi-net-tls-context-set-verify
    (foreign-procedure "cspp_net_tls_context_set_verify" (uptr int) scheme-object))
  (define ffi-net-tls-context-set-alpn
    (foreign-procedure "cspp_net_tls_context_set_alpn" (uptr ptr int int) scheme-object))
  (define ffi-net-tls-context-set-policy
    (foreign-procedure "cspp_net_tls_context_set_policy"
                       (uptr int int string string int) scheme-object))
  (define ffi-net-tls-context-import-session
    (foreign-procedure "cspp_net_tls_context_import_session"
                       (uptr ptr int int) scheme-object))
  (define ffi-net-tls-context-enable-sni
    (foreign-procedure "cspp_net_tls_context_enable_sni" (uptr) scheme-object))
  (define ffi-net-tls-connect
    (foreign-procedure "cspp_net_tls_connect" (uptr int string int) scheme-object))
  (define ffi-net-tls-accept
    (foreign-procedure "cspp_net_tls_accept" (uptr int int) scheme-object))
  (define ffi-net-tls-handshake-step
    (foreign-procedure "cspp_net_tls_handshake_step" (uptr) scheme-object))
  (define ffi-net-tls-close
    (foreign-procedure "cspp_net_tls_close" (uptr) scheme-object))
  (define ffi-net-tls-read
    (foreign-procedure "cspp_net_tls_read" (uptr int int int) scheme-object))
  (define ffi-net-tls-read-into
    (foreign-procedure "cspp_net_tls_read_into" (uptr ptr int int int int) scheme-object))
  (define ffi-net-tls-write
    (foreign-procedure "cspp_net_tls_write" (uptr ptr int int int int) scheme-object))
  (define ffi-net-tls-shutdown
    (foreign-procedure "cspp_net_tls_shutdown" (uptr) scheme-object))
  (define ffi-net-tls-protocol-version
    (foreign-procedure "cspp_net_tls_protocol_version" (uptr) scheme-object))
  (define ffi-net-tls-negotiated-alpn
    (foreign-procedure "cspp_net_tls_negotiated_alpn" (uptr) scheme-object))
  (define ffi-net-tls-cipher-name
    (foreign-procedure "cspp_net_tls_cipher_name" (uptr) scheme-object))
  (define ffi-net-tls-verified
    (foreign-procedure "cspp_net_tls_verified" (uptr) scheme-object))
  (define ffi-net-tls-session-export
    (foreign-procedure "cspp_net_tls_session_export" (uptr) scheme-object))
  (define ffi-net-tls-session-reused
    (foreign-procedure "cspp_net_tls_session_reused" (uptr) scheme-object))
  (define ffi-net-tls-session-select-context
    (foreign-procedure "cspp_net_tls_session_select_context" (uptr uptr) scheme-object))
  (define ffi-net-tls-stapled-ocsp
    (foreign-procedure "cspp_net_tls_stapled_ocsp" (uptr) scheme-object))
  (define ffi-net-tls-ocsp-result
    (foreign-procedure "cspp_net_tls_ocsp_result" (uptr) scheme-object))
  (define ffi-net-tls-peer-certificate-der
    (foreign-procedure "cspp_net_tls_peer_certificate_der" (uptr) scheme-object))
  (define ffi-net-tls-peer-certificate-chain-der
    (foreign-procedure "cspp_net_tls_peer_certificate_chain_der" (uptr) scheme-object))
  )
