(library (cspp crypto digest)
  (export digest-algorithms digest digest-bytevector digest-string digest-file
          digest-output-size digest-block-size
          make-digester digester? digester-update-bytevector! digester-update-string!
          digester-get digester-finalize! digester-reset! digester-destroy! call-with-digester

          ;; Algorithm-specific helpers; MD5 is retained for legacy interoperability.
          md5-bytevector sha224-bytevector sha256-bytevector sha384-bytevector sha512-bytevector
          sha512-224-bytevector sha512-256-bytevector sha3-224-bytevector sha3-256-bytevector
          sha3-384-bytevector sha3-512-bytevector blake2b-512-bytevector blake2s-256-bytevector
          blake3-bytevector
          md5-string sha224-string sha256-string sha384-string sha512-string sha512-224-string
          sha512-256-string sha3-224-string sha3-256-string sha3-384-string sha3-512-string
          blake2b-512-string blake2s-256-string blake3-string
          md5-file sha224-file sha256-file sha384-file sha512-file sha512-224-file sha512-256-file
          sha3-224-file sha3-256-file sha3-384-file sha3-512-file blake2b-512-file
          blake2s-256-file blake3-file)
  (import (cspp chez)
          (cspp utils)
          (cspp optional-library-check)
          (only (cspp crypto private)
                with-crypto-mutex with-crypto-operation call-with-crypto-resource)
          (only (cspp file) file-size)
          (cspp crypto ffi))

  #|proc:digest-algorithms
  The `digest-algorithms` value lists the supported digest algorithm symbols. Availability
  depends on the optional OpenSSL and BLAKE3 libraries and the active OpenSSL providers.
  MD5 is provided only for legacy interoperability and is unsuitable for secure hashing.
  |#
  (define digest-algorithms
    '(md5 sha224 sha256 sha384 sha512 sha512-224 sha512-256
      sha3-224 sha3-256 sha3-384 sha3-512 blake2b-512 blake2s-256 blake3))

  (define check-algorithm
    (lambda (who algorithm)
      (unless (memq algorithm digest-algorithms)
        (errorf who "valid digest algorithm is one of ~s" digest-algorithms))))

  (define ensure-backend
    (lambda (who algorithm)
      (require-optional-library who (if (eq? algorithm 'blake3) 'blake3 'openssl))))

  (define slice-index?
    (lambda (index) (and (fixnum? index) (fx>= index 0))))

  (define check-slice
    (lambda (who length start stop)
      (unless (and (slice-index? start) (slice-index? stop))
        (errorf who "slice indices must be nonnegative fixnums, given ~s and ~s" start stop))
      (unless (fx<= start stop length)
        (errorf who "invalid slice [~s, ~s) for length ~s" start stop length))))

  (define check-result
    (lambda (who result)
      (unless (bytevector? result)
        (errorf who "native digest operation failed"))
      result))

;;;;===----------------------------------------------------------------------===
;;;; One-shot digests and metadata
;;;;===----------------------------------------------------------------------===

  #|proc:digest-output-size
  The `digest-output-size` procedure returns the digest length in bytes for the symbol
  `algorithm`. It raises an error if the algorithm or its native backend is unavailable.
  |#
  (define-who digest-output-size
    (lambda (algorithm)
      (pcheck ([symbol? algorithm])
        (check-algorithm who algorithm)
        (ensure-backend who algorithm)
        (if (eq? algorithm 'blake3)
            32
            (let ([size (ffi-digest-output-size algorithm)])
              (unless (fx> size 0) (errorf who "digest algorithm unavailable: ~s" algorithm))
              size)))))

  #|proc:digest-block-size
  The `digest-block-size` procedure returns the compression block length in bytes for the
  symbol `algorithm`. It raises an error if the algorithm or its backend is unavailable.
  |#
  (define-who digest-block-size
    (lambda (algorithm)
      (pcheck ([symbol? algorithm])
        (check-algorithm who algorithm)
        (ensure-backend who algorithm)
        (if (eq? algorithm 'blake3)
            64
            (let ([size (ffi-digest-block-size algorithm)])
              (unless (fx> size 0) (errorf who "digest algorithm unavailable: ~s" algorithm))
              size)))))

  #|proc:digest-bytevector
  The `digest-bytevector` procedure returns the digest of `bytes` using the symbol `algorithm`.
  Optional nonnegative fixnums `start` and `stop` select the half-open byte slice; they default
  to zero and the bytevector length. The result is a fresh bytevector of raw digest bytes.
  Native backend and algorithm failures raise errors.
  |#
  (define-who digest-bytevector
    (case-lambda
      [(algorithm bytes)
       (pcheck ([bytevector? bytes])
         (digest-bytevector algorithm bytes 0 (bytevector-length bytes)))]
      [(algorithm bytes start)
       (pcheck ([bytevector? bytes])
         (digest-bytevector algorithm bytes start (bytevector-length bytes)))]
      [(algorithm bytes start stop)
       (pcheck ([symbol? algorithm] [bytevector? bytes])
         (check-algorithm who algorithm)
         (check-slice who (bytevector-length bytes) start stop)
         (ensure-backend who algorithm)
         (check-result who
           (if (eq? algorithm 'blake3)
               (ffi-blake3-bytevector bytes start stop)
               (ffi-digest-bytevector algorithm bytes start stop))))]))

  #|proc:digest-string
  The `digest-string` procedure returns the digest of `text` using the symbol `algorithm`.
  Each character is encoded as one native-endian UTF-32 value, without a byte-order mark.
  Optional nonnegative fixnums `start` and `stop` select a half-open slice in characters,
  defaulting to zero and the string length. The result is a fresh bytevector of digest bytes.
  For another encoding, encode the text explicitly and pass the bytes to `digest-bytevector`.
  Native backend and algorithm failures raise errors.
  |#
  (define-who digest-string
    (case-lambda
      [(algorithm text)
       (pcheck ([string? text]) (digest-string algorithm text 0 (string-length text)))]
      [(algorithm text start)
       (pcheck ([string? text]) (digest-string algorithm text start (string-length text)))]
      [(algorithm text start stop)
       (pcheck ([symbol? algorithm] [string? text])
         (check-algorithm who algorithm)
         (check-slice who (string-length text) start stop)
         (ensure-backend who algorithm)
         (check-result who
           (if (eq? algorithm 'blake3)
               (ffi-blake3-string text start stop)
               (ffi-digest-string algorithm text start stop))))]))

  #|proc:digest
  The `digest` procedure returns a fresh digest bytevector for the string or bytevector
  `data`, using the algorithm symbol `algorithm`. Strings use native-endian UTF-32 with
  no byte-order mark; bytevectors are hashed as supplied. Backend failures raise errors.
  |#
  (define-who digest
    (lambda (algorithm data)
      (pcheck ([symbol? algorithm]
               [(lambda (value) (or (string? value) (bytevector? value))) data])
        (if (string? data)
            (digest-string algorithm data)
            (digest-bytevector algorithm data)))))

;;;;===----------------------------------------------------------------------===
;;;; Incremental digest state and lifetime
;;;;===----------------------------------------------------------------------===

  (define-record-type (digester %make-digester %digester?)
    (opaque #t)
    (fields mutex algorithm (mutable handle) (mutable finalized?) (mutable destroyed?)))

  #|proc:digester?
  The `digester?` procedure returns `#t` if `object` is a digester, including a finalized or
  destroyed digester, and `#f` otherwise.
  |#
  (define digester?
    (lambda (object) (pcheck () (%digester? object))))

  (define check-live
    (lambda (who state)
      (when (digester-destroyed? state) (errorf who "digester is destroyed"))
      (when (digester-finalized? state) (errorf who "digester is finalized"))))

  (define blake3-state?
    (lambda (state) (eq? (digester-algorithm state) 'blake3)))

  #|proc:make-digester
  The `make-digester` procedure returns a new incremental digester for the algorithm symbol
  `algorithm`, defaulting to `sha256`. It raises an error if allocation or initialization
  fails or the backend is unavailable. Release the state with `digester-finalize!` or
  `digester-destroy!`, or manage its lifetime using `call-with-digester`.
  |#
  (define-who make-digester
    (case-lambda
      [() (make-digester 'sha256)]
      [(algorithm)
       (pcheck ([symbol? algorithm])
         (check-algorithm who algorithm)
         (ensure-backend who algorithm)
         (with-crypto-operation
           (let ([handle (if (eq? algorithm 'blake3)
                             (ffi-blake3-create)
                             (ffi-digest-state-create algorithm))])
             (when (= handle 0) (errorf who "failed to create digester for ~s" algorithm))
             (%make-digester (make-mutex 'digester) algorithm handle #f #f))))]))

  #|proc:digester-update-bytevector!
  The `digester-update-bytevector!` procedure feeds `bytes` into the live digester `state`
  and returns `state`. Optional nonnegative fixnums `start` and `stop` select the half-open
  byte slice, defaulting to zero and the bytevector length. Invalid or terminal states,
  invalid slices, and native update failures raise errors.
  |#
  (define-who digester-update-bytevector!
    (case-lambda
      [(state bytes)
       (pcheck ([bytevector? bytes])
         (digester-update-bytevector! state bytes 0 (bytevector-length bytes)))]
      [(state bytes start)
       (pcheck ([bytevector? bytes])
         (digester-update-bytevector! state bytes start (bytevector-length bytes)))]
      [(state bytes start stop)
       (pcheck ([digester? state] [bytevector? bytes])
         (with-crypto-mutex (digester-mutex state)
           (check-live who state)
           (check-slice who (bytevector-length bytes) start stop)
           (unless (= 1 ((if (blake3-state? state)
                             ffi-blake3-update-bytevector! ffi-digest-state-update-bytevector!)
                         (digester-handle state) bytes start stop))
             (errorf who "failed to update digester"))
           state))]))

  #|proc:digester-update-string!
  The `digester-update-string!` procedure feeds `text` into the live digester `state` and
  returns `state`. Each character uses native-endian UTF-32, without a byte-order mark.
  Optional nonnegative fixnums `start` and `stop` select a half-open character slice,
  defaulting to zero and the string length. Invalid or terminal states, invalid slices,
  and native update failures raise errors.
  |#
  (define-who digester-update-string!
    (case-lambda
      [(state text)
       (pcheck ([string? text])
         (digester-update-string! state text 0 (string-length text)))]
      [(state text start)
       (pcheck ([string? text])
         (digester-update-string! state text start (string-length text)))]
      [(state text start stop)
       (pcheck ([digester? state] [string? text])
         (with-crypto-mutex (digester-mutex state)
           (check-live who state)
           (check-slice who (string-length text) start stop)
           (unless (= 1 ((if (blake3-state? state)
                             ffi-blake3-update-string! ffi-digest-state-update-string!)
                         (digester-handle state) text start stop))
             (errorf who "failed to update digester"))
           state))]))

  #|proc:digester-get
  The `digester-get` procedure returns a fresh digest bytevector for the input accumulated
  by the live digester `state`. It preserves the state, so repeated reads and subsequent
  updates are allowed. Terminal states and native snapshot failures raise errors.
  |#
  (define-who digester-get
    (lambda (state)
      (pcheck ([digester? state])
        (with-crypto-mutex (digester-mutex state)
          (check-live who state)
          (check-result who
            ((if (blake3-state? state) ffi-blake3-get ffi-digest-state-get)
             (digester-handle state)))))))

  (define release-handle!
    (lambda (state)
      (unless (= (digester-handle state) 0)
        ((if (blake3-state? state) ffi-blake3-destroy ffi-digest-state-destroy)
         (digester-handle state))
        (digester-handle-set! state 0))))

  #|proc:digester-finalize!
  The `digester-finalize!` procedure returns a fresh digest bytevector for the live
  digester `state` and releases its native resources. The state becomes terminal;
  further updates, reads, resets, and finalization raise errors. Native failures raise
  errors; use `digester-destroy!` to release the state if finalization fails.
  |#
  (define-who digester-finalize!
    (lambda (state)
      (pcheck ([digester? state])
        (with-crypto-mutex (digester-mutex state)
          (check-live who state)
          (let ([result (digester-get state)])
            (release-handle! state)
            (digester-finalized?-set! state #t)
            result)))))

  #|proc:digester-reset!
  The `digester-reset!` procedure clears the accumulated input of the live digester
  `state`, preserving its algorithm and native allocation, and returns `state`.
  Terminal states and native reset failures raise errors.
  |#
  (define-who digester-reset!
    (lambda (state)
      (pcheck ([digester? state])
        (with-crypto-mutex (digester-mutex state)
          (check-live who state)
          (unless (= 1 ((if (blake3-state? state) ffi-blake3-reset! ffi-digest-state-reset!)
                        (digester-handle state)))
            (errorf who "failed to reset digester"))
          state))))

  #|proc:digester-destroy!
  The `digester-destroy!` procedure releases the native resources of `state`, discards its
  accumulated input, and returns `state`. It is safe to repeat or call after finalization.
  Subsequent updates, reads, resets, and finalization raise errors.
  |#
  (define-who digester-destroy!
    (lambda (state)
      (pcheck ([digester? state])
        (with-crypto-mutex (digester-mutex state)
          (unless (digester-destroyed? state)
            (release-handle! state)
            (digester-destroyed?-set! state #t))
          state))))

  (define call-with-owned-digester
    (lambda (algorithm proc)
      (call-with-crypto-resource 'call-with-digester
        (lambda () (make-digester algorithm)) digester-destroy! proc)))

  #|proc:call-with-digester
  The `call-with-digester` procedure creates a digester for the algorithm symbol
  `algorithm`, defaulting to `sha256`, and calls `(proc state)`. The callback should feed
  input into `state` and leave it live; its return values are ignored. This procedure
  returns the final digest bytevector and always destroys the state on exit, including
  exceptions or continuation escapes. A continuation cannot reenter a destroyed scope.
  |#
  (define call-with-digester
    (case-lambda
      [(proc) (call-with-digester 'sha256 proc)]
      [(algorithm proc)
       (pcheck ([symbol? algorithm] [procedure? proc])
         (call-with-owned-digester algorithm
           (lambda (state)
             (call-with-values (lambda () (proc state))
               (lambda ignored (digester-finalize! state))))))]))

;;;;===----------------------------------------------------------------------===
;;;; Streaming files and algorithm-specific convenience procedures
;;;;===----------------------------------------------------------------------===

  #|proc:digest-file
  The `digest-file` procedure returns a digest bytevector for the regular file at `path`
  using the algorithm symbol `algorithm`. Optional nonnegative fixnums `start` and `stop`
  select a half-open byte slice, defaulting to zero and the file size. Input is streamed
  with bounded memory, and the port and digester are released even on failure. Invalid
  slices, backend failures, and premature end-of-file raise errors.
  |#
  (define-who digest-file
    (case-lambda
      [(algorithm path)
       (pcheck ([string? path] [file-regular? path])
         (digest-file algorithm path 0 (file-size path)))]
      [(algorithm path start)
       (pcheck ([string? path] [file-regular? path])
         (digest-file algorithm path start (file-size path)))]
      [(algorithm path start stop)
       (pcheck ([symbol? algorithm] [string? path] [file-regular? path])
         (check-algorithm who algorithm)
         (check-slice who (file-size path) start stop)
         (call-with-digester algorithm
           (lambda (state)
             (call-with-crypto-resource who
               (lambda () (open-file-input-port path)) close-port
               (lambda (port)
                 (let ([buffer (make-bytevector 4096)])
                   (set-port-position! port start)
                   (let loop ([remaining (fx- stop start)])
                     (unless (fx= remaining 0)
                       (let ([count (get-bytevector-n! port buffer 0
                                                      (fxmin remaining 4096))])
                         (when (or (eof-object? count) (fx= count 0))
                           (errorf who "unexpected end of file: ~s" path))
                         (digester-update-bytevector! state buffer 0 count)
                         (loop (fx- remaining count)))))))))))]))

  (define-syntax define-specialized-digest
    (syntax-rules ()
      [(_ name algorithm generic input input?)
       (define name
         (case-lambda
           [(input) (pcheck ([input? input]) (generic 'algorithm input))]
           [(input start) (pcheck ([input? input]) (generic 'algorithm input start))]
           [(input start stop)
            (pcheck ([input? input]) (generic 'algorithm input start stop))]))]))

  #|proc:md5-bytevector
  The `md5-bytevector` procedure returns a fresh MD5 digest bytevector for `bytes`.
  Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Input is a bytevector.
  Invalid inputs and backend failures raise errors. MD5 is retained only for legacy
  interoperability; it is unsuitable for security-sensitive hashing.
  |#
  (define-specialized-digest md5-bytevector md5 digest-bytevector bytes bytevector?)

  #|proc:sha224-bytevector
  The `sha224-bytevector` procedure returns a fresh `sha224` digest bytevector for the
  bytevector `bytes`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha224-bytevector sha224 digest-bytevector bytes bytevector?)

  #|proc:sha256-bytevector
  The `sha256-bytevector` procedure returns a fresh `sha256` digest bytevector for the
  bytevector `bytes`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha256-bytevector sha256 digest-bytevector bytes bytevector?)

  #|proc:sha384-bytevector
  The `sha384-bytevector` procedure returns a fresh `sha384` digest bytevector for the
  bytevector `bytes`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha384-bytevector sha384 digest-bytevector bytes bytevector?)

  #|proc:sha512-bytevector
  The `sha512-bytevector` procedure returns a fresh `sha512` digest bytevector for the
  bytevector `bytes`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha512-bytevector sha512 digest-bytevector bytes bytevector?)

  #|proc:sha512-224-bytevector
  The `sha512-224-bytevector` procedure returns a fresh `sha512-224` digest bytevector for the
  bytevector `bytes`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha512-224-bytevector sha512-224 digest-bytevector bytes bytevector?)

  #|proc:sha512-256-bytevector
  The `sha512-256-bytevector` procedure returns a fresh `sha512-256` digest bytevector for the
  bytevector `bytes`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha512-256-bytevector sha512-256 digest-bytevector bytes bytevector?)

  #|proc:sha3-224-bytevector
  The `sha3-224-bytevector` procedure returns a fresh `sha3-224` digest bytevector for the
  bytevector `bytes`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha3-224-bytevector sha3-224 digest-bytevector bytes bytevector?)

  #|proc:sha3-256-bytevector
  The `sha3-256-bytevector` procedure returns a fresh `sha3-256` digest bytevector for the
  bytevector `bytes`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha3-256-bytevector sha3-256 digest-bytevector bytes bytevector?)

  #|proc:sha3-384-bytevector
  The `sha3-384-bytevector` procedure returns a fresh `sha3-384` digest bytevector for the
  bytevector `bytes`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha3-384-bytevector sha3-384 digest-bytevector bytes bytevector?)

  #|proc:sha3-512-bytevector
  The `sha3-512-bytevector` procedure returns a fresh `sha3-512` digest bytevector for the
  bytevector `bytes`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha3-512-bytevector sha3-512 digest-bytevector bytes bytevector?)

  #|proc:blake2b-512-bytevector
  The `blake2b-512-bytevector` procedure returns a fresh `blake2b-512` digest bytevector for the
  bytevector `bytes`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest blake2b-512-bytevector blake2b-512 digest-bytevector bytes bytevector?)

  #|proc:blake2s-256-bytevector
  The `blake2s-256-bytevector` procedure returns a fresh `blake2s-256` digest bytevector for the
  bytevector `bytes`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest blake2s-256-bytevector blake2s-256 digest-bytevector bytes bytevector?)

  #|proc:blake3-bytevector
  The `blake3-bytevector` procedure returns a fresh `blake3` digest bytevector for the
  bytevector `bytes`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest blake3-bytevector blake3 digest-bytevector bytes bytevector?)

  #|proc:md5-string
  The `md5-string` procedure returns a fresh MD5 digest bytevector for `text`.
  Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and backend failures raise errors. MD5 is retained only for legacy
  interoperability; it is unsuitable for security-sensitive hashing.
  |#
  (define-specialized-digest md5-string md5 digest-string text string?)

  #|proc:sha224-string
  The `sha224-string` procedure returns a fresh `sha224` digest bytevector for the
  string `text`. Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha224-string sha224 digest-string text string?)

  #|proc:sha256-string
  The `sha256-string` procedure returns a fresh `sha256` digest bytevector for the
  string `text`. Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha256-string sha256 digest-string text string?)

  #|proc:sha384-string
  The `sha384-string` procedure returns a fresh `sha384` digest bytevector for the
  string `text`. Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha384-string sha384 digest-string text string?)

  #|proc:sha512-string
  The `sha512-string` procedure returns a fresh `sha512` digest bytevector for the
  string `text`. Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha512-string sha512 digest-string text string?)

  #|proc:sha512-224-string
  The `sha512-224-string` procedure returns a fresh `sha512-224` digest bytevector for the
  string `text`. Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha512-224-string sha512-224 digest-string text string?)

  #|proc:sha512-256-string
  The `sha512-256-string` procedure returns a fresh `sha512-256` digest bytevector for the
  string `text`. Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha512-256-string sha512-256 digest-string text string?)

  #|proc:sha3-224-string
  The `sha3-224-string` procedure returns a fresh `sha3-224` digest bytevector for the
  string `text`. Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha3-224-string sha3-224 digest-string text string?)

  #|proc:sha3-256-string
  The `sha3-256-string` procedure returns a fresh `sha3-256` digest bytevector for the
  string `text`. Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha3-256-string sha3-256 digest-string text string?)

  #|proc:sha3-384-string
  The `sha3-384-string` procedure returns a fresh `sha3-384` digest bytevector for the
  string `text`. Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha3-384-string sha3-384 digest-string text string?)

  #|proc:sha3-512-string
  The `sha3-512-string` procedure returns a fresh `sha3-512` digest bytevector for the
  string `text`. Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha3-512-string sha3-512 digest-string text string?)

  #|proc:blake2b-512-string
  The `blake2b-512-string` procedure returns a fresh `blake2b-512` digest bytevector for the
  string `text`. Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest blake2b-512-string blake2b-512 digest-string text string?)

  #|proc:blake2s-256-string
  The `blake2s-256-string` procedure returns a fresh `blake2s-256` digest bytevector for the
  string `text`. Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest blake2s-256-string blake2s-256 digest-string text string?)

  #|proc:blake3-string
  The `blake3-string` procedure returns a fresh `blake3` digest bytevector for the
  string `text`. Optional nonnegative fixnums `start` and `stop` select a half-open
  character slice, defaulting to zero and the input length.
  Strings use native-endian UTF-32 without a byte-order mark.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest blake3-string blake3 digest-string text string?)

  #|proc:md5-file
  The `md5-file` procedure returns a fresh MD5 digest bytevector for `path`.
  Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Input is a regular file path.
  Invalid inputs and backend failures raise errors. MD5 is retained only for legacy
  interoperability; it is unsuitable for security-sensitive hashing.
  |#
  (define-specialized-digest md5-file md5 digest-file path string?)

  #|proc:sha224-file
  The `sha224-file` procedure returns a fresh `sha224` digest bytevector for the
  regular file at `path`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha224-file sha224 digest-file path string?)

  #|proc:sha256-file
  The `sha256-file` procedure returns a fresh `sha256` digest bytevector for the
  regular file at `path`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha256-file sha256 digest-file path string?)

  #|proc:sha384-file
  The `sha384-file` procedure returns a fresh `sha384` digest bytevector for the
  regular file at `path`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha384-file sha384 digest-file path string?)

  #|proc:sha512-file
  The `sha512-file` procedure returns a fresh `sha512` digest bytevector for the
  regular file at `path`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha512-file sha512 digest-file path string?)

  #|proc:sha512-224-file
  The `sha512-224-file` procedure returns a fresh `sha512-224` digest bytevector for the
  regular file at `path`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha512-224-file sha512-224 digest-file path string?)

  #|proc:sha512-256-file
  The `sha512-256-file` procedure returns a fresh `sha512-256` digest bytevector for the
  regular file at `path`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha512-256-file sha512-256 digest-file path string?)

  #|proc:sha3-224-file
  The `sha3-224-file` procedure returns a fresh `sha3-224` digest bytevector for the
  regular file at `path`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha3-224-file sha3-224 digest-file path string?)

  #|proc:sha3-256-file
  The `sha3-256-file` procedure returns a fresh `sha3-256` digest bytevector for the
  regular file at `path`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha3-256-file sha3-256 digest-file path string?)

  #|proc:sha3-384-file
  The `sha3-384-file` procedure returns a fresh `sha3-384` digest bytevector for the
  regular file at `path`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha3-384-file sha3-384 digest-file path string?)

  #|proc:sha3-512-file
  The `sha3-512-file` procedure returns a fresh `sha3-512` digest bytevector for the
  regular file at `path`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest sha3-512-file sha3-512 digest-file path string?)

  #|proc:blake2b-512-file
  The `blake2b-512-file` procedure returns a fresh `blake2b-512` digest bytevector for the
  regular file at `path`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest blake2b-512-file blake2b-512 digest-file path string?)

  #|proc:blake2s-256-file
  The `blake2s-256-file` procedure returns a fresh `blake2s-256` digest bytevector for the
  regular file at `path`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest blake2s-256-file blake2s-256 digest-file path string?)

  #|proc:blake3-file
  The `blake3-file` procedure returns a fresh `blake3` digest bytevector for the
  regular file at `path`. Optional nonnegative fixnums `start` and `stop` select a half-open
  byte slice, defaulting to zero and the input length.
  Invalid inputs and native backend failures raise errors.
  |#
  (define-specialized-digest blake3-file blake3 digest-file path string?)
)
