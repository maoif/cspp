#!chezscheme

(library (cspp file)
  (export read-lines read-string read-chars read-data read-datum read-data-fasl read-datum-fasl read-u8vec
          write-lines write-string write-chars write-data write-datum write-data-fasl write-datum-fasl write-u8vec
          write-lines! write-string! write-chars! write-data! write-datum! write-data-fasl! write-datum-fasl! write-u8vec!
          write-lines>> write-string>> write-chars>> write-data>> write-datum>> write-data-fasl>> write-datum-fasl>> write-u8vec>>

          get-u16 get-u32 get-u64 get-s16 get-s32 get-s64
          put-u16 put-u32 put-u64 put-s16 put-s32 put-s64

          GET-U16 GET-U32 GET-U64 GET-S16 GET-S32 GET-S64
          PUT-U16 PUT-U32 PUT-U64 PUT-S16 PUT-S32 PUT-S64

          make-path

          file-stat file-stat? file-stat-type file-stat-mode file-stat-inode
          file-stat-owner file-stat-group file-stat-nlinks file-stat-size
          file-stat-blocks file-stat-dev-major file-stat-dev-minor
          file-stat-access-time file-stat-modification-time file-stat-change-time
          file-stat-creation-time
          file-type file-type->symbol file-mode file-size file-size-h
          file-access-time file-modification-time file-change-time file-creation-time
          file-inode file-blocks file-nlinks file-owner file-group
          file-dev-major file-dev-minor

          file-readable? file-writable? file-executable? file-hidden? file-special? same-file? same-file-contents?

          file-mode->symbols symbols->file-mode
          file-chmod file-chmod-s file-chmod-u file-chmod-g file-chmod-o file-chmod-a

          readlink readlink2
          file-link file-symlink file-link! file-symlink!
          file-touch file-touch-atime file-touch-mtime

          walk-files file-find file-find-all file-map file-for-each print-file-tree
          fs-open-directory fs-read-directory fs-close-directory
          fs-directory? fs-directory-closed?
          fs->iter FT_unknown

          file-copymode file-copymeta copy-port file-copy file-copy!
          file-copytree file-move file-removetree

          mkdirs define-file-tree

          fswatcher-mask fswatcher-mask->symbols fswatcher-error?
          make-fswatcher close-fswatcher fswatcher-add! fswatcher-remove! fswatcher-next!

          make-i/o-file-not-regular-error   i/o-file-not-regular-error?
          make-i/o-file-not-directory-error i/o-file-not-directory-error?)
  (import (cspp chez)
          (cspp private os)
          (cspp private file)
          (cspp path)
          (cspp internal)
          (cspp utils)
          (only (cspp string) string-search)
          (cspp list)
          (cspp iter)
          (cspp io))

  #|doc
  Filesystem path arguments must be strings without NUL characters. Invalid names raise
  an error before filesystem access, rather than selecting a prefix of the supplied name.
  File contents may contain NUL characters.
  Names and symbolic-link targets read from the filesystem must be valid UTF-8.
  Undecodable bytes raise an error; they are never replaced with another filename.
  Concurrent filesystem operations on the same files have undefined behavior. Callers must
  synchronize them.
  |#
  ;; Validate the original Scheme string before any C-string conversion or filesystem probe.
  ;; Empty names retain each operation's existing behavior; NUL is never a filename character.
  (define $filename?
    (lambda (value)
      (and (string? value) (not (string-search value #\nul)))))


  (define-condition-type &i/o-file-not-regular &i/o-filename
    make-i/o-file-not-regular-error i/o-file-not-regular-error?)
  (define-condition-type &i/o-file-not-directory &i/o-filename
    make-i/o-file-not-directory-error i/o-file-not-directory-error?)

  (define $err-file
    (lambda (who x msg)
      (cond [(condition? x)
             (raise (condition (make-who-condition who) x (make-message-condition msg)))]
            [(string? x)
             (raise (condition (make-who-condition who) (make-i/o-filename-error x)
                               (make-message-condition msg)))]
            [else (assert-unreachable)])))

  (define $err-file-not-regular
    (lambda (who p)
      ($err-file who (make-i/o-file-not-regular-error p)
                 (format "file is not regular: ~a" p))))
  (define $err-file-not-directory
    (lambda (who p)
      ($err-file who (make-i/o-file-not-directory-error p)
                 (format "file is not directory: ~a" p))))
  (define $err-file-exists
    (lambda (who p)
      ($err-file who (make-i/o-file-already-exists-error p)
                 (format "file already exists: ~a" p))))
  (define $err-directory-exists
    (lambda (who p)
      ($err-file who (make-i/o-file-already-exists-error p)
                 (format "directory already exists: ~a" p))))
  (define $err-file-not-found
    (lambda (who p)
      ($err-file who (make-i/o-file-does-not-exist-error p)
                 (format "file not found: ~a" p))))

  (define $err-bad-write-mode
    (lambda (who mode)
      (errorf who "unknown write mode: ~a, should be one of '(error truncate append)" mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   simple file read/write
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define $with-port
    (lambda (port proc)
      (dynamic-wind
        void
        (lambda () (proc port))
        (lambda ()
          (unless (port-closed? port)
            (close-port port))))))

  #|proc:read-lines
  The `read-lines` procedure reads the text file at `path` and returns a list of lines.
  The `path` parameter is the filesystem path string to read.
  |#
  (define read-lines
    (lambda (path)
      (pcheck ([$filename? path] [file-regular? path])
       ($with-port (open-input-file path)
         (lambda (p)
           (let ([lb (make-list-builder)])
             (let loop ()
               (let ([x (get-line p)])
                 (if (eof-object? x)
                     (lb)
                     (begin (lb x)
                            (loop)))))))))))
  #|proc:read-string
  The `read-string` procedure reads the text file at `path` and returns its contents.
  The `path` parameter is the filesystem path string to read.
  |#
  (define read-string
    (lambda (path)
      (pcheck ([$filename? path] [file-regular? path])
       ($with-port (open-input-file path)
         (lambda (p)
           (let ([x (get-string-all p)])
             (if (eof-object? x) "" x)))))))
  #|proc:read-chars
  The `read-chars` procedure reads the text file at `path` and returns a list of chars.
  The `path` parameter is the filesystem path string to read.
  |#
  (define read-chars
    (lambda (path)
      (pcheck ([$filename? path] [file-regular? path])
       ($with-port (open-input-file path)
         (lambda (p)
           (let ([lb (make-list-builder)])
             (let loop ()
               (let ([x (get-char p)])
                 (if (eof-object? x)
                     (lb)
                     (begin (lb x)
                            (loop)))))))))))
  #|proc:read-data
  The `read-data` procedure reads datums from `path` and returns them as a list.
  The `path` parameter is the filesystem path string to read.
  A top-level `#!eof` datum terminates reading, as with Chez `load`; it and any following
  datums are excluded. An empty file returns `()`.
  |#
  (define read-data
    (lambda (path)
      (pcheck ([$filename? path] [file-regular? path])
       ($with-port (open-input-file path)
         (lambda (p)
           (let ([lb (make-list-builder)])
             (let loop ()
               (let ([x (get-datum p)])
                 (if (eof-object? x)
                     (lb)
                     (begin (lb x)
                            (loop)))))))))))
  #|proc:read-datum
  The `read-datum` procedure reads one datum from the text file at `path`.
  The `path` parameter is the filesystem path string to read.
  |#
  (define read-datum
    (lambda (path)
      (pcheck ([$filename? path] [file-regular? path])
       ($with-port (open-input-file path)
         (lambda (p) (get-datum p))))))
  ;; Identify whether the next native read will decode a datum or only consume framing.
  ;; Chez uses byte 0 for a header and 127 for a group terminator. Headers contain the
  ;; seven-byte magic suffix, two base-128 integers, and a parenthesized boot-file list.
  ;; fasl-read validates the full representation. Rewind after inspection to clear the
  ;; port buffer: byte lookahead selects a different Chez decoding path.
  (define $fasl-datum?
    (lambda (port)
      (define byte
        (lambda ()
          (let ([value (get-u8 port)])
            (when (eof-object? value)
              (errorf 'read-data-fasl "unexpected EOF in FASL header"))
            value)))
      (define skip-integer
        (lambda () (when (fxlogbit? 7 (byte)) (skip-integer))))
      (let ([start (port-position port)])
        (dynamic-wind
          void
          (lambda ()
            (let loop ()
              (let ([type (get-u8 port)])
                (cond [(or (eof-object? type) (eqv? type 127)) #f]
                      [(eqv? type 0)
                       (do ([index 0 (fx1+ index)]) ((fx= index 7)) (byte))
                       (skip-integer)
                       (skip-integer)
                       (unless (fx= (byte) 40)
                         (errorf 'read-data-fasl "invalid FASL header boot-file list"))
                       (let skip-boot-files ()
                         (unless (fx= (byte) 41) (skip-boot-files)))
                       (loop)]
                      [else #t]))))
          (lambda () (set-port-position! port start))))))

  #|proc:read-data-fasl
  The `read-data-fasl` procedure reads fasl datums from `path` as a list.
  The `path` parameter is the filesystem path string to read.
  Return all datums in order, including serialized EOF objects, stopping at physical EOF.
  FASL group headers and terminators are excluded; later groups are still read.
  An empty file returns `()`. Malformed or truncated fasl data raises an error.
  |#
  (define read-data-fasl
    (lambda (path)
      (pcheck ([$filename? path] [file-regular? path])
       ($with-port (open-file-input-port path)
         (lambda (p)
           (let ([lb (make-list-builder)])
             (let loop ()
               (let* ([start (port-position p)]
                      [datum? ($fasl-datum? p)]
                      [datum (fasl-read p)])
                 (when datum? (lb datum))
                 (if (= start (port-position p)) (lb) (loop))))))))))
  #|proc:read-datum-fasl
  The `read-datum-fasl` procedure reads one fasl datum from `path`.
  The `path` parameter is the filesystem path string to read.
  |#
  (define read-datum-fasl
    (lambda (path)
      (pcheck ([$filename? path] [file-regular? path])
       ($with-port (open-file-input-port path)
         (lambda (p) (fasl-read p))))))
  #|proc:read-u8vec
  The `read-u8vec` procedure reads the binary file at `path` as a bytevector.
  The `path` parameter is the filesystem path string to read.
  An empty file returns an empty bytevector.
  |#
  (define read-u8vec
    (lambda (path)
      (pcheck ([$filename? path] [file-regular? path])
       ($with-port (open-file-input-port path)
         (lambda (p)
           (let ([value (get-bytevector-all p)])
             (if (eof-object? value) #vu8() value)))))))

  (define all-strings? (lambda (x) (and (list? x) (andmap string? x))))
  (define all-chars?   (lambda (x) (and (list? x) (andmap char? x))))


  ;; mode when file exists: 'error, 'truncate, 'append
  (define $write-helper
    (lambda (who path mode bin? writer)
      (pcheck ([$filename? path])
              (let ([go (lambda (mode)
                          (let ([op (case mode
                                      [error (file-options)]
                                      [truncate (file-options no-fail)]
                                      [append (file-options no-truncate no-fail append)]
                                      [else ($err-bad-write-mode who mode)])])
                            ($with-port (open-file-output-port path op (buffer-mode block)
                                                               (if bin? #f (current-transcoder)))
                              (lambda (p) (writer p)))))])
                (if (file-exists? path)
                    (if (eq? mode 'error)
                        ($err-file-exists who path)
                        (if (file-regular? path)
                            (go mode)
                            ($err-file-not-regular who path)))
                    (go mode))))))

  ;; a newline is added after writing each string
  (define $write-lines
    (lambda (who path mode lines)
      (pcheck ([all-strings? lines])
              (let ([do-write (lambda (p)
                                (let loop ([lines lines])
                                  (unless (null? lines)
                                    (put-string p (car lines))
                                    (newline p)
                                    (loop (cdr lines)))))])
                ($write-helper who path mode #f do-write)))))
  (define $write-string
    (lambda (who path mode str)
      (pcheck-string
       (str)
       (let ([do-write (lambda (p)
                         (put-string p str))])
         ($write-helper who path mode #f do-write)))))
  (define $write-chars
    (lambda (who path mode chars)
      (pcheck ([all-chars? chars])
              (let ([do-write (lambda (p)
                                (let loop ([chars chars])
                                  (unless (null? chars)
                                    (put-char p (car chars))
                                    (loop (cdr chars)))))])
                ($write-helper who path mode #f do-write)))))
  ;; write a list of datum
  (define $write-data
    (lambda (who path mode data)
      (pcheck-list
       (data)
       (let ([do-write (lambda (p)
                         (let loop ([data data])
                           (unless (null? data)
                             (put-datum p (car data))
                             ;; for better readability
                             (newline p)
                             (loop (cdr data)))))])
         ($write-helper who path mode #f do-write)))))
  ;; write a single datum
  (define $write-datum
    (lambda (who path mode datum)
      (define writable?
        (lambda (x)
          (not (or (procedure? x) (hashtable? x) (port? x)))))
      (pcheck ([writable? datum])
              (let ([do-write (lambda (p)
                                (put-datum p datum)
                                (newline p))])
                ($write-helper who path mode #f do-write)))))
  ;; write a list of datum in fasl format
  (define $write-data-fasl
    (lambda (who path mode data)
      (pcheck-list
       (data)
       (let ([do-write (lambda (p)
                         (let loop ([data data])
                           (unless (null? data)
                             (fasl-write (car data) p)
                             (loop (cdr data)))))])
         ($write-helper who path mode #t do-write)))))
  ;; write a single datum in fasl format
  (define $write-datum-fasl
    (lambda (who path mode datum)
      (let ([do-write (lambda (p) (fasl-write datum p))])
        ($write-helper who path mode #t do-write))))
  (define $write-u8vec
    (lambda (who path mode u8vec)
      (pcheck-bytevector
       (u8vec)
       (let ([do-write (lambda (p) (put-bytevector p u8vec))])
         ($write-helper who path mode #t do-write)))))

  ;; error by default when file already exist
  #|proc:write-lines
  The `write-lines` procedure writes `lines` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `lines` parameter is the list of strings to write, one per line.
  The procedure errors if the file already exists.
  |#
  (define-who write-lines
    (lambda (path lines)
      ($write-lines who path 'error lines)))
  #|proc:write-string
  The `write-string` procedure writes `str` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `str` parameter is the string to write.
  The procedure errors if the file already exists.
  |#
  (define-who write-string
    (lambda (path str)
      ($write-string who path 'error str)))
  #|proc:write-chars
  The `write-chars` procedure writes `chars` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `chars` parameter is the list of characters to write.
  The procedure errors if the file already exists.
  |#
  (define-who write-chars
    (lambda (path chars)
      ($write-chars who path 'error chars)))
  #|proc:write-data
  The `write-data` procedure writes each datum in `data` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `data` parameter is the list of datums to write.
  The procedure errors if the file already exists.
  |#
  (define-who write-data
    (lambda (path data)
      ($write-data who path 'error data)))
  #|proc:write-datum
  The `write-datum` procedure writes `datum` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `datum` parameter is the datum to write.
  The procedure errors if the file already exists.
  |#
  (define-who write-datum
    (lambda (path datum)
      ($write-datum who path 'error datum)))
  #|proc:write-data-fasl
  The `write-data-fasl` procedure writes each datum in `data` to binary `path`.
  The `path` parameter is the filesystem path string to write.
  The `data` parameter is the list of datums to write in fasl format.
  The procedure errors if the file already exists.
  |#
  (define-who write-data-fasl
    (lambda (path data)
      ($write-data-fasl who path 'error data)))
  #|proc:write-datum-fasl
  The `write-datum-fasl` procedure writes `datum` to binary `path` in fasl format.
  The `path` parameter is the filesystem path string to write.
  The `datum` parameter is the datum to write.
  The procedure errors if the file already exists.
  |#
  (define-who write-datum-fasl
    (lambda (path datum)
      ($write-datum-fasl who path 'error datum)))
  #|proc:write-u8vec
  The `write-u8vec` procedure writes `u8vec` to the binary file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `u8vec` parameter is the bytevector to write.
  The procedure errors if the file already exists.
  |#
  (define-who write-u8vec
    (lambda (path u8vec)
      ($write-u8vec who path 'error u8vec)))

  ;; truncate by default
  #|proc:write-lines!
  The `write-lines!` procedure writes `lines` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `lines` parameter is the list of strings to write, one per line.
  The procedure truncates the file if it already exists.
  |#
  (define-who write-lines!
    (lambda (path lines)
      ($write-lines who path 'truncate lines)))
  #|proc:write-string!
  The `write-string!` procedure writes `str` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `str` parameter is the string to write.
  The procedure truncates the file if it already exists.
  |#
  (define-who write-string!
    (lambda (path str)
      ($write-string who path 'truncate str)))
  #|proc:write-chars!
  The `write-chars!` procedure writes `chars` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `chars` parameter is the list of characters to write.
  The procedure truncates the file if it already exists.
  |#
  (define-who write-chars!
    (lambda (path chars)
      ($write-chars who path 'truncate chars)))
  #|proc:write-data!
  The `write-data!` procedure writes each datum in `data` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `data` parameter is the list of datums to write.
  The procedure truncates the file if it already exists.
  |#
  (define-who write-data!
    (lambda (path data)
      ($write-data who path 'truncate data)))
  #|proc:write-datum!
  The `write-datum!` procedure writes `datum` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `datum` parameter is the datum to write.
  The procedure truncates the file if it already exists.
  |#
  (define-who write-datum!
    (lambda (path datum)
      ($write-datum who path 'truncate datum)))
  #|proc:write-data-fasl!
  The `write-data-fasl!` procedure writes each datum in `data` to binary `path`.
  The `path` parameter is the filesystem path string to write.
  The `data` parameter is the list of datums to write in fasl format.
  The procedure truncates the file if it already exists.
  |#
  (define-who write-data-fasl!
    (lambda (path data)
      ($write-data-fasl who path 'truncate data)))
  #|proc:write-datum-fasl!
  The `write-datum-fasl!` procedure writes `datum` to binary `path` in fasl format.
  The `path` parameter is the filesystem path string to write.
  The `datum` parameter is the datum to write.
  The procedure truncates the file if it already exists.
  |#
  (define-who write-datum-fasl!
    (lambda (path datum)
      ($write-datum-fasl who path 'truncate datum)))
  #|proc:write-u8vec!
  The `write-u8vec!` procedure writes `u8vec` to the binary file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `u8vec` parameter is the bytevector to write.
  The procedure truncates the file if it already exists.
  |#
  (define-who write-u8vec!
    (lambda (path u8vec)
      ($write-u8vec who path 'truncate u8vec)))

  ;; append by default
  #|proc:write-lines>>
  The `write-lines>>` procedure writes `lines` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `lines` parameter is the list of strings to write, one per line.
  The procedure appends to the file if it already exists.
  |#
  (define-who write-lines>>
    (lambda (path lines)
      ($write-lines who path 'append lines)))
  #|proc:write-string>>
  The `write-string>>` procedure writes `str` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `str` parameter is the string to write.
  The procedure appends to the file if it already exists.
  |#
  (define-who write-string>>
    (lambda (path str)
      ($write-string who path 'append str)))
  #|proc:write-chars>>
  The `write-chars>>` procedure writes `chars` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `chars` parameter is the list of characters to write.
  The procedure appends to the file if it already exists.
  |#
  (define-who write-chars>>
    (lambda (path chars)
      ($write-chars who path 'append chars)))
  #|proc:write-data>>
  The `write-data>>` procedure writes each datum in `data` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `data` parameter is the list of datums to write.
  The procedure appends to the file if it already exists.
  |#
  (define-who write-data>>
    (lambda (path data)
      ($write-data who path 'append data)))
  #|proc:write-datum>>
  The `write-datum>>` procedure writes `datum` to the text file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `datum` parameter is the datum to write.
  The procedure appends to the file if it already exists.
  |#
  (define-who write-datum>>
    (lambda (path datum)
      ($write-datum who path 'append datum)))
  #|proc:write-data-fasl>>
  The `write-data-fasl>>` procedure writes each datum in `data` to binary `path`.
  The `path` parameter is the filesystem path string to write.
  The `data` parameter is the list of datums to write in fasl format.
  The procedure appends to the file if it already exists.
  |#
  (define-who write-data-fasl>>
    (lambda (path data)
      ($write-data-fasl who path 'append data)))
  #|proc:write-datum-fasl>>
  The `write-datum-fasl>>` procedure writes `datum` to binary `path` in fasl format.
  The `path` parameter is the filesystem path string to write.
  The `datum` parameter is the datum to write.
  The procedure appends to the file if it already exists.
  |#
  (define-who write-datum-fasl>>
    (lambda (path datum)
      ($write-datum-fasl who path 'append datum)))
  #|proc:write-u8vec>>
  The `write-u8vec>>` procedure writes `u8vec` to the binary file at `path`.
  The `path` parameter is the filesystem path string to write.
  The `u8vec` parameter is the bytevector to write.
  The procedure appends to the file if it already exists.
  |#
  (define-who write-u8vec>>
    (lambda (path u8vec)
      ($write-u8vec who path 'append u8vec)))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   get/put operations on ports
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define $bad-endianness
    (lambda (who e)
      (errorf who "bad endianness: ~a, should be either 'big or 'little" e)))
  (define $no-enough-data
    (lambda (who expected)
      (errorf who "failed to get ~a from port" expected)))
  (define $invalid-value
    (lambda (who n expected)
      (errorf who "invalid value: ~a, expected ~a" n expected)))

  ;; from ChezScheme bytevector.ss
  (define-syntax signed-value-pred
    (lambda (x)
      (syntax-case x ()
        [(_ ?bits)
         (let ([bits (syntax->datum #'?bits)])
           (unless (and (fixnum? bits)
                        (fx> bits 0)
                        (fx= (* (fxquotient bits 8) 8) bits))
             (syntax-error #'?bits "invalid bits"))
           (cond
            [(fx<= bits (fixnum-width))
             (with-syntax ([limit- (- (expt 2 (- bits 1)))]
                           [limit+ (- (expt 2 (- bits 1)) 1)])
               #'(lambda (k) (and (fixnum? k) (fx<= limit- k limit+))))]
            [(fx= bits (fixnum-width)) #'fixnum?]
            [else
             (with-syntax ([limit- (- (expt 2 (- bits 1)))]
                           [limit+ (- (expt 2 (- bits 1)) 1)])
               #'(lambda (k)
                   (or (fixnum? k)
                       (and (bignum? k) (<= limit- k limit+)))))]))])))
  (define-syntax unsigned-value-pred
    (lambda (x)
      (syntax-case x ()
        [(_ ?bits)
         (let ([bits (syntax->datum #'?bits)])
           (unless (and (fixnum? bits)
                        (fx> bits 0)
                        (fx= (* (fxquotient bits 8) 8) bits))
             (syntax-error #'?bits "invalid bits"))
           (cond
            [(fx< bits (fixnum-width))
             (with-syntax ([limit+ (expt 2 bits)])
               #'(lambda (k) (and (fixnum? k) (#%$fxu< k limit+))))]
            [(fx= bits (fixnum-width))
             #'(lambda (k) (and (fixnum? k) (fx>= k 0)))]
            [else
             (with-syntax ([limit+ (- (expt 2 bits) 1)])
               #'(lambda (k)
                   (if (fixnum? k)
                       (fx>= k 0)
                       (and (bignum? k) (<= 0 k limit+)))))]))])))
  (define u16? (unsigned-value-pred 16))
  (define u32? (unsigned-value-pred 32))
  (define u64? (unsigned-value-pred 64))
  (define s16? (signed-value-pred 16))
  (define s32? (signed-value-pred 32))
  (define s64? (signed-value-pred 64))


  ;; All subsequent procedures raise an exception when there are no enough
  ;; bytes to make up the number.

  (define $valid-endianness
    (lambda (who end)
      (unless (memq end '(big little)) ($bad-endianness who end))))

  (define $get-integer64
    (lambda (who port end expected signed?)
      ($valid-endianness who end)
      ;; Keep storage local so a custom port can reenter integer reads safely.
      (let ([bytes (get-bytevector-n port 8)])
        (unless (and (bytevector? bytes) (fx= 8 (bytevector-length bytes)))
          ($no-enough-data who expected))
        (if signed?
            (bytevector-s64-ref bytes 0 end)
            (bytevector-u64-ref bytes 0 end)))))

  (define $get-unsigned
    (lambda (who p count end expected)
      ($valid-endianness who end)
      (let* ([fixnum? (fx<= (fx* count 8) (fixnum-width))]
             [combine-big (if fixnum?
                              (lambda (value byte) (fx+ (fx* value 256) byte))
                              (lambda (value byte) (+ (* value 256) byte)))]
             [combine-little (if fixnum?
                                 (lambda (value byte factor)
                                   (fx+ value (fx* byte factor)))
                                 (lambda (value byte factor) (+ value (* byte factor))))]
             [multiply-256 (if fixnum? (lambda (value) (fx* value 256))
                               (lambda (value) (* value 256)))])
        (if (eq? end 'big)
            (let loop ([remaining count] [value 0])
              (if (fxzero? remaining)
                  value
                  (let ([byte (get-u8 p)])
                    (when (eof-object? byte) ($no-enough-data who expected))
                    (loop (fx1- remaining) (combine-big value byte)))))
            (let loop ([remaining count] [value 0] [factor 1])
              (if (fxzero? remaining)
                  value
                  (let ([byte (get-u8 p)])
                    (when (eof-object? byte) ($no-enough-data who expected))
                    (loop (fx1- remaining) (combine-little value byte factor)
                          (multiply-256 factor)))))))))

  (define $get-signed
    (lambda (who p count end expected)
      (let* ([unsigned ($get-unsigned who p count end expected)]
             [bits (fx* count 8)]
             [sign (expt 2 (fx- bits 1))])
        (if (>= unsigned sign) (- unsigned (expt 2 bits)) unsigned))))

  (define $put-unsigned
    (lambda (who p value count end)
      ($valid-endianness who end)
      (let emit ([value value] [remaining count])
        (let ([byte (if (fx<= (fx* count 8) (fixnum-width))
                        (fxmod value 256)
                        (mod value 256))]
              [rest (if (fx<= (fx* count 8) (fixnum-width))
                        (fxquotient value 256)
                        (quotient value 256))])
          (if (eq? end 'big)
              (begin
                (unless (fx= remaining 1) (emit rest (fx1- remaining)))
                (put-u8 p byte))
              (begin
                (put-u8 p byte)
                (unless (fx= remaining 1) (emit rest (fx1- remaining)))))))))

  (define $put-integer64
    (lambda (who port value end signed?)
      ($valid-endianness who end)
      ;; A custom output callback may reenter this codec before consuming these bytes.
      (let ([bytes (make-bytevector 8)])
        (if signed?
            (bytevector-s64-set! bytes 0 value end)
            (bytevector-u64-set! bytes 0 value end))
        (put-bytevector port bytes))))

  (define $put-value
    (lambda (who p value count end expected signed?)
      (let ([valid? (case count
                      [(2) (if signed? s16? u16?)]
                      [(4) (if signed? s32? u32?)]
                      [else (if signed? s64? u64?)])])
        (if (valid? value)
            (if (fx= count 8)
                ($put-integer64 who p value end signed?)
                ($put-unsigned who p
                               (if (and signed? (< value 0))
                                   (+ value (expt 2 (fx* count 8)))
                                   value)
                               count end))
            ($invalid-value who value expected)))))

  #|proc:get-u16
  The `get-u16` procedure reads an unsigned 16-bit integer from binary input port `p`.
  The `p` parameter is the binary input port to read.
  The `end` parameter is `'little` by default and may be `'big`.
  |#
  (define-who get-u16
    (case-lambda
      [(p) (get-u16 p 'little)]
      [(p end)
       (pcheck-input-binary-port (p) ($get-unsigned who p 2 end 'u16))]))
  #|proc:get-u32
  The `get-u32` procedure reads an unsigned 32-bit integer from binary input port `p`.
  The `p` parameter is the binary input port to read.
  The `end` parameter is `'little` by default and may be `'big`.
  |#
  (define-who get-u32
    (case-lambda
      [(p) (get-u32 p 'little)]
      [(p end)
       (pcheck-input-binary-port (p) ($get-unsigned who p 4 end 'u32))]))
  #|proc:get-u64
  The `get-u64` procedure reads an unsigned 64-bit integer from binary input port `p`.
  The `p` parameter is the binary input port to read.
  The `end` parameter is `'little` by default and may be `'big`.
  |#
  (define-who get-u64
    (case-lambda
      [(p) (get-u64 p 'little)]
      [(p end)
       (pcheck-input-binary-port (p) ($get-integer64 who p end 'u64 #f))]))

  #|proc:get-s16
  The `get-s16` procedure reads a signed 16-bit integer from binary input port `p`.
  The `p` parameter is the binary input port to read.
  The `end` parameter is `'little` by default and may be `'big`.
  |#
  (define-who get-s16
    (case-lambda
      [(p) (get-s16 p 'little)]
      [(p end)
       (pcheck-input-binary-port (p) ($get-signed who p 2 end 's16))]))
  #|proc:get-s32
  The `get-s32` procedure reads a signed 32-bit integer from binary input port `p`.
  The `p` parameter is the binary input port to read.
  The `end` parameter is `'little` by default and may be `'big`.
  |#
  (define-who get-s32
    (case-lambda
      [(p) (get-s32 p 'little)]
      [(p end)
       (pcheck-input-binary-port (p) ($get-signed who p 4 end 's32))]))
  #|proc:get-s64
  The `get-s64` procedure reads a signed 64-bit integer from binary input port `p`.
  The `p` parameter is the binary input port to read.
  The `end` parameter is `'little` by default and may be `'big`.
  |#
  (define-who get-s64
    (case-lambda
      [(p) (get-s64 p 'little)]
      [(p end)
       (pcheck-input-binary-port (p) ($get-integer64 who p end 's64 #t))]))

  #|proc:put-u16
  The `put-u16` procedure writes unsigned 16-bit integer `n` to binary output port `p`.
  The `p` parameter is the binary output port to write.
  The `n` parameter must fit the unsigned 16-bit width.
  The `end` parameter is `'little` by default and may be `'big`.
  |#
  (define-who put-u16
    (case-lambda
      [(p n) (put-u16 p n 'little)]
      [(p n end)
       (pcheck-output-binary-port
        (p)
        ($put-value who p n 2 end 'u16 #f))]))
  #|proc:put-u32
  The `put-u32` procedure writes unsigned 32-bit integer `n` to binary output port `p`.
  The `p` parameter is the binary output port to write.
  The `n` parameter must fit the unsigned 32-bit width.
  The `end` parameter is `'little` by default and may be `'big`.
  |#
  (define-who put-u32
    (case-lambda
      [(p n) (put-u32 p n 'little)]
      [(p n end)
       (pcheck-output-binary-port
        (p)
        ($put-value who p n 4 end 'u32 #f))]))
  #|proc:put-u64
  The `put-u64` procedure writes unsigned 64-bit integer `n` to binary output port `p`.
  The `p` parameter is the binary output port to write.
  The `n` parameter must fit the unsigned 64-bit width.
  The `end` parameter is `'little` by default and may be `'big`.
  |#
  (define-who put-u64
    (case-lambda
      [(p n) (put-u64 p n 'little)]
      [(p n end)
       (pcheck-output-binary-port
        (p)
        ($put-value who p n 8 end 'u64 #f))]))

  #|proc:put-s16
  The `put-s16` procedure writes signed 16-bit integer `n` to binary output port `p`.
  The `p` parameter is the binary output port to write.
  The `n` parameter must fit the signed 16-bit width.
  The `end` parameter is `'little` by default and may be `'big`.
  |#
  (define-who put-s16
    (case-lambda
      [(p n) (put-s16 p n 'little)]
      [(p n end)
       (pcheck-output-binary-port
        (p)
        ($put-value who p n 2 end 's16 #t))]))
  #|proc:put-s32
  The `put-s32` procedure writes signed 32-bit integer `n` to binary output port `p`.
  The `p` parameter is the binary output port to write.
  The `n` parameter must fit the signed 32-bit width.
  The `end` parameter is `'little` by default and may be `'big`.
  |#
  (define-who put-s32
    (case-lambda
      [(p n) (put-s32 p n 'little)]
      [(p n end)
       (pcheck-output-binary-port
        (p)
        ($put-value who p n 4 end 's32 #t))]))
  #|proc:put-s64
  The `put-s64` procedure writes signed 64-bit integer `n` to binary output port `p`.
  The `p` parameter is the binary output port to write.
  The `n` parameter must fit the signed 64-bit width.
  The `end` parameter is `'little` by default and may be `'big`.
  |#
  (define-who put-s64
    (case-lambda
      [(p n) (put-s64 p n 'little)]
      [(p n end)
       (pcheck-output-binary-port
        (p)
        ($put-value who p n 8 end 's64 #t))]))


  ;; big-endian aliases
  #|proc:GET-U16
  The `GET-U16` procedure is a big-endian alias for `get-u16`.
  The `p` parameter is the binary input port to read.
  |#
  (define GET-U16 (lambda (p) (get-u16 p 'big)))
  #|proc:GET-U32
  The `GET-U32` procedure is a big-endian alias for `get-u32`.
  The `p` parameter is the binary input port to read.
  |#
  (define GET-U32 (lambda (p) (get-u32 p 'big)))
  #|proc:GET-U64
  The `GET-U64` procedure is a big-endian alias for `get-u64`.
  The `p` parameter is the binary input port to read.
  |#
  (define GET-U64 (lambda (p) (get-u64 p 'big)))
  #|proc:GET-S16
  The `GET-S16` procedure is a big-endian alias for `get-s16`.
  The `p` parameter is the binary input port to read.
  |#
  (define GET-S16 (lambda (p) (get-s16 p 'big)))
  #|proc:GET-S32
  The `GET-S32` procedure is a big-endian alias for `get-s32`.
  The `p` parameter is the binary input port to read.
  |#
  (define GET-S32 (lambda (p) (get-s32 p 'big)))
  #|proc:GET-S64
  The `GET-S64` procedure is a big-endian alias for `get-s64`.
  The `p` parameter is the binary input port to read.
  |#
  (define GET-S64 (lambda (p) (get-s64 p 'big)))

  #|proc:PUT-U16
  The `PUT-U16` procedure is a big-endian alias for `put-u16`.
  The `p` parameter is the binary output port to write.
  The `n` parameter must fit the unsigned 16-bit width.
  |#
  (define PUT-U16 (lambda (p n) (put-u16 p n 'big)))
  #|proc:PUT-U32
  The `PUT-U32` procedure is a big-endian alias for `put-u32`.
  The `p` parameter is the binary output port to write.
  The `n` parameter must fit the unsigned 32-bit width.
  |#
  (define PUT-U32 (lambda (p n) (put-u32 p n 'big)))
  #|proc:PUT-U64
  The `PUT-U64` procedure is a big-endian alias for `put-u64`.
  The `p` parameter is the binary output port to write.
  The `n` parameter must fit the unsigned 64-bit width.
  |#
  (define PUT-U64 (lambda (p n) (put-u64 p n 'big)))
  #|proc:PUT-S16
  The `PUT-S16` procedure is a big-endian alias for `put-s16`.
  The `p` parameter is the binary output port to write.
  The `n` parameter must fit the signed 16-bit width.
  |#
  (define PUT-S16 (lambda (p n) (put-s16 p n 'big)))
  #|proc:PUT-S32
  The `PUT-S32` procedure is a big-endian alias for `put-s32`.
  The `p` parameter is the binary output port to write.
  The `n` parameter must fit the signed 32-bit width.
  |#
  (define PUT-S32 (lambda (p n) (put-s32 p n 'big)))
  #|proc:PUT-S64
  The `PUT-S64` procedure is a big-endian alias for `put-s64`.
  The `p` parameter is the binary output port to write.
  The `n` parameter must fit the signed 64-bit width.
  |#
  (define PUT-S64 (lambda (p n) (put-s64 p n 'big)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   path utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  #|proc:make-path
  Combine string `first-path` and the remaining string `paths` with Chez `path-build` rules.
  Insert the host directory separator after a nonempty prefix that does not end in a separator.
  Fragments, including rooted strings, are appended as written; no normalization is performed.
  Return the resulting string. Use `path-join` on path objects for root-aware joining.
  |#
  (define make-path
    (lambda (first-path . paths)
      (pcheck ([$filename? first-path] [(lambda (values) (andmap $filename? values)) paths])
        (cond [(null? paths) first-path]
              [(null? (cdr paths)) (path-build first-path (car paths))]
              [else
               (let* ([separator (directory-separator)] [parts (cons first-path paths)]
                      [length
                       (let loop ([parts parts] [total 0] [last #f])
                         (if (null? parts) total
                             (let* ([part (car parts)] [length (string-length part)]
                                    [separate? (and last (not (directory-separator? last)))])
                               (loop (cdr parts) (+ total length (if separate? 1 0))
                                     (cond [(fx> length 0) (string-ref part (fx1- length))]
                                           [separate? separator]
                                           [else last])))))]
                      [result (make-string length)])
                 ;; Keep native separator decisions and fill one preallocated result.
                 (let loop ([parts parts] [index 0])
                   (if (null? parts) result
                       (let* ([index
                               (if (and (fx> index 0)
                                        (not (directory-separator?
                                              (string-ref result (fx1- index)))))
                                   (begin (string-set! result index separator) (fx1+ index))
                                   index)]
                              [part (car parts)] [length (string-length part)])
                         (string-copy! part 0 result index length)
                         (loop (cdr parts) (fx+ index length))))))]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   file info
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; file stats

  #|proc:file-stat?
  Return whether arbitrary `object` is a filesystem metadata snapshot returned by `file-stat`.
  |#
  (define-record-type ($file-stat make-$file-stat file-stat?)
    (nongenerative)
    (fields (immutable type   $file-stat-type)
            (immutable mode   $file-stat-mode)
            (immutable inode  $file-stat-inode)
            (immutable owner  $file-stat-owner)
            (immutable group  $file-stat-group)
            (immutable nlinks $file-stat-nlinks)
            (immutable size   $file-stat-size)
            (immutable blocks $file-stat-blocks)
            (immutable dev-major $file-stat-dev-major)
            (immutable dev-minor $file-stat-dev-minor)
            ;;(immutable attributes  file-stat-attributes)
            (immutable access-time       $file-stat-access-time)
            (immutable modification-time $file-stat-modification-time)
            (immutable change-time       $file-stat-change-time)
            (immutable creation-time     $file-stat-creation-time)))

  ;; Preserve checked public boundaries when record accessors compile without checks.
  (define-syntax $define-stat-accessor
    (syntax-rules ()
      [(_ name raw)
       (define name
         (lambda (stat)
           (pcheck ([file-stat? stat]) (raw stat))))]))

  #|proc:file-stat-type
  Return the numeric file-type bits in snapshot `stat`. Use `file-type->symbol` to name them.
  |#
  ($define-stat-accessor file-stat-type $file-stat-type)

  #|proc:file-stat-mode
  Return the integer permission and special mode bits in snapshot `stat`.
  |#
  ($define-stat-accessor file-stat-mode $file-stat-mode)

  #|proc:file-stat-inode
  Return the nonnegative integer inode number in snapshot `stat`.
  |#
  ($define-stat-accessor file-stat-inode $file-stat-inode)

  #|proc:file-stat-owner
  Return the nonnegative integer user ID of the owner in snapshot `stat`.
  |#
  ($define-stat-accessor file-stat-owner $file-stat-owner)

  #|proc:file-stat-group
  Return the nonnegative integer group ID in snapshot `stat`.
  |#
  ($define-stat-accessor file-stat-group $file-stat-group)

  #|proc:file-stat-nlinks
  Return the nonnegative integer hard-link count in snapshot `stat`.
  |#
  ($define-stat-accessor file-stat-nlinks $file-stat-nlinks)

  #|proc:file-stat-size
  Return the nonnegative integer size in bytes in snapshot `stat`.
  |#
  ($define-stat-accessor file-stat-size $file-stat-size)

  #|proc:file-stat-blocks
  Return the nonnegative integer count of allocated 512-byte blocks in snapshot `stat`.
  |#
  ($define-stat-accessor file-stat-blocks $file-stat-blocks)

  #|proc:file-stat-dev-major
  Return the nonnegative integer major number of the containing device in snapshot `stat`.
  |#
  ($define-stat-accessor file-stat-dev-major $file-stat-dev-major)

  #|proc:file-stat-dev-minor
  Return the nonnegative integer minor number of the containing device in snapshot `stat`.
  |#
  ($define-stat-accessor file-stat-dev-minor $file-stat-dev-minor)

  #|proc:file-stat-access-time
  Return the recorded access time in snapshot `stat`, or `#f` when unavailable.
  The result is a Chez `time-utc` object; no filesystem call is made.
  |#
  ($define-stat-accessor file-stat-access-time $file-stat-access-time)

  #|proc:file-stat-modification-time
  Return the recorded content modification time in snapshot `stat`, or `#f` when unavailable.
  The result is a Chez `time-utc` object; no filesystem call is made.
  |#
  ($define-stat-accessor file-stat-modification-time $file-stat-modification-time)

  #|proc:file-stat-change-time
  Return the recorded metadata change time in snapshot `stat`, or `#f` when unavailable.
  The result is a Chez `time-utc` object; no filesystem call is made.
  |#
  ($define-stat-accessor file-stat-change-time $file-stat-change-time)

  #|proc:file-stat-creation-time
  Return the recorded creation time in snapshot `stat`, or `#f` when unavailable.
  The result is a Chez `time-utc` object; no filesystem call is made.
  |#
  ($define-stat-accessor file-stat-creation-time $file-stat-creation-time)

  #|FT_unknown
  The symbol identifying unavailable or unrecognized filesystem entry types.
  |#
  (define FT_unknown 'FT_unknown)

  (define *file-types* '((#o140000 . FT_socket)
                         (#o120000 . FT_symlink)
                         (#o100000 . FT_regular)
                         (#o60000  . FT_block)
                         (#o40000  . FT_dir)
                         (#o20000  . FT_chardev)
                         (#o10000  . FT_fifo)
                         (#f        . FT_unknown)))

  #|proc:file-type->symbol
  Convert numeric `type` from stat metadata to its `FT_*` symbol. A missing
  type (`#f`) or an unrecognized integer returns `FT_unknown`.
  |#
  (define-who file-type->symbol
    (lambda (type)
      (pcheck ([(lambda (value) (or (eq? value #f) (integer? value))) type])
              (let ([entry (assoc type *file-types*)])
                (if entry (cdr entry) 'FT_unknown)))))

;;;;===----------------------------------------------------------------------===
;;;; Lazy Linux directory streams and filesystem traversal
;;;;===----------------------------------------------------------------------===

  (define-record-type ($fs-directory %make-fs-directory $fs-directory?)
    (fields (mutable handle fs-directory-handle fs-directory-handle-set!)
            (immutable path fs-directory-path)
            (mutable closed? $fs-directory-closed? fs-directory-closed?-set!))
    (opaque #t)
    (sealed #t))

  (define-condition-type &$filename-encoding &i/o-filename
    $make-filename-encoding-error $filename-encoding-error?)

  (define $fs-ensure-linux
    (let* ([name (symbol->string (machine-type))] [length (string-length name)]
           [supported? (and (fx>= length 2)
                            (string=? "le" (substring name (fx- length 2) length)))])
      (lambda (who)
        (unless supported?
          (errorf who "local directory iteration is unsupported on this platform")))))

  #|proc:fs-directory?
  Return `#t` when `object` is a local directory stream, including closed streams;
  return `#f` for other objects.
  |#
  (define fs-directory?
    (lambda (object)
      (pcheck () ($fs-directory? object))))

  #|proc:fs-directory-closed?
  Return `#t` when directory stream `directory` has been closed, otherwise `#f`.
  |#
  (define fs-directory-closed?
    (lambda (directory)
      (pcheck ([fs-directory? directory]) ($fs-directory-closed? directory))))

  #|proc:fs-open-directory
  Open local directory named by string `path` and return an opaque directory stream.
  Linux is required. A missing or inaccessible directory raises an error.
  Serialize operations on the same stream; do not close it while another thread reads it.
  |#
  (define fs-open-directory
    (lambda (path)
      (pcheck ([$filename? path])
              ($fs-ensure-linux 'fs-open-directory)
              (let* ([open (foreign-procedure "cspp_fs_open_directory" (string) uptr)]
                     [handle (open path)])
                (if (= handle 0)
                    ($err-file 'fs-open-directory path "cannot open directory")
                    (%make-fs-directory handle (string-copy path) #f))))))

  #|proc:fs-read-directory
  Read one entry from open stream `directory`. Return `(entry-name . entry-type)`,
  or `#f` at EOF. Types are `FT_*` symbols; unrecognized types are `FT_unknown`.
  Entries include `.` and `..`, as returned by Linux `readdir`.
  Reading a closed stream raises an error.
  An entry name containing invalid UTF-8 bytes raises an error.
  |#
  (define fs-read-directory
    (lambda (directory)
      (pcheck ([fs-directory? directory])
              ($fs-ensure-linux 'fs-read-directory)
              (if (fs-directory-closed? directory)
                  (errorf 'fs-read-directory "directory is closed")
                  (let* ([read (foreign-procedure "cspp_fs_read_directory" (uptr) scheme-object)]
                         [value (read (fs-directory-handle directory))])
                    (cond [(eq? value #f) #f]
                          [(eqv? value -1)
                           ($err-file 'fs-read-directory
                                      ($make-filename-encoding-error
                                       (fs-directory-path directory))
                                      "filesystem entry name is not valid UTF-8")]
                          [(string? value) ($err-file 'fs-read-directory
                                                     (fs-directory-path directory) value)]
                          [else
                           ;; The native result is fresh; translate its type in place.
                           (set-cdr! value
                                     (case (cdr value)
                                       [(4) 'FT_dir] [(8) 'FT_regular]
                                       [(10) 'FT_symlink] [(6) 'FT_block]
                                       [(2) 'FT_chardev] [(1) 'FT_fifo]
                                       [(12) 'FT_socket] [else 'FT_unknown]))
                           value]))))))

  #|proc:fs-close-directory
  Close stream `directory` and return the same object. Closing an already closed
  stream is harmless. Linux is required.
  |#
  (define fs-close-directory
    (lambda (directory)
      (pcheck ([fs-directory? directory])
              ($fs-ensure-linux 'fs-close-directory)
              (unless (fs-directory-closed? directory)
                (let* ([close (foreign-procedure "cspp_fs_close_directory" (uptr) scheme-object)]
                       [result (close (fs-directory-handle directory))])
                  ;; closedir consumes the stream even if closing its descriptor fails.
                  (fs-directory-handle-set! directory #f)
                  (fs-directory-closed?-set! directory #t)
                  (when (string? result)
                    ($err-file 'fs-close-directory (fs-directory-path directory) result))))
              directory)))

  (define $fs-entry-directory?
    (lambda (who path type follow-link?)
      (or (eq? type 'FT_dir)
          (and (or (eq? type 'FT_unknown) (and (eq? type 'FT_symlink) follow-link?))
               (eq? ($file-type who path follow-link?) 'FT_dir)))))

  ;; Use the checked native stream for every traversal, including eager walkers.
  ;; Chez directory-list replaces invalid UTF-8 and strips a leading UTF-8 BOM.
  ;; Preserve each fresh entry pair as (full-path . type) for callers that classify entries.
  (define $directory-entries
    (case-lambda
      [(path) ($directory-entries path #f)]
      [(path on-invalid)
       (let ([directory (fs-open-directory path)] [entries (make-list-builder)])
         (define next-entry
           (lambda ()
             (guard (failure [(and on-invalid ($filename-encoding-error? failure))
                              (on-invalid)
                              (next-entry)])
               (fs-read-directory directory))))
         (dynamic-wind
           void
           (lambda ()
             (let loop ([entry (next-entry)])
               (if entry
                   (begin
                     (unless (member (car entry) '("." ".."))
                       (set-car! entry (path-build path (car entry)))
                       (entries entry))
                     (loop (next-entry)))
                   (entries))))
           (lambda () (fs-close-directory directory))))]))

  (define $directory-children
    (lambda (path follow-link?)
      (let ([directories (make-list-builder)] [files (make-list-builder)])
        (for-each
         (lambda (entry)
           ((if ($fs-entry-directory? 'walk-files (car entry) (cdr entry) follow-link?)
                directories files)
            (car entry)))
         ($directory-entries path))
        (values (directories) (files)))))

  (define $file-identity
    (lambda (path follow-link?)
      (let ([stat (file-stat path follow-link?)])
        (list (file-stat-dev-major stat) (file-stat-dev-minor stat)
              (file-stat-inode stat)))))

  (define $directory-view
    (lambda (who path)
      ($file-view-key who path ($file-identity path #t))))

  #|proc:fs->iter
  Return an iterator lazily traversing directory `path`, including the root itself.
  `follow-link?` (default `#f`) enables symlink descent with directory cycle detection.
  `top-down?` (default `#t`) yields directories before children; `#f` yields them after.
  Results are unsorted path strings, with each entry yielded once. Directory handles
  open on demand and close on exhaustion, reset, errors, or `iter-finalize!`.
  Reset starts a fresh pass over the current filesystem. Linux is required.
  Distinct directory aliases are traversed separately; only ancestor cycles are skipped.
  Cycle checks distinguish Linux mount views. If mount IDs are unavailable, an ambiguous
  repeated directory identity raises an I/O error instead of omitting its descendants.
  A root symlink is followed even when `follow-link?` is `#f`; that flag controls descendants.
  Input and yielded path strings do not share storage with the iterator's traversal state.
  For a relative `path`, keep the process current directory unchanged from the first advance
  until exhaustion or finalization. Reset closes the current pass; the next pass uses the
  current directory at its first advance.
  |#
  (define fs->iter
    (case-lambda
      [(path) (fs->iter path #f #t)]
      [(path follow-link?) (fs->iter path follow-link? #t)]
      [(path follow-link? top-down?)
       (pcheck ([$filename? path] [boolean? follow-link? top-down?])
               ($fs-ensure-linux 'fs->iter)
               ;; Frames hold a private path, lazily opened stream, and ancestor identities.
               (let* ([root-path (string-copy path)]
                      [stack (list (vector root-path #f '()))]
                      [finished? #f])
                 (define close-all
                   (lambda ()
                     (let ([frames stack] [failure #f])
                       (set! stack '())
                       (for-each
                        (lambda (frame)
                          (let ([directory (vector-ref frame 1)])
                            (when directory
                              (guard (condition [else (unless failure (set! failure condition))])
                                (fs-close-directory directory)))))
                        frames)
                       (when failure (raise failure)))))
                 (make-iter
                  (lambda ()
                    (guard (condition
                            [else
                             (set! finished? #t)
                             (guard (cleanup-condition [else (void)]) (close-all))
                             (raise condition)])
                      (let loop ()
                        (if (or finished? (null? stack))
                            (begin (set! finished? #t) iter-end)
                            (let* ([frame (car stack)]
                                   [root (vector-ref frame 0)]
                                   [directory (vector-ref frame 1)])
                              (if (not directory)
                                  (let ([identity (and follow-link? ($directory-view 'fs->iter root))]
                                        [ancestors (vector-ref frame 2)])
                                    (if (and identity
                                             ($file-view-seen? 'fs->iter root identity ancestors))
                                        (begin
                                          (set! stack (cdr stack))
                                          (string-copy root))
                                        (begin
                                          (vector-set! frame 2
                                                       (if identity
                                                           (cons identity ancestors) ancestors))
                                          (vector-set! frame 1 (fs-open-directory root))
                                          (if top-down? (string-copy root) (loop)))))
                                  (let ([entry (fs-read-directory directory)])
                                    (cond
                                     [(not entry)
                                      (fs-close-directory directory)
                                      (set! stack (cdr stack))
                                      (if top-down? (loop) (string-copy root))]
                                     [(member (car entry) '("." "..")) (loop)]
                                     [else
                                      (let* ([child (path-build root (car entry))]
                                             [directory? ($fs-entry-directory?
                                                          'fs->iter child (cdr entry) follow-link?)])
                                        (if directory?
                                            (begin
                                              (set! stack
                                                    (cons (vector child #f (vector-ref frame 2))
                                                          stack))
                                              (loop))
                                            child))]))))))))
                  (lambda ()
                    (close-all)
                    (set! stack (list (vector root-path #f '())))
                    (set! finished? #f))
                  close-all)))]))

  (define $absolute-file-path
    (lambda (path)
      (path-absolute (path-parse 'unix (current-directory))
                     (path-parse 'unix path))))

  (define $statx
    (let ([ffi-statx (foreign-procedure "cspp_statx" (string boolean) scheme-object)])
      (lambda (who path follow-link?)
        (pcheck ([$filename? path] [boolean? follow-link?])
                (let ([x (ffi-statx path follow-link?)])
                  (if (vector? x)
                      x
                      ($err-file who path x)))))))
  ;; stx_attributes, stx_mnt_id, etc, are left out
  (define $statx-type (lambda (v) (vector-ref v 0)))
  (define $statx-mode (lambda (v) (vector-ref v 1)))
  (define $statx-uid  (lambda (v) (vector-ref v 3)))
  (define $statx-gid  (lambda (v) (vector-ref v 4)))
  (define $statx-nlinks (lambda (v) (vector-ref v 2)))
  (define $statx-inode  (lambda (v) (vector-ref v 13)))
  (define $statx-size   (lambda (v) (vector-ref v 14)))
  (define $statx-blocks (lambda (v) (vector-ref v 15)))
  (define $statx-atime  (lambda (v) (let ([t (vector-ref v 5)])
                                      (and t (make-time 'time-utc (vector-ref v 6) t)))))
  (define $statx-ctime  (lambda (v) (let ([t (vector-ref v 7)])
                                      (and t (make-time 'time-utc (vector-ref v 8) t)))))
  (define $statx-mtime  (lambda (v) (let ([t (vector-ref v 9)])
                                      (and t (make-time 'time-utc (vector-ref v 10) t)))))
  (define $statx-btime  (lambda (v) (let ([t (vector-ref v 11)])
                                      (and t (make-time 'time-utc (vector-ref v 12) t)))))
  (define $statx-dev-major (lambda (v) (vector-ref v 16)))
  (define $statx-dev-minor (lambda (v) (vector-ref v 17)))

  #|proc:file-stat
  Read filesystem metadata for string `path` and return a snapshot accepted by `file-stat?`.
  Boolean `follow-link?` defaults to `#t`; use `#f` to inspect a symlink itself. Missing or
  inaccessible paths raise an error. The `file-stat-` accessors read the snapshot without
  querying the filesystem again. Timestamp objects are shared with the snapshot.
  |#
  (define-who file-stat
    (case-lambda [(path)
                  (file-stat path #t)]
                 [(path follow-link?)
                  (let* ([v ($statx who path follow-link?)]
                         [type ($statx-type v)]
                         [mode ($statx-mode v)]
                         [uid  ($statx-uid v)]
                         [gid  ($statx-gid v)]
                         [nlinks ($statx-nlinks v)]
                         [inode  ($statx-inode v)]
                         [size   ($statx-size v)]
                         [blocks ($statx-blocks v)]
                         [atime ($statx-atime v)]
                         [ctime ($statx-ctime v)]
                         [mtime ($statx-mtime v)]
                         [btime ($statx-btime v)]
                         [dev-major ($statx-dev-major v)]
                         [dev-minor ($statx-dev-minor v)])
                    (make-$file-stat
                     type mode inode uid gid nlinks size blocks dev-major dev-minor
                     atime mtime ctime btime))]))


  #|proc:file-access-time
  Return the last access timestamp of the selected filesystem entry.
  Path string `path` names the entry. Boolean `follow-link?` defaults to `#t`;
  `#f` inspects a symbolic link itself. Missing or inaccessible entries raise errors.
  The result is a Chez `time-utc` object, or `#f` when unavailable.
  |#
  (define-who file-access-time
    (case-lambda [(path)
                  ($statx-atime ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-atime ($statx who path follow-link?))]))
  #|proc:file-modification-time
  Return the content modification timestamp of the selected filesystem entry.
  Path string `path` names the entry. Boolean `follow-link?` defaults to `#t`;
  `#f` inspects a symbolic link itself. Missing or inaccessible entries raise errors.
  The result is a Chez `time-utc` object, or `#f` when unavailable.
  |#
  (define-who file-modification-time
    (case-lambda [(path)
                  ($statx-mtime ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-mtime ($statx who path follow-link?))]))
  #|proc:file-change-time
  Return the metadata change timestamp of the selected filesystem entry.
  Path string `path` names the entry. Boolean `follow-link?` defaults to `#t`;
  `#f` inspects a symbolic link itself. Missing or inaccessible entries raise errors.
  The result is a Chez `time-utc` object, or `#f` when unavailable.
  |#
  (define-who file-change-time
    (case-lambda [(path)
                  ($statx-ctime ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-ctime ($statx who path follow-link?))]))
  #|proc:file-creation-time
  Return the creation timestamp of the selected filesystem entry.
  Path string `path` names the entry. Boolean `follow-link?` defaults to `#t`;
  `#f` inspects a symbolic link itself. Missing or inaccessible entries raise errors.
  The result is a Chez `time-utc` object, or `#f` when unavailable.
  |#
  (define-who file-creation-time
    (case-lambda [(path)
                  ($statx-btime ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-btime ($statx who path follow-link?))]))
  #|proc:file-type
  Return the selected filesystem entry's type as an `FT_*` symbol.
  Path string `path` names the entry. Boolean `follow-link?` defaults to `#t`;
  `#f` inspects a symbolic link itself. Missing or inaccessible entries raise errors.
  Unrecognized or unavailable type bits produce `FT_unknown`.
  |#
  (define-who file-type
    (case-lambda [(path)
                  (file-type->symbol ($statx-type ($statx who path #t)))]
                 [(path follow-link?)
                  (file-type->symbol ($statx-type ($statx who path follow-link?)))]))
  #|proc:file-mode
  Return the selected filesystem entry's integer permission and special mode bits.
  Path string `path` names the entry. Boolean `follow-link?` defaults to `#t`;
  `#f` inspects a symbolic link itself. Missing or inaccessible entries raise errors.
  The result is `#f` when those bits are unavailable.
  |#
  (define-who file-mode
    (case-lambda [(path)
                  ($statx-mode ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-mode ($statx who path follow-link?))]))
  #|proc:file-inode
  Return the selected filesystem entry's nonnegative exact inode number.
  Path string `path` names the entry. Boolean `follow-link?` defaults to `#t`;
  `#f` inspects a symbolic link itself. Missing or inaccessible entries raise errors.
  The result is `#f` when unavailable.
  |#
  (define-who file-inode
    (case-lambda [(path)
                  ($statx-inode ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-inode ($statx who path follow-link?))]))
  #|proc:file-nlinks
  Return the selected filesystem entry's nonnegative exact hard-link count.
  Path string `path` names the entry. Boolean `follow-link?` defaults to `#t`;
  `#f` inspects a symbolic link itself. Missing or inaccessible entries raise errors.
  The result is `#f` when unavailable.
  |#
  (define-who file-nlinks
    (case-lambda [(path)
                  ($statx-nlinks ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-nlinks ($statx who path follow-link?))]))
  #|proc:file-dev-major
  Return the nonnegative exact major number of the entry's containing device.
  Path string `path` names the entry. Boolean `follow-link?` defaults to `#t`;
  `#f` inspects a symbolic link itself. Missing or inaccessible entries raise errors.
  The device identity is reported independently of file type.
  |#
  (define-who file-dev-major
    (case-lambda [(path)
                  ($statx-dev-major ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-dev-major ($statx who path follow-link?))]))
  #|proc:file-dev-minor
  Return the nonnegative exact minor number of the entry's containing device.
  Path string `path` names the entry. Boolean `follow-link?` defaults to `#t`;
  `#f` inspects a symbolic link itself. Missing or inaccessible entries raise errors.
  The device identity is reported independently of file type.
  |#
  (define-who file-dev-minor
    (case-lambda [(path)
                  ($statx-dev-minor ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-dev-minor ($statx who path follow-link?))]))
  #|proc:file-blocks
  Return the selected entry's nonnegative exact count of allocated 512-byte blocks.
  Path string `path` names the entry. Boolean `follow-link?` defaults to `#t`;
  `#f` inspects a symbolic link itself. Missing or inaccessible entries raise errors.
  The result is `#f` when unavailable; sparse files can use fewer bytes than their size.
  |#
  (define-who file-blocks
    (case-lambda [(path)
                  ($statx-blocks ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-blocks ($statx who path follow-link?))]))

  #|proc:file-owner
  Return the selected filesystem entry's nonnegative exact owner user ID.
  Path string `path` names the entry. Boolean `follow-link?` defaults to `#t`;
  `#f` inspects a symbolic link itself. Missing or inaccessible entries raise errors.
  The result is `#f` when unavailable.
  |#
  (define-who file-owner
    (case-lambda [(path)
                  ($statx-uid ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-uid ($statx who path follow-link?))]))
  #|proc:file-group
  Return the selected filesystem entry's nonnegative exact owner group ID.
  Path string `path` names the entry. Boolean `follow-link?` defaults to `#t`;
  `#f` inspects a symbolic link itself. Missing or inaccessible entries raise errors.
  The result is `#f` when unavailable.
  |#
  (define-who file-group
    (case-lambda [(path)
                  ($statx-gid ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-gid ($statx who path follow-link?))]))


  (define $file-size
    (lambda (who path)
      ($statx-size ($statx who path #t))))

  #|proc:file-size
  Return the byte size of the filesystem entry at `path`, following symbolic links.
  `path` is the path string to inspect. The result is a nonnegative exact integer.
  |#
  (define-who file-size
    (lambda (path) ($file-size who path)))

  #|proc:file-size-h
  Return a human-readable string for the byte size of the filesystem entry at `path`.
  `path` is the path string to inspect; symbolic links are followed.
  |#
  (define-who file-size-h
    (lambda (path)
      (define 1K 1024)
      (define 1M (* 1K 1024))
      (define 1G (* 1M 1024))
      (let ([size ($file-size who path)])
        (cond [(< size 1K) (format "~,2f B" size)]
              [(< size 1M) (format "~,2f KiB" (inexact (/ size 1K)))]
              [(< size 1G) (format "~,2f MiB" (inexact (/ size 1M)))]
              [else (format "~,2f GiB" (inexact (/ size 1G)))]))))


  (define $mode-any-set?
    (lambda (path follow-link? bits)
      (not (fx= 0 (fxlogand (get-mode path follow-link?) bits)))))

  #|proc:file-readable?
  Return whether any user, group, or other read permission bit is set on path string `path`.
  Boolean `follow-link?` defaults to `#t`. The selected entry must exist.
  This inspects mode bits, not effective access for the current user or ACL permissions.
  |#
  (define-who file-readable?
    (case-lambda [(path) (file-readable? path #t)]
                 [(path follow-link?)
                  (pcheck ([$filename? path] [boolean? follow-link?]
                           [(lambda (x) (file-exists? x follow-link?)) path])
                          ($mode-any-set? path follow-link? #o444))]))
  #|proc:file-writable?
  Return whether any user, group, or other write permission bit is set on path string `path`.
  Boolean `follow-link?` defaults to `#t`. The selected entry must exist.
  This inspects mode bits, not effective access for the current user or ACL permissions.
  |#
  (define-who file-writable?
    (case-lambda [(path) (file-writable? path #t)]
                 [(path follow-link?)
                  (pcheck ([$filename? path] [boolean? follow-link?]
                           [(lambda (x) (file-exists? x follow-link?)) path])
                          ($mode-any-set? path follow-link? #o222))]))
  #|proc:file-executable?
  Return whether any user, group, or other execute permission bit is set on path string `path`.
  Boolean `follow-link?` defaults to `#t`. The selected entry must exist.
  This inspects mode bits, not effective access for the current user or ACL permissions.
  |#
  (define-who file-executable?
    (case-lambda [(path) (file-executable? path #t)]
                 [(path follow-link?)
                  (pcheck ([$filename? path] [boolean? follow-link?]
                           [(lambda (x) (file-exists? x follow-link?)) path])
                          ($mode-any-set? path follow-link? #o111))]))

  #|proc:file-hidden?
  Return whether filesystem path string `path` has a hidden basename on Unix.
  Boolean `follow-link?` defaults to `#t`; when true, a symlink uses its resolved target's name.
  A hidden basename starts with a dot and is neither `.` nor `..`. The selected entry must exist.
  |#
  (define-who file-hidden?
    (case-lambda [(path) (file-hidden? path #t)]
                 [(path follow-link?)
                  (pcheck ([$filename? path] [boolean? follow-link?]
                           [(lambda (x) (file-exists? x follow-link?)) path])
                          (cond
                           [(unix?)
                            (let* ([expanded ($expand-file-path who path)]
                                   [selected (if (and follow-link?
                                                      (file-symbolic-link? expanded))
                                                 (readlink2 expanded #t) expanded)]
                                   [basename (path-basename (path-parse 'unix selected))])
                              (and basename (not (member basename '("." "..")))
                                   (char=? #\. (string-ref basename 0))))]
                           [else (todo who)]))]))

  #|proc:file-special?
  A file is special if it is either a socket, a block/character device, or a FIFO.
  |#
  (define-who file-special?
    (case-lambda
      [(path) (file-special? path #t)]
      [(path follow-link?)
       (pcheck ([$filename? path] [boolean? follow-link?]
                [(lambda (x) (file-exists? x follow-link?)) path])
               (let ([t (file-type path follow-link?)])
                 (bool (memq t '(FT_socket FT_block FT_chardev FT_fifo)))))]))


  #|proc:same-file?
  Test whether the two paths, `path1` and `path2`, refer to the same file.

  On Linux, this procedure compares the files' inodes and their filesystem IDs.
  |#
  (define-who same-file?
    (case-lambda
      [(path1 path2) (same-file? path1 path2 #t)]
      [(path1 path2 follow-link?)
       (pcheck ([$filename? path1 path2] [boolean? follow-link?]
                [(lambda (x) (file-exists? x follow-link?)) path1 path2])
               (let* ([st1 (file-stat path1 follow-link?)]
                      [st2 (file-stat path2 follow-link?)]
                      [inode1 (file-stat-inode st1)]
                      [major1 (file-stat-dev-major st1)]
                      [minor1 (file-stat-dev-minor st1)]
                      [inode2 (file-stat-inode st2)]
                      [major2 (file-stat-dev-major st2)]
                      [minor2 (file-stat-dev-minor st2)])
                 (and (= inode1 inode2) (= major1 major2) (= minor1 minor2))))]))


  (define $same-file-port-contents?
    (lambda (left right)
      (let ([length (file-length left)])
        ;; Reported sizes are hints only: virtual files can report zero.
        ;; Empty files and an immediate mismatch need no buffers.
        (let ([first-left (get-u8 left)] [first-right (get-u8 right)])
          (cond [(eof-object? first-left) (eof-object? first-right)]
                [(or (eof-object? first-right) (not (fx= first-left first-right))) #f]
                [else
                 (let* ([capacity (if (zero? length) 16384 (min length 16384))]
                        [left-buffer (make-bytevector capacity 0)]
                        [right-buffer (make-bytevector capacity 0)])
                   (let loop ()
                     (let ([left-count (get-bytevector-n! left left-buffer 0 capacity)]
                           [right-count (get-bytevector-n! right right-buffer 0 capacity)])
                       (if (eof-object? left-count)
                           (eof-object? right-count)
                           (and (eqv? left-count right-count)
                                ;; Unwritten suffixes stay equal after each successful comparison.
                                (bytevector=? left-buffer right-buffer)
                                (loop))))))])))))

  (define $file-regular?
    (let ([inspect (foreign-procedure "cspp_file_regular" (string) scheme-object)])
      (lambda (who path)
        (let ([result (inspect path)])
          (if (string? result) ($err-file who path result) result)))))

  #|proc:same-file-contents?
  Return whether regular files named by path strings `path1` and `path2` have equal byte contents.
  Symlinks are always followed. Return `#f` for a missing or nonregular entry.
  Compare with bounded reusable buffers; lookup failures other than a missing path and
  all read errors propagate.
  |#
  (define-who same-file-contents?
    (lambda (path1 path2)
      (pcheck ([$filename? path1 path2])
              (if (and ($file-regular? who path1) ($file-regular? who path2))
                  ($with-port (open-file-input-port path1)
                    (lambda (p1)
                      ($with-port (open-file-input-port path2)
                        (lambda (p2)
                          ($same-file-port-contents? p1 p2)))))
                  #f))))



;;;; file modes

  (define $mode-num? (lambda (x) (and (fixnum? x) (fx<= 0 x #b111111111111))))
  (define $valid-mode-sym?  (lambda (x) (memq x '(r w x))))
  (define $valid-smode-sym? (lambda (x) (memq x '(su sg t))))

  (define $valid-modes?
    (lambda (m) (and (list? m) (andmap $valid-mode-sym? m) (unique? m))))
  (define $valid-smodes?
    (lambda (m) (and (list? m) (andmap $valid-smode-sym? m) (unique? m))))

  (define $mode-adjustment?
    (lambda (mode valid?)
      (or (not mode)
          (valid? (if (and (pair? mode) (memq (car mode) '(+ -)))
                      (cdr mode) mode)))))

  #|proc:file-mode->symbols
  Convert integer permission bits `mode` (0 through #o7777) to four fresh lists:
  special, user, group, and other permissions. Read/write/execute use `r`, `w`, `x`;
  set-user-ID, set-group-ID, and sticky bits use `su`, `sg`, `t`.
  |#
  (define file-mode->symbols
    (lambda (mode)
          (pcheck ([$mode-num? mode])
              (let* ([ss '((su . 11) (sg . 10) (t . 9))]
                     [us '((r . 8) (w . 7) (x . 6))]
                     [gs '((r . 5) (w . 4) (x . 3))]
                     [os '((r . 2) (w . 1) (x . 0))]
                     [scanX (lambda (xs)
                             (lambda (m)
                               (fold-left
                                (lambda (m p) (if (fxlogbit? (cdr p) mode)
                                                  (snoc! m (car p))
                                                  m))
                                m xs)))]
                     [s (scanX ss)]
                     [u (scanX us)]
                     [g (scanX gs)]
                     [o (scanX os)])
                `(,(s '()) ,(u '()) ,(g '()) ,(o '()))))))
  #|proc:symbols->file-mode
  Convert special/user/group/other symbol lists `smode`, `umode`, `gmode`, `omode`
  to integer permission bits. Special symbols are `su`, `sg`, `t`; other groups use
  `r`, `w`, `x`. Each list must contain unique symbols. Return an integer from 0 through #o7777.
  |#
  (define symbols->file-mode
    (lambda (smode umode gmode omode)
      (pcheck ([list? smode umode gmode omode]
               [$valid-smodes? smode]
               [$valid-modes? umode gmode omode])
              (let* ([ss '((t . 9) (sg . 10) (su . 11))]
                     [us '((x . 6) (w . 7) (r . 8))]
                     [gs '((x . 3) (w . 4) (r . 5))]
                     [os '((x . 0) (w . 1) (r . 2))]
                     [scanX (lambda (mode xs)
                             (lambda (m)
                               (fold-left
                                (lambda (m s) (if (memq s (map car xs))
                                                  (fxlogbit1 (cdr (assoc s xs)) m)
                                                  m))
                                m mode)))]
                     ;; scan symbols in each type of mode and set the bits
                     [s (scanX smode ss)]
                     [u (scanX umode us)]
                     [g (scanX gmode gs)]
                     [o (scanX omode os)])
                (o (g (u (s 0))))))))


  (define $file-chmod
    (case-lambda
      ;; numeric or symbolic for user mode
      [(who path mode)
       (cond [($mode-num? mode)
              (chmod path mode)]
             [(or (not mode) (list? mode))
              ($file-chmod who path #f mode #f #f)]
             [else (errorf who "invalid file mode: ~a" mode)])]
      [(who path umode gmode)
       ($file-chmod who path #f umode gmode #f)]
      [(who path umode gmode omode)
       ($file-chmod who path #f umode gmode omode)]
      [(who path smode umode gmode omode)
       (pcheck ([$filename? path]
                [(lambda (mode) ($mode-adjustment? mode $valid-smodes?)) smode]
                [(lambda (mode) ($mode-adjustment? mode $valid-modes?)) umode gmode omode]
                [file-exists? path])
               (let* ([m (get-mode path #t)]
                      [sm (file-mode->symbols m)]
                      [filter-mode (lambda (mode i)
                                     (if mode
                                         (if (null? mode)
                                             mode
                                             (record-case mode
                                               [+ d* (list+ (list-ref sm i) d*)]
                                               [- d* (list- (list-ref sm i) d*)]
                                               [else mode]))
                                         (list-ref sm i)))]
                      [s (filter-mode smode 0)]
                      [u (filter-mode umode 1)]
                      [g (filter-mode gmode 2)]
                      [o (filter-mode omode 3)])
                 ($copy-file-mode who path (symbols->file-mode s u g o))))]))


  #|proc:file-chmod
  Change the permission bits of a file.

  If `path` is a symlink, it is always dereferenced.

  If only one mode is given, it can be either in numerical form (as returned by `get-mode`),
  or in symbolic form (see below).
  In the former case, it specifies all modes at once.
  In the latter, it specifies the user mode to be modified.

  If more than one mode is given, they must be in symbolic form.
  If two modes are given, they represent user mode and group mode;
  If three modes are given, they represent user mode, group mode and others mode;
  If four modes are given, they represent special mode, user mode, group mode, and others mode.
  Omitted symbolic groups keep their current bits, as if supplied as `#f`.

  Special modes are set-user-ID, set-group-ID and sticky bit.


  Symbolic Permission Form:

  For user, group and others bits, the symbolic form supports specifying permission
  bits in a list of symbols consisting of 'r, 'w, and 'x.
  For example, '(r) means setting the (say, user's) bits to read only;
  and '(r w x) sets all bits.

  Set-user-ID, set-group-ID and sticky bit are represented as 'su, 'sg and 't, respectively.
  If mode is '(), all bits are cleared; if mode is #f, all bits are left unchanged.
  Each symbol in the list can only appear once. They order of symbols is unimportant.

  Adding or Subtracting Bits:

  In addition to the above way of specifying all bits to be set using the symbolic form,
  one can also add '+ and '- in front of the list to only add to or subtract from the file's
  bits the given bits.
  For example, '(- x) removes the execution bit; '(+ w x) adds the write and execution bit.
  Nothing happens when trying to remove a bit that's not set or
  when trying to add a bit that already exists.
  Symbolic requests leave an already matching mode unchanged. If a changed mode cannot retain
  all requested bits, raise an I/O error; the filesystem may already have applied some changes.
  |#
  (define-who file-chmod
    (lambda (path mode . rest)
      (pcheck ([$filename? path])
              (apply $file-chmod who path mode rest))))

  #|proc:file-chmod-s
  Change the special mode only.
  `mode` must be in symbolic form.
  |#
  (define-who file-chmod-s
    (lambda (path mode)
      ($file-chmod who path mode #f #f #f)))
  #|proc:file-chmod-u
  Change user mode only.
  `mode` must be in symbolic form.
  |#
  (define-who file-chmod-u
    (lambda (path mode)
      ($file-chmod who path #f mode #f #f)))
  #|proc:file-chmod-g
  Change group mode only.
  `mode` must be in symbolic form.
  |#
  (define-who file-chmod-g
    (lambda (path mode)
      ($file-chmod who path #f #f mode #f)))
  #|proc:file-chmod-o
  Change others mode only.
  `mode` must be in symbolic form.
  |#
  (define-who file-chmod-o
    (lambda (path mode)
      ($file-chmod who path #f #f #f mode)))
  #|proc:file-chmod-a
  Change all modes except the special mode.
  `mode` must be in symbolic form.
  |#
  (define-who file-chmod-a
    (lambda (path mode)
      ($file-chmod who path #f mode mode mode)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   filesystem utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|proc:walk-files
  Traverse the filesystem tree rooted at path string `path`.
  Procedure `proc` has signature `(lambda (directory directories files) continue?)`.

  For every directory `pdir` encountered, `proc` is applied to three values:
  1. pdir
  2. the list of all directories under `pdir`
  3. the list of all other files under `pdir`.

  All paths retain the supplied root prefix; an absolute root produces absolute paths.
  Return `#f` from `proc` to stop the whole traversal, or any other value to continue.
  Return `#f` if traversal was stopped. A complete top-down walk returns `#t`;
  a complete bottom-up walk returns the root callback's result.
  Calling `cd` or `current-directory` to change the current directory inside `proc`
  results in unspecified behavior.

  If `follow-link?` is #f (the default), symlinks to directories are not followed
  and are therefore regarded as files. A symlink supplied as the root is always followed.
  When following links, distinct aliases are visited separately while ancestor cycles are skipped.
  Cycle checks distinguish Linux mount views. If mount IDs are unavailable, an ambiguous
  repeated directory identity raises an I/O error instead of omitting its descendants.

  If `top-down?` is #t (the default), the tree is traversed in a top-down manner,
  descending into each directory as deep as possible.
  `proc` in this case is called before any of the subdirectories are visited in 2.

  If `top-down?` is #f, `walk-files` calls `proc` after all of the subdirectories are visited in 2,
  hence in a bottom-up manner.

  In top-down mode, one can set entries in 2 to #f to disable visits of those entries.
  In bottom-up mode, this behavior has no effect.
  Pathname strings passed to `proc` are read-only and may be shared between callback calls.
  |#
  (define-who walk-files
    (case-lambda
      [(proc path)
       (walk-files proc path #f #t)]
      [(proc path follow-link?)
       (walk-files proc path follow-link? #t)]
      [(proc path follow-link? top-down?)
       (pcheck ([procedure? proc] [$filename? path] [boolean? follow-link? top-down?])
               (unless (file-directory? path)
                 ($err-file-not-directory who path))
               (let ()
                 (define directory-ancestors
                   (lambda (directory ancestors)
                     (if (not follow-link?)
                         ;; A callback may replace a listed directory with a link.
                         (and (not (file-symbolic-link? directory)) ancestors)
                         (let ([identity ($directory-view who directory)])
                           (and (not ($file-view-seen? who directory identity ancestors))
                                (cons identity ancestors))))))
                 (define walk-top-down
                   (lambda (directory ancestors)
                     (let-values ([(dirs files) ($directory-children directory follow-link?)])
                       (if (proc directory dirs files)
                           (let visit ([remaining dirs])
                             (cond
                              [(null? remaining) #t]
                              [(not (car remaining)) (visit (cdr remaining))]
                              [else
                               (let ([child-ancestors
                                      (directory-ancestors (car remaining) ancestors)])
                                 (cond [(not child-ancestors) (visit (cdr remaining))]
                                       [(walk-top-down (car remaining) child-ancestors)
                                        (visit (cdr remaining))]
                                       [else #f]))]))
                           #f))))
                 (define walk-bottom-up
                   (lambda (directory ancestors)
                     (let-values ([(dirs files) ($directory-children directory follow-link?)])
                       (let visit ([remaining dirs])
                         (cond
                          [(null? remaining) (proc directory dirs files)]
                          [else
                           (let ([child-ancestors
                                  (directory-ancestors (car remaining) ancestors)])
                             (cond [(not child-ancestors) (visit (cdr remaining))]
                                   [(walk-bottom-up (car remaining) child-ancestors)
                                    (visit (cdr remaining))]
                                   [else #f]))])))))
                 ;; The supplied root is followed even when descendant links are disabled.
                 (let ([ancestors (if follow-link? (list ($directory-view who path)) '())])
                   (if top-down?
                       (walk-top-down path ancestors)
                       (walk-bottom-up path ancestors)))))]))

  #|proc:print-file-tree
  Print the file tree rooted at `path`.
  `path` must be a valid directory.
  |#
  (define print-file-tree
    (lambda (path)
      (pcheck ([$filename? path] [file-directory? path])
                        (walk-files (lambda (pdir dirs files)
                                      (printf "~a:~n" pdir)
                                      (printf "  ~a~n" dirs)
                                      (printf "  ~a~n" files)
                                      (newline))
                                    path))))


  #|proc:file-find
  Apply `pred`, a procedure of signature `(path -> boolean)`, to each entry under `path`.
  Return the first matching path, or `#f` if no entry matches. `follow-link?` controls
  whether directory symlinks are traversed and defaults to `#f`.
  |#
  (define-who file-find
    (case-lambda
      [(pred path) (file-find pred path #f)]
      [(pred path follow-link?)
       (pcheck ([procedure? pred] [$filename? path] [boolean? follow-link?])
         (let ([res #f])
           (walk-files (lambda (dir dirs files)
                       (let lp1 ([ds dirs])
                         (if (null? ds)
                             (let lp2 ([fs files])
                               (unless (null? fs)
                                 (if (pred (car fs))
                                     (begin (set! res (car fs))
                                            ;; use #f to end the walk
                                            #f)
                                     (lp2 (cdr fs)))))
                             (if (pred (car ds))
                                 (begin (set! res (car ds))
                                        #f)
                                 (lp1 (cdr ds))))))
                       path follow-link?)
           res))]))

  ;; TODO lazy (iter/stream) file-find-all
  #|proc:file-find-all
  Apply `pred`, a procedure of signature `(path -> boolean)`, to each entry under `path` and
  return a list of matching paths. `follow-link?` controls whether directory symlinks are
  traversed and defaults to `#f`.
  |#
  (define-who file-find-all
    (case-lambda
      [(pred path) (file-find-all pred path #f)]
      [(pred path follow-link?)
       (pcheck ([procedure? pred] [$filename? path] [boolean? follow-link?])
         (let ([lb (make-list-builder)])
           (walk-files (lambda (dir dirs files)
                         (let ([visit (lambda (path) (when (pred path) (lb path)))])
                           (for-each visit dirs)
                           (for-each visit files))
                         #t)
                       path follow-link?)
           (lb)))]))


  #|proc:file-map
  Apply `proc`, a procedure of signature `(path -> value)`, to each entry under `path` and
  return the resulting values as a list. `follow-link?` controls whether directory symlinks
  are traversed and defaults to `#f`.
  |#
  (define file-map
    (case-lambda
      [(proc path) (file-map proc path #f)]
      [(proc path follow-link?)
       (pcheck ([procedure? proc] [$filename? path] [boolean? follow-link?])
         (let ([lb (make-list-builder)])
           (walk-files (lambda (dir dirs files)
                         (let ([visit (lambda (path) (lb (proc path)))])
                           (for-each visit dirs)
                           (for-each visit files))
                         #t)
                       path follow-link?)
           (lb)))]))


  #|proc:file-for-each
  Apply `proc`, a procedure of signature `(path -> unspecified)`, to each entry under `path`.
  `follow-link?` controls whether directory symlinks are traversed and defaults to `#f`.
  |#
  (define file-for-each
    (case-lambda
      [(proc path) (file-for-each proc path #f)]
      [(proc path follow-link?)
       (pcheck ([procedure? proc] [$filename? path] [boolean? follow-link?])
         (walk-files (lambda (dir dirs files)
                       (for-each proc dirs)
                       (for-each proc files)
                       #t)
                     path follow-link?))]))

  ;; Some procedures only need the presence of the symlink per se, not its target.
  (define file-exists?-no-follow (lambda (path) (file-exists? path #f)))
  (define $touch-empty-file
    (lambda (path)
      ($with-port (open-file-output-port path (file-options no-fail no-truncate))
        (lambda (port) (void)))))

  (define $link-target-path
    (lambda (parent target)
      ;; Chez path-build appends even absolute fragments. Link text is never home-expanded.
      (if (and (fx> (string-length target) 0) (char=? (string-ref target 0) #\/))
          target
          (path-build parent target))))

  (define $readlink-target-link?
    (let ([inspect (foreign-procedure "cspp_file_symbolic_link" (string) scheme-object)])
      (lambda (who path)
        (let ([result (inspect path)])
          (if (string? result) ($err-file who path result) result)))))

  ;; Expand before decomposing a filesystem pathname. Keep physical dot/parent traversal
  ;; intact, and protect any literal leading tilde left by the native expansion.
  (define $expand-file-path
    (let ([expand (foreign-procedure "cspp_file_expand_pathname" (string) scheme-object)])
      (lambda (who path)
        (if (or (fxzero? (string-length path)) (not (char=? (string-ref path 0) #\~)))
            path
            (let ([expanded (expand path)])
              (when (vector? expanded) ($err-file who path (vector-ref expanded 0)))
              ;; An unknown user or relative HOME can leave a literal leading tilde.
              ;; Protect it from another home expansion during subsequent native calls.
              (if (and (fx> (string-length expanded) 0)
                       (char=? (string-ref expanded 0) #\~))
                  (path-build "." expanded)
                  expanded))))))

  (define $readlink
    (let ([ffi (let ([ffi-readlink (foreign-procedure "cspp_readlink" (string) scheme-object)])
                 (lambda (who path)
                   (let ([x (ffi-readlink path)])
                     (if (vector? x)
                         ($err-file who path (vector-ref x 0))
                         x))))])
      (lambda (who path recursive?)
        ;; The link itself may exist even when its target does not.
        (pcheck ([$filename? path] [boolean? recursive?]
                 [file-exists?-no-follow path] [file-symbolic-link? path])
          (if (not recursive?)
              (ffi who path)
              (let ([seen (make-hashtable equal-hash equal?)])
                (let loop ([path ($expand-file-path who path)])
                  ;; A relative link's target depends on its parent directory.
                  ;; Hard-linked symlinks and distinct mount views need separate contexts.
                  (let* ([parent (let ([parent (path-parent path)])
                                   (if (string=? parent "") "." parent))]
                         [identity (list ($file-identity path #f)
                                         ($directory-view who parent))])
                    (when (hashtable-contains? seen identity)
                      (errorf who "symlink cycle detected at: ~a" path))
                    (hashtable-set! seen identity #t)
                    (let* ([ln (ffi who path)] [target ($link-target-path parent ln)])
                      (if ($readlink-target-link? who target)
                          (loop target)
                          (values ln parent)))))))))))


  #|proc:readlink
  Return the symbolic-link text at path string `path`.
  Boolean `recursive?` defaults to `#f`.

  If `recursive?`, then the dereference continues until a non-symlink target is met,
  and both the target text and the final link's parent directory are returned.
  A bare link uses `.` as its parent, so joining preserves literal leading tildes.
  A leading tilde in the input pathname is expanded before determining that parent.
  Symbolic-link target text is never home-expanded or lexically normalized.
  Cycle detection includes the parent directory's mount view, so finite chains through
  different bind mounts can resolve even when their link and parent inodes match.
  Missing targets terminate resolution. Inspection failures, including intermediate cycles
  and inaccessible directories, raise an I/O error.

  Otherwise the first symlink content is returned.
  |#
  (define-who readlink
    (case-lambda [(path) ($readlink who path #f)]
                 [(path recursive?) ($readlink who path recursive?)]))

  #|proc:readlink2
  Return the symlink text at path string `path` when boolean `recursive?` is false or omitted.
  When true, resolve the link chain and return the final target pathname. Absolute targets
  retain their root; relative targets use the final link's parent directory. A leading tilde
  in `path` is expanded first; target text is not home-expanded or lexically normalized.
  Missing targets terminate resolution. A cycle or a target inspection failure raises an error.
  |#
  (define-who readlink2
    (case-lambda [(path) ($readlink who path #f)]
                 [(path recursive?)
                  (pcheck ([$filename? path] [boolean? recursive?])
                          (if recursive?
                              (let-values ([(ln pdir) ($readlink who path #t)])
                                ($link-target-path pdir ln))
                              ($readlink who path #f)))]))



;;;; soft and hard links

  (define $parse-file-path
    (lambda (who path)
      (path-parse 'unix ($expand-file-path who path))))

  (define $parent-directory-name
    (lambda (path)
      (let ([name (path-render (path-drop-basename path))])
        (if (string=? name "") "." name))))

  (define $same-file-entry?
    (lambda (who path1 path2)
      ;; Distinct hard links may be replaced, but two names for the same directory
      ;; entry must not delete the source. Do not follow the final symlink.
      (let ([left ($parse-file-path who path1)] [right ($parse-file-path who path2)])
        (and (equal? (path-basename left) (path-basename right))
             (same-file? ($parent-directory-name left) ($parent-directory-name right))))))

  (define $replace-file-entry
    (let ([stage (foreign-procedure "cspp_file_staging_directory" (string) scheme-object)])
      (lambda (who destination create)
        (let ([directory (stage ($parent-directory-name ($parse-file-path who destination)))])
          (when (vector? directory) ($err-file who destination (vector-ref directory 0)))
          (let ([temporary (path-build directory "entry")])
            (dynamic-wind
              void
              (lambda ()
                (create temporary)
                (rename-file temporary destination))
              (lambda ()
                ;; Creation may have failed after writing part of the replacement.
                (dynamic-wind
                  void
                  (lambda ()
                    (when (file-exists? temporary #f) (delete-file temporary #t)))
                  (lambda () (delete-directory directory #t))))))))))

  (define $link/copy-helper
    (lambda (who link/copy-what overwrite?)
      (case-lambda
        [(src dest) (($link/copy-helper who link/copy-what overwrite?) src dest src #f)]
        [(src dest name-src)
         (($link/copy-helper who link/copy-what overwrite?) src dest name-src #f)]
        [(src dest name-src source-path)
         (if (file-exists? dest #f)
             (let ([overwrite/error
                    (lambda (dest)
                      (if (file-exists? dest #f)
                          (if overwrite?
                              (begin
                                (when (and source-path
                                           (or ($same-file-entry? who source-path dest)
                                               ($same-file-entry? who name-src dest)))
                                  ($err-file who dest "source and destination are the same file"))
                                ;; Commit only a complete replacement; do not follow dest links.
                                ($replace-file-entry who dest
                                  (lambda (temporary) (link/copy-what src temporary))))
                              ($err-file-exists who dest))
                          (link/copy-what src dest)))])
               (if (file-directory? dest #t)
                   (let* ([basename (or (path-basename
                                        (if source-path ($parse-file-path who name-src)
                                            (path-parse 'unix name-src)))
                                        (errorf who "source has no basename for a directory destination: ~a"
                                                name-src))]
                          [newd (path-build dest basename)])
                     (if (file-directory? newd #t)
                         ($err-directory-exists who newd)
                         (overwrite/error newd)))
                   (overwrite/error dest)))
             (link/copy-what src dest))])))

  ;; Symlinks can also have hard links.
  (define $file-link
    (case-lambda
      [(who src dest overwrite?) ($file-link who src dest overwrite? src)]
      [(who src dest overwrite? name-src)
       (let* ([$link (let ([ffi (foreign-procedure "cspp_link" (string string) scheme-object)])
                       (lambda (src dest)
                         (let ([x (ffi src dest)])
                           (when (string? x)
                             ($err-file who dest x)))))]
              [link ($link/copy-helper who $link overwrite?)])
         (link src dest name-src src))]))

  ;; TODO use macro in the following?
  #|proc:file-link
  Create a hard link from `src` to `dest`.

  Symlinks are followed recursively if `follow-link?` is #t.
  If destination string `dest` names a directory or a symlink to one, create the link
  inside it using `src`'s basename. Otherwise `dest` names the new entry. Return unspecified values.
  Home notation is expanded before choosing the source basename.
  Native creation failures raise I/O errors identifying the attempted destination pathname.
  |#
  (define-who file-link
    (case-lambda [(src dest) (file-link src dest #t)]
                 [(src dest follow-link?)
                  (pcheck ([$filename? src dest] [boolean? follow-link?]
                           [(lambda (x) (file-exists? x follow-link?)) src])
                          (if (file-symbolic-link? src)
                              (if follow-link?
                                  ($file-link who (readlink2 src #t) dest #f src)
                                  ($file-link who src dest #f))
                              ($file-link who src dest #f)))]))


  #|proc:file-link!
  Similar to `file-link`, but if `dest` already exists, it is overwritten.
  Prepare the replacement before renaming it over the destination entry. Failures before
  that rename leave the destination intact. Other hard links keep their old contents.
  Directory destinations, including directory symlinks, are resolved as containers first.
  |#
  (define-who file-link!
    (case-lambda [(src dest) (file-link! src dest #t)]
                 [(src dest follow-link?)
                  (pcheck ([$filename? src dest] [boolean? follow-link?]
                           [(lambda (x) (file-exists? x follow-link?)) src])
                          (if (file-symbolic-link? src)
                              (if follow-link?
                                  ($file-link who (readlink2 src #t) dest #t src)
                                  ($file-link who src dest #t))
                              ($file-link who src dest #t)))]))


  (define $symlink-entry
    (let ([create (foreign-procedure "cspp_symlink" (string string) scheme-object)])
      (lambda (who src dest)
        (let ([result (create src dest)])
          (when (string? result) ($err-file who dest result))))))

  (define $file-symlink
    (lambda (who src dest overwrite?)
      (let ([link ($link/copy-helper who
                                   (lambda (src dest) ($symlink-entry who src dest)) overwrite?)])
        (link src dest src #f))))

  ;; A declaration names one exact entry, never a directory container. Stage a
  ;; replacement symlink so a creation failure leaves the previous entry intact.
  (define $file-tree-symlink
    (lambda (who src dest overwrite?)
      (pcheck ([$filename? src dest])
        (if (file-exists? dest #f)
            (cond [(not overwrite?) ($err-file-exists who dest)]
                  [(file-directory? dest #f) ($err-directory-exists who dest)]
                  [else
                   ($replace-file-entry who dest
                     (lambda (temporary) ($symlink-entry who src temporary)))])
            ($symlink-entry who src dest)))))


  #|proc:file-symlink
  Create a symbolic link from `src` to `dest`.
  `src` must be a string and is written as is to `dest` as the symlink content.
  If destination string `dest` names a directory or a symlink to one, create the link inside it
  using `src`'s basename after ignoring trailing separators. A root target has no such basename
  and requires an explicit destination entry name. Return unspecified values.
  Native creation failures raise I/O errors identifying the attempted destination pathname.
  |#
  (define-who file-symlink
    (lambda (src dest)
      (pcheck ([$filename? src dest])
              ($file-symlink who src dest #f))))


  #|proc:file-symlink!
  Similar to `file-symlink`, but if `dest` already exists, it is overwritten.
  Create the new link before replacing the destination entry; a creation failure preserves it.
  Directory destinations, including directory symlinks, are resolved as containers first.
  |#
  (define-who file-symlink!
    (lambda (src dest)
      (pcheck ([$filename? src dest])
              ($file-symlink who src dest #t))))



  (define $touch (foreign-procedure "cspp_touch" (string ptr ptr boolean) scheme-object))
  (define $touch!
    (lambda (who path atime mtime follow-link?)
      (let ([result ($touch path atime mtime follow-link?)])
        (when (string? result) ($err-file who path result))
        result)))
  (define $time->vec (lambda (t) (vector (time-nanosecond t) (time-second t))))
  (define $check-file-time
    (let* ([bits ((foreign-procedure "cspp_file_time_bits" () int))]
           [limit (expt 2 (fx1- bits))])
      (lambda (who time)
        (when time
          (unless (eq? 'time-utc (time-type time))
            (errorf who "invalid time type ~a (should be 'time-utc)" (time-type time)))
          ;; Chez integer conversions also accept unsigned positive values.
          (unless (<= (- limit) (time-second time) (- limit 1))
            (errorf who "UTC seconds are outside the native timestamp range: ~a"
                    (time-second time)))))))

  (define $file-touch
    (lambda (who path atime mtime follow-link? force?)
      (pcheck ([$filename? path]
               [(lambda (x) (or (not x) (time? x))) atime mtime]
               [boolean? follow-link? force?])
              ($check-file-time who atime) ($check-file-time who mtime)

              (let* ([atime (if atime ($time->vec atime) #f)] [mtime (if mtime ($time->vec mtime) #f)]
                     [force-or-not (lambda (dest)
                                     (if force?
                                         (begin ($touch-empty-file dest)
                                                ($touch! who dest atime mtime #t))
                                         ($err-file-not-found who dest)))])
                (if (file-exists? path #f)
                    (if (file-symbolic-link? path)
                        (if follow-link?
                            ;; need to make up the correct target path
                            (let ([dest (readlink2 path #t)])
                              (if (file-exists? dest #f)
                                  ($touch! who dest atime mtime #f)
                                  (force-or-not dest)))
                            ($touch! who path atime mtime #f))
                        ($touch! who path atime mtime #f))
                    (force-or-not path))))))


  ;; how to convert arbirary time to epoch time?
  #|proc:file-touch
  Change the last access and modification times of a file to `time`.
  `time` is the UTC time type ('time-utc) as can be made by `make-time`.
  By default, it is the current time as returned by `(current-time)`.
  Seconds must fit the platform's signed native timestamp range; invalid times are rejected
  before creating a missing file. The filesystem may further limit representable dates.

  If `force?`, then if the file does not exist, an empty file with the same name is created.
  Otherwise, it is an error if the file does not exist.

  If `follow-link?` is #f and `path` is a symlink, then `file-touch` updates the timestamps
  of the symlink, rather those of the file the symlink points to.
  |#
  (define-who file-touch
    (case-lambda
      [(path)
       (file-touch path (current-time) #t #t)]
      [(path time)
       (file-touch path time #t #t)]
      [(path time force?)
       (file-touch path time #t force?)]
      [(path time follow-link? force?)
       ($file-touch who path time time follow-link? force?)]))


  #|proc:file-touch-atime
  Similar to `file-touch`, but updates last access time only.
  The other one is left unchanged.
  |#
  (define-who file-touch-atime
    (case-lambda
      [(path)
       (file-touch-atime path (current-time) #t #t)]
      [(path time)
       (file-touch-atime path time #t #t)]
      [(path time force?)
       (file-touch-atime path time #t force?)]
      [(path time follow-link? force?)
       ($file-touch who path time #f follow-link? force?)]))


  #|proc:file-touch-mtime
  Similar to `file-touch`, but updates last modification time only.
  The other one is left unchanged.
  |#
  (define-who file-touch-mtime
    (case-lambda
      [(path)
       (file-touch-mtime path (current-time) #t #t)]
      [(path time)
       (file-touch-mtime path time #t #t)]
      [(path time force?)
       (file-touch-mtime path time #t force?)]
      [(path time follow-link? force?)
       ($file-touch who path #f time follow-link? force?)]))



;;;; copies and moves

  (define $copy-file-mode
    (let ([copy (foreign-procedure "cspp_file_copy_mode" (string unsigned-32) scheme-object)])
      (lambda (who path mode)
        (let ([result (copy path mode)])
          (when (string? result) ($err-file who path result))))))

  #|proc:file-copymode
  Copy permission bits from path string `src` to path string `dest`.
  Return unspecified values.

  If `src` or `dest` is a symlink, it is always dereferenced.
  An already matching mode is left unchanged. If the filesystem cannot retain all copied
  bits, including set-group-ID, raise an I/O error identifying `dest`.
  |#
  (define-who file-copymode
    (lambda (src dest)
      (pcheck ([$filename? src dest] [file-exists? src dest])
              (let ([m (get-mode src #t)])
                ($copy-file-mode who dest m)))))


  #|proc:file-copymeta
  Copy permission bits and access/modification times from path string `src` to `dest`.
  Optional booleans `follow-link-src?` and `follow-link-dest?` default to `#t` and control
  which entry supplies or receives timestamps. Permission bits always come from the source
  target and are read and copied only when `follow-link-dest?` is true. Both selected entries must
  exist. Return unspecified values; ownership, creation times, and xattrs are not copied.
  Timestamps are applied before permission bits, so restrictive directory modes do not
  prevent completing the copy through a pathname such as `directory/.`.
  An already matching mode is left unchanged. Failure to retain all copied permission bits
  raises an I/O error identifying `dest`; timestamps may already have been copied.
  |#
  (define-who file-copymeta
    (case-lambda
      [(src dest) (file-copymeta src dest #t #t)]
      [(src dest follow-link-src? follow-link-dest?)
       (pcheck ([$filename? src dest] [boolean? follow-link-src? follow-link-dest?]
                [(lambda (x) (file-exists? x follow-link-src?)) src]
                [(lambda (x) (file-exists? x follow-link-dest?)) dest])
               (let* ([st (file-stat src follow-link-src?)]
                      [atime (file-stat-access-time st)]
                      [mtime (file-stat-modification-time st)]
                      [mode (and follow-link-dest? (get-mode src #t))])
                 ;; TODO xattrs
                 ($file-touch who dest atime mtime follow-link-dest? #f)
                 (when follow-link-dest?
                   ;; if `dest` is a symlink, changing its own mode has no effect
                   ($copy-file-mode who dest mode))))]))


  (define $copy-port
    (lambda (who pin pout)
      (pcheck ([input-port? pin] [output-port? pout])
              (pcheck-open-port (pin pout)
                                (cond
                                 [(and (binary-port? pin) (binary-port? pout))
                                  (let ([buffer (make-bytevector 16384)])
                                    (let loop ()
                                      (let ([count (get-bytevector-n! pin buffer 0
                                                                      (bytevector-length buffer))])
                                        (unless (eof-object? count)
                                          (put-bytevector pout buffer 0 count)
                                          (loop)))))
                                  (flush-output-port pout)]
                                 [(and (textual-port? pin) (textual-port? pout))
                                  (let ([buffer (make-string 8192)])
                                    (let loop ()
                                      (let ([count (get-string-n! pin buffer 0
                                                                  (string-length buffer))])
                                        (unless (eof-object? count)
                                          (put-string pout buffer 0 count)
                                          (loop)))))
                                  (flush-output-port pout)]
                                 [else (errorf who "ports do not have the same type")])))))


  #|proc:copy-port
  Copy data from input port `pin` to output port `pout`.
  `pin` and `pout` must be either both binary port or both textual port.

  `copy-port` does not automatically close the ports.
  |#
  (define-who copy-port
    (lambda (pin pout)
      ($copy-port who pin pout)))


  (define $file-copy
    (case-lambda
      [(who src dest overwrite?) ($file-copy who src dest overwrite? src)]
      [(who src dest overwrite? name-src)
       ;; `src` can only be regular or symlink
       (let ([copy-file ($link/copy-helper who (lambda (src dest)
                                                 ($with-port (open-file-input-port src)
                                                   (lambda (pin)
                                                     ($with-port (open-file-output-port dest)
                                                       (lambda (pout)
                                                         ($copy-port who pin pout))))))
                                           overwrite?)]
             [copy-link ($link/copy-helper who file-symlink overwrite?)])
         (cond
          [(file-symbolic-link? src)
           (let ([ln (readlink src)])
             (copy-link ln dest name-src src))]
          [(file-regular? src #f)
           (copy-file src dest name-src src)]
          [else ($err-file-not-regular who src)]))]))


  #|proc:file-copy
  Copy a single regular file from `src` to `dest`.

  `follow-link?`: copy the symlink per se or copy the file it points to (default: #t).
  If destination string `dest` names a directory or a symlink to one, copy inside it using
  `src`'s basename. Otherwise `dest` names the new entry. Metadata is not copied.
  Home notation is expanded before choosing the source basename.
  Return unspecified values.
  |#
  (define-who file-copy
    (case-lambda
      [(src dest) (file-copy src dest #t)]
      [(src dest follow-link?)
       (pcheck ([$filename? src dest] [boolean? follow-link?]
                [(lambda (x) (file-exists? x follow-link?)) src])
               (if (file-symbolic-link? src)
                   (if follow-link?
                       ($file-copy who (readlink2 src #t) dest #f src)
                       ($file-copy who src dest #f))
                   ($file-copy who src dest #f)))]))


  #|proc:file-copy!
  Similar to `file-copy`, but overwrites `dest` if it exists.
  Finish copying before renaming the replacement over the destination entry. Copy failures
  preserve an existing destination. Directory destinations, including directory symlinks, are
  resolved as containers first. A final destination symlink is replaced; other hard links retain
  their previous contents. Metadata is not copied, as with `file-copy`.
  |#
  (define-who file-copy!
    (case-lambda
      [(src dest) (file-copy! src dest #t)]
      [(src dest follow-link?)
       (pcheck ([$filename? src dest] [boolean? follow-link?]
                [(lambda (x) (file-exists? x follow-link?)) src])
               (if (file-symbolic-link? src)
                   (if follow-link?
                       ($file-copy who (readlink2 src #t) dest #t src)
                       ($file-copy who src dest #t))
                   ($file-copy who src dest #t)))]))



  (define $canonical-file-path
    (let ([resolve (foreign-procedure "cspp_realpath" (string) scheme-object)])
      (lambda (who path)
        (let ([result (resolve (if (string=? path "") "." path))])
          (if (vector? result)
              ($err-file who path (vector-ref result 0))
              (path-parse 'unix result))))))

  (define $copytree-destination-inside-source?
    (lambda (who src dest snapshots)
      ;; Resolve existing directories before lexical comparison: normalizing first
      ;; would change the meaning of .. after a symlink.
      (let* ([source ($canonical-file-path who src)]
             [container (if (file-directory? dest)
                            dest
                            ($parent-directory-name ($parse-file-path who dest)))]
             [destination ($canonical-file-path who container)]
             [relative (path-components (path-relative-to source destination))])
        (or (not (and (pair? relative) (string=? (car relative) "..")))
            ;; realpath does not collapse bind mounts. Inspect the source before creating
            ;; anything, comparing the destination container with every source directory.
            ;; Track mount views separately: one inode can expose different child mounts.
            (let ([target ($file-identity container #t)]
                  [visited (make-hashtable equal-hash equal?)])
              (let scan ([directory src])
                (let* ([identity ($file-identity directory #t)]
                       [mount ($file-mount-id who directory)] [key (cons mount identity)])
                  (cond [(equal? identity target) #t]
                        [(hashtable-contains? visited key)
                         ;; Older kernels can inspect ordinary trees, but cannot prove that
                         ;; repeated aliases have the same view without a mount ID.
                         (unless mount
                           ($err-file who directory "mount identity is unavailable for alias"))
                         #f]
                        [else
                         (hashtable-set! visited key #t)
                         ;; Keep the first snapshot of each underlying directory. Reading
                         ;; any of its mount views can change the shared access timestamp.
                         (when (and snapshots (not (hashtable-contains? snapshots identity)))
                           (hashtable-set! snapshots identity (file-stat directory)))
                         (exists (lambda (entry)
                                   (and ($fs-entry-directory? who (car entry) (cdr entry) #f)
                                        (scan (car entry))))
                                 ($directory-entries directory))]))))))))

  (define $apply-copy-metadata
    (lambda (who dest stat)
      (let ([link? (eq? (file-type->symbol (file-stat-type stat)) 'FT_symlink)])
        ($file-touch who dest (file-stat-access-time stat) (file-stat-modification-time stat)
                     (not link?) #f)
        (unless link? ($copy-file-mode who dest (file-stat-mode stat))))))

  (define $copy-tree-file
    (lambda (src dest)
      ;; Tree traversal supplies an exact entry name, never another container.
      (when (file-directory? dest) ($err-directory-exists 'file-copytree dest))
      (file-copy src dest #f)))

  (define $copy-tree-for-move
    (lambda (src dest)
      ;; Snapshot before reading the file can update its access time.
      (let ([stat (file-stat src #f)])
        ($copy-tree-file src dest)
        ($apply-copy-metadata 'file-move dest stat))))

  (define $copy-for-move
    (lambda (src dest)
      ($copy-tree-for-move src (if (file-directory? dest)
                                  (path-build dest (path-last src)) dest))))

  (define $make-move-directory
    (let ([access (foreign-procedure "cspp_file_directory_access" (string) scheme-object)])
      (lambda (who path)
        (mkdir path #o700)
        (let ([result (access path)])
          (when (string? result) ($err-file who path result))))))

  (define $file-copytree
    (lambda (who src dest copy preserve-metadata?)
       (pcheck ([$filename? src dest] [file-directory? src] [procedure? copy])
               (let* ([source-path ($absolute-file-path src)]
                      [snapshots (and preserve-metadata? (make-hashtable equal-hash equal?))]
                      [metadata '()]
                      [remember-directory
                       (lambda (source target)
                         (when preserve-metadata?
                           (set! metadata
                                 (cons (cons target
                                             (hashtable-ref snapshots ($file-identity source #t) #f))
                                       metadata))))]
                      [make-directory
                       (lambda (source target)
                         (remember-directory source target)
                         ;; A new move destination stays private until copying finishes.
                         (if preserve-metadata?
                             ($make-move-directory who target)
                             (mkdir target #o777)))]
                      [relativize (lambda (path)
                                    (path-render
                                     (path-relative-to source-path
                                                       ($absolute-file-path path))))]
                      [do-copytree (lambda (dest)
                                     (walk-files (lambda (dir dirs files)
                                                   ;; paths are relative to `src`
                                                   (for-each (lambda (d)
                                                               (let ([newd (path-build dest (relativize d))])
                                                                 (make-directory d newd)))
                                                             dirs)
                                                   (for-each (lambda (f)
                                                               (let ([newd (path-build dest (relativize f))])
                                                                 (copy f newd)))
                                                             files))
                                                 src #f #t))])
                 (when ($copytree-destination-inside-source? who src dest snapshots)
                   (errorf who "destination cannot be inside the source directory"))
                 (unless (file-directory? dest #t)
                     (let ([parent ($parent-directory-name ($parse-file-path who dest))])
                       (if (file-directory? parent #t)
                           (make-directory src dest)
                           (errorf who "destination's parent directory doesn't exist: ~a"
                                   parent))))
                 (do-copytree dest)
                 metadata))))

  #|proc:file-copytree
  Copy the contents of directory path string `src` into directory path string `dest`.
  An existing `dest` receives the contents directly; otherwise its parent must exist and
  `dest` itself is created. A destination within `src` is rejected. A source root symlink is
  followed, but nested directory symlinks are not traversed.
  The containment check follows symlink and bind-mount aliases of the destination.
  Source directories are inspected before creating destination entries.
  Repeated directory aliases require Linux mount IDs; unavailable mount IDs raise an error
  when the check cannot distinguish their views.
  Containment and parent checks expand home notation without lexical normalization.
  Optional `copy` has signature `(lambda (source-path destination-path) ...)` and copies
  each file or symlink; its return values are ignored. By default `file-copy` preserves
  symlinks. Directory metadata is not copied. Return unspecified values.
  Existing child names cause an error unless `copy` handles a file collision; existing
  child directories are not merged recursively. A failed copy can leave a partial destination.
  |#
  (define-who file-copytree
    (case-lambda
      [(src dest)
       (file-copytree src dest $copy-tree-file)]
      [(src dest copy) ($file-copytree who src dest copy #f) (void)]))

  (define $try-rename-file
    (lambda (src dest)
      (guard (e [(error? e) #f])
        (rename-file src dest)
        #t)))

  (define $removable-entry-path
    (lambda (path)
      ;; The caller expands home notation before stripping markers or rejecting roots.
      (let* ([parsed (path-with-trailing-directory (path-parse 'unix path) #f)]
             [basename (path-basename parsed)])
        ;; Do not normalize: a parent component after a symlink has physical meaning.
        (and basename (not (member basename '("." ".."))) (path-render parsed)))))

  (define $file-move
    (lambda (who src dest copy copy-tree)
       (pcheck ([$filename? src dest] [procedure? copy])
               (let* ([expanded ($expand-file-path who src)]
                      [directory-marked? (and (fx> (string-length expanded) 0)
                                              (char=? #\/ (string-ref expanded
                                                           (fx1- (string-length expanded)))))]
                      [src (or ($removable-entry-path expanded)
                               (errorf who "source must name a removable entry: ~a" src))])
                 (unless (file-exists? src #f) ($err-file-not-found who src))
                 (when (and directory-marked? (not (file-symbolic-link? src))
                            (not (file-directory? src)))
                   ($err-file-not-directory who src))
                 (cond [(and (file-exists? dest #f) ($same-file-entry? who src dest))
                        ($err-file who dest "source and destination are the same file")]
                       [(and (not (file-exists? dest #f)) ($try-rename-file src dest)) #t]
                       [(and (file-directory? src #f) (not (file-symbolic-link? src)))
                        (let ([metadata ($file-copytree who src dest copy-tree #t)])
                          ;; Only new directories receive source metadata. Existing merge
                          ;; roots stay usable, including when they contain the source.
                          (for-each (lambda (entry)
                                      ($apply-copy-metadata who (car entry) (cdr entry)))
                                    metadata)
                          (file-removetree src #t)
                          #t)]
                       [else
                        (let* ([container? (file-directory? dest)]
                               [target (if container? (path-build dest (path-last src)) dest)])
                          (when (and (file-exists? target #f) ($same-file-entry? who src target))
                            ($err-file who target "source and destination are the same file"))
                          (if (and container? (not (file-exists? target #f))
                                   ($try-rename-file src target))
                              #t
                              (begin (copy src dest)
                                     (delete-file src))))])))))

  #|proc:file-move
  Move filesystem path string `src` to `dest`, renaming when the target entry is absent and
  the filesystem permits it. An existing directory receives a file under its source basename,
  or the contents of a source directory. For files and symlinks, the target is resolved inside
  that directory before attempting rename. Nested directory symlinks are copied as links.
  When copying is necessary, the default preserves permission bits and access/modification
  times, but not ownership, hard-link relationships, or extended attributes.
  Source directory metadata is applied only to newly created destination directories, before
  source removal. Original directory timestamps are captured before preflight enumeration.
  Existing merge roots keep their permissions and do not receive source
  timestamps; ordinary filesystem activity may update their timestamps.
  Optional `copy` has signature `(lambda (source-path destination-path) ...)` and controls
  copying individual files and symlinks, including their metadata; return values are ignored.
  A successful rename bypasses `copy`. Source entries are removed only after copying succeeds.
  The default copier verifies restored permission bits; failure to retain them raises an I/O
  error before source removal. An already matching destination mode is left unchanged.
  Moving an entry onto itself raises an error before calling `copy`.
  A trailing separator on a source symlink is ignored: the link itself is moved.
  Filesystem roots and final `.` or `..` components are rejected before any changes.
  Home notation in `src` is expanded before checking the entry and its trailing separator.
  Existing child directories are not merged recursively, as in `file-copytree`.
  A failed copy can leave a partial destination; the source remains until copying succeeds.
  Return `#t` for a renamed file or moved directory, unspecified values for a copied file.
  |#
  (define-who file-move
    (case-lambda
      [(src dest) ($file-move who src dest $copy-for-move $copy-tree-for-move)]
      [(src dest copy) ($file-move who src dest copy copy)]))


  #|proc:file-removetree
  Remove a directory, recursively.
  Path string `path` must name a directory, not a symlink, even with trailing separators.
  Filesystem roots and final `.` or `..` components are rejected before removing any contents.
  Home notation is expanded before these checks and before stripping trailing separators.

  If `error?`, an error condition is raised when the procedure fails to remove
  a file or directory.
  Otherwise, no error condition is raised and the procedure tries to remove
  as many files and directories as possible.
  Undecodable entry names are left in place; valid siblings are still removed in that mode.
  Return `#t` if the whole tree was removed without errors, or `#f` otherwise.
  A missing root or a root that is not a directory also returns `#f` when errors are suppressed.

  Use `delete-file` or `delete-directory` to remove a single file or directory.
  |#
  (define-who file-removetree
    (case-lambda
      [(path) (file-removetree path #f)]
      [(path err?)
       (pcheck ([$filename? path] [boolean? err?])
               (define complete? #t)
               (define attempt
                 (lambda (operation)
                   (guard (failure [(and (not err?) (error? failure))
                                    (set! complete? #f)
                                    #f])
                     (operation))))
               (define remove-tree
                 (lambda (directory)
                   (let ([entries
                          (attempt
                           (lambda ()
                             ($directory-entries directory
                               (and (not err?) (lambda () (set! complete? #f))))))])
                     (when entries
                       (for-each
                        (lambda (entry)
                          (attempt
                           (lambda ()
                             (let ([child (car entry)])
                               (if ($fs-entry-directory? who child (cdr entry) #f)
                                   (remove-tree child)
                                   (delete-file child #t))))))
                        entries)))
                   (attempt (lambda () (delete-directory directory #t)))))
               ;; A trailing slash makes lstat follow the final symlink on POSIX.
               (let ([entry ($removable-entry-path ($expand-file-path who path))])
                 (cond [(not entry)
                        (if err? (errorf who "path must name a removable directory: ~a" path) #f)]
                       [(not (file-exists? entry #f))
                        (if err? ($err-file-not-found who entry) #f)]
                       [(or (not (file-directory? entry #f)) (file-symbolic-link? entry))
                        (if err? ($err-file-not-directory who entry) #f)]
                       [else (remove-tree entry) complete?])))]))


  ;; Remove only terminal dot components. Keep their preceding separator so drive and
  ;; filesystem roots retain their meaning; parents after symlinks must not be normalized.
  (define $mkdirs-target
    (lambda (path)
      (let loop ([end (string-length path)] [cut #f])
        (let ([end (let skip-separators ([index end])
                     (if (and (fx> index 0)
                              (directory-separator? (string-ref path (fx1- index))))
                         (skip-separators (fx1- index))
                         index))])
          (if (and (fx> end 0) (char=? (string-ref path (fx1- end)) #\.)
                   (or (fx= end 1)
                       (directory-separator? (string-ref path (fx- end 2)))))
              (loop (fx1- end) (fx1- end))
              (if cut
                  (if (fxzero? cut) "." (substring path 0 cut))
                  path))))))

  #|proc:mkdirs
  Create directory path string `path` and any missing intermediate directories.
  Integer `mode` supplies the leaf directory's permission bits from 0 through #o7777;
  the default is #o777. Missing parents use #o777. All creation modes are subject to umask.
  Home notation is expanded before creating parents, without lexical normalization.
  Terminal `.` components do not change which newly created directory receives `mode`.
  A final directory first created as an intermediate directory in this call also receives
  the leaf mode, subject to the permissions allowed at creation. Parent traversal is physical.
  Existing directories are left unchanged, including ones created by a concurrent call.
  If applying the leaf mode cannot preserve inherited set-group-ID, an I/O error is raised.
  Return unspecified values.
  |#
  (define-who mkdirs
    (case-lambda
      [(path) (mkdirs path #o777)]
      [(path mode)
       (pcheck ([$filename? path] [$mode-num? mode])
         (let ([path ($mkdirs-target ($expand-file-path who path))])
           (if (file-exists? path)
               (unless (file-directory? path)
                 ($err-file-exists who path))
               (let ([path (string-append path (string (directory-separator)))]
                     ;; Only paths with parents can revisit an intermediate directory.
                     [created (and (string-search path "..")
                                   (make-hashtable equal-hash equal?))])
                 (let loop ([first (path-first path)] [rest (path-rest path)])
                   (if (file-exists? first)
                       (begin
                         (unless (file-directory? first)
                           ($err-file-not-directory who first))
                         (let ([target (and created (string=? rest "")
                                            (hashtable-ref created ($file-identity first #t) #f))])
                           (when target
                             ;; Intermediate #o777 creation records effective permissions,
                             ;; including umask/default ACL restrictions. Linux mkdir ignores
                             ;; requested set-ID bits, preserves inherited SGID, and sets sticky.
                             (let* ([actual (get-mode target)]
                                    [desired (fxlogior
                                              (fxlogand actual
                                                        (fxlogior (fxlogand mode #o777) #o2000))
                                              (fxlogand mode #o1000))])
                               ;; Redundant chmod can clear SGID outside the caller's groups.
                               (unless (fx= actual desired)
                                 ;; The creation path does not traverse this directory itself,
                                 ;; so inspection works after removing its search permission.
                                 (chmod target desired)
                                 (when (and (not (fxzero? (fxlogand desired #o2000)))
                                            (fxzero? (fxlogand (get-mode target) #o2000)))
                                   ($err-file who first
                                              "cannot preserve inherited set-group-ID bit")))))))
                       (let ([made?
                              (guard (failure
                                      [(and (error? failure) (file-directory? first)) #f])
                                (mkdir first (if (string=? rest "") mode #o777))
                                #t)])
                         (when (and made? created (not (string=? rest "")))
                           (hashtable-set! created ($file-identity first #t) first))))
                   (unless (string=? "" rest)
                     (loop (path-build first (path-first rest)) (path-rest rest))))))))]))



  #|macro:define-file-tree
  `define-file-tree` defines two procedures that, when called with a valid path `pdir`,
  will create a file tree rooted at `pdir`.
  The first procedure, named `create-<tree-name>`, operates in non-overwrite mode:
  if the target file tree exists, it raises an error.
  Use the second procedure, `create-<tree-name>!`, to overwrite an existing tree.

  An error is raised if parent of `pdir` does not exist.

  The current implementation supports creating directories, symbolic links,
  textual files, binary files in ChezScheme fasl format, and raw binary files
  consisting of data from bytevectors.
  Permission bits (mode) of each kind of file can be set individually.
  They can be in either octal mode or symbolic mode.
  Directory modes are applied after constructing their children, allowing read-only trees.
  Symlink declarations name exact entries. Overwrite mode replaces an existing file or
  symlink itself, including a symlink to a directory; a real directory is rejected.
  A file declaration without content touches the entry, preserving existing bytes in overwrite
  mode. Use `(text "")` to explicitly truncate a file to empty contents.
  If mode is not given, default mode is applied.
  Owner and group of files are the effective user and group of the process.

  Symlink source, `<link-src>`, is written as is, to the symlink file.
  To avoid dangling symlink error, make sure the source file already exists.

  Macro syntax:

  ```
  (define-file-tree <tree-name> <decl>*)

  <decl> := <dir-decl> | <file-decl>

  <dir-decl> := (dir <name>)
             |  (dir <name> <mode>)
             |  (dir <name> <decl>+)
             |  (dir <name> <mode> <decl>*)

  <file-decl> := (file <name>)
              |  (file <name> <mode>)
              |  (file <name> <file-type-decl>)
              |  (file <name> <mode> <file-type-decl>)
              |  (symlink <link-src> <link-target>)

  <file-type-decl> := <reg-file-decl> | <symlink-decl> |  ...

  <reg-file-decl> := (text <expr>)
                  |  (lines <expr>)
                  |  (fasl <expr>)
                  |  (raw  <expr>)

  <symlink-decl> := (symlink <link-src>)

  <name> := <string>
  <link-src> := <string>
  <link-target> := <string>
  <mode> := (mode <m>)
  <m> := <octal-mode> | <symbolic-mode>
  ```
  |#
  (define-syntax define-file-tree
    (lambda (stx)
      (define parse-file-type-decl
        (lambda (who overwrite? pdir path decl)
          (syntax-case decl ()
            [(ty e)
             (eq? 'text (datum ty))
             #`(let ([v e])
                 (if (string? v)
                     #,(if overwrite?
                           #`(write-string! #,path v)
                           #`(write-string  #,path v))
                     (errorf '#,who "fail to write text file: ~a, not a string: ~a" #,path v)))]
            [(ty e)
             (eq? 'lines (datum ty))
             #`(let ([v e])
                 (if (list? v)
                     #,(if overwrite?
                           #`(write-lines! #,path v)
                           #`(write-lines  #,path v))
                     (errorf '#,who "fail to write text file: ~a, not a list of strings: ~a" #,path v)))]
            [(ty e)
             (eq? 'fasl (datum ty))
             #`(let ([v e])
                 #,(if overwrite?
                       #`(write-datum-fasl! #,path v)
                       #`(write-datum-fasl  #,path v)))]
            [(ty e)
             (eq? 'raw (datum ty))
             #`(let ([v e])
                 (if (bytevector? v)
                     #,(if overwrite?
                           #`(write-u8vec! #,path v)
                           #`(write-u8vec  #,path v))
                     (errorf '#,who "fail to write raw file: ~a, not a bytevector: ~a" #,path v)))]
            [(ty e)
             (eq? 'symlink (datum ty))
             #`(let ([v e])
                 (if (string? v)
                     ($file-tree-symlink '#,who v #,path #,overwrite?)
                     (errorf '#,who "fail to create symlink at ~a, not a valid path: ~a" #,path v)))]
            [_ (syntax-violation 'define-file-tree
                                 "invalid file type:" decl)])))
      (define parse-file-decl
        (lambda (who overwrite? pdir decl)
          (syntax-case decl ()
            [(name . rest)
             (with-syntax ([(p) (generate-temporaries '(p))])
               #`(let ([n name])
                   (if ($filename? n)
                       (let ([p (path-build #,pdir n)])
                         #,(syntax-case #'rest ()
                             ;; files without contents are always touched
                             [() #'(file-touch p (current-time) #t)]
                             [((mode m m* ...))
                              (eq? 'mode (datum mode))
                              #`(begin (file-touch p (current-time) #t)
                                       (file-chmod p m m* ...))]
                             [((mode m m* ...) type-decl)
                              (eq? 'mode (datum mode))
                              #`(begin #,(if overwrite?
                                             (parse-file-type-decl who #t pdir #'p #'type-decl)
                                             (parse-file-type-decl who #f pdir #'p #'type-decl))
                                       (file-chmod p m m* ...))]
                             [(type-decl)
                              (if overwrite?
                                  (parse-file-type-decl who #t pdir #'p #'type-decl)
                                  (parse-file-type-decl who #f pdir #'p #'type-decl))]
                             [_ (syntax-violation 'define-file-tree
                                                  "invalid file declaration:" #'rest)]))
                       (errorf '#,who "not a valid path: ~a" n))))]
            [_ (syntax-violation 'define-file-tree
                                 "invalid file declaration:" decl)])))
      (define parse-dir-decl
        (lambda (who overwrite? pdir decl)
          (syntax-case decl ()
            [(name . rest)
             (with-syntax ([(p) (generate-temporaries '(p))])
               #`(let ([n name])
                   (if ($filename? n)
                       (let ([p (path-build #,pdir n)])
                         #,(syntax-case #'rest ()
                             [() (if overwrite? #'(mkdirs p) #'(mkdir p))]
                             [((mode m m* ...))
                              (eq? 'mode (datum mode))
                              #`(begin #,(if overwrite? #'(mkdirs p) #'(mkdir p))
                                       (file-chmod p m m* ...))]
                             [((mode m m* ...) decl* ...)
                              (eq? 'mode (datum mode))
                              #`(begin #,(if overwrite? #'(mkdirs p) #'(mkdir p))
                                       #,(parse-decls overwrite? who #'p #'(decl* ...))
                                       (file-chmod p m m* ...))]
                             [(decl* ...)
                              #`(begin #,(if overwrite? #'(mkdirs p) #'(mkdir p))
                                       #,(parse-decls overwrite? who #'p #'(decl* ...)))]
                             [_ (syntax-violation 'define-file-tree
                                                  "invalid directory declaration:" #'rest)]))
                       (errorf '#,who "not a valid path: ~a" n))))]
            [_ (syntax-violation 'define-file-tree
                                 "invalid directory declaration:" decl)])))
      (define parse-decls
        (lambda (overwrite? who pdir decl*)
          #`(begin #,@(map (lambda (decl)
                             (syntax-case decl ()
                               [(file . rest)
                                (eq? 'file (datum file))
                                (parse-file-decl who overwrite? pdir #'rest)]
                               [(dir  . rest)
                                (eq? 'dir (datum dir))
                                (parse-dir-decl who overwrite? pdir #'rest)]
                               [(symlink src path)
                                (eq? 'symlink (datum symlink))
                                #`(let ([s src] [p path])
                                    (unless (string? s)
                                      (errorf '#,who "fail to create symlink at ~a, source not valid: ~a" p s))
                                    (unless (string? p)
                                      (errorf '#,who "fail to create symlink at ~a, target not valid: ~a" p p))
                                    (let ([target (path-build #,pdir p)])
                                      ($file-tree-symlink '#,who s target #,overwrite?)))]
                               [_ (syntax-violation 'define-file-tree
                                                    "invalid file/directory declaration:" decl)]))
                           decl*))))
      (syntax-case stx ()
        [(k tree-name decl* ...)
         (with-syntax ([(pdir $mk) (generate-temporaries '(pdir $mk))]
                       [mk  ($construct-name #'k "create-" #'tree-name)]
                       [mk! ($construct-name #'k "create-" #'tree-name "!")])
           #`(begin
               (define $mk
                 (lambda (who pdir ow?)
                   (pcheck ([$filename? pdir] [boolean? ow?])
                           (if (file-exists? pdir)
                               (if ow?
                                   (unless (file-directory? pdir)
                                     ($err-file-exists who pdir))
                                   ($err-directory-exists who pdir))
                               (mkdir pdir))
                           (if ow?
                               #,(parse-decls #t #'mk! #'pdir #'(decl* ...))
                               #,(parse-decls #f #'mk  #'pdir #'(decl* ...))))))

               (define-who mk  (lambda (pdir) ($mk who pdir #f)))
               (define-who mk! (lambda (pdir) ($mk who pdir #t)))))])))




;;;;===----------------------------------------------------------------------===
;;;; filesystem watcher
;;;;===----------------------------------------------------------------------===

  #|proc:fswatcher-error?
  Return whether arbitrary `object` contains a filesystem-watcher I/O error condition.
  A watcher error also contains an I/O filename condition when the affected path is known.
  Pathless failures, such as descriptor exhaustion during initialization, omit that condition.
  |#
  (define-condition-type &fswatcher-error &i/o make-fswatcher-error fswatcher-error?)

  (define $err-fswatcher-error
    (case-lambda
      [(who message) ($err-fswatcher-error who #f message)]
      [(who path message)
       (raise (if path
                  (condition (make-who-condition who) (make-fswatcher-error)
                             (make-i/o-filename-error path) (make-message-condition message))
                  (condition (make-who-condition who) (make-fswatcher-error)
                             (make-message-condition message))))]))

  (define $fsw-init
    (let ([ffi (foreign-procedure "cspp_fswatcher_init" (boolean) ptr)])
      (lambda (who block?)
        (let ([x (ffi block?)])
          (if (string? x)
              ($err-fswatcher-error who x)
              x)))))
  (define $fsw-close
    (let ([ffi (foreign-procedure "cspp_fswatcher_close" (int) ptr)])
      (lambda (who fswid)
        (let ([x (ffi fswid)])
          (if (string? x)
              ($err-fswatcher-error who x)
              x)))))
  (define $fsw-add!
    (let ([ffi (foreign-procedure "cspp_fswatcher_add" (int string unsigned-32) ptr)])
      (lambda (who fswid path events)
        (let ([x (ffi fswid path events)])
          (if (string? x)
              ($err-fswatcher-error who path x)
              x)))))
  (define $fsw-remove!
    (let ([ffi (foreign-procedure "cspp_fswatcher_remove" (int int) ptr)])
      (lambda (who fswid fid path)
        (let ([x (ffi fswid fid)])
          (if (string? x)
              ($err-fswatcher-error who path x)
              x)))))
  (define $fsw-next!
    (let ([ffi (foreign-procedure "cspp_fswatcher_next" (int int) ptr)])
      (lambda (who fswid timeout)
        (let ([x (ffi fswid timeout)])
          (if (string? x)
              ($err-fswatcher-error who x)
              x)))))


  #|macro:fswatcher-mask
  Filesystem events bitmask that can be given to `fswatcher-add!` or returned by `fswatcher-next!`.
  Use expressions like `(fswatcher-mask FSW_access FSW_modify)` to make the right bitmask.
  |#
  #|proc:fswatcher-mask->symbols
  Decode unsigned 32-bit integer `mask` into a list of fully present `FSW_*` symbols.
  Composite aliases are included only when all their constituent bits are set.
  Symbols follow declaration order; unknown bits are ignored.
  |#
  (define-flags fswatcher-mask u32?
    (FSW_access        #x00000001)
    (FSW_modify        #x00000002)
    (FSW_attrib        #x00000004)
    (FSW_close_write   #x00000008)
    (FSW_close_nowrite #x00000010)
    (FSW_open          #x00000020)
    (FSW_moved_from    #x00000040)
    (FSW_moved_to      #x00000080)
    (FSW_create        #x00000100)
    (FSW_delete        #x00000200)
    (FSW_delete_self   #x00000400)
    (FSW_move_self     #x00000800)
    (FSW_unmount       #x00002000)
    (FSW_overflow      #x00004000)
    (FSW_ignored       #x00008000)
    (FSW_onlydir       #x01000000)
    (FSW_dont_follow   #x02000000)
    (FSW_excl_unlink   #x04000000)
    (FSW_mask_create   #x10000000)
    (FSW_mask_add      #x20000000)
    (FSW_isdir         #x40000000)
    (FSW_oneshot       #x80000000)
    (FSW_close         (or FSW_close_write FSW_close_nowrite))
    (FSW_move          (or FSW_moved_from  FSW_moved_to))
    (FSW_all           (or FSW_access FSW_modify FSW_attrib FSW_close FSW_open FSW_move FSW_create FSW_delete FSW_delete_self FSW_move_self)))

  (define-record-type $fswatcher
    (nongenerative) (sealed #t) (opaque #t)
    ;; paths maps watch ids to pairs of (first-path . active?).
    (fields (mutable open?) (immutable id) (immutable blocking?)
            (immutable paths) (mutable pending)))

  (define $fswatcher-timeout?
    (lambda (timeout)
      (and (integer? timeout) (exact? timeout) (<= 0 timeout #x7fffffff))))

  #|proc:make-fswatcher
  The `make-fswatcher` procedure returns a new filesystem watcher.
  The optional boolean `blocking?` defaults to `#t`. It controls whether a one-argument
  `fswatcher-next!` waits for an event or returns `#f` immediately when none is available.
  Close the returned watcher with `close-fswatcher` when finished. Operations on the same
  watcher must be serialized; do not close it while another thread waits for an event.
  |#
  (define-who make-fswatcher
    (case-lambda
      [() (make-fswatcher #t)]
      [(blocking?)
       (pcheck ([boolean? blocking?])
               (let ([id ($fsw-init who blocking?)])
                 (make-$fswatcher #t id blocking? (make-eqv-hashtable) '())))]))

  #|proc:close-fswatcher
  The `close-fswatcher` procedure closes the open watcher `watcher`, releases its native
  descriptor, and discards pending events. It returns an unspecified value.
  Further operations on `watcher`, including closing it again, raise an error.
  |#
  (define-who close-fswatcher
    (lambda (watcher)
      (pcheck ([$fswatcher? watcher] [$fswatcher-open? watcher])
              ($fsw-close who ($fswatcher-id watcher))
              ($fswatcher-open?-set! watcher #f)
              (hashtable-clear! ($fswatcher-paths watcher))
              ($fswatcher-pending-set! watcher '()))))

  #|proc:fswatcher-add!
  The `fswatcher-add!` procedure watches the filesystem path string `path` using `watcher`.
  The optional `events` parameter is an unsigned 32-bit mask made with `fswatcher-mask`.
  It returns an unspecified value. Directories are watched without recursion.
  Multiple paths to the same inode share a watch; events report the first recorded path.
  The recorded path is copied, so later mutations of the input string do not change it.
  Adding that inode again replaces its event mask unless `FSW_mask_add` is supplied.
  `FSW_dont_follow` watches a symbolic link itself, including a dangling link.
  Native addition failures include `path` in the I/O filename condition.

  The default mask for files is:
  `(fswatcher-mask FSW_access FSW_open FSW_modify FSW_close FSW_delete_self)`.
  The default mask for directories additionally includes `FSW_create`, `FSW_delete`, and
  `FSW_excl_unlink`.
  |#
  (define-who fswatcher-add!
    (case-lambda
      [(watcher path)
       (pcheck ([$fswatcher? watcher] [$fswatcher-open? watcher] [$filename? path])
               (cond [(file-directory? path)
                      (fswatcher-add! watcher path
                                      (fswatcher-mask FSW_access FSW_open FSW_modify FSW_close
                                                     FSW_create FSW_delete FSW_delete_self
                                                     FSW_excl_unlink))]
                     [(file-exists? path)
                      (fswatcher-add! watcher path
                                      (fswatcher-mask FSW_access FSW_open FSW_modify FSW_close
                                                     FSW_delete_self))]
                     [else ($err-file-not-found who path)]))]
      [(watcher path events)
       (pcheck ([$fswatcher? watcher] [$fswatcher-open? watcher] [$filename? path] [u32? events])
               ;; Let inotify validate existence using the requested symlink semantics.
               (let* ([fid ($fsw-add! who ($fswatcher-id watcher) path events)]
                      [paths ($fswatcher-paths watcher)])
                 (unless (hashtable-contains? paths fid)
                   (hashtable-set! paths fid (cons (string-copy path) #t)))))]))

  (define $fswatcher-next!
    (lambda (who watcher timeout)
      (let ([pending ($fswatcher-pending watcher)])
        (when (null? pending)
          (let ([batch ($fsw-next! who ($fswatcher-id watcher) timeout)])
            (when batch (set! pending (vector->list batch)))))
        (if (null? pending)
            #f
            (let* ([event (car pending)]
                   [fid (vector-ref event 0)]
                   [paths ($fswatcher-paths watcher)]
                   [entry (hashtable-ref paths fid #f)])
              ($fswatcher-pending-set! watcher (cdr pending))
              (vector-set! event 0 (and entry (string-copy (car entry))))
              ;; Keep the path for queued events, including the final ignored notification.
              (when (flag-any-set? (fswatcher-mask FSW_ignored) (vector-ref event 1))
                (hashtable-delete! paths fid))
              (when (eqv? (vector-ref event 2) -1)
                ($err-fswatcher-error who (vector-ref event 0)
                                     "filesystem event name is not valid UTF-8"))
              event)))))

  #|proc:fswatcher-next!
  The `fswatcher-next!` procedure returns the next queued filesystem event from `watcher`.
  An event is a three-element vector: the watched path, the event bitmask, and the child
  filename, or `#f` for events without a child name. A directory event reports the watched
  directory as its path. Each reported path is a fresh string independent of the watcher.
  Queue overflow is reported with `FSW_overflow` and a `#f` path;
  events lost by the kernel cannot be recovered and callers should rescan their files.
  `FSW_ignored` is delivered with the recorded path and retires the watch.
  An event with an undecodable UTF-8 name is consumed and raises a watcher error.
  That error identifies the watched path when it is still recorded.
  Subsequent calls continue with the remaining events, including valid events from the same read.
  The optional `timeout` is an exact integer from 0 through 2147483647, in milliseconds.
  Zero polls immediately; a positive timeout waits at most that long for an event, regardless
  of the watcher's blocking setting. Without `timeout`, the watcher's setting is used.
  The procedure returns `#f` when no event is available before the wait ends.
  |#
  (define-who fswatcher-next!
    (case-lambda
      [(watcher)
       (pcheck ([$fswatcher? watcher] [$fswatcher-open? watcher])
               ($fswatcher-next! who watcher (if ($fswatcher-blocking? watcher) -1 0)))]
      [(watcher timeout)
       (pcheck ([$fswatcher? watcher] [$fswatcher-open? watcher] [$fswatcher-timeout? timeout])
               ($fswatcher-next! who watcher timeout))]))

  #|proc:fswatcher-remove!
  The `fswatcher-remove!` procedure stops watching the recorded path string `path` in
  `watcher` and returns an unspecified value. It also accepts watches already removed by
  the kernel whose `FSW_ignored` event has not yet been consumed.
  If a pathname was reused for another file, remove all active registrations recorded under
  that pathname, including registrations for an earlier file that was renamed or deleted.
  Events already queued, including `FSW_ignored`, remain available with their recorded path.
  An unknown path or a watch explicitly removed already raises a watcher error.
  Removal errors include `path` in the I/O filename condition.
  |#
  (define-who fswatcher-remove!
    (lambda (watcher path)
      (pcheck ([$fswatcher? watcher] [$fswatcher-open? watcher] [$filename? path])
              (let-values ([(ids entries) (hashtable-entries ($fswatcher-paths watcher))])
                (let ([length (vector-length entries)])
                  (let loop ([index 0] [removed? #f])
                    (if (fx= index length)
                        (unless removed?
                          ($err-fswatcher-error who path (format "invalid path: ~a" path)))
                        (let ([entry (vector-ref entries index)])
                          (if (and (cdr entry) (string=? path (car entry)))
                              (begin
                                ($fsw-remove! who ($fswatcher-id watcher)
                                              (vector-ref ids index) path)
                                ;; Retain the entry for attribution of queued notifications.
                                (set-cdr! entry #f)
                                (loop (fx1+ index) #t))
                              (loop (fx1+ index) removed?))))))))))





  (record-writer (type-descriptor $file-stat)
                 (lambda (r p wr)
                   (display "#[stat " p)
                   (newline p)
                   (display (format "~ttype:  ~a~n" (file-type->symbol (file-stat-type r))) p)
                   (display (format "~tmode:  #o~a ~a~n" (number->string (file-stat-mode r) 8) (file-mode->symbols (file-stat-mode r))) p)
                   (display (format "~towner: ~a~n" (file-stat-owner r)) p)
                   (display (format "~tgroup: ~a~n" (file-stat-group r)) p)
                   (display (format "~tinode: ~a~n" (file-stat-inode r)) p)
                   (display (format "~tnlinks: ~a~n" (file-stat-nlinks r)) p)
                   (display (format "~tsize:   ~a~n" (file-stat-size r)) p)
                   (display (format "~tblocks: ~a~n" (file-stat-blocks r)) p)
                   (display (format "~tdev-major: ~a~n" (file-stat-dev-major r)) p)
                   (display (format "~tdev-minor: ~a~n" (file-stat-dev-minor r)) p)
                   (display (format "~tatime: ~a~n" (file-stat-access-time r)) p)
                   (display (format "~tmtime: ~a~n" (file-stat-modification-time r)) p)
                   (display (format "~tctime: ~a~n" (file-stat-change-time r)) p)
                   (display (format "~tbtime: ~a" (file-stat-creation-time r)) p)
                   (display "]" p)))

  )
