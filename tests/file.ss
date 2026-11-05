(import (cspp))



(define $random-file (lambda () (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))))
(define $rand-sign (lambda () (if (= 1 (mod (random 100) 2)) -1 1)))

(define $with-fs-fixture
  (lambda (proc)
    (let ([root ($random-file)])
      (dynamic-wind
        (lambda ()
          (mkdir root)
          (mkdir (path-build root "sub"))
          (file-touch (path-build root "top.txt"))
          (file-touch (path-build (path-build root "sub") "nested.txt")))
        (lambda () (proc root))
        (lambda () (when (file-directory? root #f) (file-removetree root)))))))

;; Bounded collection catches accidental dot-entry recursion without hanging.
(define $fs-take-pass
  (lambda (iterator)
    (let ([out (make-list-builder)])
      (let loop ([remaining 16])
        (let ([value (iter-next! iterator)])
          (cond [(eq? value iter-end) (out)]
                [(zero? remaining) #f]
                [else (out value) (loop (- remaining 1))]))))))

(define $fs-path-index
  (lambda (paths path)
    (let loop ([paths paths] [index 0])
      (if (string=? (car paths) path) index (loop (cdr paths) (+ index 1))))))

(define-record-type $escaped-file-datum
  (fields))

(define $escape-file-write #f)
(record-writer (record-type-descriptor $escaped-file-datum)
  (lambda (datum port write)
    ($escape-file-write 'escaped)))

(mat file-writer-argument-checks
     ;; Error case: every writer rejects a non-string path before filesystem access.
     (for-all
      (lambda (writer payload)
        (guard (failure
                [else (and (who-condition? failure) (eq? (condition-who failure) 'pcheck))])
          (writer #f payload)
          #f))
      (list write-lines write-string write-chars write-data write-datum
            write-data-fasl write-datum-fasl write-u8vec)
      (list '("a") "a" '(#\a) '(a) 'a '(a) 'a #vu8(1)))

     ;; Error cases: malformed collections must fail through pcheck, not unsafe list access.
     (let ([path ($random-file)])
       (and (for-all
             (lambda (value)
               (for-all
                (lambda (writer)
                  (guard (failure
                          [else (and (who-condition? failure)
                                     (eq? (condition-who failure) 'pcheck))])
                    (writer path value)
                    #f))
                (list write-lines write-lines! write-lines>>
                      write-chars write-chars! write-chars>>)))
             (list #\a '(#\a . #\b) '#("a") (let ([x (list #\a)]) (set-cdr! x x) x)))
            (not (file-exists? path #f)))))

(mat fs-directory-stream
     ($with-fs-fixture
      (lambda (root)
        (let ([directory (fs-open-directory root)] [out (make-list-builder)])
          (dynamic-wind
            void
            (lambda ()
              (let loop ()
                (let ([entry (fs-read-directory directory)])
                  (when entry (out entry) (loop))))
              (let ([entries (out)])
                (and (fs-directory? directory)
                     (not (fs-directory-closed? directory))
                     (equal? '("sub" . FT_dir) (assoc "sub" entries))
                     (equal? '("top.txt" . FT_regular) (assoc "top.txt" entries))
                     (not (fs-read-directory directory))
                     (eq? directory (fs-close-directory directory))
                     (fs-directory-closed? directory)
                     (eq? directory (fs-close-directory directory)))))
            (lambda () (fs-close-directory directory)))))))

(mat fs-directory-utf8-entry
     (let* ([name (string (integer->char 233) #\. #\t #\x #\t)]
            [root ($random-file)]
            [path (path-build root name)])
       (dynamic-wind
         (lambda () (mkdir root) (file-touch path))
         (lambda ()
           (let ([directory (fs-open-directory root)] [found #f])
             (let loop ([entry (fs-read-directory directory)])
               (when entry
                 (when (string=? name (car entry)) (set! found #t))
                 (loop (fs-read-directory directory))))
             (fs-close-directory directory)
             found))
         (lambda () (file-removetree root)))))

(mat fs-directory-type-mapping
     (eq? 'FT_regular (file-type->symbol #o100000))
     (eq? 'FT_dir (file-type->symbol #o40000))
     (eq? FT_unknown (file-type->symbol #f)))

(mat fs-directory-iterator-order-and-reset
     ($with-fs-fixture
      (lambda (root)
        (let ([iterator (fs->iter root)]
              [want (sort string<? (list root (path-build root "sub")
                                        (path-build (path-build root "sub") "nested.txt")
                                        (path-build root "top.txt")))])
          (dynamic-wind
            void
            (lambda ()
              (let ([first ($fs-take-pass iterator)])
                (and first
                     (equal? want (sort string<? first))
                     (string=? root (car first))
                     (< ($fs-path-index first (path-build root "sub"))
                        ($fs-path-index first (path-build (path-build root "sub") "nested.txt")))
                     (eq? iter-end (iter-next! iterator))
                     (begin (iter-reset! iterator) #t)
                     (equal? first ($fs-take-pass iterator)))))
            (lambda () (iter-finalize! iterator))))))
     ($with-fs-fixture
      (lambda (root)
        (let ([iterator (fs->iter root #f #f)])
          (dynamic-wind
            void
            (lambda ()
              (let ([paths ($fs-take-pass iterator)])
                (and paths (= 4 (length paths))
                     (string=? root (car (reverse paths)))
                     (< ($fs-path-index paths (path-build (path-build root "sub") "nested.txt"))
                        ($fs-path-index paths (path-build root "sub"))))))
            (lambda () (iter-finalize! iterator)))))))

(mat fs-directory-iterator-links
     ($with-fs-fixture
      (lambda (root)
        ;; A link to the ancestor must be yielded, but never recursively revisited.
        (file-symlink ".." (path-build (path-build root "sub") "back"))
        (let ([plain (fs->iter root #f)] [followed (fs->iter root #t)])
          (dynamic-wind
            void
            (lambda ()
              (let ([paths ($fs-take-pass plain)] [followed-paths ($fs-take-pass followed)])
                (and paths followed-paths
                     (= 5 (length paths)) (= 5 (length followed-paths))
                     (member (path-build (path-build root "sub") "back") followed-paths)
                     #t)))
            (lambda () (iter-finalize! plain) (iter-finalize! followed)))))))

(mat fs-directory-alias-descendants
     ($with-fs-fixture
      (lambda (root)
        (file-symlink "sub" (path-build root "alias"))
        (let ([iterator (fs->iter root #t)])
          (dynamic-wind
            void
            (lambda ()
              (let ([paths (iter->list iterator)])
                (and (member (path-build root "sub/nested.txt") paths)
                     (member (path-build root "alias/nested.txt") paths)
                     #t)))
            (lambda () (iter-finalize! iterator)))))))

(mat fs-directory-result-ownership
     ($with-fs-fixture
      (lambda (root)
        (let* ([input (string-copy root)] [iterator (fs->iter input)])
          (dynamic-wind
            void
            (lambda ()
              (string-set! input 0 #\X)
              (let ([first (iter-next! iterator)])
                (string-set! first 0 #\Y)
                (and (for-all (lambda (entry) (file-exists? entry #f))
                              ($fs-take-pass iterator))
                     (begin (iter-reset! iterator) #t)
                     (string=? root (iter-next! iterator)))))
            (lambda () (iter-finalize! iterator)))))))

(mat fs-directory-iterator-lifetime
     ($with-fs-fixture
      (lambda (root)
        (let* ([baseline (length (directory-list "/proc/self/fd"))]
               [iterator (fs->iter root)])
          (dynamic-wind
            void
            (lambda ()
              (and (= baseline (length (directory-list "/proc/self/fd")))
                   (string=? root (iter-next! iterator))
                   (let loop ([entry (iter-next! iterator)])
                     (and (not (eq? entry iter-end))
                          (or (string=? entry (path-build root "sub"))
                              (loop (iter-next! iterator)))))
                   (>= (length (directory-list "/proc/self/fd")) (+ baseline 2))
                   (begin (iter-reset! iterator) #t)
                   (= baseline (length (directory-list "/proc/self/fd")))
                   (string=? root (iter-next! iterator))
                   (begin (iter-finalize! iterator) #t)
                   (= baseline (length (directory-list "/proc/self/fd")))))
            (lambda ()
              (unless (iter-finalized? iterator) (iter-finalize! iterator))))))))

(mat fs-directory-iterator-fresh-reset
     ($with-fs-fixture
      (lambda (root)
        (let ([iterator (fs->iter root)])
          (dynamic-wind
            void
            (lambda ()
              (iter-next! iterator)
              (file-touch (path-build root "added.txt"))
              (iter-reset! iterator)
              (let ([paths ($fs-take-pass iterator)])
                (and paths (= 5 (length paths))
                     (member (path-build root "added.txt") paths) #t)))
            (lambda () (iter-finalize! iterator)))))))

(mat fs-directory-errors
     ;; Opening a missing directory must raise an error.
     (error? (fs-open-directory "__cspp_missing_directory__"))

     ;; The root is opened lazily; removal before the first next raises then.
     (error? ($with-fs-fixture
              (lambda (root)
                (let ([iterator (fs->iter root)])
                  (file-removetree root)
                  (dynamic-wind
                    void
                    (lambda () (iter-next! iterator))
                    (lambda () (iter-finalize! iterator)))))))

     ;; Reading a closed stream must raise an error.
     (error? (let ([directory (fs-open-directory ".")])
               (fs-close-directory directory)
               (fs-read-directory directory))))

(mat walk-files-cycle
     (let ([root ($random-file)] [visits 0])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (file-symlink "." (path-build root "cycle")))
         (lambda ()
           (walk-files (lambda (directory directories files)
                         (set! visits (fx1+ visits))
                         (fx= visits 1))
                       root #t)
           (= visits 1))
         (lambda () (when (file-directory? root #f) (file-removetree root))))))

(mat walk-files-alias-paths
     (let ([root ($random-file)])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir (path-build root "source"))
           (file-symlink "source" (path-build root "alias"))
           (file-symlink "source" (path-build root "alias2"))
           (file-symlink "." (path-build root "cycle")))
         (lambda ()
           (for-all
            (lambda (top-down?)
              (let ([visited '()])
                (walk-files (lambda (directory directories files)
                              (set! visited (cons directory visited))
                              #t)
                            root #t top-down?)
                (and (= (length visited) 4)
                     (member (path-build root "source") visited)
                     (member (path-build root "alias") visited)
                     (not (not (member (path-build root "alias2") visited))))))
            '(#t #f)))
         (lambda () (when (file-directory? root #f) (file-removetree root))))))

(mat file-find-validates-predicate
     (let ([root ($random-file)])
       (dynamic-wind
         (lambda () (mkdir root))
         (lambda ()
           (and (guard (failure [else #t]) (file-find 42 root) #f)
                (guard (failure [else #t]) (file-find-all 42 root) #f)
                (guard (failure [else #t]) (file-map 42 root) #f)
                (guard (failure [else #t]) (file-for-each 42 root) #f)))
         (lambda () (when (file-directory? root #f) (file-removetree root))))))


(mat simple-read/write

     (let ([path ($random-file)])
       (dynamic-wind
         (lambda () (write-u8vec path #vu8()))
         (lambda () (equal? #vu8() (read-u8vec path)))
         (lambda () (delete-file path))))

     ;; Error case: invalid directory modes must be rejected even when the path exists.
     ($with-fs-fixture
      (lambda (root)
        (for-all (lambda (mode)
                   (guard (failure [else #t]) (mkdirs root mode) #f))
                 '(#f bad -1 4096))))

     ;; Error case: a malformed datum must close the opened input port.
     (let ([path ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-string path "(")
           (let ([before (length (directory-list "/proc/self/fd"))])
             (do ([index 0 (fx1+ index)]) ((fx= index 8))
               (guard (failure [else (void)]) (read-datum path)))
             (= before (length (directory-list "/proc/self/fd")))))
         (lambda () (when (file-exists? path) (delete-file path)))))

     ;; Error case: continuation escape from a writer must close its output port.
     (let ([path ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (let ([before (length (directory-list "/proc/self/fd"))])
             (call/cc (lambda (continuation)
                        (set! $escape-file-write continuation)
                        (write-datum path (make-$escaped-file-datum))))
             (= before (length (directory-list "/proc/self/fd")))))
         (lambda ()
           (when (file-exists? path) (delete-file path)))))

     ;; errors
     ;; path not string
     (error? (write-lines  #f (map (lambda (x) (random-string 10)) (iota 100))))
     (error? (write-string 0 (random-string 10 20)))
     (error? (write-chars  'bla (random-char)))
     (error? (write-data   #t (random-list)))
     (error? (write-datum  '() (random-datum)))

     ;; file not exist
     (error? (read-lines  "bla.bla"))
     (error? (read-string "bla.bla"))
     (error? (read-chars  "bla.bla"))
     (error? (read-data   "bla.bla"))
     (error? (read-datum  "bla.bla"))
     (error? (read-datum-fasl  "bla.bla"))

     ;; type errors
     (error? (write-lines  "bla.bla" (map (lambda (x) (random-char)) (iota 100))))
     (error? (write-string "bla.bla" (random-list 10 20)))
     (error? (write-chars  "bla.bla" (random-box)))
     (error? (write-data   "bla.bla" #f))
     (error? (write-datum  "bla.bla" (make-eq-hashtable)))
     (error? (write-u8vec "path" 123))


;;; normal IO
     ;; lines
     (let ([lines (map (lambda (x) (random-string 100)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-lines file lines)
           (equal? lines
                   (read-lines file)))
         (lambda () (delete-file file))))
     (let ([lines (map (lambda (x) (random-string 1000)) (iota 1000))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-lines file lines)
           (equal? lines
                   (read-lines file)))
         (lambda () (delete-file file))))


     ;; string
     (let ([str (random-string 100)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-string file str)
           (equal? str
                   (read-string file)))
         (lambda () (delete-file file))))
     (let ([str (random-string 1000)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-string file str)
           (equal? str
                   (read-string file)))
         (lambda () (delete-file file))))


     ;; chars
     (let ([chars (map (lambda (x) (random-char)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-chars file chars)
           (equal? chars
                   (read-chars file)))
         (lambda () (delete-file file))))
     (let ([chars (map (lambda (x) (random-char)) (iota 1000))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-chars file chars)
           (equal? chars
                   (read-chars file)))
         (lambda () (delete-file file))))


     ;; data
     (let ([data (map (lambda (x) (random-datum)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data file data)
           (equal? data
                   (read-data file)))
         (lambda () (delete-file file))))
     (let ([data (map (lambda (x) (random-datum)) (iota 1000))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data file data)
           (equal? data
                   (read-data file)))
         (lambda () (delete-file file))))


     ;; datum
     (andmap (lambda (x)
               (let ([d (random-datum)]
                     [file ($random-file)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (write-datum file d)
                     (equal? d
                             (read-datum file)))
                   (lambda () (delete-file file)))))
             (iota 10))


     ;; write-data-fasl
     (let ([data (map (lambda (x) (random-datum)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data-fasl file data)
           (equal? data
                   (read-data-fasl file)))
         (lambda () (delete-file file))))
     (let ([data (map (lambda (x) (random-datum)) (iota 1000))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data-fasl file data)
           (equal? data
                   (read-data-fasl file)))
         (lambda () (delete-file file))))


     ;; write-datum-fasl
     (andmap (lambda (x)
               (let ([d (random-datum)]
                     [file ($random-file)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (write-datum-fasl file d)
                     (equal? d
                             (read-datum-fasl file)))
                   (lambda () (delete-file file)))))
             (iota 10))

     ;; u8vec
     (let ([data (random-u8vec 1024 2048)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-u8vec file data)
           (equal? data
                   (read-u8vec file)))
         (lambda () (delete-file file))))
     (let ([data (random-u8vec 2048 4096)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-u8vec file data)
           (equal? data
                   (read-u8vec file)))
         (lambda () (delete-file file))))


;;; truncate

     (let ([lines (map (lambda (x) (random-string 100)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-lines file lines)
           (write-lines! file lines)
           (write-lines! file lines)
           (equal? lines
                   (read-lines file)))
         (lambda () (delete-file file))))


     (let ([str (random-string 100)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-string file str)
           (write-string! file str)
           (write-string! file str)
           (equal? str
                   (read-string file)))
         (lambda () (delete-file file))))


     (let ([chars (map (lambda (x) (random-char)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-chars file chars)
           (write-chars! file chars)
           (write-chars! file chars)
           (equal? chars
                   (read-chars file)))
         (lambda () (delete-file file))))


     (let ([data (map (lambda (x) (random-datum)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data file data)
           (write-data! file data)
           (write-data! file data)
           (equal? data
                   (read-data file)))
         (lambda () (delete-file file))))


     ;; write-datum
     (andmap (lambda (x)
               (let ([d (random-datum)]
                     [file ($random-file)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (write-datum-fasl! file d)
                     (write-datum-fasl! file d)
                     (equal? d
                             (read-datum-fasl file)))
                   (lambda () (delete-file file)))))
             (iota 10))


     ;; data-fasl
     (let ([data (map (lambda (x) (random-datum)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data-fasl file data)
           (write-data-fasl! file data)
           (write-data-fasl! file data)
           (equal? data
                   (read-data-fasl file)))
         (lambda () (delete-file file))))


     ;; datum-fasl
     (andmap (lambda (x)
               (let ([d (random-datum)]
                     [file ($random-file)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (write-datum-fasl! file d)
                     (write-datum-fasl! file d)
                     (equal? d
                             (read-datum-fasl file)))
                   (lambda () (delete-file file)))))
             (iota 10))

     ;; u8vec
     (let ([data (random-u8vec 1024 2048)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-u8vec! file data)
           (write-u8vec! file data)
           (equal? data
                   (read-u8vec file)))
         (lambda () (delete-file file))))
     (let ([data (random-u8vec 2048 4096)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-u8vec! file data)
           (write-u8vec! file data)
           (equal? data
                   (read-u8vec file)))
         (lambda () (delete-file file))))


;;; append

     (let ([lines (map (lambda (x) (random-string 100)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-lines file lines)
           (write-lines>> file lines)
           (write-lines>> file lines)
           (equal? (append lines lines lines)
                   (read-lines file)))
         (lambda () (delete-file file))))


     (let ([str (random-string 100)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-string file str)
           (write-string>> file str)
           (write-string>> file str)
           (equal? (string-append str str str)
                   (read-string file)))
         (lambda () (delete-file file))))


     (let ([chars (map (lambda (x) (random-char)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-chars file chars)
           (write-chars>> file chars)
           (write-chars>> file chars)
           (equal? (append chars chars chars)
                   (read-chars file)))
         (lambda () (delete-file file))))


     (let ([data (map (lambda (x) (random-datum)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data file data)
           (write-data>> file data)
           (write-data>> file data)
           (equal? (append data data data)
                   (read-data file)))
         (lambda () (delete-file file))))


     ;; write-datum
     (andmap (lambda (x)
               (let ([d (random-datum)]
                     [file ($random-file)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (write-datum>> file d)
                     (write-datum>> file d)
                     (equal? d
                             (read-datum file)))
                   (lambda () (delete-file file)))))
             (iota 10))


     ;; data-fasl
     (let ([data (map (lambda (x) (random-datum)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data-fasl file data)
           (write-data-fasl>> file data)
           (write-data-fasl>> file data)
           (equal? (append data data data)
                   (read-data-fasl file)))
         (lambda () (delete-file file))))


     ;; datum-fasl
     (andmap (lambda (x)
               (let ([d (random-datum)]
                     [file ($random-file)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (write-datum-fasl>> file d)
                     (write-datum-fasl>> file d)
                     (write-datum-fasl>> file d)
                     (equal? (list d d d)
                             ;; not read-datum-fasl
                             (read-data-fasl file)))
                   (lambda () (delete-file file)))))
             (iota 10))

     ;; u8vec
     (let ([data (random-u8vec 1024 2048)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-u8vec>> file data)
           (write-u8vec>> file data)
           (let* ([len (bytevector-length data)] [d (make-bytevector (* 2 len))])
             (bytevector-copy! data 0 d 0 len)
             (bytevector-copy! data 0 d len len)
             (equal? d (read-u8vec file))))
         (lambda () (delete-file file))))
     (let ([data (random-u8vec 2048 4096)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-u8vec>> file data)
           (write-u8vec>> file data)
           (let* ([len (bytevector-length data)] [d (make-bytevector (* 2 len))])
             (bytevector-copy! data 0 d 0 len)
             (bytevector-copy! data 0 d len len)
             (equal? d (read-u8vec file))))
         (lambda () (delete-file file))))

     )


(define-syntax test-put/get
  (syntax-rules ()
    [(_ rseed iota put end1 get end2)
     (let-values ([(p getter) (open-bytevector-output-port)]
                  [(nums) (map (lambda (x) (random rseed)) iota)])
       (call-with-port p
         (lambda (p)
           (for-each (sect put p _ end1) nums)
           (call-with-port (open-bytevector-input-port (getter))
             (lambda (pin)
               (let loop ([nums nums])
                 (if (null? nums)
                     (port-eof? pin)
                     (and (= (car nums) (get pin end2))
                          (loop (cdr nums))))))))))]
    [(_ rseed iota put get)
     (test-put/get rseed iota put 'little get 'little)]))

(mat gets&puts

     (test-put/get (ash 1 16) (iota 4096) put-u16 get-u16)
     (test-put/get (ash 1 32) (iota 4096) put-u32 get-u32)
     (test-put/get (ash 1 64) (iota 4096) put-u64 get-u64)

     (test-put/get (ash 1 15) (iota 4096) put-s16 get-s16)
     (test-put/get (ash 1 31) (iota 4096) put-s32 get-s32)
     (test-put/get (ash 1 63) (iota 4096) put-s64 get-s64)

     ;; big endian
     (test-put/get (ash 1 16) (iota 4096) put-u16 'big get-u16 'big)
     (test-put/get (ash 1 32) (iota 4096) put-u32 'big get-u32 'big)
     (test-put/get (ash 1 64) (iota 4096) put-u64 'big get-u64 'big)

     (test-put/get (ash 1 15) (iota 4096) put-s16 'big get-s16 'big)
     (test-put/get (ash 1 31) (iota 4096) put-s32 'big get-s32 'big)
     (test-put/get (ash 1 63) (iota 4096) put-s64 'big get-s64 'big)

     ;; bad endian
     (not (test-put/get (ash 1 16) (iota 1024) put-u16 'big get-u16 'little))
     (not (test-put/get (ash 1 16) (iota 1024) put-u16 'little get-u16 'big))
     (not (test-put/get (ash 1 15) (iota 1024) put-s16 'big get-s16 'little))
     (not (test-put/get (ash 1 15) (iota 1024) put-s16 'big get-s16 'little))

     ;; error endian
     (error? (test-put/get (ash 1 15) (iota 4096) put-s16 'big get-s16 'giant))
     (error? (not (test-put/get (ash 1 16) (iota 1024) put-u16 'giant get-u16 'little)))
     (error? (test-put/get (ash 1 63) (iota 4096) put-s64 'big get-s64 'unknown))
     (error? (test-put/get (ash 1 63) (iota 4096) put-u64 '? get-u64 'big))

     ;; range error
     (error? (test-put/get (ash 1 32) (iota 1024) put-u16 get-u16))
     (error? (test-put/get (ash 1 64) (iota 1024) put-u32 get-u32))
     (error? (test-put/get (ash 1 87) (iota 1024) put-u64 get-u64))
     (error? (test-put/get (ash 1 32) (iota 1024) put-s16 get-s16))
     (error? (test-put/get (ash 1 34) (iota 1024) put-s32 get-s32))
     (error? (test-put/get (ash 1 66) (iota 1024) put-s64 get-s64))

     ;; error case: get-u32 requires an open binary input port.
     (error? (get-u32 (open-string-input-port "abcd")))

     ;; error case: get-u64 requires an open binary input port.
     (error? (get-u64 (open-string-input-port "abcdefgh")))

     ;; error case: get-s32 requires an open binary input port.
     (error? (get-s32 (open-string-input-port "abcd")))

     ;; error case: get-s64 requires an open binary input port.
     (error? (get-s64 (open-string-input-port "abcdefgh")))

     ;; An output callback may reenter integer IO; nested writes must not replace outer bytes.
     (let-values ([(other extract-other) (open-bytevector-output-port)])
       (let* ([written (make-list-builder)]
              [port (make-custom-binary-output-port
                     "reentrant-integer-write"
                     (lambda (bytes start count)
                       (put-u16 other #x3344)
                       (do ([index start (fx1+ index)])
                           ((fx= index (fx+ start count)))
                         (written (bytevector-u8-ref bytes index)))
                       count)
                     #f #f void)])
         (dynamic-wind
           void
           (lambda ()
             (set-port-output-size! port 0)
             (put-u16 port #x1122)
             (flush-output-port port)
             (equal? '(#x22 #x11) (written)))
           (lambda ()
             (close-port port)
             (close-port other)))))

     )


(mat file-stat

     (let ([stat (file-stat "file.ss")])
       (and (file-stat? stat)
            (eq? 'FT_regular (file-type->symbol (file-stat-type stat)))
            (positive? (file-stat-size stat))))

     ;; File-stat accessors reject values that are not stat snapshots.
     (error? (file-stat-size #f))

     ;; Directory size comes from metadata without opening the directory as a file.
     (let ([directory ($random-file)])
       (dynamic-wind
         (lambda () (mkdir directory))
         (lambda () (integer? (file-size directory)))
         (lambda () (when (file-directory? directory #f) (file-removetree directory)))))

     (error? (file-stat "/dev/xxxx"))

     (file-readable? "/proc/meminfo")
     (not (file-writable? "/proc/meminfo"))
     (file-executable? "/usr/bin/bash")
     (if (file-exists? "~/.bashrc") (file-hidden? "~/.bashrc") #t)
     (if (file-exists? "~/.bash_profile") (file-hidden? "~/.bash_profile") #t)

     (file-special? "/dev/tty")
     (file-special? "/dev/zero")
     (file-special? "/dev/random")
     (file-special? "/dev/null")

     (eq? 'FT_chardev (file-type "/dev/zero"))
     (eq? 'FT_chardev (file-type "/dev/random"))
     (eq? 'FT_chardev (file-type "/dev/null"))
     )

(mat file-stat-snapshot-accessors
     ($with-fs-fixture
       (lambda (root)
         (let* ([path (path-build root "top.txt")]
                [stat (file-stat path)])
           (and (file-stat? stat)
                (not (file-stat? #f))
                (eq? 'FT_regular (file-type->symbol (file-stat-type stat)))
                (= (file-stat-mode stat) (file-mode path))
                (= (file-stat-inode stat) (file-inode path))
                (= (file-stat-owner stat) (file-owner path))
                (= (file-stat-group stat) (file-group path))
                (= (file-stat-nlinks stat) (file-nlinks path))
                (= (file-stat-size stat) (file-size path))
                (= (file-stat-blocks stat) (file-blocks path))
                (= (file-stat-dev-major stat) (file-dev-major path))
                (= (file-stat-dev-minor stat) (file-dev-minor path))
                (time? (file-stat-access-time stat))
                (time? (file-stat-modification-time stat))
                (time? (file-stat-change-time stat))
                (or (not (file-stat-creation-time stat))
                    (time? (file-stat-creation-time stat)))
                (begin (write-string! path "longer")
                       (and (= 0 (file-stat-size stat)) (= 6 (file-size path))))))))

     ;; Every exported field accessor rejects a non-stat object before accessing fields.
     (for-all (lambda (accessor)
                (guard (condition [else #t]) (accessor #f) #f))
              (list file-stat-type file-stat-mode file-stat-inode file-stat-owner
                    file-stat-group file-stat-nlinks file-stat-size file-stat-blocks
                    file-stat-dev-major file-stat-dev-minor file-stat-access-time
                    file-stat-modification-time file-stat-change-time file-stat-creation-time)))


(mat file-access

     ;; any read bit should make a file readable under mode-bit semantics.
     (let ([p ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (file-touch p)
           (file-chmod p #o004)
           (and (file-readable? p)
                (file-readable? p #t)))
         (lambda () (when (file-exists? p) (delete-file p)))))

     ;; file without read bits should not report readable.
     (let ([p ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (file-touch p)
           (file-chmod p #o000)
           (not (file-readable? p)))
         (lambda () (when (file-exists? p) (delete-file p)))))

     ;; any write bit should make a file writable under mode-bit semantics.
     (let ([p ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-string p "x")
           (file-chmod p #o002)
           (file-writable? p))
         (lambda () (when (file-exists? p) (delete-file p)))))

     ;; any execute bit should make a file executable under mode-bit semantics.
     (let ([p ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-string p "#!/bin/sh\nexit 0\n")
           (file-chmod p #o001)
           (file-executable? p))
         (lambda () (when (file-exists? p) (delete-file p)))))

     ;; error case: access predicates require an existing path.
     (error? (file-readable? "file_access_does_not_exist")))


(mat file-chmod

     (error? (file-mode->symbols ""))
     (error? (file-mode->symbols #o77771))

     (equal? '((su sg t) (r w x) (r w x) (r w x)) (file-mode->symbols #o7777))
     (equal? '((su sg) (r w) (r w) (r w)) (file-mode->symbols #o6666))
     (equal? '((t) (x) (x) (x)) (file-mode->symbols #o1111))
     (equal? '(() (r w) (r w) (r)) (file-mode->symbols #o664))


     (error? (symbols->file-mode '()))
     (error? (symbols->file-mode '() '()))
     (error? (symbols->file-mode '() '() '()))
     (error? (symbols->file-mode '() '() '() '(s)))
     (error? (symbols->file-mode '() '() '() 123))
     (error? (symbols->file-mode '() '() '(a) '()))
     (error? (symbols->file-mode '() '(bla) '() '()))
     (error? (symbols->file-mode '(a) '() '() '()))
     (error? (symbols->file-mode '() '() '(r r) '()))
     (error? (symbols->file-mode '() '() '() '(r x x)))
     (error? (symbols->file-mode '(t t) '() '() '()))

     (= 0 (symbols->file-mode '() '() '() '()))
     (= #o400 (symbols->file-mode '() '(r) '() '()))
     (= #o444 (symbols->file-mode '() '(r) '(r) '(r)))
     (= #o1111 (symbols->file-mode '(t) '(x) '(x) '(x)))
     (= #o421 (symbols->file-mode '() '(r) '(w) '(x)))
     (= #o5562 (symbols->file-mode '(t su) '(x r) '(w r) '(w)))
     (= #o7777 (symbols->file-mode '(su sg t) '(r w x) '(r w x) '(r w x)))

     ;; convenience chmod APIs should pass the path through to the shared implementation.
     (let ([p ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (file-touch p)
           (file-chmod p #o644)
           (file-chmod-u p '(+ x))
           (fxlogbit? 6 (get-mode p)))
         (lambda () (when (file-exists? p) (delete-file p)))))

     ;; A single symbolic mode change must preserve group and other permissions.
     (let ([path ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (file-touch path)
           (file-chmod path #o644)
           (file-chmod path '(+ x))
           (= #o744 (file-mode path)))
         (lambda () (when (file-exists? path) (delete-file path)))))

     (let ([p ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (file-touch p)
           (file-chmod p #o600)
           (file-chmod-g p '(+ r))
           (fxlogbit? 5 (get-mode p)))
         (lambda () (when (file-exists? p) (delete-file p)))))

     (let ([p ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (file-touch p)
           (file-chmod p #o600)
           (file-chmod-o p '(+ r))
           (fxlogbit? 2 (get-mode p)))
         (lambda () (when (file-exists? p) (delete-file p)))))

     (let ([p ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (file-touch p)
           (file-chmod p #o600)
           (file-chmod-a p '(+ x))
           (and (fxlogbit? 6 (get-mode p))
                (fxlogbit? 3 (get-mode p))
                (fxlogbit? 0 (get-mode p))))
         (lambda () (when (file-exists? p) (delete-file p)))))

     )


(mat define-file-tree

     (begin (define testsrc1 "test src 1")
            (define testsrc2 "test src 2")
            (define lines (map (lambda (x) (random-string)) (iota 10)))
            (define-file-tree fstree
              ;; write-string, write-string!
              (file "README.md" (text "This is a fancy project."))
              ;; mkdir
              (dir "src"
                   ;; write-string(!), file-chmod
                   (file "src1.ss" (mode #o777) (text "somestring"))
                   ;; write-datum-fasl(!)
                   (file "src2.ss" (fasl '(a list of symbols)))
                   (dir "native"
                        ;; empty file, file-touch
                        (file "runtime.c")
                        (file "ranstr" (lines lines))))
              (dir "tests" (mode #o777)
                   (file "test1.ss" (text testsrc1))
                   (file "test2.ss" (text testsrc2))
                   ;; file-symlink(!)
                   (file "src1" (symlink "../src/src1.ss"))
                   ;; link to link, simpler syntax
                   (symlink "src1" "src1.ln")))

            (when (file-directory? "./fstree")
              (file-removetree "./fstree"))

            #t)

     (begin (create-fstree "./fstree")
            #t)

     ;; error case: non-overwrite creator should reject an existing root directory.
     (error? (create-fstree "./fstree"))

     ;; overwrite creator should accept an existing root directory.
     (begin (create-fstree! "./fstree")
            (file-directory? "./fstree"))

     (file-directory? "./fstree")
     (file-directory? "./fstree/src")
     (file-directory? "./fstree/src/native")
     (file-directory? "./fstree/tests")

     (file-regular? "./fstree/README.md")
     (file-regular? "./fstree/src/src1.ss")
     (file-regular? "./fstree/src/src2.ss")
     (file-regular? "./fstree/src/native/runtime.c")
     (file-regular? "./fstree/src/native/ranstr")
     (file-regular? "./fstree/tests/test1.ss")
     (file-regular? "./fstree/tests/test2.ss")

     (file-symbolic-link? "./fstree/tests/src1")
     (file-symbolic-link? "./fstree/tests/src1.ln")
     (string=? "../src/src1.ss" (readlink "./fstree/tests/src1"))
     (string=? "src1" (readlink "./fstree/tests/src1.ln"))

     (string=? "This is a fancy project." (read-string "./fstree/README.md"))

     (string=? "somestring" (read-string "./fstree/src/src1.ss"))
     (= #o777 (get-mode "./fstree/src/src1.ss"))
     (string=? "somestring" (read-string "./fstree/tests/src1"))
     (string=? "somestring" (read-string "./fstree/tests/src1.ln"))

     (equal? '(a list of symbols)
             (read-datum-fasl "./fstree/src/src2.ss"))

     (equal? lines (read-lines "./fstree/src/native/ranstr"))
     (equal? testsrc1 (read-string "./fstree/tests/test1.ss"))
     (equal? testsrc2 (read-string "./fstree/tests/test2.ss"))

     (file-removetree "./fstree")
     (not (file-exists? "./fstree"))
     (not (file-directory? "./fstree"))
     )


(mat file-touch

     (begin (define dir "./touch_test")
            (define atdir (lambda (x) (path-build dir x)))
            (when (file-directory? dir) (file-removetree dir))

            (define-file-tree tree
              (dir "d1"
                   (file "f1")
                   (dir "d2"
                        (symlink "../f1" "ln1")
                        (symlink "ln1" "ln2"))))
            (create-tree dir)
            #t)

     (error? (file-touch 'bla))
     ;; Error case: file-touch rejects a non-time argument at its public boundary.
     (guard (failure [(error? failure) (eq? (condition-who failure) 'pcheck)] [else #f])
       (file-touch (atdir "d1/f1") 42)
       #f)
     ;; Error case: file-touch-atime rejects a non-time argument at its public boundary.
     (guard (failure [(error? failure) (eq? (condition-who failure) 'pcheck)] [else #f])
       (file-touch-atime (atdir "d1/f1") 42)
       #f)
     ;; Error case: file-touch-mtime rejects a non-time argument at its public boundary.
     (guard (failure [(error? failure) (eq? (condition-who failure) 'pcheck)] [else #f])
       (file-touch-mtime (atdir "d1/f1") 42)
       #f)
     ;; path not exist
     (error? (file-touch "a/b/c/d"))

     ;; Error case: native timestamp update failures must be raised to the caller.
     (guard (failure [else #t]) (file-touch-mtime "/sys/kernel") #f)

     (let ([at1 (file-access-time (atdir "d1/f1"))]
           [mt1 (file-modification-time (atdir "d1/f1"))])
       (file-touch (atdir "d1/f1"))
       (let ([at2 (file-access-time (atdir "d1/f1"))]
             [mt2 (file-modification-time (atdir "d1/f1"))])
         (and (time<? at1 at2) (time<? mt1 mt2))))

     ;; create file
     (not (file-exists? (atdir "d1/f2")))
     (begin (file-touch (atdir "d1/f2"))
            (file-exists? (atdir "d1/f2")))

     ;; path is symlink, follow
     (let ([time (current-time)])
       (file-touch (atdir "d1/d2/ln1") time #t #f)
       (let ([at2 (file-access-time (atdir "d1/f1"))]
             [mt2 (file-modification-time (atdir "d1/f1"))])
         (and (time=? time at2) (time=? time mt2))))

     (let ([time (current-time)])
       (file-touch (atdir "d1/d2/ln2") time #t #f)
       (let ([at2 (file-access-time (atdir "d1/f1"))]
             [mt2 (file-modification-time (atdir "d1/f1"))])
         (and (time=? time at2) (time=? time mt2))))

     ;; path is symlink, not follow
     (let ([time (current-time)])
       (file-touch (atdir "d1/d2/ln1") time #f #f)
       (let ([at2 (file-access-time (atdir "d1/d2/ln1") #f)]
             [mt2 (file-modification-time (atdir "d1/d2/ln1") #f)])
         (and (time=? time at2) (time=? time mt2))))


     ;; path is dir
     (let ([time (current-time)])
       (file-touch (atdir "d1") time)
       (let ([at2 (file-access-time (atdir "d1"))]
             [mt2 (file-modification-time (atdir "d1"))])
         (and (time=? time at2) (time=? time mt2))))

     (file-removetree dir))



(mat readlink

     ;; Error case: readlink2 must validate the recursive flag before choosing a branch.
     ($with-fs-fixture
      (lambda (root)
        (let ([link (path-build root "link")])
          (file-symlink "top.txt" link)
          (guard (failure
                  [else (and (who-condition? failure)
                             (eq? (condition-who failure) 'pcheck))])
            (readlink2 link 'yes)
            #f))))

     ;; Hard-linked relative symlinks in different parents can form a finite chain.
     (let ([root ($random-file)])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir (path-build root "next"))
           (mkdir (path-build root "next/next"))
           (write-string (path-build root "next/next/link") "target")
           (file-symlink "next/link" (path-build root "link"))
           (file-link (path-build root "link") (path-build root "next/link") #f))
         (lambda ()
           (string=? "target" (read-string (readlink2 (path-build root "link") #t))))
         (lambda () (file-removetree root))))

     ;; Error case: repeated link identities must stop expanding lexical cycle spellings.
     (let ([root ($random-file)])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir (path-build root "sub"))
           (file-symlink "sub/../cycle" (path-build root "cycle")))
         (lambda ()
           (guard (failure [else #t])
             (readlink2 (path-build root "cycle") #t)
             #f))
         (lambda () (file-removetree root))))

     (begin (define dir "./readlink_test")
            (when (file-directory? dir) (file-removetree dir))

            (define-file-tree tree
              (dir "d1"
                   (file "f1")
                   (dir "d2"
                        (dir "d3"
                             (symlink "../../f1" "ln1")
                             (symlink "ln1"      "ln2")
                             (symlink "ln2"      "ln3")))))
            (define d1d2d3 (lambda (x) (path-build dir (path-build "d1/d2/d3" x))))

            #t)

     (begin (create-tree dir)
            (file-directory? dir))
     (file-symbolic-link? (d1d2d3 "ln1"))
     (file-symbolic-link? (d1d2d3 "ln2"))
     (file-symbolic-link? (d1d2d3 "ln3"))

     (error? (readlink 123))

     ;; non-rec
     (string=? "../../f1" (readlink (d1d2d3 "ln1")))
     (string=? "ln1" (readlink (d1d2d3 "ln2")))
     (string=? "ln2" (readlink (d1d2d3 "ln3")))

     ;; rec
     (same-file? (path-build dir "d1/f1") (readlink2 (d1d2d3 "ln2") #t))
     (same-file? (path-build dir "d1/f1") (readlink2 (d1d2d3 "ln3") #t))

     (let-values ([(ln pdir) (readlink (d1d2d3 "ln3") #t)])
       (and (string=? ln "../../f1") (string=? pdir (path-build dir "d1/d2/d3"))))

     ;; error case: recursive readlink should detect a direct symlink cycle.
     (error? (let ([cycle (path-build dir "cycle")])
               (file-symlink "cycle" cycle)
               (readlink2 cycle #t)))

     ;; error case: recursive readlink should detect an indirect symlink cycle.
     (error? (let ([a (path-build dir "cycle-a")]
                   [b (path-build dir "cycle-b")])
               (file-symlink "cycle-b" a)
               (file-symlink "cycle-a" b)
               (readlink2 a #t)))

     (file-removetree dir))


(mat file-link

     ;; Error case: overwriting through a parent alias must preserve the source file.
     (let ([root ($random-file)])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (file-symlink "." (path-build root "alias")))
         (lambda ()
           (for-all
            (lambda (operation)
              (let ([source (path-build root "leaf")])
                (write-string! source "original")
                (and (guard (failure [else #t])
                       (operation source (path-build root "alias/leaf"))
                       #f)
                     (file-exists? source)
                     (string=? "original" (read-string source)))))
            (list file-copy! file-link!)))
         (lambda () (file-removetree root))))

     ;; Error case: overwriting a hard link with itself must preserve the source.
     (let ([path ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-string path "link source")
           (and (guard (failure [else #t]) (file-link! path path) #f)
                (string=? "link source" (read-string path))))
         (lambda () (when (file-exists? path) (delete-file path)))))

     (begin (define dir "link_test")
            (define atdir (lambda (x) (path-build dir x)))
            (when (file-directory? dir) (file-removetree dir))

            (define testsrc1 "test src 1")
            (define testsrc2 "test src 2")
            (define lines (map (lambda (x) (random-string)) (iota 10)))

            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text "somestring"))
                   (file "src2.ss" (fasl '(a list of symbols)))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")
                        (file "ranstr" (lines lines))))
              (dir "tests" (mode #o777)
                   (file "test1.ss" (text testsrc1))
                   (file "test2.ss" (text testsrc2))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")))

            #t)

     (begin (create-fstree dir) #t)

     ;; src==dest

     ;; src is regular file
     (= 1 (file-nlinks (atdir "src/src1.ss")))
     (begin (file-link (atdir "src/src1.ss") (atdir "src/src1.cp"))
            #t)
     (file-exists? (atdir "src/src1.cp"))
     (= 2 (file-nlinks (atdir "src/src1.ss")))
     (= 2 (file-nlinks (atdir "src/src1.cp")))

     ;; src is symlink
     (begin (file-link (atdir "tests/src1") (atdir "src1.cp"))
            #t)
     (file-exists? (atdir "src1.cp"))
     (= 3 (file-nlinks (atdir "src/src1.ss")))
     (= 3 (file-nlinks (atdir "src1.cp")))

     ;; following a source symlink into a destination directory should use the link basename.
     (begin (file-link (atdir "tests/src1") (atdir "src/native"))
            (and (file-exists? (atdir "src/native/src1"))
                 (= (file-nlinks (atdir "src/src1.ss"))
                    (file-nlinks (atdir "src/native/src1")))))

     ;; dest is file
     (error? (file-link (atdir "tests/src1") (atdir "src1.cp")))
     (not (error? (file-link! (atdir "tests/src1") (atdir "src1.cp"))))

     ;; dest is dir
     (error? (file-link (atdir "src") (atdir "src1")))
     (error? (file-link (atdir "tests1") (atdir "tests1")))

     ;; dest is symlink to file
     (begin (file-link (atdir "tests/src1") (atdir "tests/src2"))
            (= 5 (file-nlinks (atdir "tests/src2"))))
     (begin (file-link (atdir "tests/src1.ln") (atdir "tests/src3"))
            (= 6 (file-nlinks (atdir "tests/src3"))))

     ;; dest is symlink to file (not follow)
     (begin (file-link (atdir "tests/src1.ln") (atdir "tests/src2.ln") #f)
            (= 2
               (file-nlinks (atdir "tests/src1.ln") #f)
               (file-nlinks (atdir "tests/src2.ln") #f)))
     (string=? (readlink (atdir "tests/src1.ln"))
               (readlink (atdir "tests/src2.ln")))

     ;; dest is symlink to dir
     (error? (file-link (atdir "src/tests") (atdir "tests1")))

     ;; path starting with ~ (test expand_pathname() in libcspp)

     (file-removetree dir))


(mat file-symlink

     (begin (define dir "symlink_test")
            (define atdir (lambda (x) (path-build dir x)))
            (when (file-directory? dir) (file-removetree dir))

            (define lines (map (lambda (x) (random-string)) (iota 10)))
            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text (random-string 50 100)))
                   (file "src2.ss" (fasl (random-list 20 40)))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")
                        (file "ranstr" (lines lines))))
              (dir "tests" (mode #o777)
                   (file "test1.ss" (text (random-string 50 100)))
                   (file "test2.ss" (text (random-string 50 100)))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")
                   (symlink "../src/native" "native")))

            #t)

     (begin (create-fstree dir) #t)

     ;; Symlink content is literal, even when it starts with a tilde or contains UTF-8.
     (let ([tilde (atdir "tilde-link")] [unicode (atdir "unicode-link")])
       (file-symlink "~/literal-target" tilde)
       (file-symlink "目标.txt" unicode)
       (and (string=? "~/literal-target" (readlink tilde))
            (string=? "目标.txt" (readlink unicode))))


     ;; src==dest

     ;; src is regular file
     (begin (file-symlink "src/src1.ss" (atdir "src1"))
            (string=? "src/src1.ss" (readlink (atdir "src1"))))
     (string=? (read-string (atdir "src1")) (read-string (atdir "src/src1.ss")))

     (begin (file-symlink "src1" (atdir "tests/src2"))
            (and (string=? "src1" (readlink (atdir "tests/src2")))
                 (string=? (read-string (atdir "tests/src1")) (read-string (atdir "tests/src2")))))

     ;; dest is file
     (error? (file-symlink "src/src1.ss" (atdir "src1")))
     (error? (file-symlink "src/src1.ss" (atdir "src/src2.ss")))

     (begin (file-symlink! "src1.ss" (atdir "src/src2.ss"))
            (string=? (read-string (atdir "src/src1.ss")) (read-string (atdir "src/src2.ss"))))

     ;; dest is dir
     (begin (file-symlink "tests/test1.ss" dir)
            (and (file-symbolic-link? (atdir "test1.ss"))
                 (string=? (read-string (atdir "test1.ss")) (read-string (atdir "tests/test1.ss")))))

     ;; dest is symlink to file
     (error? (file-symlink "../src/src1.ss" (atdir "tests/test1.ss")))
     (begin (file-symlink! "../src/src1.ss" (atdir "tests/test1.ss"))
            (string=? (read-string (atdir "src/src1.ss")) (read-string (atdir "tests/test1.ss"))))

     ;; dest is symlink to dir
     (begin (file-symlink "../../README.md" (atdir "tests/native"))
            (and (string=? "../../README.md" (readlink (atdir "tests/native/README.md")))
                 (string=? (read-string (atdir "README.md")) (read-string (atdir "tests/native/README.md")))))

     ;; path starting with ~

     (file-removetree dir)
     )


(mat file-copy

     ;; Binary and textual port copies must preserve all input data.
     (let* ([bytes (make-bytevector 131071)]
            [text (make-string 131071 #\x)]
            [text-input (open-input-string text)]
            [text-output (open-output-string)])
       (let loop ([index 0])
         (unless (fx= index (bytevector-length bytes))
           (bytevector-u8-set! bytes index (fxmod index 256))
           (loop (fx1+ index))))
       (let-values ([(output get-output) (open-bytevector-output-port)])
         (let ([input (open-bytevector-input-port bytes)])
           (copy-port input output)
           (close-port input)
           (close-port output)
           (and (bytevector=? bytes (get-output))
                (begin (copy-port text-input text-output)
                       (close-port text-input)
                       (close-port text-output)
                       (string=? text (get-output-string text-output)))))))

     ;; Error case: overwriting a file with itself must preserve its contents.
     (let ([path ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-string path "copy source")
           (and (guard (failure [else #t]) (file-copy! path path) #f)
                (string=? "copy source" (read-string path))))
         (lambda () (when (file-exists? path) (delete-file path)))))

     (begin (define dir "copy_test")
            (define atdir (lambda (x) (path-build dir x)))
            (when (file-directory? dir) (file-removetree dir))

            (define lines (map (lambda (x) (random-string)) (iota 10)))
            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text (random-string 50 100)))
                   (file "src2.ss" (fasl (random-list 20 40)))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")
                        (file "ranstr" (lines lines))))
              (dir "tests" (mode #o777)
                   (file "big" (fasl (random-list 99999 999999)))
                   (file "test1.ss" (text (random-string 50 100)))
                   (file "test2.ss" (text (random-string 50 100)))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")
                   (symlink "../src/native" "native")))

            #t)

     (begin (create-fstree dir) #t)

     ;; src==dest

     ;; src is regular file
     (begin (file-copy (atdir "README.md") (atdir "src/md"))
            (and (file-exists? (atdir "src/md"))
                 (string=? (read-string (atdir "README.md")) (read-string (atdir "src/md")))))
     (begin (file-copy (atdir "src/src2.ss") (atdir "tests/src2.ss"))
            (and (file-exists? (atdir "tests/src2.ss"))
                 (equal? (read-datum-fasl (atdir "src/src2.ss")) (read-datum-fasl (atdir "tests/src2.ss")))))

     ;; src is special file
     (error? (file-copy "/dev/tty" (atdir "tty")))

     ;; src is symlink to file
     (begin (file-copy (atdir "tests/src1") (atdir "src1"))
            (string=? (read-string (atdir "src1")) (read-string (atdir "tests/src1"))))
     (begin (file-copy (atdir "tests/src1.ln") (atdir "src11"))
            (string=? (read-string (atdir "src11")) (read-string (atdir "tests/src1.ln"))))

     ;; following a source symlink into a destination directory should use the link basename.
     (begin (file-copy (atdir "tests/src1") (atdir "src/native"))
            (and (file-regular? (atdir "src/native/src1"))
                 (string=? (read-string (atdir "tests/src1"))
                           (read-string (atdir "src/native/src1")))))

     ;; src is symlink to file, no follow
     (begin (file-copy (atdir "tests/src1") (atdir "src1.ln") #f)
            (and (file-symbolic-link? (atdir "src1.ln"))
                 (string=? "../src/src1.ss" (readlink (atdir "src1.ln")))))

     ;; dest is file
     (error? (file-copy (atdir "README.md") (atdir "tests/test2.ss")))
     (error? (file-copy (atdir "tests/test2.ss") (atdir "README.md")))

     (not (error? (file-copy! (atdir "README.md") (atdir "tests/test2.ss"))))
     (string=? (read-string (atdir "README.md")) (read-string (atdir "tests/test2.ss")))


     ;; dest is dir
     (begin (file-copy (atdir "README.md") (atdir "src/native"))
            (file-exists? (atdir "src/native/README.md")))
     (string=? (read-string (atdir "README.md")) (read-string (atdir "src/native/README.md")))

     ;; dest is symlink to file
     (error? (file-copy (atdir "README.md") (atdir "tests/src1")))
     (begin (file-copy! (atdir "README.md") (atdir "tests/src1"))
            (file-regular? (atdir "tests/src1")))
     (string=? (read-string (atdir "README.md")) (read-string (atdir "tests/src1")))

     ;; dest is symlink to dir
     (begin (file-copy (atdir "README.md") (atdir "src/tests"))
            (and (file-exists? (atdir "src/tests/README.md"))
                 (file-exists? (atdir "tests/README.md"))))
     (string=? (read-string (atdir "README.md")) (read-string (atdir "tests/README.md")))

     ;; big file
     (begin (file-copy (atdir "tests/big") (atdir "src"))
            (file-regular? (atdir "src/big")))
     (equal? (read-datum-fasl (atdir "src/big")) (read-datum-fasl (atdir "tests/big")))

     ;; path starting with ~

     (file-removetree dir)
     )


(mat file-copymode

     (begin (define dir "copymode_test")
            (define atdir (lambda (x) (path-build dir x)))
            (when (file-directory? dir) (file-removetree dir))

            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text (random-string 50 100)))
                   (file "src2.ss" (mode '(r w x) '(w x) '(r x)) (fasl (random-list 20 40)))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")))
              (dir "tests" (mode #o777)
                   (file "test1.ss" (text (random-string 50 100)))
                   (file "test2.ss" (text (random-string 50 100)))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")
                   (symlink "../src/native" "native")))

            #t)

     (not (file-directory? dir))
     (begin (create-fstree dir) #t)

     (= #o777 (get-mode (atdir "src/src1.ss")))
     (begin (file-copymode (atdir "src/src2.ss") (atdir "src/src1.ss"))
            (= (symbols->file-mode '() '(r w x) '(w x) '(r x)) (get-mode (atdir "src/src1.ss"))))

     (= #o777 (get-mode (atdir "tests")))
     (begin (file-copymode (atdir "src") (atdir "tests"))
            (= (get-mode (atdir "src")) (get-mode (atdir "tests"))))

     (not (= #o777 (get-mode (atdir "tests/native") #t)))
     (begin (file-copymode (atdir "src/src2.ss") (atdir "tests/native"))
            (= (get-mode (atdir "src/src2.ss")) (get-mode (atdir "src/native"))))

     (file-removetree dir))


(mat file-copymeta

     (begin (define dir "copymeta_test")
            (define atdir (lambda (x) (path-build dir x)))
            (when (file-directory? dir) (file-removetree dir))

            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text (random-string 50 100)))
                   (file "src2.ss" (mode '(r w x) '(w x) '(r x)) (fasl (random-list 20 40)))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")))
              (dir "tests" (mode #o777)
                   (file "test1.ss" (text (random-string 50 100)))
                   (file "test2.ss" (text (random-string 50 100)))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")
                   (symlink "../src/native" "native")))

            #t)

     (not (file-directory? dir))
     (begin (create-fstree dir) #t)

     ;; times of files above may all be the same...
     (let* ([5s (make-time 'time-utc 0 5)]
            [newt (time-difference (file-access-time (atdir "src/src1.ss")) 5s)])
       (set-time-type! newt 'time-utc)
       (file-touch-atime (atdir "tests/test1.ss") newt)
       (file-touch-mtime (atdir "tests/test1.ss") newt)
       #t)

     ;; compare atime, mtime and mode
     (let ([mode1  (get-mode (atdir "src/src1.ss"))]
           [atime1 (file-access-time (atdir "src/src1.ss"))]
           [mtime1 (file-modification-time (atdir "src/src1.ss"))]
           [mode2  (get-mode (atdir "tests/test1.ss"))]
           [atime2 (file-access-time (atdir "tests/test1.ss"))]
           [mtime2 (file-modification-time (atdir "tests/test1.ss"))])
       (and (not (= mode1 mode2))
            (not (time=? atime1 atime2))
            (not (time=? mtime1 mtime2))))
     (begin (file-copymeta (atdir "src/src1.ss") (atdir "tests/test1.ss"))
            (let ([mode1  (get-mode (atdir "src/src1.ss"))]
                  [atime1 (file-access-time (atdir "src/src1.ss"))]
                  [mtime1 (file-modification-time (atdir "src/src1.ss"))]
                  [mode2  (get-mode (atdir "tests/test1.ss"))]
                  [atime2 (file-access-time (atdir "tests/test1.ss"))]
                  [mtime2 (file-modification-time (atdir "tests/test1.ss"))])
              (and (= mode1 mode2)
                   (time=? atime1 atime2)
                   (time=? mtime1 mtime2))))

     ;; TODO src/dest are symlinks

     (file-removetree dir)
     )


(mat file-copytree

     ;; A basename and a basename with a directory marker both have the current parent.
     (let ([src ($random-file)] [dest ($random-file)])
       (dynamic-wind
         (lambda () (mkdir src) (write-string (path-build src "leaf") "leaf"))
         (lambda ()
           (for-all
            (lambda (suffix)
              (file-copytree src (string-append dest suffix))
              (let ([copied? (string=? "leaf" (read-string (path-build dest "leaf")))])
                (file-removetree dest #t)
                copied?))
            '("" "/")))
         (lambda ()
           (file-removetree src #t)
           (when (file-exists? dest #f) (file-removetree dest #t)))))

     ;; Error case: aliases into any source descendant must be rejected before mutation.
     (let ([root ($random-file)])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir (path-build root "source"))
           (mkdir (path-build root "source/sub"))
           (write-string (path-build root "source/sub/leaf") "leaf")
           (file-symlink "source/sub" (path-build root "alias")))
         (lambda ()
           (for-all
            (lambda (suffix)
              (let ([destination (path-build root suffix)] [calls 0])
                (and (guard (failure [else #t])
                       (file-copytree (path-build root "source") destination
                                      (lambda (from to)
                                        (set! calls (fx1+ calls))
                                        (when (fx> calls 5)
                                          (error 'file-copytree "recursive copy"))
                                        (file-copy from to)))
                       #f)
                     (fxzero? calls)
                     (not (file-exists? destination #f)))))
            '("alias/nested" "alias/../nested")))
         (lambda () (file-removetree root))))

     ;; A destination that leaves the source through a parent component is allowed.
     (let ([root ($random-file)])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir (path-build root "source"))
           (write-string (path-build root "source/leaf") "leaf"))
         (lambda ()
           (file-copytree (path-build root "source")
                          (path-build root "source/../outside"))
           (string=? "leaf" (read-string (path-build root "outside/leaf"))))
         (lambda () (when (file-directory? root #f) (file-removetree root)))))

     ;; Error case: a symlinked destination alias must not copy into the source.
     (let ([root ($random-file)] [calls 0])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir (path-build root "source"))
           (write-string (path-build root "source/leaf") "leaf")
           (file-symlink "source" (path-build root "alias")))
         (lambda ()
           (let ([nested (path-build root "alias/nested")])
             (and (guard (failure [else #t])
                    (file-copytree (path-build root "source") nested
                                   (lambda (from to)
                                     (set! calls (fx1+ calls))
                                     (when (fx> calls 5)
                                       (error 'file-copytree "recursive copy"))
                                     (file-copy from to)))
                    #f)
                  (fxzero? calls)
                  (not (file-exists? nested #f)))))
         (lambda () (when (file-directory? root #f) (file-removetree root)))))

     ;; A trailing separator on the source path must not prevent relative path calculation.
     (let ([root ($random-file)])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (mkdir (path-build root "source"))
           (write-string (path-build root "source/file.txt") "contents"))
         (lambda ()
           (let ([result
                  (capture-process "../cspp" "--script=file-copytree-process.ss"
                                   (string-append (path-build root "source") "/")
                                   (path-build root "destination")
                                   :timeout 5000 :stdout capture :stderr capture)])
             (and (process-exit-success? (process-result-status result))
                  (string=? "" (process-result-stdout result))
                  (string=? "" (process-result-stderr result))
                  (string=? "contents"
                            (read-string (path-build root "destination/file.txt"))))))
         (lambda () (when (file-directory? root #f) (file-removetree root)))))

     ;; Error case: a destination inside the source must be rejected before copying starts.
     (let ([root ($random-file)] [calls 0])
       (dynamic-wind
         (lambda ()
           (mkdir root)
           (write-string (path-build root "source.txt") "source"))
         (lambda ()
           (and (guard (failure [else #t])
                  (file-copytree root (path-build root "nested")
                                 (lambda (src dest)
                                   (set! calls (fx1+ calls))
                                   (file-copy src dest)))
                  #f)
                (fxzero? calls)))
         (lambda () (when (file-directory? root #f) (file-removetree root)))))

     (begin (define dir "copy_test")
            (define dir1 "./copy_test1")

            (define atdir  (lambda (x) (path-build dir  x)))
            (define atdir1 (lambda (x) (path-build dir1 x)))

            (when (file-directory? dir) (file-removetree dir))
            (when (file-directory? dir1) (file-removetree dir1))

            (define lines (map (lambda (x) (random-string)) (iota 10)))
            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text (random-string 50 100)))
                   (file "src2.ss" (fasl (random-list 20 40)))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")
                        (file "ranstr" (lines lines))))
              (dir "tests" (mode #o777)
                   (file "big" (fasl (random-list 99999 999999)))
                   (file "test1.ss" (text (random-string 50 100)))
                   (file "test2.ss" (text (random-string 50 100)))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")
                   (symlink "../src/native" "native")))

            #t)

     (begin (create-fstree dir) #t)

     (begin (file-copytree dir dir1)
            #t)

     (same-file-contents? (atdir "README.md") (atdir1 "README.md"))
     (same-file-contents? (atdir "README.md") (atdir1 "README.md"))
     (same-file-contents? (atdir "src/src1.ss") (atdir1 "src/src1.ss"))
     (same-file-contents? (atdir "src/src2.ss") (atdir1 "src/src2.ss"))
     (same-file-contents? (atdir "src/native/ranstr") (atdir1 "src/native/ranstr"))
     (same-file-contents? (atdir "tests/big") (atdir1 "tests/big"))
     (same-file-contents? (atdir "tests/test1.ss") (atdir1 "tests/test1.ss"))
     (same-file-contents? (atdir "tests/test2.ss") (atdir1 "tests/test2.ss"))
     (same-file-contents? (atdir "tests/src1") (atdir1 "tests/src1"))
     (same-file-contents? (atdir "tests/src1.ln") (atdir1 "tests/src1.ln"))
     (same-file-contents? (atdir "tests/native/runtime.c") (atdir1 "tests/native/runtime.c"))

     (file-symbolic-link? (atdir1 "src/tests"))
     (file-symbolic-link? (atdir1 "tests/src1.ln"))
     (file-symbolic-link? (atdir1 "tests/native"))
     (string=? "../src/native" (readlink (atdir1 "tests/native")))

     ;; TODO another copy function, dest exists

     (file-removetree dir)
     (file-removetree dir1))


(mat file-move

     ;; A dangling symlink is a movable filesystem entry even though its target is absent.
     (let ([src ($random-file)] [dest ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (file-symlink "missing-target" src)
           (and (file-move src dest)
                (not (file-exists? src #f))
                (file-symbolic-link? dest)
                (string=? "missing-target" (readlink dest))))
         (lambda ()
           (when (file-exists? src #f) (delete-file src))
           (when (file-exists? dest #f) (delete-file dest)))))

     (begin (define dir "move_test")
            (define dir1 "./move_test1")

            (define atdir  (lambda (x) (path-build dir  x)))
            (define atdir1 (lambda (x) (path-build dir1 x)))

            (when (file-directory? dir) (file-removetree dir))
            (when (file-directory? dir1) (file-removetree dir1))

            (define big (random-list 99999 999999))
            (define lines (map (lambda (x) (random-string)) (iota 10)))
            (define str (random-string 50 100))
            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text (random-string 50 100)))
                   (file "src2.ss" (text str))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")
                        (file "ranstr" (lines lines))))
              (dir "tests" (mode #o777)
                   (file "big" (fasl big))
                   (file "test1.ss" (text (random-string 50 100)))
                   (file "test2.ss" (text "str"))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")
                   (symlink "../src/native" "native")))

            #t)

     (begin (create-fstree dir) #t)

     ;; move files
     (begin (file-move (atdir "README.md") (atdir "src"))
            (string=? "This is a fancy project." (read-string (atdir "src/README.md"))))
     (not (file-exists? (atdir "README.md")))

     (begin (file-move (atdir "tests/big") (atdir "src/big1"))
            (equal? big (read-datum-fasl (atdir "src/big1"))))
     (not (file-exists? (atdir "tests/big")))

     (error? (file-move (atdir "src/src2.ss") (atdir "tests/test2.ss")))
     ;; overwrite
     (not (error? (file-move (atdir "src/src2.ss") (atdir "tests/test2.ss") file-copy!)))
     (file-exists? (atdir "tests/test2.ss"))
     (not (file-exists? (atdir "src/src2.ss")))
     (equal? str (read-string (atdir "tests/test2.ss")))

     ;; move tree
     (begin (file-move dir dir1)
            (not (file-directory? dir)))
     (file-directory? (atdir1 "src"))
     (file-directory? (atdir1 "tests"))
     (file-directory? (atdir1 "src/native"))

     (file-exists? (atdir1 "tests/test2.ss"))
     (equal? str (read-string (atdir1 "tests/test2.ss")))
     (equal? big (read-datum-fasl (atdir1 "src/big1")))

     ;; error case: moving a symlink to a directory must move the link, not remove the target tree.
     (let ([root "move_symlink_root"]
           [target "move_symlink_target"]
           [dest "move_symlink_dest"])
       (dynamic-wind
         (lambda ()
           (when (file-exists? root #f) (file-removetree root #f))
           (when (file-exists? target #f) (file-removetree target #f))
           (when (file-exists? dest #f)
             (if (file-directory? dest #f)
                 (file-removetree dest #f)
                 (delete-file dest #f))))
         (lambda ()
           (mkdir root)
           (mkdir target)
           (write-string (path-build target "keep.txt") "keep")
           (file-symlink (path-build ".." target) (path-build root "target-link"))
           (file-move (path-build root "target-link") dest)
           (and (file-symbolic-link? dest)
                (file-directory? target)
                (string=? "keep" (read-string (path-build target "keep.txt")))))
         (lambda ()
           (when (file-exists? root #f) (file-removetree root #f))
           (when (file-exists? target #f) (file-removetree target #f))
           (when (file-exists? dest #f)
             (if (file-directory? dest #f)
                 (file-removetree dest #f)
                 (delete-file dest #f))))))

     (file-removetree dir1)
     )


(mat file-removetree-symlink-safety

     (begin (define dir "removetree_symlink_test")
            (define target "removetree_symlink_target")
            (define atdir (lambda (x) (path-build dir x)))
            (define attarget (lambda (x) (path-build target x)))
            (when (file-exists? dir #f) (file-removetree dir #f))
            (when (file-exists? target #f) (file-removetree target #f))
            (mkdir dir)
            (mkdir target)
            (write-string (attarget "keep.txt") "keep")
            (file-symlink (path-build ".." target) (atdir "target-link"))
            #t)

     ;; error case: a symlink to a directory must not be accepted as a tree root.
     (error? (file-removetree (atdir "target-link") #t))

     ;; Error case: trailing separators must not allow a symlink root to be followed.
     (error? (file-removetree (atdir "target-link/") #t))

     (begin (file-removetree (atdir "target-link///") #f)
            (and (file-symbolic-link? (atdir "target-link"))
                 (file-exists? (attarget "keep.txt"))))

     ;; the target directory and file must survive the failed removal.
     (and (file-directory? target)
          (string=? "keep" (read-string (attarget "keep.txt"))))

     (begin (file-removetree dir #t)
            (file-removetree target #t)
            #t))


(define $with-fswatcher-fixture
  (case-lambda
    [(proc) ($with-fswatcher-fixture #f proc)]
    [(blocking? proc)
     ($with-fs-fixture
      (lambda (root)
        (let ([watcher (make-fswatcher blocking?)])
          (dynamic-wind
            void
            (lambda () (proc root watcher))
            (lambda () (close-fswatcher watcher))))))]))

(define $fswatcher-drain
  (lambda (watcher)
    (let ([out (make-list-builder)])
      (let loop ([remaining 512])
        (let ([event (fswatcher-next! watcher)])
          (cond [(not event) (out)]
                [(zero? remaining) #f]
                [else (out event) (loop (fx1- remaining))]))))))

(define $fswatcher-event?
  (lambda (event path mask name)
    (and (vector? event)
         (= 3 (vector-length event))
         (equal? path (vector-ref event 0))
         (flag-all-set? mask (vector-ref event 1))
         (equal? name (vector-ref event 2)))))

(mat fswatcher-empty
     ($with-fswatcher-fixture
      (lambda (root watcher) (not (fswatcher-next! watcher)))))

(mat fswatcher-batch
     ($with-fswatcher-fixture
      (lambda (root watcher)
        (fswatcher-add! watcher root (fswatcher-mask FSW_create))
        (let ([names (map (lambda (index) (format "batch~a" index)) (iota 30))])
          (for-each (lambda (name) (file-touch (path-build root name))) names)
          (let ([events ($fswatcher-drain watcher)])
            (and events
                 (= (length events) (length names))
                 (equal? names (map (lambda (event) (vector-ref event 2)) events))
                 (andmap (lambda (event)
                           ($fswatcher-event? event root (fswatcher-mask FSW_create)
                                              (vector-ref event 2)))
                         events)))))))

(mat fswatcher-multiple-batches
     ($with-fswatcher-fixture
      (lambda (root watcher)
        (fswatcher-add! watcher root (fswatcher-mask FSW_create))
        ;; These distinct names produce over 64 KiB of events before any read.
        (let ([names (map (lambda (index)
                            (string-append (make-string 250 #\x) (format "~a" index)))
                          (iota 260))])
          (for-each (lambda (name) (file-touch (path-build root name))) names)
          (let ([events ($fswatcher-drain watcher)])
            (and events
                 (equal? names (map (lambda (event) (vector-ref event 2)) events))))))))

(mat fswatcher-directory-names
     ($with-fswatcher-fixture
      (lambda (root watcher)
        (fswatcher-add! watcher root (fswatcher-mask FSW_create FSW_delete))
        (let ([name (string (integer->char 233) (integer->char #x4e2d) #\. #\t)]
              [long-name (make-string 255 #\x)])
          (file-touch (path-build root name))
          (and ($fswatcher-event? (fswatcher-next! watcher) root
                                 (fswatcher-mask FSW_create) name)
               (begin (file-touch (path-build root long-name)) #t)
               ($fswatcher-event? (fswatcher-next! watcher) root
                                 (fswatcher-mask FSW_create) long-name)
               (begin (mkdir (path-build root "child")) #t)
               ($fswatcher-event? (fswatcher-next! watcher) root
                                 (fswatcher-mask FSW_create FSW_isdir) "child")
               (begin (delete-file (path-build root name)) #t)
               ($fswatcher-event? (fswatcher-next! watcher) root
                                 (fswatcher-mask FSW_delete) name))))))

(mat fswatcher-default-masks
     ($with-fswatcher-fixture
      (lambda (root watcher)
        (let ([path (path-build root "top.txt")])
          (fswatcher-add! watcher path)
          (write-datum>> path 'change)
          (delete-file path)
          (let ([events ($fswatcher-drain watcher)])
            (and events
                 (andmap (lambda (event) (equal? path (vector-ref event 0))) events)
                 (andmap (lambda (mask)
                           (ormap (lambda (event) (flag-all-set? mask (vector-ref event 1)))
                                  events))
                         (list (fswatcher-mask FSW_open) (fswatcher-mask FSW_modify)
                               (fswatcher-mask FSW_close_write)
                               (fswatcher-mask FSW_delete_self)
                               (fswatcher-mask FSW_ignored))))))))

     ($with-fswatcher-fixture
      (lambda (root watcher)
        (fswatcher-add! watcher root)
        (file-touch (path-build root "child.txt"))
        (file-touch (path-build root "sub/deeper.txt"))
        (let ([events ($fswatcher-drain watcher)])
          (and events
               (ormap (lambda (event)
                        ($fswatcher-event? event root (fswatcher-mask FSW_create) "child.txt"))
                      events)
               ;; Watching a directory must not recurse into its existing subdirectories.
               (not (ormap (lambda (event) (equal? "deeper.txt" (vector-ref event 2)))
                           events)))))))

(mat fswatcher-files-and-aliases

     ($with-fswatcher-fixture
      (lambda (root watcher)
        (let ([input (string-copy root)])
          (fswatcher-add! watcher input (fswatcher-mask FSW_create))
          (string-set! input 0 #\X)
          (file-touch (path-build root "created"))
          (let ([event (fswatcher-next! watcher 1000)])
            (and event (string=? root (vector-ref event 0)))))))

     ($with-fswatcher-fixture
      (lambda (root watcher)
        (fswatcher-add! watcher (string-copy root) (fswatcher-mask FSW_create))
        (file-touch (path-build root "created"))
        (let ([event (fswatcher-next! watcher 1000)])
          (and event
               (begin
                 (string-set! (vector-ref event 0) 0 #\X)
                 (fswatcher-remove! watcher root)
                 #t)))))

     ($with-fswatcher-fixture
      (lambda (root watcher)
        (let* ([first (path-build root "top.txt")]
               [second (path-build root "sub/nested.txt")]
               [alias (path-build root "alias")])
          (file-link first alias)
          (fswatcher-add! watcher first (fswatcher-mask FSW_modify))
          (fswatcher-add! watcher alias (fswatcher-mask FSW_modify))
          (fswatcher-add! watcher second (fswatcher-mask FSW_modify))
          (write-datum>> alias 'first)
          (write-datum>> second 'second)
          (let ([events ($fswatcher-drain watcher)])
            (and events (= 2 (length events))
                 ($fswatcher-event? (car events) first (fswatcher-mask FSW_modify) #f)
                 ($fswatcher-event? (cadr events) second (fswatcher-mask FSW_modify) #f)))))))

(mat fswatcher-remove-queued
     ($with-fswatcher-fixture
      (lambda (root watcher)
        (let ([path (path-build root "top.txt")])
          (fswatcher-add! watcher path (fswatcher-mask FSW_modify))
          (write-datum>> path 'before-removal)
          (fswatcher-remove! watcher path)
          (write-datum>> path 'after-removal)
          (let ([events ($fswatcher-drain watcher)])
            (and events (= 2 (length events))
                 ($fswatcher-event? (car events) path (fswatcher-mask FSW_modify) #f)
                 ($fswatcher-event? (cadr events) path (fswatcher-mask FSW_ignored) #f)))))))

(mat fswatcher-remove-pending
     ($with-fswatcher-fixture
      (lambda (root watcher)
        (fswatcher-add! watcher root (fswatcher-mask FSW_create))
        (file-touch (path-build root "first"))
        (file-touch (path-build root "second"))
        (and ($fswatcher-event? (fswatcher-next! watcher) root
                               (fswatcher-mask FSW_create) "first")
             (begin (fswatcher-remove! watcher root) #t)
             ;; Error case: explicit removal cannot be repeated before ignored is consumed.
             (guard (failure [(fswatcher-error? failure) #t] [else #f])
               (fswatcher-remove! watcher root) #f)
             (begin (file-touch (path-build root "unwatched")) #t)
             ($fswatcher-event? (fswatcher-next! watcher) root
                                (fswatcher-mask FSW_create) "second")
             ($fswatcher-event? (fswatcher-next! watcher) root
                                (fswatcher-mask FSW_ignored) #f)
             (not (fswatcher-next! watcher))))))

(mat fswatcher-automatic-removal
     ($with-fswatcher-fixture
      (lambda (root watcher)
        (let ([path (path-build root "top.txt")])
          (fswatcher-add! watcher path (fswatcher-mask FSW_delete_self))
          (delete-file path)
          ;; Removal must accept a watch already removed by the kernel, before ignored is read.
          (fswatcher-remove! watcher path)
          (let ([events ($fswatcher-drain watcher)])
            (and events (= 2 (length events))
                 ($fswatcher-event? (car events) path (fswatcher-mask FSW_delete_self) #f)
                 ($fswatcher-event? (cadr events) path (fswatcher-mask FSW_ignored) #f)
                 ;; Error case: ignored retires the path, so a later remove must fail.
                 (guard (failure [(fswatcher-error? failure) #t] [else #f])
                   (fswatcher-remove! watcher path)
                   #f))))))

     ($with-fswatcher-fixture
      (lambda (root watcher)
        (let ([path (path-build root "top.txt")])
          (fswatcher-add! watcher path (fswatcher-mask FSW_modify FSW_oneshot))
          (write-datum>> path 'once)
          (let ([events ($fswatcher-drain watcher)])
            (and events (= 2 (length events))
                 ($fswatcher-event? (car events) path (fswatcher-mask FSW_modify) #f)
                 ($fswatcher-event? (cadr events) path (fswatcher-mask FSW_ignored) #f)
                 (begin (fswatcher-add! watcher path (fswatcher-mask FSW_modify)) #t)
                 (begin (write-datum>> path 'again) #t)
                 ($fswatcher-event? (fswatcher-next! watcher) path
                                    (fswatcher-mask FSW_modify) #f)))))))

(mat fswatcher-dangling-link
     ($with-fswatcher-fixture
      (lambda (root watcher)
        (let ([path (path-build root "dangling")])
          (file-symlink "missing" path)
          (fswatcher-add! watcher path (fswatcher-mask FSW_dont_follow FSW_delete_self))
          (delete-file path)
          (let ([events ($fswatcher-drain watcher)])
            (and events (= 2 (length events))
                 ($fswatcher-event? (car events) path (fswatcher-mask FSW_delete_self) #f)
                 ($fswatcher-event? (cadr events) path (fswatcher-mask FSW_ignored) #f)))))))

(mat fswatcher-timeout
     ($with-fswatcher-fixture
      (lambda (root watcher)
        (let* ([start (current-time 'time-monotonic)]
               [event (fswatcher-next! watcher 40)]
               [elapsed (time-difference (current-time 'time-monotonic) start)])
          (and (not event)
               (>= (+ (* (time-second elapsed) 1000000000) (time-nanosecond elapsed))
                   30000000)))))

     ;; Explicit zero timeout must also return immediately on a blocking watcher.
     (guard (failure [else #f])
       ($with-fs-fixture
        (lambda (root)
          (let ([result (process-capture "../cspp" "--script=fswatcher-blocking-process.ss"
                         (string-append root) "zero-timeout" :timeout 30000 :stdout capture :stderr capture)])
            (and (process-exit-success? (process-result-status result))
                 (string=? "" (process-result-stdout result))
                 (string=? "" (process-result-stderr result)))))))

     ($with-fswatcher-fixture
      (lambda (root watcher)
        (fswatcher-add! watcher root (fswatcher-mask FSW_create))
        (file-touch (path-build root "ready"))
        ($fswatcher-event? (fswatcher-next! watcher 1000) root
                           (fswatcher-mask FSW_create) "ready"))))

(mat fswatcher-errors
     ;; Error case: unknown paths cannot be removed.
     ($with-fswatcher-fixture
      (lambda (root watcher)
        (guard (failure [(fswatcher-error? failure) #t] [else #f])
          (fswatcher-remove! watcher "unknown")
          #f)))

     ;; Error cases: invalid masks, timeouts, and missing paths must be rejected.
     ($with-fswatcher-fixture
      (lambda (root watcher)
        (and (guard (failure [else #t]) (fswatcher-add! watcher root -1) #f)
             (guard (failure [else #t]) (fswatcher-add! watcher root #x100000000) #f)
             (guard (failure [else #t]) (fswatcher-next! watcher -1) #f)
             (guard (failure [else #t]) (fswatcher-next! watcher #x80000000) #f)
             (guard (failure [else #t])
               (fswatcher-add! watcher (path-build root "missing")) #f))))

     ;; Error cases: closed watchers reject add, remove, next, and a second close.
     (let ([watcher (make-fswatcher #f)])
       (close-fswatcher watcher)
       (and (guard (failure [else #t]) (fswatcher-add! watcher ".") #f)
            (guard (failure [else #t]) (fswatcher-remove! watcher ".") #f)
            (guard (failure [else #t]) (fswatcher-next! watcher) #f)
            (guard (failure [else #t]) (close-fswatcher watcher) #f))))

(mat fswatcher-blocking-timeout
     ;; A child deadline catches accidental infinite waits for positive blocking timeouts.
     (guard (failure [else #f])
       ($with-fs-fixture
        (lambda (root)
          (let ([result (process-capture "../cspp" "--script=fswatcher-blocking-process.ss"
                         (string-append root) "finite-timeout"
                         :timeout 30000 :stdout capture :stderr capture)])
            (and (process-exit-success? (process-result-status result))
                 (string=? "" (process-result-stdout result))
                 (string=? "" (process-result-stderr result))))))))

(mat fswatcher-blocking-gc
     ;; Run the blocking/GC regression in a child so a deadlock cannot hang the suite.
     (guard (failure [else #f])
       ($with-fs-fixture
        (lambda (root)
          (let ([result (process-capture "../cspp" "--script=fswatcher-blocking-process.ss"
                         (string-append root) :timeout 30000 :stdout capture :stderr capture)])
            (and (process-exit-success? (process-result-status result))
                 (string=? "" (process-result-stdout result))
                 (string=? "" (process-result-stderr result))))))))


(mat file-stat-snapshot
     (let ([path ($random-file)])
       (dynamic-wind
         (lambda () (write-string path "abc"))
         (lambda ()
           (let ([stat (file-stat path)])
             (write-string! path "longer")
             (delete-file path)
             (and (= (file-stat-size stat) 3)
                  (not (file-stat? path)))))
         (lambda () (when (file-exists? path) (delete-file path))))))
