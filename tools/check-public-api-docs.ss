(import (cspp)
        (cspp path))

(define string-prefix?
  (lambda (prefix text)
    (let ([prefix-length (string-length prefix)])
      (and (<= prefix-length (string-length text))
           (string=? prefix (substring text 0 prefix-length))))))

(define string-contains
  (lambda (text fragment)
    (let ([text-length (string-length text)] [fragment-length (string-length fragment)])
      (let loop ([index 0])
        (cond
         [(> (+ index fragment-length) text-length) #f]
         [(string=? fragment (substring text index (+ index fragment-length))) index]
         [else (loop (+ index 1))])))))

(define string-contains-from
  (lambda (text fragment start)
    (let ([text-length (string-length text)] [fragment-length (string-length fragment)])
      (let loop ([index start])
        (cond
         [(> (+ index fragment-length) text-length) #f]
         [(string=? fragment (substring text index (+ index fragment-length))) index]
         [else (loop (+ index 1))])))))

(define read-file-string
  (lambda (path)
    (call-with-port
     (open-file-input-port path)
     (lambda (port) (utf8->string (get-bytevector-all port))))))

(define scheme-file?
  (lambda (path)
    (and (> (string-length path) 3)
         (string=? ".ss" (substring path (- (string-length path) 3)
                                    (string-length path))))))

(define collect-scheme-files
  (lambda (path)
    (cond
     [(file-directory? path)
      (apply append
             (map (lambda (name)
                    (collect-scheme-files (string-append path "/" name)))
                  (filter (lambda (name) (not (member name '("." ".."))))
                          (directory-list path))))]
     [(and (file-regular? path) (scheme-file? path)) (list path)]
     [else '()])))

(define implementation-library-path?
  (lambda (path)
    (or (string-contains path "/ffi.ss")
        (string-contains path "/private.ss")
        (string-contains path "/private/")
        (exists (lambda (suffix) (string-contains path suffix))
                '("/cspp/logging/common.ss"
                  "/cspp/crypto/common.ss"
                  "/cspp/test/capture.ss"
                  "/cspp/test/common.ss"
                  "/cspp/test/descriptor.ss"
                  "/cspp/navigator/core.ss"
                  "/cspp/navigator/data.ss"
                  "/cspp/navigator/engine.ss"
                  "/cspp/rich/cell-data.ss"
                  "/cspp/rich/cells.ss"
                  "/cspp/rich/common.ss"
                  "/cspp/rich/lines.ss"
                  "/cspp/rich/render.ss")))))

(define exported-name*
  (lambda (export-form)
    (apply append
           (map (lambda (item)
                  (cond
                   [(symbol? item) (list item)]
                   [(and (pair? item) (eq? (car item) 'rename))
                    (map cadr (cdr item))]
                   [else '()]))
                (cdr export-form)))))

(define record-public-name*
  (lambda (form)
    (let ([header (cadr form)] [answer '()])
      (cond
       [(symbol? header) (set! answer (list header))]
       [(pair? header) (set! answer (filter symbol? header))])
      (for-each
       (lambda (clause)
         (when (and (pair? clause) (eq? (car clause) 'fields))
           (for-each
            (lambda (field)
              (when (and (pair? field) (>= (length field) 3))
                (set! answer (cons (caddr field) answer))
                (when (and (eq? (car field) 'mutable) (>= (length field) 4))
                  (set! answer (cons (cadddr field) answer)))))
            (cdr clause))))
       (cddr form))
      answer)))

(define definition-info*
  (lambda (body)
    (let ([answer '()])
      (for-each
       (lambda (form)
         (when (pair? form)
           (case (car form)
             [(define-who)
              (let ([name (cadr form)])
                (set! answer
                      (cons (cons (if (pair? name) (car name) name) 'proc) answer)))]
             [(define)
              (let ([name (cadr form)] [value (and (pair? (cddr form)) (caddr form))])
                (when (or (pair? name)
                          (and (pair? value) (memq (car value) '(lambda case-lambda))))
                  (set! answer
                        (cons (cons (if (pair? name) (car name) name) 'proc) answer))))]
             [(define-syntax)
              (set! answer (cons (cons (cadr form) 'macro) answer))]
             [(define-record-type)
              (let* ([header (cadr form)]
                     [type (if (pair? header) (car header) header)])
                (for-each
                 (lambda (name) (set! answer (cons (list name 'record type) answer)))
                 (record-public-name* form)))])))
       body)
      answer)))

(define doc-adjacent?
  (lambda (source kind name definition-prefix*)
    (let* ([tag (string-append "#|" (symbol->string kind) ":"
                               (symbol->string name) "\n")]
           [tag-index (string-contains source tag)])
      (and tag-index
           (let ([end-index (string-contains-from source "|#" tag-index)])
             (and end-index
                  (let skip-whitespace ([index (+ end-index 2)])
                    (cond
                     [(= index (string-length source)) #f]
                     [(char-whitespace? (string-ref source index))
                      (skip-whitespace (+ index 1))]
                     [else
                      (exists (lambda (prefix)
                                (string-prefix?
                                 prefix (substring source index (string-length source))))
                              definition-prefix*)]))))))))

(define check-doc-blocks
  (lambda (path source report)
    (let ([line* (string-split source #\newline)])
      (let loop ([remaining line*] [line-number 1] [in-doc? #f])
        (unless (null? remaining)
          (let* ([line (car remaining)]
                 [starts? (and (string-contains line "#|")
                               (or (string-contains line "#|proc:")
                                   (string-contains line "#|macro:")
                                   (string-contains line "#|record:")))]
                 [active? (or in-doc? starts?)]
                 [ends? (and active? (string-contains line "|#"))])
            (when (and active? (> (string-length line) 100))
              (report path line-number "documentation line exceeds 100 characters"))
            (when active?
              (for-each
               (lambda (phrase)
                 (when (string-contains line phrase)
                   (report path line-number
                           (string-append "vague return phrase: " phrase))))
               '("returns a value" "returns the result" "returns unspecified")))
            (loop (cdr remaining) (+ line-number 1) (and active? (not ends?)))))))))

(define check-library
  (lambda (path report)
    (unless (implementation-library-path? path)
      (let ([source (read-file-string path)])
      (check-doc-blocks path source report)
      (call-with-port
       (open-input-file path)
       (lambda (port)
         (let ([library (read port)])
           (when (and (pair? library) (eq? (car library) 'library))
             (let* ([forms (cddr library)]
                    [export-form (find (lambda (form)
                                         (and (pair? form) (eq? (car form) 'export)))
                                       forms)]
                    [exports (if export-form (exported-name* export-form) '())]
                    [definition* (definition-info* forms)]
                    [reported-record* '()])
               (for-each
                (lambda (definition)
                  (let ([name (car definition)] [kind (cdr definition)])
                    (when (memq name exports)
                      (cond
                       [(pair? kind)
                        (unless (or (memq (cadr kind) reported-record*)
                                    (doc-adjacent?
                                     source 'record (cadr kind)
                                     (list (format "(define-record-type (~a" (cadr kind))
                                           (format "(define-record-type ~a" (cadr kind)))))
                          (set! reported-record* (cons (cadr kind) reported-record*))
                          (report path #f
                                  (format "missing record documentation for ~a" (cadr kind))))]
                       [(memq kind '(proc macro))
                        (unless (doc-adjacent?
                                 source kind name
                                 (if (eq? kind 'macro)
                                     (list (format "(define-syntax ~a" name))
                                     (list (format "(define-who ~a" name)
                                           (format "(define ~a" name)
                                           (format "(define (~a" name))))
                          (report path #f
                                  (format "missing ~a documentation for ~a" kind name)))]))))
                definition*))))))))))

(define main
  (lambda ()
    (let ([arguments (command-line-arguments)] [failure* '()])
      (define report
        (lambda (path line message)
          (set! failure*
                (cons (if line
                          (format "~a:~a: ~a" path line message)
                          (format "~a: ~a" path message))
                      failure*))))
      (when (null? arguments)
        (error 'check-public-api-docs "expected one or more files or directories"))
      (for-each (lambda (path) (check-library path report))
                (apply append (map collect-scheme-files arguments)))
      (unless (null? failure*)
        (for-each (lambda (message) (display message) (newline)) (reverse failure*))
        (exit 1)))))

(main)
