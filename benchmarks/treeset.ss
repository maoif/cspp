(import (cspp)
        (cspp treeset)
        (cspp benchmark))
(load "benchmarks/common.ss")

(define make-set-case
  (lambda (type operation size)
    (let* ([specialized? (eq? type 'fxtreeset)]
           ;; Select matching generic or fixnum operations before measurement.
           [new (if specialized? (lambda () (make-fxtreeset fx= fx<))
                                (lambda () (make-treeset = <)))]
           [put! (if specialized? fxtreeset-add! treeset-add!)]
           [lookup (if specialized? fxtreeset-contains? treeset-contains?)]
           [remove! (if specialized? fxtreeset-delete! treeset-delete!)]
           [contents (if specialized? fxtreeset->list treeset->list)]
           [copy (if specialized?
                     (lambda (container) (fxtreeset-map (lambda (item) item) container))
                     (lambda (container) (treeset-map (lambda (item) item) container)))]
           [keys (iota size)])
      ;; Register a workload with deterministic inputs created outside timed regions.
      (make-benchmark
       (string->symbol (format "~a-~a" type operation))
       (lambda (state n prepared)
         (case operation
           [(populate)
            (let ([container (new)])
              (for-each (lambda (key) (put! container key)) keys)
              (benchmark-consume container))]
           [(lookup)
            (for-each (lambda (key) (benchmark-consume (lookup prepared key))) keys)]
           [(traverse) (benchmark-consume (contents prepared))]
           [(update delete)
            ;; Reset each iteration without charging setup to the operation under test.
            (benchmark-pause-timing state)
            (let ([container (copy prepared)])
              (benchmark-resume-timing state)
              (for-each (lambda (key)
                           (if (eq? operation 'delete)
                              (remove! container key)
                              (begin
                                (remove! container key)
                                (put! container key))))
                        keys)
              (benchmark-consume container))]))
       `((args (size) (,size))
         (setup . ,(lambda (state)
                     (let ([container (new)])
                       (for-each (lambda (key) (put! container key)) keys)
                       container))))))))

(benchmark-cli
 'treeset-benchmark
 "Compare generic and fixnum treeset populate, lookup, traversal, update, and delete."
 (lambda (ref)
   (let ([size (benchmark-size ref)])
     (benchmark-run-cli!
      'treeset-benchmark "treeset representations"
      (apply append
             (map (lambda (type)
                    (map (lambda (operation) (make-set-case type operation size))
                         '(populate lookup traverse update delete)))
                  '(treeset fxtreeset)))
      ref)))
 (command-line-arguments))
