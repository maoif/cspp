(import (cspp) (cspp treemap) (cspp benchmark))
(load "benchmarks/common.ss")

(define make-map-case
  (lambda (type operation size)
    (let* ([specialized? (eq? type 'fxtreemap)]
           ;; Select matching generic or fixnum operations before measurement.
           [new (if specialized? (lambda () (make-fxtreemap fx= fx<))
                                (lambda () (make-treemap = <)))]
           [put! (if specialized? fxtreemap-set! treemap-set!)]
           [lookup (if specialized? fxtreemap-ref treemap-ref)]
           [remove! (if specialized? fxtreemap-delete! treemap-delete!)]
           [contents (if specialized? fxtreemap->list treemap->list)]
           [copy (if specialized?
                     (lambda (container) (fxtreemap-map (lambda (key value) (values key value)) container))
                     (lambda (container) (treemap-map (lambda (key value) (values key value)) container)))]
           [keys (iota size)])
      ;; Register a workload with deterministic inputs created outside timed regions.
      (make-benchmark
       (string->symbol (format "~a-~a" type operation))
       (lambda (state n prepared)
         (case operation
           [(populate)
            (let ([container (new)])
              (for-each (lambda (key) (put! container key key)) keys)
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
                              (put! container key (fx+ key 1))))
                        keys)
              (benchmark-consume container))]))
       `((args (size) (,size))
         (setup . ,(lambda (state)
                     (let ([container (new)])
                       (for-each (lambda (key) (put! container key key)) keys)
                       container))))))))

(benchmark-cli
 'treemap-benchmark
 "Compare generic and fixnum treemap populate, lookup, traversal, update, and delete."
 (lambda (ref)
   (let ([size (benchmark-size ref)])
     (benchmark-run-cli!
      'treemap-benchmark "treemap representations"
      (apply append
             (map (lambda (type)
                    (map (lambda (operation) (make-map-case type operation size))
                         '(populate lookup traverse update delete)))
                  '(treemap fxtreemap)))
      ref)))
 (command-line-arguments))
