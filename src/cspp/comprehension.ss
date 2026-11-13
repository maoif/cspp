(library (cspp comprehension)
  (export for for/reduce for/list for/vector for/hashtable
          for/fold
          for/max for/min for/sum for/product
          for/fxmax for/fxmin for/fxsum for/fxproduct
          for/flmax for/flmin for/flsum for/flproduct

          for* for*/reduce for*/list for*/vector
          for*/fold
          for*/max for*/min for*/sum for*/product)
  (import (except (chezscheme) sleep)
          (cspp private comprehension)
          (cspp iter)
          (cspp private common)
          (cspp list)
          (cspp utils))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   for iterations and comprehensions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax for/reduce
    (lambda (stx)
      (syntax-case stx (break)
        ;; the order of the following cases is important
        [(k (acc-e* ...) (index vi) (cl cl* ...) (break brk brk* ...) (finish fini) e e* ...)
         (begin (printf "for/reduce 11~n")
                (and (eq? 'index (datum index)) (eq? 'finish (datum finish)) (identifier? #'vi)))
         ($generate-for/reduce #'(acc-e* ...)
                               #'vi
                               #'(cl cl* ...)
                               #'(and brk brk* ...)
                               #'fini
                               #'(begin e e* ...))]

        [(k (acc-e* ...) (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (begin (printf "for/reduce 1~n")
                (and (eq? 'index (datum index)) (identifier? #'vi)))
         ($generate-for/reduce #'(acc-e* ...)
                               #f
                               #'(cl cl* ...)
                               #'(and brk brk* ...)
                               #f
                               #'(begin e e* ...))]
        [(k (acc-e* ...) (index vi) (cl cl* ...) (finish fini) e e* ...)
         (begin (printf "for/reduce 13~n")
                (and (eq? 'index (datum index)) (eq? 'finish (datum finish)) (identifier? #'vi)))
         #'(k (acc-e* ...) (index vi) (cl cl* ...) (break #f) (finish fini) e e* ...)]
        [(k (acc-e* ...) (index vi) (cl cl* ...) e e* ...)
         (begin (printf "for/reduce 3~n")
                (and (eq? 'index (datum index)) (identifier? #'vi)))
         #'(k (acc-e* ...) (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (acc-e* ...) (cl cl* ...) (break brk brk* ...) (finish fini) e e* ...)
         (begin (printf "for/reduce 12~n") (eq? 'finish (datum finish)))
         ($generate-for/reduce #'(acc-e* ...)
                               #f
                               #'(cl cl* ...)
                               #'(and brk brk* ...)
                               #'fini
                               #'(begin e e* ...))]
        [(k (acc-e* ...) (cl cl* ...) (break brk brk* ...) e e* ...)
         (begin (printf "for/reduce 2~n") #t)
         ($generate-for/reduce #'(acc-e* ...)
                               #f
                               #'(cl cl* ...)
                               #'(and brk brk* ...)
                               #f
                               #'(begin e e* ...))]
        [(k (acc-e* ...) (cl cl* ...) (finish fini) e e* ...)
         (begin (printf "for/reduce 14~n") (eq? 'finish (datum finish)))
         #'(k (acc-e* ...) (cl cl* ...) (break #f) (finish fini) e e* ...)]
        [(k (acc-e* ...) (cl cl* ...) e e* ...)
         (begin (printf "for/reduce 4~n") #t)
         #'(k (acc-e* ...) (cl cl* ...) (break #f) e e* ...)])))


  (define-syntax for
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for/reduce () (index vi) (cl cl* ...) (break brk brk* ...) (finish (void)) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for/reduce () (cl cl* ...) (break brk brk* ...) (finish (void)) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))


  (define-syntax for/list
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(let ([lb (make-list-builder)])
             (for (index vi) (cl cl* ...) (break brk brk* ...)
                  (lb (begin e e* ...)))
             (lb))]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(let ([lb (make-list-builder)])
             (for (cl cl* ...) (break brk brk* ...)
                  (lb (begin e e* ...)))
             (lb))]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))
  (define-syntax for/vector
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/dynvec
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/hashtable
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/eq-hashtable
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/eqv-hashtable
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/sym-hashtable
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/stream
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))

  (define-syntax for/fold
    (lambda (stx)
      (syntax-case stx (break finish)
        [(k proc acc (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         (with-syntax ([(t p) (generate-temporaries '(t p))])
           #'(let ([p proc])
               (for/reduce ([t acc]) (index vi) (cl cl* ...) (break brk brk* ...)
                           (finish t)
                           (p t (begin e e* ...)))))]
        [(k proc acc (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k proc acc (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k proc acc (cl cl* ...) (break brk brk* ...) e e* ...)
         (with-syntax ([(t p) (generate-temporaries '(t p))])
           #'(let ([p proc])
               (for/reduce ([t acc]) (cl cl* ...) (break brk brk* ...)
                           (finish t)
                           (p t (begin e e* ...)))))]
        [(k proc acc (cl cl* ...) e e* ...)
         #'(k proc acc (cl cl* ...) (break #f) e e* ...)])))

  (define-syntax for/max
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/min
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/sum
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for/fold (lambda (acc x) (+ acc x)) 0
             (index vi)
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for/fold (lambda (acc x) (+ acc x)) 0
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))
  (define-syntax for/avg
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for/reduce
            ([acc #f])
            (index vi) (cl cl* ...) (break brk brk* ...)
            (finish (if (fx= 0 vi) #f (/ acc vi)))
            (let ([val (begin e e* ...)])
              (if acc (+ acc val) val)))]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for/reduce
            ([acc #f])
            (index vi) (cl cl* ...) (break brk brk* ...)
            (finish (if (fx= 0 vi) #f (/ acc vi)))
            (let ([val (begin e e* ...)])
              (if acc (+ acc val) val)))]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))
  (define-syntax for/product
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for/fold (lambda (acc x) (* acc x)) 1
             (index vi)
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for/fold (lambda (acc x) (* acc x)) 1
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))

  (define-syntax for/fxmax
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/fxmin
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/fxsum
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for/fold (lambda (acc x) (fx+ acc x)) 0
             (index vi)
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for/fold (lambda (acc x) (fx+ acc x)) 0
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))
  (define-syntax for/fxavg
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/fxproduct
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for/fold (lambda (acc x) (fx* acc x)) 1
             (index vi)
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for/fold (lambda (acc x) (fx* acc x)) 1
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))

  (define-syntax for/flmax
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/flmin
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/flsum
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for/fold (lambda (acc x) (fl+ acc x)) 0.0
             (index vi)
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for/fold (lambda (acc x) (fl+ acc x)) 0.0
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))
  (define-syntax for/flavg
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for/flproduct
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for/fold (lambda (acc x) (fl* acc x)) 1.0
             (index vi)
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for/fold (lambda (acc x) (fl* acc x)) 1.0
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   for* iterations and comprehensions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax for*/reduce
    (lambda (stx)
      (syntax-case stx (break)
        ;; the order of the following cases is important
        [(k (acc-e* ...) (index vi) (cl cl* ...) (break brk brk* ...) (finish fini) e e* ...)
         (begin (printf "for*/reduce 11~n")
                (and (eq? 'index (datum index)) (eq? 'finish (datum finish)) (identifier? #'vi)))
         ($generate-for*/reduce #'(acc-e* ...)
                                #'vi
                                #'(cl cl* ...)
                                #'(and brk brk* ...)
                                #'fini
                                #'(begin e e* ...))]

        [(k (acc-e* ...) (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (begin (printf "for*/reduce 1~n")
                (and (eq? 'index (datum index)) (identifier? #'vi)))
         ($generate-for*/reduce #'(acc-e* ...)
                                #f
                                #'(cl cl* ...)
                                #'(and brk brk* ...)
                                #f
                                #'(begin e e* ...))]
        [(k (acc-e* ...) (index vi) (cl cl* ...) (finish fini) e e* ...)
         (begin (printf "for*/reduce 13~n")
                (and (eq? 'index (datum index)) (eq? 'finish (datum finish)) (identifier? #'vi)))
         #'(k (acc-e* ...) (index vi) (cl cl* ...) (break #f) (finish fini) e e* ...)]
        [(k (acc-e* ...) (index vi) (cl cl* ...) e e* ...)
         (begin (printf "for*/reduce 3~n")
                (and (eq? 'index (datum index)) (identifier? #'vi)))
         #'(k (acc-e* ...) (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (acc-e* ...) (cl cl* ...) (break brk brk* ...) (finish fini) e e* ...)
         (begin (printf "for*/reduce 12~n") (eq? 'finish (datum finish)))
         ($generate-for*/reduce #'(acc-e* ...)
                                #f
                                #'(cl cl* ...)
                                #'(and brk brk* ...)
                                #'fini
                                #'(begin e e* ...))]
        [(k (acc-e* ...) (cl cl* ...) (break brk brk* ...) e e* ...)
         (begin (printf "for*/reduce 2~n") #t)
         ($generate-for*/reduce #'(acc-e* ...)
                                #f
                                #'(cl cl* ...)
                                #'(and brk brk* ...)
                                #f
                                #'(begin e e* ...))]
        [(k (acc-e* ...) (cl cl* ...) (finish fini) e e* ...)
         (begin (printf "for*/reduce 14~n") (eq? 'finish (datum finish)))
         #'(k (acc-e* ...) (cl cl* ...) (break #f) (finish fini) e e* ...)]
        [(k (acc-e* ...) (cl cl* ...) e e* ...)
         (begin (printf "for*/reduce 4~n") #t)
         #'(k (acc-e* ...) (cl cl* ...) (break #f) e e* ...)])))


  (define-syntax for*
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for*/reduce () (index vi) (cl cl* ...) (break brk brk* ...) (finish (void)) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for*/reduce () (cl cl* ...) (break brk brk* ...) (finish (void)) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))


  (define-syntax for*/list
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(let ([lb (make-list-builder)])
             (for* (index vi) (cl cl* ...) (break brk brk* ...)
                   (lb (begin e e* ...)))
             (lb))]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(let ([lb (make-list-builder)])
             (for* (cl cl* ...) (break brk brk* ...)
                   (lb (begin e e* ...)))
             (lb))]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))
  (define-syntax for*/vector
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/dynvec
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/hashtable
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/eq-hashtable
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/eqv-hashtable
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/sym-hashtable
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/stream
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))

  (define-syntax for*/fold
    (lambda (stx)
      (syntax-case stx (break finish)
        [(k proc acc (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         (with-syntax ([(t p) (generate-temporaries '(t p))])
           #'(let ([p proc])
               (for*/reduce ([t acc]) (index vi) (cl cl* ...) (break brk brk* ...)
                            (finish t)
                            (p t (begin e e* ...)))))]
        [(k proc acc (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k proc acc (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k proc acc (cl cl* ...) (break brk brk* ...) e e* ...)
         (with-syntax ([(t p) (generate-temporaries '(t p))])
           #'(let ([p proc])
               (for*/reduce ([t acc]) (cl cl* ...) (break brk brk* ...)
                            (finish t)
                            (p t (begin e e* ...)))))]
        [(k proc acc (cl cl* ...) e e* ...)
         #'(k proc acc (cl cl* ...) (break #f) e e* ...)])))

  (define-syntax for*/max
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/min
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/sum
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for*/fold (lambda (acc x) (+ acc x)) 0
             (index vi)
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for*/fold (lambda (acc x) (+ acc x)) 0
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))
  (define-syntax for*/avg
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for*/reduce
            ([acc #f])
            (index vi) (cl cl* ...) (break brk brk* ...)
            (finish (if (fx= 0 vi) #f (/ acc vi)))
            (let ([val (begin e e* ...)])
              (if acc (+ acc val) val)))]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for*/reduce
            ([acc #f])
            (index vi) (cl cl* ...) (break brk brk* ...)
            (finish (if (fx= 0 vi) #f (/ acc vi)))
            (let ([val (begin e e* ...)])
              (if acc (+ acc val) val)))]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))
  (define-syntax for*/product
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for*/fold (lambda (acc x) (* acc x)) 1
             (index vi)
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for*/fold (lambda (acc x) (* acc x)) 1
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))

  (define-syntax for*/fxmax
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/fxmin
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/fxavg
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/fxsum
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for*/fold (lambda (acc x) (fx+ acc x)) 0
             (index vi)
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for*/fold (lambda (acc x) (fx+ acc x)) 0
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))
  (define-syntax for*/fxproduct
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for*/fold (lambda (acc x) (fx* acc x)) 1
             (index vi)
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for*/fold (lambda (acc x) (fx* acc x)) 1
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))

  (define-syntax for*/flmax
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/flmin
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/flavg
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))
  (define-syntax for*/flsum
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for*/fold (lambda (acc x) (fl+ acc x)) 0.0
             (index vi)
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for*/fold (lambda (acc x) (fl+ acc x)) 0.0
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])))
  (define-syntax for*/flproduct
    (lambda (stx)
      (syntax-case stx (break)
        [(k (index vi) (cl cl* ...) (break brk brk* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(for*/fold (lambda (acc x) (fl* acc x)) 1.0
             (index vi)
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (index vi) (cl cl* ...) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'vi))
         #'(k (index vi) (cl cl* ...) (break #f) e e* ...)]
        [(k (cl cl* ...) (break brk brk* ...) e e* ...)
         #'(for*/fold (lambda (acc x) (fl* acc x)) 1.0
             (cl cl* ...) (break brk brk* ...) e e* ...)]
        [(k (cl cl* ...) e e* ...)
         #'(k (cl cl* ...) (break #f) e e* ...)])
      (syntax-case stx ()
        [_ (todo)])))

  )
