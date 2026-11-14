;;; Chez-Scheme library for Alex Shinn's Irregex
;;;
;;; Copyright (c) 2016 Federico Beffa <beffa@fbengineering.ch>
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(library (cspp private irregex)
  (export
   irregex
   string->irregex
   sre->irregex
   string->sre
   maybe-string->sre
   irregex?
   irregex-match-data?
   irregex-new-matches
   irregex-reset-matches!
   irregex-search
   irregex-search/matches
   irregex-match
   irregex-search/chunked
   irregex-match/chunked
   irregex-fold/chunked
   make-irregex-chunker
   irregex-match-substring
   irregex-match-subchunk
   irregex-match-start-chunk
   irregex-match-end-chunk
   irregex-match-start-index
   irregex-match-end-index
   irregex-match-num-submatches
   irregex-match-names
   irregex-match-valid-index?
   irregex-fold
   irregex-replace
   irregex-replace/all
   irregex-dfa
   irregex-dfa/search
   irregex-nfa
   irregex-flags
   irregex-lengths
   irregex-names
   irregex-num-submatches
   irregex-extract
   irregex-split
   sre->cset
   ;; originally irregex-utils
   irregex-quote
   irregex-opt
   sre->string)
  (import
   (except (rnrs) error find filter remove)
   (rnrs r5rs)
   (rnrs mutable-pairs)
   (rnrs mutable-strings)
   (only (cspp chez) for-all include get-output-string open-output-string))

  ;; definition from irregex
  (define (error msg . args)
    (apply assertion-violation 'irregex msg args))

  ;;;; irregex-utils.scm
  ;;
  ;; Copyright (c) 2010 Alex Shinn.  All rights reserved.
  ;; BSD-style license: http://synthcode.com/license.txt

  (define rx-special-chars
    "\\|[](){}.*+?^$#")

  (define (string-scan-char str c . o)
    (let ((end (string-length str)))
      (let scan ((i (if (pair? o) (car o) 0)))
        (cond ((= i end) #f)
              ((eqv? c (string-ref str i)) i)
              (else (scan (+ i 1)))))))

  (define (irregex-quote str)
    (list->string
     (let loop ((ls (string->list str)) (res '()))
       (if (null? ls)
           (reverse res)
           (let ((c (car ls)))
             (if (string-scan-char rx-special-chars c)
                 (loop (cdr ls) (cons c (cons #\\ res)))
                 (loop (cdr ls) (cons c res))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (irregex-opt ls)
    (define (make-alt ls)
      (cond ((null? (cdr ls)) (car ls))
            ((for-all char? ls) (list (list->string ls)))
            (else (cons 'or ls))))
    (define (make-seq ls)
      (cond ((null? (cdr ls)) (car ls))
            ((for-all (lambda (x) (or (string? x) (char? x))) ls)
             (apply string-append (map (lambda (x) (if (char? x) (string x) x)) ls)))
            (else (cons 'seq ls))))
    (cond
     ((null? ls) "")
     ((null? (cdr ls)) (car ls))
     (else
      (let ((chars (make-vector 256 '())))
        (let lp1 ((ls ls) (empty? #f))
          (if (null? ls)
              (let lp2 ((i 0) (res '()))
                (if (= i 256)
                    (let ((res (make-alt (reverse res))))
                      (if empty? `(? ,res) res))
                    (let ((c (integer->char i))
                          (opts (vector-ref chars i)))
                      (lp2 (+ i 1)
                           (cond
                            ((null? opts) res)
                            ((equal? opts '("")) `(,c ,@res))
                            (else `(,(make-seq (list c (irregex-opt opts)))
                                    ,@res)))))))
              (let* ((str (car ls))
                     (len (string-length str)))
                (if (zero? len)
                    (lp1 (cdr ls) #t)
                    (let ((i (char->integer (string-ref str 0))))
                      (vector-set!
                       chars
                       i
                       (cons (substring str 1 len) (vector-ref chars i)))
                      (lp1 (cdr ls) empty?))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (cset->string ls)
    (let ((out (open-output-string)))
      (let lp ((ls ls))
        (cond
         ((pair? ls)
          (cond
           ((pair? (car ls))
            (display (irregex-quote (string (caar ls))) out)
            (write-char #\- out)
            (display (irregex-quote (string (cdar ls))) out))
           (else (display (irregex-quote (string (car ls))) out)))
          (lp (cdr ls)))))
      (get-output-string out)))

  (define (sre->string obj)
    (let ((out (open-output-string)))
      (let lp ((x obj))
        (cond
         ((pair? x)
          (case (car x)
            ((: seq)
             (cond
              ((and (pair? (cdr x)) (pair? (cddr x)) (not (eq? x obj)))
               (display "(?:" out) (for-each lp (cdr x)) (display ")" out))
              (else (for-each lp (cdr x)))))
            ((submatch)
             (display "(" out) (for-each lp (cdr x)) (display ")" out))
            ((submatch-named)
             (display "(?<" out) (display (cadr x) out) (display ">" out)
             (for-each lp (cddr x)) (display ")" out))
            ((or)
             (display "(?:" out)
             (lp (cadr x))
             (for-each (lambda (x) (display "|" out) (lp x)) (cddr x))
             (display ")" out))
            ((* + ? *? ??)
             (cond
              ((or (pair? (cddr x)) (and (string? (cadr x)) (not (= 1 (string-length (cadr x))))))
               (display "(?:" out) (for-each lp (cdr x)) (display ")" out))
              (else (lp (cadr x))))
             (display (car x) out))
            ((not)
             (cond
              ((and (pair? (cadr x)) (eq? 'cset (caadr x)))
               (display "[^" out)
               (display (cset->string (cdadr x)) out)
               (display "]" out))
              (else (error "can't represent general 'not' in strings" x))))
            ((cset)
             (display "[" out)
             (display (cset->string (cdr x)) out)
             (display "]" out))
            ((- & / ~)
             (cond
              ((or (eqv? #\~ (car x))
                   (and (eq? '- (car x)) (pair? (cdr x)) (eq? 'any (cadr x))))
               (display "[^" out)
               (display (cset->string (if (eqv? #\~ (car x)) (cdr x) (cddr x))) out)
               (display "]" out))
              (else
               (lp `(cset ,@(sre->cset x))))))
            ((w/case w/nocase)
             (display "(?" out)
             (if (eq? (car x) 'w/case) (display "-" out))
             (display ":" out)
             (for-each lp (cdr x))
             (display ")" out))
            (else
             (if (string? (car x))
                 (lp `(cset ,@(string->list (car x))))
                 (error "unknown sre operator" x)))))
         ((symbol? x)
          (case x
            ((bos bol) (display "^" out))
            ((eos eol) (display "$" out))
            ((any nonl) (display "." out))
            (else (error "unknown sre symbol" x))))
         ((string? x)
          (display (irregex-quote x) out))
         ((char? x)
          (display (irregex-quote (string x)) out))
         (else
          (error "unknown sre pattern" x))))
      (get-output-string out)))

  (include "irregex-impl.ss")

  )
