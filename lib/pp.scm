;;;
;;; Copyright (c) 2008, 2015 OOHASHI, Daichi <dico.leque.comicron@gmail.com>,
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;;
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; 3. Neither the name of the authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this
;;;    software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define-module pp
  (use srfi-11)
  (use gauche.parameter)
  (use gauche.record)
  (use util.match)
  (use pp.core)
  (export pretty-print
          x->pp
          pp-format-rules
          pp-scheme-indent pp-scheme-abbrev))

(select-module pp)

(define-record-type <pp-context>
    %make-pp-context
    pp-context?
  ;; the current datum label count
  (count pp-context-count pp-context-count-set!)
  ;; obj * int mappings.
  ;; If a value n > 0, it means number of times an obj was seen in scan path.
  ;; If n <= 0, -n means a datum label for an obj in print path.
  (hash-table pp-context-hash-table))

(define (make-pp-context)
  (%make-pp-context 0 (make-hash-table)))

(define (pp-context-clear! ctx)
  ;; clear the hash-table for GC friendliness.
  ;; See also cleanup_port_context in Gauche/src/write.c
  (hash-table-clear! (pp-context-hash-table ctx))
  (pp-context-count-set! ctx 0))

;;; API: Pretty-print OBJ to PORT to fit to WIDTH.
(define pretty-print
  (case-lambda
   ((obj)
    (pretty-print obj #f #f))
   ((obj port-or-width)
    (if (port? port-or-width)
        (pretty-print obj port-or-width #f)
        (pretty-print obj #f port-or-width)))
   ((obj port width)
    (let ((port (or port (current-output-port)))
          (width (or width 78))
          (ctx (make-pp-context)))
      (pp-scan! obj ctx)
      (let ((pp (x->pp obj ctx)))
        (pp-write pp width port)
        (newline port)
        (pp-context-clear! ctx))))))

;;; API: Format rules for a specific list structure.
;;; A hashtable whose keys are symbols of a car of a list and
;;; values are format procedures.
;;; An format procedure could return a pretty-print document or #f.
;;; #f means `cannot format this list. use default strategy'.
;;; If an format function raises exception,
;;; simply ignore it and use default strategy.
(define pp-format-rules (make-parameter (make-hash-table 'eq?)))

(define <> (pp-break))

(define (pp-sp-breakable xs)
  (intersperse <> xs))

(define (pp/sep ctx sep xs)
  (intersperse <> (map (cut x->pp <> ctx) xs)))

;;; API: convert an object to a pretty-print document.
;;; You can override default behaviour.
(define-method x->pp (obj ctx)
  (write-to-string obj))

(define-method x->pp ((obj <list>) ctx)
  (define (list->pp xs)
    (let retry ((smart-indent? #t))
      (cond
       ((null? xs) "()")
       ((and smart-indent?
             (ref (pp-format-rules) (car xs) #f))
        => (lambda (proc)
             (guard (exc
                     (else
                      (warn "x->pp: ~S: ~A~%"
                            (car xs)
                            (if (&message exc)
                                (condition-ref exc 'message)
                                ""))
                      (retry #f)))
               (or (proc xs ctx)
                   (retry #f)))))
       (else
        (let loop ((ys (cdr xs))
                   (rs (list (x->pp (car xs) ctx))))
          (if (null? ys)
              (let ((zs (pp-sp-breakable (reverse rs))))
                (pp-group "(" (pp-nest 1 zs) ")"))
              (receive (label defining) (pp-label! ys ctx)
                (cond ((and label defining)
                       (loop '()
                             (cons* (list label
                                          (pp-nest (string-length label)
                                            (list->pp ys)))
                                    "." rs)))
                      (label
                       (loop '() (cons* label "." rs)))
                      ((pair? ys)
                       (loop (cdr ys) (cons (x->pp (car ys) ctx) rs)))
                      (else
                       (loop '() (cons* (x->pp ys ctx) "." rs)))))))))))
  (pp-with-label obj ctx (cut list->pp obj)))

(define-method x->pp ((v <vector>) ctx)
  (define (do-pp)
    (pp-group "#("
              (pp-nest 2 (pp/sep ctx <> (vector->list v)))
              ")"))
  (pp-with-label v ctx do-pp))

(define (pp-label! v ctx)
  (let* ((ht (pp-context-hash-table ctx))
         (label (hash-table-get ht v 1)))
    (cond
     ((> label 1)
      (let ((c (pp-context-count ctx)))
        (hash-table-put! ht v (- c))
        (inc! (pp-context-count ctx))
        (values (format "#~D=" c) #t)))
     ((< label 1)
      (values (format "#~D#" (- label)) #f))
     (else (values #f #f)))))

(define (pp-with-label v ctx do-pp)
  (receive (label defining) (pp-label! v ctx)
    (cond ((and label defining)
           (list label
                 (pp-nest (string-length label)
                   (do-pp))))
          (label label)
          (else (do-pp)))))

(define (pp-mark! obj ctx f)
  (let ((ht (pp-context-hash-table ctx)))
    (cond ((hash-table-exists? ht obj)
           (hash-table-update! ht obj (cut + <> 1)))
          (else
           (hash-table-put! ht obj 1)
           (f)))))

(define-method pp-scan! (obj ctx)
  #f)

(define-method pp-scan! ((obj <pair>) ctx)
  (pp-mark! obj ctx
            (lambda ()
              (pp-scan! (car obj) ctx)
              (pp-scan! (cdr obj) ctx))))

(define-method pp-scan! ((obj <vector>) ctx)
  (pp-mark! obj ctx
            (lambda ()
              (vector-for-each (cut pp-scan! <> ctx) obj))))

;;; A Scheme code formatter.
;;; ((pp-scheme-indent n) '(form expr1 expr2 ...))
;;; indents form as follows:
;;;
;;; when n = 0:
;;;   (form expr1 exp2 ... exprn)
;;;   or
;;;   (form
;;;     expr1
;;;     expr2
;;;     ...)
;;;
;;; when n > 0:
;;;   (form expr1 exp2 ... exprn)
;;;   or
;;;   (form expr1
;;;       expr2
;;;       ...
;;;       exprn
;;;     exprn+1
;;;     ...)
;;;
;;; when n < 0:
;;;   (form expr1 exp2 ... exprn)
;;;   or
;;;   (form expr1
;;;         expr2
;;;         ...)
(define (pp-scheme-indent n)
  (define (f xs)
    (let ((s (symbol->string (car xs))))
      (values s (string-length s))))
  (cond
   ((not (integer? n))
    (error "an integer required, but got" n))
   ((zero? n)
    (lambda (xs ctx)
      (pp-group "("
                (pp-nest 2
                  (pp/sep ctx <> xs))
                ")")))
   ((= n 1)
    ;; fast-path
    (lambda (xs ctx)
      (let-values  (((h len) (f xs))
                    ((ps) (pp/sep ctx <> (cdr xs))))
        (pp-group "(" h " "
                  (pp-nest (+ len 2) (car ps))
                  (pp-nest 2 (cdr ps))
                  ")"))))
   ((positive? n)
    (lambda (xs ctx)
      (let*-values (((s len) (f xs))
                    ((ys zs) (split-at* (cdr xs) n))
                    ((ys) (pp/sep ctx <> ys)))
        (pp-group "(" s " "
                  (pp-group
                   (pp-nest (+ 2 len) (car ys))
                   (pp-nest 4 (cdr ys)))
                  (if (null? zs)
                      '()
                      (pp-nest 2
                        <>
                        (pp-group (pp/sep ctx <> zs))))
                  ")"))))
   (else
    (lambda (xs ctx)
      (let-values  (((h len) (f xs)))
        (pp-group "(" h " "
                  (pp-nest (+ len 2)
                    (pp/sep ctx <> (cdr xs)))
                  ")"))))))

;;; Scheme abbreviation formatter.
;;; ((pp-scheme-abbrev "<pfx>") '(pfx form))
;;; pretty-prints `<pfx>form'.
(define (pp-scheme-abbrev abbr)
  (lambda (xs ctx)
    (if (and (pair? xs)
             (pair? (cdr xs))
             (null? (cddr xs)))
        (pp-group abbr
                  (pp-nest (string-length abbr) (x->pp (cadr xs) ctx)))
        #f)))

;;; R7RS Code Formatters --------------------------------------------

(define (pp-install-rules)
  (define pp- (pp-scheme-indent -1))
  (define pp0 (pp-scheme-indent 0))
  (define pp1 (pp-scheme-indent 1))
  (define pp2 (pp-scheme-indent 2))
  (define pp3 (pp-scheme-indent 3))
  (define (pp-let xs ctx)
    (if (symbol? (cadr xs))
        (pp2 xs ctx)
        (pp1 xs ctx)))
  (for-each
   (match-lambda
    ((proc sym)
     (set! (ref (pp-format-rules) sym) proc)))
   `(
     (,(pp-scheme-abbrev "'") quote)
     (,(pp-scheme-abbrev "`") quasiquote)
     (,(pp-scheme-abbrev ",") ,'unquote)
     (,(pp-scheme-abbrev ",@") ,'unquote-splicing)

     (,pp- and)
     (,pp0 begin)
     (,pp0 cond)
     (,pp0 cond-expand)
     (,pp1 define)
     (,pp3 define-record-type)
     (,pp1 define-syntax)
     (,pp1 define-values)
     (,pp2 do)
     (,pp1 guard)
     (,pp- if)
     (,pp1 lambda)
     (,pp-let let)
     (,pp1 let-*)
     (,pp1 let*-values)
     (,pp1 let-syntax)
     (,pp1 let-values)
     (,pp1 letrec)
     (,pp1 letrec*)
     (,pp1 letrec-syntax)
     (,pp- or)
     (,pp0 set!)
     (,pp0 syntax-rules)
     (,pp1 unless)
     (,pp1 when)

     (,pp1 call-with-port)
     (,pp1 call-with-values)
     (,pp1 with-exception-handler)

     (,pp1 call-with-input-file)
     (,pp1 call-with-output-file)
     (,pp1 with-input-from-file)
     (,pp1 with-output-from-file)
     )))

(pp-install-rules)
