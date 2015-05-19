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

;;; Reference: [1] Christian Lindig, "Strictly Pretty", April, 2000
;;; http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200

(define-module pp
  (use srfi-1)
  (use srfi-11)
  (use gauche.parameter)
  (use gauche.record)
  (use util.match)
  (use text.tree)
  (export pretty-print
          pp-group pp-nest pp-break
          <pp-group> pp-group? pp-group-items
          <pp-nest> pp-nest? pp-nest-indent pp-nest-items
          <pp-break> pp-break? pp-break-alternative
          pp? x->pp
          pp-format-rules
          pp-scheme-indent pp-scheme-abbrev))

(select-module pp)

;;; Pretty-print types.
;;; Documents are represend as follows:
;;;
;;; <doc> ::= ($ <pp-group> (<doc> ...))
;;;         | ($ <pp-nest> n (<doc> ...))
;;;         | ($ <pp-break> s)
;;;         | (? string?)
;;;         | (<doc> ...)
(define (pp? x)
  (or (pp-group? x)
      (pp-nest? x)
      (pp-break? x)
      (string? x)
      (pair? x)
      (null? x)))

(define-record-type <pp-group>
    make-pp-group
    pp-group?
  (items pp-group-items))

(define-record-type <pp-nest>
    make-pp-nest
    pp-nest?
    (indent pp-nest-indent)
    (items pp-nest-items))

(define-record-type <pp-break>
    make-pp-break
    pp-break?
  (alternative pp-break-alternative))

;;; API: Create a pretty-print group.
(define (pp-group . args)
  (make-pp-group args))

;;; API: Create a pretty-print nesting.
;; (put 'pp-nest 'scheme-indent-function 1)
(define (pp-nest n . args)
  (make-pp-nest n args))

;;; API: Create a pretty-print line break.
(define pp-break
  (let ((break (make-pp-break " ")))
    (case-lambda
     (() break)
     ((alt) (make-pp-break alt)))))

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
          (pp (x->pp obj)))
      (write-tree (pp-make-tree width 0 (list (ctx 0 'flat pp)))
                  port)
      (newline port)))))

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

(define (pp/sep sep xs)
  (intersperse <> (map x->pp xs)))

;;; API: convert an object to a pretty-print document.
;;; You can override default behaviour.
(define-method x->pp (obj)
  (write-to-string obj))

(define-method x->pp ((xs <list>))
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
             (or (proc xs)
                 (retry #f)))))
     (else
      (let loop ((ys (cdr xs))
                 (rs (list (x->pp (car xs)))))
        (cond ((null? ys)
               (let ((zs (pp-sp-breakable (reverse rs))))
                 (pp-group "(" (pp-nest 1 zs) ")")))
              ((pair? ys)
               (loop (cdr ys) (cons (x->pp (car ys)) rs)))
              (else
               (loop '() (cons* (x->pp ys) "." rs)))))))))

(define-method x->pp ((v <vector>))
  (pp-group "#("
            (pp-nest 2 (pp/sep <> (vector->list v)))
            ")"))

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
    (lambda (xs)
      (pp-group "("
                (pp-nest 2
                  (pp/sep <> xs))
                ")")))
   ((= n 1)
    ;; fast-path
    (lambda (xs)
      (let-values  (((h len) (f xs))
                    ((ps) (pp/sep <> (cdr xs))))
        (pp-group "(" h " "
                  (pp-nest (+ len 2) (car ps))
                  (make-pp-nest 2 (cdr ps))
                  ")"))))
   ((positive? n)
    (lambda (xs)
      (let*-values (((s len) (f xs))
                    ((ys zs) (split-at* (cdr xs) n))
                    ((ys) (pp/sep <> ys)))
        (pp-group "(" s " "
                  (pp-group
                   (pp-nest (+ 2 len) (car ys))
                   (pp-nest 4 (cdr ys)))
                  (if (null? zs)
                      '()
                      (pp-nest 2
                        <>
                        (pp-group (pp/sep <> zs))))
                  ")"))))
   (else
    (lambda (xs)
      (let-values  (((h len) (f xs)))
        (pp-group "(" h " "
                  (pp-nest (+ len 2)
                    (pp/sep <> (cdr xs)))
                  ")"))))))

;;; Scheme abbreviation formatter.
;;; ((pp-scheme-abbrev "<pfx>") '(pfx form))
;;; pretty-prints `<pfx>form'.
(define (pp-scheme-abbrev abbr)
  (lambda (xs)
    (if (and (pair? xs)
             (pair? (cdr xs))
             (null? (cddr xs)))
        (pp-group abbr
                  (pp-nest (string-length abbr) (x->pp (cadr xs))))
        #f)))

;;;; Core -----------------------------------------------------------

(define (ctx indent mode doc)
  (list indent mode doc))

(define (pp-fits? width xs)
  (and (not (negative? width))
       (match xs
         (() #t)
         (((i m ()) ys ...)
          (pp-fits? width ys))
         (((i m ($ <pp-group> (doc ...))) ys ...)
          (pp-fits? width (xcons ys (ctx i 'flat doc))))
         (((i m ($ <pp-nest> j (doc ...))) ys ...)
          (pp-fits? width (xcons ys (ctx (+ i j) m doc))))
         (((i 'break ($ <pp-break> _)) ys ...)
          #t)
         (((i 'flat ($ <pp-break> s)) ys ...)
          (pp-fits? (- width (string-length s)) ys))
         (((i m [? string? s]) ys ...)
          (pp-fits? (- width (string-length s)) ys))
         (((i m (y . ys)) zs ...)
          (pp-fits? width (cons* (ctx i m y) (ctx i m ys) zs))))))

(define (pp-make-tree width k xs)
  (match xs
    (() "")
    (((i m ()) ys ...)
     (pp-make-tree width k ys))
    (((i m ($ <pp-group> (doc ...))) ys ...)
     (let1 mode (if (pp-fits? (- width k) (cons (ctx i 'flat doc) ys))
                    'flat
                    'break)
       (pp-make-tree width k (xcons ys (ctx i mode doc)))))
    (((i m ($ <pp-nest> j (doc ...))) ys ...)
     (pp-make-tree width k (xcons ys (ctx (+ i j) m doc))))
    (((i 'break ($ <pp-break> _)) ys ...)
     (cons* #\newline
            (make-string i #\space)
            (pp-make-tree width i ys)))
    (((i 'flat ($ <pp-break> s)) ys ...)
     (cons s (pp-make-tree width (+ k 1) ys)))
    (((i m [? string? s]) ys ...)
     (cons s (pp-make-tree width (+ k (string-length s)) ys)))
    (((i m (y . ys)) zs ...)
     (pp-make-tree width k (cons* (ctx i m y) (ctx i m ys) zs)))))

;;; R7RS Code Formatters --------------------------------------------

(define (pp-install-rules)
  (define pp- (pp-scheme-indent -1))
  (define pp0 (pp-scheme-indent 0))
  (define pp1 (pp-scheme-indent 1))
  (define pp2 (pp-scheme-indent 2))
  (define pp3 (pp-scheme-indent 3))
  (define (pp-let xs)
    (if (symbol? (cadr xs))
        (pp2 xs)
        (pp1 xs)))
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
