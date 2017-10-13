;;;
;;; Copyright (c) 2008, 2015-2017 OOHASHI, Daichi <dico.leque.comicron@gmail.com>,
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

(define-module pp.core
  (use srfi-1)
  (use gauche.record)
  (use util.match)
  (use text.tree)
  (export pp-write
          pp-group pp-nest pp-break
          pp-string pp-zero-width-string
          <pp-group> pp-group? pp-group-items
          <pp-nest> pp-nest? pp-nest-indent pp-nest-items
          <pp-break> pp-break? pp-break-alternative
          <pp-string> pp-string? pp-string-content pp-string-width
          ))

(select-module pp.core)

;;; API: Write pretty-print document PP to PORT to fit to WIDTH.
(define (pp-write pp width port)
  (write-tree (pp-make-tree width 0 (list (state 0 'flat pp)))
              port))

;;; Pretty-print types.
;;; Documents are represend as follows:
;;;
;;; <doc> ::= ($ <pp-group> (<doc> ...))
;;;         | ($ <pp-nest> n (<doc> ...))
;;;         | ($ <pp-break> s)
;;;         | ($ <pp-string> s w)
;;;         | (? string?)
;;;         | (<doc> . <doc>)
;;;         | ()
(define (pp? x)
  (or (pp-group? x)
      (pp-nest? x)
      (pp-break? x)
      (pp-string? x)
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

(define-record-type <pp-string>
    %make-pp-string
    pp-string?
  (content pp-string-content)
  (width pp-string-width))

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

;;; API: string with an explicit width
(define pp-string
  (case-lambda
    ((s) (pp-string s (string-width s)))
    ((s width)
     (unless (string? s)
       (error "string required, but got" s))
     (%make-pp-string s width))))

;;; API: equivalent to (pp-string s 0)
(define (pp-zero-width-string s)
  (pp-string s 0))

(define (state indent mode doc)
  (list indent mode doc))

(define string-width string-length)

(define (pp-fits? width xs)
  (and (not (negative? width))
       (match xs
         (() #t)
         (((i m ()) ys ...)
          (pp-fits? width ys))
         (((i m ($ <pp-group> (doc ...))) ys ...)
          (pp-fits? width (xcons ys (state i 'flat doc))))
         (((i m ($ <pp-nest> j (doc ...))) ys ...)
          (pp-fits? width (xcons ys (state (+ i j) m doc))))
         (((i 'break ($ <pp-break> _)) ys ...)
          #t)
         (((i 'flat ($ <pp-break> s)) ys ...)
          (pp-fits? (- width (string-width s)) ys))
         (((i m (or ($ <pp-string> _ w)
                    (and (? string? _) (= string-width w))))
           ys ...)
          (pp-fits? (- width w) ys))
         (((i m (y . ys)) zs ...)
          (pp-fits? width (cons* (state i m y) (state i m ys) zs))))))

(define (pp-make-tree width k xs)
  (match xs
    (() "")
    (((i m ()) ys ...)
     (pp-make-tree width k ys))
    (((i m ($ <pp-group> (doc ...))) ys ...)
     (let1 mode (if (pp-fits? (- width k) (cons (state i 'flat doc) ys))
                    'flat
                    'break)
       (pp-make-tree width k (xcons ys (state i mode doc)))))
    (((i m ($ <pp-nest> j (doc ...))) ys ...)
     (pp-make-tree width k (xcons ys (state (+ i j) m doc))))
    (((i 'break ($ <pp-break> _)) ys ...)
     (cons* #\newline
            (make-string i #\space)
            (pp-make-tree width i ys)))
    (((i 'flat ($ <pp-break> s)) ys ...)
     (cons s (pp-make-tree width (+ k (string-width s)) ys)))
    (((i m (or ($ <pp-string> s w)
               (and (? string? s) (= string-width w))))
      ys ...)
     (cons s (pp-make-tree width (+ k w) ys)))
    (((i m (y . ys)) zs ...)
     (pp-make-tree width k (cons* (state i m y) (state i m ys) zs)))))
