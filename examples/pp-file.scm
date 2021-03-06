(use gauche.generator)
(use pp)

(define (main args)
  (define (f file :optional (width "78"))
    (define n (string->number width))
    (define (pp x)
      (pretty-print x :width n)
      (newline))
    (generator-for-each pp (file->generator (cadr args) read)))
  (when (null? (cdr args))
    (format (current-error-port)
            "usage: ~A file.scm [width]~%"
            (sys-basename (car args)))
    (exit 1))
  (apply f (cdr args))
  0)
