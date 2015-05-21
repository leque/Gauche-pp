(use gauche.generator)
(use pp)

(define (main args)
  (when (null? (cdr args))
    (format (current-error-port)
            "usage: ~A file.scm [width]~%"
            (sys-basename (car args)))
    (exit 1))
  (let ()
    (define n (string->number (caddr args)))
    (define (pp x)
      (pretty-print x n)
      (newline))
    (generator-for-each pp (file->generator (cadr args) read))
    0))