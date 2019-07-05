(define-module pp.json
  (use srfi-1)
  (use srfi-13)
  (use gauche.record)
  (use gauche.sequence)
  (use gauche.unicode)
  (use pp.core)
  (export pretty-print-json))

(select-module pp.json)

(define (pretty-print-json obj
                           :key
                           (port (current-output-port))
                           (indent 2)
                           (width 78)
                           )
  (let ((pp (x->json-pp obj :indent indent)))
    (pp-write pp width port)
    (newline port)))

(define json-escapes
  '((#\" . #\")
    (#\\ . #\\)
    (#\/ . #\/)
    (#\x08 . #\b)
    (#\x0c . #\f)
    (#\x0a . #\n)
    (#\x0d . #\r)
    (#\x09 . #\t)
    ))

(define (x->json-pp obj :key indent)
  (let* (($nl (if indent (pp-break) ""))
         ($nl0 (if indent (pp-break "") ""))
         ($colon (if indent ": " ":"))
         ($comma (list "," $nl))
         ($nest (if indent (cut pp-nest indent <...>) list)))
    (let loop ((obj obj))
      (cond
       ((or (eq? obj 'true) (eq? obj #t))
        "true")
       ((or (eq? obj 'false) (eq? obj #f))
        "false")
       ((eq? obj 'null)
        "null")
       ((string? obj)
        (with-output-to-string
          (lambda ()
            (write-char #\")
            (string-for-each
             (lambda (c)
               (define (hexescape n)
                 (format #t "\\u~4,'0x" n))
               (cond ((assv-ref json-escapes c #f)
                      => (lambda (p)
                           (write-char #\\)
                           (write-char p)))
                     ((eq? (char-general-category c) 'Cc)
                      (let ((n (char->ucs c)))
                        (if (< n #x10000)
                            (hexescape n)
                            (for-each hexescape (ucs4->utf16 n)))))
                     (else
                      (write-char c))))
             obj)
            (write-char #\"))))
       ((number? obj)
        (cond ((or (not (real? obj))
                   (not (finite? obj)))
               (error "cannot represent in json number" obj))
              ((not (integer? obj))
               (write-to-string (inexact obj)))
              (else
               (write-to-string obj))))
       ((or (list? obj) (is-a? obj <dictionary>))
        (pp-group "{"
                  ($nest
                   (map (lambda (k&v sep)
                          (list sep
                                (pp-group (loop (x->string (car k&v)))
                                          $colon
                                          (loop (cdr k&v)))))
                        obj
                        (cons $nl0 (circular-list $comma))))
                  $nl0
                  "}"))
       ((is-a? obj <sequence>)
        (pp-group "["
                  ($nest
                   (map (lambda (x sep)
                          (list sep (loop x)))
                        obj
                        (cons $nl0 (circular-list $comma))))
                  $nl0
                  "]"))
       (else
        (error "cannot represent in json" obj))))))
