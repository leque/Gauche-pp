;;;
;;; Test pp
;;;

(use gauche.test)

(test-start "pp")
(use pp)
(test-module 'pp)

(use pp.core)
(test-module 'pp.core)

(use pp.json)
(test-module 'pp.json)

(define-syntax test-pp
  (syntax-rules ()
    ((_ (opt ...) expr str)
     (test* (format "keys=~S: ~A"
                    '(opt ...)
                    (with-output-to-string
                      (lambda ()
                        (write/ss 'expr))))
            str
            (with-output-to-string
              (lambda ()
                (pretty-print 'expr opt ...)))))))

(define-syntax test-pp-json
  (syntax-rules ()
    ((_ (opt ...) expr str)
     (test* (format "keys=~S: ~A"
                    '(opt ...)
                    (with-output-to-string
                      (lambda ()
                        (write/ss 'expr))))
            str
            (with-output-to-string
              (lambda ()
                (pretty-print-json 'expr opt ...)))))))

(define-syntax test-pp-core
  (syntax-rules ()
    ((_ width expr str)
     (test* (format "width=~A: ~A"
                    width
                    (with-output-to-string
                      (lambda ()
                        (write/ss 'expr))))
            str
            (call-with-output-string
              (cut pp-write expr width <>))))))

(test-section "pretty-print core")

(test-pp-core
 70
 (pp-group
  (pp-string "<number>") "1" (pp-string "</number>")
  (pp-break)
  (pp-string "<number>") "2" (pp-string "</number>"))
 "\
<number>1</number> <number>2</number>")

(test-pp-core
 70
 (pp-group
  (pp-zero-width-string "<number>") "1" (pp-zero-width-string "</number>")
  (pp-break)
  (pp-zero-width-string "<number>") "2" (pp-zero-width-string "</number>"))
 "\
<number>1</number> <number>2</number>")

(test-pp-core
 20
 (pp-group
  (pp-string "<number>") "1" (pp-string "</number>")
  (pp-break)
  (pp-string "<number>") "2" (pp-string "</number>"))
 "\
<number>1</number>
<number>2</number>")

(test-pp-core
 20
 (pp-group
  (pp-zero-width-string "<number>") "1" (pp-zero-width-string "</number>")
  (pp-break)
  (pp-zero-width-string "<number>") "2" (pp-zero-width-string "</number>"))
 "\
<number>1</number> <number>2</number>")

(test-pp-core
 70
 (pp-group "1" (pp-break) "2")
 "1 2")

(test-pp-core
 70
 (pp-group "1" (pp-newline) "2")
 "\
1
2")

(test-pp-core
 70
 (pp-group "1" (pp-newline) (pp-newline) "2")
 "\
1

2")

(test-pp-core
 70
 (pp-group "<"
           (pp-nest 1 "1" (pp-newline) "2")
           ">")
 "\
<1
 2>")

(test-pp-core
 70
 (pp-group "<"
           (pp-nest 1 "1" (pp-newline) (pp-newline) "2")
           ">")
 "\
<1

 2>")

;;-------+---------+---------+---------+---------|---------+---------+---------+

(test-section "pretty-print lists")

(test-pp (:width 70) (1 2 3 4 5 6 7) "(1 2 3 4 5 6 7)\n")

(test-pp (:width 8) (1 2 3 4 5 6 7) "\
(1
 2
 3
 4
 5
 6
 7)
")

;;-------+---------+---------+---------+---------|---------+---------+---------+

(test-section "pretty-print vectors")

(test-pp (:width 70) #(1 2 3 4 5 6 7) "#(1 2 3 4 5 6 7)\n")

(test-pp (:width 8) #(1 2 3 4 5 6 7) "\
#(1
  2
  3
  4
  5
  6
  7)
")

(test-pp (:width 10) #((1 2) (3 4) (5 6) (7 8)) "\
#((1 2)
  (3 4)
  (5 6)
  (7 8))
")

;;-------+---------+---------+---------+---------|---------+---------+---------+

(test-section "pretty-print programs")

(test-pp (:width 40)
         '#(1 ,(2 3) ,@(4 5 6) (7 '(8 9)))
         "'#(1 ,(2 3) ,@(4 5 6) (7 '(8 9)))\n")

(test-pp (:width 30)
         '#(1 ,(2 3) ,@(4 5 6) (7 '(8 9)))
         "\
'#(1
   ,(2 3)
   ,@(4 5 6)
   (7 '(8 9)))
")

(test-pp (:width 10)
         '#(1 ,(2 3) ,@(4 5 6) (7 '(8 9)))
         "\
'#(1
   ,(2 3)
   ,@(4
      5
      6)
   (7
    '(8
      9)))
")

(test-pp (:width 5)
         '#(1 ,(2 3) ,@(4 5 6) (7 '(8 9)))
         "\
'#(1
   ,(2
     3)
   ,@(4
      5
      6)
   (7
    '(8
      9)))
")

(test-pp (:width 30) (let ((a 1) (b 2)) (+ a b)) "(let ((a 1) (b 2)) (+ a b))\n")

(test-pp (:width 20)
         (let ((a 1) (b 2)) (+ a b)) "\
(let ((a 1) (b 2))
  (+ a b))
")

(test-pp (:width 15)
         (let ((a 1) (b 2)) (+ a b)) "\
(let ((a 1)
      (b 2))
  (+ a b))
")

(test-pp (:width 10)
         (let ((a 1) (b 2)) (+ a b)) "\
(let ((a
       1)
      (b
       2))
  (+ a b))
")

(test-pp (:width 70)
         (let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))
         "(let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))\n")

(test-pp (:width 50)
         (let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))
         "\
(let fib ((n 10) (a 0) (b 1))
  (if (= n 0) a (fib (- n 1) b (+ a b))))
")

(test-pp (:width 30)
         (let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))
         "\
(let fib ((n 10) (a 0) (b 1))
  (if (= n 0)
      a
      (fib
       (- n 1)
       b
       (+ a b))))
")

(test-pp (:width 25)
         (let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))
         "\
(let fib
    ((n 10) (a 0) (b 1))
  (if (= n 0)
      a
      (fib
       (- n 1)
       b
       (+ a b))))
")

(test-pp (:width 20)
         (let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))
         "\
(let fib
    ((n 10)
     (a 0)
     (b 1))
  (if (= n 0)
      a
      (fib
       (- n 1)
       b
       (+ a b))))
")

(test-pp (:width 60)
         (do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a) #f)
         "(do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a) #f)\n")

(test-pp (:width 50)
         (do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a) #f)
         "\
(do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b)))
    ((= n 0) a)
  #f)
")

(test-pp (:width 15)
         (do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a) #f)
         "\
(do ((n
      10
      (- n 1))
     (a 0 b)
     (b
      1
      (+ a b)))
    ((= n 0) a)
  #f)
")

(test-pp (:width 60)
         (do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a))
         "(do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a))\n")

(test-pp (:width 50)
         (do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a))
         "\
(do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b)))
    ((= n 0) a))
")

(test-pp (:width 15)
         (do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a))
         "\
(do ((n
      10
      (- n 1))
     (a 0 b)
     (b
      1
      (+ a b)))
    ((= n 0)
     a))
")

(test-pp (:width 30) (and 1 2 3) "(and 1 2 3)\n")

(test-pp (:width 5) (and 1 2 3) "\
(and 1
     2
     3)
")

;;-------+---------+---------+---------+---------|---------+---------+---------+

(test-section "pretty-print shared structures")

(test-pp (:width 20)
         #((1) (1))
         "#((1) (1))\n")
(test-pp (:width 20 :print-shared #t)
         #(#0=(1) #0#)
         "#(#0=(1) #0#)\n")
(test-pp (:width 5) #(#0=(1) #0#) "\
#((1)
  (1))
")
(test-pp (:width 5 :print-shared #t) #(#0=(1) #0#) "\
#(#0=(1)
  #0#)
")

(test-pp (:width 20) #0=(#0#) "#0=(#0#)\n")

(test-pp (:width 30) #0=(#0# #0# #0# #0# #0#) "#0=(#0# #0# #0# #0# #0#)\n")
(test-pp (:width 10) #0=(#0# #0# #0# #0# #0#) "\
#0=(#0#
    #0#
    #0#
    #0#
    #0#)
")

(test-pp (:width 30) #0=(1 . #0#) "#0=(1 . #0#)\n")

(test-pp (:width 30) (1 . #0=(2 . #0#)) "(1 . #0=(2 . #0#))\n")

(test-pp (:width 10) (1 . #0=(2 . #0#)) "\
(1
 .
 #0=(2
     .
     #0#))
")

(test-pp (:width +inf.0) #0='#0# "#0='#0#\n")

(test-pp (:width +inf.0 :print-shared #t)
         #0=(((#1=(#0# #0# z) #1# y) . #2=(#1# #0# . x)) #2#)
         "#0=(((#1=(#0# #0# z) #1# y) . #2=(#1# #0# . x)) #2#)\n")

(test-pp () (#0=a #0#) "(a a)\n")
(test-pp () (#0=#:a #0#) "(#:a #:a)\n")

(test-pp (:print-shared #t) (#0=a #0#) "(a a)\n")
(test-pp (:print-shared #t) (#0=#:a #0#) "(#0=#:a #0#)\n")

(test-pp (:width +inf.0 :print-shared #t)
         (#0=(a) #1=(b) #2=(c) #3=(d) #4=(e) #5=(f) #6=(g) #7=(h) #8=(i) #9=(j) #10=(k) #10# #9# #8# #7# #6# #5# #4# #3# #2# #1# #0#)
         "(#0=(a) #1=(b) #2=(c) #3=(d) #4=(e) #5=(f) #6=(g) #7=(h) #8=(i) #9=(j) #10=(k) #10# #9# #8# #7# #6# #5# #4# #3# #2# #1# #0#)\n")

;;-------+---------+---------+---------+---------|---------+---------+---------+

(test-section "pretty-print json")

(test-pp-json (:width 2) #() "\
[]
")

(test-pp-json (:width 1) #() "\
[
]
")

(test-pp-json (:width 2) () "\
{}
")

(test-pp-json (:width 1) () "\
{
}
")

(test-pp-json () #(1 2 3 4) "[1, 2, 3, 4]\n")

(test-pp-json (:indent #f) #(1 2 3 4) "[1,2,3,4]\n")

(test-pp-json (:width 5) #(1 2 3 4) "\
[
  1,
  2,
  3,
  4
]\n")

(test-pp-json (:indent #f :width 1) #(1 2 3 4) "[1,2,3,4]\n")

(test-pp-json (:width 5)
              (("a" . 42)
               ("b" . 0.5))
              "\
{
  \"a\": 42,
  \"b\": 0.5
}
")

(test-pp-json (:indent #f :width 5)
              (("a" . 42)
               ("b" . 0.5))
              "{\"a\":42,\"b\":0.5}\n")

(test-pp-json (:width 20)
              (("a" . 42)
               ("b" . #(1 2 3 4 5)))
              "\
{
  \"a\": 42,
  \"b\": [
    1,
    2,
    3,
    4,
    5
  ]
}
")

(test-pp-json (:width 20)
              (("a" . 42)
               ("b" . #(1 2 3)))
              "\
{
  \"a\": 42,
  \"b\": [1, 2, 3]
}
")
;;-------+---------+---------+---------+---------|---------+---------+---------+

(test-pp-json ()
              "λ\nλ"
              "\"λ\\nλ\"\n")


;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)

;; Local Variables:
;; open-paren-in-column-0-is-defun-start: nil
;; End:

