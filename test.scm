;;;
;;; Test pp
;;;

(use gauche.test)

(test-start "pp")
(use pp)
(test-module 'pp)

(use pp.core)
(test-module 'pp.core)

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

(test-pp (:width +inf.0 :print-shared #t)
         (#0=(a) #1=(b) #2=(c) #3=(d) #4=(e) #5=(f) #6=(g) #7=(h) #8=(i) #9=(j) #10=(k) #10# #9# #8# #7# #6# #5# #4# #3# #2# #1# #0#)
         "(#0=(a) #1=(b) #2=(c) #3=(d) #4=(e) #5=(f) #6=(g) #7=(h) #8=(i) #9=(j) #10=(k) #10# #9# #8# #7# #6# #5# #4# #3# #2# #1# #0#)\n")

;;-------+---------+---------+---------+---------|---------+---------+---------+

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)

;; Local Variables:
;; open-paren-in-column-0-is-defun-start: nil
;; End:

