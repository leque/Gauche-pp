;;;
;;; Test pp
;;;

(use gauche.test)

(test-start "pp")
(use pp)
(test-module 'pp)

(define-syntax test-pp
  (syntax-rules ()
    ((_ width expr str)
     (test* (format "width=~A: ~A"
                    width
                    (with-output-to-string
                      (lambda ()
                        (write/ss 'expr))))
            str
            (with-output-to-string
              (lambda ()
                (pretty-print 'expr width)))))))

(test-section "pretty-print lists")

(test-pp 70 (1 2 3 4 5 6 7) "(1 2 3 4 5 6 7)\n")

(test-pp 8 (1 2 3 4 5 6 7) "\
\(1
 2
 3
 4
 5
 6
 7)
")

;;-------+---------+---------+---------+---------|---------+---------+---------+

(test-section "pretty-print vectors")

(test-pp 70 #(1 2 3 4 5 6 7) "#(1 2 3 4 5 6 7)\n")

(test-pp 8 #(1 2 3 4 5 6 7) "\
#(1
  2
  3
  4
  5
  6
  7)
")

(test-pp 10 #((1 2) (3 4) (5 6) (7 8)) "\
#((1 2)
  (3 4)
  (5 6)
  (7 8))
")

;;-------+---------+---------+---------+---------|---------+---------+---------+

(test-section "pretty-print programs")

(test-pp 40
         '#(1 ,(2 3) ,@(4 5 6) (7 '(8 9)))
         "'#(1 ,(2 3) ,@(4 5 6) (7 '(8 9)))\n")

(test-pp 30
         '#(1 ,(2 3) ,@(4 5 6) (7 '(8 9)))
         "\
'#(1
   ,(2 3)
   ,@(4 5 6)
   (7 '(8 9)))
")

(test-pp 10
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

(test-pp 5
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

(test-pp 30 (let ((a 1) (b 2)) (+ a b)) "(let ((a 1) (b 2)) (+ a b))\n")

(test-pp 20
         (let ((a 1) (b 2)) (+ a b)) "\
\(let ((a 1) (b 2))
  (+ a b))
")

(test-pp 15
         (let ((a 1) (b 2)) (+ a b)) "\
\(let ((a 1)
      (b 2))
  (+ a b))
")

(test-pp 10
         (let ((a 1) (b 2)) (+ a b)) "\
\(let ((a
       1)
      (b
       2))
  (+ a b))
")

(test-pp 70
         (let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))
         "(let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))\n")

(test-pp 50
         (let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))
         "\
\(let fib ((n 10) (a 0) (b 1))
  (if (= n 0) a (fib (- n 1) b (+ a b))))
")

(test-pp 30
         (let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))
         "\
\(let fib ((n 10) (a 0) (b 1))
  (if (= n 0)
      a
      (fib
       (- n 1)
       b
       (+ a b))))
")

(test-pp 25
         (let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))
         "\
\(let fib
    ((n 10) (a 0) (b 1))
  (if (= n 0)
      a
      (fib
       (- n 1)
       b
       (+ a b))))
")

(test-pp 20
         (let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))
         "\
\(let fib
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

(test-pp 60
         (do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a) #f)
         "(do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a) #f)\n")

(test-pp 50
         (do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a) #f)
         "\
\(do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b)))
    ((= n 0) a)
  #f)
")

(test-pp 15
         (do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a) #f)
         "\
\(do ((n
      10
      (- n 1))
     (a 0 b)
     (b
      1
      (+ a b)))
    ((= n 0) a)
  #f)
")

(test-pp 60
         (do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a))
         "(do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a))\n")

(test-pp 50
         (do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a))
         "\
\(do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b)))
    ((= n 0) a))
")

(test-pp 15
         (do ((n 10 (- n 1)) (a 0 b) (b 1 (+ a b))) ((= n 0) a))
         "\
\(do ((n
      10
      (- n 1))
     (a 0 b)
     (b
      1
      (+ a b)))
    ((= n 0)
     a))
")

(test-pp 30 (and 1 2 3) "(and 1 2 3)\n")

(test-pp 5 (and 1 2 3) "\
\(and 1
     2
     3)
")

;;-------+---------+---------+---------+---------|---------+---------+---------+

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
