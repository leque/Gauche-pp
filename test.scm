;;;
;;; Test pp
;;;

(use gauche.test)

(test-start "pp")
(use pp)
(test-module 'pp)

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)




