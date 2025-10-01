(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

;; theme automated tests

(def-suite theme :description "Automated tests for THEME.")
(in-suite theme)

;; (test cells ()
;;   "Tests something important
;;    (let ((gl (defgrid-layout :columns 2 :rows 2)))
;;     (is (eql (border-left (grid-layout-cell 0 0 gl)) nil)))
;;     )

(test with-theme-let
  "Tests WITH-THEME-LET."
  )

(defun run-theme-tests ()
  (run! 'theme))

;; (defun foo ()
;;   (let ((flubber (defborder)))
;;     (with-theme-let (ten1
;;                      ((one1))
;;                      ((one2 two1))
;;                      ((one3 two2 three1))
;;                      (ten2 one4)
;;                      (ten3 (one5 two3))
;;                      (ten4 (one6 two4 three2)))
;;                     flubber
;;       (format t "~a" (list ten1 ten2 ten3 ten4 one1 one2-two1 one3-two2-three1)))))

