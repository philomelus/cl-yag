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

(defun run-theme-tests ()
  (run! 'theme))

