(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

;; grid-layout automated tests

(def-suite grid-layout :description "Automated tests for grid-layout.")
(in-suite grid-layout)

(test cells ()
  "Tests changing cell slots."
  (let ((gl (defgrid-layout :columns 2 :rows 2)))
    (is (eql (border-left (layout-cell gl :column 0 :row 0)) nil))
    (is (eql (slot-value (layout-cell gl :column 0 :row 0) 'spacing-left) 0))
    (setf (slot-value (layout-cell gl :column 0 :row 0) 'spacing-left) 25)
    (is (= (slot-value (layout-cell gl :column 0 :row 0) 'spacing-left) 25))
    (is (= (slot-value (layout-cell gl :column 0 :row 0) 'spacing-right) 0))
    ;; (grid-layout-cell-set 25 'height 1 0 gl :recalc nil)
    ;; (is (= (slot-value (grid-layout-cell 1 0 gl) 'height) 25))
    ;; (grid-layout-cell-set 23 '(height width) 1 1 gl :recalc nil)
    ;; ;; Udpated?
    ;; (is (= (slot-value (grid-layout-cell 1 1 gl) 'width) 23))
    ;; ;; Updated?
    ;; (is (= (slot-value (grid-layout-cell 1 1 gl) 'height) 23))
    ;; ;; Not updated?
    ;; (is (equal (slot-value (grid-layout-cell 0 1 gl) 'width) :auto))
    ;; ;; Not updated?
    ;; (is (equal (slot-value (grid-layout-cell 0 1 gl) 'height) :auto))
    ))

(test columns ()
  "Tests changing columns of cells slots."
  ;; (let ((gl (defgrid-layout :columns 4 :rows 4)))
  ;;   ;; At default values
  ;;   (is (eql (border-left (grid-layout-cell 0 0 gl)) nil))
  ;;   (is (eql (slot-value (grid-layout-cell 0 0 gl) 'left) :auto))
  ;;   ;; Change column height
  ;;   (grid-layout-column-cells-set 10 'height 1 gl :recalc nil)
  ;;   (is (= (slot-value (grid-layout-cell 1 0 gl) 'height) 10))
  ;;   (is (= (slot-value (grid-layout-cell 1 1 gl) 'height) 10))
  ;;   (is (= (slot-value (grid-layout-cell 1 2 gl) 'height) 10))
  ;;   (is (= (slot-value (grid-layout-cell 1 3 gl) 'height) 10))
  ;;   ;; Make sure others are not affected
  ;;   (is (equal (slot-value (grid-layout-cell 0 2 gl) 'height) :auto))
  ;;   (is (equal (slot-value (grid-layout-cell 2 0 gl) 'height) :auto))
  ;;   (is (equal (slot-value (grid-layout-cell 3 3 gl) 'height) :auto))
  ;;   ;; Set height and width of column
  ;;   (grid-layout-column-cells-set 99 '(height width) 1 gl :recalc nil)
  ;;   (is (= (slot-value (grid-layout-cell 1 0 gl) 'width) 99))
  ;;   (is (= (slot-value (grid-layout-cell 1 0 gl) 'height) 99))
  ;;   (is (= (slot-value (grid-layout-cell 1 1 gl) 'width) 99))
  ;;   (is (= (slot-value (grid-layout-cell 1 1 gl) 'height) 99))
  ;;   (is (= (slot-value (grid-layout-cell 1 2 gl) 'width) 99))
  ;;   (is (= (slot-value (grid-layout-cell 1 2 gl) 'height) 99))
  ;;   (is (= (slot-value (grid-layout-cell 1 3 gl) 'width) 99))
  ;;   (is (= (slot-value (grid-layout-cell 1 3 gl) 'height) 99))
  ;;   ;; Not affected
  ;;   (is (equal (slot-value (grid-layout-cell 0 2 gl) 'height) :auto))
  ;;   (is (equal (slot-value (grid-layout-cell 2 0 gl) 'height) :auto))
  ;;   (is (equal (slot-value (grid-layout-cell 3 3 gl) 'height) :auto))
  ;;   )
  )

(test rows ()
  "Tests changing rows of cells slots."
  ;; (let ((gl (defgrid-layout :columns 4 :rows 4)))
  ;;   ;; At default values
  ;;   (is (eql (border-left (grid-layout-cell 0 0 gl)) nil))
  ;;   (is (eql (slot-value (grid-layout-cell 0 0 gl) 'width) :auto))
  ;;   ;; Change row width
  ;;   (grid-layout-row-cells-set 23 'width 2 gl :recalc nil)
  ;;   (is (= (slot-value (grid-layout-cell 0 2 gl) 'width) 23))
  ;;   (is (= (slot-value (grid-layout-cell 1 2 gl) 'width) 23))
  ;;   (is (= (slot-value (grid-layout-cell 2 2 gl) 'width) 23))
  ;;   (is (= (slot-value (grid-layout-cell 3 2 gl) 'width) 23))
  ;;   ;; Make sure others are not affected
  ;;   (is (equal (slot-value (grid-layout-cell 1 0 gl) 'width) :auto))
  ;;   (is (equal (slot-value (grid-layout-cell 0 1 gl) 'width) :auto))
  ;;   (is (equal (slot-value (grid-layout-cell 3 3 gl) 'width) :auto))
  ;;   ;; Change row height and width
  ;;   (grid-layout-row-cells-set 66 '(height width) 2 gl :recalc nil)
  ;;   (is (= (slot-value (grid-layout-cell 0 2 gl) 'width) 66))
  ;;   (is (= (slot-value (grid-layout-cell 0 2 gl) 'height) 66))
  ;;   (is (= (slot-value (grid-layout-cell 1 2 gl) 'width) 66))
  ;;   (is (= (slot-value (grid-layout-cell 1 2 gl) 'height) 66))
  ;;   (is (= (slot-value (grid-layout-cell 2 2 gl) 'width) 66))
  ;;   (is (= (slot-value (grid-layout-cell 2 2 gl) 'height) 66))
  ;;   (is (= (slot-value (grid-layout-cell 3 2 gl) 'width) 66))
  ;;   (is (= (slot-value (grid-layout-cell 3 2 gl) 'height) 66))
  ;;   ;; Make sure others are not affected
  ;;   (is (equal (slot-value (grid-layout-cell 1 0 gl) 'width) :auto))
  ;;   (is (equal (slot-value (grid-layout-cell 2 0 gl) 'height) :auto))
  ;;   (is (equal (slot-value (grid-layout-cell 0 1 gl) 'width) :auto))
  ;;   (is (equal (slot-value (grid-layout-cell 2 1 gl) 'height) :auto))
  ;;   (is (equal (slot-value (grid-layout-cell 3 3 gl) 'width) :auto))
  ;;   (is (equal (slot-value (grid-layout-cell 2 3 gl) 'height) :auto))
  ;;   )
  )

(defun run-grid-layout-tests ()
  (run! 'grid-layout))

