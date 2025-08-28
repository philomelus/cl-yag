(in-package :cl-yag-tests)

(defstruct (box-tests-data (:include tests-data)
                           (:conc-name box-tests-))
  b1
  cl1
  m
  t1
  w1)

(defparameter *box-data* (make-box-tests-data))

(defmethod tests-create ((data (eql *box-data*)))
  (with-slots (manager
               b1
               cl1
               t1
               w1)
      data

    ;; Test 1
    (setf t1 (deftext :title "Test 1" :height :auto-min :h-align :center :v-align :middle))
    (setf b1 (defbox :left 35 :top 35 :width 180 :height 280))
    (setf cl1 (defcolumn-layout :content (list (list t1 :min-height) b1)))
    (setf w1 (defwindow 25 25 200 300 :content (list cl1) :interior-color (al:map-rgb-f 1 0 0)))

    ;; The one in charge
    (setf manager (make-instance 'manager :content (list w1))))
  )

(defmethod tests-destroy ((data (eql *box-data*)))
  )

(defun box-tests-main ()
 (tests-main *box-data*))

