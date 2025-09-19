(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defstruct (layout-tests-data (:include tests-data)
                            (:conc-name layout-tests-))
  w1 w2 w3 w4 w5 w6 w7 w8
  )

(defparameter *layout-data* (make-layout-tests-data))

(defmethod tests-create ((data (eql *layout-data*)))
  (let (widgets)
    (with-slots (manager w1 w2 w3 w4 w5 w6 w7 w8)
        data

      ;; Test 1
      (setf w1 (deftests-window :standard 1))
      (push w1 widgets)
      
      ;; Test 2
      (setf w2 (deftests-window :standard 2))
      (push w2 widgets)
      
      ;; Test 3
      (setf w3 (deftests-window :standard 3))
      (push w3 widgets)
      
      ;; Test 4
      (setf w4 (deftests-window :standard 4))
      (push w4 widgets)
      
      ;; Test 5
      (setf w5 (deftests-window :standard 5))
      (push w5 widgets)
      
      ;; Test 6
      (setf w6 (deftests-window :standard 6))
      (push w6 widgets)
      
      ;; Test 7
      (setf w7 (deftests-window :standard 7))
      (push w7 widgets)
      
      ;; Test 8
      (setf w8 (deftests-window :standard 8))
      (push w8 widgets)
      
      ;; Instructions
      (mapc #'(lambda (o) (push o widgets))
              (multiple-value-list (tests-instructions-create
                                    data
                                    (list ""
                                          ""
                                          ""
                                          "")
                                    (list ""
                                          ""
                                          "c = window interior red/default"
                                          "t = theme-flat/theme-3d"))))

      ;; Rulers
      (mapc #'(lambda (o) (push o widgets)) (multiple-value-list (tests-rulers-create-standard data)))
      
      ;; The one in charge
      (setf manager (make-instance 'manager :content widgets)))))

(defmethod tests-destroy ((data (eql *layout-data*)))
  (let ((args `((eql ,data))))
    (cleanup-method tests-get-interior-color args)))

(defmethod tests-ready ((layout-data (eql *layout-data*)))
  
  (defmethod tests-get-interior-color ((data (eql layout-data)))
    (with-slots (w1 w2 w3 w4 w5 w6 w7 w8) data
      (values (list w1 w2 w3 w4 w5 w6 w7 w8) nil))))

(defun layout-tests-main ()
  (tests-main *layout-data*))

