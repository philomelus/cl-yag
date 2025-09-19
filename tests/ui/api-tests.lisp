(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defstruct (api-tests-data (:include tests-data)
                            (:conc-name api-tests-))
  w1 w2 w3 w4 w5 w6 w7 w8
  )

(defparameter *api-data-type* :standard)
;;(defparameter *api-data-type* :full)
;;(defparameter *api-data-type* :tall)
;;(defparameter *api-data-type* :wide)
;;(defparameter *api-data-type* :wide-tall)
(defparameter *api-data* (make-api-tests-data))

(defmethod tests-create ((data (eql *api-data*)))
  (let (widgets)
    (with-slots (manager w1 w2 w3 w4 w5 w6 w7 w8)
        data

      ;; Test 1
      (when (eql *api-data-type* :standard)
        (setf w1 (deftests-window :standard 1)))
      (when (eql *api-data-type* :full)
        (setf w1 (deftests-window :full 1)))
      (when (eql *api-data-type* :tall)
        (setf w1 (deftests-window :tall 1)))
      (when (eql *api-data-type* :wide)
        (setf w1 (deftests-window :wide 1)))
      (when (eql *api-data-type* :wide-tall)
        (setf w1 (deftests-window :wide-tall 1)))
      (push w1 widgets)
      
      ;; Test 2
      (when (eql *api-data-type* :standard)
        (setf w2 (deftests-window :standard 2)))
      (when (eql *api-data-type* :tall)
        (setf w2 (deftests-window :tall 2)))
      (when (eql *api-data-type* :wide)
        (setf w2 (deftests-window :wide 2)))
      (when (eql *api-data-type* :wide-tall)
        (setf w2 (deftests-window :wide-tall 2)))
      (unless (eql w2 nil)
        (push w2 widgets))
      
      ;; Test 3
      (when (eql *api-data-type* :standard)
        (setf w3 (deftests-window :standard 3)))
      (when (eql *api-data-type* :tall)
        (setf w3 (deftests-window :tall 3)))
      (when (eql *api-data-type* :wide)
        (setf w3 (deftests-window :wide 3)))
      (unless (eql w3 nil)
        (push w3 widgets))
      
      ;; Test 4
      (when (eql *api-data-type* :standard)
        (setf w4 (deftests-window :standard 4)))
      (when (eql *api-data-type* :tall)
        (setf w4 (deftests-window :tall 4)))
      (when (eql *api-data-type* :wide)
        (setf w4 (deftests-window :wide 4)))
      (unless (eql w4 nil)
        (push w4 widgets))
      
      ;; Test 5
      (when (eql *api-data-type* :standard)
        (setf w5 (deftests-window :standard 5)))
      (unless (eql w5 nil)
        (push w5 widgets))
      
      ;; Test 6
      (when (eql *api-data-type* :standard)
        (setf w6 (deftests-window :standard 6)))
      (unless (eql w6 nil)
        (push w6 widgets))
      
      ;; Test 7
      (when (eql *api-data-type* :standard)
        (setf w7 (deftests-window :standard 7)))
      (unless (eql w7 nil)
        (push w7 widgets))
      
      ;; Test 8
      (when (eql *api-data-type* :standard)
        (setf w8 (deftests-window :standard 8)))
      (unless (eql w8 nil)
        (push w8 widgets))
      
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
      (when (eql *api-data-type* :standard)
        (mapc #'(lambda (o) (push o widgets)) (multiple-value-list (tests-rulers-create-standard data))))
      (when (eql *api-data-type* :full)
        (mapc #'(lambda (o) (push o widgets)) (multiple-value-list (tests-rulers-create-full data))))
      (when (eql *api-data-type* :tall)
        (mapc #'(lambda (o) (push o widgets)) (multiple-value-list (tests-rulers-create-tall data))))
      (when (eql *api-data-type* :wide)
        (mapc #'(lambda (o) (push o widgets)) (multiple-value-list (tests-rulers-create-wide data))))
      (when (eql *api-data-type* :wide-tall)
        (mapc #'(lambda (o) (push o widgets)) (multiple-value-list (tests-rulers-create-wide-tall data))))
      
      ;; The one in charge
      (setf manager (make-instance 'manager :content widgets)))))

(defmethod tests-destroy ((data (eql *api-data*)))
  (let ((args `((eql ,data))))
    (cleanup-method tests-get-interior-color args))
  nil)

(defmethod tests-ready ((api-data (eql *api-data*)))
  (defmethod tests-get-interior-color ((data (eql api-data)))
    (with-slots (w1 w2 w3 w4 w5 w6 w7 w8) data
      (values (list w1 w2 w3 w4 w5 w6 w7 w8) nil))))

(defun api-tests-main ()
  (tests-main *api-data*))

