(in-package :cl-yag-tests)

(defstruct (grid-tests-data (:include tests-data)
                            (:conc-name grid-tests-))
  w1 w2 w3 w4 w5 w6 w7 w8
  )

(defparameter *grid-data* (make-grid-tests-data))

(defmethod tests-create ((data (eql *grid-data*)))
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
      (mapcar #'(lambda (o) (push o widgets))
              (multiple-value-list (tests-instructions-create
                                    data
                                    (list "<1>"
                                          "<2>"
                                          "<3>"
                                          "<4> - alternates theme-flat/theme-3d")
                                    (list "<5>"
                                          "<6>"
                                          "window interior red/default - <7>"
                                          "<8>"))))

      ;; Rulers
      (mapcar #'(lambda (o) (push o widgets)) (multiple-value-list (tests-rulers-create data t t)))
      
      ;; The one in charge
      (setf manager (make-instance 'manager :content widgets)))))

(defmethod tests-destroy ((data (eql *grid-data*)))
  (remove-method #'tests-command-4 (find-method #'tests-command-4 () (list (list 'eql data))))
  (remove-method #'tests-command-7 (find-method #'tests-command-7 () (list (list 'eql data))))
  nil)

(defmethod tests-ready ((grid-data (eql *grid-data*)))
  (defmethod tests-command-4 ((data (eql grid-data)))
    (tests-toggle-theme data)
    nil)
  
  (defmethod tests-command-7 ((data (eql grid-data)))
    (with-slots (w1 w2 w3 w4 w5 w6 w7 w8) data
      (tests-toggle-interior-color data (list w1 w2 w3 w4 w5 w6 w7 w8)))
    nil)
  nil)

(defun grid-tests-main ()
  (tests-main *grid-data*))
