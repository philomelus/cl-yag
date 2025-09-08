(in-package :cl-yag-tests)

(defstruct (grid-layout-tests-data (:include tests-data)
                            (:conc-name grid-layout-tests-))
  w1 w2 w3 w4 w5 w6 w7 w8
  )

(defparameter *grid-layout-data* (make-grid-layout-tests-data))

(defmethod tests-create ((data (eql *grid-layout-data*)))
  (let (widgets)
    (with-slots (manager w1 w2 w3 w4 w5 w6 w7 w8)
        data

      ;; Test 1
      (setf w1 (deftests-window :wide-tall 1))
      (push w1 widgets)
      
      ;; Test 2
      (setf w2 (deftests-window :wide-tall 2))
      (push w2 widgets)
      
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

(defmethod tests-destroy ((data (eql *grid-layout-data*)))
  (remove-method #'tests-command-4 (find-method #'tests-command-4 () (list (list 'eql data))))
  (remove-method #'tests-command-7 (find-method #'tests-command-7 () (list (list 'eql data))))
  nil)

(defmethod tests-ready ((grid-layout-data (eql *grid-layout-data*)))
  (defmethod tests-command-4 ((data (eql grid-layout-data)))
    (tests-toggle-theme data)
    nil)
  
  (defmethod tests-command-7 ((data (eql grid-layout-data)))
    (with-slots (w1 w2 w3 w4 w5 w6 w7 w8) data
      (tests-toggle-interior-color data (list w1 w2 w3 w4 w5 w6 w7 w8)))
    nil)
  nil)

(defun grid-layout-tests-main ()
  (tests-main *grid-layout-data*))

