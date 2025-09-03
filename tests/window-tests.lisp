(in-package :cl-yag-tests)

(defstruct (window-tests-data (:include tests-data)
                            (:conc-name window-tests-))
  w1 w2 w3 w4 w5 w6 w7 w8
  )

(defparameter *window-data* (make-window-tests-data))

(defmethod tests-create ((data (eql *window-data*)))
  (let (widgets)
    (with-slots (manager w1 w2 w3 w4 w5 w6 w7 w8)
        data

      ;; Test 1
      (setf w1 (defwindow +W1X+ +W1Y+ +W1W+ +W1H+))
      (push w1 widgets)
      
      ;; Test 2
      (setf w2 (defwindow +W2X+ +W2Y+ +W2W+ +W2H+))
      (push w2 widgets)
      
      ;; Test 3
      (setf w3 (defwindow +W3X+ +W3Y+ +W3W+ +W3H+))
      (push w3 widgets)
      
      ;; Test 4
      (setf w4 (defwindow +W4X+ +W4Y+ +W4W+ +W4H+))
      (push w4 widgets)
      
      ;; Test 5
      (setf w5 (defwindow +W5X+ +W5Y+ +W5W+ +W5H+))
      (push w5 widgets)
      
      ;; Test 6
      (setf w6 (defwindow +W6X+ +W6Y+ +W6W+ +W6H+))
      (push w6 widgets)
      
      ;; Test 7
      (setf w7 (defwindow +W7X+ +W7Y+ +W7W+ +W7H+))
      (push w7 widgets)
      
      ;; Test 8
      (setf w8 (defwindow +W8X+ +W8Y+ +W8W+ +W8H+))
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

(defmethod tests-destroy ((data (eql *window-data*)))
  (remove-method #'tests-command-4 (find-method #'tests-command-4 () (list (list 'eql data))))
  (remove-method #'tests-command-7 (find-method #'tests-command-7 () (list (list 'eql data))))
  nil)

(defmethod tests-ready ((window-data (eql *window-data*)))
  (defmethod tests-command-4 ((data (eql window-data)))
    (tests-toggle-theme data)
    nil)
  
  (defmethod tests-command-7 ((data (eql window-data)))
    (with-slots (w1 w2 w3 w4 w5 w6 w7 w8) data
      (tests-toggle-interior-color data (list w1 w2 w3 w4 w5 w6 w7 w8)))
    nil)
  nil)

(defun window-tests-main ()
  (tests-main *window-data*))

