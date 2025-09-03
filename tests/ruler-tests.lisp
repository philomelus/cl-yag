(in-package :cl-yag-tests)

(defstruct (ruler-tests-data (:include tests-data)
                             (:conc-name ruler-tests-))
  r1 r2 r3 r4 r5 r6 r7 r8
  w1 w2 w3 w4 w5 w6 w7 w8
  )

(defparameter *ruler-data* (make-ruler-tests-data))

(defmethod tests-create ((data (eql *ruler-data*)))
  (let (widgets)
    (with-slots (manager
                 r1 r2 r3 r4 r5 r6 r7 r8
                 w1 w2 w3 w4 w5 w6 w7 w8)
        data
      (let ((c1 (al:map-rgb-f 0 0.75 0))
            (c2 (al:map-rgb-f 0 0 1)))
        ;; Test 1
        (setf r1 (defruler :left (+ +W1X+ 10) :top (+ +W1Y+ 10) :height 10 :width (- +W1W+ 20)
                           :major-color c1 :minor-color c2 :color c1 :visible t))
        (setf r5 (defruler :left (+ +W1X+ 10) :top (+ +W1Y+ 50) :height 25 :width (- +W1W+ 20)
                           :major-color c1 :minor-color c2 :color c1 :visible t))
        (setf w1 (defwindow +W1X+ +W1Y+ +W1W+ +W1H+ :content `(,r1 ,r5)))
        (push w1 widgets)
      
        ;; Test 2
        (setf r2 (defruler :left (+ +W2X+ 10) :top (+ +W2Y+ 10) :height (- +W2H+ 20) :width 10
                           :major-color c1 :minor-color c2 :color c1 :visible t
                           :vertical t))
        (setf r6 (defruler :left (+ +W2X+ 50) :top (+ +W2Y+ 10) :height (- +W2H+ 20) :width 25
                           :major-color c1 :minor-color c2 :color c1 :visible t
                           :vertical t))
        (setf w2 (defwindow +W2X+ +W2Y+ +W2W+ +W2H+ :content `(,r2 ,r6)))
        (push w2 widgets)
      
        ;; Test 3
        (setf r3 (defruler :left (+ +W3X+ 10) :top (+ +W3Y+ 10) :height 10 :width (- +W3W+ 20)
                           :major-color c1 :minor-color c2 :color c1 :visible t
                           :major 25 :minor 5))
        (setf r7 (defruler :left (+ +W3X+ 10) :top (+ +W3Y+ 50) :height 25 :width (- +W3W+ 20)
                           :major-color c1 :minor-color c2 :color c1 :visible t
                           :major 25 :minor 5))
        (setf w3 (defwindow +W3X+ +W3Y+ +W3W+ +W3H+ :content `(,r3 ,r7)))
        (push w3 widgets)
      
        ;; Test 4
        (setf r4 (defruler :left (+ +W4X+ 10) :top (+ +W4Y+ 10) :height (- +W4H+ 20) :width 10
                           :major-color c1 :minor-color c2 :color c1 :visible t
                           :vertical t
                           :major 25 :minor 5))
        (setf r8 (defruler :left (+ +W4X+ 50) :top (+ +W4Y+ 10) :height (- +W4H+ 20) :width 25
                           :major-color c1 :minor-color c2 :color c1 :visible t
                           :vertical t
                           :major 25 :minor 5))
        (setf w4 (defwindow +W4X+ +W4Y+ +W4W+ +W4H+ :content `(,r4 ,r8)))
        (push w4 widgets)
      
        ;; Test 5
        ;; (setf w5 (defwindow +W5X+ +W5Y+ +W5W+ +W5H+))
        ;; (push w5 widgets)
      
        ;; Test 6
        ;; (setf w6 (defwindow +W6X+ +W6Y+ +W6W+ +W6H+))
        ;; (push w6 widgets)
      
        ;; Test 7
        ;; (setf w7 (defwindow +W7X+ +W7Y+ +W7W+ +W7H+))
        ;; (push w7 widgets)
      
        ;; Test 8
        ;; (setf w8 (defwindow +W8X+ +W8Y+ +W8W+ +W8H+))
        ;; (push w8 widgets)
        )      
      ;; Instructions
      (mapcar #'(lambda (o) (push o widgets))
              (multiple-value-list (tests-instructions-create
                                    data
                                    (list "<1> - align begin/middle/end"
                                          "<2>"
                                          "<3>"
                                          "<4> - theme-flat/theme-3d")
                                    (list "<5>"
                                          "<6>"
                                          "window interior color red/default - <7>"
                                          "<8>"))))

      ;; The one in charge
      (setf manager (make-instance 'manager :content widgets)))))

(defmethod tests-destroy ((data (eql *ruler-data*)))
  (cleanup-method tests-command-1 `((eql ,data)))
  (cleanup-method tests-command-4 `((eql ,data)))
  (cleanup-method tests-command-7 `((eql ,data)))
  nil)

(defmethod tests-ready ((ruler-data (eql *ruler-data*)))
  (defmethod tests-command-1 ((data (eql ruler-data)))
    (with-slots (r1 r2 r3 r4 r5 r6 r7 r8) data
      (dolist (o `(,r1 ,r2 ,r3 ,r4 ,r5 ,r6 ,r7 ,r8))
        (with-slots (cl-yag:align) o
          (case align
            (:begin
             (setf align :middle))
            (:middle
             (setf align :end))
            (:end
             (setf align :begin)))))))
  
  (defmethod tests-command-4 ((data (eql ruler-data)))
    (tests-toggle-theme data)
    nil)
  
  (defmethod tests-command-7 ((data (eql ruler-data)))
    (with-slots (w1 w2 w3 w4) data
      (tests-toggle-interior-color data (list w1 w2 w3 w4)))
    nil)
  nil)

(defun ruler-tests-main ()
  (tests-main *ruler-data*))

