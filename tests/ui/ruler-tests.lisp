(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defstruct (ruler-tests-data (:include tests-data)
                             (:conc-name ruler-tests-))
  r11 r12
  r21 r22
  r31 r32
  r41 r42
  w1 w2 w3 w4 w5 w6 w7 w8
  )

(defparameter *ruler-data* (make-ruler-tests-data))

(defmethod tests-create ((data (eql *ruler-data*)))
  (let (widgets)
    (with-slots (manager
                 r11 r12
                 r21 r22
                 r31 r32
                 r41 r42
                 w1 w2 w3 w4 w5 w6 w7 w8)
        data
      (let ((c1 (al:map-rgb-f 0 0.75 0))
            (c2 (al:map-rgb-f 0 0 1)))
        ;; Test 1
        (setf r11 (ruler-10-2 :left (+ +W1L+ 10) :top (+ +W1T+ 10) :height 10 :width (- +W1W+ 20)
                              :div-10-color c1 :div-10-extent 1
                              :div-2-color c2
                              :line-color c1 :visible t))
        (setf r12 (ruler-10-2 :left (+ +W1L+ 10) :top (+ +W1T+ 50) :height 25 :width (- +W1W+ 20)
                              :div-10-color c1 :div-10-extent 1
                              :div-2-color c2
                              :line-color c1 :visible t))
        (setf w1 (deftests-window :standard 1 :content `(,r11 ,r12)))
        (push w1 widgets)
      
        ;; Test 2
        (setf r21 (ruler-10-2 :left (+ +W2L+ 10) :top (+ +W2T+ 10) :height (- +W2H+ 20) :width 10
                              :div-10-color c1 :div-10-extent 1
                              :div-2-color c2
                              :line-color c1 :visible t :vertical t))
        (setf r22 (ruler-10-2 :left (+ +W2L+ 50) :top (+ +W2T+ 10) :height (- +W2H+ 20) :width 25
                              :div-10-color c1 :div-10-extent 1
                              :div-2-color c2
                              :line-color c1 :visible t :vertical t))
        (setf w2 (deftests-window :standard 2 :content `(,r21 ,r22)))
        (push w2 widgets)
      
        ;; Test 3
        (setf r31 (ruler-25-5 :left (+ +W3L+ 10) :top (+ +W3T+ 10) :height 10 :width (- +W3W+ 20)
                              :div-25-color c1 :div-25-extent 1
                              :div-5-color c2
                              :line-color c1 :visible t))
        (setf r32 (ruler-25-5 :left (+ +W3L+ 10) :top (+ +W3T+ 50) :height 25 :width (- +W3W+ 20)
                              :div-25-color c1 :div-25-extent 1
                              :div-5-color c2
                              :line-color c1 :visible t))
        (setf w3 (deftests-window :standard 3 :content `(,r31 ,r32)))
        (push w3 widgets)
      
        ;; Test 4
        (setf r41 (ruler-25-5 :left (+ +W4L+ 10) :top (+ +W4T+ 10) :height (- +W4H+ 20) :width 10
                              :div-25-color c1 :div-25-extent 1
                              :div-5-color c2
                              :line-color c1 :visible t :vertical t))
        (setf r42 (ruler-25-5 :left (+ +W4L+ 50) :top (+ +W4T+ 10) :height (- +W4H+ 20) :width 25
                              :div-25-color c1 :div-25-extent 1
                              :div-5-color c2
                              :line-color c1 :visible t :vertical t))
        (setf w4 (deftests-window :standard 4 :content `(,r41 ,r42)))
        (push w4 widgets))
      
      ;; Instructions
      (mapcar #'(lambda (o) (push o widgets))
              (multiple-value-list (tests-instructions-create
                                    data
                                    (list "1 = align begin/middle/end"
                                          ""
                                          ""
                                          "")
                                    (list ""
                                          ""
                                          "c = window interior color red/default"
                                          "t = theme-flat/theme-3d"))))

      ;; The one in charge
      (setf manager (make-instance 'manager :content widgets)))))

(defmethod tests-destroy ((data (eql *ruler-data*)))
  (let ((args `((eql ,data))))
    (cleanup-method tests-command-1 args)
    (cleanup-method tests-get-interior-color args)))

(defmethod tests-ready ((ruler-data (eql *ruler-data*)))
  (defmethod tests-command-1 ((data (eql ruler-data)))
    (with-slots (r11 r12 r21 r22 r31 r32 r41 r42) data
      (dolist (o `(,r11 ,r12 ,r21 ,r22 ,r31 ,r32 ,r41 ,r42))
        (with-slots (cl-yag:align) o
          (case align
            (:begin
             (setf align :middle))
            (:middle
             (setf align :end))
            (:end
             (setf align :begin)))))))
  
  (defmethod tests-get-interior-color ((data (eql ruler-data)))
    (with-slots (w1 w2 w3 w4) data
      (values (list w1 w2 w3 w4) nil))))

(defun ruler-tests-main ()
  (tests-main *ruler-data*))

