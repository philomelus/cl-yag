(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defstruct (window-tests-data (:include tests-data)
                            (:conc-name window-tests-))
  b1 t1 st11 st12 st13 st14 st15 st16 st17
  st21 st22 st23 st24
  w1 w2 w3 w4 w5 w6 w7 w8
  )

(defparameter *window-data* (make-window-tests-data))

(defmethod tests-create ((data (eql *window-data*)))
  (let (widgets)
    (with-slots (manager
                 b1 t1 st11 st12 st13 st14 st15 st16 st17
                 st21 st22 st23 st24
                 w1 w2 w3 w4 w5 w6 w7 w8
                 )
        data

      ;; Test 1
      (setf t1 (deftext :title "Title" :h-align :center :v-align :middle
                        :left (+ +W1L+ (/ (- +W1W+ 75) 2))
                        :top (+ +W1T+ (/ (- +W1H+ 25) 2))
                        :width 75 :height 25))
      (setf (border t1) (defborder :thickness 2))
      (setf b1 (defborder :thickness 10))
      (setf w1 (deftests-window :standard 1 :content `(,t1)))
      (setf (padding w1) 25)
      (setf (spacing w1) 25)
      (setf (border w1) b1)
      (push w1 widgets)
      
      ;; Test 2
      (setf w2 (deftests-window :standard 2))
      (push w2 widgets)
      
      ;; Status area (Test 3)
      (setf st11 (deftests-status :wide 2 5 2 "L/T: 0 0" 1 1))
      (setf st12 (deftests-status :wide 2 5 2 "W/H: 0 0" 1 2))
      (setf st13 (deftests-status :wide 2 5 2 "SH: 0 0" 2 1))
      (setf st14 (deftests-status :wide 2 5 2 "SV: 0 0" 2 2))
      (setf st15 (deftests-status :wide 2 5 2 "PH: 0 0" 3 1))
      (setf st16 (deftests-status :wide 2 5 2 "PV: 0 0" 3 2))
      (setf st17 (deftests-status :wide 2 5 2 "3D: DEFAULT" 4 1))
      (setf w3 (deftests-window :wide 2 :content `(,st11 ,st12 ,st13 ,st14 ,st15 ,st16 ,st17)))
      (push w3 widgets)
      
      ;; ;; Test 4
      ;; (setf w4 (deftests-window :standard 4))
      ;; (push w4 widgets)
      
      ;; ;; Test 5
      ;; (setf w5 (deftests-window :standard 5))
      ;; (push w5 widgets)
      
      ;; ;; Test 6
      ;; (setf w6 (deftests-window :standard 6))
      ;; (push w6 widgets)
      
      ;; ;; Test 7
      ;; (setf w7 (deftests-window :standard 7))
      ;; (push w7 widgets)
      
      ;; ;; Test 8
      ;; (setf w8 (deftests-window :standard 8))
      ;; (push w8 widgets)
      
      ;; Instructions
      (mapcar #'(lambda (o) (push o widgets))
              (multiple-value-list (tests-instructions-create
                                    data
                                    (list "s/S = Spacing +/-"
                                          ""
                                          ""
                                          "4 = :inset/:outset/:flat")
                                    (list ""
                                          ""
                                          "c = window interior red/default"
                                          "t = theme-flat/theme-3d"))))

      ;; Rulers
      (mapc #'(lambda (o) (push o widgets)) (multiple-value-list (tests-rulers-create-standard data)))
      
      ;; The one in charge
      (setf manager (make-instance 'manager :content widgets)))))

(defmethod tests-destroy ((data (eql *window-data*)))
  (let ((args `((eql ,data))))
    (cleanup-method tests-command-4 args)
    (cleanup-method tests-get-interior-color args)
    (cleanup-method tests-get-padding args)
    (cleanup-method tests-get-spacing args)))

(defmethod tests-ready ((window-data (eql *window-data*)))
  (defmethod tests-command-4 ((data (eql window-data)))
    (with-slots (manager theme2 b1 b2 b3 b4 b5 b6 b7) data
      (when (equal (theme manager) theme2)
        (with-slots (style) theme2
          (case style
            (:inset
             (setf style :outset))
            ((:outset :default)
             (setf style :flat))
            (:flat
             (setf style :inset))))))
    t)
  
  (defmethod tests-get-interior-color ((data (eql window-data)))
    (with-slots (w1 w2 w3 w4 w5 w6 w7 w8) data
      (values (list w1 w2 w3 w4 w5 w6 w7 w8) nil)))

  (defmethod tests-get-padding ((data (eql window-data)))
    (with-slots (t1) data
      (values (list t1) t)))
  
  (defmethod tests-get-spacing ((data (eql window-data)))
    (with-slots (w1 w2 w3 w4 w5 w6 w7 w8) data
      (values (list w1 w2) t)))
  
  (defmethod tests-command-update ((data (eql window-data)))
    (with-slots (st11 st12 st13 st14 st15 st16 st17 t1 w1 theme2) data
      (unless (eql st11 nil)
        (setf (title st11) (format nil "L/T: ~4d ~4d" (left w1) (top w1)))
        (setf (title st12) (format nil "W/H: ~4d ~4d" (width w1) (height w1)))
        (setf (title st13) (format nil "SH: ~4d ~4d" (spacing-left w1) (spacing-right w1)))
        (setf (title st14) (format nil "SV: ~4d ~4d" (spacing-top w1) (spacing-bottom w1)))
        (setf (title st15) (format nil "PH: ~4d ~4d" (padding-left t1) (padding-right t1)))
        (setf (title st16) (format nil "PV: ~4d ~4d" (padding-top t1) (padding-bottom t1)))
        (setf (title st17) (format nil "3D: ~a" (style theme2)))
        )))

  (tests-command-update window-data)
  nil)

(defun window-tests-main ()
  (tests-main *window-data*)
  nil)

