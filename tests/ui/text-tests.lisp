(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defstruct (text-tests-data (:include tests-data)
                            (:conc-name text-tests-))
  bt11 bt12 bt13 b1 cl1
  bt21 bt22 bt23 t2 cl2
  bt31 bt32 bt33 t3 cl31 cl32
  bt41 bt42 bt43 t4 b41 b42 cl4
  t5 b5 cl5
  a5
  w1 w2 w3 w4 w5 w6
  )

(defparameter *text-data* (make-text-tests-data))

(defmethod tests-create ((data (eql *text-data*)))
  (let (widgets)
    (with-slots (manager
                 bt11 bt12 bt13 b1 cl1
                 bt21 bt22 bt23 t2 cl2
                 bt31 bt32 bt33 t3 cl31 cl32
                 bt41 bt42 bt43 b41 b42 t4 cl4
                 t5 b5 x1 cl5
                 w1 w2 w3 w4 w5 w6
                 )
        data

      ;; Test 1
      (setf bt11 (defbutton :title "T 1 / O 1" :h-align :center :v-align :middle))
      (setf bt12 (defbutton :title "T 1 / O 2" :h-align :center :v-align :middle))
      (setf bt13 (defbutton :title "T 1 / O 3" :h-align :center :v-align :middle))
      (setf cl1 (defcolumn-layout :content (list bt11 bt12 bt13)))
      (setf w1 (deftests-window :standard 1 :content (list cl1)))
      (push w1 widgets)
      
      ;; Test 2
      (setf t2 (deftext :title "T 2 Title" :h-align :center :height :auto-min))
      (setf bt21 (defbutton :title "T 2 / O 1" :h-align :center :v-align :middle))
      (setf bt22 (defbutton :title "T 2 / O 2" :h-align :center :v-align :middle))
      (setf bt23 (defbutton :title "T 2 / O 3" :h-align :center :v-align :middle))
      (setf cl2 (defcolumn-layout :content (list (list t2 :min-height) bt21 bt22 bt23)))
      (setf w2 (deftests-window :standard 2 :content (list cl2)))
      (push w2 widgets)
      
      ;; Test 3
      (setf b1 (defborder :thickness 2))
      (setf t3 (deftext :title "center/auto-min a really long title to see what happens" :h-align :center :height :auto-min
                        :padding-bottom 5
                        :border-bottom b1))
      (setf bt31 (defbutton :title "T 3 / O 1" :h-align :center :v-align :middle :height :auto-min
                            :width (- +W3W+ 24)
                            :left :center :top :middle
                            :padding-top 10 :padding-bottom 10))
      (setf bt32 (defbutton :title "T 3 / O 2" :h-align :center :v-align :middle :height :auto-min
                            :width (- +W3W+ 24)
                            :left :center :top :middle
                            :padding-top 10 :padding-bottom 10))
      (setf bt33 (defbutton :title "T 3 / O 3" :h-align :center :v-align :middle :height :auto-min
                            :width (- +W3W+ 24)
                            :left :center :top :middle
                            :padding-top 10 :padding-bottom 10))
      (setf cl31 (defcolumn-layout :content (list bt31 bt32 bt33)))
      (setf cl32 (defcolumn-layout :content (list (list t3 :min-height) cl31)))
      (setf w3 (deftests-window :standard 3 :content (list cl32)))
      (push w3 widgets)
      
      ;; Test 4
      (setf b41 (defborder :thickness 2))
      (setf t4 (deftext :title "center/auto-min" :h-align :center :height :auto-min
                        :padding-bottom 5
                        :border-bottom b41
                        :font (cl-yag:default-mono-font -20)))
      (setf b42 (defborder :thickness 2))
      (setf bt41 (defbutton :title "T 4 / O 1" :h-align :center :v-align :middle :height :auto-min
                            :width (- +W4W+ 24)
                            :left :center :top :bottom
                            :padding-top 10 :padding-bottom 10
                            :border-left b42 :border-right b42 :border-top b42 :border-bottom b42
                            :hover-color (al:map-rgb-f 0 1 1)))
      (setf bt42 (defbutton :title "T 4 / O 2" :h-align :center :v-align :middle :height :auto-min
                            :width (- +W4W+ 24)
                            :left :center :top :middle
                            :padding-top 10 :padding-bottom 10))
      (setf bt43 (defbutton :title "T 4 / O 3" :h-align :center :v-align :middle :height :auto-min
                            :width (- +W4W+ 24)
                            :left :center :top :top
                            :padding-top 10 :padding-bottom 10))
      (setf cl4 (defcolumn-layout :content (list (list t4 :min-height) bt41 bt42 bt43)))
      (setf w4 (deftests-window :standard 4 :content (list cl4)))
      (push w4 widgets)

      ;; Test 5
      (setf b5 (defborder :thickness 5))
      (setf t5 (deftext :title "Spacing/Padding Test" :h-align :center :v-align :middle))
      (setf (border t5) b5)
      ;; (setf (padding t5) 25)
      ;; (setf (spacing t5) 25)
      ;; (setf (padding-right t5) 25)
      (setf (padding-left t5) 25)
      ;; (setf (padding-top t5) 25)
      ;; (setf (padding-bottom t5) 25)
      ;; (setf (spacing-right t5) 25)
      ;; (setf (padding-left t5) 25)
      ;; (setf (spacing-top t5) 25)
      ;; (setf (spacing-bottom t5) 25)
      (setf cl5 (defcolumn-layout :content `(,t5)))
      (setf w5 (deftests-window :wide 3 :content `(,cl5)))
      (push w5 widgets)

      ;; Status on wide window 4
      (setf w6 (deftests-window :wide 4))
      (push w6 widgets)
      
      ;; Instructions
      (mapcar #'(lambda (o) (push o widgets))
              (multiple-value-list (tests-instructions-create
                                    data
                                    (list "p/P = Padding +/-"
                                          "s/S = Spacing +/-"
                                          ""
                                          "")
                                    (list ""
                                          ""
                                          "c = window interior red/default"
                                          "t = theme-flat/theme-3d"))))

      ;; Rulers
      (mapc #'(lambda (o) (push o widgets)) (multiple-value-list (tests-rulers-create-standard
                                                                  data :r5 nil :r6 nil :r7 nil :r8 nil :rv2 nil)))
      (mapc #'(lambda (o) (push o widgets)) (multiple-value-list (tests-rulers-create-wide data :r1 nil :r2 nil :r4 nil
                                                                                                :r3 '(3 nil rh5))))
      
      ;; The one in charge
      (setf manager (make-instance 'manager :content widgets)))))

(defmethod tests-destroy ((data (eql *text-data*)))
  (let ((args `((eql ,data))))
    (cleanup-method tests-get-interior-color args)
    (cleanup-method tests-get-padding args)
    (cleanup-method tests-get-spacing args)
    (cleanup-method cl-yag::on-command (list (find-class 'button))))
  nil)

(defmethod tests-ready ((text-data (eql *text-data*)))
  (defmethod cl-yag::on-command ((obj button) &key)
    (v:info :tests "Commanded ~a" (cl-yag::print-raw-object obj)))

  (defmethod tests-get-interior-color ((data (eql text-data)))
    (with-slots (w1 w2 w3 w4) data
      (values (list w1 w2 w3 w4) nil)))
  
  (defmethod tests-get-padding ((data (eql text-data)))
    (values nil nil))
  
  (defmethod tests-get-spacing ((data (eql text-data)))
    (with-slots (t5) data
      (values (list t5) nil))))

(defun text-tests-main ()
  (tests-main *text-data*))

