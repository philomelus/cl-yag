(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defstruct (row-layout-tests-data (:include tests-data)
                                  (:conc-name row-layout-tests-))

  st111 st112 st121 st122 st131 st132 rl1 t11 t12 t13
  st211 st212 st221 st222 st231 st232 rl2 t21 t22 t23
  st311 st312 st321 st322 st331 st332 rl3 t31 t32 t33 b31 b32

  w1 w2 w3 w4)

(defparameter *row-layout-data* (make-row-layout-tests-data))

(defmethod tests-create ((data (eql *row-layout-data*)))
  (let (widgets)
    (with-slots (manager w1 w2 w3 w4
                 st111 st112 st121 st122 st131 st132 rl1 t11 t12 t13
                 st211 st212 st221 st222 st231 st232 rl2 t21 t22 t23
                 st311 st312 st321 st322 st331 st332 rl3 t31 t32 t33 b31 b32)
        data

      ;; Test 1
      (setf st111 (deftests-status :wide 4 11 2 "[HA1] :RIGHT" 1 1))
      (setf st112 (deftests-status :wide 4 11 2 "[VA1] :MIDDLE" 1 2))
      (setf st121 (deftests-status :wide 4 11 2 "[HA2] :CENTER" 2 1))
      (setf st122 (deftests-status :wide 4 11 2 "[VA2] :MIDDLE" 2 2))
      (setf st131 (deftests-status :wide 4 11 2 "[HA3] :LEFT" 3 1))
      (setf st132 (deftests-status :wide 4 11 2 "[VA3] :MIDDLE" 3 2))
      (setf t11 (defactive-text :title "Title 1" :v-align :middle :h-align :right))
      (setf t12 (deftext :title "Title 2" :v-align :middle :h-align :center))
      (setf t13 (defactive-text :title "Title 3" :v-align :middle :h-align :left))
      (setf rl1 (defrow-layout :content `(,t11 ,t12 ,t13)))
      (setf w1 (deftests-window :wide 1 :content `(,rl1 ,st111 ,st112 ,st121 ,st122 ,st131 ,st132)))
      (push w1 widgets)
      
      ;; Test 2
      (setf st211 (deftests-status :wide 4 11 2 "[HA1] :RIGHT" 5 1))
      (setf st212 (deftests-status :wide 4 11 2 "[VA1] :MIDDLE" 5 2))
      (setf st221 (deftests-status :wide 4 11 2 "[HA2] :CENTER" 6 1))
      (setf st222 (deftests-status :wide 4 11 2 "[VA2] :MIDDLE" 6 2))
      (setf st231 (deftests-status :wide 4 11 2 "[HA3] :LEFT" 7 1))
      (setf st232 (deftests-status :wide 4 11 2 "[VA3] :MIDDLE" 7 2))
      (setf t21 (defactive-text :title "Title 1" :v-align :middle :h-align :right))
      (setf t22 (deftext :title "Title 2" :v-align :middle :h-align :center))
      (setf t23 (defactive-text :title "Title 3" :v-align :middle :h-align :left))
      (setf rl2 (defrow-layout :content `((,t21 :min-width) (,t22 :max-width) (,t23 :min-width))))
      (setf w2 (deftests-window :wide 2 :content `(,rl2 ,st211 ,st212 ,st221 ,st222 ,st231 ,st232)))
      (push w2 widgets)
      
      ;; Test 3
      (setf st311 (deftests-status :wide 4 11 2 "[1]" 9 1))
      (setf st312 (deftests-status :wide 4 11 2 "[1]" 9 2))
      (setf st321 (deftests-status :wide 4 11 2 "[2]" 10 1))
      (setf st322 (deftests-status :wide 4 11 2 "[2]" 10 2))
      (setf st331 (deftests-status :wide 4 11 2 "[3]" 11 1))
      (setf st332 (deftests-status :wide 4 11 2 "[3]" 11 2))
      (setf t31 (deftext :title "Title 3/1" :v-align :middle :h-align :center))
      (setf b31 (defborder :thickness 2))
      (setf (border t31) b31)
      (setf (spacing t31) 5)
      (setf t32 (deftext :title "Title 3/2" :v-align :middle :h-align :center))
      (setf b32 (defborder :thickness 2))
      (setf (border t32) b32)
      (setf (spacing t32) 5)
      (setf rl3 (defrow-layout :content `(,t31 ,t32)))
      (setf w3 (deftests-window :wide 3 :content `(,rl3 ,st311 ,st312 ,st321 ,st322 ,st331 ,st332)))
      (push w3 widgets)
      
      ;; Test 4
      (setf w4 (deftests-window :wide 4))
      (push w4 widgets)
      
      ;; Instructions
      (mapc #'(lambda (o) (push o widgets))
            (multiple-value-list (tests-instructions-create
                                  data
                                  (list "h   = :left/:center/:right"
                                        "v   = :top/:middle/:bottom"
                                        "s/S = Spacing +/-"
                                        "b/B = Border +/-")
                                  (list "k/K = Thickness +/-"
                                        ""
                                        "c   = window interior red/default"
                                        "t   = theme-flat/theme-3d"))))

      ;; Rulers
      (mapc #'(lambda (o) (push o widgets)) (multiple-value-list (tests-rulers-create-wide data)))
      
      ;; The one in charge
      (setf manager (make-instance 'manager :content widgets)))))

(defmethod tests-destroy ((data (eql *row-layout-data*)))
  (let ((args `((eql ,data))))
    (cleanup-method tests-command-update args)
    (cleanup-method tests-get-h-align args)
    (cleanup-method tests-get-interior-color args)
    (cleanup-method tests-get-spacing args)
    (cleanup-method tests-get-thickness args)
    (cleanup-method tests-get-v-align args))
  nil)

(defmethod tests-ready ((row-layout-data (eql *row-layout-data*)))

  (defmethod tests-get-h-align ((data (eql row-layout-data)))
    (with-slots (t11 t12 t13 t21 t22 t23) data
      (values (list t11 t12 t13 t21 t22 t23) t)))
  
  (defmethod tests-get-interior-color ((data (eql row-layout-data)))
    (with-slots (w1 w2 w3) data
      (values (list w1 w2 w3) nil)))

  (defmethod tests-get-spacing ((data (eql row-layout-data)))
    (with-slots (w1 w2 w3) data
        (values (list w1 w2 w3) t)))

  (defmethod tests-get-thickness ((data (eql row-layout-data)))
    (with-slots (b31 b32) data
      (values (list b31 b32) t)))
  
  (defmethod tests-get-v-align ((data (eql row-layout-data)))
    (with-slots (t11 t12 t13 t21 t22 t23) data
      (values (list t11 t12 t13 t21 t22 t23) t)))

  (defmethod tests-command-update ((data (eql row-layout-data)))
    (macrolet ((update (status text)
                 `(unless (eql ,status nil)
                    (setf (title ,status) ,text))))
      (with-slots (st111 st112 t11 st121 st122 t12 st131 st132 t13
                   st211 st212 t21 st221 st222 t22 st231 st232 t23
                   st311 st312 st321 st322 st331 st332 b31 w1 w2 w3)
          data
        (unless (eql t11 nil)
          (update st111 (format nil "[HA1] :~a" (h-align t11)))
          (update st112 (format nil "[VA1] :~a" (v-align t11))))
        (unless (eql t12 nil)
          (update st121 (format nil "[HA2] :~a" (h-align t12)))
          (update st122 (format nil "[VA2] :~a" (v-align t12))))
        (unless (eql t13 nil)
          (update st131 (format nil "[HA3] :~a" (h-align t13)))
          (update st132 (format nil "[VA3] :~a" (v-align t13))))
        (unless (eql t21 nil)
          (update st211 (format nil "[HA1] :~a" (h-align t21)))
          (update st212 (format nil "[VA1] :~a" (v-align t21))))
        (unless (eql t22 nil)
          (update st221 (format nil "[HA2] :~a" (h-align t22)))
          (update st222 (format nil "[VA2] :~a" (v-align t22))))
        (unless (eql t23 nil)
          (update st231 (format nil "[HA3] :~a" (h-align t23)))
          (update st232 (format nil "[VA3] :~a" (v-align t23))))
        (unless (eql w1 nil)
          (update st311 (format nil "w1:~d" (spacing-left w1))))
        (unless (eql w2 nil)
          (update st321 (format nil "w2:~d" (spacing-left w2))))
        (unless (eql w3 nil)
          (update st331 (format nil "w3:~d" (spacing-left w3)))
          (update st332 (format nil "b1:~d" (thickness b31))))
        ))
    nil)

  (tests-command-update row-layout-data)
  
  nil)

(defun row-layout-tests-main ()
  (tests-main *row-layout-data*))

