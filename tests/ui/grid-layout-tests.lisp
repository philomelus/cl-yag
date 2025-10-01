(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defstruct (grid-layout-tests-data (:include tests-data)
                            (:conc-name grid-layout-tests-))
  t1-1 t1-2 t1-3 t1-4 t1-5 t1-6 t1-7 t1-8 t1-9 gl1
  w1
  )

(defparameter *grid-layout-data* (make-grid-layout-tests-data))

(defmethod tests-create ((data (eql *grid-layout-data*)))
  (let (widgets)
    (with-slots (manager w1
                 t1-1 t1-2 t1-3 t1-4 t1-5 t1-6 t1-7 t1-8 t1-9
                 gl1)
        data

      ;; Test 1
      (setf t1-1 (deftext :title "Grid 2 1" :h-align :center :v-align :middle))
      (setf t1-2 (deftext :title "Grid 6 1" :h-align :center :v-align :middle))
      (setf t1-3 (deftext :title "Grid 3 2" :h-align :center :v-align :middle))
      (setf t1-4 (deftext :title "Grid 5 2" :h-align :center :v-align :middle))
      (setf t1-5 (deftext :title "Grid 4 4" :h-align :center :v-align :middle))
      (setf t1-6 (deftext :title "Grid 3 5" :h-align :center :v-align :middle))
      (setf t1-7 (deftext :title "Grid 5 5" :h-align :center :v-align :middle))
      (setf t1-8 (deftext :title "Grid 2 5" :h-align :center :v-align :middle))
      (setf t1-9 (deftext :title "Grid 6 5" :h-align :center :v-align :middle))
      (setf gl1 (defgrid-layout 7 5 :content (list nil t1-1 nil nil nil t1-2 nil
                                                   nil nil t1-3 nil t1-4 nil nil
                                                   nil nil nil t1-5 nil nil nil
                                                   nil nil t1-6 nil t1-7 nil nil
                                                   nil t1-8 nil nil nil t1-9 nil)))
      ;; (setf (width (layout-column 0 gl1)) 25)
      ;; (setf (width (layout-column 6 gl1)) 25)
      ;; (setf (height (layout-row 0 gl1)) 25)
      ;; (setf (height (layout-row 4 gl1)) 25)
      ;; (layout-column-cells-set (defborder :thickness 5) 'border-right 0 gl1 :recalc nil)
      ;; (layout-column-cells-set (defborder :thickness 5) 'border-left 6 gl1 :recalc nil)
      ;; (layout-row-cells-set (defborder :thickness 5) 'border-bottom 0 gl1 :recalc nil)
      ;; (layout-row-cells-set (defborder :thickness 5) 'border-top 4 gl1 :recalc nil)
      
      ;;(setf gl1 (defgrid-layout :columns 2 :rows 2))
      (setf w1 (deftests-window :full 1 :content (list gl1)))
      (push w1 widgets)
      
      ;; Instructions
      (mapcar #'(lambda (o) (push o widgets))
              (multiple-value-list (tests-instructions-create
                                    data
                                    (list "v = top/middle/bottom"
                                          "h = left/center/right"
                                          ""
                                          "")
                                    (list ""
                                          ""
                                          "c = window interior red/default"
                                          "t = alternates theme-flat/theme-3d"))))

      ;; Rulers
      (mapc #'(lambda (o) (push o widgets)) (multiple-value-list (tests-rulers-create-full data)))
      
      ;; The one in charge
      (setf manager (make-instance 'manager :content widgets)))))

(defmethod tests-destroy ((data (eql *grid-layout-data*)))
  (let ((args (list (list 'eql data))))
    (cleanup-method tests-get-h-align args)
    (cleanup-method tests-get-interior-color args)
    (cleanup-method tests-get-v-align args))
  nil)

(defmethod tests-ready ((grid-layout-data (eql *grid-layout-data*)))
  (defmethod tests-get-h-align ((data (eql grid-layout-data)))
    (with-slots (t1-1 t1-2 t1-3 t1-4 t1-5 t1-6 t1-7 t1-8 t1-9) data
      (values (list t1-1 t1-2 t1-3 t1-4 t1-5 t1-6 t1-7 t1-8 t1-9) nil)))

  (defmethod tests-get-v-align ((data (eql grid-layout-data)))
    (with-slots (t1-1 t1-2 t1-3 t1-4 t1-5 t1-6 t1-7 t1-8 t1-9) data
      (values (list t1-1 t1-2 t1-3 t1-4 t1-5 t1-6 t1-7 t1-8 t1-9) nil)))

  (defmethod tests-get-interior-color ((data (eql grid-layout-data)))
    (with-slots (w1) data
      (values (list w1) nil)))
  
  nil)

(defun grid-layout-tests-main ()
  (tests-main *grid-layout-data*))

