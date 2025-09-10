(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defstruct (grid-layout-tests-data (:include tests-data)
                            (:conc-name grid-layout-tests-))
  t1-1 t1-2 t1-3 t1-4
  t2-1 t2-2 t2-3 t2-4
  b2-1 b2-2 b2-3 b2-4 b2-5
  gl1 gl2
  w1 w2
  )

(defparameter *grid-layout-data* (make-grid-layout-tests-data))

(defmethod tests-create ((data (eql *grid-layout-data*)))
  (let (widgets)
    (with-slots (manager w1 w2
                 t1-1 t1-2 t1-3 t1-4
                 t2-1 t2-2 t2-3 t2-4
                 b2-1 b2-2 b2-3 b2-4 b2-5
                 gl1 gl2)
        data

      ;; Test 1
      (setf t1-1 (deftext :title "Grid 1 1"))
      (setf t1-2 (deftext :title "Grid 7 1"))
      (setf t1-3 (deftext :title "Grid 1 4"))
      (setf t1-4 (deftext :title "Grid 7 4"))
      (setf gl1 (defgrid-layout :columns 7 :rows 4 :content (list t1-1 nil nil nil nil nil t1-2
                                                                  nil nil nil nil nil nil nil
                                                                  nil nil nil nil nil nil nil
                                                                  t1-3 nil nil nil nil nil t1-4)))
      ;;(setf gl1 (defgrid-layout :columns 2 :rows 2))
      (setf w1 (deftests-window :wide-tall 1 :content (list gl1)))
      (push w1 widgets)
      
      ;; Test 2
      (setf t2-1 (deftext :title "Grid 1 1"))
      (setf t2-2 (deftext :title "Grid 3 1"))
      (setf t2-3 (deftext :title "Grid 1 3"))
      (setf t2-4 (deftext :title "Grid 3 3"))
      (setf b2-1 (defbox :thickness 2 :title "Grid 2 1" :title-position :center-middle :v-align :middle))
      (setf b2-2 (defbox :thickness 2 :title "Grid 1 2" :title-position :center-middle :v-align :middle))
      (setf b2-3 (defbox :thickness 2 :title "Grid 3 2" :title-position :center-middle :v-align :middle))
      (setf b2-4 (defbox :thickness 2 :title "Grid 2 3" :title-position :center-middle :v-align :middle))
      (setf gl2 (defgrid-layout :columns 3 :rows 3 :content (list t2-1 b2-1 t2-2 b2-2 nil b2-3 t2-3 b2-4 t2-4)))
      ;;(setf gl2 (defgrid-layout :columns 3 :rows 3))
      (setf w2 (deftests-window :wide-tall 2 :content (list gl2)))
      (push w2 widgets)
      
      ;; Instructions
      (mapcar #'(lambda (o) (push o widgets))
              (multiple-value-list (tests-instructions-create
                                    data
                                    (list "<1>"
                                          "<2>"
                                          "<3>"
                                          "<4>")
                                    (list "<5>"
                                          "<6>"
                                          "window interior red/default - <7>"
                                          "alternates theme-flat/theme-3d - <8>"))))

      ;; Rulers
      (mapc #'(lambda (o) (push o widgets)) (multiple-value-list (tests-rulers-create-wide-tall data)))
      
      ;; The one in charge
      (setf manager (make-instance 'manager :content widgets)))))

(defmethod tests-destroy ((data (eql *grid-layout-data*)))
  (let ((args (list (list 'eql data))))
    (cleanup-method tests-command-7 args)
    (cleanup-method tests-command-8 args))
  nil)

(defmethod tests-ready ((grid-layout-data (eql *grid-layout-data*)))
  (defmethod tests-command-7 ((data (eql grid-layout-data)))
    (with-slots (w1 w2) data
      (tests-toggle-interior-color data (list w1 w2)))
    nil)
  
  (defmethod tests-command-8 ((data (eql grid-layout-data)))
    (tests-toggle-theme data)
    nil)
  
  nil)

(defun grid-layout-tests-main ()
  (tests-main *grid-layout-data*))

