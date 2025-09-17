(in-package :cl-yag-tests)

(defstruct (box-tests-data (:include tests-data)
                           (:conc-name box-tests-))
  b1 b2 b3 b4 b5 b6 b7 b8
  cl1 cl2 cl3 cl4 cl5 cl6 cl7 cl8
  w1 w2 w3 w4 w5 w6 w7 w8
  )

(defparameter *box-data* (make-box-tests-data))

(defmethod tests-create ((data (eql *box-data*)))

  (let (objs)
    (with-slots (manager
                 b1 b2 b3 b4 b5 b6 b7
                 cl1 cl2 cl3 cl4 cl5 cl6 cl7 cl8
                 w1 w2 w3 w4 w5 w6 w7 w8
                 )
        data
      
      ;; Test 1
      (setf b1 (defbox :left (+ +W1L+ 10) :top (+ +W1T+ 10) :width (- +W1W+ 20) :height (- +W1H+ 20)
                       :filled t
                       :thickness 2
                       :title "Test Box 1" :title-position :left-top
                       :v-align :top))
      (setf cl1 (defcolumn-layout :content (list b1)))
      (setf w1 (deftests-window :standard 1 :content (list cl1)))
      (push w1 objs)
      
      ;; Test 2
      (setf b2 (defbox :left (+ +W2L+ 10) :top (+ +W2T+ 10) :width (- +W2W+ 20) :height (- +W2H+ 20)
                       :filled t
                       :thickness 2
                       :title "Test Box 2" :title-position :left-top
                       :v-align :middle))
      (setf cl2 (defcolumn-layout :content (list b2)))
      (setf w2 (deftests-window :standard 2 :content (list cl2)))
      (push w2 objs)
      
      ;; Test 3
      (setf b3 (defbox :left (+ +W3L+ 10) :top (+ +W3T+ 10) :width (- +W3W+ 20) :height (- +W3H+ 20)
                       :filled t
                       :thickness 2
                       :title "Test Box 3" :title-position :left-top
                       :v-align :bottom))
      (setf cl3 (defcolumn-layout :content (list b3)))
      (setf w3 (deftests-window :standard 3 :content (list cl3)))
      (push w3 objs)
      
      ;; Test 4
      (setf b4 (defbox :left (+ +W4L+ 10) :top (+ +W4T+ 10) :width (- +W4W+ 20) :height (- +W4H+ 20)))
      (setf cl4 (defcolumn-layout :content (list b4)))
      (setf w4 (deftests-window :standard 4 :content (list cl4)))
      (push w4 objs)
      
      ;; Test 5
      (setf b5 (defbox :left (+ +W5L+ 10) :top (+ +W5T+ 10) :width (- +W5W+ 20) :height (- +W5H+ 20)
                       :filled t
                       :thickness 10
                       :title "Test Box 5" :title-position :left-top
                       :v-align :top))
      (setf cl5 (defcolumn-layout :content (list b5)))
      (setf w5 (deftests-window :standard 5 :content (list cl5)))
      (push w5 objs)
      
      ;; Test 6
      (setf b6 (defbox :left (+ +W6L+ 10) :top (+ +W6T+ 10) :width (- +W6W+ 20) :height (- +W6H+ 20)
                       :filled t
                       :thickness 10
                       :title "Test Box 6" :title-position :left-top
                       :v-align :middle))
      (setf cl6 (defcolumn-layout :content (list b6)))
      (setf w6 (deftests-window :standard 6 :content (list cl6)))
      (push w6 objs)
      
      ;; Test 7
      (setf b7 (defbox :left (+ +W-W4L+ 10) :top (+ +W-W4T+ 10) :width (- +W-W4W+ 20) :height (- +W-W4H+ 20)
                       :filled t
                       :thickness 10
                       :title "Test Box 7" :title-position :left-top
                       :v-align :bottom))
      (setf cl7 (defcolumn-layout :content (list b7)))
      (setf w7 (deftests-window :wide 4 :content (list cl7)))
      (push w7 objs)
    
      ;; Instructions
      (mapc #'(lambda (o) (push o objs))
              (multiple-value-list (tests-instructions-create
                                    data
                                    (list "<1> - :left/:center/:right"
                                          "<2> - :top/:middle/:bottom"
                                          "<3> - v-align :top/:middle/:bottom"
                                          "<4> - :inset/:outset/:flat")
                                    (list "decrease border width - <5>"
                                          "increase border width - <6>"
                                          "window interior red/default - <7>"
                                          "theme-flat/theme-3d- <8>"))))

      ;; Rulers
      (mapc #'(lambda (o) (push o objs)) (multiple-value-list (tests-rulers-create-standard data :r7 nil :r8 nil)))
      (mapc #'(lambda (o) (push o objs)) (multiple-value-list (tests-rulers-create-wide data :r4 '(:wide 4 rh7))))

      ;; The one in charge
      (setf manager (make-instance 'manager :content (reverse objs))))))

(defmethod tests-destroy ((data (eql *box-data*)))
  (let ((args `((eql ,data))))
    (cl-yag::cleanup-method tests-command-1 args)
    (cl-yag::cleanup-method tests-command-2 args)
    (cl-yag::cleanup-method tests-command-3 args)
    (cl-yag::cleanup-method tests-command-4 args)
    (cl-yag::cleanup-method tests-command-5 args)
    (cl-yag::cleanup-method tests-command-6 args)
    (cl-yag::cleanup-method tests-command-7 args)
    (cl-yag::cleanup-method tests-command-8 args)
    (cl-yag::cleanup-method tests-command-update args)
    (cl-yag::cleanup-method tests-render args))
  nil)

(defmethod tests-ready ((box-data (eql *box-data*)))
  (defmethod tests-command-1 ((data (eql box-data)))
    (with-slots (b1 b2 b3 b5 b6 b7) data
      (dolist (obj (list b1 b2 b3 b5 b6 b7))
        (with-accessors ((tp title-position)) obj
          (case tp
            (:left-top
             (setf tp :left-middle))
            (:left-middle
             (setf tp :left-bottom))
            (:left-bottom
             (setf tp :left-top))
          
            (:center-top
             (setf tp :center-middle))
            (:center-middle
             (setf tp :center-bottom))
            (:center-bottom
             (setf tp :center-top))
          
            (:right-top
             (setf tp :right-middle))
            (:right-middle
             (setf tp :right-bottom))
            (:right-bottom
             (setf tp :right-top))))))
    t)

  (defmethod tests-command-2 ((data (eql box-data)))
    (with-slots (b1 b2 b3 b5 b6 b7) data
      (dolist (obj (list b1 b2 b3 b5 b6 b7))
        (with-accessors ((tp title-position)) obj
          (case tp
            (:left-top
             (setf tp :center-top))
            (:left-middle
             (setf tp :center-middle))
            (:left-bottom
             (setf tp :center-bottom))
          
            (:center-top
             (setf tp :right-top))
            (:center-middle
             (setf tp :right-middle))
            (:center-bottom
             (setf tp :right-bottom))
          
            (:right-top
             (setf tp :left-top))
            (:right-middle
             (setf tp :left-middle))
            (:right-bottom
             (setf tp :left-bottom))))))
    t)

  (defmethod tests-command-3 ((data (eql box-data)))
    (with-slots (b1 b2 b3 b5 b6 b7) data
      (dolist (obj (list b1 b2 b3 b5 b6 b7))
        (with-accessors ((va v-align)) obj
          (case va
            (:top
             (setf va :middle))
            (:middle
             (setf va :bottom))
            (:bottom
             (setf va :top))))))
    t)

  (defmethod tests-command-4 ((data (eql box-data)))
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
    nil)
  
  (defmethod tests-command-5 ((data (eql box-data)))
    (with-slots (b1 b2 b3 b4 b5 b6 b7) data
      (let ((objs (list b1 b2 b3 b4 b5 b6 b7)))
        (dolist (obj objs)
          (unless (eql obj nil)
            (with-accessors ((tn thickness)) obj
              (unless (= tn 0)
                (decf tn)))))))
    nil)

  (defmethod tests-command-6 ((data (eql box-data)))
    (with-slots (b1 b2 b3 b4 b5 b6 b7) data
      (let ((objs (list b1 b2 b3 b4 b5 b6 b7)))
        (dolist (obj objs)
          (unless (eql obj nil)
            (with-accessors ((tn thickness)) obj
              (incf tn))))))
    nil)

  (defmethod tests-command-7 ((data (eql box-data)))
    (with-slots (w1 w2 w3 w4 w5 w6 w7 w8) data
      (tests-toggle-interior-color data (list w1 w2 w3 w4 w5 w6 w7 w8)))
    nil)

  (defmethod tests-command-8 ((data (eql box-data)))
    (tests-toggle-theme data)
    nil)

  (defmethod tests-command-update ((data (eql box-data)))
    (with-slots (b1 b2 b3 b4 b5 b6 b7) data
      (dolist (obj (list b1 b2 b3 b5 b6 b7))
        (unless (eql obj nil)
          (with-accessors ((tp title-position) (va v-align)) obj
            (unless (eql tp nil)
              (let ((new-title
                      (case tp
                        (:center-bottom "Ctr / B")
                        (:center-middle "Ctr / M")
                        (:center-top "Ctr / T")
                        (:left-bottom "Lft / B")
                        (:left-middle "Lfy / M")
                        (:left-top "Lft / T")
                        (:right-bottom "Rht / B")
                        (:right-middle "Rhy / M")
                        (:right-top "Rht / T"))))
                (setq new-title (concatenate 'string new-title (case va
                                                                 (:top "-t")
                                                                 (:middle "-m")
                                                                 (:bottom "-b"))))
                (setf (title obj) new-title)))))))
    nil)

  (defmethod tests-render ((data (eql box-data)))
    (with-slots (manager b1 b2 b3 b4 b5 b6 b7) data
      (al:clear-to-color (al:map-rgb-f 0.25 0.25 0.25))  
      (paint manager))
    t)
  
  ;; Initial call to set titles
  (tests-command-update box-data)

  ;; Change frame color
  (setf (frame-color (tests-theme1 box-data)) (al:map-rgb-f 0.5 0.5 0.5))
  nil)

(defun box-tests-main ()
  (tests-main *box-data*))

