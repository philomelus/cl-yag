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
                 b1 b2 b3 b4 b5 b6 b7 b8
                 cl1 cl2 cl3 cl4 cl5 cl6 cl7 cl8
                 w1 w2 w3 w4 w5 w6 w7 w8
                 )
        data
      
      ;; Test 1
      (setf b1 (defbox :left (+ +W1X+ 10) :top (+ +W1Y+ 10) :width (- +W1W+ 20) :height (- +W1H+ 20)
                       :filled t
                       :thickness 2
                       :title "Test Box 1" :title-position :left-top
                       :v-align :top))
      (setf cl1 (defcolumn-layout :content (list b1)))
      (setf w1 (defwindow +W1X+ +W1Y+ +W1W+ +W1H+ :content (list cl1)))
      (push w1 objs)
      
      ;; Test 2
      (setf b2 (defbox :left (+ +W2X+ 10) :top (+ +W2Y+ 10) :width (- +W2W+ 20) :height (- +W2H+ 20)
                       :filled t
                       :thickness 2
                       :title "Test Box 2" :title-position :left-top
                       :v-align :middle))
      (setf cl2 (defcolumn-layout :content (list b2)))
      (setf w2 (defwindow +W2X+ +W2Y+ +W2W+ +W2H+ :content (list cl2)))
      (push w2 objs)
      
      ;; Test 3
      (setf b3 (defbox :left (+ +W3X+ 10) :top (+ +W3Y+ 10) :width (- +W3W+ 20) :height (- +W3H+ 20)
                       :filled t
                       :thickness 2
                       :title "Test Box 3" :title-position :left-top
                       :v-align :bottom))
      (setf cl3 (defcolumn-layout :content (list b3)))
      (setf w3 (defwindow +W3X+ +W3Y+ +W3W+ +W3H+ :content (list cl3)))
      (push w3 objs)
      
      ;; Test 4
      (setf b4 (defbox :left (+ +W4X+ 10) :top (+ +W4Y+ 10) :width (- +W4W+ 20) :height (- +W4H+ 20)))
      (setf cl4 (defcolumn-layout :content (list b4)))
      (setf w4 (defwindow +W4X+ +W4Y+ +W4W+ +W4H+ :content (list cl4)))
      (push w4 objs)
      
      ;; Test 5
      (setf b5 (defbox :left (+ +W5X+ 10) :top (+ +W5Y+ 10) :width (- +W5W+ 20) :height (- +W5H+ 20)
                       :filled t
                       :thickness 10
                       :title "Test Box 5" :title-position :left-top
                       :v-align :top))
      (setf cl5 (defcolumn-layout :content (list b5)))
      (setf w5 (defwindow +W5X+ +W5Y+ +W5W+ +W5H+ :content (list cl5)))
      (push w5 objs)
      
      ;; Test 6
      (setf b6 (defbox :left (+ +W6X+ 10) :top (+ +W6Y+ 10) :width (- +W6W+ 20) :height (- +W6H+ 20)
                       :filled t
                       :thickness 10
                       :title "Test Box 6" :title-position :left-top
                       :v-align :middle))
      (setf cl6 (defcolumn-layout :content (list b6)))
      (setf w6 (defwindow +W6X+ +W6Y+ +W6W+ +W6H+ :content (list cl6)))
      (push w6 objs)
      
      ;; Test 7
      (setf b7 (defbox :left (+ +W7X+ 10) :top (+ +W7Y+ 10) :width (- +W7W+ 20) :height (- +W7H+ 20)
                       :filled t
                       :thickness 10
                       :title "Test Box 7" :title-position :left-top
                       :v-align :bottom))
      (setf cl7 (defcolumn-layout :content (list b7)))
      (setf w7 (defwindow +W7X+ +W7Y+ +W7W+ +W7H+ :content (list cl7)))
      (push w7 objs)
    
      ;; Test 8
      (setf b8 (defbox :left (+ +W8X+ 10) :top (+ +W8Y+ 10) :width (- +W8W+ 20) :height (- +W8H+ 20)
                       :filled t :thickness 10
                       :title "Text Box 8" :title-position :center-middle))
      (setf cl8 (defcolumn-layout :content (list b8)))
      (setf w8 (defwindow +W8X+ +W8Y+ +W8W+ +W8H+ :content (list cl8)))
      (push w8 objs)
      
      ;; Instructions
      (mapcar #'(lambda (o) (push o objs))
              (multiple-value-list (tests-instructions-create
                                    data
                                    (list "<1> - alternates left/center/right"
                                          "<2> - alternates top/middle/bottom"
                                          "<3> - alterdate v-align top/middle/bottom"
                                          "<4> - alternates theme-flat/theme-3d")
                                    (list "increase border width - <5>"
                                          "decrease border width - <6>"
                                          "window interior red/default - <7>"
                                          "- <8>"))))

      ;; Rulers
      (mapcar #'(lambda (o) (push o objs)) (multiple-value-list (tests-rulers-create data t t)))

      ;; The one in charge
      (setf manager (make-instance 'manager :content objs)))))

(defmethod tests-destroy ((data (eql *box-data*)))
  (remove-method #'tests-command-1 (find-method #'tests-command-1 () (list (list 'eql data))))
  (remove-method #'tests-command-2 (find-method #'tests-command-2 () (list (list 'eql data))))
  (remove-method #'tests-command-3 (find-method #'tests-command-3 () (list (list 'eql data))))
  (remove-method #'tests-command-4 (find-method #'tests-command-4 () (list (list 'eql data))))
  (remove-method #'tests-command-5 (find-method #'tests-command-5 () (list (list 'eql data))))
  (remove-method #'tests-command-6 (find-method #'tests-command-6 () (list (list 'eql data))))
  (remove-method #'tests-command-7 (find-method #'tests-command-7 () (list (list 'eql data))))
  nil)

(defmethod tests-ready (box-data)
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
    nil)

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
    nil)

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
    nil)
  
  (defmethod tests-command-4 ((data (eql box-data)))
    (tests-toggle-theme data)
    nil)

  (defmethod tests-command-5 ((data (eql box-data)))
    (with-slots (b1 b2 b3 b4 b5 b6 b7 b8) data
      (let ((objs (list b1 b2 b3 b4 b5 b6 b7 b8)))
        (dolist (obj objs)
          (with-accessors ((tn thickness)) obj
            (unless (= tn 0)
              (setf tn (1- tn)))))))
    nil)

  (defmethod tests-command-6 ((data (eql box-data)))
    (with-slots (b1 b2 b3 b4 b5 b6 b7 b8) data
      (let ((objs (list b1 b2 b3 b4 b5 b6 b7 b8)))
        (dolist (obj objs)
          (with-accessors ((tn thickness)) obj
            (incf tn 1)))))
    nil)

  (defmethod tests-command-7 ((data (eql box-data)))
    (with-slots (w1 w2 w3 w4 w5 w6 w7 w8) data
      (tests-toggle-interior-color data (list w1 w2 w3 w4 w5 w6 w7 w8)))
    nil)
  nil)

(defun box-tests-main ()
  (tests-main *box-data*))

