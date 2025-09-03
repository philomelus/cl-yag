(in-package :cl-yag-tests)

(defstruct (text-tests-data (:include tests-data)
                            (:conc-name text-tests-))
  a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
  b1 b2 b3
  cl1 cl2 cl3 cl4 cl5
  t1 t2 t3
  w1 w2 w3 w4
  )

(defparameter *text-data* (make-text-tests-data))

(defmethod tests-create ((data (eql *text-data*)))
  (with-slots (manager
               a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
               b1 b2 b3
               cl1 cl2 cl3 cl4 cl5
               t1 t2 t3
               w1 w2 w3 w4
               iw1 iw2)
      data

    ;; Test 1
    (setf a1 (defactive-text :title "T 1 / O 1" :h-align :center :v-align :middle))
    (setf a2 (defactive-text :title "T 1 / O 2" :h-align :center :v-align :middle))
    (setf a3 (defactive-text :title "T 1 / O 3" :h-align :center :v-align :middle))
    (setf cl1 (defcolumn-layout :content (list a1 a2 a3)))
    (setf w1 (defwindow +W1X+ +W1Y+ +W1W+ +W1H+ :content (list cl1)))

    ;; Test 2
    (setf t1 (deftext :title "T 2 Title" :h-align :center :height :auto-min))
    (setf a4 (defactive-text :title "T 2 / O 1" :h-align :center :v-align :middle))
    (setf a5 (defactive-text :title "T 2 / O 2" :h-align :center :v-align :middle))
    (setf a6 (defactive-text :title "T 2 / O 3" :h-align :center :v-align :middle))
    (setf cl2 (defcolumn-layout :content (list (list t1 :min-height) a4 a5 a6)))
    (setf w2 (defwindow +W2X+ +W2Y+ +W2W+ +W2H+ :content (list cl2)))

    ;; Test 3
    (setf b1 (defborder :thickness 2))
    (setf t2 (deftext :title "center/auto-min a really long title to see what happens" :h-align :center :height :auto-min
                      :padding-bottom 5
                      :border-bottom b1))
    (setf a7 (defactive-text :title "T 3 / O 1" :h-align :center :v-align :middle :height :auto-min
                             :width (- +W3W+ 24)
                             :left :center :top :middle
                             :padding-top 10 :padding-bottom 10))
    (setf a8 (defactive-text :title "T 3 / O 2" :h-align :center :v-align :middle :height :auto-min
                             :width (- +W3W+ 24)
                             :left :center :top :middle
                             :padding-top 10 :padding-bottom 10))
    (setf a9 (defactive-text :title "T 3 / O 3" :h-align :center :v-align :middle :height :auto-min
                             :width (- +W3W+ 24)
                             :left :center :top :middle
                             :padding-top 10 :padding-bottom 10))
    (setf cl3 (defcolumn-layout :content (list a7 a8 a9)))
    (setf cl4 (defcolumn-layout :content (list (list t2 :min-height) cl3)))
    (setf w3 (defwindow +W3X+ +W3Y+ +W3W+ +W3H+ :content (list cl4)))

    ;; Test 4
    (setf b2 (defborder :thickness 2))
    (setf t3 (deftext :title "center/auto-min" :h-align :center :height :auto-min
                      :padding-bottom 5
                      :border-bottom b2
                      :font (cl-yag:default-mono-font -20)))
    (setf b3 (defborder :thickness 2))
    (setf a10 (defactive-text :title "T 4 / O 1" :h-align :center :v-align :middle :height :auto-min
                              :width (- +W4W+ 24)
                              :left :center :top :bottom
                              :padding-top 10 :padding-bottom 10
                              :border-left b3 :border-right b3 :border-top b3 :border-bottom b3
                              :hover-color (al:map-rgb-f 0 1 1)))
    (setf a11 (defactive-text :title "T 4 / O 2" :h-align :center :v-align :middle :height :auto-min
                              :width (- +W4W+ 24)
                              :left :center :top :middle
                              :padding-top 10 :padding-bottom 10))
    (setf a12 (defactive-text :title "T 4 / O 3" :h-align :center :v-align :middle :height :auto-min
                              :width (- +W4W+ 24)
                              :left :center :top :top
                              :padding-top 10 :padding-bottom 10))
    (setf cl5 (defcolumn-layout :content (list (list t3 :min-height) a10 a11 a12)))
    (setf w4 (defwindow +W4X+ +W4Y+ +W4W+ +W4H+ :content (list cl5)))

    ;; Instructions
    (multiple-value-bind (tmp1 tmp2)
        (tests-instructions-create data
                                   (list "<1>"
                                         "<2>"
                                         "<3>"
                                         "<4> - alternates theme-flat/theme-3d")
                                   (list "- <5>"
                                         "- <6>"
                                         "window interior red/default - <7>"
                                         "- <8>"))
      (setf iw1 tmp1)
      (setf iw2 tmp2))
    
    ;; The one in charge
    (setf manager (make-instance 'manager :content (list w1 w2 w3 w4 iw1 iw2)))))

(defmethod tests-destroy ((data (eql *text-data*)))
  (remove-method #'tests-command-4 (find-method #'tests-command-4 () (list (list 'eql data))))
  (remove-method #'tests-command-7 (find-method #'tests-command-7 () (list (list 'eql data))))
  nil)

(defmethod tests-ready ((text-data (eql *text-data*)))

  (defmethod on-command ((obj active-text) &key)
    (v:info :tests "Commanded ~a" (cl-yag::print-raw-object obj)))
  
  (defmethod tests-command-4 ((data (eql text-data)))
    (tests-toggle-theme data)
    nil)
  
  (defmethod tests-command-7 ((data (eql text-data)))
    (with-slots (w1 w2 w3 w4) data
      (tests-toggle-interior-color data (list w1 w2 w3 w4)))
    nil)
  nil)

(defun text-tests-main ()
  (tests-main *text-data*))

