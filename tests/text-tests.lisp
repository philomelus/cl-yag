(in-package :cl-yag-tests)

(defstruct (text-tests-data (:include tests-data)
                            (:conc-name text-tests-))
  a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
  b1 b2 b3
  cl1 cl2 cl3 cl4 cl5
  t1 t2 t3
  w1 w2 w3 w4)

(defparameter *text-data* (make-text-tests-data))

(defmethod tests-create ((data (eql *text-data*)))
  (with-slots (manager
               a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
               b1 b2 b3
               cl1 cl2 cl3 cl4 cl5
               t1 t2 t3
               w1 w2 w3 w4)
      data

    ;; Test 1
    (setf a1 (defactive-text :title "center/middle" :h-align :center :v-align :middle))
    (setf a2 (defactive-text :title "center/middle" :h-align :center :v-align :middle))
    (setf a3 (defactive-text :title "Quit" :h-align :center :v-align :middle
                             :shortcuts (list '(:q :shift) '(:q :none))))
    (setf cl1 (defcolumn-layout :content (list a1 a2 a3)))
    (setf w1 (defwindow 25 25 200 300 :content (list cl1) :interior-color (al:map-rgb-f 1 0 0)))

    ;; Test 2
    (setf t1 (deftext :title "center/auto-min" :h-align :center :height :auto-min))
    (setf a4 (defactive-text :title "center/middle" :h-align :center :v-align :middle))
    (setf a5 (defactive-text :title "center/middle" :h-align :center :v-align :middle))
    (setf a6 (defactive-text :title "center/middle" :h-align :center :v-align :middle))
    (setf cl2 (defcolumn-layout :content (list (list t1 :min-height) a4 a5 a6)))
    (setf w2 (defwindow 250 25 200 300 :content (list cl2) :interior-color (al:map-rgb-f 1 0 0)))

    ;; Test 3
    (setf b1 (defborder :thickness 2))
    (setf t2 (deftext :title "center/auto-min a really long title to see what happens" :h-align :center :height :auto-min
                      :padding-bottom 5
                      :interior-color (al:map-rgb-f 1 0 0)
                      :border-bottom b1))
    (setf a7 (defactive-text :title "c/m/auto-min/m" :h-align :center :v-align :middle :height :auto-min
                             :width 176
                             :left :center :top :middle
                             :padding-top 10 :padding-bottom 10))
    (setf a8 (defactive-text :title "c/m/auto-min/m" :h-align :center :v-align :middle :height :auto-min
                             :width 176
                             :left :center :top :middle
                             :padding-top 10 :padding-bottom 10))
    (setf a9 (defactive-text :title "c/m/auto-min/m" :h-align :center :v-align :middle :height :auto-min
                             :width 176
                             :left :center :top :middle
                             :padding-top 10 :padding-bottom 10))
    (setf cl3 (defcolumn-layout :content (list a7 a8 a9)))
    (setf cl4 (defcolumn-layout :content (list (list t2 :min-height) cl3)))
    (setf w3 (defwindow 475 25 200 300 :content (list cl4) :interior-color (al:map-rgb-f 1 0 0)))

    ;; Test 4
    (setf b2 (defborder :thickness 2))
    (setf t3 (deftext :title "center/auto-min" :h-align :center :height :auto-min
                      :padding-bottom 5
                      :interior-color (al:map-rgb-f 1 0 0)
                      :border-bottom b2
                      :font (cl-yag:default-mono-font -20)))
    (setf b3 (defborder :thickness 2))
    (setf a10 (defactive-text :title "c/b/auto-min/m" :h-align :center :v-align :middle :height :auto-min
                              :width 176
                              :left :center :top :bottom
                              :padding-top 10 :padding-bottom 10
                              :border-left b3 :border-right b3 :border-top b3 :border-bottom b3
                              :hover-color (al:map-rgb-f 0 1 1)))
    (setf a11 (defactive-text :title "c/m/auto-min/m" :h-align :center :v-align :middle :height :auto-min
                              :width 176
                              :left :center :top :middle
                              :padding-top 10 :padding-bottom 10))
    (setf a12 (defactive-text :title "c/t/auto-min/m" :h-align :center :v-align :middle :height :auto-min
                              :width 176
                              :left :center :top :top
                              :padding-top 10 :padding-bottom 10))
    (setf cl5 (defcolumn-layout :content (list (list t3 :min-height) a10 a11 a12)))
    (setf w4 (defwindow 700 25 200 300 :content (list cl5)))

    ;; The one in charge
    (setf manager (make-instance 'manager :content (list w1 w2 w3 w4))))


  (defmethod on-command ((obj active-text) &key)
    (v:info :tests "Commanded ~a" (cl-yag::print-raw-object obj)))
           
  (defmethod on-command ((obj (eql (text-tests-a3 *text-data*))) &key)
    (setf (process (tests-data-manager *text-data*)) nil))
  )

(defmethod tests-destroy ((data (eql *text-data*)))
  )

(defun text-tests-main ()
  (tests-main *text-data*))

