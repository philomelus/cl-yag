(in-package :cl-yag-tests)

(defstruct (box-tests-data (:include tests-data)
                           (:conc-name box-tests-))
  b1 b2 b3 b4 b5 b6 b7 b8
  cl1 cl2 cl3 cl4 cl5 cl6 cl7 cl8
  rv1 rv2
  r1 r2 r3 r4 r5 r6 r7 r8
  w1 w2 w3 w4 w5 w6 w7 w8

  ;; iw1 iw2
  ;; icl1 icl2
  ;; it1 it2 it3 it4 it5 it6 it7 it8
  
  ;; Keyboard state handling
  horiz                                 ; 1 = :left :center :right
  vert                                  ; 2 = :top :middle :bottom
  vert-align                            ; 3 = :top :middle :bottom
  theme1
  theme2
  )

(defparameter *box-data* (make-box-tests-data))

(defmethod tests-create ((data (eql *box-data*)))
  (with-slots (manager
               b1 b2 b3 b4 b5 b6 b7 b8
               cl1 cl2 cl3 cl4 cl5 cl6 cl7 cl8
               rv1 rv2
               r1 r2 r3 r4 r5 r6 r7 r8
               w1 w2 w3 w4 w5 w6 w7 w8
               
               iw1 iw2
               icl1 icl2
               it1 it2 it3 it4 it5 it6 it7 it8
               )
      data

    ;; Vertical rulers
    (setf rv1 (defruler :major 25 :minor 5 :width 10 :height 300 :visible t :vertical t))
    (setf rv2 (defruler :major 25 :minor 5 :width 10 :height 300 :visible t :vertical t))

    ;; Horizontal rulers
    (setf r1 (defruler :major 25 :minor 5 :height 10 :width 200 :visible t))
    (setf r2 (defruler :major 25 :minor 5 :height 10 :width 200 :visible t))
    (setf r3 (defruler :major 25 :minor 5 :height 10 :width 200 :visible t))
    (setf r4 (defruler :major 25 :minor 5 :height 10 :width 200 :visible t))
    (setf r5 (defruler :major 25 :minor 5 :height 10 :width 200 :visible t))
    (setf r6 (defruler :major 25 :minor 5 :height 10 :width 200 :visible t))
    (setf r7 (defruler :major 25 :minor 5 :height 10 :width 200 :visible t))
    (setf r8 (defruler :major 25 :minor 5 :height 10 :width 200 :visible t))

    ;; (setf it1 (deftext :title "<1> - alternates left/center/right" :height :auto-min :padding-left 10))
    ;; (setf it2 (deftext :title "<2> - alternates top/middle/bottom" :height :auto-min :padding-left 10))
    ;; (setf it3 (deftext :title "<3> - alterdate v-align top/middle/bottom" :height :auto-min :padding-left 10))
    ;; (setf it4 (deftext :title "<4> - alternates theme-flat/theme-3d" :height :auto-min :padding-left 10))
    ;; (setf icl1 (defcolumn-layout :content (list (list it1 :min-height)
    ;;                                             (list it2 :min-height)
    ;;                                             (list it3 :min-height)
    ;;                                             (list it4 :min-height))))
    ;; (setf iw1 (defwindow +I1X+ +I1Y+ +I1W+ +I1H+))
    ;; (setf iw2 (defwindow +I2X+ +I2Y+ +I2W+ +I2H+))
    
    ;; Test 1
    (setf b1 (defbox :left 35 :top 35 :width 180 :height 280
                     :filled t
                     :thickness 5
                     :title "Test Box 1" :title-position :left-top
                     :v-align :top))
    (setf cl1 (defcolumn-layout :content (list b1)))
    (setf w1 (defwindow 25 25 200 300 :content (list cl1)))

    ;; Test 2
    (setf b2 (defbox :left 260 :top 35 :width 180 :height 280
                     :filled t
                     :thickness 2
                     :title "Test Box 2" :title-position :left-top
                     :v-align :middle))
    (setf cl2 (defcolumn-layout :content (list b2)))
    (setf w2 (defwindow 250 25 200 300 :content (list cl2)))

    ;; Test 3
    (setf b3 (defbox :left 485 :top 35 :width 180 :height 280
                     :filled t
                     :thickness 5
                     :title "Test Box 3" :title-position :left-top
                     :v-align :bottom))
    (setf cl3 (defcolumn-layout :content (list b3)))
    (setf w3 (defwindow 475 25 200 300 :content (list cl3)))

    ;; Test 4
    (setf b4 (defbox :left 710 :top 35 :width 180 :height 280))
    (setf cl4 (defcolumn-layout :content (list b4)))
    (setf w4 (defwindow 700 25 200 300 :content (list cl4)))

    ;; Test 5
    (setf b5 (defbox :left 35 :top 360 :width 180 :height 280
                     :filled t
                     :thickness 5
                     :title "Test Box 5" :title-position :left-top
                     :v-align :top))
    (setf cl5 (defcolumn-layout :content (list b5)))
    (setf w5 (defwindow 25 350 200 300 :content (list cl5)))
    
    ;; Test 6
    (setf b6 (defbox :left 260 :top 360 :width 180 :height 280
                     :filled t
                     :thickness 2
                     :title "Test Box 6" :title-position :left-top
                     :v-align :middle))
    (setf cl6 (defcolumn-layout :content (list b6)))
    (setf w6 (defwindow 250 350 200 300 :content (list cl6)))
    
    ;; Test 7
    (setf b7 (defbox :left 485 :top 360 :width 180 :height 280
                     :filled t
                     :thickness 5
                     :title "Test Box 7" :title-position :left-top
                     :v-align :bottom))
    (setf cl7 (defcolumn-layout :content (list b7)))
    (setf w7 (defwindow 475 350 200 300 :content (list cl7)))
    
    
    ;; Test 8
    (setf b8 (defbox :left 710 :top 360 :width 180 :height 280 :filled t :thickness 10
                     :title "Text Box 8" :title-position :center-middle))
    (setf cl8 (defcolumn-layout :content (list b8)))
    (setf w8 (defwindow 700 350 200 300 :content (list cl8)))
    
    ;; Instructions
    (multiple-value-bind (tmp1 tmp2) (tests-add-instruction data (list "<1> - alternates left/center/right"
                                                                       "<2> - alternates top/middle/bottom"
                                                                       "<3> - alterdate v-align top/middle/bottom"
                                                                       "<4> - alternates theme-flat/theme-3d"))
      (setf iw1 tmp1)
      (setf iw2 tmp2))

    ;; The one in charge
    (let ((objs (list rv1 rv2 r1 r2 r3 r4 r5 r6 r7 r8 iw1 iw2)))
      (setf manager (make-instance 'manager :content objs)))
  
    ;; Adjust vertical rulers
    (setf (left rv1) (- (left w1) 10))
    (setf (top rv1) (top w1))
    (setf (left rv2) (- (left w5) 10))
    (setf (top rv2) (top w5))

    ;; Adjust horizontal rulers
    (setf (left r1) (left w1))
    (setf (top r1) (- (top w1) 10))
    (setf (left r2) (left w2))
    (setf (top r2) (- (top w2) 10))
    (setf (left r3) (left w3))
    (setf (top r3) (- (top w3) 10))
    (setf (left r4) (left w4))
    (setf (top r4) (- (top w4) 10))

    (setf (left r5) (left w5))
    (setf (top r5) (- (top w5) 10))
    (setf (left r6) (left w6))
    (setf (top r6) (- (top w6) 10))
    (setf (left r7) (left w7))
    (setf (top r7) (- (top w7) 10))
    (setf (left r8) (left w8))
    (setf (top r8) (- (top w8) 10))
    )
  
  (setf (box-tests-horiz data) :left)
  (setf (box-tests-vert data) :top)
  (setf (box-tests-vert-align data) :middle)
  (setf (box-tests-theme1 data) (theme-flat-gray))
  (setf (font (box-tests-theme1 data)) (default-font -24))
  (setf (box-tests-theme2 data) (theme-3d-gray))
  (setf (font (box-tests-theme2 data)) (default-font -24))
  (setf (theme (box-tests-manager data)) (box-tests-theme1 data)))

(defmethod tests-destroy ((data (eql *box-data*))))

(defmethod tests-ready (data)
  (with-slots (manager b1 b2 b3) data
    (flet ((update-objects ()
             (let (tp)
               (with-slots (horiz vert vert-align) *box-data*
                 (case horiz
                   (:left
                    (case vert
                      (:top
                       (setq tp :left-top))
                      (:middle
                       (setq tp :left-middle))
                      (:bottom
                       (setq tp :left-bottom))))
                   (:center
                    (case vert
                      (:top
                       (setq tp :center-top))
                      (:middle
                       (setq tp :center-middle))
                      (:bottom
                       (setq tp :center-bottom))))
                   (:right
                    (case vert
                      (:top
                       (setq tp :right-top))
                      (:middle
                       (setq tp :right-middle))
                      (:bottom
                       (setq tp :right-bottom))))))

               (let ((objs (list b1 b2 b3))
                     (va (box-tests-vert-align *box-data*)))
                 (mapcar #'(lambda (obj)
                             (setf (cl-yag:title-position obj) tp)
                             (setf (cl-yag:v-align obj) va))
                         objs))) ))

  
      (defmethod cl-yag::on-char ((key (eql :1)) mods (object (eql manager)) &key)
        (with-slots (horiz) *box-data*
          (case horiz
            (:left
             (setf horiz :center))
            (:center
             (setf horiz :right))
            (:right
             (setf horiz :left))))
        (update-objects))
    
      (defmethod cl-yag::on-char ((key (eql :2)) mods (object (eql manager)) &key)
        (with-slots (vert) *box-data*
          (case vert
            (:top
             (setf vert :middle))
            (:middle
             (setf vert :bottom))
            (:bottom
             (setf vert :top))))
        (update-objects))

      (defmethod cl-yag::on-char ((key (eql :3)) mods (object (eql manager)) &key)
        )

      (defmethod cl-yag::on-char ((key (eql :4)) mods (object (eql manager)) &key)
        (with-slots (theme1 theme2) *box-data*
          (if (eql (theme object) theme1)
              (setf (theme object) theme2)
              (setf (theme object) theme1))))
      )))

(defun box-tests-main ()
  (tests-main *box-data*))

