(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;; Standard window sizes
(defconstant +WH+ 300 "Standard window height")
(defconstant +WHS+ 25 "Standard window vertical spacing")
(defconstant +WW+ 200 "Standard window width")
(defconstant +WWS+ 25 "Standard window horizontal spacing")

(defconstant +W1H+ +WH+ "Standard window 1 height")
(defconstant +W1L+ +WWS+ "Standard window 1 left")
(defconstant +W1T+ +WHS+ "Standard window 1 top")
(defconstant +W1W+ +WW+ "Standard window 1 width")

(defconstant +W2H+ +WH+ "Standard window 2 height")
(defconstant +W2L+ (+ +W1L+ +W1W+ +WHS+) "Standard window 2 left")
(defconstant +W2T+ +W1T+ "Standard window 2 top")
(defconstant +W2W+ +WW+ "Standard window 2 width")

(defconstant +W3H+ +WH+ "Standard window 3 height")
(defconstant +W3L+ (+ +W2L+ +W2W+ +WHS+) "Standard window 3 left")
(defconstant +W3T+ +W1T+ "Standard window 3 top")
(defconstant +W3W+ +WW+ "Standard window 3 width")

(defconstant +W4H+ +WH+ "Standard window 4 height")
(defconstant +W4L+ (+ +W3L+ +W2W+ +WHS+) "Standard window 4 left")
(defconstant +W4T+ +W1T+ "Standard window 4 top")
(defconstant +W4W+ +WW+ "Standard window 4 width")

(defconstant +W5H+ +WH+ "Standard window 5 height")
(defconstant +W5L+ +W1L+ "Standard window 5 left")
(defconstant +W5T+ (+ +W1T+ +W1H+ +WHS+) "Standard window 5 top")
(defconstant +W5W+ +WW+ "Standard window 5 width")

(defconstant +W6H+ +WH+ "Standard window 6 height")
(defconstant +W6L+ +W2L+ "Standard window 6 left")
(defconstant +W6T+ +W5T+ "Standard window 6 top")
(defconstant +W6W+ +WW+ "Standard window 6 width")

(defconstant +W7H+ +WH+ "Standard window 7 height")
(defconstant +W7L+ +W3L+ "Standard window 7 left")
(defconstant +W7T+ +W5T+ "Standard window 7 top")
(defconstant +W7W+ +WW+ "Standard window 7 width")

(defconstant +W8H+ +WH+ "Standard window 8 height")
(defconstant +W8L+ +W4L+ "Standard window 8 left")
(defconstant +W8T+ +W5T+ "Standard window 8 top")
(defconstant +W8W+ +WW+ "Standard window 8 width")

;; Full window size
(defconstant +F-WH+ (+ +WH+ +WWS+ +WH+) "Full window height")
(defconstant +F-WHS+ +WHS+ "Full window vertical spacing")
(defconstant +F-WW+ (+ +WW+ +WWS+ +WW+ +WWS+ +WW+ +WWS+ +WW+) "Full window width")
(defconstant +F-WWS+ +WWS+ "Full window horizontal spacing")

(defconstant +F-W1H+ +F-WH+ "Full window 1 height")
(defconstant +F-W1L+ +F-WWS+ "Full window 1 left")
(defconstant +F-W1T+ +F-WHS+ "Full window 1 top")
(defconstant +F-W1W+ +F-WW+ "Full window 1 width")

;; Tall window sizes
(defconstant +T-WH+ (+ +WH+ +WHS+ +WH+) "Tall window height")
(defconstant +T-WHS+ +WHS+ "Tall window vertical spacing")
(defconstant +T-WW+ +WW+ "Tall window width")
(defconstant +T-WWS+ +WWS+ "Tall window horizontal spacing")

(defconstant +T-W1H+ +T-WH+ "Tall window 1 height")
(defconstant +T-W1L+ +T-WWS+ "Tall window 1 left")
(defconstant +T-W1T+ +T-WHS+ "Tall window 1 top")
(defconstant +T-W1W+ +T-WW+ "Tall window 1 width")

(defconstant +T-W2H+ +T-WH+ "Tall window 2 height")
(defconstant +T-W2L+ (+ +T-W1L+ +T-W1W+ +T-WWS+) "Tall window 2 left")
(defconstant +T-W2T+ +T-WHS+ "Tall window 2 top")
(defconstant +T-W2W+ +T-WW+ "Tall window 2 width")

(defconstant +T-W3H+ +T-WH+ "Tall window 3 height")
(defconstant +T-W3L+ (+ +T-W2L+ +T-W2W+ +T-WWS+) "Tall window 3 left")
(defconstant +T-W3T+ +T-WHS+ "Tall window 3 top")
(defconstant +T-W3W+ +T-WW+ "Tall window 3 width")

(defconstant +T-W4H+ +T-WH+ "Tall window 4 height")
(defconstant +T-W4L+ (+ +T-W3L+ +T-W3W+ +T-WWS+) "Tall window 4 left")
(defconstant +T-W4T+ +T-WHS+ "Tall window 4 top")
(defconstant +T-W4W+ +T-WW+ "Tall window 4 width")

;; Wide window sizes
(defconstant +W-WH+ +WH+ "Wide window height")
(defconstant +W-WHS+ +WHS+ "Wide window vertical spacing")
(defconstant +W-WW+ (+ +WW+ +WWS+ +WW+) "Wide window width")
(defconstant +W-WWS+ +WWS+ "Wide window horizontal spacing")

(defconstant +W-W1H+ +W-WH+ "Wide window 1 height")
(defconstant +W-W1L+ +W-WWS+ "Wide window 1 left")
(defconstant +W-W1T+ +W-WHS+ "Wide window 1 top")
(defconstant +W-W1W+ +W-WW+ "Wide window 1 width")

(defconstant +W-W2H+ +W-WH+ "Wide window 2 height")
(defconstant +W-W2L+ (+ +W-W1L+ +W-W1W+ +W-WWS+) "Wide window 2 left")
(defconstant +W-W2T+ +W-WHS+ "Wide window 2 top")
(defconstant +W-W2W+ +W-WW+ "Wide window 2 width")

(defconstant +W-W3H+ +W-WH+ "Wide window 3 height")
(defconstant +W-W3L+ +W-W1L+ "Wide window 3 left")
(defconstant +W-W3T+ (+ +W-W1T+ +W-W1H+ +W-WHS+) "Wide window 3 top")
(defconstant +W-W3W+ +W-WW+ "Wide window 3 width")

(defconstant +W-W4H+ +W-WH+ "Wide window 4 height")
(defconstant +W-W4L+ +W-W2L+ "Wide window 4 left")
(defconstant +W-W4T+ +W-W3T+ "Wide window 4 top")
(defconstant +W-W4W+ +W-WW+ "Wide window 4 width")

;; Wide and Tall window sizes
(defconstant +WT-WH+ (+ +WH+ +WHS+ +WH+) "Wide and Tall window height")
(defconstant +WT-WHS+ +WHS+ "Wide and Tall window vertical spacing")
(defconstant +WT-WW+ (+ +WW+ +WWS+ +WW+) "Wide and Tall window width")
(defconstant +WT-WWS+ +WWS+ "Wide and Tall window horizontal spacing")

(defconstant +WT-W1H+ +WT-WH+ "Wide and Tall window 1 height")
(defconstant +WT-W1L+ +WT-WWS+ "Wide and Tall window 1 left")
(defconstant +WT-W1T+ +WT-WHS+ "Wide and Tall window 1 top")
(defconstant +WT-W1W+ +WT-WW+ "Wide and Tall window 1 width")

(defconstant +WT-W2H+ +WT-WH+ "Wide and Tall window 2 height")
(defconstant +WT-W2L+ (+ +WT-W1L+ +WT-W1W+ +WT-WWS+) "Wide and Tall window 2 left")
(defconstant +WT-W2T+ +WT-WHS+ "Wide and Tall window 2 top")
(defconstant +WT-W2W+ +WT-WW+ "Wide and Tall window 2 width")

;; Standard instruction sizes
(defconstant +IH+ 115)
(defconstant +IHS+ +WHS+)
(defconstant +IW+ 400)
(defconstant +IWS+ +WWS+)

(defconstant +I1H+ +IH+)
(defconstant +I1L+ +IWS+)
(defconstant +I1T+ (+ +WWS+ +WH+ +WWS+ +WH+ +WWS+))
(defconstant +I1W+ +IW+)

(defconstant +I2H+ +IH+)
(defconstant +I2L+ (- (+ +W-W2L+ +W-W2W+) +IW+))
(defconstant +I2T+ (+ +WWS+ +WH+ +WWS+ +WH+ +WWS+))
(defconstant +I2W+ +IW+)

;;;; MACROS ===================================================================

(defmacro tests-window-constant (type window field)
  "Returns the value of the window constant.

type   = member (:standard :full :tall :wide :wide-tall
window = window number.  Max windows per type:
         :standard  4 (4x2)
         :full      1 (1x1)
         :tall      4 (4x1)
         :wide      4 (2x2)
         :wide-tall 2 (1x2)
field  = character of desired field, as follows:
         #\H = height
         #\L = left
         #\T = top
         #\W = width"
  
  (a:with-gensyms (ltype lwindow lfield number-string)
    `(let ((,ltype ,type)
           (,lwindow ,window)
           (,lfield ,field))
       (let ((,number-string (format nil "~1d" ,lwindow)))
         (case ,ltype
           (:full
            (symbol-value (a:symbolicate "+F-W" ,number-string ,lfield "+")))
           (:standard
            (symbol-value (a:symbolicate "+W" ,number-string ,lfield "+")))
           (:tall
            (symbol-value (a:symbolicate "+T-W" ,number-string ,lfield "+")))
           (:wide
            (symbol-value (a:symbolicate "+W-W" ,number-string ,lfield "+")))
           (:wide-tall
            (symbol-value (a:symbolicate "+WT-W" ,number-string ,lfield "+"))))))))

(defmacro deftests-ruler (win-type number &rest rest &key &allow-other-keys)
  "Creates tests window ruler of type and number.  Automatically positioned and
sized according to type and window number."

  (a:with-gensyms (local-win-type local-number generator)
    `(flet ((,generator (l t_ w h &optional (v nil) (r100-25-5 nil))
              (if r100-25-5
                  (ruler-100-25-5 :visible t :line-color (al:map-rgb-f 1 0 0) :vertical v
                                  :div-100-color (al:map-rgb-f 1 0 0) :div-100-extent 1
                                  :div-25-color (al:map-rgb-f 0.8 0 0) :div-25-extent 0.6667
                                  :div-5-color (al:map-rgb-f 0.6 0 0) :div-5-extent 0.3333
                                  :left l :top t_ :width w :height h ,@rest)
                  (ruler-25-5 :visible t :line-color (al:map-rgb-f 1 0 0) :vertical v
                              :div-25-color (al:map-rgb-f 1 0 0) :div-25-extent 1
                              :div-5-color (al:map-rgb-f 0.6 0 0) :div-5-extent 0.5
                              :left l :top t_ :width w :height h ,@rest))))
       (let ((,local-win-type ,win-type)
             (,local-number ,number))
         (ccase ,local-win-type
           (:full
            (assert (= ,local-number 1))
            (,generator +F-W1L+ (- +F-W1T+ 10) +F-W1W+ 10 nil t))
           (:full-vertical
            (assert (= ,local-number 1))
            (,generator (- +F-W1L+ 10) +F-W1T+ 10 +F-W1H+ t t))
           (:standard
            (assert (<= ,local-number 8))
            (,generator (tests-window-constant ,local-win-type ,local-number #\L)
                        (- (tests-window-constant ,local-win-type ,local-number #\T) 10)
                        (tests-window-constant ,local-win-type ,local-number #\W)
                        10))
           (:standard-vertical
            (assert (<= ,local-number 2))
            (if (= ,local-number 2)
                (setq ,local-number 5))
            (,generator (- (tests-window-constant :standard ,local-number #\L) 10)
                        (tests-window-constant :standard ,local-number #\T)
                        10
                        (tests-window-constant :standard ,local-number #\H)
                        t))
           (:tall
            (assert (<= ,local-number 4))
            (,generator (tests-window-constant ,local-win-type ,local-number #\L)
                        (- (tests-window-constant ,local-win-type ,local-number #\T) 10)
                        (tests-window-constant ,local-win-type ,local-number #\W)
                        10))
           (:tall-vertical
            (assert (<= ,local-number 1))
            (,generator (- (tests-window-constant :tall ,local-number #\L) 10)
                        (tests-window-constant :tall ,local-number #\T)
                        10
                        (tests-window-constant :tall ,local-number #\H)
                        t t))
           (:wide
            (assert (<= ,local-number 4))
            (,generator (tests-window-constant ,local-win-type ,local-number #\L)
                        (- (tests-window-constant ,local-win-type ,local-number #\T) 10)
                        (tests-window-constant ,local-win-type ,local-number #\W)
                        10))
           (:wide-tall
            (assert (<= ,local-number 2))
            (,generator (tests-window-constant ,local-win-type ,local-number #\L)
                        (- (tests-window-constant ,local-win-type ,local-number #\T) 10)
                        (tests-window-constant ,local-win-type ,local-number #\W)
                        10))
           (:wide-tall-vertical
            (assert (<= ,local-number 1))
            (,generator (- (tests-window-constant :wide-tall ,local-number #\L) 10)
                        (tests-window-constant :wide-tall ,local-number #\T)
                        10
                        (tests-window-constant :wide-tall ,local-number #\H)
                        t t))
           (:wide-vertical
            (assert (<= ,local-number 2))
            (if (= ,local-number 2)
                (setq ,local-number 3))
            (,generator (- (tests-window-constant :wide ,local-number #\L) 10)
                        (tests-window-constant :wide ,local-number #\T)
                        10
                        (tests-window-constant :wide ,local-number #\H)
                        t)))))))

(defmacro deftests-status (window-type window rows per-row title row column)
  (declare (ignorable window-type window rows per-row title row column))
  (a:with-gensyms (lwin-type lwin lrows lper-row ltitle lrow lcol left top width height)
    `(let ((,lwin-type ,window-type)
           (,lwin ,window)
           (,lrows ,rows)
           (,lper-row ,per-row)
           (,ltitle ,title)
           (,lrow ,row)
           (,lcol ,column)
           (,height 24))
       (let ((,width (/ (- (tests-window-constant ,lwin-type ,lwin #\W) 20) ,lper-row))
             (,top (+ (- (+ (tests-window-constant ,lwin-type ,lwin #\T)
                            (tests-window-constant ,lwin-type ,lwin #\H))
                         (* ,lrows 25))
                      (* (1- ,lrow) 25))))
         (let ((,left (+ (tests-window-constant ,lwin-type ,lwin #\L) 10 (* (1- ,lcol) ,width))))
           (deftext :title ,ltitle :left ,left :top ,top :width ,width :height ,height))))))

;; TODO: Figure out how to deduplicate the defwindow's.  I've tried symbol-macrolet
;;       and couldn't get it to work.
(defmacro deftests-window (win-type number &rest rest &key &allow-other-keys)
  "Create tests window of type and number.  Determines size and location from type and number."
  
  (let ((local-win-type (gensym))
        (local-number (format nil "~1d" number)))
    `(let ((,local-win-type ,win-type))
       (defwindow (tests-window-constant ,local-win-type ,local-number #\L)
           (tests-window-constant ,local-win-type ,local-number #\T)
         (tests-window-constant ,local-win-type ,local-number #\W)
         (tests-window-constant ,local-win-type ,local-number #\H)
         ,@rest))))

(defgeneric tests-command-0 (data) (:documentation "Called when 0 is pressed."))
(defgeneric tests-command-1 (data) (:documentation "Called when 1 is pressed."))
(defgeneric tests-command-2 (data) (:documentation "Called when 2 is pressed."))
(defgeneric tests-command-3 (data) (:documentation "Called when 3 is pressed."))
(defgeneric tests-command-4 (data) (:documentation "Called when 4 is pressed."))
(defgeneric tests-command-5 (data) (:documentation "Called when 5 is pressed."))
(defgeneric tests-command-6 (data) (:documentation "Called when 6 is pressed."))
(defgeneric tests-command-7 (data) (:documentation "Called when 7 is pressed."))
(defgeneric tests-command-8 (data) (:documentation "Called when 8 is pressed."))
(defgeneric tests-command-9 (data) (:documentation "Called when 9 is pressed."))

(defgeneric tests-command-update (data) (:documentation "Called after calling a tests-command-* and it returns t."))

(defgeneric tests-create (data) (:documentation "Create windows/object/data for tests."))

(defgeneric tests-destroy (data) (:documentation "Clean up data created for tests."))

(defgeneric tests-ready (data) (:documentation "Called just before entering event loop."))

(defgeneric tests-render (data) (:documentation "Called to render display. If it returns nil, will clear screen and call
paint for manager object. Display is flipped automatically after call
regardless of result."))

;;;; TESTS-INSTRUCTIONS =======================================================

(defstruct (tests-instructions-data (:conc-name tests-instructions-))
  iw1 iw2
  icl1 icl2
  it1 it2 it3 it4 it5 it6 it7 it8)

(defun tests-instructions-create (data left &optional right)
  (macrolet ((make-inst (used field title is-left)
               (let ((u (gensym)))
                 `(let ((,u ,used))
                    (when ,u
                      ,(if is-left
                           `(setf ,field (deftext :title ,title :height :auto-min :width :auto :padding-left 10))
                           `(setf ,field (deftext :title ,title :height :auto-min :width :auto :padding-right 10 :h-align :right))))
                    ,u))))
    
    (with-slots (iw1 iw2 icl1 icl2 it1 it2 it3 it4 it5 it6 it7 it8) data
      (let ((num-left (length left))
            (num-right 0)
            widgets-l widgets-r)
        
        (assert (<= num-left 4))
        (unless (eql right nil)
          (setq num-right (length right))
          (assert (<= num-right 4)))

        ;; Create left instructions
        (if (make-inst (>= num-left 4) it4 (fourth left) t) (push (list it4 :min-height) widgets-l))
        (if (make-inst (>= num-left 3) it3 (third left) t) (push (list it3 :min-height) widgets-l))
        (if (make-inst (>= num-left 2) it2 (second left) t) (push (list it2 :min-height) widgets-l))
        (if (make-inst (>= num-left 1) it1 (first left) t) (push (list it1 :min-height) widgets-l))

        ;; Create right instructions
        (if (make-inst (>= num-right 4) it8 (fourth right) nil) (push (list it8 :min-height) widgets-r))
        (if (make-inst (>= num-right 3) it7 (third right) nil) (push (list it7 :min-height) widgets-r))
        (if (make-inst (>= num-right 2) it6 (second right) nil) (push (list it6 :min-height) widgets-r))
        (if (make-inst (>= num-right 1) it5 (first right) nil) (push (list it5 :min-height) widgets-r))

        ;; Final left preparations
        (setf icl1 (defcolumn-layout))
        (when (> (length widgets-l) 0)
          (setf (content icl1) widgets-l))
        (setf iw1 (defwindow +I1L+ +I1T+ +I1W+ +I1H+ :content (list icl1)))
        (assert (not (eql iw1 nil)))

        ;; Final right preparations
        (setf icl2 (defcolumn-layout))
        (when (> (length widgets-r) 0)
          (setf (content icl2) widgets-r))
        (setf iw2 (defwindow +I2L+ +I2T+ +I2W+ +I2H+ :content (list icl2)))
        (assert (not (eql iw2 nil)))

        (values iw1 iw2)))))

;;;; TESTS-RULERS =============================================================

(defstruct (tests-rulers-data (:include tests-instructions-data)
                              (:conc-name tests-rulers-))
  
  rv1 rv2
  rh1 rh2 rh3 rh4 rh5 rh6 rh7 rh8)

(defun tests-rulers-create-full (data &key (r1 '(:full 1 rh1))
                                        (rv1 '(:full-vertical 1 rv1)))
  (let (rulers)
    (mapc #'(lambda (arg)
              (when arg
                (let ((a1 (first arg))
                      (a2 (second arg))
                      (a3 (third arg)))
                  (setf (slot-value data a3) (deftests-ruler a1 a2))
                  (push (slot-value data a3) rulers))))
          (list rv1 r1))
    (values-list rulers)))

(defun tests-rulers-create-standard (data &key
                                            (r1 '(:standard 1 rh1))
                                            (r2 '(:standard 2 rh2))
                                            (r3 '(:standard 3 rh3))
                                            (r4 '(:standard 4 rh4))
                                            (r5 '(:standard 5 rh5))
                                            (r6 '(:standard 6 rh6))
                                            (r7 '(:standard 7 rh7))
                                            (r8 '(:standard 8 rh8))
                                            (rv1 '(:standard-vertical 1 rv1))
                                            (rv2 '(:standard-vertical 2 rv2)))
  (let (rulers)
    (mapc #'(lambda (arg)
              (when arg
                (let ((a1 (first arg))
                      (a2 (second arg))
                      (a3 (third arg)))
                  (setf (slot-value data a3) (deftests-ruler a1 a2))
                  (push (slot-value data a3) rulers))))
          (list rv1 rv2 r1 r2 r3 r4 r5 r6 r7 r8))
    (values-list rulers)))

(defun tests-rulers-create-tall (data &key (r1 '(:tall 1 rh1))
                                        (r2 '(:tall 2 rh2))
                                        (r3 '(:tall 3 rh3))
                                        (r4 '(:tall 4 rh4))
                                        (rv1 '(:tall-vertical 1 rv1)))
  (let (rulers)
    (mapc #'(lambda (arg)
              (when arg
                (let ((a1 (first arg))
                      (a2 (second arg))
                      (a3 (third arg)))
                  (setf (slot-value data a3) (deftests-ruler a1 a2))
                  (push (slot-value data a3) rulers))))
          (list rv1 r1 r2 r3 r4))
    (values-list rulers)))

(defun tests-rulers-create-wide (data &key (r1 '(:wide 1 rh1))
                                        (r2 '(:wide 2 rh2))
                                        (r3 '(:wide 3 rh3))
                                        (r4 '(:wide 4 rh4))
                                        (rv1 '(:wide-vertical 1 rv1))
                                        (rv2 '(:wide-vertical 2 rv2)))
  (let (rulers)
    (mapc #'(lambda (arg)
              (when arg
                (let ((a1 (first arg))
                      (a2 (second arg))
                      (a3 (third arg)))
                  (setf (slot-value data a3) (deftests-ruler a1 a2))
                  (push (slot-value data a3) rulers))))
          (list rv1 rv2 r1 r2 r3 r4))
    (values-list rulers)))

(defun tests-rulers-create-wide-tall (data &key (r1 '(:wide-tall 1 rh1))
                                             (r2 '(:wide-tall 2 rh2))
                                             (rv1 '(:wide-tall-vertical 1 rv1)))
  (let (rulers)
    (mapc #'(lambda (arg)
              (when arg
                (let ((a1 (first arg))
                      (a2 (second arg))
                      (a3 (third arg)))
                  (setf (slot-value data a3) (deftests-ruler a1 a2))
                  (push (slot-value data a3) rulers))))
          (list rv1 r1 r2))
    (values-list rulers)))


;;;; TESTS-THEME ==============================================================

(defstruct (tests-theme-data (:include tests-rulers-data)
                             (:conc-name tests-theme-))
  theme1 theme2)

(defun tests-toggle-theme (data)
  (with-slots (manager theme1 theme2) data
    (if (eql (theme manager) theme1)
        (setf (theme manager) theme2)
        (setf (theme manager) theme1))))

;;;; TESTS MISC ===============================================================

(defun tests-toggle-interior-color (data widgets)
  (with-slots (manager theme1) data
    (let ((ic1 (interior-color theme1))
          (ic2 (al:map-rgb-f 1 0 0)))
      (dolist (w widgets)
        (unless (eql w nil)
          (with-local-slots ((ic interior-color)) w
            (if (or (equal ic ic1)
                    (eql ic nil))
                (setf (interior-color w) ic2)
                (setf (interior-color w) ic1))))))))

