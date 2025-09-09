(in-package :cl-yag-tests)

;; Standard window sizes
;; Size 200,300
;; Spacing 25,25
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
(defconstant +W5T+ (+ +W1L+ +WH+ +WHS+) "Standard window 5 top")
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

;; Tall window sizes
;; Size +WW+, (+ +WH+ +WHS+ +WH+)
;; Spacing 25,25
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
;; Size (+ +WW+ +WWS+ +WW+),300
;; Spacing 25,25
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
;; Size (+ +WW+ +WWS+ +WW+),(+ +WH+ +WHS+ +WH+)
;; Spacing 25, 25
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
;; TODO: Hard coded ... should be calculated
(defconstant +I1H+ 115)
(defconstant +I1L+ 25)
(defconstant +I1T+ 675)
(defconstant +I1W+ 400)

(defconstant +I2H+ 115)
(defconstant +I2L+ 500)
(defconstant +I2T+ 675)
(defconstant +I2W+ 400)

(defmacro deftests-ruler (win-type number &rest rest &key &allow-other-keys)
  "Creates tests window ruler of type and number.  Automatically positioned and
sized according to type and window number."

  (let ((local-win-type (gensym))
        (local-number (gensym))
        (local-number-string (gensym))
        (generator (gensym)))
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
             (,local-number ,number)
             (,local-number-string))
         (v:info :debug "win-type:~a number:~a" ,local-win-type ,local-number)
         (setq ,local-number-string (format nil "~1d" ,local-number))
         (ccase ,local-win-type
           (:standard
            (assert (<= ,local-number 8))
            (,generator (symbol-value (a:symbolicate "+W" ,local-number-string "L+"))
                        (- (symbol-value (a:symbolicate "+W" ,local-number-string "T+")) 10)
                        (symbol-value (a:symbolicate "+W" ,local-number-string "W+"))
                        10))
           (:tall
            (assert (<= ,local-number 4))
            (,generator (symbol-value (a:symbolicate "+T-W" ,local-number-string "L+"))
                        (- (symbol-value (a:symbolicate "+T-W" ,local-number-string "T+")) 10)
                        (symbol-value (a:symbolicate "+T-W" ,local-number-string "W+"))
                        10))
           (:wide
            (assert (<= ,local-number 4))
            (,generator (symbol-value (a:symbolicate "+W-W" ,local-number-string "L+"))
                        (- (symbol-value (a:symbolicate "+W-W" ,local-number-string "T+")) 10)
                        (symbol-value (a:symbolicate "+W-W" ,local-number-string "W+"))
                        10))
           (:wide-tall
            (assert (<= ,local-number 2))
            (,generator (symbol-value (a:symbolicate "+WT-W" ,local-number-string "L+"))
                        (- (symbol-value (a:symbolicate "+WT-W" ,local-number-string "T+")) 10)
                        (symbol-value (a:symbolicate "+WT-W" ,local-number-string "W+"))
                        10))
           (:standard-vertical
            (assert (<= ,local-number 2))
            (if (= ,local-number 2)
                (setq ,local-number-string "5"))
            (,generator (- (symbol-value (a:symbolicate "+W" ,local-number-string "L+")) 10)
                        (symbol-value (a:symbolicate "+W" ,local-number-string "T+"))
                        10
                        (symbol-value (a:symbolicate "+W" ,local-number-string "H+"))
                        t))
           (:tall-vertical
            (assert (<= ,local-number 1))
            (,generator (- (symbol-value (a:symbolicate "+T-W" ,local-number-string "L+")) 10)
                        (symbol-value (a:symbolicate "+T-W" ,local-number-string "T+"))
                        10
                        (symbol-value (a:symbolicate "+T-W" ,local-number-string "H+"))
                        t t)            
            )
           (:wide-vertical
            (if (= ,local-number 2)
                (setq ,local-number-string "3"))
            (assert (<= ,local-number 2))
            (,generator (- (symbol-value (a:symbolicate "+W-W" ,local-number-string "L+")) 10)
                        (symbol-value (a:symbolicate "+W-W" ,local-number-string "T+"))
                        10
                        (symbol-value (a:symbolicate "+W-W" ,local-number-string "H+"))
                        t))
           (:wide-tall-vertical
            (assert (<= ,local-number 1))
            (,generator (- (symbol-value (a:symbolicate "+WT-W" ,local-number-string "L+")) 10)
                        (symbol-value (a:symbolicate "+WT-W" ,local-number-string "T+"))
                        10
                        (symbol-value (a:symbolicate "+WT-W" ,local-number-string "H+"))
                        t t)))))))

;; TODO: Figure out how to deduplicate the defwindow's.  I've tried symbol-macrolet
;;       and couldn't get it to work.
(defmacro deftests-window (win-type number &rest rest &key &allow-other-keys)
  "Create tests window of type and number.  Determines size and location from type and number."
  
  (let ((local-win-type (gensym))
        (local-number (format nil "~1d" number)))
    `(let ((,local-win-type ,win-type))
       (ecase ,local-win-type
         (:standard
          (symbol-macrolet ((prefix "+W"))
            (defwindow (symbol-value (a:symbolicate prefix ,local-number "L+"))
                (symbol-value (a:symbolicate prefix ,local-number "T+"))
              (symbol-value (a:symbolicate prefix ,local-number "W+"))
              (symbol-value (a:symbolicate prefix ,local-number "H+"))
              ,@rest)))
         (:tall
          (symbol-macrolet ((prefix "+T-W"))
            (defwindow (symbol-value (a:symbolicate prefix ,local-number "L+"))
                (symbol-value (a:symbolicate prefix ,local-number "T+"))
              (symbol-value (a:symbolicate prefix ,local-number "W+"))
              (symbol-value (a:symbolicate prefix ,local-number "H+"))
              ,@rest)))
         (:wide
          (symbol-macrolet ((prefix "+W-W"))
            (defwindow (symbol-value (a:symbolicate prefix ,local-number "L+"))
                (symbol-value (a:symbolicate prefix ,local-number "T+"))
              (symbol-value (a:symbolicate prefix ,local-number "W+"))
              (symbol-value (a:symbolicate prefix ,local-number "H+"))
              ,@rest)))
         (:wide-tall
          (symbol-macrolet ((prefix "+WT-W"))
            (defwindow (symbol-value (a:symbolicate prefix ,local-number "L+"))
                (symbol-value (a:symbolicate prefix ,local-number "T+"))
              (symbol-value (a:symbolicate prefix ,local-number "W+"))
              (symbol-value (a:symbolicate prefix ,local-number "H+"))
              ,@rest)))))))

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
paint for manager object.  Display is flipped automatically after call
regardless of result."))

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

(defun tests-toggle-theme (data)
  (with-slots (manager theme1 theme2) data
    (if (eql (theme manager) theme1)
        (setf (theme manager) theme2)
        (setf (theme manager) theme1))))

