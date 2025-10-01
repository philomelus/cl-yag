(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defstruct (layout-tests-data (:include tests-data)
                              (:conc-name layout-tests-))
  b11 b12 t1 l1 st11 st12 st13 st14 st15 th1 tr1
  b2 t2 l2 st21 st22 st23 st24 st25 st26 st27 st28 st29 st210
  b3 bt3 l3 st31 st32 st33 st34
  w1 w2 w3 w4
  )

(defparameter *layout-data* (make-layout-tests-data))

(defmethod tests-create ((data (eql *layout-data*)))
  (let (widgets)
    (with-slots (manager
                 b11 b12 t1 l1 st11 st12 st13 st14 st15 th1 tr1
                 b2 t2 l2 st21 st22 st23 st24 st25 st26 st27 st28 st29 st210
                 b3 bt3 l3 st31 st32 st33 st34
                 w1 w2 w3 w4
                 )
        data

      ;; Test 1
      (setf b11 (defborder :thickness 10))
      (setf t1 (deftext :title "One" :h-align :center :v-align :middle))
      (setf (border t1) b11)
      (setf (spacing t1) 10)
      (setf (padding t1) 10)
      (setf l1 (deflayout :content `(,t1)))
      (setf b12 (defborder :thickness 0))
      (setf (border (layout-cell l1)) b12)
      ;; (setf th1 (deftheme-flat-all :color (al:map-rgb-f 1 0 0)))
      ;; (setf tr1 (defthemer th1 l1))
      (setf w1 (deftests-window :wide 1 :content `(,l1)))
      (push w1 widgets)

      ;; Test 2
      (setf b2 (defborder :thickness 10))
      (setf t2 (deftext :title "Two" :h-align :center :v-align :middle
                        :width :auto-min :height :auto-min))
      (setf (border t2) b2)
      (setf (spacing t2) 10)
      (setf (padding t2) 10)
      (setf l2 (deflayout :content `(,t2)))
      (setf w2 (deftests-window :wide 2 :content `(,l2)))
      (push w2 widgets)
      
      ;; Test 3
      (setf b3 (defborder :thickness 10))
      (setf bt3 (defbutton :title "Two" :h-align :center :v-align :middle
                           :width :auto-min :height :auto-min))
      (setf (border bt3) b3)
      (setf (spacing bt3) 10)
      (setf (padding bt3) 10)
      (setf l3 (deflayout :content `(,bt3)))
      (setf w3 (deftests-window :wide 3 :content `(,l3)))
      (push w3 widgets)
      
      ;; Status (Test 4)
      ;; Test 1 status
      (setf st11 (deftests-status :wide 4 12 2 "P: 0" 1 1))
      (setf st12 (deftests-status :wide 4 12 2 "S: 0" 1 2))
      (setf st13 (deftests-status :wide 4 12 2 "B: 0" 2 1))
      (setf st14 (deftests-status :wide 4 12 2 "H: NONE" 3 1))
      (setf st15 (deftests-status :wide 4 12 2 "V: NONE" 3 2))
      ;; Test 2 status
      (setf st21 (deftests-status :wide 4 12 2 "LH: NONE" 5 1))
      (setf st22 (deftests-status :wide 4 12 2 "LV: NONE" 5 2))
      (setf st23 (deftests-status :wide 4 12 2 "MH: NONE" 6 1))
      (setf st24 (deftests-status :wide 4 12 2 "MV: NONE" 6 2))
      (setf st25 (deftests-status :wide 4 12 2 "WC: NONE" 7 1))
      (setf st26 (deftests-status :wide 4 12 2 "HC: NONE" 7 2))
      (setf st27 (deftests-status :wide 4 12 2 "T2T: 0" 8 1))
      (setf st28 (deftests-status :wide 4 12 2 "T2L: 0" 8 2))
      (setf st29 (deftests-status :wide 4 12 2 "T2W: 0" 9 1))
      (setf st210 (deftests-status :wide 4 12 2 "T2H: 0" 9 2))
      ;; Test 3 status
      (setf st31 (deftests-status :wide 4 12 2 "BT3L: 0" 11 1))
      (setf st32 (deftests-status :wide 4 12 2 "BT3T: 0" 11 2))
      (setf st33 (deftests-status :wide 4 12 2 "BT3W: 0" 12 1))
      (setf st34 (deftests-status :wide 4 12 2 "BT3H: 0" 12 2))
      (setf w4 (deftests-window :wide 4 :content `(,st11 ,st12 ,st13 ,st14 ,st15
                                                         ,st21 ,st22 ,st23 ,st24 ,st25 ,st26
                                                         ,st27 ,st28 ,st29 ,st210
                                                         ,st31 ,st32 ,st33 ,st34)))
      (push w4 widgets)
      
      ;; Instructions
      (mapc #'(lambda (o) (push o widgets))
            (multiple-value-list (tests-instructions-create
                                  data
                                  (list "k/K  = Border +/- Padding = p/P"
                                        "s/S  = Spacing +/-"
                                        "56   = :auto/:auto-min/:auto-max"
                                        "1234 = layout options x/y/w/h")
                                  (list "h = :left/:center/:right"
                                        "v = :top/:middle/:bottom"
                                        "c = window interior red/default"
                                        "t = theme-flat/theme-3d"))))

      ;; Rulers
      (mapc #'(lambda (o) (push o widgets))
            (multiple-value-list (tests-rulers-create-wide data :r4 nil)))
      
      ;; The one in charge
      (setf manager (make-instance 'manager :content widgets)))))

(defmethod tests-destroy ((data (eql *layout-data*)))
  (let ((args `((eql ,data))))
    (cleanup-method tests-command-1 args)
    (cleanup-method tests-command-2 args)
    (cleanup-method tests-command-3 args)
    (cleanup-method tests-command-4 args)
    (cleanup-method tests-command-5 args)
    (cleanup-method tests-command-6 args)
    (cleanup-method tests-command-update args)
    (cleanup-method tests-get-h-align args)
    (cleanup-method tests-get-interior-color args)
    (cleanup-method tests-get-padding args)
    (cleanup-method tests-get-rulers args)
    (cleanup-method tests-get-spacing args)
    (cleanup-method tests-get-thickness args)
    (cleanup-method tests-get-v-align args)
    ))

(defmethod tests-ready ((layout-data (eql *layout-data*)))

  (defmethod tests-command-1 ((data (eql layout-data)))
    (with-slots (l2 t2
                 l3 bt3)
        data
      (let ((child (first (content l2))))
        ;; Child has options?
        (when (atom child)
          ;; No, so move to :min-width
          (setf (content l2) (list (list t2 :left))
                (content l3) (list (list bt3 :left)))
          (return-from tests-command-1 t))
        ;; Yes
        (let ((options (rest child))
              other-opts new-opt)
          (let ((our-opts (intersection '(:center :left :right) options)))
            ;; Which of our options?
            (cond
              ((member :left our-opts)
               (setq other-opts (delete :left options)
                     new-opt :center))
              ((member :center our-opts)
               (setq other-opts (delete :center options)
                     new-opt :right))
              ((member :right our-opts)
               (setq other-opts (delete :right options)))
              (t
               (setq other-opts options
                     new-opt :left)))
            ;; Build new contents
            (let ((new-l2 ())
                  (new-l3 ()))
              (unless (null new-opt)
                (push new-opt new-l2)
                (push new-opt new-l3))
              (when (> (length other-opts) 0)
                (dolist (o other-opts)
                  (push o new-l2)
                  (push o new-l3)))
              (push t2 new-l2)
              (push bt3 new-l3)
              (if (> (length new-l2) 1)
                  (setf (content l2) (list new-l2)
                        (content l3) (list new-l3))
                  (setf (content l2) new-l2
                        (content l3) new-l3)))))))
    t)

  (defmethod tests-command-2 ((data (eql layout-data)))
    (with-slots (l2 t2
                 l3 bt3)
        data
      (let ((child (first (content l2))))
        ;; Child has options?
        (when (atom child)
          ;; No, so move to :min-width
          (setf (content l2) (list (list t2 :top))
                (content l3) (list (list bt3 :top)))
          (return-from tests-command-2 t))
        ;; Yes
        (let ((options (rest child))
              other-opts new-opt)
          (let ((our-opts (intersection '(:top :middle :bottom) options)))
            ;; Which of our options?
            (cond
              ((member :top our-opts)
               (setq other-opts (delete :top options)
                     new-opt :middle))
              ((member :middle our-opts)
               (setq other-opts (delete :middle options)
                     new-opt :bottom))
              ((member :bottom our-opts)
               (setq other-opts (delete :bottom options)))
              (t
               (setq other-opts options
                     new-opt :top)))
            ;; Build new contents
            (let ((new-l2 ())
                  (new-l3 ()))
              (unless (null new-opt)
                (push new-opt new-l2)
                (push new-opt new-l3))
              (when (> (length other-opts) 0)
                (dolist (o other-opts)
                  (push o new-l2)
                  (push o new-l3)))
              (push t2 new-l2)
              (push bt3 new-l3)
              (if (> (length new-l2) 1)
                  (setf (content l2) (list new-l2)
                        (content l3) (list new-l3))
                  (setf (content l2) new-l2
                        (content l3) new-l3)))))))
    t)
  
  (defmethod tests-command-3 ((data (eql layout-data)))
    (with-slots (l2 t2
                 l3 bt3)
        data
      (let ((child (first (content l2))))
        ;; Child has options?
        (when (atom child)
          ;; No, so move to :min-width
          (setf (content l2) (list (list t2 :min-width))
                (content l3) (list (list bt3 :min-width)))
          (return-from tests-command-3 t))
        ;; Yes
        (let ((options (rest child))
              other-opts new-opt)
          (let ((our-opts (intersection '(:min-width :max-width) options)))
            ;; Which of our options?
            (cond
              ((member :min-width our-opts)
               (setq other-opts (delete :min-width options)
                     new-opt :max-width))
              ((member :max-width our-opts)
               (setq other-opts (delete :max-width options)))
              (t
               (setq other-opts options
                     new-opt :min-width)))
            ;; Build new contents
            (let ((new-l2 ())
                  (new-l3 ()))
              (unless (null new-opt)
                (push new-opt new-l2)
                (push new-opt new-l3))
              (when (> (length other-opts) 0)
                (dolist (o other-opts)
                  (push o new-l2)
                  (push o new-l3)))
              (push t2 new-l2)
              (push bt3 new-l3)
              (if (> (length new-l2) 1)
                  (setf (content l2) (list new-l2)
                        (content l3) (list new-l3))
                  (setf (content l2) new-l2
                        (content l3) new-l3)))))))
    t)
  
  (defmethod tests-command-4 ((data (eql layout-data)))
    (with-slots (l2 t2
                 l3 bt3)
        data
      (let ((child (first (content l2))))
        ;; Child has options?
        (when (atom child)
          ;; No, so move to :min-height
          (setf (content l2) (list (list t2 :min-height))
                (content l3) (list (list bt3 :min-height)))
          (return-from tests-command-4 t))
        ;; Yes
        (let ((options (rest child))
              other-opts new-opt)
          (let ((our-opts (intersection '(:min-height :max-height) options)))
            ;; Which of our options?
            (cond
              ((member :min-height our-opts)
               (setq other-opts (delete :min-height options)
                     new-opt :max-height))
              ((member :max-height our-opts)
               (setq other-opts (delete :max-height options)))
              (t
               (setq other-opts options
                     new-opt :min-height)))
            ;; Build new contents
            (let ((new-l2 ())
                  (new-l3 ()))
              (unless (null new-opt)
                (push new-opt new-l2)
                (push new-opt new-l3))
              (when (> (length other-opts) 0)
                (dolist (o other-opts)
                  (push o new-l2)
                  (push o new-l3)))
              (push t2 new-l2)
              (push bt3 new-l3)
              (if (> (length new-l2) 1)
                  (setf (content l2) (list new-l2)
                        (content l3) (list new-l3))
                  (setf (content l2) new-l2
                        (content l3) new-l3)))))))
    t)
  
  (defmethod tests-command-5 ((data (eql layout-data)))
    (with-slots (t2 bt3) data
      (dolist (w (list t2 bt3))
        (unless (eql w nil)
          (with-slots (cl-yag::width-calc) w
            (case cl-yag::width-calc
              (:auto
               (setf cl-yag::width-calc :auto-min
                     (width w) :auto-min)
               (layout-change w))
              (:auto-min
               (setf cl-yag::width-calc :auto-max
                     (width w) :auto-max)
               (layout-change w))
              (:auto-max
               (setf cl-yag::width-calc :auto
                     (width w) :auto)
               (layout-change w))
              (otherwise
               ;; Not sure how this happened ...
               (error "can't get here ... dunno what to do ...")))))))
    t)
  
  (defmethod tests-command-6 ((data (eql layout-data)))
    (with-slots (t2 bt3) data
      (dolist (w (list t2 bt3))
        (unless (eql w nil)
          (with-slots (cl-yag::height-calc) w
            (case cl-yag::height-calc
              (:auto
               (setf cl-yag::height-calc :auto-min
                     (height w) :auto-min)
               (layout-change w))
              (:auto-min
               (setf cl-yag::height-calc :auto-max
                     (height w) :auto-max)
               (layout-change w))
              (:auto-max
               (setf cl-yag::height-calc :auto
                     (height w) :auto)
               (layout-change w))
              (otherwise
               ;; Not sure how this happened ...
               (error "can't get here ... dunno what to do ...")))))))    
    t)
  
  (defmethod tests-command-update ((data (eql layout-data)))
    (with-slots (b11 b12 t1 st11 st12 st13 st14 st15
                 l2 t2 st21 st22 st23 st24 st25 st26 st27 st28 st29 st210
                 bt3 st31 st32 st33 st34)
        data

      (tests-status-update "P: ~4d" st11 (padding-left t1))
      (tests-status-update "S: ~4d" st12 (spacing-left t1))
      (tests-status-update "B: ~4d" st13 (thickness b11))
      (tests-status-update "H: ~a" st14 (h-align t1))
      (tests-status-update "V: ~a" st15 (v-align t1))
      (tests-status-update-keywords "LH: ~a" st21 '(:left :center :right) (first (content l2)))
      (tests-status-update-keywords "LV: ~a" st22 '(:top :middle :bottom) (first (content l2)))
      (tests-status-update-keywords "MH: ~a" st23 '(:min-width :max-width) (first (content l2)))
      (tests-status-update-keywords "MV: ~a" st24 '(:min-height :max-height) (first (content l2)))
      (tests-status-update "WC: ~a" st25 (slot-value t2 'cl-yag::width-calc))
      (tests-status-update "HC: ~a" st26 (slot-value t2 'cl-yag::height-calc))
      (tests-status-update "T2L: ~f" st27 (left t2))
      (tests-status-update "T2T: ~f" st28 (top t2))
      (tests-status-update "T2W: ~f" st29 (width t2))
      (tests-status-update "T2H: ~f" st210 (height t2))
      (tests-status-update "BT3L: ~f" st31 (left bt3))
      (tests-status-update "BT3T: ~f" st32 (top bt3))
      (tests-status-update "BT3W: ~f" st33 (width bt3))
      (tests-status-update "BT3H: ~f" st34 (height bt3))))

  (defmethod tests-get-h-align ((data (eql layout-data)))
    (with-slots (t1 t2 bt3) data
      (values (list t1 t2 bt3) t)))
  
  (defmethod tests-get-interior-color ((data (eql layout-data)))
    (with-slots (w1 w2 w3) data
      (values (list w1 w2 w3) nil)))

  (defmethod tests-get-padding ((data (eql layout-data)))
    (with-slots (l1 t1 t2 bt3) data
      (values (list (layout-cell l1) t1 t2 bt3) t)))

  (defmethod tests-get-rulers ((data (eql layout-data)))
    (tests-list-rulers data))

  (defmethod tests-get-spacing ((data (eql layout-data)))
    (with-slots (l1 t1 t2 bt3) data
      (values (list (layout-cell l1) t1 t2 bt3) t)))

  (defmethod tests-get-thickness ((data (eql layout-data)))
    (with-slots (b11 b12 b2 b3) data
      (values (list b11 b12 b2 b3) t)))

  (defmethod tests-get-v-align ((data (eql layout-data)))
    (with-slots (t1 t2 bt3) data
      (values (list t1 t2 bt3) t)))
  
  (tests-command-update layout-data))

(defun layout-tests-main ()
  (tests-main *layout-data*)
  nil)

