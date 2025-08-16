(in-package #:cl-yag)

;;;; layout ===================================================================

(defclass layout (container-mixin
                  parent-mixin
                  ready-mixin)
  ((left :initform +LAYOUT-LEFT-CALC+ :initarg nil)
   (top :initform +LAYOUT-TOP-CALC+ :initarg nil)
   (width :initform +LAYOUT-WIDTH-CALC+ :initarg nil)
   (height :initform +LAYOUT-HEIGHT-CALC+ :initarg nil)))

(defmacro defcolumn-layout (&rest rest &key &allow-other-keys)
  `(make-instance 'column-layout ,@rest))

(defmethod print-object ((o layout) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "deflayout ")
    (print-mixin o s)))

(defmethod on-char (key mods (obj layout) &key)
  (dolist (child (content obj))
    (on-char key mods child))
  (my-next-method))

(defmethod on-mouse-down (x y b (obj layout) &key)
  (dolist (child (content obj))
    (if (on-mouse-down x y b child)
        (return-from on-mouse-down t)))
  nil)

(defmethod on-mouse-move (x y dx dy (obj layout) &key)
  (dolist (child (content obj))
    (on-mouse-move x y dx dy child)))

(defmethod on-mouse-up (x y b (obj layout) &key)
  (dolist (child (content obj))
    (on-mouse-up x y b child)))

;;;; column-layout ============================================================

(defclass column-layout (layout
                         padding-mixin)
  ())

(defmethod print-object ((o column-layout) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "defcolumn-layout ")
    (print-mixin o s)))

;;; methods ---------------------------------------------------------

(defmethod calc-area (child (parent column-layout) &key)
  ;; Start with existing child area
  (let ((cl (slot-value child 'left))
        (ct (slot-value child 'top))
        (cw (slot-value child 'width))
        (ch (slot-value child 'height))
        (pl (slot-value parent 'left))
        (pt (slot-value parent 'top))
        (pw (slot-value parent 'width))
        (ph (slot-value parent 'height)))

    ;; Calculate our area if needed
    (if (or (= pl +LAYOUT-LEFT-CALC+)
            (= pt +LAYOUT-TOP-CALC+)
            (= pw +LAYOUT-WIDTH-CALC+)
            (= ph +LAYOUT-HEIGHT-CALC+))
        ;; Locate first parent with area
        (let ((pam (find-parent-area-mixin parent)))
          ;; Start with its area
          (let ((pal (slot-value pam 'left))
                (pat (slot-value pam 'top))
                (paw (slot-value pam 'width))
                (pah (slot-value pam 'height)))
            ;; If it also has borders
            (when (typep pam 'border-mixin)
              ;; Adjust for border sizes
              (let ((pam-bl (border-left pam))
                    (pam-bt (border-top pam))
                    (pam-br (border-right pam))
                    (pam-bb (border-bottom pam)))
                (unless (eql pam-bl nil)
                  (incf pal (width pam-bl))
                  (decf paw (width pam-bl)))
                (unless (eql pam-bt nil)
                  (incf pat (width pam-bt))
                  (decf pah (width pam-bt)))
                (unless (eql pam-br nil)
                  (decf paw (width pam-br)))
                (unless (eql pam-bb nil)
                  (decf pah (width pam-bb)))))

            ;; If it also has spacing
            (when (typep pam 'spacing-mixin)
              ;; Adjust for spacing
              (let ((pam-sl (spacing-left pam))
                    (pam-st (spacing-top pam)))
                (incf pal pam-sl)
                (incf pat pam-st)
                (decf paw (+ pam-sl (spacing-right pam)))
                (decf pah (+ pam-st (spacing-bottom pam)))))

            ;; Save out area where needed
            (when (= pl +LAYOUT-LEFT-CALC+)
              (setf (slot-value parent 'left) pal)
              (setf pl pal))
            (when (= pt +LAYOUT-TOP-CALC+)
              (setf (slot-value parent 'top) pat)
              (setf pt pat))
            (when (= pw +LAYOUT-WIDTH-CALC+)
              (setf (slot-value parent 'width) paw)
              (setf pw paw))
            (when (= ph +LAYOUT-HEIGHT-CALC+)
              (setf (slot-value parent 'height) pah)
              (setf ph pah))))

        ;; Calcualte child size
        (let ((cp (position child (content parent)))
              (num-children (length (content parent)))
              child-hs)
          (assert (not (eql cp nil)))
          ;; Calculate our childrens heights
          (loop :for i :from 0 :to (length (content parent)) do
            (push (+ (truncate (/ ph num-children)) (if (> (mod ph num-children) 0) 1 0)) child-hs))

          ;; Update left if desired
          (when (= cl +LAYOUT-LEFT-CALC+)
            (setf (slot-value child 'left) pl))

          ;; Update top if desired
          (when (= ct +LAYOUT-TOP-CALC+)
            (when (> cp 0)
              (loop :for i :from 0 :to (1- cp) do
                (incf pt (nth i child-hs))))
            (setf (slot-value child 'top) pt))
          
          ;; Update width if desired
          (when (= cw +LAYOUT-WIDTH-CALC+)
            (setf (slot-value child 'width) pw))

          ;; Update height if desired
          (when (= ch +LAYOUT-HEIGHT-CALC+)
            (setf (slot-value child 'height) (nth cp child-hs))))))
  
  (my-next-method))

(defmethod on-paint ((obj column-layout) &key)
  (dolist (c (content obj))
    (on-paint c))
  ;; (on-paint (first (content obj)))
  (my-next-method))

