(in-package #:cl-yag)

;;;; layout ===================================================================

(defclass layout (container-mixin
                  parent-mixin
                  ready-mixin)
  ((left :initform +LAYOUT-LEFT-CALC+ :initarg nil)
   (top :initform +LAYOUT-TOP-CALC+ :initarg nil)
   (width :initform +LAYOUT-WIDTH-CALC+ :initarg nil)
   (height :initform +LAYOUT-HEIGHT-CALC+ :initarg nil)))

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

(defun calc-layout-area (object)
  ;; Start with existing area
  (let ((ll (slot-value object 'left))
        (lt (slot-value object 'top))
        (lw (slot-value object 'width))
        (lh (slot-value object 'height)))

    ;; Calculate our area if needed
    (if (or (= ll +LAYOUT-LEFT-CALC+)
            (= lt +LAYOUT-TOP-CALC+)
            (= lw +LAYOUT-WIDTH-CALC+)
            (= lh +LAYOUT-HEIGHT-CALC+))
        ;; Locate first parent with area
        (let ((pam (find-parent-area-mixin object)))
          
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

            ;; If it also has padding
            (when (typep pam 'padding-mixin)
              ;; Adjust for padding
              (let ((pam-sl (padding-left pam))
                    (pam-st (padding-top pam)))
                (incf pal pam-sl)
                (incf pat pam-st)
                (decf paw (+ pam-sl (padding-right pam)))
                (decf pah (+ pam-st (padding-bottom pam)))))

            ;; Save out area where needed
            (when (= ll +LAYOUT-LEFT-CALC+)
              (setf (slot-value object 'left) pal))
            (when (= lt +LAYOUT-TOP-CALC+)
              (setf (slot-value object 'top) pat))
            (when (= lw +LAYOUT-WIDTH-CALC+)
              (setf (slot-value object 'width) paw))
            (when (= lh +LAYOUT-HEIGHT-CALC+)
              (setf (slot-value object 'height) pah)))))))

;;;; column-layout ============================================================

(defclass column-layout (layout
                         padding-mixin)
  ())

(defmacro defcolumn-layout (&rest rest &key &allow-other-keys)
  `(make-instance 'column-layout ,@rest))

(defmethod print-object ((o column-layout) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "defcolumn-layout ")
    (print-mixin o s)))

;;; methods ---------------------------------------------------------

(defmethod calc-area (child (parent column-layout) &key)
  ;; Calculate parent area if needed
  (calc-layout-area parent)

  ;; Start with existing child area
  (let ((cl (slot-value child 'left))
        (ct (slot-value child 'top))
        (cw (slot-value child 'width))
        (ch (slot-value child 'height))
        (pl (slot-value parent 'left))
        (pt (slot-value parent 'top))
        (pw (slot-value parent 'width))
        (ph (slot-value parent 'height)))
    
    ;; Calcualte child size
    (let ((cp (position child (content parent)))
          (num-children (length (content parent)))
          child-hs)
      (assert (not (eql cp nil)))
      
      (let ((num-to-adjust (mod ph num-children))
            (base-child-height (truncate (/ ph num-children))))
        ;; Calculate our childrens heights
        (loop :for i :from 0 :to (length (content parent)) do
          ;; (push (+ (truncate (/ ph num-children)) (if (> (- num-children num-to-adjust) cp) 1 0)) child-hs)
          (push base-child-height child-hs))

        ;; Make adjustments as needed
        ;; Basically, if there are an odd number of remainders, distribute
        ;; equally starting in the middle.  If its an even number of remainders,
        ;; distribute it event from the ends.

        ;; Increment height of middle if there are odd number of left over units
        (when (oddp num-to-adjust)
          (incf (nth (truncate (/ num-children 2)) child-hs))
          (decf num-to-adjust))

        ;; Increment height of items at both ends until all left over units are used
        (assert (evenp num-to-adjust))
        (when (> num-to-adjust 0)
          (do ((front 0 (incf front))
               (back (1- num-children) (decf back)))
              ((= num-to-adjust 0))
            (incf (nth front child-hs))
            (incf (nth back child-hs))
            (decf num-to-adjust 2))))

      ;; TODO: Spacing is ignored
      
      ;; TODO: A decision needs to be made here.  We are calculating all the child
      ;;       sizes.  Why do it multiple times?  Currently, as each the first
      ;;       child coordinate or size is requested, it comes here and calculates it.
      ;;       But since we have to calculate all of the children to determine any
      ;;       specific child height, shouldn't we just go ahead and update all
      ;;       children that area still needing calculations?  It can be determined
      ;;       simply by checking for the coordinates/sizes as the calc constants.
      ;;       Seems like a good idea to me, but I'm not commited.  Yet.
      
      ;; NOTE: The above will apply to all layouts that do this type of calculation.
      ;;       For example, the grid layout will need to do it for both directions.
      
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
        (setf (slot-value child 'height) (nth cp child-hs))))
    )
  
  (my-next-method))

(defmethod on-paint ((obj column-layout) &key)
  (dolist (c (content obj))
    (on-paint c))
  ;; (on-paint (first (content obj)))
  (my-next-method))

;;;; grid-layout ==============================================================

(defclass grid-layout (layout
                       padding-mixin
                       spacing-mixin)
  ((rows :initarg :rows :initform 1 :accessor rows)
   (columns :initarg :columns :initform 1 :accessor columns)))

(defmacro defgrid-layout (&rest rest &key &allow-other-keys)
  `(make-instance 'grid-layout ,@rest))

(defmethod print-object ((o grid-layout) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)

    (format s "defgrid-layout ")
    
    (pprint-indent :current 0 s)
    (format s ":columns ~d " (columns o))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":rows ~d " (rows o))
    (pprint-newline :linear s)
    
    (print-mixin o s)))

(defmethod calc-area (child (parent grid-layout) &key)
  ;; Calculate our area if needed
  (calc-layout-area parent)

  ;; Start with existing child area
  (let ((cl (slot-value child 'left))
        (ct (slot-value child 'top))
        (cw (slot-value child 'width))
        (ch (slot-value child 'height))
        (pl (slot-value parent 'left))
        (pt (slot-value parent 'top))
        (pw (slot-value parent 'width))
        (ph (slot-value parent 'height))
        (cp (position child (content parent)))
        (child-num-h (rows parent))
        (child-num-v (columns parent))          
        child-hs child-ws)

    (assert (not (eql cp nil)))
    
    ;; Calculate our childrens heights
    (loop :for i :from 0 :to (rows parent) do
      (push (+ (truncate (/ ph child-num-v)) (if (> (mod ph child-num-v) 0) 1 0)) child-hs))

    ;; Calculate our childrens widths
    (loop :for i :from 0 :to (columns parent) do
      (push (+ (truncate (/ pw child-num-h) (if (> (mod pw child-num-h) 0) 1 0))) child-ws))

    ;; TODO: Spacing not taken into consideration yet
    
    ;; Determine row and column of child
    (let* ((row (truncate (/ cp (rows parent))))
           (col (+ (- cp row) (mod cp (rows parent)))))
      
      ;; Update left if desired
      (when (= cl +LAYOUT-LEFT-CALC+)
        (when (> col 0)
          (loop :for i :from 0 :to (1- col) do
            (incf pl (nth i child-ws))))
        (setf (slot-value child 'left) pl))

      ;; Update top if desired
      (when (= ct +LAYOUT-TOP-CALC+)
        (when (> row 0)
          (loop :for i :from 0 :to (1- row) do
            (incf pt (nth i child-hs))))
        (setf (slot-value child 'top) pt))
      
      ;; Update width if desired
      (when (= cw +LAYOUT-WIDTH-CALC+)
        (setf (slot-value child 'width) (nth col child-ws)))

      ;; Update height if desired
      (when (= ch +LAYOUT-HEIGHT-CALC+)
        (setf (slot-value child 'height) (nth row child-hs)))))
  
  (my-next-method))
