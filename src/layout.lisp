(in-package #:cl-yag)

;;;; functions ================================================================

(defun calc-layout-area (object)
  "Calculate area of a layout itself."
  ;; Start with existing area
  (let ((ll (slot-value object 'left))
        (lt (slot-value object 'top))
        (lw (slot-value object 'width))
        (lh (slot-value object 'height)))

    ;; Calculate our area if needed
    (if (or (typep ll 'keyword)
            (typep lt 'keyword)
            (typep lw 'keyword)
            (typep lh 'keyword))
        
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

            ;; Save our area where needed
            (when (member lh *AREA-HEIGHT-OPTS*)
              (setf (slot-value object 'height) pah))
            (when (member lw *AREA-WIDTH-OPTS*)
              (setf (slot-value object 'width) paw))
            (when (member ll *AREA-LEFT-OPTS*)
              (setf (slot-value object 'left) pal))
            (when (member lt *AREA-TOP-OPTS*)
              (setf (slot-value object 'top) pat)))))))

(defun calc-column-layout-children-areas (object)
  "For a column-layout, calculate the over-all area of each child.
Note that children may use different sizes themselves, this is just the
area allocated to them, whether they choose to use it or not."
  
  (with-slots (child-area) object
    (setf child-area (make-array (length (content object)) :adjustable nil))
    (let* ((num-children (length (content object)))
           (num-to-adjust (mod (height object) num-children))
           (base-height (truncate (/ (height object) num-children))))
      (assert (< num-to-adjust num-children))
      
      ;; Set initial area for children
      (loop :for i :from 0 :to (1- num-children) :do
        (setf (aref child-area i) (list (left object) (+ (top object) (* i base-height))
                                        (width object) base-height)))

      ;; If there are leftover area, and its an odd count, allocate some for middle item
      (when (oddp num-to-adjust)
        (incf (fourth (aref child-area (truncate (/ num-children 2)))))
        (decf num-to-adjust))

      ;; If there is still leftover area, allocate to ends until used up
      (assert (evenp num-to-adjust))
      (when (> num-to-adjust 0)
        (do ((front 0 (incf front))
             (back (1- num-children) (decf back)))
            ((= num-to-adjust 0))
          (incf (fourth (aref child-area front)))
          (incf (fourth (aref child-area back)))
          (decf num-to-adjust 2)))

      ;; TODO: Temporarily log them
      (dotimes (n num-children)
        (v:info :layout "child ~d area (~d ~d) @ (~d ~d)" n
                (third (elt child-area n))
                (fourth (elt child-area n))
                (first (elt child-area n))
                (second (elt child-area n)))))))

;;;; layout ===================================================================

(defclass layout (container-mixin
                  parent-mixin
                  ready-mixin)
  ((left :initform :auto :initarg nil)
   (top :initform :auto :initarg nil)
   (width :initform :auto :initarg nil)
   (height :initform :auto :initarg nil)))

(defmethod print-object ((o layout) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "deflayout ")
    (print-mixin o s)))

;;; methods ---------------------------------------------------------

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
  ((child-area :initform nil)))

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

  ;; Calculate our children areas if needed
  (when (eql (slot-value parent 'child-area) nil)
    (calc-column-layout-children-areas parent))
  
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
      ;;       In this context, that would mean ... distance from t/l/r/b of next
      ;;       child?  So, if the child is of type spacing-mixin, then make sure
      ;;       l/t/r/b are at least spacing-{l/t/r/b} from next child
      (when (typep child 'spacing-mixin)
        (error "not implmented yet"))
      
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

      ;; Update height if desired

      ;; Calculate top of this object
      (when (> cp 0)
        (loop :for i :from 0 :to (1- cp) do
          (incf pt (nth i child-hs))))

      (let ((oa (list pl pt pw (nth cp child-hs))))
        (when (member ch *AREA-HEIGHT-OPTS*)
          (setq ph (calc-height ch oa child))
          (setf (slot-value child 'height) ph))
      
        ;; Update width if desired
        (when (member cw *AREA-WIDTH-OPTS*)
          (setq pw (calc-width cw oa child))
          (setf (slot-value child 'width) pw))

        ;; Update left if desired
        (when (member cl *AREA-LEFT-OPTS*)
          (setf pl (calc-left cl oa child))
          (setf (slot-value child 'left) pl))

        ;; Update top if desired
        (when (member ct *AREA-TOP-OPTS*)
         
          (setf pt (calc-top ct oa child))
          (setf (slot-value child 'top) pt)))))

  (my-next-method))

(defmethod on-paint ((obj column-layout) &key)
  (dolist (c (content obj))
    (on-paint c))
  ;; (on-paint (first (content obj)))
  (my-next-method))

;;;; grid-layout ==============================================================

(defclass grid-layout (layout
                       padding-mixin    ; distances from parent
                       spacing-mixin)   ; distances between children
  ((rows :initarg :rows :initform 1 :accessor rows)
   (columns :initarg :columns :initform 1 :accessor columns)))

(defmacro defgrid-layout (&rest rest &key &allow-other-keys)
  `(make-instance 'grid-layout ,@rest))

(defmethod print-object ((o grid-layout) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "defgrid-layout ")
    (pprint-field columns o s :fmt "~d")
    (pprint-field rows o s :fmt "~a")
    (print-mixin o s)))

;;; methods ---------------------------------------------------------

(defmethod calc-area (child (parent grid-layout) &key)
  ;; ;; Calculate our area if needed
  ;; (calc-layout-area parent)

  ;; ;; Start with existing child area
  ;; (let ((cl (slot-value child 'left))
  ;;       (ct (slot-value child 'top))
  ;;       (cw (slot-value child 'width))
  ;;       (ch (slot-value child 'height))
  ;;       (pl (slot-value parent 'left))
  ;;       (pt (slot-value parent 'top))
  ;;       (pw (slot-value parent 'width))
  ;;       (ph (slot-value parent 'height))
  ;;       (cp (position child (content parent)))
  ;;       (child-num-h (rows parent))
  ;;       (child-num-v (columns parent))          
  ;;       child-hs child-ws)

  ;;   ;; TODO: Should this be real?
  ;;   (assert (not (eql cp nil)))
    
  ;;   ;; Calculate our childrens heights
  ;;   (loop :for i :from 0 :to (rows parent) do
  ;;     (push (+ (truncate (/ ph child-num-v)) (if (> (mod ph child-num-v) 0) 1 0)) child-hs))

  ;;   ;; Calculate our childrens widths
  ;;   (loop :for i :from 0 :to (columns parent) do
  ;;     (push (+ (truncate (/ pw child-num-h) (if (> (mod pw child-num-h) 0) 1 0))) child-ws))

  ;;   ;; TODO: Spacing not taken into consideration yet
    
  ;;   ;; Determine row and column of child
  ;;   (let* ((row (truncate (/ cp (rows parent))))
  ;;          (col (+ (- cp row) (mod cp (rows parent)))))
      
  ;;     ;; Update left if desired
  ;;     (when (member cl *AREA-OPTS*)
  ;;       (when (> col 0)
  ;;         (loop :for i :from 0 :to (1- col) do
  ;;           (incf pl (nth i child-ws))))
  ;;       (setf (slot-value child 'left) pl))

  ;;     ;; Update top if desired
  ;;     (when (member ct *AREA-OPTS*)
  ;;       (when (> row 0)
  ;;         (loop :for i :from 0 :to (1- row) do
  ;;           (incf pt (nth i child-hs))))
  ;;       (setf (slot-value child 'top) pt))
      
  ;;     ;; Update width if desired
  ;;     (when (member cw *AREA-OPTS*)
  ;;       (setf (slot-value child 'width) (nth col child-ws)))

  ;;     ;; Update height if desired
  ;;     (when (member ch *AREA-OPTS*)
  ;;       (setf (slot-value child 'height) (nth row child-hs)))))
  
  (my-next-method))
