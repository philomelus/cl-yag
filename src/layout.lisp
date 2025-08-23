(in-package #:cl-yag)

(declaim (optimize (safety 3)))

;;;; functions ================================================================

(defun calc-layout-area (object)
  "Calculate area of a layout itself."
  ;; Start with existing area
  (with-area (ll lt lw lh) object

    ;; Calculate our area if needed
    (when (or (typep ll 'keyword)
              (typep lt 'keyword)
              (typep lw 'keyword)
              (typep lh 'keyword))
      (v:debug :layout "[calc-layout-area] {~a} calculating layout area" (print-raw-object object))
      
      ;; Locate first parent that's layout or has area
      (let ((pam (find-parent-area-mixin-or-layout object)))
        (let (pal pat paw pah)
          ;; Get area of parent.  Is it another layout?
          (if (typep pam 'layout-base)
              ;; Yeah, so get our area from its child-area
              (let ((pamo (position object (content pam))))
                (assert (>= pamo 0))
                (with-area (pl pt pw ph)
                           (aref (slot-value pam 'child-area) pamo)
                  (setf pal pl pat pt paw pw pah ph)
                  (assert (>= pal 0))
                  (assert (>= pat 0))
                  (assert (>= paw 0))
                  (assert (>= pah 0))))
              ;; Nope, so get the actual area
              (setf pal (slot-value pam 'left)
                    pat (slot-value pam 'top)
                    paw (slot-value pam 'width)
                    pah (slot-value pam 'height)))

          (v:debug :layout "[calc-layout-area] {~a} parent area: (~d ~d) @ (~d ~d)"
                  (print-raw-object object) paw pah pal pat)
          
          ;; If parent has borders
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

          ;; If parent has padding
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
            (setf (slot-value object 'top) pat))

          (v:debug :layout "[calc-layout-area] {~a} new area: (~d ~d) @ (~d ~d)"
                  (print-raw-object object) paw pah pal pat)
          (v:debug :layout "[calc-layout-area] {~a} actual area: (~d ~d) @ (~d ~d)"
                  (print-raw-object object) (slot-value object 'width)
                  (slot-value object 'height) (slot-value object 'left)
                  (slot-value object 'top))
          )))))

(defun calc-column-layout-child-areas (object)
  "For a column-layout, calculate the over-all area of each child.
Note that children may use different sizes themselves, this is just the
area allocated to them, whether they choose to use it or not."

  (with-slots (child-area) object
    (setf child-area (make-array (length (content object)) :adjustable nil))
    
    (let* ((num-children (length (content object)))
           (num-to-adjust (mod (height object) num-children))
           (base-height (truncate (/ (height object) num-children))))
      (assert (< num-to-adjust num-children))
      
      ;; Set calculated initial area for children
      (loop :for i :from 0 :to (1- num-children) :do
        ;; We only do vertical space, so left and width are from object
        ;; but top and height are calculated per child offset
        (setf (aref child-area i) (make-instance '%rect :left (slot-value object 'left)
                                                        :top (+ (slot-value object 'top) (* i base-height))
                                                        :width (slot-value object 'width)
                                                        :height base-height)))

      ;; If there are leftover area, and its an odd count, allocate some for middle item
      (when (oddp num-to-adjust)
        (incf (height (aref child-area (truncate (/ num-children 2)))))
        (decf num-to-adjust))

      ;; If there is still leftover area, allocate to ends until used up
      (assert (evenp num-to-adjust))
      (when (> num-to-adjust 0)
        (do ((front 0 (incf front))
             (back (1- num-children) (decf back)))
            ((= num-to-adjust 0))
          (incf (height (aref child-area front)))
          (incf (height (aref child-area back)))
          (decf num-to-adjust 2)))

      ;; Update internal areas for children with actual area values
      (with-slots (content) object
        (loop :for i :from 0 :to (1- num-children) :do
          (with-slots ((rl left) (rt top) (rw width) (rh height))
              (aref child-area i)
            (with-slots ((cl left) (ct top) (cw width) (ch height))
                (nth i content)
              (unless (typep cl 'keyword)
                (setf rl cl))
              (unless (typep ct 'keyword)
                (setf rh ct))
              (unless (typep cw 'keyword)
                (setf rw cw))
              (unless (typep ch 'keyword)
                (setf rh ch))))))
      
      (dotimes (n num-children)
        (v:debug :layout "[calc-column-layout-child-areas] child ~d internal area (~d ~d) @ (~d ~d)" n
                (width (elt child-area n))
                (height (elt child-area n))
                (left (elt child-area n))
                (top (elt child-area n)))))))

(defun dump-layout (object &optional (indent ""))
  (when (not (eql object nil))
    (v:info :layout "~a~a:~%" indent (print-raw-object object))
    (with-slots (content) object
      (dotimes (n (length (content object)))
        (let ((child (nth n content)))
          (v:info :layout "~achild ~d area (~d ~d) @ (~d ~d)~%" (concatenate 'string indent "  ") n
                  (slot-value child 'child) (slot-value child 'height) (slot-value child 'left) (slot-value child 'top))
          (when (typep child 'layout-base)
            (dump-layout child (concatenate 'string indent "  "))))))))

(defun find-parent-area-mixin-or-layout (object)
  (assert (typep object 'parent-mixin))
  (let ((p (parent object)))
    (loop
      (when (typep p 'manager)
        (error "found manager when looking for area-mixin"))
      
      (when (eql p nil)
        (error "no parent when looking for area-mixin"))

      ;; If its a layout
      (if (typep p 'layout-base)
          (return p))
      
      ;; If it has area
      (if (typep p 'area-mixin)
          (return p))

      ;; Get next parent
      (assert (typep p 'area-mixin))
      (setf p (parent p)))))

(defun update-column-layout-child-areas (index object)
  "When the size of a child object has been changed, this recalculates the sizes
of the children that are affected by it.  For column-layout, thats all children
with indexes greater than the changed index."

  ;; TODO: Only when safety 3 #+safety
  (progn
    (when (> index 0)
      (loop :for i :from 0 :to (1- index) :do
        (with-area (rl rt rw rh) (aref (slot-value object 'child-area) i)
          (with-area (ol ot ow oh) (nth i (content object))
            (if (not (typep ol 'keyword))
                (assert (= rl ol)))
            (if (not (typep ot 'keyword))
                (assert (= rt ot)))
            (if (not (typep ow 'keyword))
                (assert (= rw ow)))
            (if (not (typep oh 'keyword))
                (assert (= rh oh)))))))
    
    (when (< index (length (content object)))
      (loop :for i :from (1+ index) :to (1- (length (content object))) :do
        (with-area (ol ot ow oh) (nth i (content object))
          (with-area (rl rt rw rh) (aref (slot-value object 'child-area) i)
            (if (not (typep ol 'keyword))
                (assert (= rl ol)))
            (if (not (typep ot 'keyword))
                (assert (= rt ot)))
            (if (not (typep ow 'keyword))
                (assert (= rw ow)))
            (if (not (typep oh 'keyword))
                (assert (= rh oh))))))))
  
  (let* ((num-children (length (content object)))
         (affected (- num-children index 1)))

    ;; Nothing to do if it was the only or last child
    (unless (> affected 0)
      (return-from update-column-layout-child-areas))

    ;; Update affected children
    (with-slots (child-area) object

      (v:debug :layout "[update-column-layout-child-areas] updating ~d internal from (~d ~d) @ (~d ~d)" index
              (width (aref child-area index))
              (height (aref child-area index))
              (left (aref child-area index))
              (top (aref child-area index)))
      (v:debug :layout "[update-column-layout-child-areas]                       to (~d ~d) @ (~d ~d) {~a}"
              (slot-value (nth index (content object)) 'width)
              (slot-value (nth index (content object)) 'height)
              (slot-value (nth index (content object)) 'left)
              (slot-value (nth index (content object)) 'top)
              (print-raw-object (nth index (content object))))

      ;; Calculate newly available height
      (let ((new-height (- (height (aref child-area index))
                           (height (nth index (content object))))))
        ;; Add height of all prior siblings
        (loop :for i :from (1+ index) :to (1- num-children) :do
          (incf new-height (height (aref child-area i))))

        (v:debug :layout "[update-column-layout-child-areas] siblings available new height ~d" new-height)
        
        ;; Recalculate rest of the children
        (let ((new-child-height (truncate (/ new-height affected)))
              (height-to-spread (mod new-height affected)))

          ;; Update child area to objects actual area
          (with-slots (content) object
            (setf (height (aref child-area index)) (slot-value (nth index content) 'height))
            (setf (width (aref child-area index)) (slot-value (nth index content) 'width))
            (setf (left (aref child-area index)) (slot-value (nth index content) 'left))
            (setf (top (aref child-area index)) (slot-value (nth index content) 'top)))
          
          ;; Update child areas
          (loop :for i :from (1+ index) :to (1- num-children) :do
            (let ((new-top (+ (bottom (aref child-area (1- i))) 1))
                  (new-height new-child-height))

              (let ((child (nth i (content object)))
                    (child-a (aref child-area i)))

                ;; Save new internal height
                (setf (height child-a) new-height)
                
                ;; Force recalc of child height if originally calculated
                (when (slot-value child 'height-calc)
                  (setf (slot-value child 'height) (slot-value child 'height-calc)))

                ;; Save new internal top
                (setf (top child-a) new-top)

                ;; Force recalc of child top if originally calculated
                (when (slot-value child 'top-calc)
                  (setf (slot-value child 'top) (slot-value child 'top-calc)))

                ;; For layouts that changed, force full recalculation of area
                (when (typep child 'layout-base)
                  (v:debug :layout "[update-column-layout-child-areas] {~a} child ~d layout forced recalc"
                          (print-raw-object child) i)
                  (setf (slot-value (nth i (content object)) 'child-area) nil)
                  (dolist (gc (content child))
                    (when (typep gc 'area-mixin)
                      (when (slot-value gc 'height-calc)
                        (setf (slot-value gc 'height) (slot-value gc 'height-calc)))
                      (when (slot-value gc 'top-calc)
                        (setf (slot-value gc 'top) (slot-value gc 'top-calc))))))
                )))

          
          ;; Allocate left over space
          (when (> height-to-spread 0)
            (do ((next (1+ index) (+ next 1)))
                ((= height-to-spread 0))
              (incf (height (aref child-area next)))
              (decf height-to-spread)))

          ;; Dump updates
          (dotimes (n affected)
            (let* ((i (+ index n 1))
                   (r (aref child-area i))
                   (cna (nth i (content object))))
              (v:debug :layout "[update-column-layout-child-areas] child ~d new internal area (~d ~d) @ (~d ~d)" i (width r) (height r) (left r) (top r))
              (v:debug :layout "[update-column-layout-child-areas] child ~d       actual area (~d ~d) @ (~d ~d) {~a}" i
                      (slot-value cna 'width) (slot-value cna 'height) (slot-value cna 'left) (slot-value cna 'top)
                      (print-raw-object cna))
              )))))
    ))

;;;; layout-base ==============================================================

(defclass layout-base (container-mixin
                       parent-mixin
                       ready-mixin)
  ((left :initform :auto :initarg nil)
   (top :initform :auto :initarg nil)
   (width :initform :auto :initarg nil)
   (height :initform :auto :initarg nil)

   ;; Internal
   (child-area :initform nil)))

;; (defmethod print-object ((o layout-base) s)
;;   (pprint-indent :current 0 s)
;;   (pprint-logical-block (s nil)
;;     (format s "deflayout-base ")
;;     (print-mixin o s)))

;;; methods ---------------------------------------------------------

(defmethod on-char (key mods (obj layout-base) &key)
  (dolist (child (content obj))
    (on-char key mods child))
  (my-next-method))

(defmethod on-mouse-down (x y b (obj layout-base) &key)
  (dolist (child (content obj))
    (if (on-mouse-down x y b child)
        (return-from on-mouse-down t)))
  nil)

(defmethod on-mouse-move (x y dx dy (obj layout-base) &key)
  (dolist (child (content obj))
    (on-mouse-move x y dx dy child)))

(defmethod on-mouse-up (x y b (obj layout-base) &key)
  (dolist (child (content obj))
    (on-mouse-up x y b child)))

;;;; column-layout ============================================================

(defclass column-layout (layout-base
                         padding-mixin)
  ())

(defmacro defcolumn-layout (&rest rest &key &allow-other-keys)
  `(make-instance 'column-layout ,@rest))

;; (defmethod print-object ((o column-layout) s)
;;   (pprint-indent :current 0 s)
;;   (pprint-logical-block (s nil)
;;     (format s "defcolumn-layout ")
;;     (print-mixin o s)))

;;; methods ---------------------------------------------------------

(defmethod calc-area (child (parent column-layout) &key)

  ;; Calculate parent area if needed
  (calc-layout-area parent)

  ;; Calculate our children areas if needed
  (when (eql (slot-value parent 'child-area) nil)
    (calc-column-layout-child-areas parent))

  (with-slots (child-area content) parent
    (let* ((cp (position child content))
           (ca (aref child-area cp))
           (oa (make-instance '%rect :left (left ca) :top (top ca) :width (width ca) :height (height ca))))

      (with-slots ((cl left) (ct top) (cw width) (ch height)) child

        (v:debug :layout "[calc-area] {~a} child ~d calculating (~a ~a ~a ~a)"
                (print-raw-object child) cp cl ct cw ch)
        
        (let ((ocl cl) (oct ct) (ocw cw) (och ch))

          (when (member ch *AREA-HEIGHT-OPTS*)
            (setf ch (calc-height ch oa child)))

          ;; Update width if desired
          (when (member cw *AREA-WIDTH-OPTS*)
            (setf cw (calc-width cw oa child)))

          ;; Update left if desired
          (when (member cl *AREA-LEFT-OPTS*)
            (setf cl (calc-left cl oa child)))

          ;; Update top if desired
          (when (member ct *AREA-TOP-OPTS*)
            (let ((ctt ct)
                  (ctn))
              (setf ctn (calc-top ctt oa child))
              (v:debug :layout "[calc-area] {~a} calc-top returned ~d" (print-raw-object child) ctn)
              (setf ct ctn)))

          (when (or (not (eql och ch))
                    (not (eql ocw cw))
                    (not (eql oct ct))
                    (not (eql ocl cl)))
            (v:debug :layout "[calc-area] {~a} child ~d changed to (~d ~d) @ (~d ~d)"
                    (print-raw-object child) cp cw ch cl ct)
            (update-column-layout-child-areas cp parent)
            )
          ))))
  (my-next-method))

(defmethod on-paint ((obj column-layout) &key)
  (dolist (c (content obj))
    (on-paint c))
  ;; (on-paint (first (content obj)))
  (my-next-method))

;;;; grid-layout ==============================================================

;; (defclass grid-layout (layout-base
;;                        padding-mixin    ; distances from parent
;;                        spacing-mixin)   ; distances between children
;;   ((rows :initarg :rows :initform 1 :accessor rows)
;;    (columns :initarg :columns :initform 1 :accessor columns)))

;; (defmacro defgrid-layout (&rest rest &key &allow-other-keys)
;;   `(make-instance 'grid-layout ,@rest))

;; (defmethod print-object ((o grid-layout) s)
;;   (pprint-indent :current 0 s)
;;   (pprint-logical-block (s nil)
;;     (format s "defgrid-layout ")
;;     (pprint-field columns o s :fmt "~d")
;;     (pprint-field rows o s :fmt "~a")
;;     (print-mixin o s)))

;;; methods ---------------------------------------------------------

;; (defmethod calc-area (child (parent grid-layout) &key)
;;   ;; Calculate our area if needed
;;   (calc-layout-area parent)

;;   ;; Start with existing child area
;;   (let ((cl (slot-value child 'left))
;;         (ct (slot-value child 'top))
;;         (cw (slot-value child 'width))
;;         (ch (slot-value child 'height))
;;         (pl (slot-value parent 'left))
;;         (pt (slot-value parent 'top))
;;         (pw (slot-value parent 'width))
;;         (ph (slot-value parent 'height))
;;         (cp (position child (content parent)))
;;         (child-num-h (rows parent))
;;         (child-num-v (columns parent))          
;;         child-hs child-ws)

;;     ;; TODO: Should this be real?
;;     (assert (not (eql cp nil)))
    
;;     ;; Calculate our childrens heights
;;     (loop :for i :from 0 :to (rows parent) do
;;       (push (+ (truncate (/ ph child-num-v)) (if (> (mod ph child-num-v) 0) 1 0)) child-hs))

;;     ;; Calculate our childrens widths
;;     (loop :for i :from 0 :to (columns parent) do
;;       (push (+ (truncate (/ pw child-num-h) (if (> (mod pw child-num-h) 0) 1 0))) child-ws))

;;     ;; TODO: Spacing not taken into consideration yet
    
;;     ;; Determine row and column of child
;;     (let* ((row (truncate (/ cp (rows parent))))
;;            (col (+ (- cp row) (mod cp (rows parent)))))
      
;;       ;; Update left if desired
;;       (when (member cl *AREA-OPTS*)
;;         (when (> col 0)
;;           (loop :for i :from 0 :to (1- col) do
;;             (incf pl (nth i child-ws))))
;;         (setf (slot-value child 'left) pl))

;;       ;; Update top if desired
;;       (when (member ct *AREA-OPTS*)
;;         (when (> row 0)
;;           (loop :for i :from 0 :to (1- row) do
;;             (incf pt (nth i child-hs))))
;;         (setf (slot-value child 'top) pt))
      
;;       ;; Update width if desired
;;       (when (member cw *AREA-OPTS*)
;;         (setf (slot-value child 'width) (nth col child-ws)))

;;       ;; Update height if desired
;;       (when (member ch *AREA-OPTS*)
;;         (setf (slot-value child 'height) (nth row child-hs)))))
  
;;   (my-next-method))
