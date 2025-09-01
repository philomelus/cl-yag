(in-package #:cl-yag)

(declaim (optimize (safety 3)))

;;;; layout-base ==============================================================

(defparameter +LAYOUT-CHILD-OPTIONS+ '(:min-height :min-width :max-height :max-width))

(defclass layout-base (container-mixin
                       parent-mixin
                       ready-mixin)
  ((left :initform :auto :initarg nil)
   (top :initform :auto :initarg nil)
   (width :initform :auto :initarg nil)
   (height :initform :auto :initarg nil)

   ;; Internal
   (child-area :initform nil)
   (child-options :initform nil)))

;;; methods ---------------------------------------------------------

(defmethod on-char (key mods (obj layout-base) &key)
  (dolist (child (content obj))
    (when (on-char key mods child)
      (return-from on-char t)))
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

;; Separate children into objects and options
(defmethod initialize-instance :after ((object column-layout) &key)
  ;; Allocate options storage
  (setf (slot-value object 'child-options) (make-array (length (content object)) :initial-element nil))

  ;; Separate the children and their options
  (dotimes (i (length (content object)))
    (let ((child (nth i (content object))))
      (when (consp child)
        
        (let ((child-object)
              (options (rest child)))
          (typecase (first child)
            (symbol
             (setq child-object (symbol-value (first child))))
            (t
             (setq child-object (first child))))
          ;; Make sure options area valid
          ;; #+safety
          (dolist (option options)
            (unless (member option +LAYOUT-CHILD-OPTIONS+)
              (error "unknown child option: ~a" option)))
          (setf (nth i (content object)) child-object)
          (setf (aref (slot-value object 'child-options) i) options))))))

;;; methods ---------------------------------------------------------

(defmethod calc-area (child (parent column-layout) &key)

  ;; Calculate parent area if needed
  (calc-layout-area parent)

  ;; Calculate our children areas if needed
  (when (eql (slot-value parent 'child-area) nil)
    (calc-layout-child-areas parent))

  (with-slots (child-area content) parent
    (let* ((cp (position child content))
           (ca (aref child-area cp))
           (oa (make-instance '%rect :left (left ca) :top (top ca) :width (width ca) :height (height ca))))

      (with-slots ((cl left) (ct top) (cw width) (ch height)) child

        (v:debug :layout "[calc-area] {~a} child ~d calculating (~a ~a) @ (~a ~a)"
                 (print-raw-object child) cp cw ch cl ct)
        
        (macrolet ((do-calc (var func)
                     `(if (typep ,var 'keyword)
                          (progn
                            (setf ,var (,func ,var oa child))
                            t)
                          nil)))
          (let ((clp (do-calc ch calc-height))
                (ctp (do-calc cw calc-width))
                (cwp (do-calc cl calc-left))
                (chp (do-calc ct calc-top)))

            ;; Call update when there are child options or any area field wasn't
            ;; calculated
            (let ((options (aref (slot-value parent 'child-options) cp)))
              (when (or (not (or clp ctp cwp chp))
                        options)
                (with-area (lcl lct lcw lch) (aref child-area cp)
                  (v:debug :layout "[calc-area] {~a} child ~d internal area (~d ~d) @ (~d ~d)"
                           (print-raw-object child) cp lcw lch lcl lct))
                (update-layout-child-areas cp parent)))

            ;; Log updated/changed area
            (v:debug :layout "[calc-area] {~a} child ~d area (~d ~d) @ (~d ~d)"
                     (print-raw-object child) cp cw ch cl ct))))))
  (my-next-method))

(defmethod on-paint ((obj column-layout) &key)
  (dolist (c (content obj))
    (if (atom c)
        (on-paint c)
        (progn
          (if (consp c)
              (on-paint (first c))))))
  (my-next-method))

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
                (assert (not (eql pamo nil)))
                (with-area (pl pt pw ph)
                           (aref (slot-value pam 'child-area) pamo)
                  (setf pal pl pat pt paw pw pah ph)
                  (assert (typep pal 'number))
                  (assert (typep pat 'number))
                  (assert (typep paw 'number))
                  (assert (typep pah 'number))))
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

(defmethod calc-layout-child-areas ((object column-layout))
  "Calculate the over-all area of each child.
Note that children may use different sizes themselves, this is just the
area allocated to them, whether they choose to use it or not."

  (with-slots (child-area) object
    (setf child-area (make-array (length (content object)) :adjustable nil))
    
    (let* ((num-children (length (content object)))
           (num-to-adjust (truncate (mod (height object) num-children)))
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

      (dotimes (n num-children)
        (v:debug :layout "[calc-layout-child-areas] child ~d internal area (~d ~d) @ (~d ~d)" n
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

(defun find-parent-area-mixin-or-layout (object &key (exclude nil excludep))
  (assert (typep object 'parent-mixin))
  (flet ((excluded (obj)
           (when excludep
             (dolist (exc exclude)
               (when (typep obj exc)
                 (return-from excluded t))))
           nil))
    (let ((p (parent object)))
      (loop
        (when (typep p 'manager)
          (error "found manager when looking for area-mixin"))
      
        (when (eql p nil)
          (error "no parent when looking for area-mixin"))

        (unless (excluded p)
          ;; If its a layout
          (if (typep p 'layout-base)
              (return p))
      
          ;; If it has area
          (if (typep p 'area-mixin)
              (return p)))

        ;; Get next parent
        (assert (typep p 'area-mixin))
        (setf p (parent p))))))

(defmethod update-layout-child-areas (index (object column-layout))
  "When a child has options changing the area used within the layout, this
recalculates the sizes of the children that are affected by it."

  (v:debug :layout "[update-layout-child-area] {~a} updating child ~d"
           (print-raw-object (nth index (content object))) index)

  ;; Figure out what fields to take from object
  (let ((upd-ch-l-p nil) (upd-ch-t-p nil) (upd-ch-w-p nil) (upd-ch-h-p nil))
    
    ;; Update internal area if field wasn't calculated
    (macrolet ((update-p (field field-calc)
                 `(if (not (slot-value (nth index (content object)) ',field-calc))
                      (progn
                        (v:debug :layout "[update-layout-child-areas] child ~d internal ~a needs update"
                                index (symbol-name ',field))
                        t)
                      nil)))
      (setq upd-ch-l-p (update-p left left-calc))
      (setq upd-ch-t-p (update-p top top-calc))
      (setq upd-ch-w-p (update-p width width-calc))
      (setq upd-ch-h-p (update-p height height-calc)))
    
    ;; Update internal area from control based on options
    (dolist (opt (aref (slot-value object 'child-options) index))
      (case opt
        (:min-height
         (setq upd-ch-h-p t)
         (v:debug :layout "[update-layout-child-areas] child ~d has option :min-height"index))
        (:min-width
         (setq upd-ch-w-p t)
         (v:debug :layout "[update-layout-child-areas] child ~d has option :min-width"index ))
        (:max-height)
        (:max-width)))

    
    (let* ((num-children (length (content object)))
           (affected (- num-children index 1)))

      ;; Nothing to do if it was the only or last child
      (unless (> affected 0)
        (return-from update-layout-child-areas))

      ;; Update affected children
      (with-slots (child-area) object

        (v:debug :layout "[update-layout-child-areas] child ~d updating internal:" index)
        (v:debug :layout "[update-layout-child-areas]         {~a}" (print-raw-object (nth index (content object))))
        (v:debug :layout "[update-layout-child-areas]         from (~d ~d) @ (~d ~d)"
                 (width (aref child-area index))
                 (height (aref child-area index))
                 (left (aref child-area index))
                 (top (aref child-area index)))
        (v:debug :layout "[update-layout-child-areas]         to (~d ~d) @ (~d ~d)"
                 (slot-value (nth index (content object)) 'width)
                 (slot-value (nth index (content object)) 'height)
                 (slot-value (nth index (content object)) 'left)
                 (slot-value (nth index (content object)) 'top))

        ;; Calculate newly available height
        (let ((new-height (- (height (aref child-area index))
                             (height (nth index (content object))))))
          ;; Add height of all prior siblings
          (loop :for i :from (1+ index) :to (1- num-children) :do
            (incf new-height (height (aref child-area i))))

          (v:debug :layout "[update-layout-child-areas] siblings available new height ~d" new-height)
          
          ;; Recalculate rest of the children
          (let ((new-child-height (truncate (/ new-height affected)))
                (height-to-spread (mod new-height affected)))

            ;; Update child area to objects actual area
            (with-slots (content) object
              (when upd-ch-h-p
                (setf (height (aref child-area index)) (slot-value (nth index content) 'height))
                (v:debug :layout "[update-layout-child-areas] child ~d internal height updated (~d)"
                         index (height (aref child-area index))))
              (when upd-ch-w-p
                (setf (width (aref child-area index)) (slot-value (nth index content) 'width))
                (v:debug :layout "[update-layout-child-areas] child ~d internal width updated (~d)"
                         index (height (aref child-area index))))
              (when upd-ch-l-p
                (setf (left (aref child-area index)) (slot-value (nth index content) 'left))
                (v:debug :layout "[update-layout-child-areas] child ~d internal left updated (~d)"
                         index (height (aref child-area index))))
              (when upd-ch-t-p
                (setf (top (aref child-area index)) (slot-value (nth index content) 'top))
                (v:debug :layout "[update-layout-child-areas] child ~d internal top updated (~d)"
                         index (height (aref child-area index)))))
            
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
                    (v:debug :layout "[update-layout-child-areas] {~a} child ~d layout forced recalc"
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
                (v:debug :layout "[update-layout-child-areas] child ~d new internal area (~d ~d) @ (~d ~d)" i (width r) (height r) (left r) (top r))
                (v:debug :layout "[update-layout-child-areas] child ~d       actual area (~d ~d) @ (~d ~d) {~a}" i
                         (slot-value cna 'width) (slot-value cna 'height) (slot-value cna 'left) (slot-value cna 'top)
                         (print-raw-object cna))
                )))))
      )))

;;;; functions ================================================================

;; Separate children into objects and options
(defun parse-children (object)
  
  ;; Allocate options storage
  (setf (slot-value object 'child-options) (make-array (length (content object)) :initial-element nil))

  ;; Separate the children and their options
  (dotimes (i (length (content object)))
    (let ((child (nth i (content object))))
      (when (consp child)
        
        (let ((child-object)
              (options (rest child)))
          (typecase (first child)
            (symbol
             (setq child-object (symbol-value (first child))))
            (t
             (setq child-object (first child))))
          ;; Make sure options area valid
          ;; #+safety
          (dolist (option options)
            (unless (member option +LAYOUT-CHILD-OPTIONS+)
              (error "unknown child option: ~a" option)))
          (setf (nth i (content object)) child-object)
          (setf (aref (slot-value object 'child-options) i) options)))))
  )
