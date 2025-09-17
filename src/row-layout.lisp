(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; row-layout ============================================================

(defclass row-layout (layout-base
                      padding-mixin     ; border to interior
                      spacing-mixin)    ; outside to border
  ())

(defmacro defrow-layout (&rest rest &key &allow-other-keys)
  `(make-instance 'row-layout ,@rest))

;;; methods ---------------------------------------------------------

(defmethod calc-layout-child-areas ((object row-layout))
  "Calculate the over-all area of each child.
Note that children may use different sizes themselves, this is just the
area allocated to them, whether they choose to use it or not."
  
  (with-slots (child-area) object
    (let ((num-children (length (content object))))
      (setf child-area (make-array num-children :adjustable nil))
      
      (with-local-slots (left top width height) object
        (let* ((num-to-adjust (truncate (mod width num-children)))
               (base-width (truncate (/ width num-children))))
          (assert (< num-to-adjust num-children))
      
          ;; Set calculated initial area for children
          (loop :for i :from 0 :below num-children :do
            ;; We only do horizontal space, so top and height are from object
            ;; but left and width are calculated per child offset
            (setf (aref child-area i) (make-instance '%rect :left (+ left (* i base-width))
                                                            :top top
                                                            :width base-width
                                                            :height height)))

          ;; If there are leftover area, and its an odd count, allocate some
          ;; for center item
          (when (oddp num-to-adjust)
            (incf (width (aref child-area (truncate (/ num-children 2)))))
            (decf num-to-adjust))

          ;; If there is still leftover area, allocate to ends until used up
          (assert (evenp num-to-adjust))
          (when (> num-to-adjust 0)
            (do ((edge1 0 (incf edge1))
                 (edge2 (1- num-children) (decf edge2)))
                ((= num-to-adjust 0))
              (incf (width (aref child-area edge1)))
              (incf (width (aref child-area edge2)))
              (decf num-to-adjust 2)))

          (dotimes (n num-children)
            (v:debug :layout "[calc-layout-child-areas] {row-layout} child ~d internal area (~d ~d) @ (~d ~d)" n
                     (width (aref child-area n))
                     (height (aref child-area n))
                     (left (aref child-area n))
                     (top (aref child-area n)))))))))

(defmethod layout-changed ((object row-layout) &key (parent nil parentp) (child nil childp))
  (declare (ignorable parent parentp child childp))
  (unless (slot-value object 'changing)
    (assert (not (eql parentp childp)))
    (if parentp
        ;; Something affecting our parents layout changed
        (progn
          (assert parent)
          (unless (eql (slot-value object 'child-area) nil)
            (v:info :layout "[layout-changed] {row-layout} forcing relayout")
            (setf (slot-value object 'child-area) nil)
            (dolist (child (content object))
              (when (typep child 'area-mixin)
                (when (slot-value child 'width-calc)
                  (setf (slot-value child 'width) (slot-value child 'width-calc)))
                (when (slot-value child 'height-calc)
                  (setf (slot-value child 'height) (slot-value child 'height-calc)))
                (when (slot-value child 'left-calc)
                  (setf (slot-value child 'left) (slot-value child 'left-calc)))
                (when (slot-value child 'top-calc)
                  (setf (slot-value child 'top) (slot-value child 'top-calc)))))
            ;; NOTE: EXACT same code/logic as RESET-AREA, but we aren't an
            ;; area-mixin so we can't use it.
            (macrolet ((update-value (dest src)
                         `(when (slot-value object ,src)
                            (setf (slot-value object ,dest) (slot-value object ,src)))))
              (update-value 'width 'width-calc)
              (update-value 'height 'height-calc)
              (update-value 'left 'left-calc)
              (update-value 'top 'top-calc))            
            (calc-layout-area object)
            (calc-layout-child-areas object)))
        ;; Something affecting our child layouts changed
        (progn
          (assert child)
          (error "not implemented")))))

(defmethod update-layout-child-areas (index (object row-layout))
  "When a child has options changing the area used within the layout, this
recalculates the sizes of the children that are affected by it."
  (declare (ignorable index object))
  
  ;; Figure out what fields to take from object
  (let ((upd-ch-l-p nil) (upd-ch-t-p nil) (upd-ch-w-p nil) (upd-ch-h-p nil))
    
    ;; Update internal area if field wasn't calculated
    (macrolet ((update-p (field field-calc)
                 `(if (not (slot-value (foro (nth index (content object))) ',field-calc))
                      (progn
                        (v:debug :layout "[update-layout-child-areas] {row-layout} child ~d internal ~a needs update"
                                 index (symbol-name ',field))
                        t)
                      nil)))
      (setq upd-ch-l-p (update-p left left-calc))
      (setq upd-ch-t-p (update-p top top-calc))
      (setq upd-ch-w-p (update-p width width-calc))
      (setq upd-ch-h-p (update-p height height-calc)))
    
    ;; Update internal area from control based on options
    (let ((options)
          (co (nth index (content object))))
      (when (consp co)
        (setq options (rest co)))
      (dolist (opt options)
        (case opt
          (:bottom
           (error "not implemented"))
          (:center
           (error "not implemented"))
          (:left
           (error "not implemented"))
          (:middle
           (error "not implemented"))
          (:min-height
           (setq upd-ch-h-p t)
           (v:debug :layout "[update-layout-child-areas] {row-layout} child ~d has option :min-height" index))
          (:min-width
           (setq upd-ch-w-p t)
           (v:debug :layout "[update-layout-child-areas] {row-layout} child ~d has option :min-width" index ))
          (:max-height
           (setq upd-ch-h-p t)
           (v:debug :layout "[update-layout-child-areas] {row-layout} child ~d has option :max-height" index))
          (:max-width
           (setq upd-ch-w-p t)
           (v:debug :layout "[update-layout-child-areas] {row-layout} child ~d has option :min-width" index ))
          (:right
           (error "not implemented"))
          (:top
           (error "not implemented")))))
    
    (let* ((num-children (length (content object)))
           (affected (- num-children index 1)))

      ;; Nothing to do if it was the only or last child
      (unless (> affected 0)
        (return-from update-layout-child-areas))

      ;; Update affected children
      (with-slots (child-area) object

        (v:debug :layout "[update-layout-child-areas] {row-layout} child ~d updating internal:" index)
        (v:debug :layout "[update-layout-child-areas] {row-layout}         {~a}" (print-raw-object (foro (nth index (content object)))))
        (v:debug :layout "[update-layout-child-areas] {row-layout}         from (~d ~d) @ (~d ~d)"
                 (width (aref child-area index)) (height (aref child-area index))
                 (left (aref child-area index)) (top (aref child-area index)))
        (let ((co (foro (nth index (content object)))))
          (v:debug :layout "[update-layout-child-areas] {row-layout}         to (~d ~d) @ (~d ~d)"
                   (slot-value co 'width) (slot-value co 'height)
                   (slot-value co 'left) (slot-value co 'top)))

        ;; Calculate newly available area
        (assert (= (- (height (aref child-area index)) (height (foro (nth index (content object))))) 0))
        (let ((new-width (- (width (aref child-area index))
                            (width (foro (nth index (content object)))))))
          ;; Add width of all prior siblings
          (loop :for i :from (1+ index) :to (1- num-children) :do
            (incf new-width (width (aref child-area i))))

          (v:debug :layout "[update-layout-child-areas] {row-layout} siblings available new width ~d" new-width)
          
          ;; Recalculate rest of the children
          (let ((new-child-width (truncate (/ new-width affected)))
                (width-to-spread (mod new-width affected)))

            ;; Update child area to objects actual area
            (with-slots (content) object
              (when upd-ch-h-p
                (setf (height (aref child-area index)) (slot-value (foro (nth index content)) 'height))
                (v:debug :layout "[update-layout-child-areas] {row-layout} child ~d internal height updated (~d)"
                         index (height (aref child-area index))))
              (when upd-ch-w-p
                (setf (width (aref child-area index)) (slot-value (foro (nth index content)) 'width))
                (v:debug :layout "[update-layout-child-areas] {row-layout} child ~d internal width updated (~d)"
                         index (width (aref child-area index))))
              (when upd-ch-l-p
                (setf (left (aref child-area index)) (slot-value (foro (nth index content)) 'left))
                (v:debug :layout "[update-layout-child-areas] {row-layout} child ~d internal left updated (~d)"
                         index (left (aref child-area index))))
              (when upd-ch-t-p
                (setf (top (aref child-area index)) (slot-value (foro (nth index content)) 'top))
                (v:debug :layout "[update-layout-child-areas] {row-layout} child ~d internal top updated (~d)"
                         index (top (aref child-area index)))))
            
            ;; Update child areas
            (loop :for i :from (1+ index) :to (1- num-children) :do
              (let ((new-left (+ (right (aref child-area (1- i))) 1))
                    (new-width new-child-width))

                (let ((child (foro (nth i (content object))))
                      (child-a (aref child-area i)))

                  ;; Save new internal width
                  (setf (width child-a) new-width)
                  
                  ;; Force recalc of child width if originally calculated
                  (when (slot-value child 'width-calc)
                    (setf (slot-value child 'width) (slot-value child 'width-calc)))

                  ;; Save new internal left
                  (setf (left child-a) new-left)

                  ;; Force recalc of child left if originally calculated
                  (when (slot-value child 'left-calc)
                    (setf (slot-value child 'left) (slot-value child 'left-calc)))

                  ;; For layouts that changed, force full recalculation of area
                  (when (typep child 'layout-base)
                    (v:debug :layout "[update-layout-child-areas] {row-layout} child ~d layout forced recalc ~a"
                             i (print-raw-object child))
                    (setf (slot-value child 'child-area) nil)
                    (dolist (gc (content child))
                      (when (typep gc 'area-mixin)
                        (reset-area gc)))))))
            
            ;; Allocate left over space
            (when (> width-to-spread 0)
              (do ((next (1+ index) (+ next 1)))
                  ((= width-to-spread 0))
                (incf (width (aref child-area next)))
                (decf width-to-spread)))

            ;; Dump updates
            (dotimes (n affected)
              (let* ((i (+ index n 1))
                     (r (aref child-area i))
                     (cna (foro (nth i (content object)))))
                (v:debug :layout "[update-layout-child-areas] {row-layout} child ~d new internal area (~d ~d) @ (~d ~d)"
                         i (width r) (height r) (left r) (top r))
                (v:debug :layout "[update-layout-child-areas] {row-layout} child ~d       actual area (~d ~d) @ (~d ~d)" i
                         (slot-value cna 'width) (slot-value cna 'height) (slot-value cna 'left) (slot-value cna 'top))))))))))


