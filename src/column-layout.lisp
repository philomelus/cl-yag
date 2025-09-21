(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; column-layout ============================================================

(defclass column-layout (layout-base
                         padding-mixin  ; border to interior
                         spacing-mixin) ; outside to border
  ())

(defmacro defcolumn-layout (&rest rest &key &allow-other-keys)
  `(make-instance 'column-layout ,@rest))

;;; methods ---------------------------------------------------------

(defmethod calc-layout-child-areas ((object column-layout))
  "Calculate the over-all area of each child.
Note that children may use different sizes themselves, this is just the
area allocated to them, whether they choose to use it or not."

  (with-slots (child-area) object
    (let ((num-children (length (content object))))
     (setf child-area (make-array num-children :adjustable nil))
    
     (let* ((num-to-adjust (truncate (mod (height object) num-children)))
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
         (v:debug :layout "[calc-layout-child-areas] {column-layout} child ~d internal area (~d ~d) @ (~d ~d)" n
                  (width (elt child-area n))
                  (height (elt child-area n))
                  (left (elt child-area n))
                  (top (elt child-area n))))))))

(defmethod update-layout-child-areas (index (object column-layout))
  "When a child has options changing the area used within the layout, this
recalculates the sizes of the children that are affected by it."

  (v:debug :layout "[update-layout-child-area] {column-layout} updating child ~d ~a"
           index (print-raw-object (nth index (content object))))

  ;; Figure out what fields to take from object
  (let ((upd-ch-l-p nil) (upd-ch-t-p nil) (upd-ch-w-p nil) (upd-ch-h-p nil))
    
    ;; Update internal area if field wasn't calculated
    (macrolet ((update-p (field field-calc)
                 `(if (not (slot-value (foro (nth index (content object))) ',field-calc))
                      (progn
                        (v:debug :layout "[update-layout-child-areas] {column-layout} child ~d internal ~a needs update"
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
           (v:debug :layout "[update-layout-child-areas] {column-layout} child ~d has option :min-height" index))
          (:min-width
           (setq upd-ch-w-p t)
           (v:debug :layout "[update-layout-child-areas] {column-layout} child ~d has option :min-width" index ))
          (:max-height
           (setq upd-ch-h-p t)
           (v:debug :layout "[update-layout-child-areas] {column-layout} child ~d has option :max-height" index))
          (:max-width
           (setq upd-ch-w-p t)
           (v:debug :layout "[update-layout-child-areas] {column-layout} child ~d has option :min-width" index ))
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

        (v:debug :layout "[update-layout-child-areas] {column-layout} child ~d updating internal:" index)
        (v:debug :layout "[update-layout-child-areas] {column-layout}         {~a}" (print-raw-object (foro (nth index (content object)))))
        (v:debug :layout "[update-layout-child-areas] {column-layout}         from (~d ~d) @ (~d ~d)"
                 (width (aref child-area index)) (height (aref child-area index))
                 (left (aref child-area index)) (top (aref child-area index)))
        (let ((co (foro (nth index (content object)))))
          (v:debug :layout "[update-layout-child-areas] {column-layout}         to (~d ~d) @ (~d ~d)"
                   (slot-value co 'width) (slot-value co 'height)
                   (slot-value co 'left) (slot-value co 'top)))

        ;; Calculate newly available area
        (let ((new-height (- (height (aref child-area index))
                             (height (foro (nth index (content object)))))))
          ;; Add height of all prior siblings
          (loop :for i :from (1+ index) :to (1- num-children) :do
            (incf new-height (height (aref child-area i))))

          (v:debug :layout "[update-layout-child-areas] {column-layout} siblings available new height ~d" new-height)
          
          ;; Recalculate rest of the children
          (assert (= (- (width (aref child-area index)) (width (foro (nth index (content object))))) 0))
          (let ((new-child-height (truncate (/ new-height affected)))
                (height-to-spread (mod new-height affected)))

            ;; Update child area to objects actual area
            (with-slots (content) object
              (when upd-ch-h-p
                (setf (slot-value (aref child-area index) 'width) (slot-value (foro (nth index content)) 'height))
                (v:debug :layout "[update-layout-child-areas] {column-layout} child ~d internal height updated (~d)"
                         index (height (aref child-area index))))
              (when upd-ch-w-p
                (setf (slot-value (aref child-area index) 'height) (slot-value (foro (nth index content)) 'width))
                (v:debug :layout "[update-layout-child-areas] {column-layout} child ~d internal width updated (~d)"
                         index (width (aref child-area index))))
              (when upd-ch-l-p
                (setf (slot-value (aref child-area index) 'left) (slot-value (foro (nth index content)) 'left))
                (v:debug :layout "[update-layout-child-areas] {column-layout} child ~d internal left updated (~d)"
                         index (left (aref child-area index))))
              (when upd-ch-t-p
                (setf (slot-value (aref child-area index) 'top) (slot-value (foro (nth index content)) 'top))
                (v:debug :layout "[update-layout-child-areas] {column-layout} child ~d internal top updated (~d)"
                         index (top (aref child-area index)))))
            
            ;; Update child areas
            (loop :for i :from (1+ index) :to (1- num-children) :do
              (let ((new-top (+ (bottom (aref child-area (1- i))) 1))
                    (new-height new-child-height))

                (let ((child (foro (nth i (content object))))
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
                    (v:debug :layout "[update-layout-child-areas] {column-layout} child ~d layout forced recalc ~a"
                             i (print-raw-object child))
                    (setf (slot-value child 'child-area) nil)
                    (dolist (gc (content child))
                      (when (typep gc 'area-mixin)
                        (reset-area gc)))))))
            
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
                     (cna (foro (nth i (content object)))))
                (v:debug :layout "[update-layout-child-areas] {column-layout} child ~d new internal area (~d ~d) @ (~d ~d)"
                         i (width r) (height r) (left r) (top r))
                (v:debug :layout "[update-layout-child-areas] {column-layout} child ~d       actual area (~d ~d) @ (~d ~d)" i
                         (slot-value cna 'width) (slot-value cna 'height) (slot-value cna 'left) (slot-value cna 'top))))))))))


