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

(defmethod calc-area ((object column-layout) &key)
  "Calculate the area of COLUMN-LAYOUT child widgets."
  
  (flet ((total-height (thing)
           (let ((total-internal 0))
             (loop :for row :from 0 :below (length (content thing)) :do
               (incf total-internal (slot-value (aref (slot-value thing 'child-area) row) 'height)))
             total-internal))
         (total-rect-height (rects)
           (let ((total-height 0))
             (loop :for row :from 0 :below (length rects) :do
               (incf total-height (slot-value (aref rects row) 'height)))
             total-height)))
    
    (v:info :layout "[calc-area] {column-layout} calculating child areas ~a" (print-raw-object object))
  
    ;; Calculate our internal child areas
    (assert (not (eql (slot-value object 'child-area) nil)))
    
    ;; At this point all vertical space should be used internally
    (assert (= (total-height object) (slot-value object 'height)))
  
    (with-slots (child-area content) object
      (with-local-slots ((object-height height)) object
        (let* ((child-count (length content))
               (requested-areas (make-array child-count :adjustable nil :fill-pointer nil))
               (requested-height 0))
          ;; Get initial area of children
          (loop :for row :from 0 :below child-count :do
            (let ((child (foro (nth row content))))
              (with-slots ((child-left left) (child-top top) (child-width width) (child-height height))
                  child
                (let (new-left new-top new-width new-height)
                  (if (keywordp child-width)
                      (setq new-width (calc-width child-width (aref child-area row) child))
                      (setq new-width child-width))
                  (if (keywordp child-height)
                      (setq new-height (calc-height child-height (aref child-area row) child))
                      (setq new-height child-height))
                  (if (keywordp child-left)
                      (setq new-left (calc-left child-left (aref child-area row) new-width new-height child))
                      (setq new-left child-left))
                  (if (keywordp child-top)
                      (setq new-top (calc-top child-top (aref child-area row) new-width new-height child))
                      (setq new-left child-left))
                  (incf requested-height new-height)
                  (setf (aref requested-areas row) (make-instance '%rect :left new-left :top new-top
                                                                         :width new-width :height new-height ))))))
          ;; Nothing should be changed yet (sanity check)
          (assert (= (total-height object) object-height))
        
          ;; Adjust for min/max height updates
          (let ((min-height-count 0)
                (max-height-count 0)
                (min-height-offsets ())
                (max-height-offsets ()))
            (loop :for row :from 0 :below child-count :do
              (let ((child-all (nth row content)))
                (when (consp child-all)
                  (let ((child-options (rest child-all)))
                    (multiple-value-bind (updated new-internal-area new-child-requested max-w min-w max-h min-h)
                        (layout-options-update-mm (aref child-area row) (aref requested-areas row) child-options)
                      (declare (ignorable min-w max-w))
                      (when updated
                        (setf (aref child-area row) new-internal-area)
                        (setf (aref requested-areas row) new-child-requested))
                      (when min-h
                        (incf min-height-count)
                        (push row min-height-offsets))
                      (when max-h
                        (incf max-height-count)
                        (push row max-height-offsets)))))))

            ;; Perform any min/max adjustments
            (when (and (> min-height-count 0))
              (v:info :layout "[calc-area] {column-layout} adjusting for :min-height")
            
              ;; Tally the min-height that needs reallocated
              (let ((extra-height 0))
                (dolist (offset min-height-offsets)
                  (let ((internal-height (slot-value (aref child-area offset) 'height))
                        (child-height (slot-value (aref requested-areas offset) 'height)))
                    (when (< child-height internal-height)
                      (let ((extra (- child-height internal-height)))
                        (assert (> extra 0))
                        (incf extra-height extra)
                        (decf (slot-value requested-areas 'top) extra)))))
                (let ((requested-difference (- object-height requested-height)))
                  (when (> requested-difference 0)
                    (incf extra-height requested-difference)
                    (setq requested-height object-height)))
                (v:info :layout "[calc-area] {column-layout} :min-height gave away ~f" extra-height)
                
                ;; Give the extra vertical space to others
                (when (> extra-height 0)
                  ;; Give extra space away
                  (if (> max-height-count 0)
                      ;; Give it to the max-height children
                      (progn
                        (v:info :layout "[calc-area] {column-layout} giving :min-height extra to :max-height siblings")
                        (assert (= (length max-height-offsets) max-height-count))
                        ;; TODO:  When large, give away blocks first
                        (loop :while (> extra-height 0) :do
                          (loop :for row :from 0 :below max-height-count :do
                            (incf (slot-value (aref requested-areas row) 'height))
                            (decf extra-height)
                            (when (= extra-height 0)
                              (return)))))
                      ;; Spread it across all non-min-height children
                      (progn
                        (v:info :layout "[calc-area] {column-layout} giving :min-height extra to siblings")
                        (assert (= (length min-height-offsets) min-height-count))
                        (when (= child-count min-height-count)
                          (error "there is no place to allocate extra space from :min-height options"))
                        ;; If the extra space if greather than number of
                        ;; siblings, attempt to give most of it away quickly
                        (let ((child-sibling-count (- child-count min-height-count)))
                          (when (> extra-height child-sibling-count)
                            (let ((split-extra-height (truncate (/ extra-height child-sibling-count))))
                              (loop :for row :from 0 :below child-count :do
                                (when (not (member row min-height-offsets))
                                  (incf (slot-value (aref requested-areas row) 'height) split-extra-height)
                                  (v:info :layout "[calc-area] {column-layout} gave ~d to sibling ~d"
                                          split-extra-height row)
                                  (decf extra-height split-extra-height))))))
                        ;; Give away the rest if needed
                        (when (> extra-height 0)
                          (loop :while (> extra-height 0) :do
                            (loop :for row :from 0 :below min-height-count :do
                              (when (not (member row min-height-offsets))
                                (incf (slot-value (aref requested-areas row) 'height))
                                (v:info :layout "[calc-area] {column-layout} gave 1 to sibling ~d" row)
                                (decf extra-height)
                                (when (= extra-height 0)
                                  (return)))))))))))

            ;; Allocate any extra (get here only when no :min-height in
            ;; siblings)
            (let ((extra-height (- object-height requested-height)))
              (when (> extra-height 0)
                (assert (= min-height-count 0))
                (v:info :layout "[calc-area] {column-layout} extra height:~d" (- object-height requested-height))
                ;; Give it away, give it away now
                (if (> max-height-count 0)
                    (progn
                      (error "not implemented"))
                    ;; Spread it across all non-max-height children
                    (progn
                      (v:info :layout "[calc-area] {column-layout} giving extra away")
                      ;; If the extra space if greather than number of
                      ;; siblings, attempt to give most of it away quickly
                      (when (> extra-height child-count)
                        (let ((split-extra-height (truncate (/ extra-height child-count))))
                          (loop :for row :from 0 :below child-count :do
                            (incf (slot-value (aref requested-areas row) 'height) split-extra-height)
                            (v:info :layout "[calc-area] {column-layout} gave ~d to child ~d"
                                    split-extra-height row)
                            (decf extra-height split-extra-height))))
                      ;; Give away the rest if needed
                      (when (> extra-height 0)
                        (loop :while (> extra-height 0) :do
                          (loop :for row :from 0 :below child-count :do
                            (incf (slot-value (aref requested-areas row) 'height))
                            (v:info :layout "[calc-area] {column-layout} gave 1 to child ~d" row)
                            (decf extra-height)
                            (when (= extra-height 0)
                              (return)))))))))
          
            ;; At this point all vertical space should be used, consistancy check
            (assert (= (total-rect-height requested-areas) object-height))
        
            ;; Finalize the child options
            (loop :for row :from 0 :below child-count :do
              (when (consp (nth row content))
                (let ((options (rest (nth row content))))
                  (multiple-value-bind (updated new-internal-area new-child-area)
                      (layout-options-update-h (aref child-area row) (aref requested-areas row) options)
                    (when updated
                      (setf (aref child-area row) new-internal-area)
                      (setf (aref requested-areas row) new-child-area)))
                  (multiple-value-bind (updated new-internal-area new-child-area)
                      (layout-options-update-v (aref child-area row) (aref requested-areas row) options)
                    (when updated
                      (setf (aref child-area row) new-internal-area)
                      (setf (aref requested-areas row) new-child-area)))))))

          ;; Update the children
          (loop :for row :from 0 :below child-count :do
            (let ((child-object (foro (nth row content))))
              (unless (eql child-object nil)
                (setf (slot-value child-object 'width) (slot-value (aref requested-areas row) 'width))
                (setf (slot-value child-object 'height) (slot-value (aref requested-areas row) 'height))
                (setf (slot-value child-object 'left) (slot-value (aref requested-areas row) 'left))
                (setf (slot-value child-object 'top) (slot-value (aref requested-areas row) 'top))
                (v:info :layout "[calc-area] {column-layout} child ~d area (~f ~f) @ (~f ~f)"
                        row (slot-value child-object 'width) (slot-value child-object 'height)
                        (slot-value child-object 'left) (slot-value child-object 'top))))))))))

(defmethod layout-changed ((object column-layout) &key (parent nil parentp) (child nil childp))
  (declare (ignorable object parent parentp child childp))
  (unless (slot-value object 'changing)
    ))

(defun layout-options-update-h (internal-area child-area options)
  "Return adjusted internal and child areas for horizontal options. Also returns
T/NIL of whether anything changed."

  (let ((new-internal-area (shallow-copy-object internal-area))
        (new-child-area (shallow-copy-object child-area))
        (changed nil))
    (flet ((update-center ()
             (let ((internal-width (slot-value internal-area 'width))
                   (child-width (slot-value child-area 'width)))
               (when (> internal-width child-width)
                 (setf (slot-value new-child-area 'left)
                       (+ (slot-value internal-area 'left)
                          (/ (- internal-width child-width) 2))))))
           (update-left ()
             (let ((internal-left (slot-value internal-area 'left)))
               (if (< internal-left (slot-value child-area 'left))
                   (setf (slot-value new-child-area 'left) internal-left))))
           

           (update-right ()
             (let ((internal-right (+ (slot-value internal-area 'left)
                                      (slot-value internal-area 'width)))
                   (child-right (+ (slot-value child-area 'left)
                                   (slot-value child-area 'width))))
               (when (> internal-right child-right)
                 (setf (slot-value new-child-area 'left)
                       (- internal-right (slot-value child-area 'width)))))))
      (dolist (option options)
        (case option
          (:center (update-center))
          (:left (update-left))
          (:right (update-right)))))
    (values changed new-internal-area new-child-area)))

(defun layout-options-update-mm (internal-area child-area options)
  "Return adjusted internal and child areas for min/max width/height options.

Arguments:

    internal-area = %rect of internal area allocated to child
    child-area    = %rect used by child
    options       = list of LAYOUT-CHILD-OPTIONS for child 

Return values:

    anyting-changed (t/nil)
    updated-internal-area (%rect)
    updated-child-area (%rect)
    max-width-encountered (t/nil)
    min-width-encountered (t/nil)
    max-height-encountered (t/nil)
    min-height-encountered (t/nil)
"

  (let ((new-internal-area (shallow-copy-object internal-area))
        (new-child-area (shallow-copy-object child-area))
        (max-h nil)
        (max-w nil)
        (min-h nil)
        (min-w nil)
        (changed nil))
    (flet ((update-max-height ()
             (assert (not max-h))
             (setq max-h t)
             (let ((internal-height (slot-value internal-area 'height)))
               (when (> internal-height (slot-value child-area 'height))
                 (setf (slot-value new-child-area 'height) internal-height)
                 (setq changed t))))
           (update-max-width ()
             (assert (not max-w))
             (setq max-w t)
             (let ((internal-width (slot-value internal-area 'width)))
               (when (> internal-width (slot-value child-area 'width))
                 (setf (slot-value new-child-area 'width) internal-area)
                 (setq changed t))))
           (update-min-height ()
             (assert (not min-h))
             (setq min-h t)
             (let ((child-height (slot-value child-area 'height)))
               (when (< child-height (slot-value internal-area 'height))
                 (setf (slot-value new-internal-area 'height) child-height)
                 (setq changed t))))
           (update-min-width ()
             (assert (not min-w))
             (setq min-w t)
             (let ((child-width (slot-value child-area 'width)))
               (when (< child-width (slot-value internal-area 'width))
                 (setf (slot-value new-internal-area 'width) child-width)
                 (setq changed t)))))
      (dolist (option options)
        (case option
          (:min-height (update-min-height))
          (:min-width (update-min-width))
          (:max-height (update-max-height))
          (:max-width (update-max-width)))))
    (values changed new-internal-area new-child-area max-w min-w max-h min-h)))

(defun layout-options-update-v (internal-area child-area options)
  "Return adjusted internal are child areas for vertical options. Also returns
T/NIL of whether anything changed."
  
  (let ((new-internal-area (shallow-copy-object internal-area))
        (new-child-area (shallow-copy-object child-area))
        (changed nil))
    (flet ((update-bottom ()
             (let ((internal-top (slot-value internal-area 'top))
                   (internal-height (slot-value internal-area 'height))
                   (child-top (slot-value child-area 'top))
                   (child-height (slot-value child-area 'height)))
               (let ((internal-bottom (+ internal-top internal-height))
                     (child-bottom (+ child-top child-height)))
                 (when (> internal-bottom child-bottom)
                   (setf (slot-value new-child-area 'top)
                         (- internal-bottom child-height))
                   (setq changed t)))))
           (update-middle ()
             (let ((internal-height (slot-value internal-area 'height))
                   (child-height (slot-value child-area 'height)))
               (when (> internal-height child-height)
                 (setf (slot-value new-child-area 'top)
                       (+ (slot-value internal-area 'top)
                          (/ (- internal-height child-height) 2)))
                 (setq changed t))))
           (update-top ()
             (let ((internal-top (slot-value internal-area 'top)))
               (when (< internal-top (slot-value child-area 'top))
                 (setf (slot-value new-child-area 'top)
                       (slot-value internal-area 'top))
                 (setq changed t)))))
      
      (dolist (option options)
        (case option
          (:bottom (update-bottom))
          (:middle (update-middle))
          (:top (update-top)))))
    (values changed new-internal-area new-child-area)))

(defmethod calc-layout-child-areas ((object column-layout))
  "Calculate the over-all area of each child.
Note that children may use different sizes themselves, this is just the
area allocated to them, whether they choose to use it or not."

  (with-slots (child-area content) object
    (let ((child-count (length content)))
      ;; Allocate internal child area
      (setf child-area (make-array child-count :adjustable nil :initial-element nil))
      (loop :for row :from 0 :below child-count :do
        (setf (aref child-area row) (make-instance '%rect)))

      (with-local-slots ((object-left left) (object-top top) (object-width width) (object-height height))
                        object
        (let ((initial-height (truncate (/ object-height child-count)))
              (distribute-height (mod object-height child-count))
              (available-height object-height)
              (current-top object-top))

          ;; Calculate internal area of children
          (loop :for row :from 0 :below child-count :do
            (with-slots ((internal-left left) (internal-top top) (internal-width width) (internal-height height))
                (aref child-area row)
              (setf internal-left object-left)
              (setf internal-width object-width)
              (setf internal-height initial-height)
              (setf internal-top current-top))
            (decf available-height initial-height)
            (incf current-top initial-height))

          ;; Distribute any excess across children
          ;; TODO: Odd in middle, then from edges
          (let ((row -1))
            (loop :while (> distribute-height 0) :do
              ;; Update offset
              (incf row)
              (when (> row child-count)
                (setq row 0))

              ;; Give one away
              (incf (slot-value (aref child-area row) 'height))
              (decf distribute-height))))))))

