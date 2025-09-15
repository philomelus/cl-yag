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

(defmethod update-layout-child-areas (index (object row-layout))
  "When a child has options changing the area used within the layout, this
recalculates the sizes of the children that are affected by it."
  (declare (ignorable index object))


  )


