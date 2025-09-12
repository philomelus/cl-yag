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

(defmethod calc-area (child (parent row-layout) &key)
  (declare (ignorable child parent))
  (error "not implemented"))

(defmethod calc-layout-child-areas ((object row-layout))
  "Calculate the over-all area of each child.
Note that children may use different sizes themselves, this is just the
area allocated to them, whether they choose to use it or not."
  (declare (ignorable object))
  (error "not implemented"))

(defmethod update-layout-child-areas (index (object row-layout))
  "When a child has options changing the area used within the layout, this
recalculates the sizes of the children that are affected by it."
  (declare (ignorable index object))
  (error "not implemented"))


