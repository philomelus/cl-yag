(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; style-3d-mixin ===========================================================

(deftype style-3d-type () '(member nil :default :inset :outset :flat))

(defclass style-3d-mixin ()
  ((style :initarg :style :type style-3d-type :initform nil :accessor style)))

(defmethod initializa-instance :after ((object style-3d-mixin) &key)
  (with-slots (style) object
    (unless (typep style 'style-3d-type)
      (error "unknown style 3d option, got: ~a" style)))
  (my-next-method))
