(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; style-3d-mixin ===========================================================

(defvar +STYLE-3D-MIXIN-OPTIONS+ '(:default :inset :outset :flat))

(defclass style-3d-mixin ()
  ((style :initarg :style :type keyword :initform :default :accessor style)))

(defmethod initializa-instance :after ((object style-3d-mixin) &key)
  (with-slots (style) object
    (unless (member style +STYLE-3D-MIXIN-OPTIONS+)
      (error "unknown style 3d option, got: ~a" style)))
  (my-next-method))
