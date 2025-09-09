(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; layout-mixin =============================================================

(defclass layout-mixin ()
  ((layout :initarg :layout :initform nil :accessor layout)))

(defmethod print-mixin ((o layout-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-field layout o s)
  (my-next-method))
