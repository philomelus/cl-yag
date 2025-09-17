(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; child-mixin ==============================================================

(defclass child-mixin ()
  ((child :initarg :child :initform nil :accessor child)))

