(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; theme-mixin ==============================================================

(defclass theme-mixin ()
  ((theme :initarg :theme :initform nil :accessor theme)))

(defmethod print-mixin ((o theme-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-object-nil theme o s)
  (my-next-method))

