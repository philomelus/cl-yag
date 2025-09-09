(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; active-mixin =============================================================

(defclass active-mixin ()
  ((active :initarg :active :initform nil :type boolean :accessor active)))

(defmethod print-mixin ((o active-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-field active o s)
  (my-next-method))

