(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; child-mixin ==============================================================

(defclass child-mixin ()
  ((child :initarg :child :initform nil :accessor child)))

(defmethod print-mixin ((o child-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-raw-nil child o s)
  (my-next-method))
