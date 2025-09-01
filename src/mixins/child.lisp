(in-package #:cl-yag)

;;;; child-mixin ==============================================================

(defclass child-mixin ()
  ((child :initarg :child :initform nil :accessor child)))

(defmethod print-mixin ((o child-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-raw-nil child o s)
  (my-next-method))
