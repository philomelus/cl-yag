(in-package #:cl-yag)

;;;; theme-mixin ==============================================================

(defclass theme-mixin ()
  ((theme :initarg :theme :initform nil :accessor theme)))

(defmethod print-mixin ((o theme-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-object-nil theme o s)
  (my-next-method))

