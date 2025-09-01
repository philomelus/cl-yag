(in-package #:cl-yag)

;;;; enable-mixin =============================================================

(defclass enable-mixin ()
  ((enabled :initarg :enabled :initform nil :type boolean :accessor enabled)))

(defmethod print-mixin ((o enable-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-field enabled o s)
  (my-next-method))

