(in-package #:cl-yag)

;;;; layout-mixin =============================================================

(defclass layout-mixin ()
  ((layout :initarg :layout :initform nil :accessor layout)))

(defmethod print-mixin ((o layout-mixin) s)
  (pprint-field layout o s)
  (my-next-method))
