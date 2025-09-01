(in-package #:cl-yag)

;;;; title-mixin ==============================================================

(defclass title-mixin ()
  ((title :initarg :title :initform "" :type string :accessor title)))

(defmethod print-mixin ((o title-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-field-nil title o s)
  (my-next-method))

