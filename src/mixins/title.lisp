(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; title-mixin ==============================================================

(defclass title-mixin ()
  ((title :initarg :title :initform "" :type string :accessor title)))

(defmethod print-mixin ((o title-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-field-nil title o s)
  (my-next-method))

