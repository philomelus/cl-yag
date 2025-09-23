(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; title-mixin ==============================================================

(defclass title-mixin ()
  ((title :type string :initarg :title :initform "" :accessor title)))

