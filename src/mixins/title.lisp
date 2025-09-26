(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; TITLE-MIXIN-BASE =========================================================

(defclass title-mixin-base ()
  ())

;;;; TITLE-MIXIN ==============================================================

(defclass title-mixin (title-mixin-base)
  ((title :type string :initarg :title :initform "" :accessor title)))

