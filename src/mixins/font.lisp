(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; font-mixin ===============================================================

(defclass font-mixin ()
  ((font :initarg :font :initform nil :accessor font)))

