(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; theme-mixin ==============================================================

(defclass theme-mixin ()
  ((theme :initarg :theme :initform nil :accessor theme)))

