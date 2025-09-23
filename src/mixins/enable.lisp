(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; enable-mixin =============================================================

(defclass enable-mixin ()
  ((enabled :type boolean :initarg :enabled :initform nil :accessor enabled)))


