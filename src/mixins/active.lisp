(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; active-mixin =============================================================

(defclass active-mixin ()
  ((active :initarg :active :initform nil :type boolean :accessor active)))


