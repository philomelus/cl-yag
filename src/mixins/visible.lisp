(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; visible-mixin ============================================================

(defclass visible-mixin ()
  ((visible :type boolean :initarg :visible :initform nil :accessor visible)))


