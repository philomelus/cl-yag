(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; VISIBLE-MIXIN-BASE =======================================================

(defclass visible-mixin-base ()
  ())

;;;; VISIBLE-MIXIN ============================================================

(defclass visible-mixin (visible-mixin-base)
  ((visible :type boolean :initarg :visible :initform nil :accessor visible)))


