(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; THEME-MIXIN-BASE =========================================================

(defclass theme-mixin-base ()
  ())

;;;; THEME-MIXIN ==============================================================

(defclass theme-mixin (theme-mixin-base)
  ((theme :initarg :theme :initform nil :accessor theme)))

