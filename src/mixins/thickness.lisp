(in-package :cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

;;;; THICKNESS-MIXIN-BASE =====================================================

(defclass thickness-mixin-base ()
  ())

;;;; THICKNESS-MIXIN ==========================================================

(defclass thickness-mixin (thickness-mixin-base)
  ((thickness :initarg :thickness :initform nil :accessor thickness)))

