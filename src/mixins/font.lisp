(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; FONT-MIXIN-BASE ==========================================================

(defclass font-mixin-base ()
  ())

;;;; FONT-MIXIN ===============================================================

(deftype font-type () '(or null cffi::foreign-pointer))

(defclass font-mixin (font-mixin-base)
  ((font :type font-type :initarg :font :initform nil :accessor font)))

