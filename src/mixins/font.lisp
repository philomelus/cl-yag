(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; font-mixin ===============================================================

(deftype font-type () '(or null cffi::foreign-pointer))

(defclass font-mixin ()
  ((font :type font-type :initarg :font :initform nil :accessor font)))

