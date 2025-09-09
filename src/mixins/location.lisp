(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; location-mixin ===========================================================

(defclass location-mixin ()
  ((x :initarg :x :initform 0 :accessor location-x)
   (y :initarg :y :initform 0 :accessor location-y)))

(defmethod print-mixin ((o location-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-field x o s :fmt "~d")
  ;; (pprint-field y o s :fmt "~d")
  (my-next-method))

(defmethod (setf location) (x y (object location-mixin))
  (setf (location-x object) x)
  (setf (location-y object) y)
  (my-next-method))

