(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; padding-mixin ============================================================

(defclass padding-mixin ()
  ((padding-left :initarg :padding-left :initform 0 :accessor padding-left)
   (padding-right :initarg :padding-right :initform 0 :accessor padding-right)
   (padding-top :initarg :padding-top :initform 0 :accessor padding-top)
   (padding-bottom :initarg :padding-bottom :initform 0 :accessor padding-bottom)))

(defmethod print-mixin ((o padding-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-field padding-left o s :fmt "~d")
  ;; (pprint-field padding-top o s :fmt "~d")
  ;; (pprint-field padding-right o s :fmt "~d")
  ;; (pprint-field padding-bottom o s :fmt "~d")
  (my-next-method))

(defmethod (setf padding) (value (object padding-mixin))
  (setf (padding-h object) value)
  (setf (padding-v object) value)
  (my-next-method))

(defmethod (setf padding-h) (value (object padding-mixin))
  (unless (typep value 'float)
    (setq value (coerce value 'float)))
  (setf (padding-left object) value)
  (setf (padding-right object) value)
  (my-next-method))

(defmethod (setf padding-v) (value (object padding-mixin))
  (unless (typep value 'float)
    (setq value (coerce value 'float)))
  (setf (padding-top object) value)
  (setf (padding-bottom object) value)
  (my-next-method))

