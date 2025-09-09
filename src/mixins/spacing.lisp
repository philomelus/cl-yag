(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; spacing-mixin ============================================================

(defclass spacing-mixin ()
  ((spacing-left :initarg :spacing-left :initform 0 :accessor spacing-left)
   (spacing-top :initarg :spacing-top :initform 0 :accessor spacing-top)
   (spacing-right :initarg :spacing-right :initform 0 :accessor spacing-right)
   (spacing-bottom :initarg :spacing-bottom :initform 0 :accessor spacing-bottom)))

(defmethod print-mixin ((o spacing-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-field spacing-left o s :fmt "~a")
  ;; (pprint-field spacing-top o s :fmt "~a")
  ;; (pprint-field spacing-right o s :fmt "~a")
  ;; (pprint-field spacing-bottom o s :fmt "~a")
  (my-next-method))

(defmethod (setf spacing) (value (object spacing-mixin))
  (setf (spacing-h object) value)
  (setf (spacing-v object) value)
  (my-next-method))

(defmethod (setf spacing-h) (value (object spacing-mixin))
  (setf (spacing-left object) value)
  (setf (spacing-right object) value)
  (my-next-method))

(defmethod (setf spacing-v) (value (object spacing-mixin))
  (setf (spacing-top object) value)
  (setf (spacing-bottom object) value)
  (my-next-method))

