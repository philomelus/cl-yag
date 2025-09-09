(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; macros ===================================================================

(defmacro with-borders ((&rest vars) object &body body)
  (assert (= (length vars) 4))
  `(let ((,(first vars) (border-left ,object))
         (,(second vars) (border-right ,object))
         (,(third vars) (border-top ,object))
         (,(fourth vars) (border-bottom ,object)))
     ,@body))

;;;; border-base ==============================================================

(defclass border-theme-mixin ()
  ())

(defclass border-flat-theme-mixin (color-mixin)
  ())

(defclass border-3d-theme-mixin (color-3d-mixin
                                 style-3d-mixin)
  ((thickness :initform 2.0)
   (style :accessor border-style)))

(defclass border (border-theme-mixin)
  ((thickness :initarg :thickness :initform 1.0 :accessor thickness)))

(defmacro defborder (&rest rest &key &allow-other-keys)
  `(make-instance 'border ,@rest))

;;;; border-mixin =============================================================

(defclass border-mixin ()
  ((border-left :initarg :border-left :initform nil :accessor border-left)
   (border-right :initarg :border-right :initform nil :accessor border-right)
   (border-top :initarg :border-top :initform nil :accessor border-top)
   (border-bottom :initarg :border-bottom :initform nil :accessor border-bottom)))

(defmethod print-mixin ((o border-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-indent :current 0 s)
  ;; (pprint-object-nil border-left o s)
  ;; (pprint-object-nil border-right o s)
  ;; (pprint-object-nil border-top o s)
  ;; (pprint-object-nil border-bottom o s)
  (my-next-method))

(defmethod (setf border) ((value border) (object border-mixin))
  (setf (border-h object) value)
  (setf (border-v object) value)
  (my-next-method))

(defmethod (setf border-h) ((value border) (object border-mixin))
  (setf (border-left object) value)
  (setf (border-right object) value)
  (my-next-method))

(defmethod (setf border-v) ((value border) (object border-mixin))
  (setf (border-top object) value)
  (setf (border-bottom object) value)
  (my-next-method))
