(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))


;;;; THEME-MIXIN  ==============================================================

(defclass border-theme-mixin ()
  ((thickness :initarg :thickness :accessor thickness)))

(defclass border-theme-3d-mixin (color-3d-mixin
                                 style-3d-mixin)
  ((thickness :initform 2.0)
   (style :accessor border-style)))

(defclass border-theme-flat-mixin (color-mixin)
  ())

(defclass border (border-theme-mixin)
  ((thickness :initform 1.0)))

(defmacro defborder (&rest rest &key &allow-other-keys)
  `(make-instance 'border ,@rest))

;;;; BORDER-MIXIN-BASE ========================================================

(defclass border-mixin-base ()
  ()
  (:documentation "Any object derived from this or its descendents have the ability to draw a
border somewhere."))

;;;; BORDER-MIXIN =============================================================

(defclass border-mixin (border-mixin-base)
  ((border-left :initarg :border-left :initform nil :accessor border-left)
   (border-right :initarg :border-right :initform nil :accessor border-right)
   (border-top :initarg :border-top :initform nil :accessor border-top)
   (border-bottom :initarg :border-bottom :initform nil :accessor border-bottom)))

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

;;;; MACROS ===================================================================

(deftype border-thickness-type () '(thickness-type))

(defmacro with-borders ((left right top bottom) object &body body)
  (a:with-gensyms (instance)
    `(let ((,instance ,object))
       (let ((,left (border-left ,instance))
             (,right (border-right ,instance))
             (,top (border-top ,instance))
             (,bottom (border-bottom ,instance)))
         (declare (ignorable ,left ,right, top ,bottom))
         ,@body))))

