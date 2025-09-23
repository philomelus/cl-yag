(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; spacing-mixin ============================================================

(defclass spacing-mixin ()
  ((spacing-left :type thickness-type :initarg :spacing-left :initform 0 :accessor spacing-left)
   (spacing-top :type thickness-type :initarg :spacing-top :initform 0 :accessor spacing-top)
   (spacing-right :type thickness-type :initarg :spacing-right :initform 0 :accessor spacing-right)
   (spacing-bottom :type thickness-type :initarg :spacing-bottom :initform 0 :accessor spacing-bottom)))

(defmethod (setf spacing) (value (object spacing-mixin))
  (with-slots (spacing-left spacing-right spacing-top spacing-bottom) object
    (setf spacing-left value)
    (setf spacing-right value)
    (setf spacing-top value)
    (setf spacing-bottom value))
  (layout-change object)
  (my-next-method))

(defmethod (setf spacing-h) (value (object spacing-mixin))
  (with-slots (spacing-left spacing-right) object
    (setf spacing-left value)
    (setf spacing-right value))
  (layout-change object)
  (my-next-method))

(defmethod (setf spacing-v) (value (object spacing-mixin))
  (with-slots (spacing-top spacing-bottom) object
    (setf spacing-top value)
    (setf spacing-bottom value))
  (layout-change object)
  (my-next-method))

(macrolet ((changed-value (object field)
             (a:with-gensyms (instance old)
               `(let ((,instance ,object))
                  (with-slots (,field) ,instance
                    (let ((,old ,field))
                      (call-next-method)
                      (unless (equal ,old ,field)
                        (layout-change ,instance))))))))

  (defmethod (setf spacing-left) :around (value (object spacing-mixin))
    (changed-value object spacing-left))
  (defmethod (setf spacing-right) :around (value (object spacing-mixin))
    (changed-value object spacing-right))
  (defmethod (setf spacing-top) :around (value (object spacing-mixin))
    (changed-value object spacing-top))
  (defmethod (setf spacing-bottom) :around (value (object spacing-mixin))
    (changed-value object spacing-bottom)))


