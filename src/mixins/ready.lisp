(in-package #:cl-yag)

(defclass ready-mixin ()
  ((ready :initform nil)))

(defmethod initialize-instance :after ((object ready-mixin) &key)
  (assert (not (slot-value object 'ready)))
  (setf (slot-value object 'ready) t)
  (my-next-method))

