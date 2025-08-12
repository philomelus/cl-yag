(in-package #:cl-yag)

;;;; location-mixin ===========================================================

(defclass location-mixin ()
  ((x :initarg :x :initform 0 :type integer :accessor location-x)
   (y :initarg :y :initform 0 :type integer :accessor location-y)))

(defmethod print-mixin ((o location-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":x ~d " (location-x o))
  (pprint-newline :linear s)
  
  (pprint-indent :current 0 s)
  (format s ":y ~d " (location-y o))
  (pprint-newline :linear s)
  
  (my-next-method))

(defmethod (setf location) (x y (object location-mixin))
  (setf (location-x object) x)
  (setf (location-y object) y)
  (my-next-method))

