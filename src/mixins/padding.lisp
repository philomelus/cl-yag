(in-package #:cl-yag)

;;;; padding-mixin ============================================================

(defclass padding-mixin ()
  ((padding-left :initarg :padding-left :initform 0 :type integer :accessor padding-left)
   (padding-right :initarg :padding-right :initform 0 :type integer :accessor padding-right)
   (padding-top :initarg :padding-top :initform 0 :type integer :accessor padding-top)
   (padding-bottom :initarg :padding-bottom :initform 0 :type integer :accessor padding-bottom)))

(defmethod print-mixin ((o padding-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":padding-left ~d " (padding-left o))
  (pprint-newline :linear s)
  
  (pprint-indent :current 0 s)
  (format s ":padding-right ~d " (padding-right o))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":padding-top ~d " (padding-top o))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":padding-bottom ~d " (padding-bottom o))
  (pprint-newline :linear s)

  (my-next-method))

(defmethod (setf padding) (value (object padding-mixin))
  (setf (padding-h object) value)
  (setf (padding-v object) value)
  (my-next-method))

(defmethod (setf padding-h) (value (object padding-mixin))
  (setf (padding-left object) value)
  (setf (padding-right object) value)
  (my-next-method))

(defmethod (setf padding-v) (value (object padding-mixin))
  (setf (padding-top object) value)
  (setf (padding-bottom object) value)
  (my-next-method))

