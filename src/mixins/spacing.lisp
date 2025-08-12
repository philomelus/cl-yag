(in-package #:cl-yag)

;;;; spacing-mixin ============================================================

(defclass spacing-mixin ()
  ((spacing-left :initarg :spacing-left :initform 0 :type integer :accessor spacing-left)
   (spacing-right :initarg :spacing-right :initform 0 :type integer :accessor spacing-right)
   (spacing-top :initarg :spacing-top :initform 0 :type integer :accessor spacing-top)
   (spacing-bottom :initarg :spacing-bottom :initform 0 :type integer :accessor spacing-bottom)))

(defmethod print-mixin ((o spacing-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":spacing-left ~d " (spacing-left o))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":spacing-right ~d " (spacing-right o))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":spacing-top ~d " (spacing-top o))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":spacing-bottom ~d " (spacing-bottom o))
  (pprint-newline :linear s)

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

