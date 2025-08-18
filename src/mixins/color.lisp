(in-package #:cl-yag)

;;;; color-mixin ==============================================================

(defclass color-mixin ()
  ((color :initarg :color :initform (al:map-rgb-f 1 1 1) :type list :accessor color)))

(defmethod print-mixin ((o color-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":color (~a) " (print-color (color o)))
  (pprint-newline :linear s)
  (my-next-method))

;;;; color-fore-back-mixin ====================================================

(defclass color-fore-back-mixin ()
  ((fore-color :initarg :fore-color :initform nil :type list :accessor fore-color)
   (back-color :initarg :back-color :initform nil :type list :accessor back-color)))

(defmethod print-mixin ((o color-fore-back-mixin) s)
  (pprint-indent :current 0 s)
  (if (eql nil (fore-color o))
      (format s ":fore-color nil")
      (format s ":fore-color (~a) " (print-color (fore-color o))))
  (pprint-newline :linear s)
  (pprint-indent :current 0 s)
  (if (eql nil (back-color o))
      (format s ":back-color nil ")
      (format s ":back-color (~a) " (print-color (back-color o))))
  (pprint-newline :linear s)
  (my-next-method))

