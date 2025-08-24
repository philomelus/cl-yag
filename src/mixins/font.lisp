(in-package #:cl-yag)

;;;; font-mixin ===============================================================

(defclass font-mixin ()
  ((font :initarg :font :initform nil :accessor font)))

(defmethod print-mixin ((o font-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":font ")
  (if (eq nil (font o))
      (format s "nil ")
      (format s "~a " (print-raw-object (font o) :name "AL:FONT ")))
  (pprint-newline :linear s)
  (my-next-method))

