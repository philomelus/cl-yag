(in-package #:cl-yag)

;;;; manager-mixin ============================================================

(defclass manager-mixin ()
  ((manager :initarg :manager :initform nil :accessor manager)))

(defmethod print-mixin ((o manager-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":manager ")
  (if (eq nil (font o))
      (format s "nil ")
      (format s "~a " (print-raw-object (manager o))))
  (pprint-newline :linear s)
  (my-next-method))

