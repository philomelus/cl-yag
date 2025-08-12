(in-package #:cl-yag)

;;;; active-mixin =============================================================

(defclass active-mixin ()
  ((active :initarg :active :initform nil :type boolean :accessor active)))

(defmethod print-mixin ((o active-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":active ~a " (if (active o) "t" "nil"))
  (pprint-newline :linear s)
  (my-next-method))

