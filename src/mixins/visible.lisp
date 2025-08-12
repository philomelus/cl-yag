(in-package #:cl-yag)

;;;; visible-mixin ============================================================

(defclass visible-mixin ()
  ((visible :initarg :visible :initform nil :type boolean :accessor visible)))

(defmethod print-mixin ((o visible-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":visible ~a " (if (visible o) "t" "nil"))
  (pprint-newline :linear s)
  (my-next-method))

