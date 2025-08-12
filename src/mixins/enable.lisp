(in-package #:cl-yag)

;;;; enable-mixin =============================================================

(defclass enable-mixin ()
  ((enabled :initarg :enabled :initform nil :type boolean :accessor enabled)))

(defmethod print-mixin ((o enable-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":enabled ~a " (if (enabled o) "t" "nil"))
  (pprint-newline :linear s)
  (my-next-method))

