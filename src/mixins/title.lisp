(in-package #:cl-yag)

;;;; title-mixin ==============================================================

(defclass title-mixin ()
  ((title :initarg :title :initform "" :type string :accessor title)))

(defmethod print-mixin ((o title-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":title ~s" (title o))
  (pprint-newline :linear s)
  (my-next-method))

