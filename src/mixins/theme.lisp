(in-package #:cl-yag)

;;;; theme-mixin ==============================================================

(defclass theme-mixin ()
  ((theme :initarg :theme :initform nil :accessor theme)))

(defmethod print-mixin ((o theme-mixin) s)
  (pprint-indent :current 0 s)
  (cond 
    ((eq nil (theme o)) (format s ":theme nil "))
    (t (format s ":theme (~a) " (theme o))))
  (pprint-newline :linear s)
  (my-next-method))

;; Some methods in src/theme.lisp because macro and global reference
;; each other
