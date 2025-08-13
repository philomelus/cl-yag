(in-package #:cl-yag)

;;;; macros ===================================================================

(defmacro my-next-method ()
  `(if (next-method-p) (call-next-method)))

