(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; font-mixin ===============================================================

(defclass font-mixin ()
  ((font :initarg :font :initform nil :accessor font)))

(defmethod print-mixin ((o font-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-indent :current 0 s)
  ;; (format s ":font ")
  ;; (if (eq nil (font o))
  ;;     (format s "nil ")
  ;;     (format s "~a " (print-raw-object (font o) :name "AL:FONT ")))
  ;; (pprint-newline :linear s)
  (my-next-method))

