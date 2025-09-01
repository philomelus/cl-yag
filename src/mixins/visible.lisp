(in-package #:cl-yag)

;;;; visible-mixin ============================================================

(defclass visible-mixin ()
  ((visible :initarg :visible :initform nil :type boolean :accessor visible)))

(defmethod print-mixin ((o visible-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-field visible o s)
  (my-next-method))

