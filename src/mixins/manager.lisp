(in-package #:cl-yag)

;;;; manager-mixin ============================================================

(defclass manager-mixin ()
  ((manager :initarg :manager :initform nil :accessor manager)))

(defmethod print-mixin ((o manager-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-object-nil manager o s)
  (my-next-method))

