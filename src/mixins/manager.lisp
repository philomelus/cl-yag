(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; MANAGER-MIXIN-BASE =======================================================

(defclass manager-mixin-base ()
  ())

;;;; MANAGER-MIXIN ============================================================

(defclass manager-mixin (manager-mixin-base)
  ((manager :initarg :manager :initform nil :accessor manager)))

