(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; visible-mixin ============================================================

(defclass visible-mixin ()
  ((visible :initarg :visible :initform nil :type boolean :accessor visible)))

(defmethod print-mixin ((o visible-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-field visible o s)
  (my-next-method))

