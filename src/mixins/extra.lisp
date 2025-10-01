(in-package :cl-yag)

;;;; EXTRA-MIXIN-BASE =========================================================

(defclass extra-mixin-base ()
  ()
  (:documentation "Object with this as a base have some type of EXTRA content that can be
disabled. Primarily used by LAYOUT's to determine whether any particular
CELL/COLUMN/ROW should accept leftover area when layout is calculated."))

;;;; EXTRA-MIXIN ==============================================================

(defclass extra-mixin (extra-mixin-base)
  ((extra :type boolean :initarg :extra :initform t :accessor extra :documentation "When T, object accepts/wants extra, otherwise doesn't.")))
