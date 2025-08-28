(in-package #:cl-yag)

;;;; box ======================================================================

;;; theme-mixin -----------------------------------------------------

(defclass box-theme-mixin (interior-color-mixin)
  ())

(defclass box-3d-theme-mixin (color-3d-mixin
                              style-3d-mixin)
  ())

(defclass box-flat-theme-mixin (frame-color-mixin)
  ())

;;; box ---- --------------------------------------------------------

(defclass box (box-theme-mixin
               area-mixin
               parent-mixin
               theme-mixin)
  ((thickness :initarg :thickness :initform 1 :accessor thickness :documentation "Number of pixels wide for the box frame.")
   (filled :initarg :filled :initform nil :accessor filled :documentation "When T, fill interior of box.")))

(defmacro defbox (&rest rest &key &allow-other-keys)
  `(make-instance 'box ,@rest))

;;; method ----------------------------------------------------------

;; #+safety
(defmethod (setf theme) (value (object box))
  (assert (member (type-of value) '(box-3d-theme-mixin 'box-flat-theme-mixin))))

(defmethod on-paint ((object box) &key)
  )
