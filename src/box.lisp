(in-package #:cl-yag)

;;;; box ======================================================================

;;; theme-mixin -----------------------------------------------------

(defclass box-theme-mixin ()
  ())

(defclass box-3d-theme-mixin ()
  ())

(defclass box-flat-theme-mixin ()
  ())

;;; box-base --------------------------------------------------------

(defclass box (box-theme-mixin
               area-mixin
               parent-mixin
               title-mixin)
  ((thickness :initarg :thickness :initform 1 :accessor thickness :documentation "Number of pixels wide for the box frame.")))

