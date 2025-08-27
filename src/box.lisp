(in-package #:cl-yag)

;;;; box ======================================================================

;;; theme-mixin -----------------------------------------------------

(defclass box-theme-mixin ()
  ())

;;; box-base --------------------------------------------------------

(defclass box-base (box-theme-mixin
                    area-mixin
                    parent-mixin
                    title-mixin)
  ((width :initarg :width :initform 1 :accessor width :documentation "Width in pixels of box.")))

;;;; box-flat =================================================================

;;; theme-mixin -----------------------------------------------------

(defclass box-flat-theme-mixin (color-mixin)
  ())

;;; box-flat --------------------------------------------------------

(defclass box-flat (box-base
                    box-flat-theme-mixin)
  ())

;;;; box-3d ===================================================================

;;; theme-mixin -----------------------------------------------------

(defclass box-3d-theme-mixin (color-3d-mixin)
  ())

;;; box-3d ----------------------------------------------------------

(defclass box-3d (box-base
                  box-3d-theme-mixin
                  style-3d-mixin)
  ())

