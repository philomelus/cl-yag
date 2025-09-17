(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; color-mixin ==============================================================

(defclass color-mixin ()
  ((color :initarg :color :initform nil :type list :accessor color)))

;;;; color-3d-mixin ===========================================================

(defclass color-3d-mixin ()
  ((normal :initarg :normal :initform nil :accessor normal-color)
   (dark :initarg :dark :initform nil :accessor dark-color)
   (light :initarg :light :initform nil :accessor light-color)
   (very-dark :initarg :very-dark :initform nil :accessor very-dark-color)
   (very-light :initarg :very-light :initform nil :accessor very-light-color)))

;;;; back-color-mixin =========================================================

(defclass back-color-mixin ()
  ((back-color :initarg :back-color :initform nil :accessor back-color)))

;;;; fore-color-mixin =========================================================

(defclass fore-color-mixin ()
  ((fore-color :initarg :fore-color :initform nil :accessor fore-color)))

;;;; back-fore-color-mixin ====================================================

(defclass back-fore-color-mixin (back-color-mixin
                                 fore-color-mixin)
  ())

;;;; frame-color-mixin ========================================================

(defclass frame-color-mixin ()
  ((frame-color :initarg :frame-color :initform nil :accessor frame-color)))

;;;; interior-color-mixin =====================================================

(defclass interior-color-mixin ()
  ((interior-color :initarg :interior-color :initform nil :accessor interior-color)))

