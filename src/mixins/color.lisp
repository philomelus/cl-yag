(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; COLOR-MIXIN ==============================================================

(defclass color-mixin ()
  ((color :initarg :color :initform nil :type list :accessor color)))

;;;; COLOR-3D-MIXIN ===========================================================

(defclass color-3d-mixin ()
  ((normal :initarg :normal :initform nil :accessor normal-color)
   (dark :initarg :dark :initform nil :accessor dark-color)
   (light :initarg :light :initform nil :accessor light-color)
   (very-dark :initarg :very-dark :initform nil :accessor very-dark-color)
   (very-light :initarg :very-light :initform nil :accessor very-light-color)))

;;;; BACK-COLOR-MIXIN =========================================================

(defclass back-color-mixin ()
  ((back-color :initarg :back-color :initform nil :accessor back-color)))

;;;; FORE-COLOR-MIXIN =========================================================

(defclass fore-color-mixin ()
  ((fore-color :initarg :fore-color :initform nil :accessor fore-color)))

;;;; FRAME-COLOR-MIXIN ========================================================

(defclass frame-color-mixin ()
  ((frame-color :initarg :frame-color :initform nil :accessor frame-color)))

;;;; INTERIOR-COLOR-MIXIN =====================================================

(defclass interior-color-mixin ()
  ((interior-color :initarg :interior-color :initform nil :accessor interior-color)))

;;;; TITLE-COLOR-MIXIN ========================================================

(defclass title-color-mixin ()
  ((title-color :initarg :title-color :initform nil :accessor title-color)))
