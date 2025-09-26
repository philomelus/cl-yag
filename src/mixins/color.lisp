(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; COLOR-MIXIN-BASE =========================================================

(defclass color-mixin-base ()
  ()
  (:documentation "Any class derived from this class or its descendents have the ability to use
color."))

;;;; COLOR-MIXIN ==============================================================

(defclass color-mixin (color-mixin-base)
  ((color :initarg :color :initform nil :type list :accessor color)))

;;;; COLOR-3D-MIXIN-BASE ======================================================

(defclass color-3d-mixin-base ()
  ()
  (:documentation "Any class using thie class as a base is letting other classes know it is
capable of using colors to draw something in a 3D style."))

;;;; COLOR-3D-MIXIN ===========================================================

(defclass color-3d-mixin (color-3d-mixin-base)
  ((normal :initarg :normal :initform nil :accessor normal-color)
   (dark :initarg :dark :initform nil :accessor dark-color)
   (light :initarg :light :initform nil :accessor light-color)
   (very-dark :initarg :very-dark :initform nil :accessor very-dark-color)
   (very-light :initarg :very-light :initform nil :accessor very-light-color)))


;;;; BACK-COLOR-MIXIN-BASE ====================================================

(defclass back-color-mixin-base ()
  ())

;;;; BACK-COLOR-MIXIN =========================================================

(defclass back-color-mixin (back-color-mixin-base)
  ((back-color :initarg :back-color :initform nil :accessor back-color)))

;;;; FORE-COLOR-MIXIN-BASE ====================================================

(defclass fore-color-mixin-base ()
  ())

;;;; FORE-COLOR-MIXIN =========================================================

(defclass fore-color-mixin (fore-color-mixin-base)
  ((fore-color :initarg :fore-color :initform nil :accessor fore-color)))

;;;; FRAME-COLOR-MIXIN-BASE ===================================================

(defclass frame-color-mixin-base ()
  ())

;;;; FRAME-COLOR-MIXIN ========================================================

(defclass frame-color-mixin (frame-color-mixin-base)
  ((frame-color :initarg :frame-color :initform nil :accessor frame-color)))

;;;; INTERIOR-COLOR-MIXIN-BASE ================================================

(defclass interior-color-mixin-base ()
  ())

;;;; INTERIOR-COLOR-MIXIN =====================================================

(defclass interior-color-mixin (interior-color-mixin-base)
  ((interior-color :initarg :interior-color :initform nil :accessor interior-color)))

;;;; TITLE-COLOR-MIXIN-BASE ===================================================

(defclass title-color-mixin-base ()
  ())

;;;; TITLE-COLOR-MIXIN ========================================================

(defclass title-color-mixin (title-color-mixin-base)
  ((title-color :initarg :title-color :initform nil :accessor title-color)))
