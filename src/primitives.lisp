(in-package #:cl-yag)

;;;; %point2 ==================================================================

(defclass %point2 ()
  ((h :initarg :h :initarg :left :initarg :right :initarg :x
      :type float :initform 0.0
      :accessor h :accessor left :accessor right :accessor x)
   (v :initarg :v :initarg :top :initarg :bottom :initarg :y
      :type float :initform 0.0
      :accessor h :accessor top :accessor bottom :accessor y)))

;;;; arc ======================================================================

;;;; circle ===================================================================

;;;; ellipse ==================================================================

;;;; line =====================================================================

(defclass line-theme-mixin ()
  ())

(defclass line-theme-mixin-2d (line-theme-mixin
                               color-mixin)
  ())

(defclass line-theme-mixin-3d (line-theme-mixin
                               color-3d-mixin
                               style-3d-mixin)
  ())

(defclass line (line-theme-mixin)
  ((start :initarg :start :type %point2 :accessor start)
   (end :initarg :end :type %point2 :accessor end)
   (width :initarg :width :initform 1.0 :accessor width))
  (:documentation "Draws a line from p1 to p2 of width pixels wide."))

(defmethod on-paint ((object line) &key)
  (let ((theme (find-theme object)))
    (typecase theme
      (line-theme-mixin-2d
       (error "not implemented"))
      (line-theme-mixin-3d
       (error "not implemented"))
      (t
       (error "unknown line theme, got: ~a" (type-of theme))))))

;;;; pixel ====================================================================

;;;; polygon ==================================================================

;;;; rectangle ================================================================

(defclass rectangle-theme-mixin ()
  ((filled :initarg :filled :initform nil :accessor filled)
   (fill-color :initarg :fill-color :initform nil :accessor fill-color)))

(defclass rectangle-theme-mixin-2d (rectangle-theme-mixin
                                    color-mixin)
  ())

(defclass rectangle-theme-mixin-3d (rectangle-theme-mixin
                                    color-3d-mixin
                                    style-3d-mixin)
  ())

(defclass rectangle (rectangle-theme-mixin)
  ((p1 :initarg :p1 :type %point2 :accessor p1)
   (p2 :initarg :p2 :type %point2 :accessor p2)))

(defmethod on-paint ((object rectangle) &key)
  (let ((theme (find-theme object)))
    (typecase theme
      (rectangle-theme-mixin-2d
       (error "not implemented"))
      (rectangle-theme-mixin-3d
       (error "not implemented"))
      (t
       (error "unknown rectangle theme, got: ~a" (type-of theme))))))

;;;; spline ===================================================================

;;;; triangle =================================================================

