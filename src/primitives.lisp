(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; %point2 ==================================================================

(defclass %point2 ()
  ((h :initarg :h :initarg :left :initarg :right :initarg :x
      :type float :initform 0.0
      :accessor h :accessor left :accessor right :accessor x)
   (v :initarg :v :initarg :top :initarg :bottom :initarg :y
      :type float :initform 0.0
      :accessor v :accessor top :accessor bottom :accessor y)))

;;;; arc ======================================================================

;;;; circle ===================================================================

;;;; ellipse ==================================================================

;;;; line =====================================================================

(defparameter *LINE-THEME-DATA* `((line flat color)
                                  (line 3d style)
                                  (line 3d color)
                                  (line 3d dark-color)
                                  (line 3d light-color)
                                  (line 3d very-dark-color)
                                  (line 3d very-light-color)))

(defclass line ()
  ((start :initarg :start :type %point2 :accessor start)
   (end :initarg :end :type %point2 :accessor end)
   (width :initarg :width :initform 1.0 :accessor width))
  (:documentation "Draws a line from p1 to p2 of width pixels wide."))

(defmacro defline (&rest rest &key &allow-other-keys)
  `(make-instance 'line ,@rest))

(defmethod on-paint ((object line) &key)
  ;; TODO:
  (declare (ignorable object)))

;;;; pixel ====================================================================

;;;; polygon ==================================================================

;;;; rectangle ================================================================

(defparameter *RECTANGLE-THEME-DATA* `((rectangle nil filled)
                                       (rectangle nil fill-color)
                                       (rectangle flat frame-color)
                                       (rectangle 3d style)
                                       (rectangle 3d color)
                                       (rectangle 3d dark-color)
                                       (rectangle 3d light-color)
                                       (rectangle 3d very-dark-color)
                                       (rectangle 3d very-light-color)))

(defclass rectangle ()
  ((p1 :initarg :p1 :type %point2 :accessor p1)
   (p2 :initarg :p2 :type %point2 :accessor p2)))

(defmacro defrectangle (&rest rest &key &allow-other-keys)
  `(make-instance 'rectangle ,@rest))

(defmethod on-paint ((object rectangle) &key)
  ;; TODO:
  (declare (ignorable object)))

;;;; spline ===================================================================

;;;; triangle =================================================================

