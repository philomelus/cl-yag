(in-package #:cl-yag)

;;;; %rect =====================================================================

(defclass %rect ()
  ((left :initarg :left :type integer :initform 0 :accessor left)
   (top :initarg :top :type integer :initform 0 :accessor top)
   (width :initarg :width :type integer :initform 0 :accessor width)
   (height :initarg :height :type integer :initform 0 :accessor height)))

(defmethod bottom ((object %rect))
  (+ (slot-value object 'top) (slot-value object 'height)))

(defmethod height ((object %rect))
  (slot-value object 'height))

(defmethod left ((object %rect))
  (slot-value object 'left))

(defmethod right ((object %rect))
  (+ (slot-value object 'left) (slot-value object 'width)))

(defmethod top ((object %rect))
  (slot-value object 'top))

(defmethod width ((object %rect))
  (slot-value object 'width))

