(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; %rect =====================================================================

(defclass %rect ()
  ((left :initarg :left :initform 0.0 :accessor left)
   (top :initarg :top :initform 0.0 :accessor top)
   (width :initarg :width :initform 0.0 :accessor width)
   (height :initarg :height :initform 0.0 :accessor height)))

(defmethod print-object ((object %rect) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "%RECT ~d ~d ~d ~d" (left object) (top object) (width object) (height object))))

;;; methods ---------------------------------------------------------

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

