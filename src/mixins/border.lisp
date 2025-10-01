(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

;;;; BORDER-MIXIN-BASE ========================================================

(defclass border-mixin-base ()
  ()
  (:documentation "Any object derived from this or its descendents have the ability to draw a
border somewhere."))

;;;; BORDER-MIXIN =============================================================

(defclass border-mixin (border-mixin-base
                        theme-mixin)
  ((border-left :type (or null border) :initarg :border-left :initform nil :accessor border-left)
   (border-right :type (or null border) :initarg :border-right :initform nil :accessor border-right)
   (border-top :type (or null border) :initarg :border-top :initform nil :accessor border-top)
   (border-bottom :type (or null border) :initarg :border-bottom :initform nil :accessor border-bottom)))

;;; METHODS ---------------------------------------------------------

(defmethod (setf border) ((value border) (object border-mixin))
  (setf (border-h object) value)
  (setf (border-v object) value)
  (my-next-method))

(defmethod (setf border-h) ((value border) (object border-mixin))
  (setf (border-left object) value)
  (setf (border-right object) value)
  (my-next-method))

(defmethod (setf border-v) ((value border) (object border-mixin))
  (setf (border-top object) value)
  (setf (border-bottom object) value)
  (my-next-method))

(defmethod paint-border ((object border-mixin))
  (with-borders (bl br bt bb) object
    (let ((style (get-theme-style object))
          (blp (not (eql bl nil)))
          (brp (not (eql br nil)))
          (btp (not (eql bt nil)))
          (bbp (not (eql bb nil))))
      (when blp
        (paint-border-left bl style object :blend-top btp :blend-bottom bbp))
      (when btp
        (paint-border-top bt style object :blend-left blp :blend-right brp))
      (when brp
        (paint-border-right br style object :blend-top btp :blend-bottom bbp))
      (when bbp
        (paint-border-bottom bb style object :blend-left blp :blend-right brp)))))

