(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; color-mixin ==============================================================

(defclass color-mixin ()
  ((color :initarg :color :initform nil :type list :accessor color)))

(defmethod print-mixin ((o color-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-color-nil color o s)
  (my-next-method))

;;;; color-3d-mixin ===========================================================

(defclass color-3d-mixin ()
  ((normal :initarg :normal :initform nil :accessor normal-color)
   (dark :initarg :dark :initform nil :accessor dark-color)
   (light :initarg :light :initform nil :accessor light-color)
   (very-dark :initarg :very-dark :initform nil :accessor very-dark-color)
   (very-light :initarg :very-light :initform nil :accessor very-light-color)))

(defmethod print-mixin ((object color-3d-mixin) &optional stream)
  (declare (ignore stream))
  ;; (pprint-color-nil normal object stream)
  ;; (pprint-color-nil dark object stream)
  ;; (pprint-color-nil light object stream)
  ;; (pprint-color-nil very-dark object stream)
  ;; (pprint-color-nil very-light object stream)
  (my-next-method))

;;;; back-color-mixin =========================================================

(defclass back-color-mixin ()
  ((back-color :initarg :back-color :initform nil :accessor back-color)))

(defmethod print-mixin ((o back-color-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-color-nil back-color o s)
  (my-next-method))

;;;; fore-color-mixin =========================================================

(defclass fore-color-mixin ()
  ((fore-color :initarg :fore-color :initform nil :accessor fore-color)))

(defmethod print-mixin ((o fore-color-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-color-nil fore-color o s)
  (my-next-method))

;;;; back-fore-color-mixin ====================================================

(defclass back-fore-color-mixin (back-color-mixin
                                 fore-color-mixin)
  ())

;;;; frame-color-mixin ========================================================

(defclass frame-color-mixin ()
  ((frame-color :initarg :frame-color :initform nil :accessor frame-color)))

(defmethod print-mixin ((o frame-color-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-color-nil frame-color o s)
  (my-next-method))

;;;; interior-color-mixin =====================================================

(defclass interior-color-mixin ()
  ((interior-color :initarg :interior-color :initform nil :accessor interior-color)))

(defmethod print-mixin ((o interior-color-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-color-nil interior-color o s)
  (my-next-method))
