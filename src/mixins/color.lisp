(in-package #:cl-yag)

;;;; color-mixin ==============================================================

(defclass color-mixin ()
  ((color :initarg :color :initform nil :type list :accessor color)))

(defmethod print-mixin ((o color-mixin) s)
  (pprint-color-nil color o s)
  (my-next-method))

;;;; color-3d-mixin ===========================================================

(defclass color-3d-mixin ()
  ((normal :initarg :normal :initform nil :accessor normal-color)
   (dark :initarg :dark :initform nil :accessor dark-color)
   (light :initarg :light :initform nil :accessor light-color)
   (very-dark :initarg :very-dark :initform nil :accessor very-dark-color)
   (very-light :initarg :very-light :initform nil :accessor very-light-color)))

(defmethod print-mixin ((object color-3d-mixin) stream)
  (pprint-color-nil normal object stream)
  (pprint-color-nil dark object stream)
  (pprint-color-nil light object stream)
  (pprint-color-nil very-dark object stream)
  (pprint-color-nil very-light object stream)
  (my-next-method))

;;;; color-fore-back-mixin ====================================================

(defclass color-fore-back-mixin ()
  ((fore-color :initarg :fore-color :initform nil :type list :accessor fore-color)
   (back-color :initarg :back-color :initform nil :type list :accessor back-color)))

(defmethod print-mixin ((o color-fore-back-mixin) s)
  (pprint-color-nil fore-color o s)
  (pprint-color-nil back-color o s)
  (my-next-method))

;;;; frame-color-mixin ========================================================

(defclass frame-color-mixin ()
  ((frame-color :initarg :frame-color :initform nil :accessor frame-color)))

(defmethod print-mixin ((o frame-color-mixin) s)
  (pprint-color-nil frame-color o s)
  (my-next-method))

;;;; interior-color-mixin =====================================================

(defclass interior-color-mixin ()
  ((interior-color :initarg :interior-color :initform nil :accessor interior-color)))

(defmethod print-mixin ((o interior-color-mixin) s)
  (pprint-color-nil interior-color o s)
  (my-next-method))
