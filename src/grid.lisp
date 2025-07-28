(in-package #:clg)

;;;; vertical-grid ============================================================

(defclass vertical-grid (h-align-mixin
                         v-align-mixin)
  ((content :initarg :content :initform () :type list :accessor vertical-grid-content)))

(defun print-vertical-grid (obj stream)
  (format stream "vertical-grid:~&  h-align: ~a~&  v-align: ~a~&  content: ~a"
          (h-align obj) (v-align obj)
          (vertical-grid-content obj)))

(defmethod layout ((obj vertical-grid) (mgr manager) &key parent)
  (declare (ignore parent))
  (dolist (child (vertical-grid-content obj))
    (layout child mgr :parent obj))
  (next-method))

(defmethod on-mouse-clicked ((obj vertical-grid) x y b &key)
  ;; If click is within boundary of contained items,
  ;; somehow return that object as the result of the processing...
  (next-method))

(defmethod on-mouse-enter ((obj vertical-grid) x y &key)
  ;; Pass on to contained object if its within their controlled area
  (next-method))

(defmethod on-mouse-exit ((obj vertical-grid) x y &key)
  ;; Pass on to contained object if we were in their area and are not now
  (next-method))

;; Paint all contained objects and set clean
(defmethod on-paint ((obj vertical-grid) &key)
  (dolist (c (vertical-grid-content obj))
    (on-paint c))
  (next-method))

(defmethod ready ((obj vertical-grid) &key manager parent)
  (declare (ignore parent))
  (dolist (child (vertical-grid-content obj))
    (ready child :manager manager :parent obj))
  (next-method))

