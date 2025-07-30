(in-package #:clg)

;;;; vertical-grid ============================================================

(defclass vertical-grid (h-align-mixin
                         parent-mixin
                         v-align-mixin)
  ((content :initarg :content :initform () :type list :accessor vertical-grid-content)))

;;-------------------------------------------------------------------
;; Make sure our children know we are their parent

(defmethod initialize-instance :after ((obj vertical-grid) &key)
  (dolist (child (vertical-grid-content obj))
    (setf (parent child) obj)))

(defmethod (setf vertical-grid-content) :after (value (object vertical-grid))
  (dolist (child (vertical-grid-content object))
    (if (typep child 'parent-mixin)
        (setf (parent child) object)))
  (next-method))

(defmethod (setf parent) :after (val (obj vertical-grid))
  (dolist (child (vertical-grid-content obj))
    (if (typep child 'parent-mixin)
        (setf (parent child) obj)))
  (next-method))

;;-------------------------------------------------------------------

(defmethod area-height ((obj vertical-grid))
  (/ (area-height (coerce obj 'area-mixin)) (length (vertical-grid-content obj))))

(defmethod (setf content) :after (value (obj vertical-grid))
  (dolist (child (vertical-grid-content obj))
    (setf (parent child) obj)))

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

