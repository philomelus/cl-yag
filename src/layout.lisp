(in-package #:clg)

;;;; column-layout ============================================================

(defclass column-layout (container-mixin
                         parent-mixin)
  ())

;;-------------------------------------------------------------------
;; Make sure our children know we are their parent

(defmethod initialize-instance :after ((obj column-layout) &key)
  (dolist (child (content obj))
    (setf (parent child) obj)))

(defmethod (setf content) :after (value (object column-layout))
  (dolist (child (content object))
    (if (typep child 'parent-mixin)
        (setf (parent child) object)))
  (next-method))

(defmethod (setf parent) :after (val (obj column-layout))
  (dolist (child (content obj))
    (if (typep child 'parent-mixin)
        (setf (parent child) obj)))
  (next-method))

;;-------------------------------------------------------------------

(defmethod area-height ((obj column-layout))
  
  (let ((ah (/ (area-height (coerce obj 'area-mixin)) (length (content obj)))))
    (format *standard-output* "column-layout: height: ~d" ah)
    ah))

(defmethod (setf content) :after (value (obj column-layout))
  (dolist (child (content obj))
    (setf (parent child) obj)))

;; Paint all contained objects and set clean
(defmethod on-paint ((obj column-layout) &key)
  (dolist (c (content obj))
    (on-paint c))
  (next-method))

(defmethod ready ((obj column-layout) &key manager parent)
  (declare (ignore parent))
  (dolist (child (content obj))
    (ready child :manager manager :parent obj))
  (next-method))

