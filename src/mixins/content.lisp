(in-package #:cl-yag)

;;;; content-mixin ============================================================
;;;; Has contained objects

(defclass content-mixin ()
  ((content :initarg :content :initform (list) :type list :accessor content)))

(defmethod print-mixin ((o content-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":content (")
  (if (= (length (content o)) 0)
      (format s "() ")
      (let ((children (content o)))
        (dolist (child children)
          (pprint-indent :block 0 s)
          (pprint-logical-block (s nil)
            (pprint-indent :block 0 s)
            (format s "(~a)" child)
            (pprint-newline :fill s)))))
  (format s ")")
  (pprint-newline :linear s)
  (my-next-method))

(defmethod initialize-instance :after ((obj content-mixin) &key)
  ;; Let children know who their parent is
  (dolist (child (content obj))
    ;; If child has a parent slot
    (when (typep child 'parent-mixin)
      (setf (parent child) obj))

    ;; If child is a container-mixin pass on area
    (when (typep child 'container-mixin)
      (container-calc-area obj child)))
  (my-next-method))

(defmethod (setf content) :after (value (obj content-mixin))
  ;; Let children know who their parent is
  (dolist (child (content obj))
    ;; If child has a pernt slot
    (when (typep child 'parent-mixin)
      (setf (parent child) obj))

    ;; If child is a container-mixin pass on area
    (when (typep child 'container-mixin)
      (container-calc-area obj child)))

  (my-next-method))

