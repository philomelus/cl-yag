(in-package #:cl-yag)

;;;; content-mixin ============================================================
;;;; Has contained objects

(defclass content-mixin ()
  ((content :initarg :content :initform (list) :type list :accessor content)))

(defmethod print-mixin ((o content-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-object-nil content o s)
  (my-next-method))

(defmethod initialize-instance :after ((obj content-mixin) &key)
  ;; Let children know who their parent is
  (dolist (child (content obj))
    ;; If child has a parent slot
    (when (typep child 'parent-mixin)
      (setf (parent child) obj)))

  (my-next-method))

(defmethod (setf content) :after (value (obj content-mixin))
  ;; Let children know who their parent is
  (dolist (child (content obj))
    ;; If child has a parent slot
    (when (typep child 'parent-mixin)
      (setf (parent child) obj))

    ;; If child has content
    (when (typep child 'content-mixin)
      (calc-area child obj)))

  (my-next-method))


