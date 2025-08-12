(in-package #:cl-yag)

;;;; parent-mixin =============================================================

(defclass parent-mixin ()
  ((parent :initarg :parent :initform nil :type t :accessor parent)))

(defmethod print-mixin ((o parent-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":parent ")
  (let ((p (parent o)))
    (if (eq p nil)
        (format s "NIL ")
        (format s "~a " (print-raw-object p))))
  (pprint-newline :linear s)
  (my-next-method))

(defmethod (setf parent) :after (val (obj parent-mixin))
  ;; If we have content
  (if (typep obj 'content-mixin)
      (dolist (child (content obj))
        ;; If child has a parent slot
        (when (typep child 'parent-mixin)
          (setf (parent child) obj))))
  (my-next-method))

(defmethod owner ((obj parent-mixin))
  "Returns manager of current object or nil."
  (let ((par-obj (parent obj)))
    (loop
      (if (typep par-obj 'manager)
          (return-from owner par-obj))
      (if (eq nil par-obj)
          (return-from owner nil))
      (setf par-obj (parent par-obj)))))

