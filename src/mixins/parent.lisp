(in-package #:cl-yag)

;;;; parent-mixin =============================================================

(defclass parent-mixin ()
  ((parent :initarg :parent :initform nil :type t :accessor parent)))

(defmethod print-mixin ((o parent-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-object-nil parent o s)
  (my-next-method))

;;; methods ---------------------------------------------------------

(defmethod (setf content) :after (value (object parent-mixin))
  (v:info :parent "[setf content] {parent-mixin} updating children")
  (dolist (child value)
    (when (typep child 'parent-mixin)
      (with-accessors ((p parent)) child
        (unless (eql p object)
          (v:info :parent "[setf content] {parent-mixin} setting child parent: ~a" (print-raw-object child))
          (setf (parent child) object)))))
  (my-next-method))

(defmethod (setf parent) :after (val (obj parent-mixin))
  (v:info :parent "[setf parent] {parent-mixin} updating children")
  
  ;; If we have content
  (if (typep obj 'content-mixin)
      (dolist (child (content obj))
        ;; If child has a parent slot
        (when (typep child 'parent-mixin)
          (v:info :parent "[setf parent] {parent-mixin} setting child parent: ~a" (print-raw-object child))
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

