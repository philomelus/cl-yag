(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; parent-mixin =============================================================

(defclass parent-mixin ()
  ((parent :initarg :parent :initform nil :type t :accessor parent)))

(defmethod print-mixin ((o parent-mixin) &optional s)
  (declare (ignore s))
  ;; (pprint-object-nil parent o s)
  (my-next-method))

;;; methods ---------------------------------------------------------

(defmethod (setf content) :after (value (object parent-mixin))
  (v:debug :parent "[setf content] {parent-mixin} updating children")
  
  (dolist (child value)
    (let ((co (foro child)))
      (with-accessors ((p parent)) co
        (unless (eql p object)
          (v:debug :parent "[setf content] {parent-mixin} setting child parent: ~a" (print-raw-object co))
          (setf p object)))))
  (my-next-method))

(defmethod (setf parent) :after (val (obj parent-mixin))
  (v:debug :parent "[setf parent] {parent-mixin} updating children")
  
  ;; If we have content
  (if (typep obj 'content-mixin)
      (dolist (child (content obj))
        (let ((co (foro child)))
          (when (typep co 'parent-mixin)
            (v:debug :parent "[setf parent] {parent-mixin} setting child parent: ~a" (print-raw-object co))
            (setf (parent co) obj)))))
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

