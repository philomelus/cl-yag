(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; CONTENT-MIXIN-BASE =======================================================

(defclass content-mixin-base ()
  ()
  (:documentation "Any class inheriting from this class it claiming that it contains other
widgets."))

;;;; CONTENT-MIXIN ============================================================

(defclass content-mixin ()
  ((content :initarg :content :initform (list) :type list :accessor content)))

(defmethod initialize-instance :after ((obj content-mixin) &key)
  ;; Let children know who their parent is
  (dolist (child (content obj))
    (let ((co (foro child)))
      ;; If child has a parent
      (when (typep co 'parent-mixin)
        (v:debug :parent "[initialize-instance] {content-mixin} setting child parent: ~a" (print-raw-object co))
        (setf (parent co) obj))))
  
  (my-next-method))

(defmethod (setf content) :after (value (obj content-mixin))
  ;; Let children know who their parent is
  (dolist (child (content obj))
    (let ((co (foro child)))
      ;; If child has a parent slot
      (when (typep co 'parent-mixin)
        (v:debug :parent "[setf content] {content-mixin} setting child parent: ~a" (print-raw-object co))
        (setf (parent co) obj))

      ;; If child has content
      (when (typep co 'content-mixin)
        (v:debug :layout "[setf content] {content-mixin} calc-area for ~a" co)
        (calc-area obj))))

  (my-next-method))

