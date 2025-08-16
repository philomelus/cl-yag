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
      (calc-area obj child)))
  (my-next-method))

(defmethod (setf content) :after (value (obj content-mixin))
  ;; Let children know who their parent is
  (dolist (child (content obj))
    ;; If child has a pernt slot
    (when (typep child 'parent-mixin)
      (setf (parent child) obj))

    ;; If child is a container-mixin pass on area
    (when (typep child 'container-mixin)
      (calc-area obj child)))

  (my-next-method))

;; Determine left, top, width, and height from parent area
(defmethod calc-area (parent (object content-mixin) &key)
  (setf (left object) (1+ (left parent))
        (top object) (1+ (top parent))
        (height object) (- (height parent) 2)
        (width object) (- (width parent) 2))

  (assert (> (height object) 0))
  (assert (> (width object) 0))
  
  ;; Adjust for padding if needed
  (if (typep object 'padding-mixin)
      (progn
        (incf (left object) (padding-left object))
        (incf (top object) (padding-top object))
        (decf (height object) (+ (padding-top object) (padding-bottom object)))
        (decf (width object) (+ (padding-left object) (padding-right object)))))

  (assert (> (height object) 0))
  (assert (> (width object) 0))

  ;; Adjust for border if needed
  (if (typep parent 'border-mixin)
      (progn
        (let (bo w)
          (setf bo (border-left parent))
          (unless (eql nil bo)
            (setf w (width bo))
            (decf (width object) w)
            (incf (left object) w))
          (setf bo (border-right parent))
          (unless (eql nil bo)
            (setf w (width bo))
            (decf (width object) w))
          (setf bo (border-top parent))
          (unless (eql nil bo)
            (setf w (width bo))
            (decf (height object) w)
            (incf (top object) w))
          (setf bo (border-bottom parent))
          (unless (eql nil bo)
            (setf w (width bo))
            (decf (height object) w)))))

  (assert (> (height object) 0))
  (assert (> (width object) 0)))

