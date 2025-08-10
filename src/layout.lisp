(in-package #:cl-yag)

;;;; layout ===================================================================

(defclass layout (container-mixin
                  parent-mixin)
  ())

(defmacro defcolumn-layout (content &rest rest &key &allow-other-keys)
  `(make-instance 'column-layout :content (list ,@content) ,@rest))

(defmethod print-object ((o layout) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "deflayout ")
    (print-mixin o s)))

(defmethod on-mouse-down (x y b (obj layout) &key)
  (dolist (child (content obj))
    (if (on-mouse-down x y b child)
        (return-from on-mouse-down t)))
  nil)

(defmethod on-mouse-move (x y dx dy (obj layout) &key)
  (dolist (child (content obj))
    (on-mouse-move x y dx dy child)))

(defmethod on-mouse-up (x y b (obj layout) &key)
  (dolist (child (content obj))
    (on-mouse-up x y b child)))

;;;; column-layout ============================================================

(defclass column-layout (layout
                         padding-mixin)
  ())

(defmethod print-object ((o column-layout) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "defcolumn-layout ")
    (print-mixin o s)))

;;; methods ---------------------------------------------------------

(defmethod on-paint ((obj column-layout) &key)
  (dolist (c (content obj))
    (on-paint c))
  (my-next-method))

(defmethod container-calc-child-height (child (container column-layout) &key)
  (let ((c (content container))
        (h (slot-value container 'height)))
    (let ((p (position child c))
          (l (length c)))
      (assert (not (eq p nil)))
      (let ((ch (truncate (/ h l))))
        (if (> (mod h l) 0)
            (incf ch 1))
        ch))))

(defmethod container-calc-child-left (child (container column-layout) &key)
  (slot-value container 'left))

(defmethod container-calc-child-top (child (container column-layout) &key)
  (let ((h (slot-value container 'top))
        (c (content container)))
    (let ((p (position child c)))
      (if (> p 0)
          (loop :for i :from 0 :to (1- p) :do
            (incf h (container-calc-child-height (nth i c) container))))
      h)))

(defmethod container-calc-child-width (child (container column-layout) &key)
  (slot-value container 'width))

