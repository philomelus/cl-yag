(in-package #:cl-yag)

;;;; area-mixin ===============================================================

(defclass area-mixin ()
  ((left :initarg :left :initform +LAYOUT-LEFT-CALC+ :type integer :accessor left)
   (top :initarg :top :initform +LAYOUT-TOP-CALC+ :type integer :accessor top)
   (width :initarg :width :initform +LAYOUT-HEIGHT-CALC+ :type integer :accessor width)
   (height :initarg :height :initform +LAYOUT-WIDTH-CALC+ :type integer :accessor height)))

;;; functions -------------------------------------------------------

(defmethod print-mixin ((o area-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":left ~d " (left o))
  (pprint-newline :linear s)
  (pprint-indent :current 0 s)
  (format s ":top ~d " (top o))
  (pprint-newline :linear s)
  (pprint-indent :current 0 s)
  (format s ":width ~d " (width o))
  (pprint-newline :linear s)
  (pprint-indent :current 0 s)
  (format s ":height ~d " (height o))
  (pprint-newline :linear s)
  (my-next-method))

(defun find-parent-area-mixin (obj)
  "Returns first parent that is a subclass of area-mixin, or nil."
  (if (not (typep obj 'parent-mixin))
      (return-from find-parent-area-mixin nil))  
  (let ((par-obj (parent obj)))
    (loop
      (if (eq nil par-obj)
          (return-from find-parent-area-mixin nil))
      (if (typep par-obj 'area-mixin)
          (return-from find-parent-area-mixin par-obj))
      (if (not (typep par-obj 'parent-mixin))
          (return-from find-parent-area-mixin nil))
      (setf par-obj (parent par-obj)))))

;;; methods ---------------------------------------------------------

(defmethod bottom ((obj area-mixin) &key)
  (+ (top obj) (height obj)))

(defmethod height ((obj area-mixin))
  (let ((h (slot-value obj 'height)))
    (if (= h +LAYOUT-HEIGHT-CALC+)
        (setf h 0))
    (when (typep obj 'parent-mixin)
      (let ((p (parent obj)))
       (when (typep p 'container-mixin)
         (setf h (container-calc-child-height obj p)))))
    h))

(defmethod left ((obj area-mixin))
  (let ((l (slot-value obj 'left)))
    (if (= l +LAYOUT-LEFT-CALC+)
        (setf l 0))
    (when (typep obj 'parent-mixin)
      (let ((p (parent obj)))
        (when (typep p 'container-mixin)
         (setf l (container-calc-child-left obj p)))))
    l))

(defmethod on-mouse-move (x y dx dy (obj area-mixin) &key)
  ;; If we are a container, pass on to all children
  (if (typep obj 'container-mixin)
      (dolist (child (content obj))
        (on-mouse-move x y dx dy child)))
  (my-next-method))

(defmethod on-mouse-down (x y b (obj area-mixin) &key)
  (if (typep obj 'container-mixin)
      (dolist (child (content obj))
        (if (on-mouse-down x y b child)
            (return-from on-mouse-down t))))
  (my-next-method))

(defmethod on-mouse-up (x y b (obj area-mixin) &key)
  (if (typep obj 'container-mixin)
      (dolist (child (content obj))
        (on-mouse-up x y b child)))
  (my-next-method))

(defmethod right ((obj area-mixin) &key)
  (+ (left obj) (width obj)))

(defmethod top ((obj area-mixin))
  (let ((top (slot-value obj 'top)))
    (if (= top +LAYOUT-TOP-CALC+)
        (setf top 0))
    (when (typep obj 'parent-mixin)
      (let ((p (parent obj)))
        (when (typep p 'container-mixin)
          (setf top (container-calc-child-top obj p)))))
    top))

(defmethod width ((obj area-mixin))
  (let ((w (slot-value obj 'width)))
    (if (= w +LAYOUT-WIDTH-CALC+)
        (setf w 0))
    (when (typep obj 'parent-mixin)
      (let ((p (parent obj)))
        (when (typep p 'container-mixin)
          (setf w (container-calc-child-width obj p)))))
    w))

(defmethod within (x y (obj area-mixin) &key)
  (let ((left (left obj))
        (top (top obj))
        (right (right obj))
        (bottom (bottom obj)))
    (if (and (> x left) (< x right)
             (> y top) (< y bottom))
        t
        nil)))

(defmethod (setf area) (x y w h (obj area-mixin))
  (setf (left obj) x)
  (setf (top obj) y)
  (setf (width obj) w)
  (setf (height obj) h))

