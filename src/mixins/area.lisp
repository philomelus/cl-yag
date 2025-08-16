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

(defun find-parent-area-mixin (object)
  (assert (typep object 'parent-mixin))
  (let ((p (parent object)))
    (loop
      (when (typep p 'manager)
        (error "found manager when looking for area-mixin"))
      
      (when (eql p nil)
        (error "no parent when looking for area-mixin"))
      
      ;; If it has area
      (if (typep p 'area-mixin)
          (return p))

      ;; Get next parent
      (assert (typep p 'area-mixin))
      (setf p (parent p))
      )))

;;; methods ---------------------------------------------------------

(defmethod bottom ((obj area-mixin) &key)
  (+ (top obj) (height obj)))

(defmethod height ((obj area-mixin))
  (let ((h (slot-value obj 'height)))
    ;; Calculate if requested
    (when (= h +LAYOUT-HEIGHT-CALC+)
      (calc-area obj (parent obj))
      (setf h (slot-value obj 'height)))
    h))

(defmethod left ((obj area-mixin))
  (let ((l (slot-value obj 'left)))
    (when (= l +LAYOUT-LEFT-CALC+)
      (calc-area obj (parent obj))
      (setf l (slot-value obj 'left)))
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
    (when (= top +LAYOUT-TOP-CALC+)
      (calc-area obj (parent obj))
      (setf top (slot-value obj 'top)))
    top))

(defmethod width ((obj area-mixin))
  (let ((w (slot-value obj 'width)))
    (when (= w +LAYOUT-WIDTH-CALC+)
        (calc-area obj (parent obj))
        (setf w (slot-value obj 'width)))
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

