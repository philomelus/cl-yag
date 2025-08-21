(in-package #:cl-yag)

;;;; functions ================================================================

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
      (setf p (parent p)))))

;;;; area-mixin ===============================================================

(defvar *AREA-OPTS* '(:auto :auto-max :auto-min))

(defclass area-mixin ()
  ((left :initarg :left :initform :auto :accessor left)
   (top :initarg :top :initform :auto :accessor top)
   (width :initarg :width :initform :auto :accessor width)
   (height :initarg :height :initform :auto :accessor height)))

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

;;; methods ---------------------------------------------------------

(defmethod initialize-object :after ((object area-mixin) &key)
  (let ((opts '(:auto :auto-max :auto-min)))
    (with-slots (left top width height) object
      (macrolet ((validate (field)
                   `(unless (typep ,field 'number)
                      (if (typep ,field 'keyword)
                          (unless (member ,field opts)
                            (error "unrecognized keyword for ~a: ~a" (symbol-name ,field) ,field))
                          (error "unrecognized format for ~a: ~a" (symbol-name ,field) ,field)))))
        (validate 'left)
        (validate 'top)
        (validate 'width)
        (validate 'height)))))

(defmethod bottom ((obj area-mixin) &key)
  (+ (top obj) (height obj)))

(defmethod height ((obj area-mixin))
  (let ((h (slot-value obj 'height)))
    ;; Calculate if requested
    (when (member h *AREA-OPTS*)
      (calc-area obj (parent obj))
      (setf h (slot-value obj 'height)))
    h))

(defmethod left ((obj area-mixin))
  (let ((l (slot-value obj 'left)))
    (when (member l *AREA-OPTS*)
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
    (when (member top *AREA-OPTS*)
      (calc-area obj (parent obj))
      (setf top (slot-value obj 'top)))
    top))

(defmethod width ((obj area-mixin))
  (let ((w (slot-value obj 'width)))
    (when (member w *AREA-OPTS*)
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

