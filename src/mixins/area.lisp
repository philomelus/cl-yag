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

(defvar *AREA-HEIGHT-OPTS* '(:auto :auto-max :auto-min))
(defvar *AREA-LEFT-OPTS* '(:auto :left :center :right))
(defvar *AREA-TOP-OPTS* '(:auto :top :middle :bottom))
(defvar *AREA-WIDTH-OPTS* '(:auto :auto-max :auto-min))

(defclass area-mixin ()
  ((left :initarg :left :initform :auto :accessor left)
   (top :initarg :top :initform :auto :accessor top)
   (width :initarg :width :initform :auto :accessor width)
   (height :initarg :height :initform :auto :accessor height)

   (left-calc :initform nil)
   (top-calc :initform nil)
   (width-calc :initform nil)
   (height-calc :initform nil)))

(defmethod print-mixin ((o area-mixin) s)
  (pprint-field left o s :fmt "~d")
  (pprint-field top o s :fmt "~d")
  (pprint-field width o s :fmt "~d")
  (pprint-field height o s :fmt "~d")
  (my-next-method))

;;; methods ---------------------------------------------------------

(defmethod initialize-object :after ((object area-mixin) &key)
  (with-slots (left top width height) object
    ;; Validate fields are valid
    (macrolet ((validate (field opts)
                 `(unless (typep ,field 'number)
                    (if (typep ,field 'keyword)
                        (unless (member ,field ,opts)
                          (error "unrecognized keyword for ~a: ~a" (symbol-name ,field) ,field))
                        (error "unrecognized format for ~a: ~a" (symbol-name ,field) ,field)))))
      (validate 'height *AREA-HEIGHT-OPTS*)
      (validate 'left *AREA-LEFT-OPTS*)
      (validate 'top *AREA-TOP-OPTS*)
      (validate 'width *AREA-WIDTH-OPTS*))

    ;; Save original state of area
    (flet ((is-keyword (field)
             (if (typep field 'keyword)
                 field
                 nil)))
     (setf (slot-value object 'left-calc) (is-keyword left))
     (setf (slot-value object 'top-calc) (is-keyword top))
     (setf (slot-value object 'width-calc) (is-keyword width))
     (setf (slot-value object 'height-calc) (is-keyword height)))))

(defmethod bottom ((obj area-mixin))
  (+ (top obj) (height obj)))

(defmethod height ((obj area-mixin))
  (with-slots ((h height)) obj
    (when (typep h 'keyword)
      (v:debug :layout "[height] {~a} calculating" (print-raw-object obj))
      (assert (member h *AREA-HEIGHT-OPTS*))
      (calc-area obj (parent obj))
      (assert (typep (slot-value obj 'height) 'number))
      (setf h (slot-value obj 'height))
      (v:debug :layout "[height] {~a} result ~d" (print-raw-object obj) h))
    h))

(defmethod left ((obj area-mixin))
  (with-slots ((l left)) obj
    (when (typep l 'keyword)
      (v:debug :layout "[left] {~a} calculating" (print-raw-object obj))
      (assert (member l *AREA-LEFT-OPTS*))
      (calc-area obj (parent obj))
      (assert (typep (slot-value obj 'left) 'number))
      (setf l (slot-value obj 'left))
      (v:debug :layout "[left] {~a} result ~d" (print-raw-object obj) l))
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

(defmethod right ((obj area-mixin))
  (let ((l (left obj))
        (w (width obj)))
    (+ l w)))

(defmethod top ((obj area-mixin))
  (with-slots ((top top)) obj
    (when (typep top 'keyword)
      (v:debug :layout "[top] {~a} calculating" (print-raw-object obj))
      (assert (member top *AREA-TOP-OPTS*))
      (calc-area obj (parent obj))
      (assert (typep (slot-value obj 'top) 'number))
      (setf top (slot-value obj 'top))
      (v:debug :layout "[top] {~a} result ~d" (print-raw-object obj) top))
    top))

(defmethod width ((obj area-mixin))
  (with-slots ((w width)) obj
    (when (typep w 'keyword)
      (v:debug :layout "[width] {~a} calculating" (print-raw-object obj))
      (assert (member w *AREA-WIDTH-OPTS*))
      (calc-area obj (parent obj))
      (assert (typep (slot-value obj 'width) 'number))
      (setf w (slot-value obj 'width))
      (v:debug :layout "[width] {~a} result ~d" (print-raw-object obj) w))
    w))

(defmethod within (x y (obj area-mixin) &key)
  (let ((left (left obj))
        (top (top obj))
        (right (right obj))
        (bottom (bottom obj)))
    (when (typep obj 'padding-mixin)
      (decf left (padding-left obj))
      (incf right (padding-right obj))
      (decf top (padding-top obj))
      (incf bottom (padding-bottom obj)))
    (if (and (> x left) (< x right)
             (> y top) (< y bottom))
        t
        nil)))

(defmethod (setf area) (x y w h (obj area-mixin))
  (setf (left obj) x)
  (setf (top obj) y)
  (setf (width obj) w)
  (setf (height obj) h))

