(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(deftype coordinate () '(or integer float))

;;;; area-mixin ===============================================================

;; TODO:
;; minimize maximize
;; none (means default or auto or both)

(defvar +AREA-HEIGHT-OPTS+ '(:auto :auto-max :auto-min))
(defvar +AREA-LEFT-OPTS+ '(:auto :left :center :right))
(defvar +AREA-TOP-OPTS+ '(:auto :top :middle :bottom))
(defvar +AREA-WIDTH-OPTS+ '(:auto :auto-max :auto-min))

;; TODO: (deftype area-height () '(or integer float (member :auto :auto-max :auto-min)))

(defclass area-mixin ()
  ((left :initarg :left :initform :auto :accessor left)
   (top :initarg :top :initform :auto :accessor top)
   (width :initarg :width :initform :auto :accessor width)
   (height :initarg :height :initform :auto :accessor height)

   (left-calc :initform nil)
   (top-calc :initform nil)
   (width-calc :initform nil)
   (height-calc :initform nil)))

;;; methods ---------------------------------------------------------

(defmethod initialize-instance :after ((object area-mixin) &key)
  (with-slots (left top width height) object
    ;; Validate fields are valid
    (macrolet ((validate (field opts)
                 `(unless (or (typep ,field 'number)
                              (constantp ,field))
                    (if (typep ,field 'keyword)
                        (unless (member ,field ,opts)
                          (error "unrecognized keyword for ~a: ~a" (symbol-name ,field) ,field))
                        (error "unrecognized format for ~a: ~a" (symbol-name ,field) ,field)))))
      (validate height +AREA-HEIGHT-OPTS+)
      (validate left +AREA-LEFT-OPTS+)
      (validate top +AREA-TOP-OPTS+)
      (validate width +AREA-WIDTH-OPTS+))

    ;; Save original state of area
    (flet ((is-keyword (field)
             (if (typep field 'keyword)
                 field
                 nil)))
      (setf (slot-value object 'left-calc) (is-keyword left))
      (setf (slot-value object 'top-calc) (is-keyword top))
      (setf (slot-value object 'width-calc) (is-keyword width))
      (setf (slot-value object 'height-calc) (is-keyword height))
      ))
  (my-next-method))

(defmethod bottom ((obj area-mixin))
  (+ (top obj) (height obj)))

(defmethod height ((obj area-mixin))
  (with-slots ((h height)) obj
    (when (typep h 'keyword)
      (v:debug :layout "[height] {area-mixin} calculating ~a" (print-raw-object obj))
      (assert (member h +AREA-HEIGHT-OPTS+))
      (calc-area obj (parent obj))
      (assert (typep (slot-value obj 'height) 'number))
      (setf h (slot-value obj 'height))
      (v:debug :layout "[height] {area-mixin} result:~d ~a" h (print-raw-object obj)))
    h))

(defmethod left ((obj area-mixin))
  (with-slots ((l left)) obj
    (when (typep l 'keyword)
      (v:debug :layout "[left] {area-mixin} calculating ~a" (print-raw-object obj))
      (assert (member l +AREA-LEFT-OPTS+))
      (calc-area obj (parent obj))
      (assert (typep (slot-value obj 'left) 'number))
      (setf l (slot-value obj 'left))
      (v:debug :layout "[left] {area-mixin} result:~d ~a" l (print-raw-object obj)))
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
      (v:debug :layout "[top] {area-mixin} calculating ~a" (print-raw-object obj))
      (assert (member top +AREA-TOP-OPTS+))
      (calc-area obj (parent obj))
      (assert (typep (slot-value obj 'top) 'number))
      (setf top (slot-value obj 'top))
      (v:debug :layout "[top] {area-mixin} result:~d ~a" top (print-raw-object obj)))
    top))

(defmethod width ((obj area-mixin))
  (with-slots ((w width)) obj
    (when (typep w 'keyword)
      (v:debug :layout "[width] {area-mixin} calculating ~a" (print-raw-object obj))
      (assert (member w +AREA-WIDTH-OPTS+))
      (calc-area obj (parent obj))
      (assert (typep (slot-value obj 'width) 'number))
      (setf w (slot-value obj 'width))
      (v:debug :layout "[width] {area-mixin} result:~d ~a" w (print-raw-object obj)))
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

;;;; area-cache-mixin =========================================================

;; (defclass area-cache-mixin ()
;;   ((cache-valid :initform nil :type boolean :documentation "T when rest of area cache slots are valid, nil otherwise")
;;    (cache-left :initform nil :type (or coordinate null) :documentation "Calculated left of interior area.")
;;    (cache-top :initform nil :type (or coordinate null) :documentation "Calculated top of interior are.")
;;    (cache-right :initform nil :type (or coordinate null) :documentation "Calculated right of interior area.")
;;    (cache-bottom :initform nil :type (or coordinate null) :documentation "Calculated bottom of interior area.")))

;;;; area-cache-border-mixin ==================================================

;; (defclass area-cache-border-mixin ()
;;   ((cache-border-valid :initform nil :type boolean :documentation "T when rest of border cache slots are valid, nil otherwise")
;;    (cache-border-left :initform nil :type (or coordinate null) :documentation "Calculated left,top,right,bottom of left border.")
;;    (cache-border-top :initform nil :type (or coordinate null) :documentation "Calculated left,top,right,bottom of top border.")
;;    (cache-border-right :initform nil :type (or coordinate null) :documentation "Calculated left,top,right,bottom of right border.")
;;    (cache-border-bottom :initform nil :type (or coordinate null) :documentation "Calculated left,top,right,bottom of bottom border.")))

;;;; area-cache-padding-mixin =================================================

;; (defclass area-cache-padding-mixin ()
;;   (cache-padding-valid :initform nil :type boolean :documentation "T when other padding slots are valid, nil otherwise.")
;;   (cache-padding-lb :initform nil :type (or coordinate null) :documentation "Calculated left,top,right,bottom of lower left padding.")
;;   (cache-padding-lt :initform nil :type (or coordinate null) :documentation "Calculated left,top,right,bottom of upper left padding.")
;;   (cache-padding-rb :initform nil :type (or coordinate null) :documentation "Calculated left,top,right,bottom of lower right padding.")
;;   (cache-padding-rt :initform nil :type (or coordinate null) :documentation "Calculated left,top,right,bottom of upper right padding."))

;;;; area-cache-border-spacing-mixin ==========================================

;; (defclass area-cache-spacing-mixin ()
;;   ((cache-spacing-valid :initform nil :type boolean :documentation "T when rest of spacing cache slots are valid, nil otherwise.")
;;    (cache-spacing-lb :initform nil :type (or coordinate null) :documentation "Calculated left,top,right,bottom of lower left exterior.")
;;    (cache-spacing-lt :initform nil :type (or coordinate null) :documentation "Calculated left,top,right,bottom of upper left of exterior.")
;;    (cache-spacing-rb :initform nil :type (or coordinate null) :documentation "Calculated left,top,right,bottom of lower right exterior.")
;;    (cache-spacing-rt :initform nil :type (or coordinate null) :documentation "Calculated left,top,right,bottom of upper right of exterior.")))

;;;; functions ================================================================

(defun area (object)
  "Return left, top, width, and height as mutliple values."
  
  (assert (typep object 'area-mixin))
  (values (left object) (top object) (width object) (height object)))

(defun area-rb (object)
  "Return left, top, right, and bottom as multiple values."
  
  (assert (typep object 'area-mixin))
  (values (left object) (top object) (right object) (bottom object)))

(defun find-parent-area (object)
  "Find first parent of OBJECT derived from parent-mixin. Generates error on
failure."
  
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

(defun reset-area (object)
  "Return layout of object to original state, allowing it to be
relayed-out."

  (macrolet ((update-value (dest src)
               `(when (slot-value object ,src)
                  (setf (slot-value object ,dest) (slot-value object ,src)))))
    (update-value 'width 'width-calc)
    (update-value 'height 'height-calc)
    (update-value 'left 'left-calc)
    (update-value 'top 'top-calc)))

