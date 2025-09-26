(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; AREA-MIXIN-BASE ==========================================================

(defclass area-mixin-base ()
  ((left :type layout-left-type :initarg :left :initform :auto :accessor left :documentation "Coordinate of left side.")
   (top :type layout-top-type :initarg :top :initform :auto :accessor top :documentation "Coordinate of top side.")
   (width :type layout-width-type :initarg :width :initform :auto :accessor width :documentation "Width of object.")
   (height :type layout-height-type :initarg :height :initform :auto :accessor height :documentation "Height of object."))
  (:documentation "Mixin that lets other objects know the derived object has display area."))

;;;; AREA-MIXIN ===============================================================

;; TODO:
;; minimize maximize
;; none (means default or auto or both)

(deftype area-calc-type () '(or integer float ratio (member :auto :auto-max :auto-min)))

(defclass area-mixin (area-mixin-base)
  ((left-calc :initform nil)
   (top-calc :initform nil)
   (width-calc :initform nil)
   (height-calc :initform nil)))

;;; methods ---------------------------------------------------------

(defmethod bottom ((object area-mixin))
  (+ (top object) (height object)))

(defmethod height ((object area-mixin))
  (with-local-slots ((local-height height)) object
    (when (keywordp local-height)
      (assert (typep local-height 'layout-height-type))
      (calc-area (find-parent-layout object))
      (assert (typep (slot-value object 'height) 'coordinate))
      (setf local-height (slot-value object 'height)))
    local-height))

(defmethod initialize-instance :after ((object area-mixin) &key)
  (with-slots (left top width height) object
    ;; Validate fields are valid
    (macrolet ((validate (field opt)
                 `(unless (or (typep ,field 'number)
                              (constantp ,field))
                    (if (keywordp ,field)
                        (unless (typep ,field ,opt)
                          (error "unrecognized keyword for ~a: ~a" (symbol-name ,field) ,field))
                        (error "unrecognized format for ~a: ~a" (symbol-name ,field) ,field)))))
      (validate height 'layout-height-type)
      (validate left 'layout-left-type)
      (validate top 'layout-top-type)
      (validate width 'layout-width-type))

    ;; Save original state of area
    (flet ((is-keyword (field)
             (if (keywordp field)
                 field
                 nil)))
      (setf (slot-value object 'left-calc) (is-keyword left))
      (setf (slot-value object 'top-calc) (is-keyword top))
      (setf (slot-value object 'width-calc) (is-keyword width))
      (setf (slot-value object 'height-calc) (is-keyword height))
      ))
  (my-next-method))

(defmethod left ((object area-mixin))
  (with-local-slots ((local-left left)) object
    (when (keywordp local-left)
      (assert (typep local-left 'layout-left-type))
      (calc-area (find-parent-layout object))
      (assert (typep (slot-value object 'left) 'coordinate))
      (setf local-left (slot-value object 'left)))
    local-left))

(defmethod on-mouse-move (x y dx dy (obj area-mixin) &key)
  ;; If we are a container, pass on to all children
  (if (typep obj 'content-mixin-base)
      (dolist (child (content obj))
        (unless (eql child nil)
         (on-mouse-move x y dx dy child))))
  (my-next-method))

(defmethod on-mouse-down (x y b (obj area-mixin) &key)
  (if (typep obj 'content-mixin-base)
      (dolist (child (content obj))
        (unless (eql child nil)
            (if (on-mouse-down x y b child)
             (return-from on-mouse-down t)))))
  (my-next-method))

(defmethod on-mouse-up (x y b (obj area-mixin) &key)
  (if (typep obj 'content-mixin-base)
      (dolist (child (content obj))
        (on-mouse-up x y b child)))
  (my-next-method))

(defmethod right ((object area-mixin))
  (+ (left object) (width object)))

(defmethod top ((object area-mixin))
  (with-slots ((local-top top)) object
    (when (keywordp local-top)
      (assert (typep local-top 'layout-top-type))
      (calc-area (find-parent-layout object))
      (assert (typep (slot-value object 'top) 'coordinate))
      (setf local-top (slot-value object 'top)))
    local-top))

(defmethod width ((object area-mixin))
  (with-slots ((local-width width)) object
    (when (keywordp local-width)
      (assert (typep local-width 'layout-width-type))
      (calc-area (find-parent-layout object))
      (assert (typep (slot-value object 'width) 'coordinate))
      (setf local-width (slot-value object 'width)))
    local-width))

(defmethod within (x y (object area-mixin) &key)
  "Return T when x and y are within area allocated to object."
  
  (let ((left (left object))
        (top (top object))
        (right (right object))
        (bottom (bottom object)))
    (if (and (> x left) (<= x right)
             (> y top) (<= y bottom))
        t
        nil)))

(defmethod (setf area) (x y w h (obj area-mixin))
  "Syntactic suger for setting left, top, width, and height in one method."
  
  (setf (left obj) x)
  (setf (top obj) y)
  (setf (width obj) w)
  (setf (height obj) h))

;;;; functions ================================================================

(defun area (object)
  "Return left, top, width, and height of object as mutliple values."
  
  (assert (typep object 'area-mixin-base))
  (values (left object) (top object) (width object) (height object)))

(defun area-rb (object)
  "Return left, top, right, and bottom of object as multiple values."
  
  (assert (typep object 'area-mixin-base))
  (values (left object) (top object) (right object) (bottom object)))

(defun area-rect (object)
  "Return %rect holding left, top, width, and height of object."
  
  (make-instance '%rect :left (left object) :top (top object) :width (width object) :height (height object)))

(defun area-save-calc (object)
  (assert (typep object 'area-mixin-base))
  (with-slots (left top width height) object
    (flet ((is-keyword (field)
             (if (keywordp field)
                 field
                 nil)))
      (setf (slot-value object 'left-calc) (is-keyword left))
      (setf (slot-value object 'top-calc) (is-keyword top))
      (setf (slot-value object 'width-calc) (is-keyword width))
      (setf (slot-value object 'height-calc) (is-keyword height)))))

(defun find-parent-area (object)
  "Find first parent of OBJECT derived from parent-mixin. Generates error on
failure."
  
  (assert (typep object 'parent-mixin-base))
  (let ((p (parent object)))
    (loop
      (when (typep p 'manager)
        (error "found manager when looking for area-mixin"))
      
      (when (eql p nil)
        (error "no parent when looking for area-mixin"))
      
      ;; If it has area
      (if (typep p 'area-mixin-base)
          (return p))

      ;; Get next parent
      (assert (typep p 'parent-mixin-base))
      (setf p (parent p)))))

(defun reset-area (object &optional original)
  "Return layout of object to original state, allowing it to be
relayedout. When ORIGINAL provided, updates area of OBJECT based on slots of
ORIGINAL."

  (macrolet ((update-value (dest src)
               `(unless (keywordp (slot-value object ,dest))
                  (let ((value (slot-value object ,src)))
                    (when value
                      (setf (slot-value object ,dest) value)))))
             
             (update-if (old new dest src)
               `(when (eql (slot-value ,old ,dest) (slot-value ,new ,dest))
                  (when (slot-value ,new ,src)
                    (setf (slot-value ,new ,dest) (slot-value ,old ,src))))))
    
    (if (eql original nil)
        (progn
          (update-value 'width 'width-calc)
          (update-value 'height 'height-calc)
          (update-value 'left 'left-calc)
          (update-value 'top 'top-calc))
        (progn
          (update-if original object 'width 'width-calc)
          (update-if original object 'height 'height-calc)
          (update-if original object 'left 'left-calc)
          (update-if original object 'top 'top-calc)))))

