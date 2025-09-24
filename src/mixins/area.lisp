(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; area-mixin-base ==========================================================

(defclass area-mixin-base ()
  ((left :type layout-left-type :initarg :left :initform :auto :accessor left :documentation "Coordinate of left side.")
   (top :type layout-top-type :initarg :top :initform :auto :accessor top :documentation "Coordinate of top side.")
   (width :type layout-width-type :initarg :width :initform :auto :accessor width :documentation "Width of object.")
   (height :type layout-height-type :initarg :height :initform :auto :accessor height :documentation "Height of object.")))

;;;; area-mixin ===============================================================

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
             (if (typep field 'keyword)
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
  (if (typep obj 'content-mixin)
      (dolist (child (content obj))
        (unless (eql child nil)
         (on-mouse-move x y dx dy child))))
  (my-next-method))

(defmethod on-mouse-down (x y b (obj area-mixin) &key)
  (if (typep obj 'content-mixin)
      (dolist (child (content obj))
        (unless (eql child nil)
            (if (on-mouse-down x y b child)
             (return-from on-mouse-down t)))))
  (my-next-method))

(defmethod on-mouse-up (x y b (obj area-mixin) &key)
  (if (typep obj 'content-mixin)
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
  "Return left, top, width, and height of object as mutliple values."
  
  (assert (typep object 'area-mixin))
  (values (left object) (top object) (width object) (height object)))

(defun area-rb (object)
  "Return left, top, right, and bottom of object as multiple values."
  
  (assert (typep object 'area-mixin))
  (values (left object) (top object) (right object) (bottom object)))

(defun area-rect (object)
  "Return %rect holding left, top, width, and height of object."
  
  (make-instance '%rect :left (left object) :top (top object) :width (width object) :height (height object)))

(defun area-save-calc (object)
  (assert (typep object 'area-mixin))
  (with-slots (left top width height) object
    (flet ((is-keyword (field)
             (if (typep field 'keyword)
                 field
                 nil)))
      (setf (slot-value object 'left-calc) (is-keyword left))
      (setf (slot-value object 'top-calc) (is-keyword top))
      (setf (slot-value object 'width-calc) (is-keyword width))
      (setf (slot-value object 'height-calc) (is-keyword height)))))

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

(defun reset-area (object &optional original)
  "Return layout of object to original state, allowing it to be
relayedout. When ORIGINAL provided, updates area of OBJECT based on slots of
ORIGINAL."

  (macrolet ((update-value (dest src)
               `(unless (typep (slot-value object ,dest) 'keyword)
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

