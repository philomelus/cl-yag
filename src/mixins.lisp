(in-package #:clg)

;;=============================================================================

(defclass active-mixin ()
  ((enabled :initarg :active :initform nil :type boolean :accessor active)))

;;=============================================================================

(defclass align-mixin (h-align-mixin v-align-mixin)
  ())

;;=============================================================================

(defclass area-mixin ()
  ((left :initarg :left :initform 0 :type integer :accessor area-left)
   (top :initarg :top :initform 0 :type integer :accessor area-top)
   (width :initarg :width :initform 0 :type integer :accessor area-width)
   (height :initarg :height :initform 0 :type integer :accessor area-height)))

(defmethod area-bottom ((obj area-mixin) &key)
  (+ (area-top obj) (area-height obj)))

(defun find-parent-area-mixin (obj)
  (if (not (typep obj 'parent-mixin))
      (return-from find-parent-area-mixin nil))  
  (let ((par-obj (parent obj)))
    (loop
      (if (eq nil par-obj)
          (return-from find-parent-area-mixin nil))
      (if (typep par-obj 'area-mixin)
         (return-from find-parent-area-mixin par-obj))
      (if (not (typep par-obj 'parent-mixin))
          (return-from find-parent-area-mixin nil)
          (setf par-obj (parent par-obj))))))

(defmethod area-height ((obj area-mixin))
  (let ((h (slot-value obj 'height)))
    (if (> h 0)
        (return-from area-height h))
    ;; Invalid height, so calculate it if possible
    (let ((am (find-parent-area-mixin obj)))
      (if (eq am nil)
          (error "height not specified for any object in hierarchy")
          (area-height am)))))

(defmethod area-left ((obj area-mixin))
  (let ((ol (slot-value obj 'left)))
   ;; If no parent, return objects left
   (if (not (typep obj 'parent-mixin))
       (return-from area-left ol))
  
   ;; Otherwise add the parent's left
   (let ((am (find-parent-area-mixin obj)))
     ;; Parent area-mixin?
     (if (eq am nil)
         (return-from area-left ol))
     (+ ol (area-left am)))))

(defmethod area-right ((obj area-mixin) &key)
  (+ (area-left obj) (area-width obj)))

(defmethod area-top ((obj area-mixin))
  (let ((ot (slot-value obj 'top)))
   ;; If no parent, return objects top
   (if (not (typep obj 'parent-mixin))
       (return-from area-top ot))

   ;; Otherwise add the parents top
   (let ((am (find-parent-area-mixin obj)))
     ;; Parent area-mixin?
     (if (eq am nil)
         (return-from area-top ot))
     (+ ot (area-top am)))))

(defmethod area-width ((obj area-mixin))
  (let ((w (slot-value obj 'width)))
    (if (> w 0)
        (return-from area-width w))
    ;; Invalid width, so calculate it if possible
    (let ((am (find-parent-area-mixin obj)))
      (if (eq am nil)
          (error "width not specified for any object in hierarchy")
          (area-width am)))))

(defmethod (setf area) (x y w h (obj area-mixin))
  (setf (area-left obj) x)
  (setf (area-top obj) y)
  (setf (area-width obj) w)
  (setf (area-height obj) h))

;;;; border-mixin ===============================================================

(defclass border ()
  ((border-color :initarg :color :initform (al:map-rgb-f 1 1 1) :type list :accessor color)
   (border-style :initarg :style :initform :default :type keyword :accessor style)
   (border-width :initarg :width :initform 1 :type integer :accessor width)))

(defclass border-mixin ()
  ((border-left :initarg :border-left :initform nil :accessor border-left)
   (border-right :initarg :border-right :initform nil :accessor border-right)
   (border-top :initarg :border-top :initform nil :accessor border-top)
   (border-bottom :initarg :border-bottom :initform nil :accessor border-bottom)))

(defmethod (setf border) ((value border) (object border-mixin))
  (setf (border-h object) value)
  (setf (border-v object) value)
  (next-method))

(defmethod (setf border-h) ((value border) (object border-mixin))
  (setf (border-left object) value)
  (setf (border-right object) value)
  (next-method))

(defmethod (setf border-v) ((value border) (object border-mixin))
  (setf (border-top object) value)
  (setf (border-bottom object) value)
  (next-method))

;;=============================================================================

(defclass color-mixin ()
  ((color :initarg :color :initform (al:map-rgb-f 1 1 1) :type list :accessor color)))

;;=============================================================================

(defclass color-fore-back--mixin ()
  ((fore-color :initarg :color :initform (al:map-rgb-f 1 1 1) :type list :accessor fore-color)
   (back-color :initarg :back-color :initform (al:map-rgb-f 0 0 0) :type list :accessor back-color)))

;;=============================================================================

(defclass enable-mixin ()
  ((enabled :initarg :enabled :initform nil :type boolean :accessor enabled)))

;;=============================================================================

(defclass font-mixin ()
  ((font :initarg :font :initform (cffi:null-pointer) :type cffi:foreign-pointer
         :accessor font)))

;;=============================================================================

(defclass h-align-mixin ()
  ((h-align :initarg :h-align :initform :none :type keyword :accessor h-align)))

;; Only allow valid keywords
;; #+safety
(defmethod (setf h-align) :after (newval (obj h-align-mixin))
  (let ((msg "Expected :none, :left, :right, or :center but got ~s"))
    (typecase newval
            (keyword (unless (member newval '(:none :left :right :center))
                       (error msg newval)))
            (t (error msg newval))))
  (next-method))

;;=============================================================================

(defclass location-mixin ()
  ((x :initarg :x :initform 0 :type integer :accessor location-x)
   (y :initarg :y :initform 0 :type integer :accessor location-y)))

(defmethod (setf location) (x y (object location-mixin))
  (setf (location-x object) x)
  (setf (location-y object) y)
  (next-method))

;;=============================================================================

(defclass padding-mixin ()
  ((padding-left :initarg :pad-left :initform 0 :type integer :accessor padding-left)
   (padding-right :initarg :pad-right :initform 0 :type integer :accessor padding-right)
   (padding-top :initarg :pad-top :initform 0 :type integer :accessor padding-top)
   (padding-bottom :initarg :pad-bottom :initform 0 :type integer :accessor padding-bottom)))

(defmethod (setf padding) (value (object padding-mixin))
  (setf (padding-h object) value)
  (setf (padding-v object) value)
  (next-method))

(defmethod (setf padding-h) (value (object padding-mixin))
  (setf (padding-left object) value)
  (setf (padding-right object) value)
  (next-method))

(defmethod (setf padding-v) (value (object padding-mixin))
  (setf (padding-top object) value)
  (setf (padding-bottom object) value)
  (next-method))

;;=============================================================================

(defclass parent-mixin ()
  ((parent :initarg :parent :initform nil :type t :accessor parent)))

;;=============================================================================

(defclass spacing-mixin ()
  ((spacing-left :initarg :spacing-left :initform 0 :type integer :accessor spacing-left)
   (spacing-right :initarg :spacing-right :initform 0 :type integer :accessor spacing-right)
   (spacing-top :initarg :spacing-top :initform 0 :type integer :accessor spacing-top)
   (spacing-bottom :initarg :spacing-bottom :initform 0 :type integer :accessor spacing-bottom)))

(defmethod (setf spacing) (value (object spacing-mixin))
  (setf (spacing-h object) value)
  (setf (spacing-v object) value)
  (next-method))

(defmethod (setf spacing-h) (value (object spacing-mixin))
  (setf (spacing-left object) value)
  (setf (spacing-right object) value)
  (next-method))

(defmethod (setf spacing-v) (value (object spacing-mixin))
  (setf (spacing-top object) value)
  (setf (spacing-bottom object) value)
  (next-method))

;; title-mixin ================================================================

(defclass title-mixin ()
  ((title :initarg :title :initform "" :type string :accessor title)))

;;=============================================================================

(defclass v-align-mixin ()
  ((v-align :initarg :v-align :initform :none :type keyword :accessor v-align)))

;; Only allow valid keywords
;; #+safety
(defmethod (setf v-align) :after (newval (obj v-align-mixin))
  (let ((msg "Expected :none, :top, :bottom, or :middle but got ~s"))
    (typecase newval
            (keyword (unless (member newval '(:none :top :bottom :middle))
                       (error msg newval)))
            (t (error msg newval))))
  (next-method))

;;=============================================================================

(defclass visible-mixin ()
  ((visible :initarg :visible :initform nil :type boolean :accessor visible)))

