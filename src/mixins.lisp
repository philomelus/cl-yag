(in-package #:clg-mixins)

;;=============================================================================

(defclass active-mixin ()
  ((enabled :initarg :active :initform nil :type boolean :accessor active)))

;;=============================================================================

(defclass align-mixin (h-align-mixin v-align-mixin)
  ())

;;=============================================================================

(defclass area-mixin ()
  ((left :initarg :left :initarg :x :initform 0 :type integer
         :accessor area-left :accessor area-x)
   (top :initarg :top :initarg :y :initform 0 :type integer
        :accessor area-top :accessor area-y)
   (width :initarg :width :initarg :w :initform 0 :type integer
          :accessor area-width :accessor area-w)
   (height :initarg :height :initarg :h :initform 0 :type integer
           :accessor area-height :accessor area-h)))

(defgeneric area-right (obj &rest rest &key &allow-other-keys)
  (:method ((obj area-mixin) &rest _ &key)
    (declare (ignore _))
    (+ (area-left obj) (area-width obj))))

(defgeneric area-bottom (obj &rest rest &key &allow-other-keys)
  (:method ((obj area-mixin) &rest _ &key)
    (declare (ignore _))
    (+ (area-top obj) (area-height obj))))

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
  ((h-align :initarg :h-align :initarg :align-h :initform :none :type keyword
            :accessor h-align :accessor align-h)))

;; Only allow valid keywords
;; #+safety
(defmethod (setf h-align) :after (newval (obj h-align-mixin))
  (let ((msg "Expected :none, :left, :right, or :center but got ~s"))
    (typecase newval
            (keyword (unless (member newval '(:none :left :right :center))
                       (error msg newval)))
            (t (error msg newval))))
  (call-next-method))

;;=============================================================================

(defclass location-mixin ()
  ((x :initarg :x :initform 0 :type integer :accessor location-x)
   (y :initarg :y :initform 0 :type integer :accessor location-y)))

(defgeneric (setf location) (x y object &key &allow-other-keys)
  (:method (x y (object location-mixin) &key)
    (setf (location-x object) x)
    (setf (location-y object) y)))

;;=============================================================================

(defclass padding-mixin ()
  ((padding-left :initarg :pad-left :initform 0 :type integer :accessor padding-left)
   (padding-right :initarg :pad-right :initform 0 :type integer :accessor padding-right)
   (padding-top :initarg :pad-top :initform 0 :type integer :accessor padding-top)
   (padding-bottom :initarg :pad-bottom :initform 0 :type integer :accessor padding-bottom)))

(defgeneric (setf padding) (value object &key &allow-other-keys)
  (:method (value (object padding-mixin) &key)
    (setf (padding-h object) value)
    (setf (padding-v object) value)))

(defgeneric (setf padding-h) (value object &key &allow-other-keys)
  (:method (value (object padding-mixin) &key)
    (setf (padding-left object) value)
    (setf (padding-right object) value)))

(defgeneric (setf padding-v) (value object &key &allow-other-keys)
  (:method (value (object padding-mixin) &key)
    (setf (padding-top object) value)
    (setf (padding-bottom object) value)))

;;=============================================================================

(defclass spacing-mixin ()
  ((spacing-left :initarg :pad-left :initform 0 :type integer :accessor spacing-left)
   (spacing-right :initarg :pad-right :initform 0 :type integer :accessor spacing-right)
   (spacing-top :initarg :pad-top :initform 0 :type integer :accessor spacing-top)
   (spacing-bottom :initarg :pad-bottom :initform 0 :type integer :accessor spacing-bottom)))

(defgeneric (setf spacing) (value object &key &allow-other-keys)
  (:method (value (object spacing-mixin) &key)
    (setf (spacing-h object) value)
    (setf (spacing-v object) value)))

(defgeneric (setf spacing-h) (value object &key &allow-other-keys)
  (:method (value (object spacing-mixin) &key)
    (setf (spacing-left object) value)
    (setf (spacing-right object) value)))

(defgeneric (setf spacing-v) (value object &key &allow-other-keys)
  (:method (value (object spacing-mixin) &key)
    (setf (spacing-top object) value)
    (setf (spacing-bottom object) value)))

;;=============================================================================

(defclass v-align-mixin ()
  ((v-align :initarg :v-align :initarg :align-v :initform :none :type keyword
            :accessor v-align :accessor align-v)))

;; Only allow valid keywords
;; #+safety
(defmethod (setf v-align) :after (newval (obj v-align-mixin))
  (let ((msg "Expected :none, :top, :bottom, or :middle but got ~s"))
    (typecase newval
            (keyword (unless (member newval '(:none :top :bottom :middle))
                       (error msg newval)))
            (t (error msg newval))))
  (call-next-method))

;;=============================================================================

(defclass visible-mixin ()
  ((visible :initarg :visible :initform nil :type boolean :accessor visible)))

