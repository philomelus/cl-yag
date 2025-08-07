(in-package #:cl-yag)

;;;; active-mixin =============================================================

(defclass active-mixin ()
  ((enabled :initarg :active :initform nil :type boolean :accessor active)))

(defun dump-active-mixin (obj stream)
  (format stream ":enabled ~a" (if (active obj) "t" "nil")))

;;;; align-mixin ==============================================================

(defclass align-mixin (h-align-mixin v-align-mixin)
  ())

(defun dump-align-mixin (obj stream)
  (format stream "~a ~a" (dump-h-align-mixin obj nil) (dump-v-align-mixin obj nil)))

;;;; area-mixin ===============================================================

(defclass area-mixin ()
  ((left :initarg :left :initform +LAYOUT-LEFT-CALC+ :type integer :accessor left)
   (top :initarg :top :initform +LAYOUT-TOP-CALC+ :type integer :accessor top)
   (width :initarg :width :initform +LAYOUT-HEIGHT-CALC+ :type integer :accessor width)
   (height :initarg :height :initform +LAYOUT-WIDTH-CALC+ :type integer :accessor height)))

;;; functions -------------------------------------------------------

(defun dump-area-mixin (obj stream)
  (format stream ":left ~d :top ~d :width ~d :height ~d" (left obj) (top obj)
          (width obj) (height obj)))

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
  (next-method))

(defmethod on-mouse-down (x y b (obj area-mixin) &key)
  (if (typep obj 'container-mixin)
      (dolist (child (content obj))
        (on-mouse-down x y b child)))
  (next-method))

(defmethod on-mouse-up (x y b (obj area-mixin) &key)
  (if (typep obj 'container-mixin)
      (dolist (child (content obj))
        (on-mouse-up x y b child)))
  (next-method))

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

;;;; border-mixin ===============================================================

(defclass border ()
  ((color :initarg :color :initform (al:map-rgb-f 1 1 1) :type list :accessor color)
   (style :initarg :style :initform :default :type keyword :accessor style)
   (width :initarg :width :initform 1 :type integer :accessor width)))

(defun dump-border (obj stream)
  (with-slots (color style width) obj
   (format stream "border :color (al:map-rgba-f ~d ~d ~d ~d) :style ~a :width ~d"
           (color-r color) (color-g color) (color-b color) (color-a color)
           style width)))

(defclass border-mixin ()
  ((border-left :initarg :border-left :initform nil :accessor border-left)
   (border-right :initarg :border-right :initform nil :accessor border-right)
   (border-top :initarg :border-top :initform nil :accessor border-top)
   (border-bottom :initarg :border-bottom :initform nil :accessor border-bottom)))

(defun dump-border-mixin (obj stream)
  (let ((out (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s out)
      (if (slot-boundp obj 'border-left)
          (if (eql (border-left obj) nil)
              (format s ":border-left nil")
              (format s ":border-left '(~a)" (dump-border (border-left obj) nil))))
      (if (slot-boundp obj 'border-right)
          (if (eql (border-right obj) nil)
              (format s " :border-right nil")
              (format s " :border-right '(~a)" (dump-border (border-right obj) nil))))
      (if (slot-boundp obj 'border-top)
          (if (eql (border-top obj) nil)
              (format s " :border-top nil")
              (format s " :border-top '(~a)" (dump-border (border-top obj) nil))))
      (if (slot-boundp obj 'border-bottom)
          (if (eql (border-bottom obj) nil)
              (format s " :border-bottom nil")
              (format s " :border-bottom '(~a)" (dump-border (border-bottom obj) nil)))))
    (format stream out)))

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

;; Only allow valid keywords
;; #+safety
(defmethod (setf style) :after (newval (obj border))
  (let ((msg "Expected :default, :none, :in, :out, or :flat but got ~s"))
    (typecase newval
      (keyword (unless (member newval '(:default :none :in :out :flat))
                 (error msg newval)))
      (t (error msg newval))))
  (next-method))

;;;; color-mixin ==============================================================

(defclass color-mixin ()
  ((color :initarg :color :initform (al:map-rgb-f 1 1 1) :type list :accessor color)))

(defun dump-color-mixin (obj stream)
  (with-slots (color) obj
    (format stream ":color (al:map-rgba-f ~d ~d ~d ~d)"
            (color-r color) (color-g color) (color-b color) (color-a color))))

;;=============================================================================

(defclass color-fore-back-mixin ()
  ((fore-color :initarg :fore-color :initform (al:map-rgb-f 1 1 1) :type list :accessor fore-color)
   (back-color :initarg :back-color :initform (al:map-rgb-f 0 0 0) :type list :accessor back-color)))

(defun dump-color-fore-back-mixin (obj stream)
  (with-slots ((fore fore-color) (back back-color)) obj
    (format stream ":fore-color (al:map-rgba-f ~d ~d ~d ~d) :back-color (al:map-rgba-f ~d ~d ~d ~d)"
            (color-r fore) (color-g fore) (color-b fore) (color-a fore)
            (color-r back) (color-g back) (color-b back) (color-a back))))

;;;; container-mixin ==========================================================
;;;; Controls area for contained objects

(defclass container-mixin (area-mixin
                           content-mixin)
  ())

(defun dump-container-mixin (obj stream)
  (format stream "~a ~a" (dump-area-mixin obj nil) (dump-content-mixin obj nil)))

(defmethod initialize-instance :after ((obj container-mixin) &key)
  ;; Take on the area of our (eventual) parent if exists
  (let ((pam (find-parent-area-mixin obj)))
    (if (not (eq pam nil))
        (container-copy-area pam obj)))
  
  (next-method))

(defun container-copy-area (source container)
  "Copy area contents from source to container."
  (setf (left container) (1+ (left source))
        (top container) (1+ (top source))
        (height container) (- (height source) 2)
        (width container) (- (width source) 2))

  ;; Adjust for padding if needed
  (if (typep container 'padding-mixin)
      (progn
        (incf (left container) (padding-left container))
        (incf (top container) (padding-top container))
        (decf (height container) (+ (padding-top container) (padding-bottom container)))
        (decf (width container) (+ (padding-left container) (padding-right container))))))

;;;; content-mixin ============================================================
;;;; Has contained objects

(defclass content-mixin ()
  ((content :initarg :content :initform (list) :type list :accessor content)))

(defun dump-content-mixin (obj stream)
  (format stream ":content ~a" (content obj)))

(defmethod initialize-instance :after ((obj content-mixin) &key)
  ;; Let children know who their parent is
  (dolist (child (content obj))
    ;; If child has a parent slot
    (when (typep child 'parent-mixin)
      (setf (parent child) obj))

    ;; If child is a container-mixin pass on area
    (when (typep child 'container-mixin)
      (container-copy-area obj child)))
  (next-method))

(defmethod (setf content) :after (value (obj content-mixin))
  ;; Let children know who their parent is
  (dolist (child (content obj))
    ;; If child has a pernt slot
    (when (typep child 'parent-mixin)
      (setf (parent child) obj))

    ;; If child is a container-mixin pass on area
    (when (typep child 'container-mixin)
      (container-copy-area obj child)))

  (next-method))

;;;; enable-mixin =============================================================

(defclass enable-mixin ()
  ((enabled :initarg :enabled :initform nil :type boolean :accessor enabled)))

(defun dump-enable-mixin (obj stream)
  (format stream ":enabled ~a" (if (enabled obj) "t" "nil")))

;;;; font-mixin ===============================================================

(defclass font-mixin ()
  ((font :initarg :font :initform (cffi:null-pointer) :type cffi:foreign-pointer
         :accessor font)))

(defun dump-font-mixin (obj stream)
  (format stream ":font ~a" (font obj)))

;;;; h-align-mixin ============================================================

(defclass h-align-mixin ()
  ((h-align :initarg :h-align :initform :none :type keyword :accessor h-align)))

(defun dump-h-align-mixin (obj stream)
  (format stream ":h-align ~a" (h-align obj)))

;; Only allow valid keywords
;; #+safety
(defmethod (setf h-align) :after (newval (obj h-align-mixin))
  (let ((msg "Expected :none, :left, :right, or :center but got ~s"))
    (typecase newval
      (keyword (unless (member newval '(:none :left :right :center))
                 (error msg newval)))
      (t (error msg newval))))
  (next-method))

;;;; location-mixin ===========================================================

(defclass location-mixin ()
  ((x :initarg :x :initform 0 :type integer :accessor location-x)
   (y :initarg :y :initform 0 :type integer :accessor location-y)))

(defun dump-location-mixin (obj stream)
  (format stream ":x ~d :y ~d" (location-x obj) (location-y obj)))

(defmethod (setf location) (x y (object location-mixin))
  (setf (location-x object) x)
  (setf (location-y object) y)
  (next-method))

;;;; manager-mixin ============================================================

(defclass manager-mixin ()
  ((manager :initarg :manager :initform nil :accessor manager)))

(defun dump-manager-mixin (obj stream)
  (format stream ":manager ~a" (manager obj)))

;;;; padding-mixin ============================================================

(defclass padding-mixin ()
  ((padding-left :initarg :pad-left :initform 0 :type integer :accessor padding-left)
   (padding-right :initarg :pad-right :initform 0 :type integer :accessor padding-right)
   (padding-top :initarg :pad-top :initform 0 :type integer :accessor padding-top)
   (padding-bottom :initarg :pad-bottom :initform 0 :type integer :accessor padding-bottom)))

(defun dump-padding-mixin (obj stream)
  (format stream ":padding-left ~d :padding-right ~d :padding-top ~d :padding-bottom ~d"
          (padding-left obj) (padding-right obj) (padding-top obj) (padding-bottom obj)))

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

;;;; parent-mixin =============================================================

(defclass parent-mixin ()
  ((parent :initarg :parent :initform nil :type t :accessor parent)))

(defun dump-parent-mixin (obj stream)
  (format stream ":parent ~a" (parent obj)))

(defmethod (setf parent) :after (val (obj parent-mixin))
  ;; If we have content
  (if (typep obj 'content-mixin)
      (dolist (child (content obj))
        ;; If child has a parent slot
        (when (typep child 'parent-mixin)
          (setf (parent child) obj))))
  (next-method))

(defmethod owner ((obj parent-mixin))
  "Returns manager of current object or nil."
  (let ((par-obj (parent obj)))
    (loop
      (if (typep par-obj 'manager)
          (return-from owner par-obj))
      (if (eq nil par-obj)
          (return-from owner nil))
      (setf par-obj (parent par-obj)))))

;;;; spacing-mixin ============================================================

(defclass spacing-mixin ()
  ((spacing-left :initarg :spacing-left :initform 0 :type integer :accessor spacing-left)
   (spacing-right :initarg :spacing-right :initform 0 :type integer :accessor spacing-right)
   (spacing-top :initarg :spacing-top :initform 0 :type integer :accessor spacing-top)
   (spacing-bottom :initarg :spacing-bottom :initform 0 :type integer :accessor spacing-bottom)))

(defun dump-spacing-mixin (obj stream)
  (format stream ":spacing-left ~d :psacing-right ~d :spacing-top ~d :spacing-bottom ~d"
          (spacing-left obj) (spacing-right obj) (spacing-top obj) (spacing-bottom obj)))

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

(defun dump-title-mixin (obj stream)
  (format stream ":title ~s" (title obj)))

;;;; v-align-mixin ============================================================

(defclass v-align-mixin ()
  ((v-align :initarg :v-align :initform :none :type keyword :accessor v-align)))

(defun dump-v-align-mixin (obj stream)
  (format stream ":v-align ~a" (v-align obj)))

;; Only allow valid keywords
;; #+safety
(defmethod (setf v-align) :after (newval (obj v-align-mixin))
  (let ((msg "Expected :none, :top, :bottom, or :middle but got ~s"))
    (typecase newval
      (keyword (unless (member newval '(:none :top :bottom :middle))
                 (error msg newval)))
      (t (error msg newval))))
  (next-method))

;;;; visible-mixin ============================================================

(defclass visible-mixin ()
  ((visible :initarg :visible :initform nil :type boolean :accessor visible)))

(defun dump-visible-mixin (obj stream)
  (format stream ":visible ~a" (if (visible obj) "t" "nil")))

