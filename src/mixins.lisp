(in-package #:cl-yag)

;;;; active-mixin =============================================================

(defclass active-mixin ()
  ((active :initarg :active :initform nil :type boolean :accessor active)))

(defmethod print-mixin ((o active-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":active ~a " (if (active o) "t" "nil"))
  (pprint-newline :linear s)
  (my-next-method))

;;;; align-mixin ==============================================================

(defclass align-mixin (h-align-mixin v-align-mixin)
  ())

(defmethod print-mixin ((o align-mixin) s)
  ;; Base mixins get printed via call-next-method, so nothing to do here
  (my-next-method))

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
            (progn
              (format *standard-output* "~&on-mouse-down: area-mixin: claimed by ~a" (print-raw-object child))
              (return-from on-mouse-down t)))))
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

;;;; border-mixin ===============================================================

(defclass border ()
  ((color :initarg :color :initform (al:map-rgb-f 1 1 1) :type list :accessor color)
   (style :initarg :style :initform :default :type keyword :accessor style)
   (width :initarg :width :initform 1 :type integer :accessor width)))

(defmacro defborder (&rest rest &key &allow-other-keys)
  `(make-instance 'border ,@rest))

;; TODO:  This should allow the border slots to be wrapped.  Haven't figured out
;;        how to do so at this point ...
(defun print-border (o s)
  (pprint-logical-block (s nil)
    (format s "(defborder ")
    (if (eq o nil)
        (format s "nil) ")
        (progn
          (format s ":color (~a) " (print-color (color o)))
          (format s ":style :~a " (string-downcase (style o)))
          (format s ":width ~d) " (width o))))))

(defclass border-mixin ()
  ((border-left :initarg :border-left :initform nil :accessor border-left)
   (border-right :initarg :border-right :initform nil :accessor border-right)
   (border-top :initarg :border-top :initform nil :accessor border-top)
   (border-bottom :initarg :border-bottom :initform nil :accessor border-bottom)))

(defmethod print-mixin ((o border-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":border-left ")
  (let ((oo (border-left o)))
    (if (eq nil oo)
        (format s "nil ")
        (print-border oo s)))
  (pprint-newline :linear s)
  
  (pprint-indent :current 0 s)
  (format s ":border-right ")
  (let ((oo (border-right o)))
    (if (eq nil oo)
        (format s "nil ")
        (print-border oo s)))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":border-top ")
  (let ((oo (border-top o)))
    (if (eq nil oo)
        (format s "nil ")
        (print-border oo s)))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":border-bottom ")
  (let ((oo (border-bottom o)))
    (if (eq nil oo)
        (format s "nil ")
        (print-border oo s)))
  (pprint-newline :linear s)

  (my-next-method))

(defmethod (setf border) ((value border) (object border-mixin))
  (setf (border-h object) value)
  (setf (border-v object) value)
  (my-next-method))

(defmethod (setf border-h) ((value border) (object border-mixin))
  (setf (border-left object) value)
  (setf (border-right object) value)
  (my-next-method))

(defmethod (setf border-v) ((value border) (object border-mixin))
  (setf (border-top object) value)
  (setf (border-bottom object) value)
  (my-next-method))

;; Only allow valid keywords
;; #+safety
(defmethod (setf style) :after (newval (obj border))
  (let ((msg "Expected :default, :none, :in, :out, or :flat but got ~s"))
    (typecase newval
      (keyword (unless (member newval '(:default :none :in :out :flat))
                 (error msg newval)))
      (t (error msg newval))))
  (my-next-method))

;;;; color-mixin ==============================================================

(defclass color-mixin ()
  ((color :initarg :color :initform (al:map-rgb-f 1 1 1) :type list :accessor color)))

(defmethod print-mixn ((o color-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":color (~a) " (print-color (color o)))
  (pprint-newline :linear s)
  (my-next-method))

;;=============================================================================

(defclass color-fore-back-mixin ()
  ((fore-color :initarg :fore-color :initform (al:map-rgb-f 1 1 1) :type list :accessor fore-color)
   (back-color :initarg :back-color :initform (al:map-rgb-f 0 0 0) :type list :accessor back-color)))

(defmethod print-mixin ((o color-fore-back-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":fore-color (~a) " (print-color (fore-color o)))
  (pprint-newline :linear s)
  (pprint-indent :current 0 s)
  (format s ":back-color (~a) " (print-color (back-color o)))
  (pprint-newline :linear s)
  (my-next-method))

;;;; container-mixin ==========================================================
;;;; Controls area for contained objects

(defclass container-mixin (area-mixin
                           content-mixin)
  ())

(defmethod print-mixin ((o container-mixin) s)
  ;; Base mixins get printed via call-next-method, so nothing to do here
  (my-next-method))

(defmethod initialize-instance :after ((obj container-mixin) &key)
  ;; Take on the area of our (eventual) parent if exists
  (let ((pam (find-parent-area-mixin obj)))
    (if (not (eq pam nil))
        (container-copy-area pam obj)))
  
  (my-next-method))

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

(defmethod print-mixin ((o content-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":content ")
  (if (= (length (content o)) 0)
      (format s "() ")
      (let ((children (content o)))
        (pprint-logical-block (s children :prefix "(" :suffix ")")
          ;;(pprint-exit-if-list-exhausted)
          (pprint-indent :block 0 s)
          (let ((oo (pprint-pop)))
            (pprint-indent :block 0 s)
            (format s "(~a)" oo)
            (pprint-newline :fill s)
            )
          )
        )
      )
  (pprint-newline :linear s)
  (my-next-method))

(defmethod initialize-instance :after ((obj content-mixin) &key)
  ;; Let children know who their parent is
  (dolist (child (content obj))
    ;; If child has a parent slot
    (when (typep child 'parent-mixin)
      (setf (parent child) obj))

    ;; If child is a container-mixin pass on area
    (when (typep child 'container-mixin)
      (container-copy-area obj child)))
  (my-next-method))

(defmethod (setf content) :after (value (obj content-mixin))
  ;; Let children know who their parent is
  (dolist (child (content obj))
    ;; If child has a pernt slot
    (when (typep child 'parent-mixin)
      (setf (parent child) obj))

    ;; If child is a container-mixin pass on area
    (when (typep child 'container-mixin)
      (container-copy-area obj child)))

  (my-next-method))

;;;; enable-mixin =============================================================

(defclass enable-mixin ()
  ((enabled :initarg :enabled :initform nil :type boolean :accessor enabled)))

(defmethod print-mixin ((o enable-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":enabled ~a " (if (enabled o) "t" "nil"))
  (pprint-newline :linear s)
  (my-next-method))

;;;; font-mixin ===============================================================

(defclass font-mixin ()
  ((font :initarg :font :initform (cffi:null-pointer) :type cffi:foreign-pointer
         :accessor font)))

(defmethod print-mixin ((o font-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":font ")
  (if (eq nil (font o))
      (format s "nil ")
      (progn
        (print-unreadable-object ((font o) s :identity t)
          (format s "AL:FONT"))
        (write-char #\Space s)))
  (pprint-newline :linear s)
  (my-next-method))

;;;; h-align-mixin ============================================================

(defclass h-align-mixin ()
  ((h-align :initarg :h-align :initform :none :type keyword :accessor h-align)))

(defmethod print-mixin ((o h-align-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":h-align :~a" (string-downcase (h-align o)))
  (pprint-newline :linear s)
  (my-next-method))

;; Only allow valid keywords
;; #+safety
(defmethod (setf h-align) :after (newval (obj h-align-mixin))
  (let ((msg "Expected :none, :left, :right, or :center but got ~s"))
    (typecase newval
      (keyword (unless (member newval '(:none :left :right :center))
                 (error msg newval)))
      (t (error msg newval))))
  (my-next-method))

;;;; location-mixin ===========================================================

(defclass location-mixin ()
  ((x :initarg :x :initform 0 :type integer :accessor location-x)
   (y :initarg :y :initform 0 :type integer :accessor location-y)))

(defun dump-location-mixin (obj stream)
  (format stream ":x ~d :y ~d " (location-x obj) (location-y obj)))

(defmethod (setf location) (x y (object location-mixin))
  (setf (location-x object) x)
  (setf (location-y object) y)
  (my-next-method))

;;;; manager-mixin ============================================================

(defclass manager-mixin ()
  ((manager :initarg :manager :initform nil :accessor manager)))

(defmethod print-mixin ((o manager-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":manager ")
  (if (eq nil (font o))
      (format s "nil ")
      (progn
        (print-unreadable-object ((font o) s :type t :identity t)
          (format s ""))
        (write-char #\Space s)))
  (pprint-newline :linear s)
  (my-next-method))

;;;; padding-mixin ============================================================

(defclass padding-mixin ()
  ((padding-left :initarg :pad-left :initform 0 :type integer :accessor padding-left)
   (padding-right :initarg :pad-right :initform 0 :type integer :accessor padding-right)
   (padding-top :initarg :pad-top :initform 0 :type integer :accessor padding-top)
   (padding-bottom :initarg :pad-bottom :initform 0 :type integer :accessor padding-bottom)))

(defmethod print-mixin ((o padding-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":padding-left ~d " (padding-left o))
  (pprint-newline :linear s)
  
  (pprint-indent :current 0 s)
  (format s ":padding-right ~d " (padding-right o))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":padding-top ~d " (padding-top o))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":padding-bottom ~d " (padding-bottom o))
  (pprint-newline :linear s)

  (my-next-method))

(defmethod (setf padding) (value (object padding-mixin))
  (setf (padding-h object) value)
  (setf (padding-v object) value)
  (my-next-method))

(defmethod (setf padding-h) (value (object padding-mixin))
  (setf (padding-left object) value)
  (setf (padding-right object) value)
  (my-next-method))

(defmethod (setf padding-v) (value (object padding-mixin))
  (setf (padding-top object) value)
  (setf (padding-bottom object) value)
  (my-next-method))

;;;; parent-mixin =============================================================

(defclass parent-mixin ()
  ((parent :initarg :parent :initform nil :type t :accessor parent)))

(defmethod print-mixin ((o parent-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":parent ")
  (let ((p (parent o)))
    (if (eq p nil)
        (format s "NIL ")
        (progn
          (print-unreadable-object ((parent o) s :type t :identity t)
            (format s ""))
          (write-char #\Space s)
          )))
  (pprint-newline :linear s)
  (my-next-method))

(defmethod (setf parent) :after (val (obj parent-mixin))
  ;; If we have content
  (if (typep obj 'content-mixin)
      (dolist (child (content obj))
        ;; If child has a parent slot
        (when (typep child 'parent-mixin)
          (setf (parent child) obj))))
  (my-next-method))

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

(defmethod print-mixin ((o spacing-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":spacing-left ~d " (spacing-left o))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":spacing-right ~d " (spacing-right o))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":spacing-top ~d " (spacing-top o))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":spacing-bottom ~d " (spacing-bottom o))
  (pprint-newline :linear s)

  (my-next-method))

(defmethod (setf spacing) (value (object spacing-mixin))
  (setf (spacing-h object) value)
  (setf (spacing-v object) value)
  (my-next-method))

(defmethod (setf spacing-h) (value (object spacing-mixin))
  (setf (spacing-left object) value)
  (setf (spacing-right object) value)
  (my-next-method))

(defmethod (setf spacing-v) (value (object spacing-mixin))
  (setf (spacing-top object) value)
  (setf (spacing-bottom object) value)
  (my-next-method))

;; title-mixin ================================================================

(defclass title-mixin ()
  ((title :initarg :title :initform "" :type string :accessor title)))

(defmethod print-mixin ((o title-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":title ~s" (title o))
  (pprint-newline :linear s)
  (my-next-method))

;;;; v-align-mixin ============================================================

(defclass v-align-mixin ()
  ((v-align :initarg :v-align :initform :none :type keyword :accessor v-align)))

(defmethod print-mixin ((o v-align-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":v-align :~a" (string-downcase (v-align o)))
  (pprint-newline :linear s)
  (my-next-method))

;; Only allow valid keywords
;; #+safety
(defmethod (setf v-align) :after (newval (obj v-align-mixin))
  (let ((msg "Expected :none, :top, :bottom, or :middle but got ~s"))
    (typecase newval
      (keyword (unless (member newval '(:none :top :bottom :middle))
                 (error msg newval)))
      (t (error msg newval))))
  (my-next-method))

;;;; visible-mixin ============================================================

(defclass visible-mixin ()
  ((visible :initarg :visible :initform nil :type boolean :accessor visible)))

(defmethod print-mixin ((o visible-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":visible ~a " (if (visible o) "t" "nil"))
  (pprint-newline :linear s)
  (my-next-method))

