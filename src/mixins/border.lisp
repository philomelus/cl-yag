(in-package #:cl-yag)

;;;; border-base ==============================================================

(defclass border-base ()
  ((width :initarg :width :initform 1 :type integer :accessor width)))

(defmethod print-object ((object border-base) s)
  (pprint-logical-block (s nil)
    (format s "(defborder-base ")
    (if (eq object nil)
        (format s "NIL) ")
        (format s ":width ~d) " (width object)))))

;;;; border-flat ==============================================================

(defclass border-flat (border-base)
  ((color :initarg :color :initform nil :type list :accessor color)))

(defmacro defborder-flat (&rest rest &key &allow-other-keys)
  `(make-instance 'border-flat ,@rest))

(defmethod print-object ((object border-flat) s)
  (pprint-logical-block (s nil)
    (format s "(defborder-flat ")
    (if (eq object nil)
        (format s "NIL) ")
        (with-slots (width color) object
          (format s ":width ~d " width)
          (if (eql color nil)
              (format s ":color NIL) ")
              (format s ":color ~a) " (print-color color)))))))

;;;; border-3d ================================================================

(defclass border-3d (border-base)
  ((width :initform 2)
   (style :initarg :style :initform :default :type keyword :accessor style)
   (theme :initarg :theme :initform nil :accessor theme)))

(defmacro defborder-3d (&rest rest &key &allow-other-keys)
  `(make-instance 'border-3d ,@rest))

(defmethod print-object ((object border-3d) s)
  (pprint-logical-block (s nil)
    (format s "(defborder-3d ")
    (if (eq object nil)
        (format s "NIL) ")
        (progn
          (format s ":width ~d " (width object))
          (format s ":style :~a " (style object))
          (format s ":theme ")
          (let ((th (theme object)))
            (if (eql th nil)
                (format s "NIL) ")
                (format s "~a) " (print-raw-object th))))))))

#+safety
(defmethod (setf style) :after (value (object border-3d))
  (unless (member value '(:flat :inset :outset :default))
    (error "expected :flat, :inset, :outset, or :default, got: ~a" value)))

;;;; border-mixin =============================================================

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
        (prin1 oo s)))
  (pprint-newline :linear s)
  
  (pprint-indent :current 0 s)
  (format s ":border-right ")
  (let ((oo (border-right o)))
    (if (eq nil oo)
        (format s "nil ")
        (prin1 oo s)))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":border-top ")
  (let ((oo (border-top o)))
    (if (eq nil oo)
        (format s "nil ")
        (prin1 oo s)))
  (pprint-newline :linear s)

  (pprint-indent :current 0 s)
  (format s ":border-bottom ")
  (let ((oo (border-bottom o)))
    (if (eq nil oo)
        (format s "nil ")
        (prin1 oo s)))
  (pprint-newline :linear s)

  (my-next-method))

(defmethod (setf border) ((value border-base) (object border-mixin))
  (setf (border-h object) value)
  (setf (border-v object) value)
  (my-next-method))

(defmethod (setf border-h) ((value border-base) (object border-mixin))
  (setf (border-left object) value)
  (setf (border-right object) value)
  (my-next-method))

(defmethod (setf border-v) ((value border-base) (object border-mixin))
  (setf (border-top object) value)
  (setf (border-bottom object) value)
  (my-next-method))
