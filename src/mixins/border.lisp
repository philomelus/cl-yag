(in-package #:cl-yag)

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

