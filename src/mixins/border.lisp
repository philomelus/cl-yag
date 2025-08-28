(in-package #:cl-yag)

;;;; macros ===================================================================

(defmacro with-borders ((&rest vars) object &body body)
  (assert (= (length vars) 4))
  `(let ((,(first vars) (border-left ,object))
         (,(second vars) (border-right ,object))
         (,(third vars) (border-top ,object))
         (,(fourth vars) (border-bottom ,object)))
     ,@body))

;;;; border-base ==============================================================

(defclass border-base ()
  ((width :initarg :width :initform 1 :type integer :accessor width)))

;; (defmethod print-object ((object border-base) s)
;;   (pprint-logical-block (s nil)
;;     (format s "(defborder-base ")
;;     (if (eq object nil)
;;         (format s "NIL) ")
;;         (format s ":width ~d) " (width object)))))

;;;; border-flat ==============================================================

;;; theme-mixin -----------------------------------------------------

(defclass border-flat-theme-mixin (color-mixin)
  ())

;; print-mixin not needed because there are no non-base slots

;;; border-flat -----------------------------------------------------

(defclass border-flat (border-base
                       border-flat-theme-mixin)
  ())

(defmacro defborder-flat (&rest rest &key &allow-other-keys)
  `(make-instance 'border-flat ,@rest))

;; (defmethod print-object ((object border-flat) s)
;;   (pprint-logical-block (s nil)
;;     (format s "(defborder-flat ")
;;     (pprint-field width object s)
;;     (print-mixin object s)))

;;;; border-3d ================================================================

;;; theme-mixin -----------------------------------------------------

(defclass border-3d-theme-mixin (color-3d-mixin)
  ())

;; print-mixin not needed because there are no non-base slots

;;; border-3d -------------------------------------------------------

(defclass border-3d (border-base
                     border-3d-theme-mixin)
  ((width :initform 2)
   (style :initarg :style :initform :default :type keyword :accessor style)))

(defmacro defborder-3d (&rest rest &key &allow-other-keys)
  `(make-instance 'border-3d ,@rest))

;; (defmethod print-object ((object border-3d) s)
;;   (pprint-logical-block (s nil)
;;     (format s "(defborder-3d ")
;;     (pprint-field width object s)
;;     (pprint-field-keyword style object s)
;;     (print-mixin object s)))

#+safety
(defmethod initialize-object :after ((object border-3d) &key)
  (with-slots (style) object
    (unless (member style '(:flat :inset :outset :default))
      (error "style should be :flat, :inset, :outset, or :default, got: ~a" style))))

#+safety
(defmethod (setf style) :after (value (object border-3d))
  (unless (member value '(:flat :inset :outset :default))
    (error "style should be :flat, :inset, :outset, or :default, got: ~a" value)))

;;;; border-mixin =============================================================

(defclass border-mixin ()
  ((border-left :initarg :border-left :initform nil :accessor border-left)
   (border-right :initarg :border-right :initform nil :accessor border-right)
   (border-top :initarg :border-top :initform nil :accessor border-top)
   (border-bottom :initarg :border-bottom :initform nil :accessor border-bottom)))

(defmethod print-mixin ((o border-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":border-left ")
  (pprint-object-nil border-left o s)
  (pprint-object-nil border-right o s)
  (pprint-object-nil border-top o s)
  (pprint-object-nil border-bottom o s)
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
