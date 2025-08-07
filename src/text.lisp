(in-package #:cl-yag)

;;;; functions ================================================================

(defun text-calc-left (obj)
  (let ((al (left obj))
        (ha (h-align obj)))

    (assert (not (eq nil (font obj))))
    
    (let ((tw (al:get-text-width (font obj) (title obj)))
          (aw (width obj)))
      (case ha
        (:left)
        
        (:center
         ;; Does text fit?
         (if (> (- aw tw) 1)
             ;; Yes so center it
             (incf al (truncate (/ (- aw tw) 2)))))
        
        (:right
         (if (> (- aw tw) 0)
             (incf al (- aw tw))))

        (:none)
        (otherwise
         (error "unknown horizontal align option: ~a" ha))))
    al))

(defun text-calc-top (obj)
  (let ((at (top obj))
        (va (v-align obj)))

    (assert (not (eql nil (font obj))))
    
    ;; When auto-calculated, start at 0 offset from parent
    (if (= at +LAYOUT-LEFT-CALC+)
        (setf at 0))

    (case va
      (:top)
      
      (:middle
       (let ((h (height obj))
             (fh (al:get-font-line-height (font obj))))
         (incf at (truncate (/ (- h fh) 2)))))
      
      (:bottom
       (let ((h (height obj))
             (fh (al:get-font-line-height (font obj))))
         (incf at (- h fh))))
      
      (:none)
      (otherwise
       (error "text unknown vertical alignment: ~a" va)))
   at))

;;;; text-base ================================================================

(defclass text-base (align-mixin
                     area-mixin
                     border-mixin
                     font-mixin
                     parent-mixin
                     title-mixin)


  ((original-area :initform (list) :type list)))

(defmacro text-base (&rest rest &key &allow-other-keys)
  `(make-instance 'active-text ,@rest))

;; (defmethod print-object ((obj text-base) stream)
;;   (print-unreadable-object (obj stream :type t)
;;     (format stream "text-base ~a ~a ~a ~a ~a ~a" (dump-align-mixin obj nil)
;;            (dump-area-mixin obj nil) (dump-border-mixin obj nil)
;;            (dump-font-mixin obj nil) (dump-parent-mixin obj nil)
;;            (dump-title-mixin obj nil))))

(defmethod initialize-instance :after ((obj text-base) &key)
  (setf (slot-value obj 'original-area) (list (slot-value obj 'left) (slot-value obj 'top)
                                              (slot-value obj 'width) (slot-value obj 'height))))

(defmethod (setf height) :after (value (obj text-base))
  (setf (fourth (slot-value obj 'original-area)) value))

(defmethod (setf left) :after (value (obj text-base))
  (setf (first (slot-value obj 'original-area)) value))

(defmethod (setf top) :after (value (obj text-base))
  (setf (second (slot-value obj 'original-area)) value))

(defmethod (setf width) :after (value (obj text-base))
  (setf (third (slot-value obj 'original-area)) value))

;;;; text =====================================================================

(defclass text (text-base
                color-mixin)
  ())

(defmacro text (&rest rest &key &allow-other-keys)
  `(make-instance 'active-text ,@rest))

;; (defmethod print-object ((obj text-base) stream)
;;   (print-unreadable-object (obj stream :type t)
;;     (format stream "text ~a ~a ~a ~a ~a ~a ~a" (dump-align-mixin obj nil)
;;            (dump-area-mixin obj nil) (dump-border-mixin obj nil)
;;            (dump-font-mixin obj nil) (dump-parent-mixin obj nil)
;;            (dump-title-mixin obj nil) (dump-color-mixin obj nil))))

;;; methods ---------------------------------------------------------

(defmethod on-paint ((obj text) &key)
  (al:draw-text (font obj) (color obj) (text-calc-left obj) (text-calc-top obj) 0 (title obj))
  (next-method))

;;;; active-text ==============================================================

(defclass active-text (text-base)
  ((color-down :initarg :color-down :initform (al:map-rgb-f 0 0 0) :type list :accessor color-down)
   (color-hover :initarg :color-hover :initform (al:map-rgb-f 0.5 0.5 0.5) :type list :accessor color-hover)
   (color-up :initarg :color-up :initform (al:map-rgb-f 1 1 1) :type list :accessor color-up)
   (inside :initform nil :type boolean)
   (down :initform nil :type boolean)
   (was-down :initform nil :type boolean)
   (original-area :initform (list) :type list)))

(defmacro active-text (&rest rest &key &allow-other-keys)
  `(make-instance 'active-text ,@rest))

;; (defmethod print-object ((obj active-text) stream)
;;   (print-unreadable-object (obj stream :type t)
;;     (format stream "~a ~a ~a ~a ~a ~a :color-down (~a) :color-hover (~a) :color-up (~a)"
;;             (dump-align-mixin obj nil) (dump-area-mixin obj nil) (dump-border-mixin obj nil)
;;             (dump-font-mixin obj nil) (dump-parent-mixin obj nil) (dump-title-mixin obj nil)
;;             (dump-color (color-down obj) nil) (dump-color (color-hover obj) nil) (dump-color (color-up obj) nil))))

;;; methods ---------------------------------------------------------

(defmethod on-mouse-down (x y b (obj active-text) &key)
  (if (and (= b 1)
           (within x y obj)
           (not (slot-value obj 'was-down)))
      (setf (slot-value obj 'was-down) t)))

(defmethod on-mouse-move (x y dx dy (obj active-text) &key)
  ;; (format *standard-output* "~&on-mouse-move: active-text: (~d, ~d) (~d, ~d)" x y dx dy)
  (if (and (> x (left obj))
           (< x (right obj))
           (> y (top obj))
           (< y (bottom obj)))
      (if (not (slot-value obj 'inside))
          (setf (slot-value obj 'inside) t))
      (if (slot-value obj 'inside)
          (setf (slot-value obj 'inside) nil))))

(defmethod on-mouse-up (x y b (obj active-text) &key)
  (if (and (slot-value obj 'was-down)
           (= b 1)
           (within x y obj))
      (progn
        (setf (slot-value obj 'was-down) nil
              (slot-value obj 'down) t)
        (on-mouse-click x y b obj)
        (setf (slot-value obj 'down) nil))))

(defmethod on-paint ((obj active-text) &key)
  (let ((in (slot-value obj 'inside))
        (down (or (slot-value obj 'down) (slot-value obj 'was-down)))
        (cd (color-down obj))
        (ch (color-hover obj))
        (cu (color-up obj)))
    (al:draw-text (font obj) (if down cd cu)
                  (text-calc-left obj) (text-calc-top obj) 0 (title obj))
    (if in
        (al:draw-rectangle (1+ (left obj)) (1+ (top obj)) (1- (right obj)) (1- (bottom obj)) ch 1)))
  (next-method))
