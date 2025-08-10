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

(defmacro deftext-base (&rest rest &key &allow-other-keys)
  `(make-instance 'active-text ,@rest))

(defmethod print-object ((o text-base) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "deftext-base ")
    (print-mixin o s)))

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

(defmacro deftext (&rest rest &key &allow-other-keys)
  `(make-instance 'active-text ,@rest))

(defmethod print-object ((o text-base) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "deftext ")
    (print-mixin o s)))

;;; methods ---------------------------------------------------------

(defmethod on-paint ((obj text) &key)
  (al:draw-text (font obj) (color obj) (text-calc-left obj) (text-calc-top obj) 0 (title obj))
  (my-next-method))

;;;; active-text ==============================================================

(defclass active-text (text-base)
  ((color-down :initarg :color-down :initform (al:map-rgb-f 0 0 0) :type list :accessor color-down)
   (color-hover :initarg :color-hover :initform (al:map-rgb-f 0.5 0.5 0.5) :type list :accessor color-hover)
   (color-up :initarg :color-up :initform (al:map-rgb-f 1 1 1) :type list :accessor color-up)
   (inside :initform nil :type boolean)
   (down :initform nil :type boolean)
   (was-down :initform nil :type boolean)
   (original-area :initform (list) :type list)))

(defmacro defactive-text (&rest rest &key &allow-other-keys)
  `(make-instance 'active-text ,@rest))

(defmethod print-object ((o active-text) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "defactive-text ")

    (pprint-indent :current 0 s)
    (with-slots ((c color-down)) o
      (format s ":color-down (al:map-rgba-f ~d ~d ~d ~d) " (color-r c) (color-g c) (color-b c) (color-a c)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (with-slots ((c color-hover)) o
      (format s ":color-hover (al:map-rgba-f ~d ~d ~d ~d) " (color-r c) (color-g c) (color-b c) (color-a c)))
    (pprint-newline :linear s)
    
    (pprint-indent :current 0 s)
    (with-slots ((c color-up)) o
      (format s ":color-up (al:map-rgba-f ~d ~d ~d ~d) " (color-r c) (color-g c) (color-b c) (color-a c)))
    (pprint-newline :linear s)

    (print-mixin o s)))

;;; methods ---------------------------------------------------------

(defmethod on-mouse-down (x y b (obj active-text) &key)
  (if (and (= b +MOUSE-BUTTON-LEFT+)
           (within x y obj)
           (not (slot-value obj 'was-down)))
      (progn
        (setf (slot-value obj 'was-down) t)
        (on-mouse-down-accept obj (owner obj))
        (return-from on-mouse-down t)))
  nil)

(defmethod on-mouse-move (x y dx dy (obj active-text) &key)
  (if (within x y obj)
      (if (not (slot-value obj 'inside))
          (setf (slot-value obj 'inside) t))
      (if (slot-value obj 'inside)
          (setf (slot-value obj 'inside) nil))))

(defmethod on-mouse-up (x y b (obj active-text) &key)
  (if (and (slot-value obj 'was-down)
           (= b 1))
      (if (within x y obj)
          (progn
            (v:debug :event "on-mouse-up: active-text: :x ~d :y ~d :b ~d" x y b)
            (setf (slot-value obj 'was-down) nil
                  (slot-value obj 'down) t)
            (on-mouse-click x y b obj)
            (setf (slot-value obj 'down) nil)
            (return-from on-mouse-up t))
          (progn
            (setf (slot-value obj 'was-down) nil
                  (slot-value obj 'down) nil)
            (v:info :event "on-mouse-up: active-text: aborted"))))
  nil)

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
  (my-next-method))
