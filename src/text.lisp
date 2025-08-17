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
                     color-fore-back-mixin
                     font-mixin
                     parent-mixin
                     shortcuts-mixin
                     title-mixin)
  ((fore-color :initform nil)
   (back-color :initform nil)))

(defmacro deftext-base (&rest rest &key &allow-other-keys)
  `(make-instance 'active-text ,@rest))

(defmethod print-object ((o text-base) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "deftext-base ")
    (print-mixin o s)))

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
  ((color-down :initarg :color-down :initform nil :type list :accessor color-down)
   (color-hover :initarg :color-hover :initform nil :type list :accessor color-hover)
   (color-up :initarg :color-up :initform nil :type list :accessor color-up)
   (shortcuts :initarg :shortcuts :initform nil :type list :accessor shortcuts)
   
   ;; Internal stuff
   (inside :initform nil :type boolean)
   (down :initform nil :type boolean)
   (was-down :initform nil :type boolean)))

(defmacro defactive-text (&rest rest &key &allow-other-keys)
  `(make-instance 'active-text ,@rest))

(defmethod print-object ((o active-text) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "defactive-text ")

    (pprint-indent :current 0 s)
    (if (eq nil (color-down o))
        (format s ":color-down nil ")
        (format s ":color-down (~a) " (print-color (color-down o))))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (if (eq nil (color-hover o))
        (format s ":color-hover nil ")
        (format s ":color-hover (~a) " (print-color (color-hover o))))
    (pprint-newline :linear s)
    
    (pprint-indent :current 0 s)
    (if (eq nil (color-up o))
        (format s ":color-up nil ")
        (format s ":color-up (~a) " (print-color (color-up o))))
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
            (on-command obj)
            (setf (slot-value obj 'down) nil)
            (return-from on-mouse-up t))
          (progn
            (setf (slot-value obj 'was-down) nil
                  (slot-value obj 'down) nil)
            (v:debug :event "on-mouse-up: active-text: aborted"))))
  nil)

(defmethod on-paint ((obj active-text) &key)
  (let ((in (slot-value obj 'inside))
        (down (or (slot-value obj 'down) (slot-value obj 'was-down)))
        (cd (color-down obj))
        (ch (color-hover obj))
        (cu (color-up obj))
        (fg (fore-color obj))
        (bg (back-color obj)))

    ;; Make sure colors are correct
    (if (eql cd nil)
        (setf cd (theme-vd obj)))
    (if (eql ch nil)
        (setf ch (theme-vl obj)))
    (if (eql cu nil)
        (setf cu (theme-n obj)))
    (if (eql bg nil)
        (setf bg (theme-l obj)))
    (if (eql fg nil)
        (setf fg (theme-vl obj)))

    (let ((left (left obj)))
      
      ;; Draw background
      (al:draw-filled-rectangle left (top obj) (right obj) (bottom obj) bg)
    
      ;; Draw border
      (paint-border obj (find-theme obj))
    
      ;; Draw text
      (al:draw-text (font obj) (if down cd fg)
                    (text-calc-left obj) (text-calc-top obj) 0 (title obj))
    
      ;; Hilight if needed
      (when in
        (let ((left (+ (left obj) 2))
              (top (+ (top obj) 2))
              (right (- (right obj) 3))
              (bottom (- (bottom obj) 3))
              (bl (border-left obj))
              (bt (border-top obj))
              (br (border-right obj))
              (bb (border-bottom obj)))
          (unless (eql bl nil)
            (incf left (width bl)))
          (unless (eql bt nil)
            (incf top (width bt)))
          (unless (eql br nil)
            (decf right (width br)))
          (unless (eql bb nil)
            (decf bottom (width bb)))
          (al:draw-rectangle left top right bottom ch 1)))))
  
  (my-next-method))

(defmethod (setf theme) ((theme theme-flat) (o active-text))
  (setf (color-down o) (theme-vd theme))
  (setf (color-hover o) (theme-vl theme))
  (setf (color-up o) (theme-n theme))
  (setf (back-color o) (theme-d theme))
  (setf (fore-color o) (theme-l theme))
  
  (unless (eq nil (border-left o))
    (setf (color (border-left o)) (theme-l theme)))
  
  (unless (eq nil (border-right o))
    (setf (color (border-right o)) (theme-l theme)))
  
  (unless (eq nil(border-top o))
    (setf (color (border-top o)) (theme-l theme)))
  
  (unless (eq nil (border-bottom o))
    (setf (color (border-bottom o)) (theme-l theme)))
  )

;; ;; TODO: Dunno what to do yet
;; (defmethod (setf theme) ((theme theme-3d) (o active-text))
;;   (my-next-method))
