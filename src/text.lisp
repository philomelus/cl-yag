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

(defclass text-base-theme-mixin (color-fore-back-mixin)
  ())

(defmethod print-mixin ((object text-base-theme-mixin) stream)
  ;; Nothing to do, as base mixin will print fields
  (my-next-method))

;; This exists purely to allow a common base for all text objects.  If that
;; changes, the other logic in this file may need revalidation.
(defclass text-base (text-base-theme-mixin
                     align-mixin
                     area-mixin
                     border-mixin
                     font-mixin
                     parent-mixin
                     title-mixin)
  ())

;;;; text =====================================================================

(defclass text-theme-mixin (text-base-theme-mixin)
  ((interior-color :initarg :interior-color :initform nil :accessor interior-color)))

(defclass text (text-base
                text-theme-mixin)
  ())

(defmethod print-mixin ((object text-theme-mixin) stream)
  (pprint-color-nil interior-color object stream)
  (my-next-method))

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

(defmethod (setf theme) ((theme text-theme-mixin) (object text))
  (with-slots ((ic interior-color) (fc fore-color) (bc back-color))
      theme
    (setf (interior-color object) ic)
    (setf (fore-color object) fc)
    (setf (back-color object) bc)))

;;;; active-text ==============================================================

;;; theme-mixin -----------------------------------------------------

(defclass active-text-theme-mixin (text-theme-mixin)
  ((down-color :initarg :down-color :initform nil :accessor down-color)
   (hover-color :initarg :hover-color :initform (al:map-rgb-f 0.5 0.5 0.5) :accessor hover-color)
   (up-color :initarg :up-color :initform nil :accessor up-color)))

(defmethod print-mixin ((object active-text-theme-mixin) stream)
  (pprint-color-nil down-color object stream)
  (pprint-color-nil hover-color object stream)
  (pprint-color-nil up-color object stream)
  (my-next-method))

;;; active-text -----------------------------------------------------

(defclass active-text (text-base
                       active-text-theme-mixin
                       shortcuts-mixin)
  ((inside :initform nil :type boolean) ;internal use
   (down :initform nil :type boolean)   ;internal use
   (was-down :initform nil :type boolean))) ;internal use

(defmacro defactive-text (&rest rest &key &allow-other-keys)
  `(make-instance 'active-text ,@rest))

(defmethod print-object ((o active-text) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "defactive-text ")
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
  (with-slots ((in inside) (down was-down)) obj
    (let ((cd (down-color obj))
          (ch (hover-color obj))
          (cu (up-color obj))
          (fg (fore-color obj))
          (bg (back-color obj))
          (ic (interior-color obj)))

      (when (or (eql cd nil)
                (eql cu nil)
                (eql cu nil)
                (eql fg nil)
                (eql bg nil)
                (eql ic nil))
        (let ((theme (find-theme obj)))
          (when (eql cd nil)
            (setq cd (down-color theme)))
          (when (eql ch nil)
            (setq ch (hover-color theme)))
          (when (eql cu nil)
            (setq cu (up-color theme)))
          (when (eql fg nil)
            (setq fg (fore-color theme)))
          (when (eql bg nil)
            (setq bg (back-color theme)))
          (when (eql ic nil)
            (setq ic (interior-color theme)))))

      (let ((left (left obj)))
      
        ;; Draw background
        (assert (not (eql bg nil)))
        (al:draw-filled-rectangle left (top obj) (right obj) (bottom obj) ic)
    
        ;; Draw border
        (paint-border obj (find-theme obj))
    
        ;; Draw text
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-INVERSE-ALPHA+)
          (assert (not (eql (if down cd cu) nil)))
          (al:draw-text (font obj) (if down cd cu)
                        (text-calc-left obj) (text-calc-top obj) 0 (title obj)))
    
        ;; Hilight if needed
        (when in
          (assert (not (eql ch nil)))
          (let ((left (+ (left obj) 2))
                (top (+ (top obj) 2))
                (right (- (right obj) 1))
                (bottom (- (bottom obj) 1))
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
            (al:draw-rectangle left top right bottom ch 1))))))
  
  (my-next-method))

(defmethod (setf theme) ((theme active-text-theme-mixin) (object active-text))
  (with-slots ((dc down-color) (hc hover-color)
               (ic interior-color) (uc up-color)
               (fc fore-color) (bc back-color))
      theme
    (setf (down-color object) dc)
    (setf (hover-color object) hc)
    (setf (interior-color object) ic)
    (setf (up-color object) uc)
    (setf (fore-color object) fc)
    (setf (back-color object) bc)))
