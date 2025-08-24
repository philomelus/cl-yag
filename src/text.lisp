(in-package #:cl-yag)

;;;; functions ================================================================

(declaim (ftype (function (keyword %rect (or text active-text)) number) text-calc-height))
(defun text-calc-height (type area object)
  (let ((rv (height area)))
    (case type
      ((:auto :auto-max))               ; nothing to do
      
      (:auto-min
       (let ((fh (al:get-font-line-height (theme-font object))))
         (incf fh (padding-top object))
         (incf fh (padding-bottom object))
         (setq rv (min fh (height area))))))
    rv))

(defun text-calc-left (type area object)
  (let ((rv (left area)))
    (case type
      ((:left :auto))                   ; Nothing to do
      
      (:center
       (let ((w (slot-value object 'width)))
         (incf w (padding-left object))
         (incf w (padding-right object))
         (let ((aw (width area)))
           (if (> aw w)
               (incf rv (truncate (/ (- aw w) 2)))))))

      (:right
       (let ((w (slot-value object 'width)))
         (incf w (padding-left object))
         (incf w (padding-right object))
         (setq rv (- (width area) w)))))
    rv))

(defun text-calc-title-top (obj)
  (let ((at (top obj))
        (va (v-align obj))
        (fnt (font obj)))

    ;; Make sure we have a font
    (when (eql fnt nil)
      (setq fnt (font (find-theme obj))))
    (assert (not (eql fnt nil)))
    
    ;; When auto-calculated, start at 0 offset from parent
    (if (member at *AREA-TOP-OPTS*)
        (setf at 0))

    (case va
      (:top)
      
      (:middle
       (let ((h (height obj))
             (fh (al:get-font-line-height fnt)))
         (incf at (truncate (/ (- h fh) 2)))))
      
      (:bottom
       (let ((h (height obj))
             (fh (al:get-font-line-height fnt)))
         (incf at (- h fh))))
      
      (:none)
      (otherwise
       (error "text unknown vertical alignment: ~a" va)))
   at))

(defun text-calc-top (type area object)
  (let ((rv (top area)))
    (case type
      ((:top :auto))                    ; already done

      (:middle
       ;; Calculate our height
       (let ((h (slot-value object 'height)))
         (incf h (padding-top object))
         (incf h (padding-bottom object))
         (let ((ha (height area)))
           ;; Is there room for us to move?
           (if (> ha h)
               ;; Yup, so move to middle
               (incf rv (truncate (/ (- ha h) 2)))))))

      (:bottom
       (let ((h (slot-value object 'height)))
         (incf h (padding-top object))
         (incf h (padding-bottom object))
         (incf rv (- (height area) h)))))
    rv))

(defun text-calc-width (type area object)
  (let (rv)
    (case type
      ((:auto :auto-max)
       (setq rv (width area)))

      (:auto-min
       (let ((tw (al:get-text-width (theme-font object) (title object))))
         (incf tw (padding-left object))
         (incf tw (padding-right object))
         (setq rv (min tw (width area))))))
    rv))

;;;; text-base ================================================================

(defclass text-base-theme-mixin (color-fore-back-mixin
                                 font-mixin)
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
                     padding-mixin      ; between title and edge
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
  `(make-instance 'text ,@rest))

;; (defmethod print-object ((o text-base) s)
;;   (pprint-indent :current 0 s)
;;   (pprint-logical-block (s nil)
;;     (format s "deftext ")
;;     (print-mixin o s)))

;;; methods ---------------------------------------------------------

(defmethod calc-height (type (area %rect) (object text))
  (v:debug :layout "[calc-height] {~a} area: (~d ~d) @ (~d ~d)" (print-raw-object object)
          (width area) (height area) (left area) (top area))
  (let ((rv (text-calc-height type area object)))
    (v:debug :layout "[calc-height] {~a} result: ~d" (print-raw-object object) rv)
    rv))

(defmethod calc-left (type (area %rect) (object text))
  (v:debug :layout "[calc-left] {~a} area: (~d ~d) @ (~d ~d)" (print-raw-object object)
          (width area) (height area) (left area) (top area))
  (let ((rv (text-calc-left type area object)))
    (v:debug :layout "[calc-left] {~a} result: ~d" (print-raw-object object) rv)
    rv))

(defmethod calc-top (type (area %rect) (object text))
  (v:debug :layout "[calc-top] {~a} area: (~d ~d) @ (~d ~d)" (print-raw-object object)
          (width area) (height area) (left area) (top area))
  (let ((rv (text-calc-top type area object)))
    (v:debug :layout "[calc-top] {~a} result: ~d" (print-raw-object object) rv)
    rv))

(defmethod calc-width (type (area %rect) (object text))
  (v:debug :layout "[calc-width] {~a} area: (~d ~d) @ (~d ~d)" (print-raw-object object)
          (width area) (height area) (left area) (top area))
  (let ((rv (text-calc-width type area object))) 
    (v:debug :layout "[calc-width] {~a} result: ~d" (print-raw-object object) rv)
    rv))

(defmethod on-paint ((obj text) &key)
  (let ((fc (fore-color obj))
        (fnt (font obj)))
    (when (or (eql fc nil)
              (eql fnt nil))
      
      (let ((theme (find-theme obj)))
        (when (eql fc nil)
          (setq fc (fore-color theme)))
        (when (eql fnt nil)
          (setq fnt (font theme)))))
    (let (x f)
      (case (h-align obj)
        (:left
         (setq x (left obj))
         (setq f +ALIGN-LEFT+))
        (:center
         (setq x (+ (left obj) (/ (width obj) 2)))
         (setq f +ALIGN-CENTER+))
        (:right
         (setq x (right obj))
         (setq f +ALIGN-RIGHT+)))
      (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-INVERSE-ALPHA+)
        (assert (not (eql fc nil)))
        (al:draw-text fnt fc x (text-calc-title-top obj) f (title obj)))))
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

;; (defmethod print-object ((o active-text) s)
;;   (pprint-indent :current 0 s)
;;   (pprint-logical-block (s nil)
;;     (format s "defactive-text ")
;;     (print-mixin o s)))

;;; methods ---------------------------------------------------------

(defmethod calc-height (type (area %rect) (object active-text))
  (v:debug :layout "[calc-height] {~a} area: (~d ~d) @ (~d ~d)" (print-raw-object object)
          (width area) (height area) (left area) (top area))
  (let ((rv (text-calc-height type area object)))
    (v:debug :layout "[calc-height] {~a} result: ~d" (print-raw-object object) rv)
    rv))

(defmethod calc-left (type (area %rect) (object active-text))
  (v:debug :layout "[calc-left] {~a} area: (~d ~d) @ (~d ~d)" (print-raw-object object)
          (width area) (height area) (left area) (top area))
  (let ((rv (text-calc-left type area object)))
    (v:debug :layout "[calc-left] {~a} result: ~d" (print-raw-object object) rv)
    rv))

(defmethod calc-top (type (area %rect) (object active-text))
  (v:debug :layout "[calc-top] {~a} area: (~d ~d) @ (~d ~d)" (print-raw-object object)
          (width area) (height area) (left area) (top area))
  (let ((rv (text-calc-top type area object)))
    (v:debug :layout "[calc-top] {~a} result: ~d" (print-raw-object object) rv)
    rv))

(defmethod calc-width (type (area %rect) (object active-text))
  (v:debug :layout "[calc-width] {~a} area: (~d ~d) @ (~d ~d)" (print-raw-object object)
          (width area) (height area) (left area) (top area))
  (let ((rv (text-calc-width type area object))) 
    (v:debug :layout "[calc-width] {~a} result: ~d" (print-raw-object object) rv)
    rv))

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
          (ic (interior-color obj))
          (fnt (font obj)))

      (when (or (eql cd nil)
                (eql cu nil)
                (eql cu nil)
                (eql fg nil)
                (eql bg nil)
                (eql ic nil)
                (eql fnt nil))
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
            (setq ic (interior-color theme)))
          (when (eql fnt nil)
            (setq fnt (font theme)))))

      ;; Draw background
      (assert (not (eql bg nil)))
      (al:draw-filled-rectangle (left obj) (top obj) (right obj) (bottom obj) ic)
    
      ;; Draw border
      (paint-border obj (find-theme obj))
    
      ;; Draw text
      (let (x f)
        (case (h-align obj)
          (:left
           (setq x (left obj))
           (setq f +ALIGN-LEFT+))
          (:center
           (setq x (+ (left obj) (/ (width obj) 2)))
           (setq f +ALIGN-CENTER+))
          (:right
           (setq x (right obj))
           (setq f +ALIGN-RIGHT+)))
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-INVERSE-ALPHA+)
          (assert (not (eql (if down cd cu) nil)))
          (al:draw-text fnt (if down cd cu) x (text-calc-title-top obj) f (title obj))))
    
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
          
          (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-SRC-COLOR+)
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
