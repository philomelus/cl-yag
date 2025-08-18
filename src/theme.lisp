(in-package #:cl-yag)

;;;; forward declaration =====================================================

(defvar *theme-default*)

;;;; macros ===================================================================

(defmacro best-theme (what obj)
  `(,what (find-theme ,obj)))

(defun find-theme (o)
  ;; Does object have theme?
  (if (typep o 'theme-mixin)
      ;; Yes, is it valid?
      (if (not (eq nil (theme o)))
          ;; Yes, so use it
          (progn
            (v:debug :theme "find-theme: using object: ~a" (print-raw-object o))
            (return-from find-theme (theme o)))))

  ;; No theme, does it have a parent?
  (unless (typep o 'parent-mixin)
    ;; No parent and no theme so use default
    (v:debug :theme "find-theme: no theme, no parent, using default.")
    (return-from find-theme *theme-default*))

  ;; Object has parent, so a parent with a theme
  (let ((count 0)
        (p (parent o)))
    (loop
      ;; Valid parent?
      (if (not (eq nil p))
          ;; yes, does parent have theme?
          (if (typep p 'theme-mixin)
              ;; Yes, is it valid?
              (progn
                (unless (eq nil (theme p))
                  ;; Yes, so use it
                  (v:debug :theme "find-theme: using parent ~d: ~a" count (print-raw-object p))
                  (return-from find-theme (theme p)))

                ;; Parent has invalid theme, does it have a parent?
                (if (typep p 'parent-mixin)
                    ;; Yes, so loop
                    (progn
                      (setf p (parent p))
                      (incf count 1))
                    ;; No valid theme and no parent, use default
                    (progn
                      (v:debug :theme "find-theme: invalid parent theme, no parent, use default.")
                      (return-from find-theme *theme-default*))))
              ;; No, so does it have a parent?
              (if (typep p 'parent-mixin)
                  ;; Doesn't have a theme, but has a parent
                  (progn
                    (setf p (parent p))
                    (incf count 1))
                  ;; Doesn't have a theme and has no parent, use default
                  (progn
                    (v:debug :theme "find-theme: no contained theme, no parent, use default.")
                    (return-from find-theme *theme-default*))))

          ;; Parent not valid, so use default
          (progn
            (v:debug :theme "find-theme: no theme, no parent, use default.")
            (return-from find-theme *theme-default*))))))

;;;; methods ==================================================================

;; For any requested theme colors where the object doesn't have a theme itself,
;; try to find a parent that has a theme.  If no parent has a theme, use
;; the default theme.

(defmethod theme-d (o)
  (let ((th (find-theme o)))
    (assert (not (eql nil th)))
    (theme-d th)))

(defmethod theme-l (o)
  (let ((th (find-theme o)))
    (assert (not (eql nil th)))
    (theme-l th)))

(defmethod theme-n (o)
  (let ((th (find-theme o)))
    (assert (not (eql nil th)))
    (theme-n th)))

(defmethod theme-vd (o)
  (let ((th (find-theme o)))
    (assert (not (eql nil th)))
    (theme-vd th)))

(defmethod theme-vl (o)
  (let ((th (find-theme o)))
    (assert (not (eql nil th)))
    (theme-vl th)))

;;;; theme-base ===============================================================

(defclass theme-base (color-fore-back-mixin)
  ((dark :initarg :dark :initform (al:map-rgb 127 127 127) :reader theme-d)
   (light :initarg :light :initform (al:map-rgb 223 223 223) :reader theme-l)
   (normal :initarg :normal :initform (al:map-rgb 212 208 200) :reader theme-n)
   (very-dark :initarg :very-dark :initform (al:map-rgb 95 95 95) :reader theme-vd)
   (very-light :initarg :very-light :initform (al:map-rgb 245 245 245) :reader theme-vl)
   (fore-color :initform (make-color 0 0 0))
   (back-color :initform (make-color 255 255 255))))

(defmacro deftheme-base (&rest rest &key &allow-other-keys)
  `(make-instance 'theme-base ,@rest))

(defmethod print-object ((o theme-base) s)
(pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "deftheme-base ")

    (pprint-indent :current 0 s)
    (format s ":dark (~a) " (print-color (theme-d o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":light (~a) " (print-color (theme-l o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":normal (~a) " (print-color (theme-n o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":very-dark (~a) " (print-color (theme-vd o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":very-light (~a) " (print-color (theme-vl o)))
    (pprint-newline :linear s)

    (print-mixin o s)))

(defmethod paint-border ((object area-mixin) (theme theme-base))
  ;; Left side
  (let ((b (border-left object)))
    (unless (eql b nil)
      (paint-border-left b object theme)))

  ;; Top side
  (let ((b (border-top object)))
    (unless (eql b nil)
      (paint-border-top b object theme)))

  ;; Right side
  (let ((b (border-right object)))
    (unless (eql b nil)
      (paint-border-right b object theme)))
          
  ;; Bottom side
  (let ((b (border-bottom object)))
    (unless (eql b nil)
      (paint-border-bottom b object theme))))

;;;; theme-flat ===============================================================

(defclass theme-flat (theme-base)
  ())

(defmacro deftheme-flat (&rest rest &key &allow-other-keys)
  `(make-instance 'theme-flat ,@rest))

(defmethod print-object ((o theme-flat) s)
(pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "deftheme-flat ")

    (pprint-indent :current 0 s)
    (format s ":dark (~a) " (print-color (theme-d o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":light (~a) " (print-color (theme-l o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":normal (~a) " (print-color (theme-n o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":very-dark (~a) " (print-color (theme-vd o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":very-light (~a) " (print-color (theme-vl o)))
    (pprint-newline :linear s)

    (print-mixin o s)))

;;;; theme-3d =================================================================

(defclass theme-3d (theme-base)
  ())

(defmacro deftheme-3d (&rest rest &key &allow-other-keys)
  `(make-instance 'theme-3d ,@rest))

(defmethod print-object ((o theme-3d) s)
(pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "deftheme-3d ")

    (pprint-indent :current 0 s)
    (format s ":dark (~a) " (print-color (theme-d o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":light (~a) " (print-color (theme-l o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":normal (~a) " (print-color (theme-n o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":very-dark (~a) " (print-color (theme-vd o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":very-light (~a) " (print-color (theme-vl o)))
    (pprint-newline :linear s)

    (print-mixin o s)))

;;;; common methods ===========================================================

;; Flat left and top
(macrolet ((paint-border-side-flat (&body body)
             `(with-accessors ((color color) (b-width width)) border
                (assert (> b-width 0))
                (with-accessors ((left left) (top top) (o-width width) (height height)) object
                  (let ((c color)
                        (w b-width)
                        (w2 (truncate (/ b-width 2))))
                    (if (eql c nil)
                        (setq c (theme-n theme)))
                    ,@body)))))
  
  (defmethod paint-border-left ((border border-flat) (object area-mixin) (theme theme-base))
    (paint-border-side-flat
      (let ((xx (+ left w2)))
        (al:draw-line xx top xx (1- (+ top height)) c w))))
  
  (defmethod paint-border-top ((border border-flat) (object area-mixin) (theme theme-base))
    (paint-border-side-flat
      (let ((yy (+ top w2)))
        (al:draw-line left yy (1- (+ left o-width)) yy c w)))))

;; Flat right and bottom
(macrolet ((paint-border-side-flat (other-border &body body)
             `(with-accessors ((color color) (b-width width)) border
                (assert (> b-width 0))
                (with-accessors ((left left) (top top) (o-width width) (height height) (bo ,other-border)) object
                  (let ((c color)
                        (w b-width)
                        (w2 (truncate (/ b-width 2)))
                        (bow 0))
                    (if (eql c nil)
                        (setq c (theme-n theme)))
                    (unless (eql bo nil)
                      (setq bow (width bo)))
                    ,@body)))))
  
  (defmethod paint-border-bottom ((border border-flat) (object area-mixin) (theme theme-base))
    (paint-border-side-flat border-bottom
      (let ((yy (+ (1- (+ top (- height bow))) w2)))
        (al:draw-line left yy (1- (+ left o-width)) yy c w))))

  (defmethod paint-border-right ((border border-flat) (object area-mixin) (theme theme-base))
    (paint-border-side-flat border-left
      (let ((xx (+ (1- (+ left (- o-width bow))) w2)))
        (al:draw-line xx top xx (1- (+ top height)) c w)))))

;; 3d left and top
(defmethod paint-border-left ((border border-3d) (object area-mixin) (theme theme-base))
  (with-accessors ((b-width width) (style style) (b-theme theme)) border
    (with-accessors ((left left) (top top) (height height)) object
      (let (active-theme c1 c2)
        (if (eql nil b-theme)
            (setf active-theme theme)
            (setf active-theme b-theme))
        (with-slots (normal dark very-dark very-light) active-theme
          (case style
            (:inset (setf c1 very-dark c2 dark))
            ((:outset :default) (setf c1 normal c2 very-light))
            (:flat (setf c1 very-dark c2 dark))))
        (let ((b (+ top height))
              (hw (/ b-width 2)))
          (let ((l1 (+ left (/ b-width 4)))
                (l2 (+ left (* b-width 0.75))))
            (al:draw-line l1 (+ top hw) l1 (- b hw) c1 hw)
            (al:draw-line l2 (+ top b-width) l2 (- b b-width) c2 hw)))))))

(defmethod paint-border-top ((border border-3d) (object area-mixin) (theme theme-base))
  (with-accessors ((b-theme theme) (b-width width)) border
    (with-accessors ((left left) (top top) (o-width width) (height height)) object
      (let (active-theme c1 c2)
        (if (eql b-theme nil)
            (setq active-theme theme)
            (setq active-theme b-theme))
        (with-slots (very-dark dark normal very-light) active-theme
          (case (style border)
            (:inset (setf c1 very-dark c2 dark))
            ((:outset :default) (setf c1 normal c2 very-light))
            (:flat (setf c1 very-dark c2 dark))))
        (let ((r (+ left o-width))
              (hw (/ b-width 2)))
          (let ((t1 (+ top (/ b-width 4)))
                (t2 (+ top (* b-width 0.75))))
            (al:draw-line left t1 (- r hw) t1 c1 hw)
            (al:draw-line (+ left hw) t2 (- r b-width) t2 c2 hw)))))))

;; 3d right and bottom
(defmethod paint-border-bottom ((border border-3d) (object area-mixin) (theme theme-base))
  (with-accessors ((b-theme theme) (b-width width)) border
    (with-accessors ((left left) (top top) (o-width width) (height height)) object
      (let (active-theme c1 c2)
        (if (eql b-theme nil)
            (setq active-theme theme)
            (setq active-theme b-theme))
        (with-slots (very-light normal very-dark dark) active-theme
          (case (style border)
            (:inset (setf c1 very-light c2 normal))
            ((:outset :default) (setf c1 very-dark c2 dark))
            (:flat (setf c1 very-dark c2 dark))))
        (let ((b (+ top height))
              (r (+ left o-width))
              (hw (/ b-width 2)))
          (let ((b1 (- b (/ b-width 4)))
                (b2 (- b (* b-width 0.75))))
            (al:draw-line left b1 r b1 c1 hw)
            (al:draw-line (+ left hw) b2 (- r hw) b2 c2 hw)))))))

(defmethod paint-border-right ((border border-3d) (object area-mixin) (theme theme-base))
  (with-accessors ((b-theme theme) (b-width width)) border
    (with-accessors ((left left) (top top) (o-width width) (height height)) object
      (let (active-theme c1 c2)
        (if (eql b-theme nil)
            (setq active-theme theme)
            (setq active-theme b-theme))
        (with-slots (very-light normal very-dark dark) active-theme
          (case (style border)
            (:inset (setf c1 very-light c2 normal))
            ((:outset :default) (setf c1 very-dark c2 dark))
            (:flat (setf c1 very-dark c2 dark))))
        (let ((r (+ left o-width))
              (b (+ top height))
              (hw (/ b-width 2)))
          (let ((r1 (- r (/ b-width 4)))
                (r2 (- r (* b-width 0.75))))
            (al:draw-line r1 top r1 (- b hw) c1 hw)
            (al:draw-line r2 (+ top hw) r2 (- b hw) c2 hw)))))))

;;;; global themes ============================================================

;;; flat ------------------------------------------------------------

(defmacro deftheme-flat-obj (normal dark light very-dark very-light &optional (fore-color '(make-color 255 255 255)) (back-color '(make-color 0 0 0)))
  `(make-instance 'theme-flat :dark ,dark :light ,light :normal ,normal :very-dark ,very-dark
                              :very-light ,very-light :fore-color ,fore-color :back-color ,back-color))

(defvar *theme-flat-aqua* (deftheme-flat-obj (make-color 0 255 255) (make-color 0 191 191) (make-color 127 255 255) (make-color 0 127 127) (make-color 191 255 255)))
(defvar *theme-flat-blue* (deftheme-flat-obj (make-color 0 0 255) (make-color 0 0 191) (make-color 159 159 255) (make-color 0 0 159) (make-color 191 191 255)))
(defvar *theme-flat-gray* (deftheme-flat-obj (make-color 212 208 200) (make-color 128 128 128) (make-color 223 223 223) (make-color 95 95 95) (make-color 245 245 245) (make-color 0 0 0) (make-color 255 255 255)))
(defvar *theme-flat-green* (deftheme-flat-obj (make-color 0 255 0) (make-color 0 191 0) (make-color 127 255 127) (make-color 0 159 0) (make-color 191 255 191)))
(defvar *theme-flat-purple* (deftheme-flat-obj (make-color 255 0 255) (make-color 191 0 191) (make-color 255 159 255) (make-color 159 0 159) (make-color 255 191 255)))
(defvar *theme-flat-red* (deftheme-flat-obj (make-color 255 0 0) (make-color 191 0 0) (make-color 255 127 127) (make-color 159 0 0) (make-color 255 191 191)))
(defvar *theme-flat-yellow* (deftheme-flat-obj (make-color 255 255 0) (make-color 191 191 0) (make-color 255 255 127) (make-color 127 127 0) (make-color 255 255 191)))

;;; 3d --------------------------------------------------------------

(defmacro deftheme-3d-obj (normal dark light very-dark very-light &optional (fore-color '(make-color 255 255 255)) (back-color '(make-color 0 0 0)))
  `(make-instance 'theme-3d :dark ,dark :light ,light :normal ,normal :very-dark ,very-dark
                            :very-light ,very-light :fore-color ,fore-color :back-color ,back-color))

(defvar *theme-3d-aqua* (deftheme-3d-obj (make-color 0 255 255) (make-color 0 191 191) (make-color 127 255 255) (make-color 0 127 127) (make-color 191 255 255)))
(defvar *theme-3d-blue* (deftheme-3d-obj (make-color 0 0 255) (make-color 0 0 191) (make-color 159 159 255) (make-color 0 0 159) (make-color 191 191 255)))
(defvar *theme-3d-gray* (deftheme-3d-obj (make-color 212 208 200) (make-color 128 128 128) (make-color 223 223 223) (make-color 95 95 95) (make-color 245 245 245) (make-color 0 0 0) (make-color 255 255 255)))
(defvar *theme-3d-green* (deftheme-3d-obj (make-color 0 255 0) (make-color 0 191 0) (make-color 127 255 127) (make-color 0 159 0) (make-color 191 255 191)))
(defvar *theme-3d-purple* (deftheme-3d-obj (make-color 255 0 255) (make-color 191 0 191) (make-color 255 159 255) (make-color 159 0 159) (make-color 255 191 255)))
(defvar *theme-3d-red* (deftheme-3d-obj (make-color 255 0 0) (make-color 191 0 0) (make-color 255 127 127) (make-color 159 0 0) (make-color 255 191 191)))
(defvar *theme-3d-yellow* (deftheme-3d-obj (make-color 255 255 0) (make-color 191 191 0) (make-color 255 255 127) (make-color 127 127 0) (make-color 255 255 191)))

;;;------------------------------------------------------------------

(defparameter *theme-default* *theme-flat-gray*)

