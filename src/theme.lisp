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
   (very-light :initarg :very-light :initform (al:map-rgb 245 245 245) :reader theme-vl)))

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
  (with-accessors ((b-theme theme) (b-width width)) border
    (with-slots (normal dark light very-dark very-light) theme
      (with-accessors ((left left) (top top) (o-width width) (height height)) object
        (let ((b (+ top height))
              (l1 (1+ left))
              (l2 (2+ left)))
          (case (style border)
            (:inset
             (al:draw-line l1 (1+ top) l1 (1- b) very-dark -0.5)
             (al:draw-line l2 (1+ top) l2 (2- b) dark -0.5))
    
            ((:outset :default)
             (al:draw-line l1 (1+ top) l1 (1- b) normal -0.5)
             (al:draw-line l2 (1+ top) l2 (2- b) very-light -0.5))
    
            (:flat
             (al:draw-line l1 (1+ top) l1 (1- b) very-dark -0.5)
             (al:draw-line l2 (1+ top) l2 (2- b) dark -0.5))))))))

(defmethod paint-border-top ((border border-3d) (object area-mixin) (theme theme-base))
  (with-accessors ((b-theme theme) (b-width width)) border
    (with-slots (normal dark light very-dark very-light) theme
      (with-accessors ((left left) (top top) (o-width width) (height height)) object
        (let ((r (+ left o-width))
              (t1 (1+ top))
              (t2 (2+ top)))
          (case (style border)
            (:inset
             (al:draw-line left t1 (1- r) t1 very-dark -0.5)
             (al:draw-line (1+ left) t2 (2- r) t2 dark -0.5))
            
            ((:outset :default)
             (al:draw-line left t1 (1- r) t1 normal -0.5)
             (al:draw-line (1+ left) t2 (2- r) t2 very-light -0.5))
            
            (:flat
             (al:draw-line left t1 (1- r) t1 very-dark -0.5)
             (al:draw-line (1+ left) t2 (2- r) t2 dark -0.5))))))))

;; 3d right and bottom
(defmethod paint-border-bottom ((border border-3d) (object area-mixin) (theme theme-base))
  (with-accessors ((b-theme theme) (b-width width)) border
    (with-slots (normal dark light very-dark very-light) theme
      (with-accessors ((left left) (top top) (o-width width) (height height)) object
        (let ((b (+ top height))
              (r (+ left o-width)))
          (case (style border)
            (:inset
             (al:draw-line left b r b very-light -0.5)
             (al:draw-line (1+ left) (1- b) (1- r) (1- b) normal -0.5))
            
            ((:outset :default)
             (al:draw-line left b r b very-dark -0.5)
             (al:draw-line (1+ left) (1- b) (1- r) (1- b) dark -0.5))
            
            (:flat
             (al:draw-line left b r b very-dark -0.5)
             (al:draw-line (1+ left) (1- b) (1- r) (1- b) dark -0.5))))))))

(defmethod paint-border-right ((border border-3d) (object area-mixin) (theme theme-base))
  (with-accessors ((b-theme theme) (b-width width)) border
    (with-slots (normal dark light very-dark very-light) theme
      (with-accessors ((left left) (top top) (o-width width) (height height)) object
        (let ((r (+ left o-width))
              (b (+ top height)))
          (case (style border)
            (:inset
             (al:draw-line r top r (1- b) very-light -2.5)
             (al:draw-line (1- r) (1+ top) (1- r) (2- b) normal -0.5))
            
            ((:outset :default)
             (al:draw-line r top r (1- b) very-dark -2.5)
             (al:draw-line (1- r) (1+ top) (1- r) (2- b) dark -0.5))
            
            (:flat
             (al:draw-line r top r (1- b) very-dark -2.5)
             (al:draw-line (1- r) (1+ top) (1- r) (2- b) dark -0.5))))))))

;;;; global themes ============================================================

;;; flat ------------------------------------------------------------

(defmacro deftheme-flat-obj (normal dark light very-dark very-light)
  `(make-instance 'theme-flat :dark ,dark :light ,light :normal ,normal :very-dark ,very-dark :very-light ,very-light))

(defvar *theme-flat-aqua* (deftheme-flat-obj (make-color 0 255 255) (make-color 0 191 191) (make-color 127 255 255) (make-color 0 127 127) (make-color 191 255 255)))
(defvar *theme-flat-blue* (deftheme-flat-obj (make-color 0 0 255) (make-color 0 0 191) (make-color 159 159 255) (make-color 0 0 159) (make-color 191 191 255)))
(defvar *theme-flat-gray* (deftheme-flat-obj (make-color 212 208 200) (make-color 128 128 128) (make-color 223 223 223) (make-color 95 95 95) (make-color 245 245 245)))
(defvar *theme-flat-green* (deftheme-flat-obj (make-color 0 255 0) (make-color 0 191 0) (make-color 127 255 127) (make-color 0 159 0) (make-color 191 255 191)))
(defvar *theme-flat-purple* (deftheme-flat-obj (make-color 255 0 255) (make-color 191 0 191) (make-color 255 159 255) (make-color 159 0 159) (make-color 255 191 255)))
(defvar *theme-flat-red* (deftheme-flat-obj (make-color 255 0 0) (make-color 191 0 0) (make-color 255 127 127) (make-color 159 0 0) (make-color 255 191 191)))
(defvar *theme-flat-yellow* (deftheme-flat-obj (make-color 255 255 0) (make-color 191 191 0) (make-color 255 255 127) (make-color 127 127 0) (make-color 255 255 191)))

;;; 3d --------------------------------------------------------------

(defmacro deftheme-3d-obj (normal dark light very-dark very-light)
  `(make-instance 'theme-3d :dark ,dark :light ,light :normal ,normal :very-dark ,very-dark :very-light ,very-light))

(defvar *theme-3d-aqua* (deftheme-3d-obj (make-color 0 255 255) (make-color 0 191 191) (make-color 127 255 255) (make-color 0 127 127) (make-color 191 255 255)))
(defvar *theme-3d-blue* (deftheme-3d-obj (make-color 0 0 255) (make-color 0 0 191) (make-color 159 159 255) (make-color 0 0 159) (make-color 191 191 255)))
(defvar *theme-3d-gray* (deftheme-3d-obj (make-color 212 208 200) (make-color 128 128 128) (make-color 223 223 223) (make-color 95 95 95) (make-color 245 245 245)))
(defvar *theme-3d-green* (deftheme-3d-obj (make-color 0 255 0) (make-color 0 191 0) (make-color 127 255 127) (make-color 0 159 0) (make-color 191 255 191)))
(defvar *theme-3d-purple* (deftheme-3d-obj (make-color 255 0 255) (make-color 191 0 191) (make-color 255 159 255) (make-color 159 0 159) (make-color 255 191 255)))
(defvar *theme-3d-red* (deftheme-3d-obj (make-color 255 0 0) (make-color 191 0 0) (make-color 255 127 127) (make-color 159 0 0) (make-color 255 191 191)))
(defvar *theme-3d-yellow* (deftheme-3d-obj (make-color 255 255 0) (make-color 191 191 0) (make-color 255 255 127) (make-color 127 127 0) (make-color 255 255 191)))

;;;------------------------------------------------------------------

(defparameter *theme-default* *theme-flat-gray*)

;;; These are here instead of src/mixins/theme.lisp because they need
;;; the macro defitions

(defmethod theme-d ((o theme-mixin))
  (let ((th (find-theme o)))
    (assert (not (eql nil th)))
    (theme-d th)))

(defmethod theme-l ((o theme-mixin))
  (let ((th (find-theme o)))
    (assert (not (eql nil th)))
    (theme-l th)))

(defmethod theme-n ((o theme-mixin))
  (let ((th (find-theme o)))
    (assert (not (eql nil th)))
    (theme-n th)))

(defmethod theme-vd ((o theme-mixin))
  (let ((th (find-theme o)))
    (assert (not (eql nil th)))
    (theme-vd th)))

(defmethod theme-vl ((o theme-mixin))
  (let ((th (find-theme o)))
    (assert (not (eql nil th)))
    (theme-vl th)))

