(in-package #:cl-yag)

;;;; macros ===================================================================

(defmacro theme-field (field obj)
  `(let ((th-ob (,field ,obj)))
     (when (eql th-ob nil)
       (setq th-ob (,field (find-theme ,obj))))
     th-ob))

;; TODO:  When I get better with macros and lisp ... save a LOT of code!
;;
;; (defmacro with-theme ((fields) obj &body body)
;;   )
;;
;; (with-theme ((f font) (fc fore-color) (bc back-color)) obj
;;   (format t "~a" f)
;;   (format t "~a" fc)
;;   (format t "~a" bc))
;;
;; (let ((f (font obj))
;;       (fc (font-color obj))
;;       (bc (back-color obj)))
;;   (when (not (or f fc bc))
;;     (let ((theme (find-theme obj)))
;;       (when (not f)
;;         (setq f (font theme)))
;;       (when (not fc)
;;         (setq fc (fore-color theme)))
;;       (when (not bc)
;;         (setq bc (back-color theme)))))
;;   (format t "~a" f)
;;   (format t "~a" fc)
;;   (format t "~a" bc))

(defmacro theme-font (obj)
  `(theme-field font ,obj))

;;;; forward declaration =====================================================

(defvar *theme-default*)

;;;; macros ===================================================================

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

;;;; theme-base ===============================================================

(defclass theme-base (color-fore-back-mixin)
  ())

;; (defmethod print-object ((o theme-base) s)
;;   (pprint-indent :current 0 s)
;;   (pprint-logical-block (s nil)
;;     (format s "deftheme-base")
;;     (print-mixin o s)))

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
  ((frame-color :initarg :frame-color :initform nil :accessor frame-color)
   (interior-color :initarg :interior-color :initform nil :accessor interior-color)))

(defmacro deftheme-flat (&rest rest &key &allow-other-keys)
  `(make-instance 'theme-flat ,@rest))

;; (defmethod print-object ((o theme-flat) s)
;; (pprint-indent :current 0 s)
;;   (pprint-logical-block (s nil)
;;     (format s "deftheme-flat ")
;;     (print-mixin o s)))

;;; methods ---------------------------------------------------------

(macrolet ((paint-border-side-flat (&body body)
             `(with-accessors ((color color) (b-width width)) border
                (assert (> b-width 0))
                (with-accessors ((left left) (top top) (o-width width) (height height)) object
                  (let ((c color)
                        (w b-width)
                        (w2 (truncate (/ b-width 2))))
                    (if (eql c nil)
                        (setq c (frame-color theme)))
                    ,@body)))))
  
  (defmethod paint-border-left ((border border-flat) (object area-mixin) (theme theme-flat))
    (paint-border-side-flat
      (let ((xx (+ left w2)))
        (assert (not (eql c nil)))
        (al:draw-line xx top xx (+ top height) c w))))
  
  (defmethod paint-border-top ((border border-flat) (object area-mixin) (theme theme-flat))
    (paint-border-side-flat
      (let ((yy (+ top w2)))
        (al:draw-line left yy (+ left o-width) yy c w)))))

(macrolet ((paint-border-side-flat (other-border &body body)
             `(with-accessors ((color color) (b-width width)) border
                (assert (> b-width 0))
                (with-accessors ((left left) (top top) (o-width width) (height height) (bo ,other-border)) object
                  (let ((c color)
                        (w b-width)
                        (w2 (truncate (/ b-width 2)))
                        (bow 0))
                    (if (eql c nil)
                        (setq c (frame-color theme)))
                    (unless (eql bo nil)
                      (setq bow (width bo)))
                    ,@body)))))
  
  (defmethod paint-border-bottom ((border border-flat) (object area-mixin) (theme theme-flat))
    (paint-border-side-flat border-bottom
      (let ((yy (+ (+ top (- height bow)) w2)))
        (al:draw-line left yy (+ left o-width) yy c w))))

  (defmethod paint-border-right ((border border-flat) (object area-mixin) (theme theme-flat))
    (paint-border-side-flat border-left
      (let ((xx (+ (+ left (- o-width bow)) w2)))
        (al:draw-line xx top xx (+ top height) c w)))))

;;;; theme-flat-all ===========================================================

(defclass theme-flat-all (theme-flat
                          active-text-theme-mixin
                          border-flat-theme-mixin
                          grid-theme-mixin
                          ruler-theme-mixin
                          text-theme-mixin
                          window-theme-mixin)
  ())

;; (defmethod print-object ((object theme-flat-all) stream)
;;   )

;;;; theme-3d =================================================================

(defclass theme-3d (theme-base
                    color-3d-mixin)
  ())

(defmacro deftheme-3d (&rest rest &key &allow-other-keys)
  `(make-instance 'theme-3d ,@rest))

;; (defmethod print-object ((o theme-3d) s)
;;   (pprint-indent :current 0 s)
;;   (pprint-logical-block (s nil)
;;     (format s "deftheme-3d ")
;;     (print-mixin o s)))

;;;; methods ==================================================================

(macrolet ((theme-read-field (name field)
             `(progn
                (defmethod ,name (object)
                  (let ((th (find-theme object)))
                    (assert (not (eql th nil)))
                    (slot-value th ,field)))
                (defmethod ,name ((object theme-mixin))
                  (with-slots (theme) object
                    (assert (typep theme 'theme-3d))
                    (slot-value theme ,field))))))
  (theme-read-field theme-3d-d 'dark)
  (theme-read-field theme-3d-l 'light)
  (theme-read-field theme-3d-n 'normal)
  (theme-read-field theme-3d-vd 'very-dark)
  (theme-read-field theme-3d-vl 'very-light))

;;;; common methods ===========================================================

(macrolet ((with-theme-3d-colors
               ((n d l vd vl) border theme &body body)
               `(let ((,n (normal-color ,border))
                      (,d (dark-color ,border))
                      (,l (light-color ,border))
                      (,vd (very-dark-color ,border))
                      (,vl (very-light-color ,border)))
                  (when (eql ,n nil)
                    (setf ,n (normal-color ,theme)))
                  (when (eql ,d nil)
                    (setf ,d (dark-color ,theme)))
                  (when (eql ,l nil)
                    (setf ,l (light-color ,theme)))
                  (when (eql ,vd nil)
                    (setf ,vd (very-dark-color ,theme)))
                  (when (eql ,vl nil)
                    (setf ,vl (very-light-color ,theme)))
                  ,@body)))
  
(defmethod paint-border-left ((border border-3d) (object area-mixin) (theme theme-3d))
  (with-theme-3d-colors (n d l vd vl) border theme
    (with-accessors ((left left) (top top)) object
      (with-slots (width) border
        (let (c1 c2)
          (case (style border)
            (:inset (setf c1 vd c2 d))
            ((:outset :default) (setf c1 n c2 vl))
            (:flat (setf c1 vd c2 d)))
          (let ((b (+ top (height object)))
                (hw (/ width 2)))
            (let ((l1 (+ left (/ width 4)))
                  (l2 (+ left (* width 0.75))))
              (with-blender (+OP-ADD+ +BLEND-SRC-COLOR+ +BLEND-SRC-COLOR+)
                (al:draw-line l1 (+ top hw) l1 (- b hw) c1 hw)
                (al:draw-line l2 (+ top width) l2 (- b width) c2 hw)))))))))
  
(defmethod paint-border-top ((border border-3d) (object area-mixin) (theme theme-3d))
  (with-theme-3d-colors (n d l vd vl) border theme
    (with-accessors ((left left) (top top)) object
      (with-slots (width) border
        (let (c1 c2)
          (case (style border)
            (:inset (setf c1 vd c2 d))
            ((:outset :default) (setf c1 n c2 vl))
            (:flat (setf c1 vd c2 d)))
          (let ((r (+ left (width object)))
                (hw (/ width 2)))
            (let ((t1 (+ top (/ width 4)))
                  (t2 (+ top (* width 0.75))))
              (with-blender (+OP-ADD+ +BLEND-SRC-COLOR+ +BLEND-SRC-COLOR+)
                (al:draw-line left t1 (- r hw) t1 c1 hw)
                (al:draw-line (+ left hw) t2 (- r width) t2 c2 hw)))))))))
  
  (defmethod paint-border-bottom ((border border-3d) (object area-mixin) (theme theme-3d))
    (with-theme-3d-colors (n d l vd vl) border theme
      (with-accessors ((left left) (top top)) object
        (with-slots (width) border
          (let (c1 c2)
            (case (style border)
              (:inset (setf c1 vl c2 n))
              ((:outset :default) (setf c1 vd c2 d))
              (:flat (setf c1 vd c2 d)))
            (let ((b (+ top (height object)))
                  (r (+ left (width object)))
                  (hw (/ width 2)))
              (let ((b1 (- b (/ width 4)))
                    (b2 (- b (* width 0.75))))
                (with-blender (+OP-ADD+ +BLEND-SRC-COLOR+ +BLEND-SRC-COLOR+)
                  (al:draw-line left b1 r b1 c1 hw)
                  (al:draw-line (+ left hw) b2 (- r hw) b2 c2 hw)))))))))
  
  (defmethod paint-border-right ((border border-3d) (object area-mixin) (theme theme-3d))
    (with-theme-3d-colors (n d l vd vl) border theme
      (with-accessors ((left left) (top top)) object
        (with-slots (width) border
          (let (c1 c2)
            (case (style border)
              (:inset (setf c1 vl c2 n))
              ((:outset :default) (setf c1 vd c2 d))
              (:flat (setf c1 vd c2 d)))
            (let ((r (+ left (width object)))
                  (b (+ top (height object)))
                  (hw (/ width 2)))
              (let ((r1 (- r (/ width 4)))
                    (r2 (- r (* width 0.75))))
                (with-blender (+OP-ADD+ +BLEND-SRC-COLOR+ +BLEND-SRC-COLOR+)
                  (al:draw-line r1 top r1 (- b hw) c1 hw)
                  (al:draw-line r2 (+ top hw) r2 (- b hw) c2 hw))))))))))

;;;; theme-3d-all =============================================================

(defclass theme-3d-all (theme-3d
                        active-text-theme-mixin
                        border-3d-theme-mixin
                        border-flat-theme-mixin
                        grid-theme-mixin
                        ruler-theme-mixin
                        text-theme-mixin
                        window-theme-mixin)
  ())

;; (defmethod print-object ((object theme-3d-all) stream)
;;   (pprint-indent :current 0 stream)
;;   (pprint-logical-block (stream nil)
;;     (format stream "deftheme-3d-all ")
;;     (print-mixin object stream)))

;;;; global object theme handlers =============================================

;;; active-text -----------------------------------------------------

(defmethod (setf theme) ((theme theme-flat) (object active-text))
  (with-slots ((f frame-color) (i interior-color)
               (fc fore-color) (bc back-color))
      theme
    (setf (down-color object) f)
    (setf (hover-color object) f)
    (setf (interior-color object) i)
    (setf (up-color object) i)
    (setf (fore-color object) fc)
    (setf (back-color object) bc)))

(defmethod (setf theme) ((theme theme-3d) (object active-text))
  (with-slots ((d dark-color) (n normal-color)
               (vd very-dark-color) (vl very-light-color)
               (fc fore-color) (bc back-color))
      theme
    (setf (down-color object) vd)
    (setf (hover-color object) d)
    (setf (interior-color object) n)
    (setf (up-color object) vl)
    (setf (fore-color object) fc)
    (setf (back-color object) bc)))

;;; grid ------------------------------------------------------------

(defmethod (setf theme) ((theme theme-flat) (object grid))
  (when (typep theme 'ruler-theme-mixin))
  (with-slots ((fc frame-color)) theme
    (setf (major-color-h object) fc)
    (setf (major-color-v object) fc)
    (setf (minor-color-h object) fc)
    (setf (minor-color-v object) fc)))

(defmethod (setf theme) ((theme theme-3d) (object grid))
  (with-slots ((d dark) (vd very-dark)) theme
    (setf (major-color-h object) d)
    (setf (major-color-v object) d)
    (setf (minor-color-h object) vd)
    (setf (minor-color-v object) vd)))

;;; ruler -----------------------------------------------------------

(defmethod (setf theme) ((theme theme-flat) (object ruler))
  (with-slots ((f frame-color)) theme
    (setf (color object) f)
    (setf (major-color object) f)
    (setf (minor-color object) f)))

(defmethod (setf theme) ((theme theme-3d) (object ruler))
  (with-slots ((l light) (vd very-dark) (vl very-light)) theme
    (setf (color object) vd)
    (setf (major-color object) vl)
    (setf (minor-color object) l)))

;;; text ------------------------------------------------------------

(defmethod (setf theme) ((theme theme-flat) (object text))
  (with-slots ((i interior-color) (fc fore-color) (bc back-color))
      theme
    (setf (interior-color object) i)
    (setf (fore-color object) fc)
    (setf (back-color object) bc)))

(defmethod (setf theme) ((theme theme-3d) (object text))
  (with-slots ((n normal-color) (fc fore-color) (bc back-color))
      theme
    (setf (interior-color object) n)
    (setf (fore-color object) fc)
    (setf (back-color object) bc)))

;;; window ----------------------------------------------------------

(defmethod (setf theme) ((theme theme-flat) (object window))
  (with-slots ((bc back-color) (fc fore-color) (i interior-color))
      theme
    (setf (interior-color object) i)
    (setf (back-color object) bc)
    (setf (fore-color object) fc)))

(defmethod (setf theme) ((theme theme-3d) (object window))
  (with-slots ((d dark-color) (n normal-color)) theme
    (setf (frame-color object) d)
    (setf (interior-color object) n)))

;;;; global themes ============================================================

;; These should initialize their fields the same way the above (setf theme)
;; functions do.  TODO: Make it so the code exists once!

;;; flat ------------------------------------------------------------

;; Adjustments for global themes

(defmacro deftheme-flat-all-obj (frame interior
                                 &optional (fore '(al:map-rgb 255 255 255)) (back '(al:map-rgb-f 0.0 0.0 0.0))
                                 &body body)
  (let ((object (gensym)))
    `(funcall (lambda (f i fc bc)
                (let ((,object (make-instance 'theme-flat-all :fore-color fc :back-color bc)))
                  ;; active-text
                  (setf (font ,object) (default-font))
                  (setf (down-color ,object) f)
                  (setf (hover-color ,object) f)
                  (setf (interior-color ,object) i)
                  (setf (up-color ,object) i)
                  ;; border-3d is not valid from a flat theme
                  ;; border-flat
                  (setf (color ,object) f)
                  ;; grid
                  (setf (major-color-h ,object) f)
                  (setf (major-color-v ,object) f)
                  (setf (minor-color-h ,object) f)
                  (setf (minor-color-v ,object) f)
                  ;; ruler
                  (setf (major-color ,object) f)
                  (setf (minor-color ,object) f)
                  ;; text
                  ;; window
                  (setf (frame-color ,object) f)

                  ;; Custom initialization code
                  (let ((object ,object))
                    ,@body
                    (setf ,object object))
                  
                  ;; Make sure all slots get set
                  (dolist (slot (class-slots (find-class 'theme-flat-all)))
                    (when (eql (slot-value ,object (slot-definition-name slot)) nil)
                      (error "Missing theme-flat-all setting: ~a" (slot-definition-name slot))))
                  ,object))
              ,frame ,interior ,fore ,back)))

(defun theme-flat-aqua ()
  (deftheme-flat-all-obj (al:map-rgb 0 191 191) (al:map-rgb 0 255 255)))

(defun theme-flat-blue ()
  (deftheme-flat-all-obj (al:map-rgb 0 0 191) (al:map-rgb 0 0 255)))

(defun theme-flat-gray ()
  (deftheme-flat-all-obj (al:map-rgb-f 0.5 0.5 0.5) (al:map-rgb 212 208 200) (al:map-rgb-f 0.0 0.0 0.0) (al:map-rgb-f 1.0 1.0 1.0)
    ;; Override the up color
    (setf (up-color object) (fore-color object))))

(defun theme-flat-green ()
  (deftheme-flat-all-obj (al:map-rgb 0 191 0) (al:map-rgb 0 255 0)))

(defun theme-flat-purple ()
  (deftheme-flat-all-obj (al:map-rgb 191 0 191) (al:map-rgb 255 0 255)))

(defun theme-flat-red ()
  (deftheme-flat-all-obj (al:map-rgb 191 0 0) (al:map-rgb 255 0 0)))

(defun theme-flat-yellow ()
  (deftheme-flat-all-obj (al:map-rgb 191 191 0) (al:map-rgb 255 255 0)))

;;; 3d --------------------------------------------------------------

(defmacro deftheme-3d-obj (normal dark light very-dark very-light
                           &optional (fore-color '(al:map-rgb-f 1.0 1.0 1.0)) (back-color '(al:map-rgb 0 0 0))
                           &body body)
  (let ((object (gensym)))
    `(funcall (lambda (n d l vd vl fc bc)
                (let ((,object (make-instance 'theme-3d-all :normal n :dark d :light l :very-dark vd :very-light vl :fore-color fc :back-color bc)))
                  ;; active-text
                  (setf (font ,object) (default-font))
                  (setf (down-color ,object) vd)
                  (setf (hover-color ,object) d)
                  (setf (interior-color ,object) n)
                  (setf (up-color ,object) vl)
                  ;; border-3d
                  ;; border-flat
                  (setf (color ,object) d)
                  ;; grid
                  (setf (major-color-h ,object) d)
                  (setf (major-color-v ,object) d)
                  (setf (minor-color-h ,object) vd)
                  (setf (minor-color-v ,object) vd)
                  ;; ruler
                  (setf (major-color ,object) vl)
                  (setf (minor-color ,object) l)
                  ;; text taken care of above
                  ;; window

                  ;; Custom initialization code
                  (let ((object ,object))
                    ,@body
                    (setf ,object object))

                  ;; Make sure all slots get set
                  (dolist (slot (class-slots (find-class 'theme-3d-all)))
                    (when (eql (slot-value ,object (slot-definition-name slot)) nil)
                      (error "Missing theme-3d-all setting: ~a" (slot-definition-name slot))))
                  
                  ,object))
              ,normal ,dark ,light ,very-dark ,very-light, fore-color, back-color)))

(defun theme-3d-aqua ()
  (deftheme-3d-obj (al:map-rgb 0 255 255) (al:map-rgb 0 191 191) (al:map-rgb 127 255 255) (al:map-rgb 0 127 127) (al:map-rgb 191 255 255)))

(defun theme-3d-blue ()
  (deftheme-3d-obj (al:map-rgb 0 0 255) (al:map-rgb 0 0 191) (al:map-rgb 159 159 255) (al:map-rgb 0 0 159) (al:map-rgb 191 191 255)))

(defun theme-3d-gray ()
  (deftheme-3d-obj
      (al:map-rgb 212 208 200) (al:map-rgb 128 128 128) (al:map-rgb 223 223 223) (al:map-rgb 95 95 95) (al:map-rgb 245 245 245) (al:map-rgb 0 0 0) (al:map-rgb 255 255 255)
    ;; Override the active-text up/down for this one
    (setf (up-color object) (fore-color object))
    (setf (down-color object) (dark-color object))))

(defun theme-3d-green ()
  (deftheme-3d-obj (al:map-rgb 0 255 0) (al:map-rgb 0 191 0) (al:map-rgb 127 255 127) (al:map-rgb 0 159 0) (al:map-rgb 191 255 191)))

(defun theme-3d-purple ()
  (deftheme-3d-obj (al:map-rgb 255 0 255) (al:map-rgb 191 0 191) (al:map-rgb 255 159 255) (al:map-rgb 159 0 159) (al:map-rgb 255 191 255)))

(defun theme-3d-red ()
  (deftheme-3d-obj (al:map-rgb 255 0 0) (al:map-rgb 191 0 0) (al:map-rgb 255 127 127) (al:map-rgb 159 0 0) (al:map-rgb 255 191 191)))

(defun theme-3d-yellow ()
  (deftheme-3d-obj (al:map-rgb 255 255 0) (al:map-rgb 191 191 0) (al:map-rgb 255 255 127) (al:map-rgb 127 127 0) (al:map-rgb 255 255 191)))

;;;------------------------------------------------------------------

(defparameter *theme-default* (theme-flat-gray))

