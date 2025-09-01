(in-package #:cl-yag)

;;;; forward declaration =====================================================

(defvar *theme-default*)

;;;; functions ================================================================

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

(defclass theme-base (back-fore-color-mixin)
  ())

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

;;; methods ---------------------------------------------------------

;;;; theme-3d =================================================================

(defclass theme-3d (theme-base
                    color-3d-mixin)
  ())

(defmacro deftheme-3d (&rest rest &key &allow-other-keys)
  `(make-instance 'theme-3d ,@rest))

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

(macrolet ((paint-border-side-flat (theme &body body)
             `(let ((color (color ,theme)))
                (with-accessors ((b-width thickness)) border
                  (assert (> b-width 0))
                  (with-accessors ((left left) (top top) (o-width width) (height height)) object
                    (let ((c color)
                          (w b-width)
                          (w2 (truncate (/ b-width 2))))
                      (if (eql c nil)
                          (setq c (frame-color theme)))
                      ,@body))))))
  
  (defmethod paint-border-left ((border border) (object area-mixin) (theme theme-flat))
    (paint-border-side-flat theme
                            (let ((xx (+ left w2)))
                              (assert (not (eql c nil)))
                              (al:draw-line xx top xx (+ top height) c w))))
  
  (defmethod paint-border-top ((border border) (object area-mixin) (theme theme-flat))
    (paint-border-side-flat theme
                            (let ((yy (+ top w2)))
                              (al:draw-line left yy (+ left o-width) yy c w)))))

(macrolet ((paint-border-side-flat (other-border theme &body body)
             `(let ((color (color ,theme)))
                (with-accessors ((b-width thickness)) border
                  (assert (> b-width 0))
                  (with-accessors ((left left) (top top) (o-width width) (height height) (bo ,other-border)) object
                    (let ((c color)
                          (w b-width)
                          (w2 (truncate (/ b-width 2)))
                          (bow 0))
                      (if (eql c nil)
                          (setq c (frame-color theme)))
                      (unless (eql bo nil)
                        (setq bow (thickness bo)))
                      ,@body))))))
  
  (defmethod paint-border-bottom ((border border) (object area-mixin) (theme theme-flat))
    (paint-border-side-flat border-bottom theme
                            (let ((yy (+ (+ top (- height bow)) w2)))
                              (al:draw-line left yy (+ left o-width) yy c w))))

  (defmethod paint-border-right ((border border) (object area-mixin) (theme theme-flat))
    (paint-border-side-flat border-left theme
                            (let ((xx (+ (+ left (- o-width bow)) w2)))
                              (al:draw-line xx top xx (+ top height) c w)))))

(macrolet ((with-theme-3d-colors
               ((n d l vd vl) border theme &body body)
               (let ((border-var (gensym))
                     (theme-var (gensym)))
                 `(let ((,border-var ,border)
                        (,theme-var ,theme))
                    (let ((,n (normal-color ,border-var))
                          (,d (dark-color ,border-var))
                          (,l (light-color ,border-var))
                          (,vd (very-dark-color ,border-var))
                          (,vl (very-light-color ,border-var)))
                      (when (eql ,n nil)
                        (setf ,n (normal-color ,theme-var)))
                      (when (eql ,d nil)
                        (setf ,d (dark-color ,theme-var)))
                      (when (eql ,l nil)
                        (setf ,l (light-color ,theme-var)))
                      (when (eql ,vd nil)
                        (setf ,vd (very-dark-color ,theme-var)))
                      (when (eql ,vl nil)
                        (setf ,vl (very-light-color ,theme-var)))
                      ,@body)))))
  
  (defmethod paint-border-left ((border border) (object area-mixin) (theme theme-3d))
    (with-theme-3d-colors (n d l vd vl) border theme
      (with-accessors ((left left) (top top)) object
        (with-local-slots ((bw width)) border
          (let (c1 c2)
            (case (style border)
              (:inset (setf c1 vd c2 d))
              ((:outset :default) (setf c1 n c2 vl))
              (:flat (setf c1 vd c2 d)))
            (let ((b (+ top (height object)))
                  (hw (/ bw 2)))
              (let ((l1 (+ left (/ bw 4)))
                    (l2 (+ left (* bw 0.75))))
                (with-blender (+OP-ADD+ +BLEND-SRC-COLOR+ +BLEND-SRC-COLOR+)
                  (al:draw-line l1 (+ top hw) l1 (- b hw) c1 hw)
                  (al:draw-line l2 (+ top bw) l2 (- b bw) c2 hw)))))))))
  
  (defmethod paint-border-top ((border border) (object area-mixin) (theme theme-3d))
    (with-theme-3d-colors (n d l vd vl) border theme
      (with-accessors ((left left) (top top)) object
        (with-local-slots ((bw width)) border
          (let (c1 c2)
            (case (style border)
              (:inset (setf c1 vd c2 d))
              ((:outset :default) (setf c1 n c2 vl))
              (:flat (setf c1 vd c2 d)))
            (let ((r (+ left (width object)))
                  (hw (/ bw 2)))
              (let ((t1 (+ top (/ bw 4)))
                    (t2 (+ top (* bw 0.75))))
                (with-blender (+OP-ADD+ +BLEND-SRC-COLOR+ +BLEND-SRC-COLOR+)
                  (al:draw-line left t1 (- r hw) t1 c1 hw)
                  (al:draw-line (+ left hw) t2 (- r bw) t2 c2 hw)))))))))
  
  (defmethod paint-border-bottom ((border border) (object area-mixin) (theme theme-3d))
    (with-theme-3d-colors (n d l vd vl) border theme
      (with-accessors ((left left) (top top)) object
        (with-local-slots ((bw width)) border
          (let (c1 c2)
            (case (style border)
              (:inset (setf c1 vl c2 n))
              ((:outset :default) (setf c1 vd c2 d))
              (:flat (setf c1 vd c2 d)))
            (let ((b (+ top (height object)))
                  (r (+ left (width object)))
                  (hw (/ bw 2)))
              (let ((b1 (- b (/ bw 4)))
                    (b2 (- b (* bw 0.75))))
                (with-blender (+OP-ADD+ +BLEND-SRC-COLOR+ +BLEND-SRC-COLOR+)
                  (al:draw-line left b1 r b1 c1 hw)
                  (al:draw-line (+ left hw) b2 (- r hw) b2 c2 hw)))))))))
  
  (defmethod paint-border-right ((border border) (object area-mixin) (theme theme-3d))
    (with-theme-3d-colors (n d l vd vl) border theme
      (with-accessors ((left left) (top top)) object
        (with-slots ((bw width)) border
          (let (c1 c2)
            (case (style border)
              (:inset (setf c1 vl c2 n))
              ((:outset :default) (setf c1 vd c2 d))
              (:flat (setf c1 vd c2 d)))
            (let ((r (+ left (width object)))
                  (b (+ top (height object)))
                  (hw (/ bw 2)))
              (let ((r1 (- r (/ bw 4)))
                    (r2 (- r (* bw 0.75))))
                (with-blender (+OP-ADD+ +BLEND-SRC-COLOR+ +BLEND-SRC-COLOR+)
                  (al:draw-line r1 top r1 (- b hw) c1 hw)
                  (al:draw-line r2 (+ top hw) r2 (- b hw) c2 hw))))))))))

