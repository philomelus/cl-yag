(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(declaim (inline paint-border-bottom-3d paint-border-left-3d paint-border-right-3d
                 paint-border-top-3d))

;;;; forward declaration =====================================================

(defvar *theme-default*)

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
                (with-accessors ((tn thickness)) border
                  (assert (> tn 0))
                  (with-accessors ((left left) (top top) (width width) (height height)) object
                    (let ((c color)
                          (w tn)
                          (w2 (truncate (/ tn 2))))
                      (if (eql c nil)
                          (setq c (frame-color theme)))
                      ,@body))))))
  
  (defmethod paint-border-left ((border border) (object %rect) (theme theme-flat))
    (paint-border-side-flat theme
                            (let ((xx (+ left w2)))
                              (assert (not (eql c nil)))
                              (al:draw-line xx top xx (+ top height) c w))))
  
  (defmethod paint-border-left ((border border) (object area-mixin) (theme theme-flat))
    (paint-border-side-flat theme
                            (let ((xx (+ left w2)))
                              (assert (not (eql c nil)))
                              (al:draw-line xx top xx (+ top height) c w))))
  
  (defmethod paint-border-top ((border border) (object %rect) (theme theme-flat))
    (paint-border-side-flat theme
                            (let ((yy (+ top w2)))
                              (al:draw-line left yy (+ left width) yy c w))))
  
  (defmethod paint-border-top ((border border) (object area-mixin) (theme theme-flat))
    (paint-border-side-flat theme
                            (let ((yy (+ top w2)))
                              (al:draw-line left yy (+ left width) yy c w)))))

(macrolet ((paint-border-side-flat (theme &body body)
             `(let ((color (color ,theme)))
                (with-accessors ((tn thickness)) border
                  (assert (> tn 0))
                  (with-accessors ((left left) (top top) (width width) (height height)) object
                    (let ((c color)
                          (w tn)
                          (w2 (truncate (/ tn 2)))
                          (bow tn))
                      (if (eql c nil)
                          (setq c (frame-color theme)))
                      ,@body))))))
  
  (defmethod paint-border-bottom ((border border) (object %rect) (theme theme-flat))
    (paint-border-side-flat theme
                            (let ((yy (+ (+ top (- height bow)) w2)))
                              (al:draw-line left yy (+ left width) yy c w))))  
  
  (defmethod paint-border-bottom ((border border) (object area-mixin) (theme theme-flat))
    (paint-border-side-flat theme
                            (let ((yy (+ (+ top (- height bow)) w2)))
                              (al:draw-line left yy (+ left width) yy c w))))

  (defmethod paint-border-right ((border border) (object %rect) (theme theme-flat))
    (paint-border-side-flat theme
                            (let ((xx (+ (+ left (- width bow)) w2)))
                              (al:draw-line xx top xx (+ top height) c w))))
  
  (defmethod paint-border-right ((border border) (object area-mixin) (theme theme-flat))
    (paint-border-side-flat theme
                            (let ((xx (+ (+ left (- width bow)) w2)))
                              (al:draw-line xx top xx (+ top height) c w)))))

(macrolet ((with-theme-3d-colors
               ((n d l vd vl) theme &body body)
             (let ((theme-var (gensym)))
               `(let ((,theme-var ,theme))
                  (let ((,n (normal-color ,theme-var))
                        (,d (dark-color ,theme-var))
                        (,l (light-color ,theme-var))
                        (,vd (very-dark-color ,theme-var))
                        (,vl (very-light-color ,theme-var)))
                    (assert (not (eql ,n nil)))
                    (assert (not (eql ,d nil)))
                    (assert (not (eql ,l nil)))
                    (assert (not (eql ,vd nil)))
                    (assert (not (eql ,vl nil)))
                    ,@body)))))

  (defmethod paint-border-left ((border border) object (theme theme-3d))
    (with-theme-3d-colors (n d l vd vl) theme
      (with-accessors ((left left) (top top) (width width) (height height)) object
        (with-local-slots (thickness) border
          (paint-border-left-3d n d l vd vl left top width height thickness (border-style theme))))))
  
  (defmethod paint-border-top ((border border) object (theme theme-3d))
    (with-theme-3d-colors (n d l vd vl) theme
      (with-accessors ((left left) (top top) (width width) (height height)) object
        (with-local-slots (thickness) border
          (paint-border-top-3d n d l vd vl left top width height thickness (border-style theme))))))

  (defmethod paint-border-bottom ((border border) (object area-mixin) (theme theme-3d))
    (with-theme-3d-colors (n d l vd vl) theme
      (with-accessors ((left left) (top top) (width width) (height height)) object
        (with-local-slots (thickness) border
          (paint-border-bottom-3d n d l vd vl left top width height thickness (border-style theme))))))
  
  (defmethod paint-border-right ((border border) object (theme theme-3d))
    (with-theme-3d-colors (n d l vd vl) theme
      (with-accessors ((left left) (top top) (width width) (height height)) object
        (with-slots (thickness) border
          (paint-border-right-3d n d l vd vl left top width height thickness (border-style theme)))))))

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

(defun paint-border-bottom-3d (normal dark light very-dark very-light
                               left top width height thickness style)
  (declare (ignorable normal dark light very-dark very-light left top width height thickness style))
  
  (let (c1 c2)
    (case style
      (:inset (setf c1 very-light c2 normal))
      ((:outset :default) (setf c1 very-dark c2 dark))
      (:flat (setf c1 very-dark c2 dark)))
    (let ((b (+ top height))
          (r (+ left width))
          (hw (/ thickness 2)))
      (let ((b1 (- b (/ thickness 4)))
            (b2 (- b (* thickness 0.75))))
        (with-blender (+OP-ADD+ +BLEND-SRC-COLOR+ +BLEND-SRC-COLOR+)
          (al:draw-line left b1 r b1 c1 hw)
          (al:draw-line (+ left hw) b2 (- r hw) b2 c2 hw))))))

(defun paint-border-left-3d (normal dark light very-dark very-light
                             left top width height thickness style)
  (declare (ignorable normal dark light very-dark very-light left top width height thickness style))
  (let (c1 c2)
    (case style
      (:inset (setf c1 very-dark c2 dark))
      ((:outset :default) (setf c1 normal c2 very-light))
      (:flat (setf c1 very-dark c2 dark)))
    (let ((b (+ top height))
          (hw (/ thickness 2)))
      (let ((l1 (+ left (/ thickness 4)))
            (l2 (+ left (* thickness 0.75))))
        (with-blender (+OP-ADD+ +BLEND-SRC-COLOR+ +BLEND-SRC-COLOR+)
          (al:draw-line l1 (+ top hw) l1 (- b hw) c1 hw)
          (al:draw-line l2 (+ top thickness) l2 (- b thickness) c2 hw))))))

(defun paint-border-right-3d (normal dark light very-dark very-light
                              left top width height thickness style)
  (declare (ignorable normal dark light very-dark very-light left top width height thickness style))
  
  (let (c1 c2)
    (case style
      (:inset (setf c1 very-light c2 normal))
      ((:outset :default) (setf c1 very-dark c2 dark))
      (:flat (setf c1 very-dark c2 dark)))
    (let ((r (+ left width))
          (b (+ top height))
          (hw (/ thickness 2)))
      (let ((r1 (- r (/ thickness 4)))
            (r2 (- r (* thickness 0.75))))
        (with-blender (+OP-ADD+ +BLEND-SRC-COLOR+ +BLEND-SRC-COLOR+)
          (al:draw-line r1 top r1 (- b hw) c1 hw)
          (al:draw-line r2 (+ top hw) r2 (- b hw) c2 hw))))))

(defun paint-border-top-3d (normal dark light very-dark very-light
                            left top width height thickness style)
  (declare (ignorable normal dark light very-dark very-light left top width height thickness style))
  
  (let (c1 c2)
    (case style
      (:inset (setf c1 very-dark c2 dark))
      ((:outset :default) (setf c1 normal c2 very-light))
      (:flat (setf c1 very-dark c2 dark)))
    (let ((r (+ left width))
          (hw (/ thickness 2)))
      (let ((t1 (+ top (/ thickness 4)))
            (t2 (+ top (* thickness 0.75))))
        (with-blender (+OP-ADD+ +BLEND-SRC-COLOR+ +BLEND-SRC-COLOR+)
          (al:draw-line left t1 (- r hw) t1 c1 hw)
          (al:draw-line (+ left hw) t2 (- r thickness) t2 c2 hw))))))
