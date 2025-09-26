(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; macros ==================================================================

(defmacro with-theme-3d-colors ((n d l vd vl) theme &body body)
  (a:with-gensyms (instance)
    `(let ((,instance ,theme))
       (let ((,n (normal-color ,instance))
             (,d (dark-color ,instance))
             (,l (light-color ,instance))
             (,vd (very-dark-color ,instance))
             (,vl (very-light-color ,instance)))
         (declare (ignorable ,n ,d ,l ,vd ,vl))
         (assert (not (eql ,n nil)))
         (assert (not (eql ,d nil)))
         (assert (not (eql ,l nil)))
         (assert (not (eql ,vd nil)))
         (assert (not (eql ,vl nil)))
         ,@body))))

;;;; forward declaration =====================================================

(defvar *theme-default*)

;;;; theme-base ===============================================================

(defclass theme-base (back-color-mixin
                      fore-color-mixin)
  ())

(defmethod paint-border ((object border-mixin) (theme theme-base))
  (with-borders (bl br bt bb) object
    (let ((blp (not (eql bl nil)))
          (brp (not (eql br nil)))
          (btp (not (eql bt nil)))
          (bbp (not (eql bb nil))))
      (when blp
        (paint-border-left bl object theme :blend-top btp :blend-bottom bbp))
      (when btp
        (paint-border-top bt object theme :blend-left blp :blend-right brp))
      (when brp
        (paint-border-right br object theme :blend-top btp :blend-bottom bbp))
      (when bbp
        (paint-border-bottom bb object theme :blend-left blp :blend-right brp)))))

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

(defmethod paint-border-bottom ((border border) object (theme theme-3d) &key blend-left blend-right)
  (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors theme)
    (declare (ignorable lto lti rbo rbi))
    (with-area-and-spacing (object-left object-top object-right object-bottom) object
      (assert (< object-left object-right))
      (assert (< object-top object-bottom))
      (let ((thickness (thickness border)))
        (let ((1/4-thick (/ thickness 4))
              (1/2-thick (/ thickness 2))
              (others ()))
          (when blend-left
            (push :left others))
          (when blend-right
            (push :right others))
          (let ((3/4-thick (+ 1/2-thick 1/4-thick)))
            (draw-side :bottom
                       (+ object-left 3/4-thick) (- object-top 1/4-thick)
                       (- object-right 3/4-thick) (- object-bottom 1/4-thick)
                       1/2-thick rbo :others others)
            (draw-side :bottom
                       (+ object-left 3/4-thick) (- object-top 3/4-thick)
                       (- object-right 3/4-thick) (- object-bottom 3/4-thick)
                       1/2-thick rbi :others others :inside t)))))))

(defmethod paint-border-bottom ((border border) object (theme theme-flat) &key blend-left blend-right)
  (declare (ignorable blend-left blend-right))
  (let ((color (color theme)))
    (with-accessors ((tn thickness)) border
      (when (> tn 0)
        (with-area-and-spacing (asl ast asr asb) object
          (let ((c color)
                (w2 (/ tn 2)))
            (if (eql c nil)
                (setq c (frame-color theme)))
            (let ((yy (+ asb (- tn) w2)))
              (al:draw-line asl yy asr yy c tn))))))))

(defmethod paint-border-left ((border border) object (theme theme-3d) &key blend-top blend-bottom)
  (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors theme)
    (declare (ignorable lto lti rbo rbi))
    (with-area-and-spacing (object-left object-top object-right object-bottom) object
      (let ((thickness (thickness border)))
        (when (> thickness 0)
          (let ((1/4-thick (/ thickness 4))
                (1/2-thick (/ thickness 2))
                (others ()))
            (when blend-top
              (push :top others))
            (when blend-bottom
              (push :bottom others))
            (let ((3/4-thick (+ 1/4-thick 1/2-thick)))
              (draw-side :left
                         (+ object-left 1/4-thick) (+ object-top 1/4-thick)
                         (+ object-right 1/4-thick) (- object-bottom 1/4-thick) 
                         1/2-thick lto :others others)
              (draw-side :left
                         (+ object-left 3/4-thick) (+ object-top 3/4-thick)
                         (+ object-right 3/4-thick) (- object-bottom 3/4-thick)
                         1/2-thick lti :others others :inside t))))))))

(defmethod paint-border-left ((border border) object (theme theme-flat) &key blend-top blend-bottom)
  (declare (ignorable blend-top blend-bottom))
  (let ((color (color theme)))
    (with-accessors ((tn thickness)) border
      (when (> tn 0)
        (with-area-and-spacing (asl ast asr asb) object
          (let ((c color)
                (w tn)
                (w2 (/ tn 2)))
            (if (eql c nil)
                (setq c (frame-color theme)))
            (let ((xx (+ asl w2)))
              (assert (not (eql c nil)))
              (al:draw-line xx ast xx asb c w))))))))

(defmethod paint-border-right ((border border) object (theme theme-3d) &key blend-top blend-bottom)
  (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors theme)
    (declare (ignorable lto lti rbo rbi))
    (with-area-and-spacing (object-left object-top object-right object-bottom) object
      (let ((thickness (thickness border)))
        (when (> thickness 0)
          (let ((1/4-thick (/ thickness 4))
                (1/2-thick (/ thickness 2))
                (others ()))
            (when blend-top
              (push :top others))
            (when blend-bottom
              (push :bottom others))
            (let ((3/4-thick (+ 1/2-thick 1/4-thick)))
              (draw-side :right (- object-left 1/4-thick) (+ object-top 3/4-thick)
                         (- object-right 1/4-thick) (- object-bottom 1/4-thick)
                         1/2-thick rbo :others others)
              (draw-side :right (- object-left 3/4-thick) (+ object-top 3/4-thick)
                         (- object-right 3/4-thick) (- object-bottom 3/4-thick)
                         1/2-thick rbi :others others :inside t))))))))

(defmethod paint-border-right ((border border) object (theme theme-flat) &key blend-top blend-bottom)
  (declare (ignorable blend-top blend-bottom))
  (let ((color (color theme)))
    (with-accessors ((tn thickness)) border
      (when (> tn 0)
        (with-area-and-spacing (asl ast asr asb) object
          (let ((c color)
                (w2 (/ tn 2)))
            (if (eql c nil)
                (setq c (frame-color theme)))
            (let ((xx (+ asr (- tn) w2)))
              (al:draw-line xx ast xx asb c tn))))))))

(defmethod paint-border-top ((border border) object (theme theme-3d) &key blend-left blend-right)
  (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors theme)
    (declare (ignorable lto lti rbo rbi))
    (with-area-and-spacing (object-left object-top object-right object-bottom) object
      (let ((thickness (thickness border)))
        (when (> thickness 0)
          (let ((1/4-thick (/ thickness 4))
                (1/2-thick (/ thickness 2))
                (others ()))
            (when blend-left
              (push :left others))
            (when blend-right
              (push :right others))
            (let ((3/4-thick (+ 1/4-thick 1/2-thick)))
              (draw-side :top (+ object-left 1/4-thick) (+ object-top 1/4-thick)
                         object-right (+ object-bottom 1/4-thick)
                         1/2-thick lto :others others)
              (draw-side :top (+ object-left 3/4-thick) (+ object-top 3/4-thick)
                         object-right (+ object-bottom 3/4-thick)
                         1/2-thick lti :others others :inside t))))))))

(defmethod paint-border-top ((border border) object (theme theme-flat) &key blend-left blend-right)
  (declare (ignorable blend-left blend-right))
  (let ((color (color theme)))
    (with-accessors ((tn thickness)) border
      (when (> tn 0)
        (with-area-and-spacing (asl ast asr asb) object
          (let ((c color)
                (w2 (/ tn 2)))
            (if (eql c nil)
                (setq c (frame-color theme)))
            (let ((yy (+ ast w2)))
              (al:draw-line asl yy asr yy c tn))))))))

;;;; functions ================================================================

(defun theme-3d-style-colors (theme)
  "Return 3d drawing colors from 3d drawing style (left-top-outside
left-top-inside right-bottom-outside right-bottom-inside)."
  
  (with-theme-3d-colors (normal dark light very-dark very-light) theme
    (assert (not (eql normal nil)))
    (assert (not (eql dark nil)))
    (assert (not (eql light nil)))
    (assert (not (eql very-dark nil)))
    (assert (not (eql very-light nil)))
    (assert (member (style theme) '(:inset :outset :default :flat)))
    (case (style theme)
      (:inset
       (values very-dark dark light very-light))
      ((:outset :default)
       (values light very-light very-dark dark))
      (:flat
       (values very-dark dark very-dark dark)))))

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

