(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; window ===================================================================

;;; theme-mixin -----------------------------------------------------

(defclass window-theme-mixin (back-fore-color-mixin)
  ((interior-color :initarg :interior-color :initform nil :accessor interior-color)))

(defmethod print-mixin ((object window-theme-mixin) &optional stream)
  (declare (ignore stream))
  ;; (pprint-color-nil interior-color object stream)
  (my-next-method))

;;; window ----------------------------------------------------------

(defclass window (window-theme-mixin
                  area-mixin
                  border-mixin
                  content-mixin
                  parent-mixin
                  padding-mixin
                  spacing-mixin)
  ())

(defmacro defwindow (l top w h &rest rest &key &allow-other-keys)
  `(make-instance 'window :left ,l :top ,top :width ,w :height ,h ,@rest))

;;; methods ---------------------------------------------------------

(defmethod on-char (key mods (obj window) &key)
  (dolist (child (content obj))
    (unless (eql child nil)
      (when (on-char key mods child)
        (return-from on-char t))))
  (my-next-method))

(defmethod on-mouse-down (x y b (obj window) &key)
  (dolist (child (content obj))
    (unless (eql child nil)
      (if (on-mouse-down x y b child)
          (return-from on-mouse-down t))))
  nil)

(defmethod on-mouse-move (x y dx dy (obj window) &key)
  (dolist (child (content obj))
    (unless (eql child nil)
      (if (on-mouse-move x y dx dy child)
          (return t))))
  nil)

(defmethod on-mouse-up (x y b (obj window) &key)
  (dolist (child (content obj))
    (unless (eql child nil)
      (if (on-mouse-up x y b child)
          (return t))))
  nil)

(defmethod on-paint ((obj window) &key)
  ;; Fill interior if desired
  (with-object-or-theme ((ic interior-color)) obj
    (with-area-and-spacing (le to ri bo) obj
      (al:draw-filled-rectangle le to ri bo ic)))
    
  ;; Draw border
  (paint-border obj (find-theme obj))

  ;; Paint children
  (with-slots (content) obj
    (let ((children content))
      (dolist (c children)
        (unless (eql c nil)
          (on-paint c)))))

  (my-next-method))

