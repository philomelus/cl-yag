(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; WINDOW ===================================================================

(defclass window (area-mixin
                  border-mixin
                  content-mixin
                  parent-mixin
                  padding-mixin
                  spacing-mixin
                  theme-mixin)
  ((filled :type boolean :initarg :filled :initform t :accessor filled
           :documentation "When T will fill interior of window with INTERIOR-COLOR before drawing
BORDER's or children.")))

(defmacro defwindow (l top w h &rest rest &key &allow-other-keys)
  `(make-instance 'window :left ,l :top ,top :width ,w :height ,h ,@rest))

;;; methods ---------------------------------------------------------

(defmethod default-theme-type ((object window))
  'window)

(defmethod initialize-instance :before ((object window) &key)
  "Add WINDOW theme data to *THEME-ALL-DATA*."

  (unless (theme-value-defaultp 'window nil 'interior-color)
    (let ((ic (get-theme-value-default nil nil 'interior-color)))
      (set-theme-value-default 'window nil 'interior-color ic)
      (set-theme-value-default 'window '3d 'interior-color ic)
      (set-theme-value-default 'window 'flat 'interior-color ic))))

(defmethod on-char (key mods (obj window) &key)
  (dolist (child (content obj))
    (unless (eql (a:ensure-car child) nil)
      (when (on-char key mods (a:ensure-car child))
        (return-from on-char t))))
  (my-next-method))

(defmethod on-mouse-down (x y b (obj window) &key)
  (dolist (child (content obj))
    (unless (eql (a:ensure-car child) nil)
      (if (on-mouse-down x y b (a:ensure-car child))
          (return-from on-mouse-down t))))
  (my-next-method))

(defmethod on-mouse-move (x y dx dy (obj window) &key)
  (dolist (child (content obj))
    (unless (eql (a:ensure-car child) nil)
      (if (on-mouse-move x y dx dy (a:ensure-car child))
          (return t))))
  (my-next-method))

(defmethod on-mouse-up (x y b (obj window) &key)
  (dolist (child (content obj))
    (unless (eql (a:ensure-car child) nil)
      (if (on-mouse-up x y b (a:ensure-car child))
          (return t))))
  (my-next-method))

(defmethod on-paint ((object window) &key)
  ;; Fill interior if desired
  (when (slot-value object 'filled)
    (let ((wic (get-theme-value object 'window 'interior-color :style nil)))
      (with-area-and-spacing (left-interior top-interior right-interior bottom-interior)
                             object
        (al:draw-filled-rectangle left-interior top-interior right-interior bottom-interior wic))))
    
  ;; Draw border
  (paint-border object)

  ;; Paint children
  (with-slots (content) object
    (dolist (child (slot-value object 'content))
      (let ((child-object (a:ensure-car child)))
        (unless (eql child-object nil)
          (on-paint child-object)))))

  (my-next-method))

