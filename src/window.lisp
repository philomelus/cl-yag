(in-package #:cl-yag)

;;;; window ===================================================================

;;; theme-mixin -----------------------------------------------------

(defclass window-theme-mixin (color-fore-back-mixin)
  ((interior-color :initarg :interior-color :initform nil :accessor interior-color)))

(defmethod print-mixin ((object window-theme-mixin) stream)
  (pprint-color-nil interior-color object stream)
  (my-next-method))

;;; window ----------------------------------------------------------

(defclass window (window-theme-mixin
                  area-mixin
                  border-mixin
                  content-mixin
                  parent-mixin
                  padding-mixin)
  ())

(defmacro defwindow (l top w h &rest rest &key &allow-other-keys)
  `(make-instance 'window :left ,l :top ,top :width ,w :height ,h ,@rest))

;; (defmethod print-object ((o window) s)
;;   (pprint-indent :current 0 s)
;;   (pprint-logical-block (s nil)
;;     (format s "(defwindow ")
;;     (print-mixin o s)
;;     (format s ")")))

;;; methods ---------------------------------------------------------

(defmethod on-char (key mods (obj window) &key)
  (dolist (child (content obj))
    (on-char key mods child))
  (my-next-method))

(defmethod on-mouse-down (x y b (obj window) &key)
  (dolist (child (content obj))
    (if (on-mouse-down x y b child)
        (return-from on-mouse-down t)))
  nil)

(defmethod on-mouse-move (x y dx dy (obj window) &key)
  (dolist (child (content obj))
    (if (on-mouse-move x y dx dy child)
        (return t)))
  nil)

(defmethod on-mouse-up (x y b (obj window) &key)
  (dolist (child (content obj))
    (if (on-mouse-up x y b child)
        (return t)))
  nil)

(defmethod on-paint ((obj window) &key)
  (with-slots (content left top) obj
    
    ;; Fill interior if desired
    (let ((ic (interior-color obj)))
      (when (eql ic nil)
        (setq ic (interior-color (find-theme obj))))
      (with-border-area (l t_ r b) obj
        (al:draw-filled-rectangle l t_ r b ic)))
    
    ;; Draw border
    (paint-border obj (find-theme obj))
    
    ;; Let children paint
    (let ((children content))
      (dolist (c children)
        (on-paint c))))

  (my-next-method))

(defmethod (setf theme) ((theme window-theme-mixin) (object window))
  (with-slots ((i interior-color) (fc fore-color) (bc back-color))
      theme
    (setf (interior-color object) i)
    (setf (back-color object) bc)
    (setf (fore-color object) fc)))
