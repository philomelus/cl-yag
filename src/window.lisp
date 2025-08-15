(in-package #:cl-yag)

;;;; window ===================================================================

(defclass window (area-mixin
                  border-mixin
                  color-fore-back-mixin
                  content-mixin
                  parent-mixin)
  ())

(defmacro defwindow (l top w h content &rest rest &key &allow-other-keys)
  `(make-instance 'window :left ,l :top ,top :width ,w :height ,h :content (list ,@content) ,@rest))

(defmethod print-object ((o window) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "(defwindow ")
    (print-mixin o s)
    (format s ")")))

;;; methods ---------------------------------------------------------

(defmethod on-char (key mods (obj window) &key)
  (dolist (child (content obj))
    (on-char key mods child)))

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
  ;; Clear background
  (al:draw-filled-rectangle (left obj) (top obj) (right obj) (bottom obj) (back-color obj))

  ;; Draw border
  (let ((th (find-theme obj)))
    (assert (not (eql nil th)))
    (paint-border obj th))

  ;; Let children paint themselves
  (let ((children (content obj)))
    (dolist (c children)
      (progn
        (on-paint c))))
  (my-next-method))

