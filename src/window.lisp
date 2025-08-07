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


;; (defmethod print-object ((obj window) stream)
;;   (format stream "'window ~a ~a ~a ~a ~a" (dump-area-mixin obj nil) (dump-border-mixin obj nil)
;;           (dump-color-fore-back-mixin obj nil) (dump-content-mixin obj nil) (dump-parent-mixin obj nil)))

;;; methods ---------------------------------------------------------

(defmethod on-mouse-down (x y b (obj window) &key)
  (dolist (child (content obj))
    ;; (format *standard-output* "~&on-mouse-move: window: passing to ~a" child)
    (on-mouse-down x y b child)))

(defmethod on-mouse-move (x y dx dy (obj window) &key)
  (dolist (child (content obj))
    ;; (format *standard-output* "~&on-mouse-move: window: passing to ~a" child)
    (on-mouse-move x y dx dy child)))

(defmethod on-mouse-up (x y b (obj window) &key)
  (dolist (child (content obj))
    ;; (format *standard-output* "~&on-mouse-move: window: passing to ~a" child)
    (on-mouse-up x y b child)))

(defmethod on-paint ((obj window) &key)
  ;; Clear background
  (al:draw-filled-rectangle (left obj) (top obj) (right obj) (bottom obj) (back-color obj))

  ;; Draw border
  (let ((x (left obj))
        (y (top obj)))
    (let ((r (+ x (width obj) -1))
          (b (+ y (height obj) -1)))
      ;; Left side
      (if (slot-boundp obj 'border-left)
          (let ((bo (border-left obj)))
            (case (style bo)
              (:default
               (al:draw-line x y x b (color bo) (width bo))))))
      ;; Top side
      (if (slot-boundp obj 'border-top)
          (let ((bo (border-top obj)))
            (case (style bo)
              (:default
               (al:draw-line x y r y (color bo) (width bo))))))
      ;; Right side
      (if (slot-boundp obj 'border-right)
          (let ((bo (border-right obj)))
            (case (style bo)
              (:default
               (al:draw-line r y r b (color bo) (width bo))))))
      ;; Bottom side
      (if (slot-boundp obj 'border-bottom)
          (let ((bo (border-bottom obj)))
            (case (style bo)
              (:default
               (al:draw-line x b r b (color bo) (width bo))))))))

  ;; Let children paint themselves
  (let ((children (content obj)))
    (dolist (c children)
      (progn
        (on-paint c))))
  (next-method))

