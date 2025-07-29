(in-package #:clg)

;;;; text =====================================================================

(defclass text (area-mixin
                color-mixin
                font-mixin
                h-align-mixin
                parent-mixin
                title-mixin
                v-align-mixin)
  ())

;;; methods ---------------------------------------------------------

(defmethod layout ((obj text) mgr &key parent)
  (declare (ignore mgr parent))
  (next-method))

(defmethod on-paint ((obj text) &key)
  (al:draw-text (font obj) (color obj) (text-calc-left obj) (area-top obj) 0 (title obj))
  (next-method))

(defmethod ready ((obj text) &key manager parent)
  (declare (ignore manager parent))
  (next-method))

(defun text-calc-left (obj)
  (let ((al (area-left obj)))
   (case (h-align obj)
     (:center
      (let ((tw (al:get-text-width (font obj) (title obj)))
            (aw (area-width obj)))
        ;; Does text fit?
        (if (> (- aw tw) 1)
            (+ (truncate (/ (- aw tw) 2)) al)
            al)))
     (:right
      (let ((tw (al:get-text-width (font obj) (title obj)))
            (aw (area-width obj)))
        (if (> (- aw tw) 0)
            (+ al (- aw tw))
            al)))
     ((:none :left)
      al))))

;;;; active-text ==============================================================

(defclass active-text (text)
  ((inside :initform nil :type boolean :accessor active-text-inside)))

;;; methods ---------------------------------------------------------

(defmethod layout ((obj active-text) mgr &key parent)
  (declare (ignore mgr parent))
  (next-method))

(defmethod on-mouse-enter ((obj active-text) x y &key)
  ;; Repaint with hilite of some sort
  (next-method))

(defmethod on-mouse-exit ((obj active-text) x y &key)
  ;; Repaint normally
  (next-method))

(defmethod on-paint ((obj active-text) &key)
  (al:draw-text (font obj) (if (active-text-inside obj)
                               (color-inverse (color obj))
                               (color obj))
                (text-calc-left obj) (area-top obj) 0 (title obj))
  (next-method))

(defmethod ready ((obj active-text) &key manager parent)
  (declare (ignore manager parent))
  (next-method))

