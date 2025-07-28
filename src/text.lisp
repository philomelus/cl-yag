(in-package #:clg)

;;;; text =====================================================================

(defclass text (area-mixin
                color-mixin
                font-mixin
                h-align-mixin
                v-align-mixin)
  ((title :initarg :title :initform "" :type string :accessor text-title)))

(defun print-color (color stream)
  (format stream "R: ~d, G: ~d, B: ~d, A: ~d"
          (clg-utils:color-r color)
          (clg-utils:color-g color)
          (clg-utils:color-b color)
          (clg-utils:color-a color)))

(defun print-text (obj stream &optional (title "text"))
  (format stream "~&~a: ~a~&  title: ~s~&  color: ~a~&  area: ~a~&  font: ~a~&  h-align: ~a~&  v-align: ~a"
          title obj (text-title obj) (print-color (color obj) nil)
          (list (area-left obj) (area-top obj) (area-height obj) (area-width obj))
          (font obj) (h-align obj) (v-align obj)))

(defmethod layout ((obj text) mgr &key parent)
  (declare (ignore mgr parent))
  (next-method))

(defmethod on-paint ((obj text) &key)
  (al:draw-text (font obj) (color obj) (area-left obj) (area-top obj) 0 (text-title obj))
  (next-method))

(defmethod ready ((obj text) &key manager parent)
  (declare (ignore manager parent))
  (next-method))

;;;; active-text ==============================================================

(defclass active-text (text)
  ((inside :initform nil :type boolean :accessor active-text-inside)))

(defun print-active-text (obj stream)
  (print-text obj stream "active-text"))

(defmethod (setf text-title) :after (newval (obj active-text))
  (if (and (not (cffi:null-pointer-p (font obj)))
           (not (eq nil newval))
           (> (length newval) 0)
           (member (h-align obj) (list :left :right :center)))
      (progn
        (case (h-align obj)
          (:left
           (setf (area-width obj) (al:get-text-width (font obj) (text-title obj))))
          (:right)
          (:center)))
      )
  )

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
                (area-left obj) (area-top obj) 0 (text-title obj))
  (next-method))

(defmethod ready ((obj active-text) &key manager parent)
  (declare (ignore manager parent))
  (next-method))

