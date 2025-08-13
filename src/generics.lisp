(in-package #:cl-yag)

;;;; generics =================================================================

(defgeneric between (lo hi))

(defgeneric bottom (obj &key &allow-other-keys))

(defgeneric container-calc-child-height (obj container &key &allow-other-keys))

(defgeneric container-calc-child-left (obj container &key &allow-other-keys))

(defgeneric container-calc-child-top (obj container &key &allow-other-keys))

(defgeneric container-calc-child-width (obj container &key &allow-other-keys))

(defgeneric must-init (test desc))

(defgeneric on-char (key mods object &key &allow-other-keys))

(defgeneric on-mouse-click (x y b o &key &allow-other-keys))

(defgeneric on-mouse-down (x y b o &key &allow-other-keys))

(defgeneric on-mouse-down-accept (o m));

(defgeneric on-mouse-move (x y dx dy o &key &allow-other-keys))

(defgeneric on-mouse-up (x y b o &key &allow-other-keys))

(defgeneric on-paint (obj &key &allow-other-keys))

(defgeneric on-resize (obj x y w h &key &allow-other-keys))

(defgeneric on-timer (timer count object))

(defgeneric owner (obj))

(defgeneric paint (obj &key &allow-other-keys)
  (:method (obj &key)
    (on-paint obj)))

(defgeneric paint-border (object theme))

(defgeneric print-mixin (object stream))

(defgeneric process-events (queue object &key &allow-other-keys))

(defgeneric right (obj &key &allow-other-keys))

(defgeneric unhandled-event (event object) (:method (e o)))

(defgeneric within (x y obj &key &allow-other-keys))

;;;; setf =====================================================================

(defgeneric (setf area) (x y w h object))

(defgeneric (setf border) (border-object object))

(defgeneric (setf border-h) (border-object object))

(defgeneric (setf border-v) (border-object object))

(defgeneric (setf location) (x y object))

(defgeneric (setf padding) (value object))

(defgeneric (setf padding-h) (value object))

(defgeneric (setf padding-v) (value object))

(defgeneric (setf spacing) (value object))

(defgeneric (setf spacing-h) (value object))

(defgeneric (setf spacing-v) (value object))

(defgeneric (setf theme) (theme object)
  (:method (theme object)
    (v:warn :theme "no (setf theme) method for ~a" (print-raw-object object))))
