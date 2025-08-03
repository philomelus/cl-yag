(in-package #:cl-yag)

(defgeneric bottom (obj &key &allow-other-keys))

(defgeneric container-calc-child-height (obj container &key &allow-other-keys))

(defgeneric container-calc-child-left (obj container &key &allow-other-keys))

(defgeneric container-calc-child-top (obj container &key &allow-other-keys))

(defgeneric container-calc-child-width (obj container &key &allow-other-keys))

(defgeneric on-char (key mods object &key &allow-other-keys))

(defgeneric on-mouse-down (x y b o &key &allow-other-keys))

(defgeneric on-mouse-click (x y b o &key &allow-other-keys))

(defgeneric on-mouse-move (x y dx dy o &key &allow-other-keys))

(defgeneric on-mouse-up (x y b o &key &allow-other-keys))

(defgeneric on-paint (obj &key &allow-other-keys))

(defgeneric on-resize (obj x y w h &key &allow-other-keys))

(defgeneric owner (obj))

(defgeneric paint (obj &key &allow-other-keys))

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
