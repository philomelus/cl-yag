(in-package #:clg)

(defgeneric area-bottom (obj &key &allow-other-keys))

(defgeneric area-right (obj &key &allow-other-keys))

(defgeneric connect (name obj mgr func &key &allow-other-keys))

(defgeneric disconnect (name obj mgr func &key &allow-other-keys))

(defgeneric disconnect-all (mgr &key &allow-other-keys))

(defgeneric hide (obj &key &allow-other-keys))

(defgeneric manage (obj mgr &key &allow-other-keys))

(defgeneric on-mouse-click (obj x y b &key &allow-other-keys))

(defgeneric on-mouse-down (obj x y b &key &allow-other-keys))

(defgeneric on-mouse-enter (obj x y &key &allow-other-keys))

(defgeneric on-mouse-exit (obj x y &key &allow-other-keys))

(defgeneric on-mouse-move (obj x y dx dy &key &allow-other-keys))

(defgeneric on-mouse-up (obj x y b &key &allow-other-keys))

(defgeneric on-paint (obj &key &allow-other-keys))

(defgeneric on-resize (obj x y w h &key &allow-other-keys))

(defgeneric paint (obj &key &allow-other-keys))

(defgeneric process-events (queue object &key unhandled-event-proc &allow-other-keys))

(defgeneric ready (obj &key manager parent &allow-other-keys))

(defgeneric show (obj &key &allow-other-keys))

(defgeneric unmanage (obj mgr &key &allow-other-keys))

(defgeneric update (obj &key &allow-other-keys))

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
