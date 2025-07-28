(in-package #:clg)

;;;; functionality ------------------------------------------------------------

(defgeneric clear-connections (mgr &key &allow-other-keys))

(defgeneric connect (name obj mgr func &key &allow-other-keys))

(defgeneric disconnect (name obj mgr func &key &allow-other-keys))

(defgeneric hide (obj &key &allow-other-keys))

(defgeneric layout (obj mgr &key parent &allow-other-keys)
  (:method (obj mgr &key)))

(defgeneric manage (obj mgr &key &allow-other-keys))

(defgeneric paint (obj &key &allow-other-keys))

(defgeneric process-events (queue object &key unhandled-event-proc &allow-other-keys))

(defgeneric ready (obj &key manager parent &allow-other-keys)
  (:method (obj &key)))

(defgeneric show (obj &key &allow-other-keys))

(defgeneric unmanage (obj mgr &key &allow-other-keys))

(defgeneric update (obj &key &allow-other-keys))

;;;; events -------------------------------------------------------------------

(defgeneric on-mouse-click (obj x y b &key &allow-other-keys))

(defgeneric on-mouse-down (obj x y b &key &allow-other-keys))

(defgeneric on-mouse-enter (obj x y &key &allow-other-keys))

(defgeneric on-mouse-exit (obj x y &key &allow-other-keys))

(defgeneric on-mouse-move (obj x y dx dy &key &allow-other-keys))

(defgeneric on-mouse-up (obj x y b &key &allow-other-keys))

(defgeneric on-paint (obj &key &allow-other-keys))

(defgeneric on-resize (obj x y w h &key &allow-other-keys))

