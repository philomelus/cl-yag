(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; generics =================================================================

(defgeneric bottom (object))

(defgeneric calc-area (object parent &key &allow-other-keys))

(defgeneric calc-border-left (side area object)
  (:documentation "Calculate the left position of border of object.

side   = member (:left :right :top :bottom)
area   = (left top width height) allocated to object within parent
object = widget calculating left position for"))

(defgeneric calc-border-top (side area object)
  (:documentation "Calculate the top position of border of object.

side   = member (:left :right :top :bottom)
area   = (left top width height) allocated to object within parent
object = widget calculating top position for"))

(defgeneric calc-height (type area object)
  (:documentation "Calculate height of object.

type = widget specific positioning option
area = (left top width height) within parent
object = object doing height calculation for"))

(defgeneric calc-layout-child-areas (object)
  (:documentation "Called when layout first needs to know area of its children."))

(defgeneric calc-left (type area object)
  (:documentation "Calculate the left position of object.

type = widget specific positioning option
area = (left top width height) within parent
object = object doing left calculation for

At the point this is called, calculated height and width of object have
already been calcuated and set to object."))

(defgeneric calc-top (type area object)
  (:documentation "Calculate the top position of object.

type = widget specific positioning option
area = (left top width height) within parent
object = object doing top calculation for

At the point this is called, calculated height and width of object have
already been calcuated and set to object."))

(defgeneric calc-width (type area object)
  (:documentation "Calculate width of object.

type = widget specific positioning option
area = (left top width height) within parent
object = object doing width calculation for"))

(defgeneric must-init (test desc))

(defgeneric on-char (key mods object &key &allow-other-keys)
  (:method (key mods object &Key)
    ;; Should never trigger
    (my-next-method)))

(defgeneric on-command (o &key &allow-other-keys))

(defgeneric on-key-down (key mod obj &key &allow-other-keys)
  (:method (key mod obj &key)
    ;; Should never trigger
    (my-next-method)))

(defgeneric on-key-up (key mod obj &key &allow-other-keys)
  (:method (key mod obj &key)
    ;; Should never trigger
    (my-next-method)))

(defgeneric on-mouse-down (x y b o &key &allow-other-keys))

(defgeneric on-mouse-down-accept (o m));

(defgeneric on-mouse-move (x y dx dy o &key &allow-other-keys))

(defgeneric on-mouse-up (x y b o &key &allow-other-keys))

(defgeneric on-paint (obj &key &allow-other-keys))

(defgeneric on-resize (obj x y w h &key &allow-other-keys)
  (:method (obj x y w h &key)
    (my-next-method)))

(defgeneric on-timer (timer count object &key &allow-other-keys)
  (:method (timer count object &key)
    (my-next-method)))

(defgeneric owner (obj))

(defgeneric paint (obj &key &allow-other-keys)
  (:method (obj &key)
    (on-paint obj)
    ;; Should never trigger
    (my-next-method)))

(defgeneric paint-border (object theme))
(defgeneric paint-border-bottom (border object theme))
(defgeneric paint-border-left (border object theme))
(defgeneric paint-border-right (border object theme))
(defgeneric paint-border-top (border object theme))

(defgeneric paint-box (box theme)
  (:documentation "Called to paint a box. Unless custom drawing is desired, just call
PAINT-BOX-FRAME, PAINT-BOX-INTERIOR, and PAINT-BOX-TITLE.

theme - Theme used for painting box."))

(defgeneric paint-box-frame (box theme)
  (:documentation "Called to paint the frame of a box.

theme  - Theme used to paint box frame."))

(defgeneric paint-box-interior (box theme)
  (:documentation "Called to paint the interior of a box.

theme - Theme used to paint box interior."))

(defgeneric paint-box-title (box theme)
  (:documentation "Called to paint the title of a box.

theme - Theme used to paint box title."))

(defgeneric print-mixin (object &optional stream))

(defgeneric process-events (queue object &key &allow-other-keys))

(defgeneric right (object))

(defgeneric unhandled-event (event object) (:method (e o)))

;; (defgeneric update-area-cache (object)
;;   (:documentation "Called to update the area cache of object. This is called for all derived
;; types of area-cache's. Should return T/NIL for whether the cache update
;; succeeded."))

(defgeneric update-layout-child-areas (index object)
  (:documentation "After a child's area has been modified, call this to update the internal child
areas as well as redistribute newly available area to siblings.

index  = Index of modified child
object = Object containing modified child"))

(defgeneric within (x y obj &key &allow-other-keys))

;;;; setf =====================================================================

(defgeneric (setf area) (x y w h object))

(defgeneric (setf border) (border-object object)
  (:documentation "Sets BORDER-LEFT, BORDER-RIGHT, BORDER-TOP, and BORDER-BOTTOM of OBJECT to
BORDER-OBJECT."))

(defgeneric (setf border-h) (border-object object)
  (:documentation "Sets BORDER-LEFT and BORDER-RIGHT of OBJECT to BORDER-OBJECT."))

(defgeneric (setf border-v) (border-object object)
  (:documentation "Sets BORDER-ROP and BORDER-BOTTOM of OBJECT to BORDER-OBJECT."))

(defgeneric (setf padding) (value object))

(defgeneric (setf padding-h) (value object))

(defgeneric (setf padding-v) (value object))

(defgeneric (setf spacing) (value object))

(defgeneric (setf spacing-h) (value object))

(defgeneric (setf spacing-v) (value object))

