(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; generics =================================================================

(defgeneric bottom (object))

(defgeneric calc-area (object &key &allow-other-keys)
  (:documentation "Called to calculate child areas. Typically called from LEFT, TOP, WIDTH, or
HEIGHT when called first time for a widget with auto-calculated positions.

Its acceptable to call directly to pre-calculate the layout before display if
desired. There must be at some point in the hierarchy of objects an object
with actual numerical LEFT, TOP, WIDTH, and HEIGHT. It is not possible to
calculate the rest of the hierarchy without a starting area.

Calculations are done for all immediate children of a layout. If there are
child layouts of the layout being calculated, those layouts will be completed
after their parent layout (the current one) is complete.

Options are calculated in 2 steps: First the MIN/MAX WIDTH/HEIGHT options are
completed to determine area of each child, then second comes the horizontal
and vertical options, in no particular order (possibly at the same time when
multiprocessing).

Seting LEFT, TOP, WIDTH, or HEIGHT to an actual numerical value disables all
calculations for that coordinate only. Any that do not contain a numerical
value will still be part of the layout calculations, and will use the
specified values for the ones set to numericqal values (for example, if you
set WIDTH but leave LEFT, TOP, and HEIGHT as AUTO, then the width will be used
to calculate the LEFT coordinate as if it had been calculated internally).
Setting a numerical value will override any layout options such as MIN-WIDTH,
etc. for their respective coordinates (so setting WIDTH, then adding MIN-WIDTH
to layout options, will cause the MIN-WIDTH to be ignored).

*** MIN-WIDTH/MAX-WIDTH OPTIONS ***

- When there are both MIN-WIDTH and MAX-WIDTH in options for different
siblings, any horizontal space released by siblings with MIN-WIDTH will be
allocated to any siblings with MAX-WIDTH, as evenly as possible.

- When there are one or more siblings with MIN-WIDTH, and no siblings with
MAX-WIDTH, then extra horizontal space given away from MIN-WIDTH is allocated
as evenly as possible across all children without MIN-WIDTH.

- When there are one or more siblings with MAX-WIDTH, and no siblings with
MIN-WIDTH, the any extra horizontal space will be allocated to the siblings
with MAX-WIDTH, as evenly as possible.

*** MIN-HEIGHT/MAX-HEIGHT OPTIONS ***

- When there are both MIN-HEIGHT and MAX-HEIGHT in options for different
siblings, any vertical space released by siblings with MIN-HEIGHT will be
allocated to any siblings with MAX-HEIGHT, as evenly as possible.

- When there are one or more siblings with MIN-HEIGHT, and no siblings with
MAX-HEIGHT, then extra vertical space given away from MIN-HEIGHT is allocated
as evenly as possible across all children without MIN-HEIGHT.

- When there are one or more siblings with MAX-HEIGHT, and no siblings with
MIN-HEIGHT, any extra vertical space will be allocated to the siblings with
MAX-HEIGHT, as evenly as possible.

*** LEFT/CENTER/RIGHT OPTIONS ***

Children with a LEFT option will be moved as far left as possible within the
allocated space for the child.

Children with a CENTER option will be moved to the horizontal center of the
space allocated for the child.

Children with a RIGHT option will be moved to the right most horizontal
position within thier allocated space.

*** TOP/MIDDLE/BOTTOM OPTIONS ***

Children with a TOP option will be moved as close to the top as possible
within the allocated space for the child.

Children with a MIDDLE option will be moved to the vertical center of the
space allocated for the child.

Children with a BOTTOM option will be moved to the bottom most vertical
position within thier allocated space.

*** RETURN ***

When this method returns, all siblings will have their LEFT, TOP, WIDTH, and
HEIGHT set to numerical values. It is guaranteed that all horizontal and
vertical space allocated to the layout will be used by the siblings or layout
itself (for example, SPACING may use some space for the layout that won't be
allocated to any children."))

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

type = Widget specific positioning option
area = %rect allocated to widget within parent area
object = object doing height calculation for"))

(defgeneric calc-layout-child-areas (object)
  (:documentation "Called when layout first needs to know area of its children."))

(defgeneric calc-left (type area width height object)
  (:documentation "Calculate the left position of object.

type   = Widget specific positioning option
area   = %rect allocated to widget within parent area
width  = Value returned from calc-width for this object
height = Value returned from calc-height for this object
object = object doing left calculation for"))

(defgeneric calc-top (type area width height object)
  (:documentation "Calculate the top position of object.

type   = Widget specific positioning option
area   = %rect allocated to widget within parent area
width  = Value returned from calc-width for this object
height = Value returned from calc-height for this object
object = object doing top calculation for"))

(defgeneric calc-width (type area object)
  (:documentation "Calculate width of object.

type = Widget specific positioning option
area = %rect allocated to widget within parent area
object = object doing width calculation for"))

(defgeneric layout-changed (layout &key parent child)
  (:documentation "Called to notify a layout that either a parent's or a child's layout has
changed. The layout will then take whatever action is needed to update its own
and its children's layouts.

When a parent is notifying its child layout, CHILD must be T, and NIL
otherwise.

When a child is notifying its parent layout, PARENT must be T, and NIL
otherwise."))

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
    (my-next-method)))

(defgeneric paint-border (object theme)
  (:documentation "Paint borders for object.

Default implementation calls the paint-border-* generics with apropriate
keywords."))

(defgeneric paint-border-bottom (border object theme &key blend-left blend-right)
  (:documentation "Paint bottom border.

BLEND-LEFT is T when the left border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side.

BLEND-RIGHT is T when the right border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side."))

(defgeneric paint-border-left (border object theme &key blend-top blend-bottom)
  (:documentation "Paint left border.

BLEND-TOP is T when the top border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side.

BLEND-BOTTOM is T when the bottom border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side."))

(defgeneric paint-border-right (border object theme &key blend-top blend-bottom)
  (:documentation "Paint right border.

BLEND-TOP is T when the top border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side.

BLEND-BOTTOM is T when the bottom border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side."))

(defgeneric paint-border-top (border object theme &key blend-left blend-right)
  (:documentation "Paint top border.

BLEND-LEFT is T when the left border will also be painted, so perform any
required blending to make it look correct. NIL means to paint the full side.

BLEND-RIGHT is T when the right border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side."))

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

(defgeneric process-events (queue object &key &allow-other-keys))

(defgeneric right (object))

(defgeneric unhandled-event (event object) (:method (e o)))

;; (defgeneric update-area-cache (object)
;;   (:documentation "Called to update the area cache of object. This is called for all derived
;; types of area-cache's. Should return T/NIL for whether the cache update
;; succeeded."))

;; index  = Index of modified child
;; object = Object containing modified child"))

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

