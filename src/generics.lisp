(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

;;;; GENERICS =================================================================

;;; AREA ------------------------------------------------------------

(defgeneric (setf area) (x y w h object))

(defgeneric bottom (object))

(defgeneric right (object))

(defgeneric within (x y obj &key &allow-other-keys))

;;; BORDER ----------------------------------------------------------

(defgeneric (setf border) (border-object object)
  (:documentation "Sets all borders of OBJECT to BORDER-OBJECT."))

(defgeneric (setf border-h) (border-object object)
  (:documentation "Sets horizontal borders of OBJECT to BORDER-OBJECT."))

(defgeneric (setf border-v) (border-object object)
  (:documentation "Sets vertical borders of OBJECT to BORDER-OBJECT."))

(defgeneric paint-border (object)
  (:documentation "Paint borders for object.

For BORDER-MIXIN classes this calls the PAINT-BORDER-* generics with the
correct keywords specified for multiple borders.

For all others, this actually draws the border."))

(defgeneric paint-border-bottom (border style object &key blend-left blend-right)
  (:documentation "Paint bottom border.

BLEND-LEFT is T when the left border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side.

BLEND-RIGHT is T when the right border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side."))

(defgeneric paint-border-left (border style object &key blend-top blend-bottom)
  (:documentation "Paint left border.

BLEND-TOP is T when the top border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side.

BLEND-BOTTOM is T when the bottom border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side."))

(defgeneric paint-border-right (border style object &key blend-top blend-bottom)
  (:documentation "Paint right border.

BLEND-TOP is T when the top border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side.

BLEND-BOTTOM is T when the bottom border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side."))

(defgeneric paint-border-top (border style object &key blend-left blend-right)
  (:documentation "Paint top border.

BLEND-LEFT is T when the left border will also be painted, so perform any
required blending to make it look correct. NIL means to paint the full side.

BLEND-RIGHT is T when the right border will also be painted, so perform any
required blending to make it look correct. NIL means to paint full side."))

;;; BOX -------------------------------------------------------------

(defgeneric paint-box (box)
  (:documentation "Called to paint a box. Unless custom drawing is desired, just call
PAINT-BOX-FRAME, PAINT-BOX-INTERIOR, and PAINT-BOX-TITLE.

theme - Theme used for painting box."))

(defgeneric paint-box-frame (style box)
  (:documentation "Called to paint the frame of a box."))

(defgeneric paint-box-interior (style box)
  (:documentation "Called to paint the interior of a box."))

(defgeneric paint-box-title (style box)
  (:documentation "Called to paint the title of a box."))

;;; LAYOUT ----------------------------------------------------------

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

(defgeneric layout-cell (layout &key &allow-other-keys)
  (:documentation "Returns a LAYOUT-CELL-DATA. Keywords are used to specify LAYOUT specific
location informationm."))

(defgeneric layout-cell-options (layout &key &allow-other-keys)
  (:documentation "Allows setting all slots of a LAYOUT-CELL-DATA in a single method."))

(defgeneric layout-cells (layout &key &allow-other-keys)
  (:documentation "Returns a list of all LAYOUT-CELL-DATA for a LAYOUT. The exact composition of
the return value is specific to the type of layout."))

(defgeneric layout-changed (layout &key parent child)
  (:documentation "Called to notify a layout that either a parent's or a child's layout has
changed. The layout will then take whatever action is needed to update its own
and its children's layouts.

When a parent is notifying its child layout, CHILD must be T, and NIL
otherwise.

When a child is notifying its parent layout, PARENT must be T, and NIL
otherwise."))

(defgeneric layout-child (layout &key &allow-other-keys)
  (:documentation "Returns the child (widget) for a LAYOUT. Keywords specific to the LAYOUT are
used for child location."))

(defgeneric layout-column (layout &key &allow-other-keys)
  (:documentation "Returns a LAYOUT-COLMN-DATA. Keywords are used to specify LAYOUT specific
location informationm."))

(defgeneric layout-column-cells (layout &key &allow-other-keys)
  (:documentation "Returns a list of all LAYOUT-CELL-DATA for a column within a LAYOUT."))

(defgeneric layout-column-options (layout &key &allow-other-keys)
  (:documentation "Allows setting all slots of a LAYOUT-COLUMN-DATA in a single method."))

(defgeneric layout-row (layout &key &allow-other-keys)
  (:documentation "Returns a LAYOUT-ROW-DATA. Keywords are used to specify LAYOUT specific
location informationm."))

(defgeneric layout-row-cells (layout &key &allow-other-keys)
  (:documentation "Returns a list of all LAYOUT-CELL-DATA for a row within a LAYOUT."))

(defgeneric layout-row-options (layout &key &allow-other-keys)
  (:documentation "Allows setting all slots of a LAYOUT-ROW-DATA in a single method."))

;;; MANAGER ---------------------------------------------------------

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

(defgeneric owner (object)
  (:documentation "Returns the MANAGER (or MANAGER derived) object that owns OBJECT."))

(defgeneric paint (obj &key &allow-other-keys)
  (:method (object &key)
    (unless (eql object nil)
      (on-paint object))
    (my-next-method)))

(defgeneric process-events (queue object &key &allow-other-keys)
  (:documentation "Process events until told not too. In the case of MANAGER, that means until
slot PROCESS is set to NIL.

While events are being processed, any MOUSE-DOWN handlers should call
ON-MOUSE-DOWN-ACCEPT when they are claiming the mouse button event.

While this method is executing, any events not handled/dispatched internally
are passed to any UNHANDLED-EVENT methods that area specialized on
OBJECT (which is normally a MANGER)."))

(defgeneric unhandled-event (event object) (:method (e o))
  (:documentation "While PROCESS-EVENTS is executing, any events received that it doesn't handle
itself (even if through method dispatch), will be passed to this method when
the method is specialized on the same OBJECT."))

;;; PADDING ---------------------------------------------------------

(defgeneric (setf padding) (value object)
  (:documentation "Sets all PADDING for OBJECT to VALUE."))

(defgeneric (setf padding-h) (value object)
  (:documentation "Sets all horizontal PADDING for OBJECT to VALUE."))

(defgeneric (setf padding-v) (value object)
  (:documentation "Sets all vertical PADDING for OBJECT to VALUE."))

;;; SPACING ---------------------------------------------------------

(defgeneric (setf spacing) (value object)
  (:documentation "Sets all SPACING for OBJECT to VALUE."))

(defgeneric (setf spacing-h) (value object)
  (:documentation "Sets all horizontal SPACING for OBJECT to VALUE."))

(defgeneric (setf spacing-v) (value object)
  (:documentation "Sets all vertical SPACING for OBJECT to VALUE."))

;;; RULER -----------------------------------------------------------

(defgeneric paint-ruler-horizontal (ruler)
  (:documentation "Called from RULER's ON-PAINT method to draw a horizontal ruler."))

(defgeneric paint-ruler-vertical (ruler)
  (:documentation "Called from RULER's ON-PAINT method to draw a vertical ruler."))

;;; THEME -----------------------------------------------------------

(defgeneric default-theme-style (object)
  (:documentation "Returns the default value for the STYLE of theme data based on OBJECT. Default
implementations use the THEME-STYLE (from THEME-MIXIN) from OBJECT or its
parent(s)."))

(defgeneric default-theme-type (object)
  (:documentation "Returns the default value for the TYPE of theme data base on OBJECT."))

(defgeneric get-theme-value (object type id &key style)
  (:documentation "Returns first theme data of TYPE, STYLE, and ID. This will walk the object
heirarcy as needed until it finds a THEME that contains the required daa. If
its unable to find an object that provides the needed data, it will request it
from *THEME-DEFAULT, and then *THEME-ALL-DATA*.

TYPE, STYLE, and ID should be SYMBOLP, KEYWORDP, or NIL."))

(defgeneric get-theme-value-default (type style id)
  (:documentation "Returns the theme data of TYPE, STYLE, and ID from *THEME-ALL-DATA*."))

(defgeneric set-theme-value (object type id value &key style)
  (:documentation "Adds theme data of TYPE, STYLE, and ID to OBJECT with data VALUE.

TYPE, STYLE, and ID should be symbols (preferred), keywords, or strings.

If STYLE is not provided, it will use the THEME-STYLE (from THEME-MIXIN) from
the OBJECT or its parent(s)."))

(defgeneric set-theme-value-default (type style id value)
  (:documentation "Adds theme data of TYPE, STYLE, and ID to *THEME-ALL-DATA* with data VALUE."))

(defgeneric theme-valuep (object type id &key style)
  (:documentation "Returns T if the OBJECT contains the theme-data TYPE, STYLE, and ID.

If STYLE is not provided, it will use the THEME-STYLE (from THEME-MIXIN) from
the OBJECT or its parent(s)."))

(defgeneric theme-value-defaultp (type style id)
  (:documentation "Returns T if the *THEME-ALL-DATA* contains the theme-data TYPE, STYLE, and ID."))


