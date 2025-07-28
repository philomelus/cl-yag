(in-package :clg)

;;;;000001111111111222222222233333333334444444444555555555566666666667777777777

                 ;;;;0000000001111111111222222222233333333334444444444555555555566666666667777777777

(docs:define-docs (active-text
                    "Display text at specified coordinates.
When mouse is over text area, will hilight title by inversing color."))

(docs:define-docs (bottom
                    "Returns bottom coordinate of objects controlled space."))

(docs:define-docs (is-dirty
                    "Returns t if the object is dirty.  If recurse is t, then will also check
wether any contained objects are dirty."))

(docs:define-docs (on-mouse-click
                    "Called when a click occurs within an objects controlled space."))

;; (docs:define-docs (on-mouse-enter
;;                     "Called when mouse enters objects controlled space."))

;; (docs:define-docs (on-mouse-exit
;;                     "Called when mouse exits an objects controlled space."))

(docs:define-docs (on-paint
                    "Called when an object needs to draw itself."))

(docs:define-docs (manage
                    "Adds object to manager."))

(docs:define-docs (manager
                    "Given a list of objects to manager, handles interaction of user interface
events between them and the current display.

connect Events:
  char obj mgr code modfifiers
  key-down obg mgr code modifiers
  key-up obj mgr code modifiers
  mouse-click obj mgr x y b
  mouse-down obj mgr x y b
  mouse-move obj mgr x y
  mouse-moved obj mgr
  mouse-up obj mgr x y b

The following events also contain pre and post events as well:
(for example \"mouse-down-pre\", \"mouse-down-post\")
  mouse-down, mouse-up, key-down, key-up, mouse-move, paint
"))

(docs:define-docs (process-events
                    "Process events until something says to exit."))

(docs:define-docs (right
                    "Returns right coordinate of objects controlled space."))

(docs:define-docs (show
                    "Change :visible to t and force initial paint."))

(docs:define-docs (text
                   "Display title at specified coordinates with specified font."))

(docs:define-docs (unmanage
                    "Removes object from manager."))

(docs:define-docs (window-active
                    "Returns whether contained objects are drawn as active or inactive.
Objects must decide what active and inactive actually mean."))
(docs:define-docs ((setf window-active)
                    "Sets whether contained controls are drawn as active or inactive.
Will mark itself and all contained objects as dirty when changed."))

(docs:define-docs (window-content
                    "List of objects to show/interact with within the windows controlled area."))
(docs:define-docs ((setf window-content)
                    "Sets the list of objects to show/interact with within the windows controlled area.  Will mark object dirty."))

(docs:define-docs (window-dirty
                    "When t, repaint is needed."))

(docs:define-docs (window-enabled
                    "When t, object can generate non-paint events."))
(docs:define-docs ((setf window-enabled)
                    "Sets whether window can generate non-paint events.  Will mark object as dirty if changed."))

(docs:define-docs (window-h
                    "Alias of window-height."))
(docs:define-docs ((setf window-h)
                    "Alias of (setf window-height)."))

(docs:define-docs (window-height
                    "Returns height of controlled area."))
(docs:define-docs ((setf window-height)
                    "Sets the height of the controlled area.  Will mark object as dirty if changed."))

(docs:define-docs (window-left
                    "Returns left coordinate of controlled area."))
(docs:define-docs ((setf window-left)
                    "Sets the left cooridinate of the controlled area.  Will mark object as dirty if changed."))

(docs:define-docs (window-options
                    "Various keywords that change the behaviour of a window, per below.

:auto-width  - Calculate width of contained objects automatically.
:auto-height - Calculate height of contained objects automatically.
:auto-size   - Calculate size of contained objects automatically.
:centered    - Center the contained objects both vertically and horizontally.
:left        - Place contained objects along left side of controlled area.
:right       - Place contained objects along right side of controlled area.
:top         - Place contained objects at top of controlled area.
:bottom      - Place contained objects at bottom of controlled area.

Derived objects can add to the list as desired."))

(docs:define-docs (window-top
                    "Returns top coordinate of controlled area."))

(docs:define-docs (window-visible
                    "When t, window is painted and (possibly) responds to events."))
(docs:define-docs ((setf window-visible) "Determines when window contents are painted and (possibly) responding to events.
Will mark object and all contained objects as dirty when changed."))

(docs:define-docs (window-w
                    "Alias for window-width."))

(docs:define-docs (window-width
                    "Returns width of controlled area."))

(docs:define-docs (window-x
                    "Alias of window-left."))

(docs:define-docs (window-y
                    "Alias for window-top."))

(docs:define-docs (((setf window-active) (t window))
                    "Specifically sets the active state of a window."))


