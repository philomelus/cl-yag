(in-package #:clg)

(docs:define-docs (window
                    "Virtual grouping of functionality together in whatever way makes sense for the application."))

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
