(in-package :cl-yag)

(docs:define-docs (bottom
                    "Returns bottom coordinate of objects controlled space."))

(docs:define-docs (on-mouse-click
                    "Called when a click occurs within an objects controlled space."))

(docs:define-docs (on-mouse-enter
                    "Called when mouse enters objects controlled space."))

(docs:define-docs (on-mouse-exit
                    "Called when mouse exits an objects controlled space."))

(docs:define-docs (on-paint
                    "Called when an object needs to draw itself."))

(docs:define-docs (manage
                    "Adds object to manager."))

(docs:define-docs (process-events
                    "Process events until something says to exit."))

(docs:define-docs (right
                    "Returns right coordinate of objects controlled space."))

(docs:define-docs (show
                    "Change :visible to t and force initial paint."))

(docs:define-docs (unmanage
                    "Removes object from manager."))

