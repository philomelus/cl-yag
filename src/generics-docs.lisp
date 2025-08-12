(in-package :cl-yag)

;;;; generics =================================================================

(docs:define-docs (defgeneric between
                    "Returns value randomly between lo and hi."))

(docs:define-docs (defgeneric bottom
                    "Returns bottom coordinate of objects controlled space."))

(docs:define-docs (defgeneric container-calc-child-height
                    ""))

(docs:define-docs (defgeneric container-calc-child-left
                    ""))

(docs:define-docs (defgeneric container-calc-child-top
                    ""))

(docs:define-docs (defgeneric container-calc-child-width
                    ""))

(docs:define-docs (defgeneric must-init
                    "If the test is nil, genereates error.  Otherwise returns."))

(docs:define-docs (defgeneric on-char
                    ""))

(docs:define-docs (defgeneric on-mouse-click
                    "Called when a click occurs within an objects controlled space."))

(docs:define-docs (defgeneric on-mouse-down
                    ""))

(docs:define-docs (defgeneric on-mouse-down-accept
                    "Call this when an on-mouse-down hanlder has accepted ownership of event."))

(docs:define-docs (defgeneric on-mouse-move
                    ""))

(docs:define-docs (defgeneric on-mouse-up
                    ""))

(docs:define-docs (defgeneric on-paint
                    "Called when an object needs to draw itself."))

(docs:define-docs (defgeneric on-resize
                    ""))

(docs:define-docs (defgeneric owner
                    "Returns owner of object if known, or nil otheriwse."))

(docs:define-docs (defgeneric paint
                    ""))

(docs:define-docs (defgeneric print-mixin
                    ""))

(docs:define-docs (defgeneric process-events
                    "Process events until something says to exit."))

(docs:define-docs (defgeneric right
                    "Returns right coordinate of objects controlled space."))

(docs:define-docs (defgeneric unhandled-event
                    ""))

(docs:define-docs (defgeneric within
                    "Returns t when coordinate is within objects controlled area, otherwise nil."))

;;;; setf =====================================================================

(docs:define-docs ((setf area)
                    ""))

(docs:define-docs ((setf border)
                    ""))

(docs:define-docs ((setf border-h)
                    ""))

(docs:define-docs ((setf border-v)
                    ""))

(docs:define-docs ((setf location)
                    ""))

(docs:define-docs ((setf padding)
                    ""))

(docs:define-docs ((setf padding-h)
                    ""))

(docs:define-docs ((setf padding-v)
                    ""))

(docs:define-docs ((setf spacing)
                    ""))

(docs:define-docs ((setf spacing-h)
                    ""))

(docs:define-docs ((setf spacing-v)
                    ""))


