(in-package #:clg)

;;;; EVENT

(defmacro event-type (event)
  `(cffi:foreign-slot-value ,event '(:union al:event) 'al::type))

;;;; KEYBOARD-EVENT

(defmacro keyboard-event (event field)
  `(cffi:foreign-slot-value ,event '(:struct al:keyboard-event) ,field))

;;;; MOUSE-EVENT

(defmacro mouse-event (event field)
  `(cffi:foreign-slot-value ,event '(:struct al:mouse-event) ,field))

(defmacro mouse-event-x (event)
  `(mouse-event ,event 'al::x))

(defmacro mouse-event-y (event)
  `(mouse-event ,event 'al::y))

(defmacro mouse-event-button (event)
  `(mouse-event ,event 'al::button))
