(in-package #:cl-yag)

(docs:define-docs (defun color2assoc
                    "Return r, g, b, and a of allegro color as association list.

(color2assoc (map-rgba-f 0.2 0.4 0.6 0.8)
returns
((:r 0.2) (:g 0.4) (:b 0.6) (:a 0.8))"))

(docs:define-docs (defun color2list
                    "Returns r, g, b, and a of allegro color as list.

(color2list (map-rgba-f 0.2 0.4 0.6 0.8)
returns
(0.2 0.4 0.6 0.8)"))

(docs:define-docs (defun color-a
                    "Returns a of allegro color."))

(docs:define-docs (defun color-b
                    "Returns b of allegro color."))

(docs:define-docs (defun color-g
                    "Returns g of allegro color."))

(docs:define-docs (defun color-inverse
                    "Returns simple inverse of color."))

(docs:define-docs (defun color-r
                    "Returns r of allegro color."))

(docs:define-docs (defgeneric make-color
                      "Returns allegro color for specified parameters."))

(docs:define-docs (defun print-color
                    "Returns an allegro map-rgba-f call for color.
print-color(al:map-rgb-f 0 0 0)
would generate
al:map-rgba-f 0.0 0.0 0.0 1.0"))

;;;; function wrappers ========================================================

