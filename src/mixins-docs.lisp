(in-package #:cl-yag)

;;;; align-mixin ==============================================================

(docs:define-docs (align-mixin ""))

;;;; area-mixin ===============================================================

(docs:define-docs (area-mixin ""))

;;;; enable-mixin =============================================================

(docs:define-docs (enable-mixin ""))

(docs:define-docs (enabled ""))

(docs:define-docs ((setf enabled) ""))

;;;; border-mixin =============================================================

(docs:define-docs (border
                    "Represents a graphical border.

Styles:
  :default - Single colored line"))

(docs:define-docs ((setf border)
                    "Sets the border for all sides."))

(docs:define-docs (border-borrom
                    "Details for drawing bottom border."))

(docs:define-docs (border-color
                    "Color of border line."))

(docs:define-docs ((setf border-h)
                    "Sets the border for both the left and right sides."))

(docs:define-docs (border-left
                    "Details for drawing left border."))

(docs:define-docs (border-mixin
                    "Adds a graphical border to an object."))

(docs:define-docs (border-right
                    "Details for drawing right border."))

(docs:define-docs (border-style
                    "Style used for border. See (:class border)."))

(docs:define-docs (border-top
                    "Details for drawing top border."))

(docs:define-docs ((setf border-v)
                    "Sets the border for both the top and bottom sides."))

(docs:define-docs (border-width
                    "Width of border in pixels."))

;;;; color-mixin ==============================================================

(docs:define-docs (color-mixin
                    "Adds a color field to object."))

(docs:define-docs (color
                    "Returns the color of the object."))

(docs:define-docs ((setf color)
                    "Sets the color for the object."))

;;;; color-fore-back-mixin ====================================================

(docs:define-docs (color-fore-back-mixin
                    "Adds fore-color and back-color as color fields to object."))

(docs:define-docs (back-color
                    "Returns the back-color for object.  Meant as a background color."))

(docs:define-docs ((setf back-color)
                    "Sets the back-color for the object.  Meant as a background color."))

(docs:define-docs (fore-color
                    "Returns the fore-color for object.  Meant as a foreground color."))

(docs:define-docs ((setf fore-color)
                    "Sets the fore-color for the object.  Meant as a foreground color."))

;;;; enable-mixin =============================================================

(docs:define-docs (enable-mixin
                    "Adds an enabled slot to object."))

;;;; font-mixin ===============================================================

(docs:define-docs (font-mixin ""))

(docs:define-docs (font ""))

(docs:define-docs ((setf font) ""))

;;;; h-align-mixin ============================================================

(docs:define-docs (h-align-mixin "Valid keywords are:
    :none   - No processing done.
    :left   - To align to left side.
    :right  - To align to right side.
    :center - To center horizontally."))

(docs:define-docs (h-align ""))

(docs:define-docs ((setf h-align) ""))

(docs:define-docs (align-h "Alias for h-align."))

(docs:define-docs ((setf align-h) "Alias for (setf h-align)."))

;;;; location-mixin ===========================================================

(docs:define-docs (location-mixin ""))

(docs:define-docs ((setf location) ""))

(docs:define-docs (location-x ""))

(docs:define-docs ((setf location-x) ""))

(docs:define-docs (location-y ""))

(docs:define-docs ((setf location-y) ""))

;;;; padding-mixin ============================================================

(docs:define-docs (padding-mixin ""))

(docs:define-docs ((setf padding) ""))

(docs:define-docs (padding-bottom ""))

(docs:define-docs ((setf padding-bottom) ""))

(docs:define-docs ((setf padding-h) ""))

(docs:define-docs (padding-left ""))

(docs:define-docs ((setf padding-left) ""))

(docs:define-docs (padding-right ""))

(docs:define-docs ((setf padding-right) ""))

(docs:define-docs (padding-top ""))

(docs:define-docs ((setf padding-top) ""))

(docs:define-docs ((setf padding-v) ""))

;;;; parent-mixin =============================================================

(docs:define-docs (parent-mixin
                    "Adds a parent slot to object."))

;;;; spacing-mixin ============================================================

(docs:define-docs (spacing-mixin ""))

(docs:define-docs ((setf spacing) ""))

(docs:define-docs (spacing-bottom ""))

(docs:define-docs ((setf spacing-bottom) ""))

(docs:define-docs ((setf spacing-h) ""))

(docs:define-docs (spacing-left ""))

(docs:define-docs ((setf spacing-left) ""))

(docs:define-docs (spacing-right ""))

(docs:define-docs ((setf spacing-right) ""))

(docs:define-docs (spacing-top ""))

(docs:define-docs ((setf spacing-top) ""))

(docs:define-docs ((setf spacing-v) ""))

;;;; title-mixin ==============================================================

(docs:define-docs (title-mixin
                    "Adds a title slot to object."))

;;;; v-align-mixin ============================================================

(docs:define-docs (v-align-mixin "Valid keywords are:
    :none   - No processing done.
    :top    - To align to top side.
    :bottom - To align to bottom side.
    :middle - To center vertically."))

(docs:define-docs (v-align ""))

(docs:define-docs ((setf v-align) ""))

(docs:define-docs (align-v "Alias for v-align."))

(docs:define-docs ((setf align-v) "Alias for (setf v-align)."))

;;;; visible-mixin =============================================================

(docs:define-docs (visible-mixin ""))

(docs:define-docs (visible ""))

(docs:define-docs ((setf visible) ""))

