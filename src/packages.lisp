(in-package :asdf-user)
(defpackage :cl-yag
  (:use #:cl)
  (:local-nicknames
   (#:v #:org.shirakumo.verbose))  
  (:import-from #:cffi
                #:foreign-alloc
                #:foreign-enum-value
                #:foreign-free
                #:foreign-pointer
                #:foreign-slot-value
                #:null-pointer
                #:null-pointer-p)
  ;; (:import-from #:cffi-object
  ;;               #:define-cobject-class)
  (:export #:main
           ;; Constants
           #:+LAYOUT-HEIGHT-CALC+
           #:+LAYOUT-LEFT-CALC+
           #:+LAYOUT-TOP-CALC+
           #:+LAYOUT-WIDTH-CALC+

           ;; GUI classes / Macros
           #:active-text
           #:column-layout
           #:defactive-text
           #:defborder
           #:defcolumn-layout
           #:defmanager
           #:defruler
           #:deftext
           #:deftheme-3d
           #:deftheme-base
           #:deftheme-flat
           #:defwindow
           #:layout
           #:manager                    ;class
           #:ruler
           #:text
           #:text-base
           #:theme-3d
           #:theme-base
           #:theme-flat
           #:window

           ;; Classes
           #:active-mixin
           #:align-mixin
           #:area-mixin
           #:border-mixin
           #:color-mixin
           #:color-fore-back-mixin
           #:container-mixin
           #:content-mixin
           #:enable-mixin
           #:font-mixin
           #:h-align-mixin
           #:location-mixin
           #:manager-mixin
           #:padding-mixin
           #:parent-mixin
           #:shortcuts-mixin
           #:spacing-mixin
           #:v-align-mixin
           #:theme-mixin
           #:title-mixin
           #:visible-mixin
           
           ;; Class generics
           #:active
           #:back-color
           #:border
           #:border-h
           #:border-v
           #:border-bottom
           #:border-left
           #:border-right
           #:border-top
           #:color
           #:color-down
           #:color-hover
           #:color-up
           #:content
           #:dark
           #:enabled
           #:font
           #:fore-color
           #:h-align
           #:height
           #:left
           #:light
           #:location
           #:location-x
           #:location-y
           #:manager                    ;generic
           #:normal
           #:padding
           #:padding-bottom
           #:padding-h
           #:padding-left
           #:padding-right
           #:padding-top
           #:padding-v
           #:parent
           #:process
           #:spacing
           #:spacing-bottom
           #:spacing-h
           #:spacing-left
           #:spacing-right
           #:spacing-top
           #:spacing-v
           #:style
           #:theme
           #:theme-d
           #:theme-l
           #:theme-n
           #:theme-vd
           #:theme-vl
           #:top
           #:title
           #:v-align
           #:very-dark
           #:very-light
           #:visible
           #:width

           ;; Other generics
           #:area
           #:between
           #:bottom
           #:container-calc-child-height
           #:container-calc-child-left
           #:container-calc-child-top
           #:container-calc-child-width
           #:on-char
           #:on-command
           #:on-key-down
           #:on-key-up
           #:on-mouse-down
           #:on-mouse-down-accept
           #:on-mouse-move
           #:on-mouse-up
           #:on-paint
           #:on-resize
           #:owner
           #:paint
           #:paint-border
           #:print-mixin
           #:process-events
           #:right
           #:unhandled-event
           #:within
           
           ;; Misc
           #:print-raw-object
           
           ;; Allegro specific
           #:color-a
           #:color-b
           #:color-g
           #:color-inverse
           #:color-r
           #:display-event-height
           #:display-event-x
           #:display-event-width
           #:display-event-y
           #:event-type
           #:get-text-dimensions
           #:keyboard-event-keycode
           #:keyboard-event-modifiers
           #:make-color
           #:mouse-event-button
           #:mouse-event-dx
           #:mouse-event-dy
           #:mouse-event-x
           #:mouse-event-y
           #:print-color
           #:timer-event-count
           #:timer-event-source

           ;; Globals
           #:*theme-flat-aqua*
           #:*theme-flat-blue*
           #:*theme-flat-gray*
           #:*theme-flat-green*
           #:*theme-flat-purple*
           #:*theme-flat-red*
           #:*theme-flat-yellow*
           #:*theme-3d-aqua*
           #:*theme-3d-blue*
           #:*theme-3d-gray*
           #:*theme-3d-green*
           #:*theme-3d-purple*
           #:*theme-3d-red*
           #:*theme-3d-yellow*

           ))

(in-package #:cl-yag)
