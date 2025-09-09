(in-package :asdf-user)
(defpackage :cl-yag
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:v #:org.shirakumo.verbose)
   (#:bt #:bordeaux-threads))
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
  (:import-from #:closer-mop
                #:class-slots
                #:find-class
                #:slot-definition-name)
  (:export #:main

           ;; Global init/load/etc.
           #:default-font
           #:default-mono-font
           #:theme-flat-aqua
           #:theme-flat-blue
           #:theme-flat-gray
           #:theme-flat-green
           #:theme-flat-purple
           #:theme-flat-red
           #:theme-flat-yellow
           #:theme-3d-aqua
           #:theme-3d-blue
           #:theme-3d-gray
           #:theme-3d-green
           #:theme-3d-purple
           #:theme-3d-red
           #:theme-3d-yellow
           
           ;; GUI classes / Macros
           #:active-text
           #:column-layout
           #:defactive-text
           #:defborder
           #:defbox
           #:defcolumn-layout
           #:defgrid
           #:defgrid-layout
           #:defmanager
           #:defruler
           #:deftext
           #:deftheme-3d
           #:deftheme-base
           #:deftheme-flat
           #:defwindow
           #:division
           #:division-10
           #:division-100
           #:division-2
           #:division-25
           #:division-5
           #:layout
           #:manager
           #:ruler
           #:ruler-100-25-5
           #:ruler-10-2
           #:ruler-25-5
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
           #:back-fore-color-mixin
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
           #:align
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
           #:columns
           #:content
           #:dark
           #:enabled
           #:font
           #:fore-color
           #:frame-color
           #:h-align
           #:height
           #:interior-color
           #:left
           #:light
           #:location
           #:location-x
           #:location-y
           #:manager
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
           #:rows
           #:spacing
           #:spacing-bottom
           #:spacing-h
           #:spacing-left
           #:spacing-right
           #:spacing-top
           #:spacing-v
           #:style
           #:title-position
           #:theme
           #:theme-3d-d
           #:theme-3d-l
           #:theme-3d-n
           #:theme-3d-vd
           #:theme-3d-vl
           #:thickness
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
           #:area-allocated
           #:area-br
           #:cleanup-method
           #:find-parent-content
           #:print-raw-object
           #:theme-field
           #:theme-field-object
           #:with-area-border
           #:with-area-border-and-padding
           #:with-area-padding
           #:with-local-accessors
           #:with-local-slots
           #:with-theme
           #:with-object-or-theme
           
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
           #:mouse-event-button
           #:mouse-event-dx
           #:mouse-event-dy
           #:mouse-event-x
           #:mouse-event-y
           #:print-color
           #:timer-event-count
           #:timer-event-source
           #:+MOUSE-BUTTON-LEFT+
           #:+MOUSE-BUTTON-RIGHT+
           #:+MOUSE-BUTTON-MIDDLE+
           #:+OP-ADD+
           #:+OP-SRC-MINUS-DEST+
           #:+OP-DEST-MINUS-SRC+
           #:+BLEND-ZERO+
           #:+BLEND-ONE+
           #:+BLEND-ALPHA+
           #:+BLEND-INVERSE-ALPHA+
           #:+BLEND-SRC-COLOR+
           #:+BLEND-DEST-COLOR+
           #:+BLEND-INVERSE-SRC-COLOR+
           #:+BLEND-INVERSE-DEST-COLOR+
           #:+BLEND-CONST-COLOR+
           #:+BLEND-INVERSE-CONS-COLOR+
           #:+P-F-RGB-888+
           #:+P-F-BGR-888+
           #:+P-F-ABGR-8888+
           #:+P-F-ARGB-8888+
           #:+LOCK_READWRITE+
           #:+LOCK_READONLY+
           #:+LOCK_WRITEONLY+

           ;; Globals
           #:*theme-default*
           
           ))

(in-package #:cl-yag)
