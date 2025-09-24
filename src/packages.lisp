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
           
           ;; ========== Font ==========
           #:default-font
           #:default-mono-font

           ;; ========== Theme ==========
           #:*theme-default*            ; default: (theme-flat-gray)
           #:deftheme-3d
           #:deftheme-3d-all
           #:deftheme-flat
           #:deftheme-flat-all
           #:find-theme
           #:frame-color
           #:interior-color
           #:theme-3d
           #:theme-3d-all
           #:theme-3d-aqua
           #:theme-3d-blue
           #:theme-3d-gray
           #:theme-3d-green
           #:theme-3d-purple
           #:theme-3d-red
           #:theme-3d-yellow
           #:theme-base
           #:theme-flat
           #:theme-flat-all
           #:theme-flat-aqua
           #:theme-flat-blue
           #:theme-flat-gray
           #:theme-flat-green
           #:theme-flat-purple
           #:theme-flat-red
           #:theme-flat-yellow

           ;; ========== Manager ==========
           #:defmanager
           #:manager
           #:process
           #:process-events

           ;; ========== Types ==========
           #:coordinate
           
           ;; ========== Layout ==========
           ;; layout
           #:area-allocated
           #:calc-layout-area
           #:deflayout
           #:dump-layout
           #:find-parent-area-or-layout
           #:find-parent-content
           #:height
           #:layout
           #:layout-base
           #:layout-cell
           #:layout-cell-reset
           #:layout-change
           #:layout-child-type
           #:layout-coordinate-option-type
           #:layout-height-type
           #:layout-left-type
           #:layout-top-type
           #:layout-width-type
           #:left
           #:top
           #:validate-layout-base-options
           #:width
           #:with-changes
           
           ;; column-layout
           #:column-layout
           #:defcolumn-layout
           
           ;; grid-layout
           #:columns
           #:defgrid-layout
           #:grid-layout
           #:grid-layout-cell
           #:grid-layout-cell-set
           #:grid-layout-child
           #:grid-layout-column
           #:grid-layout-column-cells-set
           #:grid-layout-column-options
           #:grid-layout-column-type
           #:grid-layout-row
           #:grid-layout-row-cells-set
           #:grid-layout-row-height-type
           #:grid-layout-row-options
           #:height-type
           #:rows
           #:width-type

           ;; row-layout
           #:row-layout
           #:defrow-layout
           
           ;; ========== Mixins ==========
           ;; align
           #:align-mixin
           #:h-align
           #:h-align-mixin
           #:h-align-type
           #:v-align
           #:v-align-mixin
           #:v-align-type
           
           ;; area
           #:area
           #:area-mixin
           #:area-rb
           #:area-rect
           #:bottom
           #:find-parent-area
           #:height
           #:left
           #:right
           #:top
           #:width
           
           ;; border
           #:border
           #:border-theme-3d-mixin
           #:border-bottom
           #:border-theme-flat-mixin
           #:border-h
           #:border-left
           #:border-mixin
           #:border-right
           #:border-theme-mixin
           #:border-top
           #:border-v
           #:defborder
           #:style
           #:thickness
           #:with-borders
           
           ;; button
           #:border-thickness
           #:borderp
           #:button
           #:button-theme-3d-mixin
           #:button-theme-flat-mixin
           #:button-theme-mixin
           #:defbutton
           #:down-color
           #:hover-color
           #:hover-inside-borderp
           #:hover-inside-paddingp
           #:hover-thickness
           #:up-color
           
           ;; color
           #:back-color
           #:back-color-mixin
           #:back-fore-color-mixin
           #:color
           #:color-3d-mixin
           #:color-mixin
           #:dark
           #:fore-color
           #:fore-color-mixin
           #:frame-color
           #:frame-color-mixin
           #:interior-color
           #:interior-color-mixin
           #:light
           #:normal
           #:very-dark
           #:very-light
           
           ;; content
           #:content
           #:content-mixin
           
           ;; font
           #:font
           #:font-mixin
           
           ;; manager
           #:manager
           #:manager-mixin
           
           ;; padding
           #:padding
           #:padding-bottom
           #:padding-h
           #:padding-left
           #:padding-mixin
           #:padding-right
           #:padding-top
           #:padding-v
           
           ;; parent
           #:owner
           #:parent
           #:parent-mixin
           
           ;; shortcuts
           #:convert-shortcuts
           #:mod2val
           #:shortcuts
           #:shortcuts-mixin
           #:val2mod
           #:validate-shortcuts
           
           ;; spacing
           #:spacing
           #:spacing-bottom
           #:spacing-h
           #:spacing-left
           #:spacing-mixin
           #:spacing-right
           #:spacing-top
           #:spacing-v
           
           ;; style
           #:style
           #:style-3d-mixin
           #:style-3d-type
           
           ;; theme
           #:theme
           #:theme-mixin
           
           ;; title
           #:title
           #:title-mixin
           
           ;; visible
           #:visible
           #:visible-mixin
           
           ;; ========== Widgets ==========
           ;; box
           #:box
           #:box-theme-3d-mixin
           #:box-theme-flat-mixin
           #:box-theme-mixin
           #:box-title-position-type
           #:defbox
           #:filled
           #:thickness
           #:title-position
           #:validate-box-options
           
           ;; grid
           #:color
           #:color-h
           #:color-v
           #:defgrid
           #:grid
           #:grid-theme-mixin
           #:major
           #:major-color
           #:major-color-h
           #:major-color-v
           #:minor
           #:minor-color
           #:minor-color-h
           #:minor-color-v
           
           ;; primitives
           #:%point2
           #:bottom
           #:defline
           #:defrectangle
           #:end
           #:h
           #:left
           #:line
           #:line-theme-3d-mixin
           #:line-theme-flat-mixin
           #:line-theme-mixin
           #:rectangle
           #:rectangle-theme-3d-mixin
           #:rectangle-theme-flat-mixin
           #:rectangle-theme-mixin
           #:right
           #:start
           #:top
           #:v
           #:width
           #:x
           #:y
           
           ;; rect
           #:%rect
           #:bottom
           #:height
           #:left
           #:right
           #:top
           #:width
           
           ;; ruler
           #:align
           #:defdivision
           #:defruler
           #:div-10
           #:div-10-color
           #:div-10-extent
           #:div-10-extent-type
           #:div-100
           #:div-100-color
           #:div-100-extent
           #:div-100-extent-type
           #:div-2
           #:div-2-color
           #:div-2-extent
           #:div-2-extent-type
           #:div-25
           #:div-25-color
           #:div-25-extent
           #:div-25-extent-type
           #:div-5
           #:div-5-color
           #:div-5-extent
           #:div-5-extent-type
           #:division
           #:division-10
           #:division-100
           #:division-2
           #:division-25
           #:division-5
           #:division-theme-mixin
           #:divisions
           #:extent
           #:extent-type
           #:extent-type-type
           #:period
           #:ruler
           #:ruler-10-2
           #:ruler-100-25-5
           #:ruler-25-5
           #:ruler-align-type
           #:ruler-theme-mixin
           #:thickness
           #:validate-ruler-options
           #:vertical

           ;; text
           #:deftext
           #:interior-color
           #:text
           #:text-calc-height
           #:text-calc-left
           #:text-calc-title-left
           #:text-calc-title-top
           #:text-calc-top
           #:text-calc-width
           #:text-theme-mixin
           
           ;; window
           #:defwindow
           #:interior-color
           #:window
           #:window-theme-mixin
           
           ;; ========== Generics ==========
           #:area
           #:border
           #:border-h
           #:border-v
           #:calc-area
           #:calc-height
           #:calc-layout-child-areas
           #:calc-left
           #:calc-top
           #:calc-width
           #:left
           #:location
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
           #:on-timer
           #:owner
           #:padding
           #:padding-h
           #:padding-v
           #:paint
           #:paint-border
           #:paint-border-bottom
           #:paint-border-left
           #:paint-border-right
           #:paint-border-top
           #:process-events
           #:right
           #:spacing
           #:spacing-h
           #:spacing-v
           #:unhandled-event
           #:update-child-layout-areas
           #:within
           
           ;; ========== Macros ==========
           #:cleanup-method
           #:foro
           #:my-next-method
           #:theme-field
           #:theme-field-object
           #:with-area-border
           #:with-area-border-and-spacing
           #:with-area-padding
           #:with-local-accessoors
           #:with-local-slots
           #:with-object-and-theme
           #:with-object-or-theme
           #:with-parent-area
           #:with-parent-content
           #:with-theme
           
           ;; ========== Utilities ==========
           #:2+
           #:2-
           #:asset
           #:print-object-as-string
           #:print-raw-object
           #:print-thread-name
           #:remove-keyword-params
           
           ;; ========== Allegro ==========
           #:+ALIGN-CENTER+
           #:+ALIGN-LEFT+
           #:+ALIGN-RIGHT+
           #:+BLEND-ALPHA+
           #:+BLEND-CONST-COLOR+
           #:+BLEND-DEST-COLOR+
           #:+BLEND-INVERSE-ALPHA+
           #:+BLEND-INVERSE-CONS-COLOR+
           #:+BLEND-INVERSE-DEST-COLOR+
           #:+BLEND-INVERSE-SRC-COLOR+
           #:+BLEND-ONE+
           #:+BLEND-SRC-COLOR+
           #:+BLEND-ZERO+
           #:+KEY-B+
           #:+KEY-DOWN+
           #:+KEY-ESC+
           #:+KEY-LEFT+
           #:+KEY-MAX+
           #:+KEY-RIGHT+
           #:+KEY-UP+
           #:+KEY-X+
           #:+LOCK-READONLY+
           #:+LOCK-READWRITE+
           #:+LOCK-WRITEONLY+
           #:+MOUSE-BUTTON-LEFT+
           #:+MOUSE-BUTTON-MIDDLE+
           #:+MOUSE-BUTTON-RIGHT+
           #:+OP-ADD+
           #:+OP-DEST-MINUS-SRC+
           #:+OP-SRC-MINUS-DEST+
           #:+P-F-ABGR-8888+
           #:+P-F-BGR-888+
           #:+P-F-RGB-888+
           #:+P-F-RGBA-8888+
           #:+TFF-NO-KERNING+
           #:+TTF-MONOCHROME+
           #:+TTF-NO-AUTOHINT+
           #:color-a
           #:color-b
           #:color-g
           #:color-inverse
           #:color-r
           #:color2assoc
           #:color2list
           #:draw-prim
           #:event-type
           #:get-blender
           #:get-text-dimensions
           #:print-color
           #:unmap-rgba
           #:with-blender
           #:with-clipping
           ))

(in-package #:cl-yag)
