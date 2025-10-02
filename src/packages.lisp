(in-package :asdf-user)
(defpackage :cl-yag
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:bt #:bordeaux-threads)
   (#:mop #:closer-mop)
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
  (:import-from #:closer-mop
                #:class-slots
                #:ensure-class
                #:find-class
                #:slot-definition-name
                #:slot-value-using-class)
  (:export #:main

           ;; ========== COLORS ==========
           #:+COLOR-AQUA+
           #:+COLOR-BLACK+
           #:+COLOR-BLUE+
           #:+COLOR-DARK-AQUA+
           #:+COLOR-DARK-BLUE+
           #:+COLOR-DARK-GRAY+
           #:+COLOR-DARK-GREEN+
           #:+COLOR-DARK-PURPLE+
           #:+COLOR-DARK-RED+
           #:+COLOR-DARK-YELLOW+
           #:+COLOR-GRAY+
           #:+COLOR-GREEN+
           #:+COLOR-LIGHT-AQUA+
           #:+COLOR-LIGHT-BLUE+
           #:+COLOR-LIGHT-GRAY+
           #:+COLOR-LIGHT-GREEN+
           #:+COLOR-LIGHT-PURPLE+
           #:+COLOR-LIGHT-RED+
           #:+COLOR-LIGHT-YELLOW+
           #:+COLOR-PURPLE+
           #:+COLOR-RED+
           #:+COLOR-VERY-DARK-AQUA+
           #:+COLOR-VERY-DARK-BLUE+
           #:+COLOR-VERY-DARK-GRAY+
           #:+COLOR-VERY-DARK-GREEN+
           #:+COLOR-VERY-DARK-PURPLE+
           #:+COLOR-VERY-DARK-RED+
           #:+COLOR-VERY-DARK-YELLOW+
           #:+COLOR-VERY-LIGHT-AQUA+
           #:+COLOR-VERY-LIGHT-BLUE+
           #:+COLOR-VERY-LIGHT-GRAY+
           #:+COLOR-VERY-LIGHT-GREEN+
           #:+COLOR-VERY-LIGHT-PURPLE+
           #:+COLOR-VERY-LIGHT-RED+
           #:+COLOR-VERY-LIGHT-YELLOW+
           #:+COLOR-WHITE+
           #:+COLOR-YELLOW+
           
           ;; ========== Font ==========
           #:default-font
           #:default-mono-font

           ;; ========== Theme ==========
           #:*theme-default*
           #:default-theme-style
           #:default-theme-type
           #:get-theme-value
           #:get-theme-value-default
           #:set-theme-value
           #:set-theme-value-default
           #:theme
           #:theme-base
           #:theme-style
           
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
           #:layout-cell
           #:layout-cell-reset
           #:layout-change
           #:layout-child
           #:layout-child-type
           #:layout-column
           #:layout-column-options
           #:layout-column-type
           #:layout-coordinate-option-type
           #:layout-height-type
           #:layout-left-type
           #:layout-row
           #:layout-row-height-type
           #:layout-row-options
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
           #:height-type
           #:width-type
           #:rows

           ;; row-layout
           #:row-layout
           #:defrow-layout
           
           ;; ========== Mixins ==========
           ;; align
           #:h-align
           #:h-align-mixin
           #:h-align-mixin-base
           #:h-align-type
           #:v-align
           #:v-align-mixin
           #:v-align-type
           
           ;; area
           #:area
           #:area-mixin
           #:area-mixin-base
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
           #:border-bottom
           #:border-h
           #:border-left
           #:border-mixin
           #:border-mixin-base
           #:border-right
           #:border-top
           #:border-v
           #:with-borders
           
           ;; button
           #:border-thickness
           #:borderp
           #:button
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
           #:back-color-mixin-base
           #:color
           #:color-3d-mixin
           #:color-mixin
           #:color-mixin-base
           #:dark
           #:fore-color
           #:fore-color-mixin
           #:fore-color-mixin-base
           #:frame-color
           #:frame-color-mixin
           #:frame-color-mixin-base
           #:interior-color
           #:interior-color-mixin
           #:interior-color-mixin-base
           #:light
           #:normal
           #:very-dark
           #:very-light
           
           ;; content
           #:content
           #:content-mixin
           #:content-mixin-base

           ;; extra
           #:extra
           #:extra-mixin
           #:extra-mixin-base
           
           ;; font
           #:font
           #:font-mixin
           #:font-mixin-base
           
           ;; manager
           #:manager
           #:manager-mixin
           #:manager-mixin-base
           
           ;; padding
           #:padding
           #:padding-bottom
           #:padding-h
           #:padding-left
           #:padding-mixin
           #:padding-mixin-base
           #:padding-right
           #:padding-top
           #:padding-v
           
           ;; parent
           #:owner
           #:parent
           #:parent-mixin
           #:parent-mixin-base
           
           ;; shortcuts
           #:convert-shortcuts
           #:mod2val
           #:shortcuts
           #:shortcuts-mixin
           #:shortcuts-mixin-base
           #:val2mod
           #:validate-shortcuts
           
           ;; spacing
           #:spacing
           #:spacing-bottom
           #:spacing-h
           #:spacing-left
           #:spacing-mixin
           #:spacing-mixin-base
           #:spacing-right
           #:spacing-top
           #:spacing-v
           
           ;; theme
           #:theme
           #:theme-mixin
           #:theme-mixin-base
           
           ;; title
           #:title
           #:title-mixin
           #:title-mixin-base
           
           ;; visible
           #:visible
           #:visible-mixin
           #:visible-mixin-base
           
           ;; ========== Widgets ==========
           ;; border
           #:border
           #:defborder
           
           ;; box
           #:box
           #:box-title-position-type
           #:defbox
           #:filled
           #:title-position
           #:validate-box-options
           
           ;; grid
           #:defgrid
           #:grid
           #:major
           #:minor
           
           ;; primitives
           #:%point2
           #:bottom
           #:defline
           #:defrectangle
           #:end
           #:h
           #:left
           #:line
           #:rectangle
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

           ;; window
           #:defwindow
           #:interior-color
           #:window
           
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
           #:slime-init
           
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
