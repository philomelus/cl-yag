(defpackage :clg-utils
  (:use #:cl)
  (:import-from #:cffi
                #:foreign-enum-value
                #:null-pointer-p)
  (:export #:+KEY-B+
           #:+KEY-ESC+
           #:+KEY-MAX+
           #:+KEY-DOWN+
           #:+KEY-LEFT+
           #:+KEY-RIGHT+
           #:+KEY-UP+
           #:+KEY-X+
           #:between
           #:between-f
           #:color2assoc
           #:color2list
           #:color-a
           #:color-b
           #:color-g
           #:color-inverse
           #:color-r
           #:must-init
           #:rect-collide))

(defpackage :clg-mixins
  (:use #:cl)
  (:export #:active-mixin #:active
           #:align-mixin
           #:area-mixin #:area-bottom #:area-h #:area-height #:area-left #:area-right
                        #:area-top #:area-w #:area-width #:area-x #:area-y
           #:color-mixin #:color
           #:color-back-fore-mixin #:back-color #:fore-color
           #:enable-mixin #:enabled
           #:font-mixin #:font
           #:h-align-mixin #:h-align #:align-h
           #:location-mixin #:location-x #:location-y #:location
           #:padding-mixin #:padding #:padding-bottom #:padding-h #:padding-left
                           #:padding-right #:padding-top #:padding-v
           #:spaceing-mixin #:spacing #:spacing-bottom #:spacing-h #:spacing-left
                           #:spacing-right #:spacing-top #:spacing-v
           #:v-align-mixin #:v-align #:align-v
           #:visible-mixin #:visible))

(defpackage :clg
  (:use #:cl)
  (:import-from #:cffi
                #:null-pointer
                #:foreign-pointer)
  (:import-from #:clg-mixins
                #:active-mixin #:active
                #:area-mixin #:area-height #:area-left #:area-top #:area-width
                #:color-mixin #:color
                #:enable-mixin #:enabled
                #:font-mixin #:font
                #:h-align-mixin #:h-align
                #:v-align-mixin #:v-align
                #:visible-mixin #:visible)
  (:import-from #:event-glue
                #:bind
                #:data
                #:dispatch
                #:event
                #:trigger
                #:unbind
                #:unbind-all)
  (:import-from #:cffi-object
                #:define-cobject-class)
  (:import-from #:clg-utils
                #:color-inverse)
  (:export #:main))

(defpackage :clg-asteroids
  (:use #:cl)
  (:import-from #:cffi
                #:foreign-alloc
                #:foreign-slot-value
                #:null-pointer-p
                #:null-pointer
                #:foreign-pointer
                #:foreign-free
                #:foreign-enum-value)
  (:export #:main :as :asteroids-main))

(defpackage :clg-blastem
  (:use #:cl)
  (:import-from #:cffi
                #:foreign-alloc
                #:foreign-slot-value
                #:null-pointer-p
                #:null-pointer
                #:foreign-pointer
                #:foreign-free
                #:foreign-enum-value)
  (:import-from :clg-utils
                #:+KEY-MAX+
                #:+KEY-DOWN+
                #:+KEY-LEFT+
                #:+KEY-RIGHT+
                #:+KEY-UP+
                #:+KEY-X+
                #:+KEY-ESC+
                #:+KEY-B+
                #:must-init
                #:between
                #:between-f
                #:rect-collide)
  (:export #:main :as :blastem-main))

