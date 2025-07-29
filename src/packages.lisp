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

(defpackage :clg
  (:use #:cl)
  (:import-from #:cffi
                #:null-pointer
                #:foreign-pointer)
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

