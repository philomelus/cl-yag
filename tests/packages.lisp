(in-package :asdf-user)
(defpackage :cl-yag-tests
  (:use #:cl #:cl-yag)
  (:local-nicknames
   (#:v #:org.shirakumo.verbose)
   (#:mop #:closer-mop))
  
  ;; (:import-from #:cl-yag
  ;;               ;; GUI classes / Macros
  ;;               #:defactive-text
  ;;               #:defborder
  ;;               #:defcolumn-layout
  ;;               #:defwindow
  ;;               #:manager

  ;;               ;; Classes
                
  ;;               ;; Class generics
  ;;               #:border
  ;;               #:fore-color
  ;;               #:process
  ;;               #:theme

  ;;               ;; Other generics
  ;;               #:on-resize
  ;;               #:paint
  ;;               #:process-events
                
  ;;               ;; Misc
                
  ;;               ;; Allegro specific
  ;;               #:display-event-height
  ;;               #:display-event-width
  ;;               #:display-event-x
  ;;               #:display-event-y
  ;;               #:event-type
  ;;               #:+OP-ADD+
  ;;               #:+BLEND-ONE+
  ;;               #:+BLEND-ZERO+

  ;;               #:height
  ;;               #:width
  ;;               #:left
  ;;               #:top
  ;;               #:defmanager
  ;;               #:padding
  ;;               #:content
  ;;               #:defgrid
  ;;               #:defruler
  ;;               )
  
  (:export #:text-test-main))

(in-package #:cl-yag-tests)
