(defpackage :clg
  (:use #:cl)
  (:import-from #:cffi
                #:foreign-alloc
                #:foreign-enum-value
                #:foreign-free
                #:foreign-pointer
                #:foreign-slot-value
                #:null-pointer
                #:null-pointer-p)
  (:import-from #:event-glue
                #:bind
                #:data
                #:dispatch
                #:event
                #:trigger
                #:unbind
                #:unbind-all)
  ;; (:import-from #:cffi-object
  ;;               #:define-cobject-class)
  (:export #:main))

