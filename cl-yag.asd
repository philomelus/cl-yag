(in-package :asdf-user)
(defsystem "cl-yag"
  :author "Russell E. Gibson <russg@rnstech.com>"
  :version "0.0.1"
  :license "MIT"
  :description "Common Lisp GUI on top of liballegro."
  :homepage "https://github.com/philomelus/cl-yag.git"
  :bug-tracker "https://github.com/philomelus/cl-yag/issues"
  :source-control (:git "https://github.com/philomelus/cl-yag.git")

  ;; Dependencies.
  :depends-on ("alexandria"
               "bordeaux-threads"
               "cl-liballegro"
               "cffi-object"
               "closer-mop"
               "verbose")

  ;; Project stucture.
  :components ((:module "src"
                :components ((:file "packages")
                             
                             (:file "macros")
                             (:file "generics")
                             
                             (:file "utils")

                             (:file "forwards") ; ftypes for functions that are needed before the code can be loaded
                             
                             ;; Ease of use ...
                             (:file "allegro")

                             ;; Composition objects
                             (:file "mixins/active")
                             (:file "mixins/align")
                             (:file "mixins/area")
                             (:file "mixins/border")
                             (:file "mixins/child")
                             (:file "mixins/color")
                             (:file "mixins/container")
                             (:file "mixins/content")
                             (:file "mixins/enable")
                             (:file "mixins/font")
                             (:file "mixins/layout")
                             (:file "mixins/manager")
                             (:file "mixins/padding")
                             (:file "mixins/parent")
                             (:file "mixins/ready")
                             (:file "mixins/shortcuts")
                             (:file "mixins/spacing")
                             (:file "mixins/style")
                             (:file "mixins/theme")
                             (:file "mixins/title")
                             (:file "mixins/visible")

                             (:file "primitives")
                             (:file "rect")
                             
                             (:file "manager")
                             (:file "fonts")

                             ;; Layouts
                             (:file "layout")
                             (:file "column-layout")
                             (:file "grid-layout")
                             (:file "row-layout")

                             ;; Interactive objects
                             (:file "box")
                             (:file "grid")
                             (:file "ruler")
                             (:file "text" :depends-on ("theme"))
                             (:file "window")

                             ;; theme needs to be last as it relies on all the
                             ;; other objects -theme-mixins
                             (:file "theme")
                             (:file "theme-vars" :depends-on ("box" "grid" "ruler" "text" "theme" "window"))

                             ;; Used during development, and relies on ALL
                             ;; other files
                             (:file "cl-yag"))))
  
  :around-compile
  (lambda (next)
    (proclaim '(optimize (compilation-speed 0) (debug 3) (safety 3) (space 1) (speed 0)))    
    ;;(proclaim '(optimize (debug 3) (safety 3)))    
    (funcall next)))

