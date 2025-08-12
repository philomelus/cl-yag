(defsystem "cl-yag"
  :author "Russell E. Gibson <russg@rnstech.com>"
  :version "0.0.1"
  :license "MIT"
  :description "Common Lisp GUI on top of liballegro."
  :homepage "yeah, right, sure, I'll get right on that."
  :bug-tracker "Pffftt"
  :source-control (:git "https://github.com/philomelus/cl-yag.git")

  ;; Dependencies.
  :depends-on ("cl-liballegro"
               ;; "cffi-object"
               "documentation-utils"
               "verbose")

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                :components ((:file "packages")
                             (:file "constants")
                             (:file "macros")
                             (:file "generics")
                             (:file "utils")
                             (:file "allegro")
                             (:file "mixins/active")
                             (:file "mixins/align")
                             (:file "mixins/area")
                             (:file "mixins/border")
                             (:file "mixins/color")
                             (:file "mixins/container")
                             (:file "mixins/content")
                             (:file "mixins/enable")
                             (:file "mixins/font")
                             (:file "mixins/location")
                             (:file "mixins/manager")
                             (:file "mixins/padding")
                             (:file "mixins/parent")
                             (:file "mixins/spacing")
                             (:file "mixins/theme")
                             (:file "mixins/title")
                             (:file "mixins/visible")
                             (:file "layout")
                             (:file "theme")
                             (:file "manager")
                             (:file "text")
                             (:file "window")
                             (:file "cl-yag")
                             ;; (:file "cl-yag-docs")
                             ;; (:file "allegro-docs")
                             ;; (:file "constants-docs")
                             ;; (:file "generics-docs")
                             ;; (:file "layout-docs")
                             ;; (:file "manager-docs")
                             ;; (:file "text-docs")
                             ;; (:file "theme-docs")
                             ;; (:file "utils-docs")
                             ;; (:file "window-docs")
                             ))
               )
  
  :around-compile
  (lambda (next)
    ;; (proclaim '(optimize (compilation-speed 0) (debug 3) (safety 3) (space 0) (speed 0)))    
    (proclaim '(optimize (debug 3) (safety 3)))    
    (funcall next)))

