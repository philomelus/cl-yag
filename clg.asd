(defsystem "clg"
  :author "Russell E. Gibson <russg@rnstech.com>"
  :version "0.0.1"
  :license "MIT"
  :description "Common Lisp GUI on top of liballegro."
  :homepage "yeah, right, sure, I'll get right on that."
  :bug-tracker "Pffftt"
  :source-control (:git "https://github.com/philomelus/clg.git")

  ;; Dependencies.
  :depends-on ("cl-liballegro"
               "event-glue"
               "cffi-object"
               "documentation-utils")

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "macros")
                             (:file "clg")
                             (:file "clg-docs")
                             (:file "utils")
                             (:file "utils-docs")
                             (:file "generics")
                             (:file "generics-docs")
                             (:file "mixins")
                             (:file "mixins-docs")
                             (:file "manager")
                             (:file "manager-docs")
                             (:file "grid")
                             (:file "grid-docs")
                             (:file "text")
                             (:file "text-docs")
                             (:file "window")
                             (:file "window-docs")

                             ;; (:file "asteroids")
                             ;; (:file "asteroids-docs")
                             
                             ;; (:file "blastem")
                             ;; (:file "blastem-docs")
                             )))
  :around-compile
  (lambda (next)
    ;; (proclaim '(optimize (compilation-speed 0) (debug 3) (safety 3) (space 0) (speed 0)))    
    (proclaim '(optimize (debug 3) (safety 3)))    
    (funcall next)))

