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
                             (:file "asteroids")
                             (:file "asteroids-docs")
                             (:file "blastem")
                             (:file "blastem-docs")
                             (:file "generics")
                             (:file "generics-docs")
                             (:file "mixins")
                             (:file "mixins-docs")
                             (:file "utils")
                             (:file "utils-docs")
                             (:file "clg")
                             (:file "clg-docs")))))

