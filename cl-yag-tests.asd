(in-package :asdf-user)
(defsystem "cl-yag-tests"
  :description "Test suite for the cl-yag system"
  :author "Russell E. Gibson <russg@rnstech.com>"
  :version "0.0.1"
  :license "MIT"
  :homepage "https://github.com/philomelus/cl-yag.git"
  :bug-tracker "https://github.com/philomelus/cl-yag/issues"
  :source-control (:git "https://github.com/philomelus/cl-yag.git")
  
  :depends-on ("cl-yag"
               "closer-mop"
               ;;:fiveam
               "verbose")

  :components ((:module "tests"
                :components ((:file "packages")
                             (:file "api")
                             (:file "cl-yag-tests" :depends-on ("api"))
                             (:file "box-tests" :depends-on ("api" "cl-yag-tests"))
                             (:file "layout-layout-tests" :depends-on ("api" "cl-yag-tests"))
                             (:file "grid-layout-tests" :depends-on ("api" "cl-yag-tests"))
                             (:file "grid-tests" :depends-on ("api" "cl-yag-tests"))
                             (:file "layout-tests" :depends-on ("api" "cl-yag-tests"))
                             (:file "prototype" :depends-on ("api" "cl-yag-tests"))
                             (:file "ruler-tests" :depends-on ("api" "cl-yag-tests"))
                             (:file "text-tests" :depends-on ("api" "cl-yag-tests"))
                             (:file "window-tests" :depends-on ("api" "cl-yag-tests"))))))
