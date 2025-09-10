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
               "fiveam"
               "verbose")

  :components ((:module "tests"
                :components ((:file "packages")
                             
                             (:module "ui"
                              :components ((:file "api")
                                           (:file "tests-main" :depends-on ("api"))
                                           (:file "box-tests" :depends-on ("api" "tests-main"))
                                           (:file "column-layout-tests" :depends-on ("api" "tests-main"))
                                           (:file "grid-layout-tests" :depends-on ("api" "tests-main"))
                                           (:file "grid-tests" :depends-on ("api" "tests-main"))
                                           (:file "layout-tests" :depends-on ("api" "tests-main"))
                                           (:file "prototype" :depends-on ("api" "tests-main"))
                                           (:file "ruler-tests" :depends-on ("api" "tests-main"))
                                           (:file "text-tests" :depends-on ("api" "tests-main"))
                                           (:file "window-tests" :depends-on ("api" "tests-main"))))
                             
                             (:module "auto"
                              :components ((:file "grid-layout")))))))
