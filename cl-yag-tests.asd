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
                             (:file "cl-yag-tests")
                             (:file "text-tests"))))
  
  ;; :perform (test-op (o c) (symbol-call :cl-yag-tests :run-all-tests))
  )
