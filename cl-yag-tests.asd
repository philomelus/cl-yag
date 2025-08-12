(in-package :asdf-user)
(defsystem "cl-yag-tests"
  :description "Test suite for the cl-yag system"
  :author "Russell E. Gibson <russg@rnstech.com>"
  :version "0.0.1"
  :depends-on (:cl-yag
               ;;:fiveam
               )
  ;;:pathname "tests/"
  :license "MIT"
  :components ((:module "tests"
                :serial t
                :components ((:file "packages")
                             (:file "cl-yag-tests")
                             (:file "text-tests"))))
  
  ;; :perform (test-op (o c) (symbol-call :cl-yag-tests :run-all-tests))
  )
