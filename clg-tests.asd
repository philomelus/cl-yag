(in-package :asdf-user)
(defsystem "clg-tests"
  :description "Test suite for the clg system"
  :author "Russ Gibson <russg@rnstech.com>"
  :version "0.0.1"
  :depends-on (:clg
               :fiveam)
  :pathname "tests/"
  :license "MIT"
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "packages")
                             (:file "clg-tests"))))

  :perform (test-op (o c) (symbol-call :clg-tests :run-all-tests)))
