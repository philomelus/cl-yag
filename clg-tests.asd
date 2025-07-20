(in-package :asdf-user)
(defsystem "clg-tests"
  :description "Test suite for the clg system"
  :author "Russ Gibson <russg@rnstech.com>"
  :version "0.0.1"
  :depends-on (:clg
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-clg"))))

  ;; The following would not return the right exit code on error, but still 0.
  ;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  )
