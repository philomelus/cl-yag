(in-package :asdf-user)

(defsystem "clg"
  :author "Russ Gibson <russg@rnstech.com>"
  :version "0.0.1"
  :license "MIT"
  :description "Common Lisp GUI on top of liballegro."
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")

  ;; Dependencies.
  :depends-on ()

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "clg"))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "clg"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "clg:main")
