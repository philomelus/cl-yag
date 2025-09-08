(in-package :asdf-user)
(defpackage :cl-yag-tests
  (:use #:cl #:cl-yag)
  (:local-nicknames
   (#:a #:alexandria)
   (#:mop #:closer-mop)
   (#:v #:org.shirakumo.verbose))
  
  (:export #:box-tests-main
           #:column-layout-tests-main
           #:grid-layout-tests-main
           #:grid-tests-main
           #:layout-tests-main
           #:prototype-tests-main       ; Not a test
           #:ruler-tests-main
           #:text-test-main
           #:window-tests-main))

(in-package #:cl-yag-tests)
