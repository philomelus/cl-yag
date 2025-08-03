
(load "cl-yag.asd")
(load "cl-yag-tests.asd")

(ql:quickload "cl-yag-tests")

(in-package :cl-yag-tests)

(uiop:quit (if (run-all-tests) 0 1))
