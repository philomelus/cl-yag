
(load "clg.asd")
(load "clg-tests.asd")

(ql:quickload "clg-tests")

(in-package :clg-tests)

(uiop:quit (if (run-all-tests) 0 1))
