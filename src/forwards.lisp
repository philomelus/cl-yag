(in-package :cl-yag)

;; theme.lisp
(declaim (ftype (function (t) t) find-theme))

;; layout.lisp
(declaim (ftype (function (t &key (:failp boolean)) t)
                find-parent-area-or-layout
                find-parent-content
                find-parent-layout))

