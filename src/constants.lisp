(in-package #:cl-yag)

(defconstant +LAYOUT-HEIGHT-CALC+ -1)
(defconstant +LAYOUT-LEFT-CALC+ -1)
(defconstant +LAYOUT-TOP-CALC+ -1)
(defconstant +LAYOUT-WIDTH-CALC+ -1)

;;;; from cl-liballegro =======================================================

(defconstant +KEY-MAX+ (foreign-enum-value 'al::keycodes :key-max))

(defconstant +KEY-DOWN+ (foreign-enum-value 'al::keycodes :down))
(defconstant +KEY-LEFT+ (foreign-enum-value 'al::keycodes :left))
(defconstant +KEY-RIGHT+ (foreign-enum-value 'al::keycodes :right))
(defconstant +KEY-UP+ (foreign-enum-value 'al::keycodes  :up))
(defconstant +KEY-X+ (foreign-enum-value 'al::keycodes :x))
(defconstant +KEY-ESC+ (foreign-enum-value 'al::keycodes  :escape))
(defconstant +KEY-B+ (foreign-enum-value 'al::keycodes :b))

(defconstant +MOUSE-BUTTON-LEFT+ 1)
(defconstant +MOUSE-BUTTON-RIGHT+ 2)
(defconstant +MOUSE-BUTTON-MIDDLE+ 3)

