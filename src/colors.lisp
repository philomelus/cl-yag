(in-package :cl-yag)

;;;; GLOBAL COLORS ============================================================

(defparameter +COLOR-BLACK+ (al:color-name "black"))
(defparameter +COLOR-WHITE+ (al:color-name "white"))

(defparameter +COLOR-AQUA+ (al:map-rgb 0 255 255))
(defparameter +COLOR-DARK-AQUA+ (al:map-rgb 0 191 191))
(defparameter +COLOR-LIGHT-AQUA+ (al:map-rgb 127 255 255))
(defparameter +COLOR-VERY-DARK-AQUA+ (al:map-rgb 0 127 127))
(defparameter +COLOR-VERY-LIGHT-AQUA+ (al:map-rgb 191 255 255))

(defparameter +COLOR-BLUE+ (al:map-rgb 0 0 255))
(defparameter +COLOR-DARK-BLUE+ (al:map-rgb 0 0 191))
(defparameter +COLOR-LIGHT-BLUE+ (al:map-rgb 159 159 255))
(defparameter +COLOR-VERY-DARK-BLUE+ (al:map-rgb 0 0 159))
(defparameter +COLOR-VERY-LIGHT-BLUE+ (al:map-rgb 191 191 255))

(defparameter +COLOR-GRAY+ (al:map-rgb 212 208 200))
(defparameter +COLOR-DARK-GRAY+ (al:map-rgb 128 128 128))
(defparameter +COLOR-LIGHT-GRAY+ (al:map-rgb 223 223 223))
(defparameter +COLOR-VERY-DARK-GRAY+ (al:map-rgb 95 95 95))
(defparameter +COLOR-VERY-LIGHT-GRAY+ (al:map-rgb 245 245 245))

(defparameter +COLOR-GREEN+ (al:map-rgb 0 255 0))
(defparameter +COLOR-DARK-GREEN+ (al:map-rgb 0 191 0))
(defparameter +COLOR-LIGHT-GREEN+ (al:map-rgb 127 255 127))
(defparameter +COLOR-VERY-DARK-GREEN+ (al:map-rgb 0 159 0))
(defparameter +COLOR-VERY-LIGHT-GREEN+ (al:map-rgb 191 255 191))

(defparameter +COLOR-PURPLE+ (al:map-rgb 255 0 255))
(defparameter +COLOR-DARK-PURPLE+ (al:map-rgb 191 0 191))
(defparameter +COLOR-LIGHT-PURPLE+ (al:map-rgb 255 159 255))
(defparameter +COLOR-VERY-DARK-PURPLE+ (al:map-rgb 159 0 159))
(defparameter +COLOR-VERY-LIGHT-PURPLE+ (al:map-rgb 255 191 255))

(defparameter +COLOR-RED+ (al:map-rgb 255 0 0))
(defparameter +COLOR-DARK-RED+ (al:map-rgb 191 0 0))
(defparameter +COLOR-LIGHT-RED+ (al:map-rgb 255 127 127))
(defparameter +COLOR-VERY-DARK-RED+ (al:map-rgb 159 0 0))
(defparameter +COLOR-VERY-LIGHT-RED+ (al:map-rgb 255 191 191))

(defparameter +COLOR-YELLOW+ (al:map-rgb 255 255 0))
(defparameter +COLOR-DARK-YELLOW+ (al:map-rgb 191 191 0))
(defparameter +COLOR-LIGHT-YELLOW+ (al:map-rgb 255 255 127))
(defparameter +COLOR-VERY-DARK-YELLOW+ (al:map-rgb 127 127 0))
(defparameter +COLOR-VERY-LIGHT-YELLOW+ (al:map-rgb 255 255 191))
