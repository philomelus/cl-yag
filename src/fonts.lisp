(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defun default-mono-font (&optional (size -16))
  (al:load-ttf-font (asset "aurulent-sans-mono.ttf") size +TTF-MONOCHROME+))

(defun default-font (&optional (size -16))
  (al:load-ttf-font (asset "aurulent-sans.ttf") size +TTF-MONOCHROME+))
