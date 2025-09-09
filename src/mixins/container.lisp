(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; container-mixin ==========================================================
;;;; Controls area for contained objects

(defclass container-mixin (area-mixin
                           content-mixin)
  ())

(defmethod print-mixin ((o container-mixin) &optional s)
  (declare (ignore s))
  ;; Base mixins get printed via call-next-method, so nothing to do here
  (my-next-method))

