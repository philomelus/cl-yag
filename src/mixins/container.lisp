(in-package #:cl-yag)

;;;; container-mixin ==========================================================
;;;; Controls area for contained objects

(defclass container-mixin (area-mixin
                           content-mixin)
  ())

(defmethod print-mixin ((o container-mixin) &optional s)
  (declare (ignore s))
  ;; Base mixins get printed via call-next-method, so nothing to do here
  (my-next-method))

