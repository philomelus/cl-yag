(in-package #:cl-yag)

;;;; container-mixin ==========================================================
;;;; Controls area for contained objects

(defclass container-mixin (area-mixin
                           content-mixin)
  ())

(defmethod print-mixin ((o container-mixin) s)
  ;; Base mixins get printed via call-next-method, so nothing to do here
  (my-next-method))

(defmethod initialize-instance :after ((obj container-mixin) &key)
  ;; Take on the area of our (eventual) parent if exists
  (let ((pam (find-parent-area-mixin obj)))
    (if (not (eq pam nil))
        (calc-area pam obj)))
  
  (my-next-method))

