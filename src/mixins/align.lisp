(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; align-mixin ==============================================================

(defclass align-mixin (h-align-mixin v-align-mixin)
  ())

;;;; h-align-mixin ============================================================

(defclass h-align-mixin ()
  ((h-align :initarg :h-align :initform :none :type keyword :accessor h-align)))

;; Only allow valid keywords
;; #+safety
(defmethod (setf h-align) :after (newval (obj h-align-mixin))
  (let ((msg "Expected :none, :left, :right, or :center but got ~s"))
    (typecase newval
      (keyword (unless (member newval '(:none :left :right :center))
                 (error msg newval)))
      (t (error msg newval))))
  (my-next-method))


;;;; v-align-mixin ============================================================

(defclass v-align-mixin ()
  ((v-align :initarg :v-align :initform :none :type keyword :accessor v-align)))

;; Only allow valid keywords
;; #+safety
(defmethod (setf v-align) :after (newval (obj v-align-mixin))
  (let ((msg "Expected :none, :top, :bottom, or :middle but got ~s"))
    (typecase newval
      (keyword (unless (member newval '(:none :top :bottom :middle))
                 (error msg newval)))
      (t (error msg newval))))
  (my-next-method))

