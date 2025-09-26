(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

;;;; H-ALIGN-MIXIN-BASE =======================================================

(defclass h-align-mixin-base ()
  ()
  (:documentation "Mixin that lets other objects know that the derived object has the ability to
be aligned horizontally."))

;;;; H-ALIGN-MIXIN ============================================================

(deftype h-align-type () '(member :none :left :center :right))

(defclass h-align-mixin (h-align-mixin-base)
  ((h-align :type h-align-type :initarg :h-align :initform :none :accessor h-align)))

;; Only allow valid keywords
;; #+safety
(defmethod (setf h-align) :after (newval (obj h-align-mixin))
  (let ((msg "Expected :none, :left, :right, or :center but got ~s"))
    (typecase newval
      (keyword (unless (member newval '(:none :left :right :center))
                 (error msg newval)))
      (t (error msg newval))))
  (my-next-method))


;;;; V-ALIGN-MIXIN-BASE =======================================================

(defclass v-align-mixin-base ()
  ()
  (:documentation "Mixin that lets other objects know that the derived object has the ability to
be aligned vertically."))

;;;; V-ALIGN-MIXIN ============================================================

(deftype v-align-type () '(member :none :top :middle :bottom))

(defclass v-align-mixin (v-align-mixin-base)
  ((v-align :type v-align-type :initarg :v-align :initform :none :accessor v-align)))

;; Only allow valid keywords
;; #+safety
(defmethod (setf v-align) :after (newval (obj v-align-mixin))
  (let ((msg "Expected :none, :top, :bottom, or :middle but got ~s"))
    (typecase newval
      (keyword (unless (member newval '(:none :top :bottom :middle))
                 (error msg newval)))
      (t (error msg newval))))
  (my-next-method))

