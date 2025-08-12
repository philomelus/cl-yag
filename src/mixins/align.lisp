(in-package #:cl-yag)

;;;; align-mixin ==============================================================

(defclass align-mixin (h-align-mixin v-align-mixin)
  ())

(defmethod print-mixin ((o align-mixin) s)
  ;; Base mixins get printed via call-next-method, so nothing to do here
  (my-next-method))

;;;; h-align-mixin ============================================================

(defclass h-align-mixin ()
  ((h-align :initarg :h-align :initform :none :type keyword :accessor h-align)))

(defmethod print-mixin ((o h-align-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":h-align :~a" (string-downcase (h-align o)))
  (pprint-newline :linear s)
  (my-next-method))

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

(defmethod print-mixin ((o v-align-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":v-align :~a" (string-downcase (v-align o)))
  (pprint-newline :linear s)
  (my-next-method))

;; Only allow valid keywords
;; #+safety
(defmethod (setf v-align) :after (newval (obj v-align-mixin))
  (let ((msg "Expected :none, :top, :bottom, or :middle but got ~s"))
    (typecase newval
      (keyword (unless (member newval '(:none :top :bottom :middle))
                 (error msg newval)))
      (t (error msg newval))))
  (my-next-method))

