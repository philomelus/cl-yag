(in-package #:cl-yag)

;;;; TYPES ====================================================================
;; Types used through out

(deftype coordinate () `(or (integer 0 ,most-positive-fixnum) (float 0 ,most-positive-short-float) ratio))
(deftype thickness-type () `(or (integer 0 ,most-positive-fixnum) (float 0 ,most-positive-short-float) ratio))
