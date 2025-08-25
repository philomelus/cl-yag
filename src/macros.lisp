(in-package #:cl-yag)

;;;; macros ===================================================================

(defmacro with-border-area ((left top right bottom) object &body body)
  "Provide left, top, right, and bottom coordinates of object, adjusting for border."
  (let ((bl (gensym))
        (bt (gensym))
        (br (gensym))
        (bb (gensym)))
    `(let ((,left (left ,object))
           (,top (top ,object))
           (,right (right ,object))
           (,bottom (bottom ,object))
           (,bl (border-left ,object))
           (,bt (border-top ,object))
           (,br (border-right ,object))
           (,bb (border-bottom ,object)))
       (unless (eql ,bl nil)
         (incf ,left (width ,bl)))
       (unless (eql ,bt nil)
         (incf ,top (width ,bt)))
       (unless (eql ,br nil)
         (decf ,right (width ,br)))
       (unless (eql ,bb nil)
         (decf ,bottom (width ,bb)))
       ,@body)))

(defmacro with-border-and-padding-area ((left top right bottom) object &body body)
  "Provide left, top, right, and bottom coordinates of object, adjusting for
border and spacing of object."
  `(with-border-area (,left ,top ,right ,bottom) object
     (incf ,left (padding-left ,object))
     (incf ,top (padding-top ,object))
     (decf ,right (padding-right ,object))
     (decf ,bottom (padding-bottom ,object))
     ,@body))

(defmacro my-next-method ()
  `(if (next-method-p) (call-next-method)))

;;;------------------------------------------------------------------
;; For print-mixin (and print-object sometimes)

(defmacro pprint-color (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a (~a) " (symbol-name ',field) (print-color (slot-value ,object ',field)))
     (pprint-newline :linear ,stream)))

(defmacro pprint-color-nil (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a " (symbol-name ',field))
     (with-slots (,field) ,object
       (if (eql ,field nil)
           (format ,stream "NIL ")
           (format ,stream "(~a) " (print-color ,field))))
     (pprint-newline :linear ,stream)))

(defmacro pprint-field (field object stream &key (fmt "~a"))
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream (concatenate 'string ":~a " ,fmt " ") (symbol-name ',field) (slot-value ,object ',field))
     (pprint-newline :linear ,stream)))

(defmacro pprint-field-keyword (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a :~a" (symbol-name ',field) (slot-value ,object ',field))
     (pprint-newline :linear ,stream)))

(defmacro pprint-field-nil (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a " (symbol-name ',field))
     (with-slots (,field) ,object
       (if (eql ,field nil)
           (format ,stream "NIL ")
           (format ,stream "~a " ,field)))
     (pprint-newline :linear ,stream)))

(defmacro pprint-object (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a (~a) " (symbol-name ',field) (slot-value ,object ',field))
     (pprint-newline :linear ,stream)))

(defmacro pprint-object-nil (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a " (symbol-name ',field))
     (with-slots (,field) ,object
       (if (eql ,field nil)
           (format ,stream "NIL ")
           (format ,stream "(~a) " ,field)))
     (pprint-newline :linear ,stream)))

(defmacro pprint-raw (field object stream)
  `(progn
     (pprint-index :current 0 ,stream)
     (format ,stream ":~a ~a" (symbol-name ',field) (print-raw-object (slot-value ,object ',field)))
     (pprint-newline :linear ,stream)))

(defmacro pprint-raw-nil (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a " (symbol-name ',field))
     (with-slots (,field) ,object
       (if (eql ,field nil)
           (format ,stream "NIL ")
           (format ,stream "~a " (print-raw-object ,field))))
     (pprint-newline :linear ,stream)))

;;;; with-* ===================================================================

(defmacro with-area ((left top width height) object &body body)
  `(with-slots ((,left left) (,top top) (,width width) (,height height))
       ,object
     ,@body))

