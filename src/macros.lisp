(in-package #:cl-yag)

;;;; macros ===================================================================

(defmacro my-next-method ()
  `(if (next-method-p) (call-next-method)))

;;;------------------------------------------------------------------
;; For print-mixin (and print-object sometimes)

(defmacro pprint-color (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a (~a) " (symbol-name ',field) (print-color (slot-value ,object ',field)))
     (pprint-newline :mandatory ,stream)))

(defmacro pprint-color-nil (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a " (symbol-name ',field))
     (with-slots (,field) ,object
       (if (eql ,field nil)
           (format ,stream "NIL ")
           (format ,stream "(~a) " (print-color ,field))))
     (pprint-newline :mandatory ,stream)))

(defmacro pprint-field (field object stream &key (fmt "~a"))
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream (concatenate 'string ":~a " ,fmt " ") (symbol-name ',field) (slot-value ,object ',field))
     (pprint-newline :mandatory ,stream)))

(defmacro pprint-field-keyword (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a :~a" (symbol-name ',field) (slot-value ,object ',field))
     (pprint-newline :mandatory ,stream)))

(defmacro pprint-field-nil (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a " (symbol-name ',field))
     (with-slots (,field) ,object
       (if (eql ,field nil)
           (format ,stream "NIL ")
           (format ,stream "~a " ,field)))
     (pprint-newline :mandatory ,stream)))

(defmacro pprint-object (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a (~a) " (symbol-name ',field) (slot-value ,object ',field))
     (pprint-newline :mandatory ,stream)))

(defmacro pprint-object-nil (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a " (symbol-name ',field))
     (with-slots (,field) ,object
       (if (eql ,field nil)
           (format ,stream "NIL ")
           (format ,stream "(~a) " ,field)))
     (pprint-newline :mandatory ,stream)))

(defmacro pprint-raw (field object stream)
  `(progn
     (pprint-index :current 0 ,stream)
     (format ,stream ":~a ~a" (symbol-name ',field) (print-raw-object (slot-value ,object ',field)))
     (pprint-newline :mandatory ,stream)))

(defmacro pprint-raw-nil (field object stream)
  `(progn
     (pprint-indent :current 0 ,stream)
     (format ,stream ":~a " (symbol-name ',field))
     (with-slots (,field) ,object
       (if (eql ,field nil)
           (format ,stream "NIL ")
           (format ,stream "~a " (print-raw-object ,field))))
     (pprint-newline :mandatory ,stream)))

;;;; various ==================================================================

(defmacro theme-field (field obj)
  `(let ((th-ob (,field ,obj)))
     (when (eql th-ob nil)
       (setq th-ob (,field (find-theme ,obj))))
     th-ob))

(defmacro theme-field-object (field obj theme-obj)
  `(let ((th-ob (,field ,obj)))
     (when (eql th-ob nil)
       (setq th-ob (,field ,theme-obj)))
     th-ob))

;;;; with-* ===================================================================

(defmacro with-area ((left top width height) object &body body)
  "Creates local bingings of let, top, width, and height of an area-mixin"
  
  ;; `(with-slots ((,left left) (,top top) (,width width) (,height height))
  ;;      ,object
  ;;    ,@body)

  (let ((instance (gensym)))
    `(let ((,instance ,object))
       (with-local-slots ((,left left) (,top top) (,width width) (,height height))
                         ,instance
         ,@body))))

(defmacro with-area-rb ((left top right bottom) object &body body)
  "Creates local bindings of left, top, and local bindings of result of right, and
bottom for an area-mixin."
  
  (let ((instance (gensym)))
    `(let ((,instance ,object))
       (with-local-slots ((,left left) (,top top)) ,instance
         (let ((,right (right ,instance))
               (,bottom (bottom ,instance)))
           ,@body)))))

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

(defmacro with-local-accessors ((&rest slots) instance &body body)
  "Like with-slots, but slots are cached in local variables and changes are not
updated on setq/setf."
  
  (let ((in (gensym)))
    `(let ((,in ,instance))
       (let (,@(mapcar #'(lambda (obj)
                           (if (consp obj)
                               `(,(first obj) (,(second obj) ,in))
                               (error "malformed slots")))
                       slots))
         ,@body))))

(defmacro with-local-slots ((&rest slots) instance &body body)
  "Like with-slots, but slots are cached in local variables and changes are not
updated on setq/setf."
  
  (let ((in (gensym)))
    `(let ((,in ,instance))
       (let (,@(mapcar #'(lambda (obj)
                           (if (consp obj)
                               `(,(first obj) (slot-value ,in ',(second obj)))
                               (error "malformed slots")))
                       slots))
         ,@body))))

;; BUGBUG: TODO:  Args evaluated more than one time
(defmacro with-theme-object ((&rest fields) object &body body)
  (let ((instance (gensym))
        (theme (gensym)))
    `(let ((,instance ,object))
       (let (,@(mapcar #'(lambda (f)
                           `(,(first f) (,(second f) ,instance)))
                       fields))
         (when (or ,@(mapcar #'(lambda (f)
                                 `(eql ,(first f) nil))
                             fields))
           (let ((,theme (find-theme ,instance)))
             ,@(mapcar #'(lambda (f)
                           `(when (not ,(first f))
                              (setq ,(first f) (,(second f) ,theme))))
                       fields)))
         ,@(mapcar #'(lambda (f)
                       `(assert (not (eql ,(first f) nil))))
                   fields)
         ,@body))))

(defmacro with-theme ((&rest fields) theme-object &body body)
  (let ((instance (gensym)))
    `(let ((,instance ,theme-object))
       (let (,@(mapcar #'(lambda (f)
                           `(,(first f) (,(second f) ,instance)))
                       fields))
         ,@(mapcar #'(lambda (f)
                       `(assert (not (eql ,(first f) nil))))
                   fields)
         ,@body))))

