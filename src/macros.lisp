(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; macros ===================================================================

(defmacro cleanup-method (func args)
  "Calls remove-method after calling find-method to locate method.

func should be symbol of function (not function pointer).
args should be the specialization list.
If :where is specified it should be the type of method (:before, :after, etc.)
(where unimplement as haven't needed it ... yet"

  `(remove-method #',func (find-method #',func () ,args)))

(defmacro foro (object)
  "Returns object or first object.

If object is an atom, returns it.
If object is an atom, and is a symbol, returns value of symbol.
If object is list, and first item in list is object, returns it.
If object is list, and first tiem is symbol, returns value of it."
  
  (let ((lobject (gensym))
        (cobject (gensym)))
    `(let ((,lobject ,object))
       (if (consp ,lobject)
           (let ((,cobject (first ,lobject)))
             (when (symbolp ,cobject)
               (setq ,cobject (symbol-value ,cobject)))
             ,cobject)
           (if (symbolp ,lobject)
               (symbol-value ,lobject)
               ,lobject)))))

(defmacro my-next-method ()
  `(if (next-method-p) (call-next-method)))

;;;------------------------------------------------------------------

;; For print-mixin (and print-object sometimes)
;; Seemed like a good idea, but I'm not so sure now.  print-object
;; is a bit of a pain to work with (which probably means I still'
;; don't fully understand how it works)

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
  "Return value of slot within object. If the value is nil, locate theme for
object and return the slot value from it.  Field should be an accessor."
  
  (let ((instance (gensym))
        (value (gensym))
        (theme (gensym)))
    `(let* ((,instance ,obj)
            (,value (,field ,instance)))
       (when (eql ,value nil)
         (let ((,theme (find-theme ,instance)))
           (assert (not (eql ,theme nil)))
           (setq ,value (,field ,theme))))
       ,value)))

(defmacro theme-field-object (field obj theme-obj)
  "Return value of slot within object.  If the value is nil, return slot value
from theme. Field should be an accessor."
  
  (let ((instance (gensym))
        (value (gensym)))
    `(let* ((,instance ,obj)
            (,value (,field ,instance)))
       (when (eql ,value nil)
         (setq value (,field ,theme-obj)))
       ,value)))

;;;; with-* ===================================================================

(defmacro with-area-border ((left top right bottom) object &body body)
  "Provide left, top, right, and bottom coordinates of object, removing border space."
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

(defmacro with-area-border-and-padding ((left top right bottom) object &body body)
  "Provide left, top, right, and bottom coordinates of object, removing border and
 spacing of object."
  
  `(with-area-border (,left ,top ,right ,bottom) object
     (incf ,left (padding-left ,object))
     (incf ,top (padding-top ,object))
     (decf ,right (padding-right ,object))
     (decf ,bottom (padding-bottom ,object))
     ,@body))

(defmacro with-area-padding ((left top right bottom) object &body body)
  "Provide left, top, right, and bottom coordiates of object, removing padding
space."
  
  (let ((instance (gensym)))
    `(let ((,instance ,object))
       (with-area-rb (,left ,top ,right ,bottom) ,instance
         (incf ,left (padding-left ,instance))
         (incf ,top (padding-top ,instance))
         (decf ,right (padding-right ,instance))
         (decf ,bottom (padding-bottom ,instance))
         ,@body))))

(defmacro with-local-accessors ((&rest slots) instance &body body)
  "Like with-slots, but slots are cached in local variables and changes are not
updated on setq/setf."
  
  (let ((in (gensym)))
    `(let ((,in ,instance))
       (let (,@(mapcar #'(lambda (obj)
                           (if (consp obj)
                               `(,(first obj) (,(second obj) ,in))
                               `(,obj (,obj ,in))))
                       slots))
         ,@body))))

(defmacro with-local-slots ((&rest slots) instance &body body)
  "Like with-slots, but slots are cached in local variables and slots are not
updated (changes are local only)."

  (let ((in (gensym)))
    `(let ((,in ,instance))
       (let (,@(mapcar #'(lambda (obj)
                           (if (consp obj)
                               `(,(first obj) (slot-value ,in ',(second obj)))
                               `(,obj (slot-value ,in ',obj))))
                       slots))
         ,@body))))

(defmacro with-theme ((&rest fields) theme-object &body body)
  "Create local instances of theme slots."
  (let ((instance (gensym)))
    `(let ((,instance ,theme-object))
       (let (,@(mapcar #'(lambda (f)
                           (if (consp f)
                               `(,(first f) (,(second f) ,instance))
                               `(,f (,f ,instance))))
                       fields))
         ,@(mapcar #'(lambda (f)
                       (if (atom f)
                           `(assert (not (eql ,f nil)))
                           `(assert (not (eql ,(first f) nil)))))
                   fields)
         ,@body))))

;; BUGBUG: TODO:  Args evaluated more than one time
(defmacro with-object-or-theme ((&rest fields) object &body body)
  "Create local instances of theme related slots from object.  If the object's
slot value is nil, locate the active theme for object and get the value from it."
  
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

(defmacro with-object-and-theme ((&rest fields) object theme &body body)
  "Create local instances of theme related slots from object.  If the object's
slot value is nil, use the slot from the theme."
  
  (let ((instance (gensym))
        (theme-obj (gensym)))
    `(let ((,instance ,object))
       (let (,@(mapcar #'(lambda (f)
                           (if (atom f)
                               `(,f (,f ,instance))
                               `(,(first f) (,(second f) ,instance))))
                       fields))
         (when (or ,@(mapcar #'(lambda (f)
                                 (if (atom f)
                                     `(eql ,f nil)
                                     `(eql ,(first f) nil)))
                             fields))
           (let ((,theme-obj ,theme))
             ,@(mapcar #'(lambda (f)
                           (if (atom f)
                               `(when (eql ,f nil)
                                  (setq ,f (,f ,theme-obj)))
                               `(when (eql ,(first f) nil)
                                  (setq ,(first f) (,(second f) ,theme-obj)))))
                       fields)))
         ,@(mapcar #'(lambda (f)
                       (if (atom f)
                           `(assert (not (eql ,f nil)))
                           `(assert (not (eql ,(first f) nil)))))
                   fields)
         ,@body))))
