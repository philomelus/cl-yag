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
  
  (a:with-gensyms (lobject cobject)
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

;;;; various ==================================================================

(defmacro theme-field (field obj)
  "Return value of slot within object. If the value is nil, locate theme for
object and return the slot value from it.  Field should be an accessor."
  
  (a:with-gensyms (instance value theme)
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
  
  (a:with-gensyms (instance value)
    `(let* ((,instance ,obj)
            (,value (,field ,instance)))
       (when (eql ,value nil)
         (setq value (,field ,theme-obj)))
       ,value)))

;;;; with-* ===================================================================

(defmacro with-area-border ((left top right bottom) object &body body)
  "Provide left, top, right, and bottom coordinates of object, removing border
and paddiing space."
  
  (a:with-gensyms (instance bl bt br bb)
    `(let ((,instance ,object))
      (let ((,left (left ,instance))
            (,top (top ,instance))
            (,right (right ,instance))
            (,bottom (bottom ,instance))
            (,bl (border-left ,instance))
            (,bt (border-top ,instance))
            (,br (border-right ,instance))
            (,bb (border-bottom ,instance)))
        (declare (dynamic-extent ,bl ,bt ,br ,bb))
        (declare (ignorable ,left ,top ,right ,bottom))
        (unless (eql ,bl nil)
          (incf ,left (thickness ,bl)))
        (unless (eql ,bt nil)
          (incf ,top (thickness ,bt)))
        (unless (eql ,br nil)
          (decf ,right (thickness ,br)))
        (unless (eql ,bb nil)
          (decf ,bottom (thickness ,bb)))
        ,@body))))

(defmacro with-area-border-and-spacing ((left top right bottom) object &body body)
  "Provide left, top, right, and bottom coordinates of object, removing border,
padding, and spacing of object."
  
  (a:with-gensyms (instance bl bt br bb padding)
    `(let ((,instance ,object))
       (let ((,left (left ,instance))
             (,top (top ,instance))
             (,right (right ,instance))
             (,bottom (bottom ,instance)))
         (declare (ignorable ,left ,top ,right ,bottom))
         (when (typep ,instance 'border-mixin)
           (let ((,bl (border-left ,instance))
                 (,bt (border-top ,instance))
                 (,br (border-right ,instance))
                 (,bb (border-bottom ,instance))
                 (,padding (typep ,instance 'padding-mixin)))
             (declare (dynamic-extent ,bl ,bt ,br ,bb ,padding))
             (unless (eql ,bl nil)
               (incf ,left (thickness ,bl))
               (when ,padding
                 (incf ,left (padding-left ,instance))))
             (unless (eql ,bt nil)
               (incf ,top (thickness ,bt))
               (when ,padding
                (incf ,top (padding-top ,instance))))
             (unless (eql ,br nil)
               (decf ,right (thickness ,br))
               (when ,padding
                (decf ,right (padding-right ,instance))))
             (unless (eql ,bb nil)
               (decf ,bottom (thickness ,bb))
               (when ,padding
                (decf ,bottom (padding-bottom ,instance))))))
         ,@body))))

(defmacro with-area-and-spacing ((left top right bottom) object &body body)
  "Provide left, top, right, and bottom from object area removing spacing."

  (a:with-gensyms (instance)
    `(let ((,instance ,object))
       (let ((,left (left ,instance))
             (,top (top ,instance))
             (,right (right ,instance))
             (,bottom (bottom ,instance)))
         (declare (ignorable ,left ,top ,right ,bottom))
         (when (typep ,instance 'spacing-mixin)
           (incf ,left (spacing-left ,instance))
           (incf ,top (spacing-top ,instance))
           (decf ,right (spacing-right ,instance))
           (decf ,bottom (spacing-bottom ,instance)))
         ,@body))))

(defmacro with-local-accessors ((&rest slots) instance &body body)
  "Like with-slots, but slots are cached in local variables and changes are not
updated on setq/setf."
  
  (a:with-gensyms (in)
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

  (a:with-gensyms (in)
    `(let ((,in ,instance))
       (let (,@(mapcar #'(lambda (obj)
                           (if (consp obj)
                               `(,(first obj) (slot-value ,in ',(second obj)))
                               `(,obj (slot-value ,in ',obj))))
                       slots))
         (declare (dynamic-extent ,@(mapcar #'(lambda (obj)
                                                (if (consp obj)
                                                    (first obj)
                                                    obj))
                                            slots)))
         ,@body))))

;; BUGBUG: TODO: slots evaluated more than once
(defmacro with-local-slots-update ((&rest slots) instance &body body)
  "Like with slots, but slots are cached locally until BODY exits, at which point
the slots are updated from local cache. The point of this is that there are
many times when one needs to perform a significant amount of manipulation of
the slot value before the final new value is known. In those cases, there is
no reason to perform slot access overhead over and over; Perform the
manipulations on a local dynamic variable, then update the slot when complete,
saving all the slot access overhead."

  (let ((in (gensym)))
    `(let ((,in ,instance))
       (let (,@(mapcar #'(lambda (obj)
                           (if (consp obj)
                               `(,(first obj) (slot-value ,in ',(second obj)))
                               `(,obj (slot-value ,in ',obj))))
                       slots))
         (unwind-protect (progn ,@body)
           (progn
             (setf ,@(mapc #'(lambda (obj)
                               `(slot-value ,in ',(second obj)) `,(first obj))
                           slots))))))))

;; BUGBUG: TODO: fields evaluated more than once
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
  
  (a:with-gensyms (instance theme)
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

;; BUGBUG: TODO: fields evaluated more than once
(defmacro with-object-and-theme ((&rest fields) object theme &body body)
  "Create local instances of theme related slots from object.  If the object's
slot value is nil, use the slot from the theme."
  
  (a:with-gensyms (instance theme-obj)
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

(defmacro with-parent-area (name object &body body)
  "Create binding NAME for first parent of OBJECT that is derived from
area-mixin."
  
  (a:with-gensyms (instance)
    `(let* ((,instance ,object)
            (,name (find-parent-area-or-layout ,instance)))
       ,@body)))

(defmacro with-parent-content (name object &body body)
  "Create binding NAME for first parent of OBJECT that is derived from
content-mixin."
  
  (a:with-gensyms (instance)
    `(let* ((,instance ,object)
            (,name (find-parent-content object)))
       ,@body)))
