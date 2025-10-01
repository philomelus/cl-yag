(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; THEME-MIXIN-BASE =========================================================

(defclass theme-mixin-base ()
  ()
  (:documentation "Subclasses of THEME-MIXIN-BASE have object instance theme support."))

;;;; THEME-MIXIN ==============================================================
;; The idea here is that each widget has creates a hash-table with all
;; possible theme data required to perform ON-PAINT in the slot
;; THEME-CLASS-DATA. Additionally, each widget should have a
;; INITIALIZE-INSTANCE :BEFORE method that registers the contents of the
;; THEME-CLASS-DATA with *THEME-ALL-DATA*, then marks the
;; THEME-CLASS-DATA-REGISTERED field to T.

;; Common theme styles: 'FLAT '3D NIL
;; Common theme id's: 'TEXT-COLOR 'TEXT-FONT 'THICKNESS 'WIDTH etc.

(deftype theme-style () '(member :unset :flat :3d-out :3d-in :3d-flat))

(defclass theme-mixin (theme-mixin-base)
  ((theme-data :initform nil)
   (theme-style :type theme-style :initarg :theme-style :initform :unset :accessor theme-style)))

;;; METHODS ---------------------------------------------------------

(defmethod default-theme-style ((object theme-mixin))
  (theme-data-style (theme-style object)))

(defmethod get-theme-value ((object theme-mixin) type id &key (style nil style-p))
  (let ((local-style (if style-p
                         style
                         (theme-data-style (theme-style object)))))
    (when (theme-valuep object type id :style local-style)
      (let ((hash-key (theme-data-hash-key type local-style id)))
        (return-from get-theme-value (gethash hash-key (slot-value object 'theme-data)))))
    (get-theme-value-default type local-style id)))

(defmethod set-theme-value ((object theme-mixin) type id value &key (style nil stylep))
  "Set theme-data for THEME-MIXIN subclass."
  
  (when (null (slot-value object 'theme-data))
    (setf (slot-value object 'theme-data) (make-hash-table :test 'equalp)))
  (let ((local-style (if stylep
                         style
                         (theme-data-style (theme-style object)))))
    (setf (gethash (theme-data-hash-key type local-style id) (slot-value object 'theme-data)) value)
    (v:debug :theme "[SET-THEME-VALUE] {THEME-MIXIN} set ~a/~a/~a to ~a" type local-style id value)))

(defmethod theme-valuep ((object theme-mixin) type id &key (style nil stylep))
  "Returns T if object's theme-data contains requested theme-data."
  
  (let ((local-style (if stylep
                         style
                         (theme-data-style (theme-style object)))))
    (let ((theme-data (slot-value object 'theme-data))
          (hash-key (theme-data-hash-key type local-style id)))
      (unless (null theme-data)
        (multiple-value-bind (value present) (gethash hash-key theme-data)
          (declare (ignore value))
          (when present
            (return-from theme-valuep t))))))
  nil)

;;;; MACROS ===================================================================

(defmacro with-theme-let ((&rest vars) object &body body)
  "Creates local symbols for VARS, then locates the THEME-DATA for each name
within VARS. VARS is formatted either as (theme-data...)
or ((var (theme-data))...) in any combination.

theme-data can be (id), (style id), or (type style id). When type or style are
omitted, they are gathered from OBJECT or defaulted as needed (for example, if
style doesn't apply to OBJECT, then its ignored; If it does apply, then its
either substituted form a default or gathered from OBJECT).

When var name is ommited, variable name wiil be [type-[style-[id]]]. Missing
type and style will not be added even when default or object specific values
are used for them."

  (flet ((get-name (var-entry)
           (if (consp var-entry)
               (let ((left (first var-entry)))
                 (when (atom left)
                   (return-from get-name left))
                 (case (length left)
                   (1 (return-from get-name (first left)))
                   (2 (return-from get-name (a:symbolicate (first left) "-" (second left))))
                   (3 (return-from get-name (a:symbolicate (first left) "-" (second left) "-" (third left))))
                   (Otherwise
                    (error "malformed theme-data list"))))
               (return-from get-name var-entry)))
         (get-data (var-entry)
           (let (rv)
             (if (consp var-entry)
                 (let ((right (rest var-entry)))
                   (if (eql right nil)
                       (let ((right (mapcan #'identity var-entry)))
                         (case (length right)
                           (1 (setq rv `(nil nil ',(first right))))
                           (2 (setq rv `(',(first right) nil ',(second right))))
                           (3 (setq rv `(',(first right) ',(second right) ',(third right))))
                           (t
                            (error "malformed theme-data list"))))
                       (if (atom (first right))
                           (setq rv `(nil nil ',(first right)))
                           (progn
                             (setq right (mapcan #'identity right))
                             (case (length right)
                               (2 (setq rv `(',(first right) nil ',(second right))))
                               (3 (setq rv `(',(first right) ',(second right) ',(third right))))
                               (t
                                (error "malformed theme-data list")))))))
                 (setq rv `(nil nil ',var-entry)))
             rv)))
    (a:with-gensyms (local-object)
      `(let ((,local-object ,object))
         (let (,@(mapcar #'(lambda (n c)
                             `(,n (get-theme-value ,local-object
                                                   ,(let ((type (first c)))
                                                      (if (eql type nil)
                                                          `(default-theme-type ,local-object)
                                                          type))
                                                   ,(third c)
                                                   ,@(let ((style (second c)))
                                                      (unless (null style)
                                                         (list :style `,style))))))
                         (mapcar #'get-name `,vars)
                         (mapcar #'get-data `,vars)))
           ,@body)))))

