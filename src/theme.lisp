(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

;;;; FORWARDS =================================================================

(declaim (ftype (function ((or null keyword string symbol) (or null keyword string symbol) (or keyword string symbol)) string) theme-data-hash-key))
(declaim (inline theme-data-hash-key))

(declaim (inline theme-data-style))

(defvar *theme-all-data* nil "Contains hash-table of all possible theme data.")
(defvar *theme-all-data-initialized* nil "When NIL causes MAKE-INSTANCE 'THEME to add default non-type specific theme
data to *THEME-ALL-DATA*.")

(defvar *theme-default* nil "Default theme.")

;; Needed before it'd be in correct section
(defun theme-data-hash-key (type style id)
  (let (hash-type hash-style hash-id)
    (typecase type
      (null
       (setq hash-type "NONE"))
      ((or keyword symbol)
       (setq hash-type (symbol-name type)))
      (string
       (setq hash-type (string-upcase type))))
    
    (typecase style
      (null
       (setq hash-style "NONE"))
      ((or keyword symbol)
       (setq hash-style (symbol-name style)))
      (string
       (setq hash-style (string-upcase style))))
    
    (typecase id
      (string
       (setq hash-id (string-upcase id)))
      ((or symbol keyword)
       (setq hash-id (symbol-name id))))
    
    (concatenate 'string hash-type "-" hash-style "-" hash-id)))

(defun theme-data-style (style)
  (if (member style '(:unset))
      nil
      style))

(defun get-theme-style (object)
  ;; Find first non-unset theme
  (let ((local-object object)
        style)
    (loop
      (assert (not (null local-object)))
      ;; From object?
      (when (typep local-object 'theme-mixin)
        (setf style (theme-style local-object))
        (unless (eql style :unset)
          (return-from get-theme-style style)))

      ;; No, from object's theme?
      (when (typep local-object 'theme-base)
        (setf style (theme-style local-object))
        (unless (eql style :unset)
          (return-from get-theme-style style)))

      ;; No, did we get to manager?
      (when (typep local-object 'manager)
        (let ((local-theme (theme local-object)))
          ;; Yes, does it have a valid theme?
          (unless (null local-theme)
            ;; Yes so get its style
            (setf style (theme-style local-theme)))
          ;; Find good theme?
          (when (eql style :unset)
            ;; No, so et global theme style
            (setf style (theme-style *theme-default*)))
          ;; Now did we get a good theme?
          (when (eql style :unset)
            ;; No, so use FLAT
            (setf style :flat)
            (v:debug :theme "[GET-THEME-STYLE] no THEME-STYLE set, using FLAT."))
          ;; Theme is valid now!
          (return-from get-theme-style style)))

      ;; No, check next parent
      (unless (typep local-object 'parent-mixin)
        (error "failed to locate MANAGER or valid THEME-STTLE when no more PARENT's"))
      (setf local-object (parent local-object)))))

;;;; THEME-BASE ===============================================================

(defclass theme-base ()
  ()
  (:documentation "Base class for all THEME's. Any derived class will be assumed to be able to
have GET-THEME-VALUE and SET-THEME-VALUE work with them."))

;;;; THEME ====================================================================

(deftype theme-style () '(member :unset :flat :3d-out :3d-in :3d-flat))

(defclass theme (theme-base)
  ((theme-data :initform nil)
   (theme-style :type theme-style :initarg :theme-style :initform :unset :accessor theme-style))
  (:documentation "Contains theme data. Any widget below the object in widget hierarchy with the
THEME will use the theme-data from the THEME, unless overridden in inidivual
widgets themselves."))

(defmacro deftheme (&rest rest &key &allow-other-keys)
  `(make-instance 'theme ,@rest))

;;; METHODS ---------------------------------------------------------

(defmethod default-theme-style (object)
  (unless (a:starts-with (type-of object) '%RULER)
   (v:warn :theme "[DEFAULT-THEME-STYLE] {} missing for type ~a" (type-of object))))

(defmethod default-theme-type (object)
  (unless (a:starts-with (type-of object) '%RULER)
    (v:warn :theme "[DEFAULT-THEME-TYPE] {} missing for type ~a" (type-of object))))

(defmethod get-theme-value-default (type style id)
  (let ((hash-key (theme-data-hash-key type style id)))
    (multiple-value-bind (value present) (gethash hash-key *theme-all-data*)
      (unless present
        (error "[GET-THEME-VALUE] {} missing default value for ~a/~a/~a" type style id))
      (v:debug :theme "[GET-THEME-VALUE-DEFAULT] ~a/~a/~a returned ~a" type style id value)
      value)))

(defparameter *theme-all-data* (make-hash-table :test 'equalp))
(defmethod initialize-instance :before ((object theme) &key)
  "Add default non-type specific theme data to *THEME-ALL-DATA*"

  (unless (theme-value-defaultp nil nil 'interior-color)
    (set-theme-value-default nil nil 'interior-color +color-gray+)
    (set-theme-value-default nil nil 'frame-color +color-very-dark-gray+)
    (set-theme-value-default nil nil 'text-color +color-black+)
    (set-theme-value-default nil nil 'text-font (default-font -24))
    (set-theme-value-default nil nil 'color +color-gray+)
    (set-theme-value-default nil nil 'dark-color +color-dark-gray+)
    (set-theme-value-default nil nil 'light-color +color-light-gray+)
    (set-theme-value-default nil nil 'very-dark-color +color-very-dark-gray+)
    (set-theme-value-default nil nil 'very-light-color +color-very-light-gray+)))

(defmethod set-theme-value-default (type style id value)
  (let ((hash-key (theme-data-hash-key type style id)))
    (setf (gethash hash-key *theme-all-data*) value)
    (v:debug :theme "[SET-THEME-VALUE-DEFAULT] {} value set for ~a / ~a / ~a" type style id)))

(defmethod theme-value-defaultp (type style id)
  (let ((hash-key (theme-data-hash-key type (theme-data-style style) id)))
    (multiple-value-bind (value present) (gethash hash-key *theme-all-data*)
      (declare (ignore value))
      present)))

;;;; GLOBALS ==================================================================

(defparameter *theme-default* (make-instance 'theme))

;;;; FUNCTIONS ================================================================

(defun theme-3d-style-colors (type style object)
  "Return 3d drawing colors from 3d drawing style (left-top-outside
left-top-inside right-bottom-outside right-bottom-inside)."

  (let ((color (get-theme-value object type 'color :style style))
        (dark-color (get-theme-value object type 'dark-color :style style))
        (light-color (get-theme-value object type 'light-color :style style))
        (very-dark-color (get-theme-value object type 'very-dark-color :style style))
        (very-light-color (get-theme-value object type 'very-light-color :style style)))
    (assert (not (eql color nil)))
    (assert (not (eql dark-color nil)))
    (assert (not (eql light-color nil)))
    (assert (not (eql very-dark-color nil)))
    (assert (not (eql very-light-color nil)))
    (ecase style
      (:3d-in
       (values very-dark-color dark-color light-color very-light-color))
      (:3d-out
       (values light-color very-light-color very-dark-color dark-color))
      ((:flat :3d-flat :unset)
       (values very-dark-color dark-color very-dark-color dark-color)))))

;;;; GLOBAL THEMES ============================================================

(defun theme-aqua ()
  (unless (theme-value-defaultp nil nil 'interior-color)
    (set-theme-value-default nil nil 'interior-color +color-aqua+)
    (set-theme-value-default nil nil 'frame-color +color-very-dark-aqua+)
    (set-theme-value-default nil nil 'text-color +color-black+)
    (set-theme-value-default nil nil 'text-font (default-font -24))
    (set-theme-value-default nil nil 'color +color-aqua+)
    (set-theme-value-default nil nil 'dark-color +color-dark-aqua+)
    (set-theme-value-default nil nil 'light-color +color-light-aqua+)
    (set-theme-value-default nil nil 'very-dark-color +color-very-dark-aqua+)
    (set-theme-value-default nil nil 'very-lightcolor +color-very-light-aqua+)))

(defun theme-blue ()
  (unless (theme-value-defaultp nil nil 'interior-color)
    (set-theme-value-default nil nil 'interior-color +color-blue+)
    (set-theme-value-default nil nil 'frame-color +color-very-dark-blue+)
    (set-theme-value-default nil nil 'text-color +color-black+)
    (set-theme-value-default nil nil 'text-font (default-font -24))
    (set-theme-value-default nil nil 'color +color-blue+)
    (set-theme-value-default nil nil 'dark-color +color-dark-blue+)
    (set-theme-value-default nil nil 'light-color +color-light-blue+)
    (set-theme-value-default nil nil 'very-dark-color +color-very-dark-blue+)
    (set-theme-value-default nil nil 'very-lightcolor +color-very-light-blue+)))

(defun theme-gray ()
  (unless (theme-value-defaultp nil nil 'interior-color)
    (set-theme-value-default nil nil 'interior-color +color-gray+)
    (set-theme-value-default nil nil 'frame-color +color-very-dark-gray+)
    (set-theme-value-default nil nil 'text-color +color-black+)
    (set-theme-value-default nil nil 'text-font (default-font -24))
    (set-theme-value-default nil nil 'color +color-gray+)
    (set-theme-value-default nil nil 'dark-color +color-dark-gray+)
    (set-theme-value-default nil nil 'light-color +color-light-gray+)
    (set-theme-value-default nil nil 'very-dark-color +color-very-dark-gray+)
    (set-theme-value-default nil nil 'very-lightcolor +color-very-light-gray+)))

(defun theme-green ()
  (unless (theme-value-defaultp nil nil 'interior-color)
    (set-theme-value-default nil nil 'interior-color +color-green+)
    (set-theme-value-default nil nil 'frame-color +color-very-dark-green+)
    (set-theme-value-default nil nil 'text-color +color-black+)
    (set-theme-value-default nil nil 'text-font (default-font -24))
    (set-theme-value-default nil nil 'color +color-green+)
    (set-theme-value-default nil nil 'dark-color +color-dark-green+)
    (set-theme-value-default nil nil 'light-color +color-light-green+)
    (set-theme-value-default nil nil 'very-dark-color +color-very-dark-green+)
    (set-theme-value-default nil nil 'very-lightcolor +color-very-light-green+)))

(defun theme-purple ()
  (unless (theme-value-defaultp nil nil 'interior-color)
    (set-theme-value-default nil nil 'interior-color +color-purple+)
    (set-theme-value-default nil nil 'frame-color +color-very-dark-purple+)
    (set-theme-value-default nil nil 'text-color +color-black+)
    (set-theme-value-default nil nil 'text-font (default-font -24))
    (set-theme-value-default nil nil 'color +color-purple+)
    (set-theme-value-default nil nil 'dark-color +color-dark-purple+)
    (set-theme-value-default nil nil 'light-color +color-light-purple+)
    (set-theme-value-default nil nil 'very-dark-color +color-very-dark-purple+)
    (set-theme-value-default nil nil 'very-lightcolor +color-very-light-purple+)))

(defun theme-red ()
  (unless (theme-value-defaultp nil nil 'interior-color)
    (set-theme-value-default nil nil 'interior-color +color-red+)
    (set-theme-value-default nil nil 'frame-color +color-very-dark-red+)
    (set-theme-value-default nil nil 'text-color +color-black+)
    (set-theme-value-default nil nil 'text-font (default-font -24))
    (set-theme-value-default nil nil 'color +color-red+)
    (set-theme-value-default nil nil 'dark-color +color-dark-red+)
    (set-theme-value-default nil nil 'light-color +color-light-red+)
    (set-theme-value-default nil nil 'very-dark-color +color-very-dark-red+)
    (set-theme-value-default nil nil 'very-lightcolor +color-very-light-red+)))

(defun theme-yellow ()
  (unless (theme-value-defaultp nil nil 'interior-color)
    (set-theme-value-default nil nil 'interior-color +color-yellow+)
    (set-theme-value-default nil nil 'frame-color +color-very-dark-yellow+)
    (set-theme-value-default nil nil 'text-color +color-black+)
    (set-theme-value-default nil nil 'text-font (default-font -24))
    (set-theme-value-default nil nil 'color +color-yellow+)
    (set-theme-value-default nil nil 'dark-color +color-dark-yellow+)
    (set-theme-value-default nil nil 'light-color +color-light-yellow+)
    (set-theme-value-default nil nil 'very-dark-color +color-very-dark-yellow+)
    (set-theme-value-default nil nil 'very-lightcolor +color-very-light-yellow+)))
