(in-package #:cl-yag)

;; Theme stuff that can't be in main theme file because of dependancies

;;;; global object theme handlers =============================================
;; These should set theme colors the same way the global themes are set for
;; consistantly.  If something looks wrong, fix both.

;; TODO: Find a way to express the connections from themes to objects in
;;       a way to allow it to be specified once.

;;;; theme-flat-all ===========================================================

(defclass theme-flat-all (theme-flat
                          active-text-theme-mixin
                          border-flat-theme-mixin
                          box-flat-theme-mixin
                          grid-theme-mixin
                          ruler-theme-mixin
                          window-theme-mixin)
  ())

;;;; theme-3d-all =============================================================

(defclass theme-3d-all (theme-3d
                        active-text-theme-mixin
                        border-3d-theme-mixin
                        border-flat-theme-mixin
                        box-3d-theme-mixin
                        box-flat-theme-mixin
                        grid-theme-mixin
                        ruler-theme-mixin
                        window-theme-mixin)
  ())

;;; active-text -----------------------------------------------------

(defmethod (setf theme) ((theme theme-flat) (object active-text))
  (with-local-accessors ((f frame-color) (i interior-color)
                         (fc fore-color) (bc back-color))
                        theme
    (setf (down-color object) f)
    (setf (hover-color object) f)
    (setf (interior-color object) i)
    (setf (up-color object) i)
    (setf (fore-color object) fc)
    (setf (back-color object) bc)))

(defmethod (setf theme) ((theme theme-3d) (object active-text))
  (with-local-accessors ((d dark-color) (n normal-color)
                         (vd very-dark-color) (vl very-light-color)
                         (fc fore-color) (bc back-color))
                        theme
    (setf (down-color object) vd)
    (setf (hover-color object) d)
    (setf (interior-color object) n)
    (setf (up-color object) vl)
    (setf (fore-color object) fc)
    (setf (back-color object) bc)))

;;; box -------------------------------------------------------------

(defmethod (setf theme) ((theme theme-flat) (object box))
  (error "not implemented"))

(defmethod (setf theme) ((theme theme-3d) (object box))
  (error "not implemented"))

;;; grid ------------------------------------------------------------

(defmethod (setf theme) ((theme theme-flat) (object grid))
  (with-local-accessors ((fc frame-color)) theme
    (setf (major-color-h object) fc)
    (setf (major-color-v object) fc)
    (setf (minor-color-h object) fc)
    (setf (minor-color-v object) fc)))

(defmethod (setf theme) ((theme theme-3d) (object grid))
  (with-local-accessors ((d dark-color) (vd very-dark-color)) theme
    (setf (major-color-h object) d)
    (setf (major-color-v object) d)
    (setf (minor-color-h object) vd)
    (setf (minor-color-v object) vd)))

;;; ruler -----------------------------------------------------------

(defmethod (setf theme) ((theme theme-flat) (object ruler))
  (with-local-accessors ((f frame-color)) theme
    (setf (color object) f)
    (setf (major-color object) f)
    (setf (minor-color object) f)))

(defmethod (setf theme) ((theme theme-3d) (object ruler))
  (with-local-accessors ((l light-color) (vd very-dark-color) (vl very-light-color))
                        theme
    (setf (color object) vd)
    (setf (major-color object) vl)
    (setf (minor-color object) l)))

;;; text ------------------------------------------------------------

(defmethod (setf theme) ((theme theme-flat) (object text))
  (with-local-accessors ((i interior-color) (fc fore-color) (bc back-color))
                        theme
    (setf (interior-color object) i)
    (setf (fore-color object) fc)
    (setf (back-color object) bc)))

(defmethod (setf theme) ((theme theme-3d) (object text))
  (with-local-accessors ((n normal-color) (fc fore-color) (bc back-color))
                        theme
    (setf (interior-color object) n)
    (setf (fore-color object) fc)
    (setf (back-color object) bc)))

;;; window ----------------------------------------------------------

(defmethod (setf theme) ((theme theme-flat) (object window))
  (with-local-accessors ((bc back-color) (fc fore-color) (i interior-color))
                        theme
    (setf (interior-color object) i)
    (setf (back-color object) bc)
    (setf (fore-color object) fc)))

(defmethod (setf theme) ((theme theme-3d) (object window))
  (with-local-accessors ((d dark-color) (n normal-color)) theme
    (setf (frame-color object) d)
    (setf (interior-color object) n)))

;;;; global themes ============================================================

;; These should initialize their fields the same way the above (setf theme)
;; functions do.  TODO: Make it so the code exists once!

;;; flat ------------------------------------------------------------

;; Adjustments for global themes

(defmacro deftheme-flat-all-obj (frame interior
                                 &optional (fore '(al:map-rgb 255 255 255)) (back '(al:map-rgb-f 0.0 0.0 0.0))
                                 &body body)
  (let ((object (gensym)))
    `(funcall (lambda (f i fc bc)
                (let ((,object (make-instance 'theme-flat-all :fore-color fc :back-color bc :frame-color f :interior-color i)))

                  ;; active-text-theme-mixin
                  (setf (font ,object) (default-font))
                  (setf (down-color ,object) f)
                  (setf (hover-color ,object) f)
                  (setf (interior-color ,object) i)
                  (setf (up-color ,object) i)
                  ;; border-3d-theme-mixin is not valid from a flat theme
                  ;; border-flat-theme-mixin
                  (setf (color ,object) f)
                  ;; box-flat-theme-mixin covered by theme-flat
                  ;; grid-theme-mixin
                  (setf (major-color-h ,object) f)
                  (setf (major-color-v ,object) f)
                  (setf (minor-color-h ,object) f)
                  (setf (minor-color-v ,object) f)
                  ;; ruler-theme-mixin
                  (setf (division-color ,object) f)
                  (setf (line-color ,object) f)
                  ;; text-theme-mixin covered by active-text-theme-mixin
                  ;; window-theme-mixin covered by theme-flat

                  ;; Custom initialization code
                  (let ((object ,object))
                    ,@body
                    (setf ,object object))
                  
                  ;; Make sure all slots get set
                  ;; (dolist (slot (class-slots (find-class 'theme-flat-all)))
                  ;;   (when (eql (slot-value ,object (slot-definition-name slot)) nil)
                  ;;     (error "Missing theme-flat-all setting: ~a" (slot-definition-name slot))))
                  ,object))
              ,frame ,interior ,fore ,back)))

(defun theme-flat-aqua ()
  (deftheme-flat-all-obj (al:map-rgb 0 191 191) (al:map-rgb 0 255 255)))

(defun theme-flat-blue ()
  (deftheme-flat-all-obj (al:map-rgb 0 0 191) (al:map-rgb 0 0 255)))

(defun theme-flat-gray ()
  (deftheme-flat-all-obj (al:map-rgb-f 0.5 0.5 0.5) (al:map-rgb 212 208 200) (al:map-rgb-f 0.0 0.0 0.0) (al:map-rgb-f 1.0 1.0 1.0)
    ;; Override the up color
    (setf (up-color object) (fore-color object))
    ))

(defun theme-flat-green ()
  (deftheme-flat-all-obj (al:map-rgb 0 191 0) (al:map-rgb 0 255 0)))

(defun theme-flat-purple ()
  (deftheme-flat-all-obj (al:map-rgb 191 0 191) (al:map-rgb 255 0 255)))

(defun theme-flat-red ()
  (deftheme-flat-all-obj (al:map-rgb 191 0 0) (al:map-rgb 255 0 0)))

(defun theme-flat-yellow ()
  (deftheme-flat-all-obj (al:map-rgb 191 191 0) (al:map-rgb 255 255 0)))

;;; 3d --------------------------------------------------------------

(defmacro deftheme-3d-obj (normal dark light very-dark very-light
                           &optional (fore-color '(al:map-rgb-f 1.0 1.0 1.0)) (back-color '(al:map-rgb 0 0 0))
                           &body body)
  (let ((object (gensym)))
    `(funcall (lambda (n d l vd vl fc bc)
                (let ((,object (make-instance 'theme-3d-all :normal n :dark d :light l :very-dark vd :very-light vl :fore-color fc :back-color bc)))
                  
                  ;; active-text-theme-mixin
                  (setf (font ,object) (default-font))
                  (setf (down-color ,object) vd)
                  (setf (hover-color ,object) d)
                  (setf (interior-color ,object) n)
                  (setf (up-color ,object) vl)
                  ;; border-3d-theme-mixin covered by theme-3d
                  ;; border-flat-theme-mixin
                  (setf (color ,object) d)
                  ;; box-3d-theme-mixin covered by theme-3d
                  ;; box-flat-theme-mixin covered by active-text-theme-mixin and border-flat-theme-mixin
                  ;; grid-theme-mixn
                  (setf (major-color-h ,object) d)
                  (setf (major-color-v ,object) d)
                  (setf (minor-color-h ,object) vd)
                  (setf (minor-color-v ,object) vd)
                  ;; ruler-theme-mixin
                  (setf (division-color ,object) l)
                  (setf (line-color ,object) l)
                  ;; text-theme-mixin covered by active-text-theme-mixin
                  ;; window-theme-mixin converted by active-text-theme-mixin

                  ;; Custom initialization code
                  (let ((object ,object))
                    ,@body
                    (setf ,object object))

                  ;; Make sure all slots get set
                  ;; (dolist (slot (class-slots (find-class 'theme-3d-all)))
                  ;;   (when (eql (slot-value ,object (slot-definition-name slot)) nil)
                  ;;     (error "Missing theme-3d-all setting: ~a" (slot-definition-name slot))))
                  
                  ,object))
              ,normal ,dark ,light ,very-dark ,very-light, fore-color, back-color)))

(defun theme-3d-aqua ()
  (deftheme-3d-obj (al:map-rgb 0 255 255) (al:map-rgb 0 191 191) (al:map-rgb 127 255 255) (al:map-rgb 0 127 127) (al:map-rgb 191 255 255)))

(defun theme-3d-blue ()
  (deftheme-3d-obj (al:map-rgb 0 0 255) (al:map-rgb 0 0 191) (al:map-rgb 159 159 255) (al:map-rgb 0 0 159) (al:map-rgb 191 191 255)))

(defun theme-3d-gray ()
  (deftheme-3d-obj
      (al:map-rgb 212 208 200) (al:map-rgb 128 128 128) (al:map-rgb 223 223 223) (al:map-rgb 95 95 95) (al:map-rgb 245 245 245) (al:map-rgb 0 0 0) (al:map-rgb 255 255 255)
    ;; Override the active-text up/down for this one
    (setf (up-color object) (fore-color object))
    (setf (down-color object) (dark-color object))
    ))

(defun theme-3d-green ()
  (deftheme-3d-obj (al:map-rgb 0 255 0) (al:map-rgb 0 191 0) (al:map-rgb 127 255 127) (al:map-rgb 0 159 0) (al:map-rgb 191 255 191)))

(defun theme-3d-purple ()
  (deftheme-3d-obj (al:map-rgb 255 0 255) (al:map-rgb 191 0 191) (al:map-rgb 255 159 255) (al:map-rgb 159 0 159) (al:map-rgb 255 191 255)))

(defun theme-3d-red ()
  (deftheme-3d-obj (al:map-rgb 255 0 0) (al:map-rgb 191 0 0) (al:map-rgb 255 127 127) (al:map-rgb 159 0 0) (al:map-rgb 255 191 191)))

(defun theme-3d-yellow ()
  (deftheme-3d-obj (al:map-rgb 255 255 0) (al:map-rgb 191 191 0) (al:map-rgb 255 255 127) (al:map-rgb 127 127 0) (al:map-rgb 255 255 191)))

;;;------------------------------------------------------------------

(defparameter *theme-default* (theme-flat-gray))

