(in-package #:cl-yag)

;;;; cffi-object ==============================================================

;; (cobj:define-cobject-class (:struct al:mouse-state))

;; (cobj:define-cobject-class (:struct al:any-event))
;; (cobj:define-cobject-class (:struct al:display-event))
;; (cobj:define-cobject-class (:struct al:keyboard-event))
;; (cobj:define-cobject-class (:struct al:mouse-event))
;; (cobj:define-cobject-class (:struct al:timer-event))

;; (cobj:define-cobject-class (:struct al:event-queue))

;;;; constants/enums ==========================================================

(defconstant +KEY-MAX+ (foreign-enum-value 'al::keycodes :key-max))

(defconstant +KEY-DOWN+ (foreign-enum-value 'al::keycodes :down))
(defconstant +KEY-LEFT+ (foreign-enum-value 'al::keycodes :left))
(defconstant +KEY-RIGHT+ (foreign-enum-value 'al::keycodes :right))
(defconstant +KEY-UP+ (foreign-enum-value 'al::keycodes  :up))
(defconstant +KEY-X+ (foreign-enum-value 'al::keycodes :x))
(defconstant +KEY-ESC+ (foreign-enum-value 'al::keycodes  :escape))
(defconstant +KEY-B+ (foreign-enum-value 'al::keycodes :b))

(defconstant +MOUSE-BUTTON-LEFT+ 1)
(defconstant +MOUSE-BUTTON-RIGHT+ 2)
(defconstant +MOUSE-BUTTON-MIDDLE+ 3)

;; blend operations
(defconstant +OP-ADD+ (cffi:foreign-enum-value 'al::blend-operations ':add))
(defconstant +OP-SRC-MINUS-DEST+ (cffi:foreign-enum-value 'al::blend-operations ':src-minus-dest))
(defconstant +OP-DEST-MINUS-SRC+ (cffi:foreign-enum-value 'al::blend-operations ':dest-minus-src))

;; blend modes
(defconstant +BLEND-ZERO+ (cffi:foreign-enum-value 'al::blend-mode ':zero))
(defconstant +BLEND-ONE+ (cffi:foreign-enum-value 'al::blend-mode ':one))
(defconstant +BLEND-ALPHA+ (cffi:foreign-enum-value 'al::blend-mode ':alpha))
(defconstant +BLEND-INVERSE-ALPHA+ (cffi:foreign-enum-value 'al::blend-mode ':inverse-alpha))
(defconstant +BLEND-SRC-COLOR+ (cffi:foreign-enum-value 'al::blend-mode ':src-color))
(defconstant +BLEND-DEST-COLOR+ (cffi:foreign-enum-value 'al::blend-mode ':dest-color))
(defconstant +BLEND-INVERSE-SRC-COLOR+ (cffi:foreign-enum-value 'al::blend-mode ':inverse-src-color))
(defconstant +BLEND-INVERSE-DEST-COLOR+ (cffi:foreign-enum-value 'al::blend-mode ':inverse-dest-color))
(defconstant +BLEND-CONST-COLOR+ (cffi:foreign-enum-value 'al::blend-mode ':const-color))
(defconstant +BLEND-INVERSE-CONS-COLOR+ (cffi:foreign-enum-value 'al::blend-mode ':inverse-cons-color))

;; Pixel formats
(defconstant +P-F-RGB-888+ (cffi:foreign-enum-value 'al::pixel-format ':rgb-888))
(defconstant +P-F-BGR-888+ (cffi:foreign-enum-value 'al::pixel-format ':bgr-888))
(defconstant +P-F-ABGR-8888+ (cffi:foreign-enum-value 'al::pixel-format ':abgr-8888))
(defconstant +P-F-ARGB-8888+ (cffi:foreign-enum-value 'al::pixel-format ':argb-8888))

;; locking-flags
(defconstant +LOCK_READWRITE+ (cffi:foreign-enum-value 'al::locking-flags ':readwrite))
(defconstant +LOCK_READONLY+ (cffi:foreign-enum-value 'al::locking-flags ':readonly))
(defconstant +LOCK_WRITEONLY+ (cffi:foreign-enum-value 'al::locking-flags ':writeonly))

;;;; allegro struct wrappers ==================================================

;;; display-event ---------------------------------------------------

(macrolet ((display-event (object field)
             `(cffi:foreign-slot-value ,object '(:struct al:display-event) ,field)))
  
  (defun display-event-height (object)
    (display-event object 'al::height))
  (defun display-event-x (object)
    (display-event object 'al::x))
  (defun display-event-width (object)
    (display-event object 'al::width))
  (defun display-event-y (object)
    (display-event object 'al::y)))

;;; event -----------------------------------------------------------

(macrolet ((event (object field)
             `(cffi:foreign-slot-value ,object '(:union al:event) ,field)))
  
  (defun event-type (object)
    (event object 'al::type)))

;;; keyboard-event --------------------------------------------------

(macrolet ((keyboard-event (object field)
             `(cffi:foreign-slot-value ,object '(:struct al:keyboard-event) ,field)))
  
  (defun keyboard-event-keycode (object)
    (keyboard-event object 'al::keycode))
  (defun keyboard-event-modifiers (object)
    (keyboard-event object 'al::modifiers)))

;;; mouse-event -----------------------------------------------------

(macrolet ((mouse-event (event field)
             `(cffi:foreign-slot-value ,event '(:struct al:mouse-event) ,field)))
  
  (defun mouse-event-button (event)
    (mouse-event event 'al::button))
  (defun mouse-event-dx (event)
    (mouse-event event 'al::dx))
  (defun mouse-event-dy (event)
    (mouse-event event 'al::dy))
  (defun mouse-event-x (event)
    (mouse-event event 'al::x))
  (defun mouse-event-y (event)
    (mouse-event event 'al::y)))

;;; timer-event -----------------------------------------------------

(macrolet ((timer-event (object field)
             `(cffi:foreign-slot-value ,object '(:struct al:timer-event) ,field)))
  (defun timer-event-source (object)
    (timer-event object 'al::source))
  (defun timer-event-count (object)
    (timer-event object 'al::count)))

;;;;===========================================================================

(defun color2assoc (color)
  (let ((rgba (color2list color)))
    `((:r ,(first rgba)) (:g ,(second rgba)) (:b ,(third rgba)) (:a ,(fourth rgba)))))

(defun color2list (color)
  (list
   (nth (1+ (position 'al::r color)) color)
   (nth (1+ (position 'al::g color)) color)
   (nth (1+ (position 'al::b color)) color)
   (nth (1+ (position 'al::a color)) color)))

(defun color-a (color)
  (fourth (color2list color)))

(defun color-b (color)
  (third (color2list color)))

(defun color-g (color)
  (second (color2list color)))

(defun color-inverse (color)
  (al:map-rgb-f (- 1 (color-r color))
                (- 1 (color-g color))
                (- 1 (color-b color))))

(defun color-r (color)
  (first (color2list color)))

(defun print-color (color &optional (stream nil))
  (assert (not (eq nil color)))
  (format stream "al:map-rgba-f ~d ~d ~d ~d" (color-r color) (color-g color) (color-b color) (color-a color)))

;;;; function wrappers ========================================================

(defun get-blender ()
  (cffi:with-foreign-objects ((op :int) (src :int) (dst :int))
    (al:get-blender op src dst)
    (values (cffi:mem-ref op :int)
            (cffi:mem-ref src :int)
            (cffi:mem-ref dst :int))))

(defun get-text-dimensions (font title)
  (cffi:with-foreign-objects ((x :int) (y :int) (w :int) (h :int))
    (al:get-text-dimensions font title x y w h)
    (values (cffi:mem-ref x :int)
            (cffi:mem-ref y :int)
            (cffi:mem-ref w :int)
            (cffi:mem-ref h :int))))
    
(defun unmap-rgba (c)
  (cffi:with-foreign-objects ((r :float) (g :float) (b :float) (a :float))
    (al:unmap-rgba-f c r g b a)
    (values (cffi:mem-ref r :float)
            (cffi:mem-ref g :float)
            (cffi:mem-ref b :float)
            (cffi:mem-ref a :float))))

;;;; helper macros =============================================================

(defmacro with-blender ((op src dst) &body body)
  (let ((sop (gensym))
        (ssrc (gensym))
        (sdst (gensym)))
    `(progn
       (multiple-value-bind (,sop ,ssrc ,sdst) (get-blender)
         (al:set-blender ,op ,src ,dst)
         ,@body
         (al:set-blender ,sop ,ssrc ,sdst)))))

