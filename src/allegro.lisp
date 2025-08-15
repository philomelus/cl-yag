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

(defconstant +OP-ADD+ (foreign-enum-value 'al::blend-operations ':add))
(defconstant +BLEND-ONE+ (foreign-enum-value 'al::blend-mode ':one))
(defconstant +BLEND-INVERSE-ALPHA+ (foreign-enum-value 'al::blend-mode ':inverse-alpha))

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

(defgeneric make-color (r g b &optional a)
  (:method (r g b &optional a)
    (typecase r
      (integer
       (assert (typep g 'integer))
       (assert (typep b 'integer))
       (if (eq nil a)
           (al:map-rgb r g b)
           (al:map-rgba r g b a)))
      (float
       (assert (typep g 'float))
       (assert (typep b 'float))
       (if (eq nil a)
           (al:map-rgb-f r g b)
           (al:map-rgba-f r g b a)))
      (otherwise
       (error "unhandled color argument type: ~a" (type-of r))))))

(defun print-color (color &optional (stream nil))
  (assert (not (eq nil color)))
  (format stream "al:map-rgba-f ~d ~d ~d ~d" (color-r color) (color-g color) (color-b color) (color-a color)))

;;;; function wrappers ========================================================

(defun get-text-dimensions (font title)
  (cffi:with-foreign-objects ((x :int) (y :int) (w :int) (h :int))
    (al:get-text-dimensions font title x y w h)
    (values (cffi:mem-ref x :int)
            (cffi:mem-ref y :int)
            (cffi:mem-ref w :int)
            (cffi:mem-ref h :int))))

    
