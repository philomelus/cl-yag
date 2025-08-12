(in-package #:cl-yag)

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

    
