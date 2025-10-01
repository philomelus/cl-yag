(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; theme-mixin ==============================================================

(defparameter *GRID-THEME-DATA* `((grid nil major-color-h)
                                  (grid nil major-color-v)
                                  (grid nil minor-color-h)
                                  (grid nil minor-color-v)))

;;;; grid =====================================================================

(defclass grid (area-mixin
                parent-mixin
                shortcuts-mixin
                visible-mixin)
  ((major :initarg :major :initform 25 :accessor major)
   (minor :initarg :minor :initform 5 :accessor minor)))

(defmacro defgrid (&rest rest &key &allow-other-keys)
  `(make-instance 'grid ,@rest))

;;; methods ---------------------------------------------------------

(defmethod (setf color) (value (object grid))
  (set-theme-value object 'grid 'major-color-h value)
  (set-theme-value object 'grid 'major-color-v value)
  (set-theme-value object 'grid 'minor-color-h value)
  (set-theme-value object 'grid 'minor-color-v value))

(defmethod (setf major-color) (value (object grid))
  (set-theme-value object 'grid 'major-color-h value)
  (set-theme-value object 'grid 'major-color-v value))

(defmethod (setf minor-color) (value (object grid))
  (set-theme-value object 'grid 'minor-color-h value)
  (set-theme-value object 'grid 'minor-color-v value))

(defmethod on-command ((object grid) &key)
  (if (visible object)
      (setf (visible object) nil)
      (setf (visible object) t)))

(defmethod on-paint ((object grid) &key)
  (with-slots (visible major minor left top width height) object
    (when visible
      (let ((mjch (get-theme-value object 'grid 'major-color-h))
            (mjcv (get-theme-value object 'grid 'major-color-v))
            (mnch (get-theme-value object 'grid 'minor-color-h))
            (mncv (get-theme-value object 'grid 'minor-color-v)))

        ;; Draw grid
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
          (let ((right (+ left width))
                (bottom (+ top height)))

            ;; Draw minor horizontals
            (do ((x left (+ x minor)))
                ((> x right))
              (al:draw-line x top x bottom mnch -0.5))
          
            ;; Draw minor verticals
            (do ((y top (+ y minor)))
                ((> y bottom))
              (al:draw-line left y right y mncv -0.5))
           
            ;; Draw major horizontals
            (do ((x left (+ x major)))
                ((> x right))
              (al:draw-line x top x bottom mjch -0.5))
          
            ;; Draw major verticals
            (do ((y top (+ y major)))
                ((> y bottom))
              (al:draw-line left y right y mjcv -0.5))))))))

(defmethod (setf color) (value (object grid))
  (set-theme-value object 'grid 'major-color-h value)
  (set-theme-value object 'grid 'major-color-v value)
  (set-theme-value object 'grid 'minor-color-h value)
  (set-theme-value object 'grid 'minor-color-v value))

(defmethod (setf color-h) (value (object grid))
  (set-theme-value object 'grid 'major-color-h value)
  (set-theme-value object 'grid 'minor-color-h value))

(defmethod (setf color-v) (value (object grid))
  (set-theme-value object 'grid 'major-color-v value)
  (set-theme-value object 'grid 'minor-color-v value))

(defmethod (setf major-color) (value (object grid))
  (set-theme-value object 'grid 'major-color-h value)
  (set-theme-value object 'grid 'major-color-v value))

(defmethod (setf minor-color) (value (object grid))
  (set-theme-value object 'grid 'minor-color-h value)
  (set-theme-value object 'grid 'minor-color-v value))
