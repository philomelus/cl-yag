(in-package #:cl-yag)

(defclass grid (area-mixin
                color-mixin
                shortcuts-mixin
                visible-mixin)
  ((major :initarg :major :initform 25 :accessor major)
   (major-color-h :initarg :major-color-h :initform nil :accessor major-color-h)
   (major-color-v :initarg :major-color-v :initform nil :accessor major-color-v)
   (minor :initarg :minor :initform 5 :accessor minor)
   (minor-color-h :initarg :minor-color-h :initform nil :accessor minor-color-h)
   (minor-color-v :initarg :minor-color-v :initform nil :accessor minor-color-v)))

(defmacro defgrid (&rest rest &key &allow-other-keys)
  `(make-instance 'grid ,@rest))

(defmethod print-object ((o grid) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "defgrid ")

    (pprint-indent :current 0 s)
    (if (eq nil (major o))
        (format s ":major nil ")
        (format s ":major (~a) " (major o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (if (eq nil (major-color-h o))
        (format s ":major-color-h nil ")
        (format s ":major-color-h (~a) " (print-color (major-color-h o))))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (if (eq nil (major-color-v o))
        (format s ":major-color-v nil ")
        (format s ":major-color-v (~a) " (print-color (major-color-v o))))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (if (eq nil (minor o))
        (format s ":minor nil ")
        (format s ":minor (~a) " (minor o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (if (eq nil (minor-color-h o))
        (format s ":minor-color-h nil ")
        (format s ":minor-color-h (~a) " (print-color (minor-color-h o))))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (if (eq nil (minor-color-v o))
        (format s ":minor-color-v nil ")
        (format s ":minor-color-v (~a) " (print-color (minor-color-v o))))
    (pprint-newline :linear s)

    (print-mixin o s)))

(defmethod (setf major-color) (value (object grid))
  (setf (major-color-h object) value)
  (setf (major-color-v object) value))

(defmethod (setf minor-color) (value (object grid))
  (setf (minor-color-h object) value)
  (setf (minor-color-v object) value))

(defmethod on-command ((object grid) &key)
  (if (visible object)
      (setf (visible object) nil)
      (setf (visible object) t)))

(defmethod on-paint ((object grid) &key)
  (with-slots (visible major minor left top width height color) object
    (when visible
      (let ((mjch (major-color-h object))
            (mjcv (major-color-v object))
            (mnch (minor-color-h object))
            (mncv (minor-color-v object)))
        
        ;; Make sure we have colors
        (when (eql mjch nil)
          (setq mjch color))
        (when (eql mjcv nil)
          (setq mjcv color))
        (when (eql mnch nil)
          (setq mnch color))
        (when (eql mncv nil)
          (setq mncv color))

        ;; Draw grid
       
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-DEST-COLOR+)
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
