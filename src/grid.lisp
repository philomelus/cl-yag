(in-package #:cl-yag)

;;;; theme-mixin ==============================================================

(defclass grid-theme-mixin ()
  ((major-color-h :initarg :major-color-h :initform nil :accessor major-color-h)
   (major-color-v :initarg :major-color-v :initform nil :accessor major-color-v)
   (minor-color-h :initarg :minor-color-h :initform nil :accessor minor-color-h)
   (minor-color-v :initarg :minor-color-v :initform nil :accessor minor-color-v)))

(defmethod print-mixin ((object grid-theme-mixin) stream)
  (pprint-color-nil major-color-h object stream)
  (pprint-color-nil major-color-v object stream)
  (pprint-color-nil minor-color-h object stream)
  (pprint-color-nil minor-color-v object stream)  
  (my-next-method))

;;;; grid =====================================================================

(defclass grid (grid-theme-mixin
                area-mixin
                shortcuts-mixin
                visible-mixin)
  ((major :initarg :major :initform 25 :accessor major)
   (minor :initarg :minor :initform 5 :accessor minor)))

(defmacro defgrid (&rest rest &key &allow-other-keys)
  `(make-instance 'grid ,@rest))

;; (defmethod print-object ((o grid) s)
;;   (pprint-indent :current 0 s)
;;   (pprint-logical-block (s nil)
;;     (format s "defgrid ")
;;     (pprint-field-nil major o s)
;;     (pprint-field-nil minor o s)
;;     (print-mixin o s)))

;;; methods ---------------------------------------------------------

(defmethod (setf color) (value (object grid))
  (setf (major-color-h object) value)
  (setf (major-color-v object) value)
  (setf (minor-color-h object) value)
  (setf (minor-color-v object) value))

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
  (with-slots (visible major minor left top width height) object
    (when visible
      (let ((mjch (major-color-h object))
            (mjcv (major-color-v object))
            (mnch (minor-color-h object))
            (mncv (minor-color-v object)))
        ;; Make sure we have colors
        (when (or (eql mjch nil)
                  (eql mjcv nil)
                  (eql mnch nil)
                  (eql mncv nil))
          (let ((theme (find-theme object)))
            (when (eql mjch nil)
              (setq mjch (major-color-h theme)))
            (when (eql mjcv nil)
              (setq mjcv (major-color-v theme)))
            (when (eql mnch nil)
              (setq mnch (minor-color-h theme)))
            (when (eql mncv nil)
              (setq mncv (minor-color-v theme)))))

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

(defmethod (setf color) (value (object grid))
  (setf (major-color-h object) value)
  (setf (major-color-v object) value)
  (setf (minor-color-h object) value)
  (setf (minor-color-v object) value))

(defmethod (setf color-h) (value (object grid))
  (setf (major-color-h object) value)
  (setf (minor-color-h object) value))

(defmethod (setf color-v) (value (object grid))
  (setf (major-color-v object) value)
  (setf (minor-color-v object) value))

(defmethod (setf major-color) (value (object grid))
  (setf (major-color-h object) value)
  (setf (major-color-v object) value))

(defmethod (setf minor-color) (value (object grid))
  (setf (minor-color-h object) value)
  (setf (minor-color-v object) value))

(defmethod (setf theme) ((theme grid-theme-mixin) (object grid))
  (with-slots ((mjh major-color-h) (mjv major-color-v)
               (mnh minor-color-h) (mnv minor-color-v))
      theme
    (setf (major-color-h object) mjh)
    (setf (major-color-v object) mjv)
    (setf (minor-color-h object) mnh)
    (setf (minor-color-v object) mnv)))
 
