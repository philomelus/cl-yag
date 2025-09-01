(in-package #:cl-yag)

;;;; theme-mixin ==============================================================

(defclass ruler-theme-mixin (color-mixin)
  ((major-color :initarg :major-color :initform nil :accessor major-color)
   (minor-color :initarg :minor-color :initform nil :accessor minor-color)))

(defmethod print-mixin ((object ruler-theme-mixin) &optional stream)
  (declare (ignore stream))
  ;; (pprint-color-nil major-color object stream)
  ;; (pprint-color-nil major-color object stream)
  (my-next-method))

;;;; ruler ====================================================================

(defclass ruler (ruler-theme-mixin
                 area-mixin
                 shortcuts-mixin
                 visible-mixin)
  ((major :initarg :major :initform 10 :accessor major)
   (minor :initarg :minor :initform 5 :accessor minor)
   (vertical :initarg :vertical :initform nil :accessor vertical)
   (invert :initarg :invert :initform nil :accessor invert :documentation "When t, draw on right/bottom instead of left/top")))

(defmacro defruler (&rest rest &key &allow-other-keys)
  `(make-instance 'ruler ,@rest))

;;; methods ---------------------------------------------------------

(defmethod on-command ((object ruler) &key)
  (if (visible object)
      (setf (visible object) nil)
      (setf (visible object) t)))

(defmethod on-paint ((object ruler) &key)
  (with-slots (visible vertical major minor left top width height)
      object
    (when visible
      (let ((mjc (major-color object))
            (mnc (minor-color object))
            (c (color object)))
       
        ;; Make sure colors are valid
        (when (or (eql mjc nil)
                  (eql mnc nil)
                  (eql c nil))
          (let ((theme (find-theme object)))
            (when (eql c nil)
              (setf c (color theme)))
            (when (eql mjc nil)
              (setf mjc (major-color theme)))
            (when (eql mnc nil)
              (setf mnc (minor-color theme)))))
      
        (if vertical
            ;; Draw vertical ruler
            (progn
              (let* ((bottom (+ top height))
                     (x (if (invert object) (- left width) (+ left width)))
                     (xr (1- x)))
                (al:draw-line x (1- top) x bottom c -0.5)
               
                ;; NOTE:  Not effecient, as major will overwirte the common minor's,
                ;;        so some minors are drawn when not needed.  The exact
                ;;        algorithm to do both at same time escapes me at the moment
                ;;        so this is a kluge until I decide to fix it.
                ;;        Drawing both in same loop doesn't work because we cannot
                ;;        increment both at the same time.  Updating major for every
                ;;        partial minor would work, technically, but then floating
                ;;        point errors would be an issue...
               
                ;; Draw minor first
                (let ((xn (if (invert object) (+ x (truncate (/ width 2))) (- x (truncate (/ width 2))))))
                  (do ((yn top (+ yn minor)))
                      ((> yn bottom))
                    (al:draw-line xn yn xr yn mnc -0.5)))
                (let ((xj (if (invert object) (+ x width) (- x width))))
                  (do ((yj top (+ yj major)))
                      ((> yj bottom))
                    (al:draw-line xj yj xr yj mjc -0.5)))))
        
            ;; Draw horizontal ruler
            (progn
              (let* ((right (+ left width))
                     (y (if (invert object) (- top height) (+ top height)))
                     (yb (1- y)))
                ;; Draw mai line
                (al:draw-line (1- left) y right y c -0.5)
               
                ;; NOTE:  Not effecient, as major will overwirte the common minor's,
                ;;        so some minors are drawn when not needed.  The exact
                ;;        algorithm to do both at same time escapes me at the moment
                ;;        so this is a kluge until I decide to fix it.
                ;;        Drawing both in same loop doesn't work because we cannot
                ;;        increment both at the same time.  Updating major for every
                ;;        partial minor would work, technically, but then floating
                ;;        point errors would be an issue...
               
                ;; Draw minor divisions
                (let ((yn (if (invert object) (+ y (truncate (/ height 2))) (- y (truncate (/ height 2))))))
                  (do ((xn left (+ xn minor)))
                      ((> xn right))
                    (al:draw-line xn yn xn yb mnc -0.5)))
                ;; Draw major divisions
                (let ((yj (if (invert object) (+ y height) (- y height))))
                  (do ((xj left (+ xj major)))
                      ((> xj right))
                    (al:draw-line xj yj xj yb mjc -0.5))))))))))


(defmethod (setf theme) ((theme ruler-theme-mixin) (object ruler))
  (with-slots ((mj major-color) (mn minor-color) (c color)) theme
    (setf (color object) c)
    (setf (major-color object) mj)
    (setf (minor-color object) mn)))

