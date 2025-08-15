(in-package #:cl-yag)

(defclass ruler (area-mixin
                 color-mixin)
  ((major :initarg :major :initform nil :accessor major)
   (major-color :initarg :major-color :initform nil :accessor major-color)
   (minor :initarg :minor :initform nil :accessor minor)
   (minor-color :initarg :minor-color :initform nil :accessor minor-color)
   (vertical :initarg :vertical :initform nil :accessor vertical)))

(defmacro defruler (&rest rest &key &allow-other-keys)
  `(make-instance 'ruler ,@rest))

(defmethod print-object ((o ruler) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "defruler ")

    (pprint-indent :current 0 s)
    (if (eq nil (major o))
        (format s ":major nil ")
        (format s ":major (~a) " (major o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (if (eq nil (major-color o))
        (format s ":major-color nil ")
        (format s ":major-color (~a) " (print-color (major-color o))))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (if (eq nil (minor o))
        (format s ":minor nil ")
        (format s ":minor (~a) " (minor o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (if (eq nil (minor-color o))
        (format s ":minor-color nil ")
        (format s ":minor-color (~a) " (print-color (minor-color o))))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":vertical ~a " (if (vertical o) "t" "nil"))
    (pprint-newline :linear s)

    (print-mixin o s)))

(defmethod on-paint ((object ruler) &key)
  (with-slots (vertical major major-color minor minor-color left top width height color) object
    
    (let ((mjc major-color)
          (mnc minor-color))
      ;; Make sure colors are valid
      (when (eql nil mjc)
        (setf mjc color))
      (when (eql nil mnc)
        (setf mnc color))
      
     (if vertical
         ;; Draw vertical ruler
         (progn
           (let* ((bottom (+ top height))
                  (x (+ left width))
                  (xr (1- x)))
             (al:draw-line x (1- top) x bottom color -0.5)
             ;; Draw minor first
             ;; NOTE:  Not effecient, as major will overwirte the common minor's,
             ;;        so some minors are drawn when not needed.  The exact
             ;;        algorithm to do both at same time escapes me at the moment
             ;;        so this is a kluge until I decide to fix it.
             ;;        Drawing both in same loop doesn't work because we cannot
             ;;        increment both at the same time.  Updating major for every
             ;;        partial minor would work, technically, but then floating
             ;;        point errors would be an issue...
             (let ((xn (- x (truncate (/ width 2)))))
               (do ((yn top (+ yn minor)))
                   ((> yn bottom))
                 (al:draw-line xn yn xr yn mnc -0.5)))
             (let ((xj (- x width)))
               (do ((yj top (+ yj major)))
                   ((> yj bottom))
                 (al:draw-line xj yj xr yj mjc -0.5)))))
        
         ;; Draw horizontal ruler
         (progn
           (let* ((right (+ left width))
                  (y (+ top height))
                  (yb (1- y)))
             ;; Draw mai line
             (al:draw-line (1- left) y right y color -0.5)
             ;; Draw minor divisions
             ;; NOTE:  Not effecient, as major will overwirte the common minor's,
             ;;        so some minors are drawn when not needed.  The exact
             ;;        algorithm to do both at same time escapes me at the moment
             ;;        so this is a kluge until I decide to fix it.
             ;;        Drawing both in same loop doesn't work because we cannot
             ;;        increment both at the same time.  Updating major for every
             ;;        partial minor would work, technically, but then floating
             ;;        point errors would be an issue...
             (let ((yn (- y (truncate (/ height 2)))))
               (do ((xn left (+ xn minor)))
                   ((> xn right))
                 (al:draw-line xn yn xn yb mnc -0.5)))
             ;; Draw major divisions
             (let ((yj (- y height)))
               (do ((xj left (+ xj major)))
                   ((> xj right))
                 (al:draw-line xj yj xj yb mjc -0.5)))))))))

