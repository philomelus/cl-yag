(in-package #:cl-yag)

(defclass ruler (area-mixin
                 color-mixin)
  ((major :initarg :major :initform nil :accessor major)
   (minor :initarg :minor :initform nil :accessor minor)
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
    (if (eq nil (minor o))
        (format s ":minor nil ")
        (format s ":minor (~a) " (minor o)))
    (pprint-newline :linear s)

    (pprint-indent :current 0 s)
    (format s ":vertical ~a " (if (vertical o) "t" "nil"))
    (pprint-newline :linear s)

    (print-mixin o s)))

(defmethod on-paint ((object ruler) &key)
  (with-slots (vertical major minor left top width height color) object
    (if vertical
        ;; Draw vertical ruler
        (progn
          (let* ((bottom (+ top height))
                 (x (+ left width))
                 (xr (1- x)))
            (al:draw-line x (1- top) x bottom color -0.5)
            (let ((xj (- x width)))
              (do ((yj top (+ yj major)))
                  ((> yj bottom))
                (al:draw-line xj yj xr yj color -0.5)))
            (let ((xn (- x (truncate (/ width 2)))))
              (do ((yn top (+ yn minor)))
                  ((> yn bottom))
                (al:draw-line xn yn xr yn color -0.5)))))
        
        ;; Draw horizontal ruler
        (progn
          (let* ((right (+ left width))
                 (y (+ top height))
                 (yb (1- y)))
            ;; Draw mai line
            (al:draw-line (1- left) y right y color -0.5)
            ;; Draw major divisions
            (let ((yj (- y height)))
              (do ((xj left (+ xj major)))
                  ((> xj right))
                (al:draw-line xj yj xj yb color -0.5)))
            ;; Draw minor divisions
            (let ((yn (- y (truncate (/ height 2)))))
              (do ((xn left (+ xn minor)))
                  ((> xn right))
                (al:draw-line xn yn xn yb color -0.5))))))))

