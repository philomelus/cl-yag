(in-package #:cl-yag)

;; TODO: Add ability to thickness of lines
;; TODO: Add ability to specify length of lines (as percentage or exact length)
;; TODO: Add ability to have more than 1 major and 1 minor
;; TODO: As previous, but allow full options for all available divisions,
;;       regardless of the number of them.  Specify division, length,
;;       thickness, and color per division.  I'm thinking implementing like
;;       how borders are done where divisions are generic, and you specify
;;       the details.  Having some default sizes (10,2) (10,5) (25,5) (100,25,10)
;;       and whatever others make sense.

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

(defparameter +RULER-ALIGN-OPTIONS+ '(:begin :center :middle :end))

(defclass ruler (ruler-theme-mixin
                 area-mixin
                 parent-mixin
                 shortcuts-mixin
                 visible-mixin)
  ((major :initarg :major :initform 10 :accessor major)
   (minor :initarg :minor :initform 5 :accessor minor)
   (vertical :initarg :vertical :initform nil :accessor vertical)
   (align :initarg :align :initform :end :accessor align)))

(defmacro defruler (&rest rest &key &allow-other-keys)
  `(make-instance 'ruler ,@rest))

(defmethod initialize-instance :after ((object ruler) &key)
  (validate-ruler-options object))

(defmethod (setf align) :after (value (object ruler))
  (validate-ruler-options object))

;;; methods ---------------------------------------------------------

(defmethod on-command ((object ruler) &key)
  (if (visible object)
      (setf (visible object) nil)
      (setf (visible object) t)))

(defmethod on-paint ((object ruler) &key)
  (with-local-slots (visible vertical) object
    (when visible
      (if vertical
          (paint-ruler-vertical object)
          (paint-ruler-horizontal object)))))

;; (defmethod (setf theme) ((theme ruler-theme-mixin) (object ruler))
;;   (with-slots ((mj major-color) (mn minor-color) (c color)) theme
;;     (setf (color object) c)
;;     (setf (major-color object) mj)
;;     (setf (minor-color object) mn)))

;;; functions -------------------------------------------------------

(declaim (ftype (function (ruler) null) paint-ruler-horizontal))
(defun paint-ruler-horizontal (object)
  (with-local-accessors (left top width height) object
    (with-local-slots (major minor align) object
      (with-object-or-theme ((mjc major-color) (mnc minor-color) (c color)) object
        (let* ((right (+ left width))
               (y (case align
                    (:begin
                     top)
                    ((:center :middle)
                     (+ top (/ height 2)))
                    (:end
                     (+ top height))))
               (yb (1- y))
               major-divs minor-divs)
          
          ;; Draw main line
          (al:draw-line (1- left) y right y c -0.5)
          
          ;; Calculate the major divisions
          (do ((h left (+ h major)))
              ((> h right))
            (push h major-divs))
          
          ;; Calculate the minor divisions
          (do ((h left (+ h minor)))
              ((> h right))
            (if (not (member h major-divs))
                (push h minor-divs)))
          
          ;; Draw minor divisions
          (let (yn)
            (case align
              (:begin
               (incf yb)
               (setq yn (+ y (/ height 2)))
               (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-SRC-COLOR+)
                 (dolist (div minor-divs)
                   (al:draw-line div yn div yb mnc -0.5))))
              ((:center :middle)
               (setq yb (+ y (/ height 4)))
               (setq yn (- y (/ height 4)))
               (let ((ynbh (/ (- yb yn 1) 2)))
                 (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-SRC-COLOR+)
                   (dolist (div minor-divs)
                     (al:draw-line div yn div (+ yn ynbh) mnc -0.5)
                     (al:draw-line div (- yb ynbh) div yb mnc -0.5)))))
              (:end
               (setq yn (- y (/ height 2)))
               (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-SRC-COLOR+)
                 (dolist (div minor-divs)
                   (al:draw-line div yn div yb mnc -0.5))))))
          
          ;; Draw major divisions
          (let (yj)
            (case align
              (:begin
               (setq yj (+ y height))
               (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-SRC-COLOR+)
                 (dolist (div major-divs)
                   (al:draw-line div yj div yb mjc -0.5))))
              ((:center :middle)
               (setq yb (+ y (/ height 2)))
               (setq yj (- y (/ height 2)))
               (let ((yjbh (/ (- yb yj 1) 2))
                     (s (min yj yb))
                     (e (max yj yb)))
                 (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-SRC-COLOR+)
                   (dolist (div major-divs)
                     (al:draw-line div (1- s) div (+ s yjbh) mjc -0.5)
                     (al:draw-line div (- e yjbh) div e mjc -0.5)))))
              (:end
               (setq yj (- y height))
               (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-SRC-COLOR+)
                 (dolist (div major-divs)
                   (al:draw-line div (1- (min yj yb)) div (max yj yb) mjc -0.5)))))))))))

(declaim (ftype (function (ruler) null) paint-ruler-vertical))
(defun paint-ruler-vertical (object)
  (with-local-accessors (left top width height) object
    (with-local-slots (major minor align) object
      (with-object-or-theme ((mjc major-color) (mnc minor-color) (c color)) object
        ;; Draw vertical ruler
        (let* ((bottom (+ top height))
               (x (case align
                    (:begin
                     left)
                    ((:center :middle)
                     (+ left (/ width 2)))
                    (:end
                     (+ left width))))
               (xr (1- x))
               major-divs minor-divs)
          (al:draw-line x (1- top) x bottom c -0.5)

          ;; Calculate the major divisions
          (do ((v top (+ v major)))
              ((> v bottom))
            (push v major-divs))
          
          ;; Calculate the minor divisions
          (do ((v top (+ v minor)))
              ((> v bottom))
            (if (not (member v major-divs))
                (push v minor-divs)))
          
          ;; Draw minor division
          (let (xn)
            (case align
              (:begin
               (incf xr)
               (setq xn (+ x (/ width 2)))
               (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-SRC-COLOR+)
                 (dolist (div minor-divs)
                   (al:draw-line xn div xr div mnc -0.5))))
              ((:center :middle)
               (setq xr (+ x (/ width 4)))
               (setq xn (- x (/ width 4)))
               (let ((xnrw (/ (- xr xn 1) 2)))
                 (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-SRC-COLOR+)
                   (dolist (div minor-divs)
                     (al:draw-line xn div (+ xn xnrw) div mnc -0.5)
                     (al:draw-line (- xr xnrw) div xr div mnc -0.5)))))
              (:end
               (setq xn (- x (/ width 2)))
               (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-SRC-COLOR+)
                 (dolist (div minor-divs)
                   (al:draw-line xn div xr div mnc -0.5))))))

          ;; Draw major division
          (let (xj)
            (case align
              (:begin
               (setq xj (+ x width))
               (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-SRC-COLOR+)
                 (dolist (div major-divs)
                   (al:draw-line xj div xr div mjc -0.5))))
              ((:center :middle)
               (setq xj (- x (/ width 2)))
               (setq xr (+ x (/ width 2)))
               (let ((xjrw (/ (- xr xj 1) 2))
                     (s (min xj xr))
                     (e (max xj xr)))
                 (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-SRC-COLOR+)
                   (dolist (div major-divs)
                     (al:draw-line (1- s) div (+ s xjrw) div mjc -0.5)
                     (al:draw-line (- e xjrw) div e div mjc -0.5)))))
              (:end
               (setq xj (- x width))
               (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-SRC-COLOR+)
                 (dolist (div major-divs)
                   (al:draw-line (1- (min xj xr)) div (max xj xr) div mjc -0.5)))))))))))

(declaim (ftype (function (ruler) null) validate-ruler-options))
(defun validate-ruler-options (object)
  (with-local-slots (align) object
    (unless (member align +RULER-ALIGN-OPTIONS+)
      (error "unknown align option: ~a" align))))

;;;; macros ===================================================================

;; These are great ideas ... just have no clue how to make them work
;;
;; What I want is when I create a bunch of the same type of rulers,
;; to specify the ruler colors once and have the defruler macro
;; automatically pick it up.  Again, no idea how to make it happen.

;; (defmacro with-ruler-colors (major-color minor-color)
;;   )

;; (defmacro with-ruler-major-color (major-color)
;;   )

;; (defmacro with-ruler-minor-color (minor-color)
;;   )
