(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

;;;; THEME-DATA  ==============================================================

(defparameter *BORDER-THEME-DATA* `((border nil width)
                                    (border flat color)
                                    (border 3d style)
                                    (border 3d color)
                                    (border 3d dark-color)
                                    (border 3d light-color)
                                    (border 3d very-dark-color)
                                    (border 3d very-light-color)))

;;;; BORDER ===================================================================

(defclass border (color-mixin
                  thickness-mixin)
  ((thickness :type (or null thickness-type))
   (style :type (or null theme-style) :initarg :style :initform nil :accessor style)))

(defmacro defborder (&rest rest &key &allow-other-keys)
  `(make-instance 'border ,@rest))

;;; METHODS ---------------------------------------------------------

(defmethod initialize-instance :before ((object border) &key)
  (unless (theme-value-defaultp 'border nil 'border-color)
    (let ((frame-color (get-theme-value-default nil nil 'frame-color)))
      (set-theme-value-default 'border nil 'border-color frame-color)
      (set-theme-value-default 'border 'flat 'border-color frame-color))
    (set-theme-value-default 'border nil 'thickness 1)
    (let ((color (get-theme-value-default nil nil 'color))
          (dark-color (get-theme-value-default nil nil 'dark-color))
          (light-color (get-theme-value-default nil nil 'light-color))
          (very-dark-color (get-theme-value-default nil nil 'very-dark-color))
          (very-light-color (get-theme-value-default nil nil 'very-light-color)))
      (dolist (style '(:3d-in :3d-out :3d-flat))
        (set-theme-value-default 'border style 'color color)
        (set-theme-value-default 'border style 'dark-color dark-color)
        (set-theme-value-default 'border style 'light-color light-color)
        (set-theme-value-default 'border style 'very-dark-color very-dark-color)
        (set-theme-value-default 'border style 'very-light-color very-light-color)))))

(defmethod paint-border-bottom ((border border) (style (eql :3d-out)) object &key blend-left blend-right)
  (paint-border-bottom-3d border style object blend-left blend-right))

(defmethod paint-border-bottom ((border border) (style (eql :3d-in)) object &key blend-left blend-right)
  (paint-border-bottom-3d border style object blend-left blend-right))

(defmethod paint-border-bottom ((border border) (style (eql :3d-flat)) object &key blend-left blend-right)
  (paint-border-bottom-3d border style object blend-left blend-right))

(defun paint-border-bottom-3d (border style object blend-left blend-right)
  (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors 'border style object)
    (declare (ignorable lto lti rbo rbi))
    (with-area-and-spacing (object-left object-top object-right object-bottom) object
      (assert (< object-left object-right))
      (assert (< object-top object-bottom))
      (let ((thickness (thickness border)))
        (when (null thickness)
          (setf thickness (get-theme-value object 'border 'thickness :style style)))
        (let ((1/4-thick (/ thickness 4))
              (1/2-thick (/ thickness 2))
              (others ()))
          (when blend-left
            (push :left others))
          (when blend-right
            (push :right others))
          (let ((3/4-thick (+ 1/2-thick 1/4-thick)))
            (draw-side :bottom
                       (+ object-left 3/4-thick) (- object-top 1/4-thick)
                       (- object-right 3/4-thick) (- object-bottom 1/4-thick)
                       1/2-thick rbo :others others)
            (draw-side :bottom
                       (+ object-left 3/4-thick) (- object-top 3/4-thick)
                       (- object-right 3/4-thick) (- object-bottom 3/4-thick)
                       1/2-thick rbi :others others :inside t)))))))

(defmethod paint-border-bottom ((border border) (style (eql :flat)) object &key blend-left blend-right)
  (declare (ignorable blend-left blend-right))
  (let ((color (color border))
        (thickness (thickness border)))
    (when (null color)
      (setf color (get-theme-value object 'border 'border-color)))
    (when (null thickness)
      (setf thickness (get-theme-value object 'border 'thickness :style style)))
    (when (> thickness 0)
      (with-area-and-spacing (asl ast asr asb) (a:ensure-car object)
        (let ((w2 (/ thickness 2)))
          (let ((yy (+ asb (- thickness) w2)))
            (al:draw-line asl yy asr yy color thickness)))))))

(defmethod paint-border-left ((border border) (style (eql :3d-out)) object &key blend-top blend-bottom)
  (paint-border-left-3d border style object blend-top blend-bottom))

(defmethod paint-border-left ((border border) (style (eql :3d-in)) object &key blend-top blend-bottom)
  (paint-border-left-3d border style object blend-top blend-bottom))

(defmethod paint-border-left ((border border) (style (eql :3d-flat)) object &key blend-top blend-bottom)
  (paint-border-left-3d border style object blend-top blend-bottom))

(defun paint-border-left-3d (border style object blend-top blend-bottom)
  (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors 'border style object)
    (declare (ignorable lto lti rbo rbi))
    (with-area-and-spacing (object-left object-top object-right object-bottom) object
      (let ((thickness (thickness border)))
        (when (null thickness)
          (setf thickness (get-theme-value object 'border 'thickness :style style)))
        (when (> thickness 0)
          (let ((1/4-thick (/ thickness 4))
                (1/2-thick (/ thickness 2))
                (others ()))
            (when blend-top
              (push :top others))
            (when blend-bottom
              (push :bottom others))
            (let ((3/4-thick (+ 1/4-thick 1/2-thick)))
              (draw-side :left
                         (+ object-left 1/4-thick) (+ object-top 1/4-thick)
                         (+ object-right 1/4-thick) (- object-bottom 1/4-thick) 
                         1/2-thick lto :others others)
              (draw-side :left
                         (+ object-left 3/4-thick) (+ object-top 3/4-thick)
                         (+ object-right 3/4-thick) (- object-bottom 3/4-thick)
                         1/2-thick lti :others others :inside t))))))))

(defmethod paint-border-left ((border border) (style (eql :flat)) object &key blend-top blend-bottom)
  (declare (ignorable blend-top blend-bottom))
  (let ((color (color border))
        (tn (thickness border)))
    (when (null color)
      (setf color (get-theme-value object 'border 'border-color :style style)))
    (when (null tn)
      (setf tn (get-theme-value object 'border 'thickness :style style)))
    (when (> tn 0)
      (with-area-and-spacing (asl ast asr asb) (a:ensure-car object)
        (let ((w tn)
              (w2 (/ tn 2)))
          (let ((xx (+ asl w2)))
            (al:draw-line xx ast xx asb color w)))))))

(defmethod paint-border-right ((border border) (style (eql :3d-out)) object &key blend-top blend-bottom)
  (paint-border-right-3d border style object blend-top blend-bottom))

(defmethod paint-border-right ((border border) (style (eql :3d-in)) object &key blend-top blend-bottom)
  (paint-border-right-3d border style object blend-top blend-bottom))

(defmethod paint-border-right ((border border) (style (eql :3d-flat)) object &key blend-top blend-bottom)
  (paint-border-right-3d border style object blend-top blend-bottom))

(defun paint-border-right-3d (border style object blend-top blend-bottom)
  (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors 'border style object)
    (declare (ignorable lto lti rbo rbi))
    (with-area-and-spacing (object-left object-top object-right object-bottom) object
      (let ((thickness (thickness border)))
        (when (null thickness)
          (setf thickness (get-theme-value object 'border 'thickness :style style)))
        (when (> thickness 0)
          (let ((1/4-thick (/ thickness 4))
                (1/2-thick (/ thickness 2))
                (others ()))
            (when blend-top
              (push :top others))
            (when blend-bottom
              (push :bottom others))
            (let ((3/4-thick (+ 1/2-thick 1/4-thick)))
              (draw-side :right (- object-left 1/4-thick) (+ object-top 3/4-thick)
                         (- object-right 1/4-thick) (- object-bottom 1/4-thick)
                         1/2-thick rbo :others others)
              (draw-side :right (- object-left 3/4-thick) (+ object-top 3/4-thick)
                         (- object-right 3/4-thick) (- object-bottom 3/4-thick)
                         1/2-thick rbi :others others :inside t))))))))

(defmethod paint-border-right ((border border) (style (eql :flat)) object &key blend-top blend-bottom)
  (declare (ignorable blend-top blend-bottom))
  (let ((color (color border))
        (tn (thickness border)))
    (when (null color)
      (setf color (get-theme-value object 'border 'border-color :style style)))
    (when (null tn)
      (setf tn (get-theme-value object 'border 'thickness :style style)))
    (when (> tn 0)
      (with-area-and-spacing (asl ast asr asb) (a:ensure-car object)
        (let ((w2 (/ tn 2)))
          (let ((xx (+ asr (- tn) w2)))
            (al:draw-line xx ast xx asb color tn)))))))

(defmethod paint-border-top ((border border) (style (eql :3d-out)) object &key blend-left blend-right)
  (paint-border-top-3d border style object blend-left blend-right))

(defmethod paint-border-top ((border border) (style (eql :3d-in)) object &key blend-left blend-right)
  (paint-border-top-3d border style object blend-left blend-right))

(defmethod paint-border-top ((border border) (style (eql :3d-flat)) object &key blend-left blend-right)
  (paint-border-top-3d border style object blend-left blend-right))

(defun paint-border-top-3d (border style object blend-left blend-right)
  (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors 'border style object)
    (declare (ignorable lto lti rbo rbi))
    (with-area-and-spacing (object-left object-top object-right object-bottom) object
      (let ((thickness (thickness border)))
        (when (null thickness)
          (setf thickness (get-theme-value object 'border 'thickness :style style)))
        (when (> thickness 0)
          (let ((1/4-thick (/ thickness 4))
                (1/2-thick (/ thickness 2))
                (others ()))
            (when blend-left
              (push :left others))
            (when blend-right
              (push :right others))
            (let ((3/4-thick (+ 1/4-thick 1/2-thick)))
              (draw-side :top (+ object-left 1/4-thick) (+ object-top 1/4-thick)
                         object-right (+ object-bottom 1/4-thick)
                         1/2-thick lto :others others)
              (draw-side :top (+ object-left 3/4-thick) (+ object-top 3/4-thick)
                         object-right (+ object-bottom 3/4-thick)
                         1/2-thick lti :others others :inside t))))))))

(defmethod paint-border-top ((border border) (style (eql :flat)) object &key blend-left blend-right)
  (declare (ignorable blend-left blend-right))
  (let ((color (color border))
        (tn (thickness border)))
    (when (null color)
      (setf color (get-theme-value object 'border 'border-color :style style)))
    (when (null tn)
      (setf tn (get-theme-value object 'border 'thickness :style style)))
    (when (> tn 0)
      (with-area-and-spacing (asl ast asr asb) (a:ensure-car object)
        (let ((w2 (/ tn 2)))
          (let ((yy (+ ast w2)))
            (al:draw-line asl yy asr yy color tn)))))))

;;;; MACROS ===================================================================

(deftype border-thickness-type () '(thickness-type))

(defmacro with-borders ((left right top bottom) object &body body)
  (a:with-gensyms (instance)
    `(let ((,instance ,object))
       (let ((,left (border-left ,instance))
             (,right (border-right ,instance))
             (,top (border-top ,instance))
             (,bottom (border-bottom ,instance)))
         (declare (ignorable ,left ,right, top ,bottom))
         ,@body))))

