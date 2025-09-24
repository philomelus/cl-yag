(in-package #:cl-yag)

;;;; BUTTON ===================================================================

;;; THEME-MIXIN -----------------------------------------------------

;; Default hover location is in spacing between border and object egdge. Thus,
;; you'd probably want a spacing of at least 3 for it to show up good.

(defclass button-theme-mixin (font-mixin
                              fore-color-mixin
                              interior-color-mixin)
  ((down-color :initarg :down-color :initform nil :accessor down-color :documentation "Color for title when button commanded by click.")
   (borderp :type boolean :initarg :borderp :initform t :accessor borderp :documentation "When T border is drawn around button, and moved down and to the right when commanded via click.")
   (border-thickness :type border-thickness-type :initarg :border-thickness :accessor border-thickness :documentation "Thickness of the border around the button.  Different themes require different minimums.")
   (hover-color :initarg :hover-color :initform (al:map-rgb-f 0.5 0.5 0.5) :accessor hover-color :documentation "Color of hover outline and title when widget is active (mouse is over).")
   (hover-inside-borderp :type boolean :initarg :hover-inside-borderp :initform t :accessor hover-inside-borderp :documentation "When T hover is draw between title and start of border padding.")
   (hover-inside-paddingp :type boolean :initarg :hover-inside-paddingp :initform nil :accessor hover-inside-paddingp :documentation "When T hover is drawn inside border padding.")
   (hover-thickness :type thickness-type :initarg :hover-thickness :accessor hover-thickness :documentation "Thickness of hover line.")
   (up-color :initarg :up-color :initform nil :accessor up-color :documentation "Deprecated.")))

(defclass button-theme-3d-mixin (button-theme-mixin)
  ((border-thickness :initform 2)
   (hover-thickness :initform 2)))

(defclass button-theme-flat-mixin (button-theme-mixin
                                   color-3d-mixin)
  ((border-thickness :initform 1)
   (hover-thickness :initform 1)))

;;; button -----------------------------------------------------

(defclass button (area-mixin
                  border-mixin
                  h-align-mixin
                  padding-mixin
                  parent-mixin
                  spacing-mixin
                  title-mixin
                  button-theme-mixin
                  shortcuts-mixin
                  v-align-mixin)
  
  ((inside :initform nil :type boolean)     ;internal use
   (down :initform nil :type boolean)       ;internal use
   (was-down :initform nil :type boolean))) ;internal use

(defmacro defbutton (&rest rest &key &allow-other-keys)
  `(make-instance 'button ,@rest))

;;; methods ---------------------------------------------------------

(defmethod calc-height (type (area %rect) (object button))
  (v:debug :layout "[calc-height] {button} area: (~d ~d) @ (~d ~d) ~a"
           (width area) (height area) (left area) (top area) (print-raw-object object))
  (let ((rv (text-calc-height type area object)))
    (v:debug :layout "[calc-height] {button} result:~d" rv (print-raw-object object))
    rv))

(defmethod calc-left (type (area %rect) width height (object button))
  (v:debug :layout "[calc-left] {button} area: (~d ~d) @ (~d ~d) ~a"
           (width area) (height area) (left area) (top area) (print-raw-object object))
  (let ((rv (text-calc-left type area width height object)))
    (v:debug :layout "[calc-left] {button} result:~d ~a" rv (print-raw-object object))
    rv))

(defmethod calc-top (type (area %rect) width height (object button))
  (v:debug :layout "[calc-top] {button} area: (~d ~d) @ (~d ~d) ~a"
           (width area) (height area) (left area) (top area) (print-raw-object object))
  (let ((rv (text-calc-top type area width height object)))
    (v:debug :layout "[calc-top] {button} result:~d" rv (print-raw-object object))
    rv))

(defmethod calc-width (type (area %rect) (object button))
  (v:debug :layout "[calc-width] {button} area: (~d ~d) @ (~d ~d) ~a"
           (width area) (height area) (left area) (top area) (print-raw-object object))
  (let ((rv (text-calc-width type area object))) 
    (v:debug :layout "[calc-width] {button} result:~d ~a" rv (print-raw-object object))
    rv))

(defmethod on-mouse-down (x y b (obj button) &key)
  (if (and (= b +MOUSE-BUTTON-LEFT+)
           (within x y obj)
           (not (slot-value obj 'was-down)))
      (progn
        (setf (slot-value obj 'was-down) t)
        (on-mouse-down-accept obj (owner obj))
        (return-from on-mouse-down t)))
  nil)

(defmethod on-mouse-move (x y dx dy (obj button) &key)
  (if (within x y obj)
      (if (not (slot-value obj 'inside))
          (setf (slot-value obj 'inside) t))
      (if (slot-value obj 'inside)
          (setf (slot-value obj 'inside) nil))))

(defmethod on-mouse-up (x y b (obj button) &key)
  (if (and (slot-value obj 'was-down)
           (= b 1))
      (if (within x y obj)
          (progn
            (v:debug :event "on-mouse-up: button: :x ~d :y ~d :b ~d" x y b)
            (setf (slot-value obj 'was-down) nil
                  (slot-value obj 'down) t)
            (on-command obj)
            (setf (slot-value obj 'down) nil)
            (return-from on-mouse-up t))
          (progn
            (setf (slot-value obj 'was-down) nil
                  (slot-value obj 'down) nil)
            (v:debug :event "on-mouse-up: button: aborted"))))
  nil)

(defmethod on-paint ((obj button) &key)
  (with-local-slots ((in inside) (down was-down)) obj
    (with-local-accessors (left top width height) obj
      (with-object-or-theme ((cd down-color) (ch hover-color) (cu up-color)
                             (fg fore-color) (ic interior-color)
                             (fnt font))
                            obj
      
        ;; Draw background
        (al:draw-filled-rectangle (left obj) (top obj) (right obj) (+ (bottom obj) 0.99) ic)
      
        ;; Draw border
        (paint-border obj (find-theme obj))
      
        ;; Draw text
        (multiple-value-bind (x f) (text-calc-title-left obj)
          (with-clipping (left top width height)
            (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-INVERSE-ALPHA+)
              (al:draw-text fnt (if down cd cu) x (text-calc-title-top obj) f (title obj)))))
      
        ;; Hilight if needed
        (when in
          (assert (not (eql ch nil)))
          (let ((calc-left (+ left 2))
                (calc-top (+ top 2))
                (calc-right (- (right obj) 1.5))
                (calc-bottom (- (bottom obj) 1.5)))
            (with-borders (bl bt br bb) obj
              (unless (eql bl nil)
                (incf calc-left (thickness bl))
                (incf calc-left (padding-left obj)))
              (unless (eql bt nil)
                (incf calc-top (thickness bt))
                (incf calc-top (padding-top obj)))
              (unless (eql br nil)
                (decf calc-right (thickness br))
                (decf calc-right (padding-right obj)))
              (unless (eql bb nil)
                (decf calc-bottom (thickness bb))
                (decf calc-bottom (padding-bottom obj))))
            (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
              (al:draw-rectangle calc-left calc-top calc-right calc-bottom ch 1))))

        )))
  
  (my-next-method))

