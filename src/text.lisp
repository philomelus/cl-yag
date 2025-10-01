(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; FORWARD ==================================================================

(declaim (ftype (function (keyword %rect t) (or float integer ratio)) text-calc-height))
(declaim (ftype (function (keyword %rect coordinate coordinate t) coordinate) text-calc-left))
(declaim (ftype (function (t) (values (or float integer ratio) integer)) text-calc-title-left))
(declaim (ftype (function (t) (or float integer ratio)) text-calc-title-top))
(declaim (ftype (function (keyword %rect coordinate coordinate t) coordinate) text-calc-top))
(declaim (ftype (function (keyword %rect t) (or float integer ratio)) text-calc-width))

;;;; TEXT-BASE ================================================================

(defparameter *text-base-theme-data* `((text-base nil text-color)
                                       (text-base nil text-font)))

;; This exists purely to allow a common base for all text objects.  If that
;; changes, the other logic in this file may need revalidation.
(defclass text-base (area-mixin
                     border-mixin
                     h-align-mixin
                     padding-mixin      ; between title and border
                     parent-mixin
                     spacing-mixin      ; between border and edge
                     title-mixin
                     v-align-mixin)
  ())

;;;; TEXT =====================================================================

(defparameter *text-theme-data* `((text nil interior-color)))

(defclass text (text-base
                theme-mixin)
  ())

(defmacro deftext (&rest rest &key &allow-other-keys)
  `(make-instance 'text ,@rest))

;;; METHODS ---------------------------------------------------------

(defmethod calc-border-let (side (area %rect) (object text))
  (case side
    (:bottom)
    (:left)
    (:right)
    (:top)
    (otherwise
     (error "expected :LEFT, :TOP, :RIGHT, or :BOTTOM, got: ~a" side)))
  (error "not implemented"))

(defmethod calc-border-top (side (area %rect) (object text))
  (case side
    (:bottom)
    (:left)
    (:right)
    (:top)
    (otherwise
     (error "expected :LEFT, :TOP, :RIGHT, or :BOTTOM, got: ~a" side)))
  (error "not implemented"))

(defmethod calc-height (type (area %rect) (object text))
  (v:debug :layout "[calc-height] {text} area: (~d ~d) @ (~d ~d) ~a"
           (width area) (height area) (left area) (top area) (print-raw-object object))
  (let ((rv (text-calc-height type area object)))
    (v:debug :layout "[calc-height] {text} result:~d ~a" rv (print-raw-object object))
    rv))

(defmethod calc-left (type (area %rect) width height (object text))
  (v:debug :layout "[calc-left] {text} area: (~d ~d) @ (~d ~d) ~a" 
           (width area) (height area) (left area) (top area) (print-raw-object object))
  (let ((rv (text-calc-left type area width height object)))
    (v:debug :layout "[calc-left] {text} result:~d ~a" rv (print-raw-object object))
    rv))

(defmethod calc-top (type (area %rect) width height (object text))
  (v:debug :layout "[calc-top] {text} area: (~d ~d) @ (~d ~d) ~a"
           (width area) (height area) (left area) (top area) (print-raw-object object))
  (let ((rv (text-calc-top type area width height object)))
    (v:debug :layout "[calc-top] {text} result:~d ~a" rv (print-raw-object object))
    rv))

(defmethod calc-width (type (area %rect) (object text))
  (let ((rv (text-calc-width type area object))) 
    (v:debug :layout "[calc-width] {text} result:~d ~a" rv (print-raw-object object))
    rv))

(defmethod default-theme-style ((object text))
  nil)

(defmethod default-theme-type ((object text))
  'text)

(defmethod initialize-instance :before ((object text) &key)
  (unless (theme-value-defaultp 'text nil 'text-color)
    (set-theme-value-default 'text nil 'text-color (get-theme-value-default nil nil 'text-color))
    (set-theme-value-default 'text nil 'text-font (get-theme-value-default nil nil 'text-font))
    (set-theme-value-default 'text nil 'interior-color (get-theme-value-default nil nil 'interior-color))))

(defmethod on-paint ((object text) &key)
  (with-theme-let ((ttc (text nil text-color))
                   (ttf (text nil text-font))
                   (tic (text nil interior-color)))
                  object

    (with-local-accessors ((l left) (t_ top) (w width) (h height)) object
      ;; Draw background
      (al:draw-filled-rectangle l t_ (right object) (+ (bottom object) 0.99) tic)

      ;; Draw border
      (paint-border object)
      
      (multiple-value-bind (x f) (text-calc-title-left object)
        (with-clipping (l t_ w h)
          (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-INVERSE-ALPHA+)
            (al:draw-text ttf ttc x (text-calc-title-top object) f (title object))))))))

;;;; FUNCTIONS ================================================================

(defun text-calc-height (type area object)
  (with-local-slots ((rv height)) area
    (case type
      ((:auto :auto-max))               ; nothing to do
      
      (:auto-min
       (with-local-slots (spacing-top spacing-bottom
                          border-top border-bottom
                          padding-top padding-bottom)
                         object
         (setq rv (al:get-font-line-height (get-theme-value object 'text 'text-font)))
         (incf rv spacing-top)
         (incf rv spacing-bottom)
         (unless (eql border-top nil)
           (incf rv (thickness border-top))
           (incf rv padding-top))
         (unless (eql border-bottom nil)
           (incf rv (thickness border-bottom))
           (incf rv padding-bottom)))
       (assert (<= rv (height area)))))
    rv))

(defun text-calc-left (type area width height object)
  "For any widget that only uses a title inside its content, this will calculate
the left coordinate. Handles spacing, borders, and padding if widgets are
derived from appropriate mixins."
  (declare (ignorable height))
  
  (with-local-slots ((rv left)) area
    (ccase type
      ((:left :auto :right))
      
      (:center
       (let ((calc-width width))
         (when (typep object 'spacing-mixin)
           (incf calc-width (spacing-left object))
           (incf calc-width (spacing-right object)))
         (when (typep object 'border-mixin)
           (let ((local-border-left (border-left object))
                 (local-border-right (border-right object))
                 (paddingp (typep object 'padding-mixin)))
             (unless (eql local-border-left nil)
               (incf calc-width (thickness local-border-left))
               (when paddingp
                 (incf calc-width (padding-left object))))
             (unless (eql local-border-right nil)
               (incf calc-width (thickness local-border-right))
               (when paddingp
                 (incf calc-width (padding-right object))))))
         (let ((aw (width area)))
           (assert (>= aw width))
           (incf rv (/ (- aw width) 2))))))
    rv))

(defun text-calc-title-left (object)
  "Calculate left coordinate of title.  Returns left coordinate and text align flag."
  
  (let (flags calc-left)
    (case (h-align object)
      ((:none :left)
       (setq calc-left (left object))
       (incf calc-left (spacing-left object))
       (with-slots (border-left) object
         (unless (eql border-left nil)
           (incf calc-left (thickness border-left))
           (incf calc-left (padding-left object))))
       (setq flags +ALIGN-LEFT+)
       (v:debug :layout "[text-calc-title-left] aligning text left:~d ~a" calc-left (print-raw-object object)))
     
      (:center
       (with-slots (border-left border-right
                    padding-left padding-right
                    spacing-left spacing-right)
           object
         (let ((calc-width (width object)))
           (decf calc-width (+ spacing-left spacing-right))
           (unless (eql border-left nil)
             (decf calc-width (thickness border-left))
             (decf calc-width padding-left))
           (unless (eql border-right nil)
             (decf calc-width (thickness border-right))
             (decf calc-width padding-right))
           ;;(setq x (+ (left object) (/ calc-width 2)))
           (setq calc-left (+ (left object) (/ (width object) 2)))))
       (setq flags +ALIGN-CENTER+)
       (v:debug :layout "[text-calc-title-left] aligning text center:~d ~a" calc-left (print-raw-object object)))
     
      (:right
       (setq calc-left (right object))
       (decf calc-left (spacing-right object))
       (with-slots (border-right) object
         (unless (eql border-right nil)
           (decf calc-left (thickness border-right))
           (decf calc-left (padding-right object))))
       (setq flags +ALIGN-RIGHT+)
       (v:debug :layout "[text-calc-title-left] aligning text right:~d ~a" calc-left (print-raw-object object))))
    
    (values calc-left flags)))

(defun text-calc-title-top (obj)
  "Calculate top coordinate of title.  Returns the coordinate."
  
  (with-local-slots ((at top) (va v-align)) obj
    (with-theme-let ((fnt (text nil text-font))) obj
      (assert (and (not (null fnt))
                   (not (cffi:null-pointer-p fnt))))
      
      ;; When auto-calculated, start at 0 offset from parent
      (if (typep at 'layout-coordinate-option-type)
          (setf at 0))

      (ecase va
        ((:top :none)
         (with-local-slots (spacing-top padding-top border-top) obj
           (incf at spacing-top)
           (unless (eql border-top nil)
             (incf at (thickness border-top))
             (incf at padding-top)))
         (v:debug :layout "[text-calc-title-top] aligning text top:~a ~a" at (print-raw-object obj)))
        
        (:middle
         (let ((h (height obj))
               (fh (al:get-font-line-height fnt)))
           (incf at (truncate (/ (- h fh) 2))))
         (v:debug :layout "[text-calc-title-top] aligning text middle:~a ~a" at (print-raw-object obj)))
        
        (:bottom
         (with-local-slots (spacing-bottom padding-bottom border-bottom height) obj
           (let ((raise (al:get-font-line-height fnt)))
             (incf raise spacing-bottom)
             (unless (eql border-bottom nil)
               (incf raise (thickness border-bottom))
               (incf raise padding-bottom))
             (incf at (- height raise))))
         (v:debug :layout "[text-calc-title-top] aligning text bottom:~a ~a" at (print-raw-object obj))))
      at)))

(defun text-calc-top (type area width height object)
  "For any widget that only uses a title inside its content, this will calculate
the top coordinate. Handles spacing, borders, and padding if widgets are
derived from appropriate mixins."
  (declare (ignorable width))
  
  (with-local-slots ((rv top)) area
    (ecase type
      ((:top :none :auto))              ; already done

      (:middle
       (incf rv (/ (- (height area) height) 2)))

      (:bottom
       (let ((calc-height height))
         (when (typep object 'spacing-mixin)
           (incf calc-height (spacing-top object))
           (incf calc-height (spacing-bottom object)))
         (when (typep object 'border-mixin)
           (let ((local-border-top (border-top object))
                 (local-border-bottom (border-bottom object))
                 (paddingp (typep object 'padding-mixin)))
             (unless (eql local-border-top nil)
               (incf calc-height (thickness local-border-top))
               (when paddingp
                 (incf calc-height (padding-top object))))
             (unless (eql local-border-bottom nil)
               (incf calc-height (thickness local-border-bottom))
               (when paddingp
                 (incf calc-height (padding-bottom object))))))
         (incf rv (- (height area) calc-height)))))
    rv))

(defun text-calc-width (type area object)
  (let (rv)
    (case type
      ((:auto :auto-max)
       (setq rv (width area)))

      (:auto-min
       (with-local-slots (spacing-left spacing-right
                          border-left border-right
                          padding-left padding-right)
                         object
         (setq rv (al:get-text-width (theme-field font object) (title object)))
         (incf rv spacing-left)
         (incf rv spacing-right)
         (unless (eql border-left nil)
           (incf rv (thickness border-left))
           (incf rv padding-left))
         (unless (eql border-right nil)
           (incf rv (thickness border-right))
           (incf rv padding-right)))
       ;; User should know when it doesn't fit
       (assert (<= rv (width area)))))
    
    rv))

