(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; box ======================================================================

;;; theme-mixin -----------------------------------------------------

(defclass box-theme-mixin (font-mixin
                           fore-color-mixin
                           interior-color-mixin)
  ())

(defclass box-theme-3d-mixin (color-3d-mixin
                              style-3d-mixin)
  ())

(defclass box-theme-flat-mixin (frame-color-mixin)
  ())

;;; box ---- --------------------------------------------------------

(deftype box-title-position-type () '(member :left-top :left-middle :left-bottom
                                      :center-top :center-middle :center-bottom
                                      :right-top :right-middle :right-bottom nil))

(defclass box (box-theme-mixin
               area-mixin
               parent-mixin
               theme-mixin
               title-mixin
               v-align-mixin)
  ((thickness :initarg :thickness :initform 1 :accessor thickness :documentation "Number of pixels wide for the box frame.")
   (filled :initarg :filled :initform nil :accessor filled :documentation "When T, fill interior of box.")
   (title-position :type box-title-position-type :initarg :title-position :initform nil :accessor title-position)))

(defmacro defbox (&rest rest &key &allow-other-keys)
  `(make-instance 'box ,@rest))

;;; methods ---------------------------------------------------------

(defmethod initialize-instance :after ((object box) &key)
  (validate-box-options object))

(defmethod (setf title-position) :after (value (object box))
  (validate-box-options object))

(defmethod calc-height (type area (object box))
  (slot-value area 'height))

(defmethod calc-left (type area width height (object box))
  (slot-value area 'left))

(defmethod calc-top (type area width height (object box))
  (slot-value area 'top))

(defmethod calc-width (type area (object box))
  (slot-value area 'width))

(defmethod on-paint ((object box) &key)
  (paint-box object (find-theme object)))

;;; functions -------------------------------------------------------

(defun draw-side (side left top right bottom width color &key (others nil) (inside nil) (title nil titlep))
  "Draw a box side in 3d.

OTHERS can provide a list of other sides that will also eventually be drawn,
in which case the correct blending in the corners will be done. Without
OTHERS, sides will fully drawn edge to edge.

INSIDE should be T when draing a side for the interior portion of 3d.

TITLE can contain a triplet of (BEGIN, END, TOPP). BEGIN is the starting
horizontal coordinate. END is the ending horizontal coordinate. TOPP is T for
leaving the title blank at top, or NIL for bottom."

  (assert (not (eql color nil)))
  
  (let ((half-width (/ width 2))
        (side-bottom-extra 0)
        (side-left-extra 0)
        (side-right-extra 0)
        (side-top-extra 0)
        (title-start 0)
        (title-end 0)
        (title-top nil)
        side-left side-top side-right side-bottom)
    (declare (ignorable side-bottom-extra side-left-extra
                        side-right-extra side-top-extra
                        title-end title-top))
    (case side
      (:bottom
       (setq side-left (min left right)
             side-top (max top bottom)
             side-right (max left right)
             side-bottom (max top bottom))
       
       ;; Leave space for title?
       (when titlep
         (unless (third title)
           (setq title-start (first title)
                 title-end (second title)
                 title-top (third title))))
       
       ;; Other sides being drawn too?
       (unless (eql others nil)
         (when (member :left others)
           (if inside
               (setq side-left-extra (- width))
               (setq side-left-extra 0)))
         (when (member :right others)
           (if inside
               (setq side-right-extra (- width))
               (setq side-right-extra 0)))))
      
      (:left
       (setq side-left (min left right)
             side-top (min top bottom)
             side-right (min left right)
             side-bottom (max top bottom))
       
       ;; Other sides being drawn too?
       (unless (eql others nil)
         (when (member :top others)
           (if inside
               (setq side-top-extra 0)
               (setq side-top-extra 0)))
         (when (member :bottom others)
           (if inside
               (setq side-bottom-extra 0)
               (setq side-bottom-extra 0)))))
      
      (:right
       (setq side-left (max left right)
             side-top (min top bottom)
             side-right (max left right)
             side-bottom (max top bottom))
       ;; Other sides being drawn too?
       (unless (eql others nil)
         (when (member :top others)
           (if inside
               (setq side-top-extra (- width))
               (setq side-top-extra 0))
           )))
      
      (:top
       (setq side-left (min left right)
             side-top (min top bottom)
             side-right (max left right)
             side-bottom (min top bottom))
       
       ;; Leave space for title?
       (when titlep
         (when (third title)
           (setq title-start (first title)
                 title-end (second title)
                 title-top (third title))))
       
       ;; Other sides being drawn too?
       (unless (eql others nil)
         (when (member :left others)
           (if inside
               (setq side-left-extra (- width))
               (setq side-left-extra 0)))
         (when (member :right others)
           (if inside
               (setq side-right-extra (- width))
               (setq side-right-extra (- half-width)))))))
    
    (let (v)
      (let ((x1 (- side-left half-width side-left-extra))
            (y1 (- side-top half-width side-top-extra))
            (x2 (+ side-right half-width side-right-extra))
            (y2 (+ side-bottom half-width side-bottom-extra)))
        (let ()

          (if (and titlep (> title-start 0))
              (progn
                (if (member side (list :top :bottom))
                    (let ((start (min title-start title-end))
                          (end (max title-start title-end)))
                      ;; Start
                      (push (list x1 y1 color) v)
                      (push (list x1 y2 color) v)
                      
                      ;; Start of title
                      (push (list start y1 color) v)
                      (push (list start y2 color) v)
                      (draw-prim v :triangle-strip)
                      
                      ;; End of title
                      (setf v nil)
                      (push (list end y1 color) v)
                      (push (list end y2 color) v)
                      
                      ;; End
                      (push (list x2 y1 color) v)
                      (push (list x2 y2 color) v)
                      (draw-prim v :triangle-strip))
                    
                    (progn
                      ;; Start
                      (push (list x1 y1 color) v)
                      (push (list x2 y1 color) v)

                      ;; End
                      (push (list x1 y2 color) v)
                      (push (list x2 y2 color) v))))
              
              (progn
                ;; Start
                (push (list x1 y1 color) v)
                (push (list x2 y1 color) v)

                ;; End
                (push (list x1 y2 color) v)
                (push (list x2 y2 color) v)))))
      
      (draw-prim v :triangle-strip))))

(defun validate-box-options (object)
  (with-local-slots ((tp title-position) (ti title) (va v-align)) object
    ;; Make sure they gave title-position if title is not empty
    (if (= (length ti) 0)
        (progn
          (unless (eq tp nil)
            (warn "title-position not needed when title empty"))
          (unless (eql va :none)
            (warn "v-align not needed when title empty")))
        (progn
          (when (eql tp nil)
            (error "when using title, title-position is required"))
          (when (eql va :none)
            (error "when using title, v-align is required"))))))

