(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; BOX ======================================================================

(deftype box-title-position-type () '(member :left-top :left-middle :left-bottom
                                      :center-top :center-middle :center-bottom
                                      :right-top :right-middle :right-bottom nil))

(defclass box (area-mixin
               parent-mixin
               theme-mixin
               thickness-mixin
               title-mixin
               v-align-mixin)
  ((thickness :initform 1 :documentation "Number of pixels wide for the box frame.")
   (filled :initarg :filled :initform nil :accessor filled :documentation "When T, fill interior of box.")
   (title-position :type box-title-position-type :initarg :title-position :initform nil :accessor title-position
                   :documentation "Where to position the TITLE.

V-ALIGN adjusts relative vertical position of TITLE, as well as final location
of box line:

    TOP    = Above (top) or below (bottom) box line.
    BOTTOM = Below (top) or above (bottom) box line.
    MIDDLE = Replaces part of the box line.
")))

(defmacro defbox (&rest rest &key &allow-other-keys)
  `(make-instance 'box ,@rest))

;;; methods ---------------------------------------------------------

(defmethod calc-height (type area (object box))
  (slot-value area 'height))

(defmethod calc-left (type area width height (object box))
  (slot-value area 'left))

(defmethod calc-top (type area width height (object box))
  (slot-value area 'top))

(defmethod calc-width (type area (object box))
  (slot-value area 'width))

(defmethod initialize-instance :before ((object box) &key)
  (unless (theme-value-defaultp 'box nil 'frame-color)
    (let ((frame-color (get-theme-value-default nil nil 'frame-color)))
      (set-theme-value-default 'box nil 'frame-color frame-color)
      (set-theme-value-default 'box 'flat 'frame-color frame-color))
    (let ((interior-color (get-theme-value-default nil nil 'interior-color)))
      (set-theme-value-default 'box nil 'interior-color interior-color)
      (set-theme-value-default 'box 'flat 'interior-color interior-color))
    (let ((text-color (get-theme-value-default nil nil 'text-color)))
      (set-theme-value-default 'box nil 'text-color text-color))
    (let ((text-font (get-theme-value-default nil nil 'text-font)))
      (set-theme-value-default 'box nil 'text-font text-font))
    (let ((color (get-theme-value-default nil nil 'color))
          (dark-color (get-theme-value-default nil nil 'dark-color))
          (light-color (get-theme-value-default nil nil 'light-color))
          (very-dark-color (get-theme-value-default nil nil 'very-dark-color))
          (very-light-color (get-theme-value-default nil nil 'very-light-color)))
      (dolist (style '(:3d-in :3d-out :3d-flat))
        (set-theme-value-default 'box style 'color color)
        (set-theme-value-default 'box style 'dark-color dark-color)
        (set-theme-value-default 'box style 'light-color light-color)
        (set-theme-value-default 'box style 'very-dark-color very-dark-color)
        (set-theme-value-default 'box style 'very-light-color very-light-color)))))

(defmethod initialize-instance :after ((object box) &key)
  (validate-box-options object))

(defmethod on-paint ((object box) &key)
  (paint-box object))

(defmethod paint-box ((object box))
  (let ((style (get-theme-style object)))
    (when (filled object)
      (paint-box-interior style object))
    (paint-box-frame style object)
    (unless (= (length (title object)) 0)
      (paint-box-title style object))))

(defmethod paint-box-frame ((style (eql :3d-flat)) (object box))
  (paint-box-frame-3d style object))

(defmethod paint-box-frame ((style (eql :3d-in)) (object box))
  (paint-box-frame-3d style object))

(defmethod paint-box-frame ((style (eql :3d-out)) (object box))
  (paint-box-frame-3d style object))

(defmethod paint-box-frame ((style (eql :flat)) (object box))
  (let ((font (get-theme-value object 'box 'text-font :style nil))
        (frame-color (get-theme-value object 'box 'frame-color :style style)))
    (with-local-accessors ((ol left) (ot top) (ow width) (oh height)
                           title thickness title-position v-align)
                          object
      (let ((or (+ ol ow))
            (ob (+ ot oh))
            (title-height (al:get-font-line-height font)) ; title height
            (title-width (al:get-text-width font title))  ; title width
            (tn2 (/ thickness 2))                         ; half thickness
            cb ct)
        (setq ct
              (if (or (= (length title) 0) (eql title-position nil))
                  ot
                  (case title-position
                    ((:center-bottom :left-bottom :right-bottom) ot)
                    ((:center-middle :left-middle :right-middle) ot)
                    ((:center-top :left-top :right-top)
                     (case v-align
                       (:bottom
                        ot)
                       (:middle
                        (+ ot (/ (+ thickness title-height) 2)))
                       (:top
                        (+ ot (/ thickness 2) title-height)))))))
        
        (setq cb
              (if (or (= (length title) 0) (eql title-position nil))
                  ob
                  (case title-position
                    ((:center-bottom :left-bottom :right-bottom)
                     (case v-align
                       (:bottom
                        (- ob title-height thickness))
                       (:middle
                        (- ob (/ thickness 2) (/ title-height 2)))
                       (:top
                        ob)))
                    ((:center-middle :left-middle :right-middle) ob)
                    ((:center-top :left-top :right-top) ob))))
        
        ;; Draw frame
        (when (/= thickness 0)
          (if (eql v-align ':middle)
              (case title-position
                ((:center-top :left-top :right-top)
                 (let (v)
                   (let ((x1 (+ ol tn2))
                         (y1 (+ ct tn2))
                         (x2 (- or tn2))
                         (y2 (- cb tn2)))
                     (let ((x1x1 (- x1 tn2))
                           (x1x2 (+ x1 tn2))
                           (y1y1 (- y1 tn2))
                           (y1y2 (+ y1 tn2))
                           (x2x1 (- x2 tn2))
                           (x2x2 (+ x2 tn2))
                           (y2y1 (- y2 tn2))
                           (y2y2 (+ y2 tn2))
                           x1x3 x1x4)
                       (case title-position
                         (:center-top
                          (let ((amt (/ (- (- x2x1 x1x2) title-width thickness thickness) 2)))
                            (setq x1x3 (+ x1x2 amt))
                            (setq x1x4 (- x2x1 amt))))
                         (:left-top
                          (setq x1x3 (+ x1x2 thickness ))
                          (setq x1x4 (+ x1x2 thickness title-width thickness)))
                         (:right-top
                          (setq x1x3 (- x2x1 thickness title-width thickness))
                          (setq x1x4 (- x2x1 thickness))))

                       ;; After text
                       (push (list x1x4 y1y1 frame-color) v)
                       (push (list x1x4 y1y2 frame-color) v)

                       ;; To the right upper corner
                       (push (list x2x2 y1y1 frame-color) v)
                       (push (list x2x1 y1y2 frame-color) v)
                       
                       ;; To right lower corner
                       (push (list x2x2 y2y2 frame-color) v)
                       (push (list x2x1 y2y1 frame-color) v)

                       ;; To left lower corner
                       (push (list x1x1 y2y2 frame-color) v)
                       (push (list x1x2 y2y1 frame-color) v)

                       ;; To left upper corner
                       (push (list x1x1 y1y1 frame-color) v)
                       (push (list x1x2 y1y2 frame-color) v)

                       ;; Left upper corner to before text
                       (push (list x1x3 y1y1 frame-color) v)
                       (push (list x1x3 y1y2 frame-color) v)))
                   (draw-prim v :triangle-strip)))
                
                ((:center-middle :left-middle :right-middle)
                 (al:draw-rectangle (+ ol tn2) (+ ct tn2) (- or tn2) (- cb tn2) frame-color thickness))
                
                ((:center-bottom :left-bottom :right-bottom)
                 (let (v)
                   (let ((x1 (+ ol tn2))
                         (y1 (+ ct tn2))
                         (x2 (- or tn2))
                         (y2 (- cb tn2)))
                     (let ((x1x1 (- x1 tn2))
                           (x1x2 (+ x1 tn2))
                           (y1y1 (- y1 tn2))
                           (y1y2 (+ y1 tn2))
                           (x2x1 (- x2 tn2))
                           (x2x2 (+ x2 tn2))
                           (y2y1 (- y2 tn2))
                           (y2y2 (+ y2 tn2))
                           x2x3 x2x4)
                       (case title-position
                         (:center-bottom
                          (let ((amt (/ (- (- x2x1 x1x2) title-width thickness thickness) 2)))
                            (setq x2x3 (- x2x1 amt))
                            (setq x2x4 (+ x1x2 amt))))
                         (:left-bottom
                          (setq x2x3 (+ x1x2 thickness title-width thickness))
                          (setq x2x4 (+ x1x2 thickness)))
                         (:right-bottom
                          (setq x2x3 (- x2x1 thickness))
                          (setq x2x4 (- x2x1 thickness title-width thickness))))

                       ;; Lower left after text
                       (push (list x2x4 y2y2 frame-color) v)
                       (push (list x2x4 y2y1 frame-color) v)
                       
                       ;; To left lower corner
                       (push (list x1x1 y2y2 frame-color) v)
                       (push (list x1x2 y2y1 frame-color) v)

                       ;; To left upper corner
                       (push (list x1x1 y1y1 frame-color) v)
                       (push (list x1x2 y1y2 frame-color) v)

                       ;; To right upper corner
                       (push (list x2x2 y1y1 frame-color) v)
                       (push (list x2x1 y1y2 frame-color) v)
                       
                       ;; To right lower corner
                       (push (list x2x2 y2y2 frame-color) v)
                       (push (list x2x1 y2y1 frame-color) v)
                       
                       ;; To right lower before text
                       (push (list x2x3 y2y2 frame-color) v)
                       (push (list x2x3 y2y1 frame-color) v)))
                   (draw-prim v :triangle-strip))))

              ;; Else draw normal rectangle
              (al:draw-rectangle (+ ol tn2) (+ ct tn2) (- or tn2) (- cb tn2) frame-color thickness)))))))

(defun paint-box-frame-3d (style object)
  (let ((font (get-theme-value object 'box 'text-font :style nil)))
    (with-local-accessors ((object-left left) (object-top top)
                           thickness title title-position v-align)
                         object

      (let ((1/4-thick (/ thickness 4))
            (1/2-thick (/ thickness 2))
            (object-right (right object))
            (object-bottom (bottom object))
            (title-height (al:get-font-line-height font))
            (title-width (al:get-text-width font title))
            (title-options (list 0 0 nil)))

        ;; Adjust top for title/no-title
        (unless (or (= (length title) 0) (eql title-position nil))
          (case title-position
            ((:center-bottom :left-bottom :right-bottom))
            ((:center-middle :left-middle :right-middle))
            ((:center-top :left-top :right-top)
             (case v-align
               (:bottom)
               (:middle
                (incf object-top (/ (+ thickness title-height) 2)))
               (:top
                (incf object-top (+ 1/2-thick title-height)))))))

        ;; Adjust bottom for title/no-title
        (unless (or (= (length title) 0) (eql title-position nil))
          (case title-position
            ((:center-bottom :left-bottom :right-bottom)
             (case v-align
               (:bottom
                (decf object-bottom (+ title-height thickness)))
               (:middle
                (decf object-bottom (+ 1/2-thick (/ title-height 2))))
               (:top)))
            ((:center-middle :left-middle :right-middle))
            ((:center-top :left-top :right-top))))

        ;; Draw the box frame
        (let ((3/4-thick (+ 1/2-thick 1/4-thick))
              (2-thick (* 2 thickness))
              (3-thick (* 3 thickness)))
          
          ;; Make space for title if needed
          (when (eql v-align :middle)
            (case title-position
              (:center-top
               (setf (third title-options) t)
               (let ((amt (/ (- (- object-right object-left) (+ 2-thick title-width)) 2)))
                 (setf (first title-options) (+ object-left 1/4-thick amt)
                       (second title-options) (- (+ object-right 1/4-thick) amt))))
          
              (:left-top
               (setf (third title-options) t)
               (setf (first title-options) (+ object-left 2-thick)
                     (second title-options) (+ object-left 3-thick title-width)))
          
              (:right-top
               (setf (third title-options) t)
               (setf (first title-options) (- (+ object-right 1/4-thick) 3-thick title-width)
                     (second title-options) (- (+ object-right 1/4-thick) 2-thick)))

              (:center-bottom
               (let ((amt (/ (- (- object-right object-left) 2-thick title-width) 2)))
                 (setf (first title-options) (- object-right amt))
                 (setf (second title-options) (+ object-left amt))))
          
              (:left-bottom
               (setf (first title-options) (+ object-left 3-thick title-width))
               (setf (second title-options) (+ object-left 2-thick)))
          
              (:right-bottom
               (setf (first title-options) (- object-right 2-thick))
               (setf (second title-options) (- object-right 3-thick title-width)))))

          (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors (type-of object) style object)
            (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
              (draw-side :left (+ object-left 1/4-thick) (+ object-top 1/4-thick)
                         (+ object-right 1/4-thick) (- object-bottom 1/4-thick)
                         1/2-thick lto :others '(:top :right :bottom) :title title-options)
              (draw-side :left (+ object-left 3/4-thick) (+ object-top 3/4-thick)
                         (+ object-right 3/4-thick) (- object-bottom 3/4-thick)
                         1/2-thick lti :others '(:top :right :bottom) :inside t :title title-options)
             
              (draw-side :top (+ object-left 1/4-thick) (+ object-top 1/4-thick)
                         object-right (+ object-bottom 1/4-thick)
                         1/2-thick lto :others '(:left :right :bottom) :title title-options)
              (draw-side :top (+ object-left 3/4-thick) (+ object-top 3/4-thick)
                         object-right (+ object-bottom 3/4-thick)
                         1/2-thick lti :others '(:left :right :bottom) :inside t :title title-options)
             
              (draw-side :right (- object-left 1/4-thick) (+ object-top 3/4-thick)
                         (- object-right 1/4-thick) (- object-bottom 1/4-thick)
                         1/2-thick rbo :others '(:left :top :bottom) :title title-options)
              (draw-side :right (- object-left 3/4-thick) (+ object-top 3/4-thick)
                         (- object-right 3/4-thick) (- object-bottom 3/4-thick)
                         1/2-thick rbi :others '(:left :top :bottom) :inside t :title title-options)
             
              (draw-side :bottom (+ object-left 3/4-thick) (- object-top 1/4-thick)
                         (- object-right 3/4-thick) (- object-bottom 1/4-thick)
                         1/2-thick rbo :others '(:left :top :right) :title title-options)
              (draw-side :bottom (+ object-left 3/4-thick) (- object-top 3/4-thick)
                         (- object-right 3/4-thick) (- object-bottom 3/4-thick)
                         1/2-thick rbi :others '(:left :top :right) :inside t :title title-options))))))))

(defmethod paint-box-interior ((style (eql :3d-flat)) (object box))
  (paint-box-interior-3d object))

(defmethod paint-box-interior ((style (eql :3d-in)) (object box))
  (paint-box-interior-3d object))

(defmethod paint-box-interior ((style (eql :3d-out)) (object box))
  (paint-box-interior-3d object))

(defmethod paint-box-interior ((style (eql :flat)) (object box))
  (with-local-accessors ((ol left) (ot top) (ow width) (oh height) (tn thickness)) object
    (let ((or (+ ol ow))
          (ob (+ ot oh)))
      (let ((ic (get-theme-value object 'box 'interior-color :style style)))
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
          (al:draw-filled-rectangle (2- (+ ol tn)) (2- (+ ot tn)) (1+ (- or tn)) (1+ (- ob tn)) ic))))))

(defun paint-box-interior-3d (object)
  (with-local-accessors ((ol left) (ot top) (ow width) (oh height) (tn thickness)) object
    (let ((or (+ ol ow))
          (ob (+ ot oh)))
      (let ((ic (get-theme-value object 'box 'interior-color :style nil)))
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
          (al:draw-filled-rectangle (2- (+ ol tn)) (2- (+ ot tn)) (1+ (- or tn)) (1+ (- ob tn)) ic))))))

(defmethod paint-box-title ((style (eql :3d-flat)) (object box))
  (paint-box-title-3d object))

(defmethod paint-box-title ((style (eql :3d-in)) (object box))
  (paint-box-title-3d object))

(defmethod paint-box-title ((style (eql :3d-out)) (object box))
  (paint-box-title-3d object))

(defmethod paint-box-title ((style (eql :flat)) (object box))
  (let ((fore-color (get-theme-value object 'box 'text-color :style nil))
        (font (get-theme-value object 'box 'text-font :style nil)))
    (with-local-accessors (title title-position v-align thickness
                           (ol left) (ot top) (ow width) (oh height))
                          object
      (let ((title-height (al:get-font-line-height font))
            (title-width (al:get-text-width font title)))
        (let ((calc-left)
              (calc-top)
              (flags +ALIGN-CENTER+))
          
          (setq calc-left
                (case title-position
                  ((:center-bottom :center-middle :center-top)
                   (ecase v-align
                     (:bottom
                      (+ ol (/ ow 2)))
                     (:middle
                      (+ ol (/ ow 2)))
                     (:top
                      (+ ol (/ ow 2)))))
                  (:left-bottom
                   (ecase v-align
                     (:bottom
                      (+ ol thickness))
                     (:middle
                      (+ ol thickness (/ thickness 2) (/ (+ thickness title-width thickness) 2)))
                     (:top
                      (+ ol thickness))))
                  (:left-middle
                   (ecase v-align
                     (:bottom
                      (+ ol thickness))
                     (:middle
                      (+ ol thickness))
                     (:top
                      (+ ol thickness))))
                  (:left-top
                   (ecase v-align
                     (:bottom
                      (+ ol thickness))
                     (:middle
                      (+ ol thickness (/ thickness 2) (/ (+ thickness title-width thickness) 2)))
                     (:top
                      (+ ol thickness))))
                  (:right-bottom
                   (ecase v-align
                     (:bottom
                      (- (+ ol ow) thickness))
                     (:middle
                      (- (+ ol ow) thickness (/ thickness 2) (/ (+ title-width thickness thickness) 2)))
                     (:top
                      (- (+ ol ow) thickness))))
                  (:right-middle
                   (ecase v-align
                     (:bottom
                      (- (+ ol ow) thickness))
                     (:middle
                      (- (+ ol ow) thickness))
                     (:top
                      (- (+ ol ow) thickness))))
                  (:right-top
                   (ecase v-align
                     (:bottom
                      (- (+ ol ow) thickness))
                     (:middle
                      (- (+ ol ow) thickness (/ thickness 2) (/ (+ title-width thickness thickness) 2)))
                     (:top
                      (- (+ ol ow) thickness))))))
          (assert (not (eql calc-left nil)))

          (setq calc-top
                (case title-position
                  ((:center-bottom :left-bottom :right-bottom)
                   (if (eql v-align nil)
                       ot
                       (case v-align
                         (:bottom
                          (- (+ ot oh) thickness title-height))
                         (:middle
                          (- (+ ot oh) thickness title-height))
                         (:top
                          (- (+ ot oh) thickness title-height)))))
                  ((:center-middle :left-middle :right-middle)
                   (if (eql v-align nil)
                       (- (+ ot (/ oh 2)))
                       (case v-align
                         (:bottom
                          (+ (- (+ ot (/ oh 2) (/ title-height 2)) title-height) (/ title-height 2)))
                         (:middle
                          (- (+ ot (/ oh 2) (/ title-height 2)) title-height))
                         (:top
                          (- (+ ot (/ oh 2) (/ title-height 2)) title-height (/ title-height 2))))))
                  ((:center-top :left-top :right-top)
                   (if (eql v-align nil)
                       ot
                       (case v-align
                         (:bottom
                          (+ ot thickness))
                         (:middle
                          (+ ot (/ thickness 2)))
                         (:top
                          ot))))))
          (assert (not (eql calc-top nil)))
          
          (case title-position
            (:left-bottom
             (case v-align
               (:bottom
                (setq flags +ALIGN-LEFT+))
               (:middle)
               (:top
                (setq flags +ALIGN-LEFT+))))
            (:left-middle
             (setq flags +ALIGN-LEFT+))
            (:left-top
             (case v-align
               (:bottom
                (setq flags +ALIGN-LEFT+))
               (:middle)
               (:top
                (setq flags +ALIGN-LEFT+))))
            (:right-bottom
             (case v-align
               (:bottom
                (setq flags +ALIGN-RIGHT+))
               (:middle)
               (:top
                (setq flags +ALIGN-RIGHT+))))
            (:right-middle
             (setq flags +ALIGN-RIGHT+))
            (:right-top
             (case v-align
               (:bottom
                (setq flags +ALIGN-RIGHT+))
               (:middle)
               (:top
                (setq flags +ALIGN-RIGHT+)))))
          
          ;; Draw the text
          (with-clipping (ol ot ow oh)
            (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-INVERSE-ALPHA+)
              (al:draw-text font fore-color calc-left calc-top flags title)
              )))))))

(defun paint-box-title-3d (object)
  (let ((color (get-theme-value object 'box 'text-color :style nil))
        (font (get-theme-value object 'box 'text-font :style nil)))
    (with-local-accessors (title title-position v-align thickness
                           (ol left) (ot top) (ow width) (oh height))
                          object
      (let ((title-height (al:get-font-line-height font))
            (title-width (al:get-text-width font title)))
        (let ((calc-left)
              (calc-top)
              (flags +ALIGN-CENTER+))
          
          (setq calc-left
                (case title-position
                  ((:center-bottom :center-middle :center-top)
                   (ecase v-align
                     (:bottom
                      (+ ol (/ ow 2)))
                     (:middle
                      (+ ol (/ ow 2)))
                     (:top
                      (+ ol (/ ow 2)))))
                  (:left-bottom
                   (ecase v-align
                     (:bottom
                      (+ ol thickness))
                     (:middle
                      (+ ol thickness (/ thickness 2) (/ (+ thickness title-width thickness) 2)))
                     (:top
                      (+ ol thickness))))
                  (:left-middle
                   (ecase v-align
                     (:bottom
                      (+ ol thickness))
                     (:middle
                      (+ ol thickness))
                     (:top
                      (+ ol thickness))))
                  (:left-top
                   (ecase v-align
                     (:bottom
                      (+ ol thickness))
                     (:middle
                      (+ ol thickness (/ thickness 2) (/ (+ thickness title-width thickness) 2)))
                     (:top
                      (+ ol thickness))))
                  (:right-bottom
                   (ecase v-align
                     (:bottom
                      (- (+ ol ow) thickness))
                     (:middle
                      (- (+ ol ow) thickness (/ thickness 2) (/ (+ title-width thickness thickness) 2)))
                     (:top
                      (- (+ ol ow) thickness))))
                  (:right-middle
                   (ecase v-align
                     (:bottom
                      (- (+ ol ow) thickness))
                     (:middle
                      (- (+ ol ow) thickness))
                     (:top
                      (- (+ ol ow) thickness))))
                  (:right-top
                   (ecase v-align
                     (:bottom
                      (- (+ ol ow) thickness))
                     (:middle
                      (- (+ ol ow) thickness (/ thickness 2) (/ (+ title-width thickness thickness) 2)))
                     (:top
                      (- (+ ol ow) thickness))))))
          (assert (not (eql calc-left nil)))

          (setq calc-top
                (case title-position
                  ((:center-bottom :left-bottom :right-bottom)
                   (if (eql v-align nil)
                       ot
                       (case v-align
                         (:bottom
                          (- (+ ot oh) thickness title-height))
                         (:middle
                          (- (+ ot oh) thickness title-height))
                         (:top
                          (- (+ ot oh) thickness title-height)))))
                  ((:center-middle :left-middle :right-middle)
                   (if (eql v-align nil)
                       (- (+ ot (/ oh 2)))
                       (case v-align
                         (:bottom
                          (+ (- (+ ot (/ oh 2) (/ title-height 2)) title-height) (/ title-height 2)))
                         (:middle
                          (- (+ ot (/ oh 2) (/ title-height 2)) title-height))
                         (:top
                          (- (+ ot (/ oh 2) (/ title-height 2)) title-height (/ title-height 2))))))
                  ((:center-top :left-top :right-top)
                   (if (eql v-align nil)
                       ot
                       (case v-align
                         (:bottom
                          (+ ot thickness))
                         (:middle
                          (+ ot (/ thickness 2)))
                         (:top
                          ot))))))
          (assert (not (eql calc-top nil)))
          
          (case title-position
            (:left-bottom
             (case v-align
               (:bottom
                (setq flags +ALIGN-LEFT+))
               (:middle)
               (:top
                (setq flags +ALIGN-LEFT+))))
            (:left-middle
             (setq flags +ALIGN-LEFT+))
            (:left-top
             (case v-align
               (:bottom
                (setq flags +ALIGN-LEFT+))
               (:middle)
               (:top
                (setq flags +ALIGN-LEFT+))))
            (:right-bottom
             (case v-align
               (:bottom
                (setq flags +ALIGN-RIGHT+))
               (:middle)
               (:top
                (setq flags +ALIGN-RIGHT+))))
            (:right-middle
             (setq flags +ALIGN-RIGHT+))
            (:right-top
             (case v-align
               (:bottom
                (setq flags +ALIGN-RIGHT+))
               (:middle)
               (:top
                (setq flags +ALIGN-RIGHT+)))))
          
          ;; Draw the text
          (with-clipping (ol ot ow oh)
            (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-INVERSE-ALPHA+)
              (al:draw-text font color calc-left calc-top flags title))))))))

(defmethod (setf title-position) :after (value (object box))
  (validate-box-options object))

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

