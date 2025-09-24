(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; THEME COMPILE/LOAD ORDER FORCES AFTER THEME ==============================

(defmethod paint-box ((object box) (theme theme-3d))
  (when (filled object)
    (paint-box-interior object theme))
  (paint-box-frame object theme)
  (unless (= (length (title object)) 0)
    (paint-box-title object theme)))

(defmethod paint-box ((object box) (theme theme-flat))
  (when (filled object)
    (paint-box-interior object theme))
  (paint-box-frame object theme)
  (unless (= (length (title object)) 0)
    (paint-box-title object theme)))

(defmethod paint-box-frame ((object box) (theme theme-3d))
  (with-theme (font style) theme
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

          (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors theme)
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

(defmethod paint-box-frame ((object box) (theme theme-flat))
  (with-object-and-theme (font) object theme
    (with-theme (frame-color) theme
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
                (al:draw-rectangle (+ ol tn2) (+ ct tn2) (- or tn2) (- cb tn2) frame-color thickness))))))))

(defmethod paint-box-interior ((object box) (theme theme-3d))
  (with-local-accessors ((ol left) (ot top) (ow width) (oh height) (tn thickness)) object
    (let ((or (+ ol ow))
          (ob (+ ot oh)))
      (with-theme ((ic interior-color)) theme
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
          (al:draw-filled-rectangle (2- (+ ol tn)) (2- (+ ot tn)) (1+ (- or tn)) (1+ (- ob tn)) ic))))))

(defmethod paint-box-interior ((object box) (theme theme-flat))
  (with-local-accessors ((ol left) (ot top) (ow width) (oh height) (tn thickness)) object
    (let ((or (+ ol ow))
          (ob (+ ot oh)))
      (with-theme ((ic interior-color)) theme
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
          (al:draw-filled-rectangle (2- (+ ol tn)) (2- (+ ot tn)) (1+ (- or tn)) (1+ (- ob tn)) ic))))))

(defmethod paint-box-title ((object box) (theme theme-3d))
  (with-object-and-theme (fore-color font) object theme
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
              ))
          )))))

(defmethod paint-box-title ((object box) (theme theme-flat))
  (with-object-and-theme (fore-color font) object theme
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
              ))
          )))))

;;;; THEME-3D-ALL =============================================================

(defclass theme-3d-all (theme-3d
                        border-theme-3d-mixin
                        box-theme-3d-mixin
                        button-theme-3d-mixin
                        grid-theme-mixin
                        ruler-theme-mixin
                        text-theme-mixin
                        window-theme-mixin)
  ())

(defmacro deftheme-3d-all (&rest rest &key &allow-other-keys)
  `(make-instance 'theme-3d-all ,@rest))

;;;; THEME-FLAT-ALL ===========================================================

(defclass theme-flat-all (theme-flat
                          border-theme-flat-mixin
                          box-theme-flat-mixin
                          button-theme-flat-mixin
                          grid-theme-mixin
                          ruler-theme-mixin
                          text-theme-mixin
                          window-theme-mixin)
  ())

(defmacro deftheme-flat-all (&rest rest &key &allow-other-keys)
  `(make-instance 'theme-flat-all ,@rest))

;;;; GLOBAL THEMES ============================================================

;;; THEME-3D-ALL ----------------------------------------------------

(defmacro deftheme-3d-obj (normal dark light very-dark very-light
                           &optional (fore-color '(al:map-rgb-f 1.0 1.0 1.0)) (back-color '(al:map-rgb 0 0 0))
                           &body body)
  (let ((object (gensym)))
    `(funcall (lambda (n d l vd vl fc bc)
                (let ((,object (make-instance 'theme-3d-all :normal n :dark d :light l :very-dark vd :very-light vl :fore-color fc :back-color bc)))
                  
                  ;; border-theme-3d-mixin covered by theme-3d
                  ;; box-theme-3d-mixin
                  ;; button-theme-3d-mixin
                  (setf (font ,object) (default-font))
                  (setf (down-color ,object) vd)
                  (setf (hover-color ,object) d)
                  (setf (interior-color ,object) n)
                  (setf (up-color ,object) vl)
                  (setf (border-thickness ,object) 1)
                  (setf (borderp ,object) t)
                  (setf (hover-thickness ,object) 1)
                  (setf (hover-inside-paddingp ,object) nil)
                  (setf (hover-inside-borderp ,object) nil)
                  (setf (style ,object) :default)
                  ;; grid-theme-mixn
                  (setf (major-color-h ,object) d)
                  (setf (major-color-v ,object) d)
                  (setf (minor-color-h ,object) vd)
                  (setf (minor-color-v ,object) vd)
                  ;; ruler-theme-mixin
                  (setf (division-color ,object) l)
                  (setf (line-color ,object) l)
                  ;; text-theme-mixin covered by active-text-theme-mixin
                  ;; window-theme-mixin converted by active-text-theme-mixin

                  ;; Custom initialization code
                  (let ((object ,object))
                    ,@body
                    (setf ,object object))

                  ;; Make sure all slots get set
                  ;; (dolist (slot (class-slots (find-class 'theme-3d-all)))
                  ;;   (when (eql (slot-value ,object (slot-definition-name slot)) nil)
                  ;;     (error "Missing theme-3d-all setting: ~a" (slot-definition-name slot))))
                  
                  ,object))
              ,normal ,dark ,light ,very-dark ,very-light, fore-color, back-color)))

(defun theme-3d-aqua ()
  (deftheme-3d-obj (al:map-rgb 0 255 255) (al:map-rgb 0 191 191) (al:map-rgb 127 255 255) (al:map-rgb 0 127 127) (al:map-rgb 191 255 255)))

(defun theme-3d-blue ()
  (deftheme-3d-obj (al:map-rgb 0 0 255) (al:map-rgb 0 0 191) (al:map-rgb 159 159 255) (al:map-rgb 0 0 159) (al:map-rgb 191 191 255)))

(defun theme-3d-gray ()
  (deftheme-3d-obj
      (al:map-rgb 212 208 200) (al:map-rgb 128 128 128) (al:map-rgb 223 223 223) (al:map-rgb 95 95 95) (al:map-rgb 245 245 245) (al:map-rgb 0 0 0) (al:map-rgb 255 255 255)
    ;; Override the active-text up/down for this one
    (setf (up-color object) (fore-color object))
    (setf (down-color object) (dark-color object))
    ))

(defun theme-3d-green ()
  (deftheme-3d-obj (al:map-rgb 0 255 0) (al:map-rgb 0 191 0) (al:map-rgb 127 255 127) (al:map-rgb 0 159 0) (al:map-rgb 191 255 191)))

(defun theme-3d-purple ()
  (deftheme-3d-obj (al:map-rgb 255 0 255) (al:map-rgb 191 0 191) (al:map-rgb 255 159 255) (al:map-rgb 159 0 159) (al:map-rgb 255 191 255)))

(defun theme-3d-red ()
  (deftheme-3d-obj (al:map-rgb 255 0 0) (al:map-rgb 191 0 0) (al:map-rgb 255 127 127) (al:map-rgb 159 0 0) (al:map-rgb 255 191 191)))

(defun theme-3d-yellow ()
  (deftheme-3d-obj (al:map-rgb 255 255 0) (al:map-rgb 191 191 0) (al:map-rgb 255 255 127) (al:map-rgb 127 127 0) (al:map-rgb 255 255 191)))

;;; THEME-FLAT-ALL --------------------------------------------------

(defmacro deftheme-flat-all-obj (frame interior
                                 &optional (fore '(al:map-rgb 255 255 255)) (back '(al:map-rgb-f 0.0 0.0 0.0))
                                 &body body)
  (let ((object (gensym)))
    `(funcall (lambda (f i fc bc)
                (let ((,object (make-instance 'theme-flat-all :fore-color fc :back-color bc :frame-color f :interior-color i)))

                  ;; border-theme-3d-mixin
                  ;; border-theme-flat-mixin
                  (setf (color ,object) f)
                  ;; box-theme-3d-mixin
                  ;; box-theme-flat-mixin
                  (setf (font ,object) (default-font))
                  ;; button-theme-3d-mixin
                  ;; button-theme-flat-mixin
                  (setf (down-color ,object) f)
                  (setf (hover-color ,object) f)
                  (setf (interior-color ,object) i)
                  (setf (up-color ,object) i)
                  (setf (border-thickness ,object) 1)
                  (setf (borderp ,object) t)
                  (setf (hover-thickness ,object) 1)
                  (setf (hover-inside-paddingp ,object) nil)
                  (setf (hover-inside-borderp ,object) nil)
                  ;; grid-theme-mixin
                  (setf (major-color-h ,object) f)
                  (setf (major-color-v ,object) f)
                  (setf (minor-color-h ,object) f)
                  (setf (minor-color-v ,object) f)
                  ;; ruler-theme-mixin
                  (setf (division-color ,object) f)
                  (setf (line-color ,object) f)
                  ;; text-theme-mixin covered by theme-flat
                  ;; window-theme-mixin covered by theme-flat

                  ;; Custom initialization code
                  (let ((object ,object))
                    ,@body
                    (setf ,object object))
                  
                  ;; Make sure all slots get set
                  ;; (dolist (slot (class-slots (find-class 'theme-flat-all)))
                  ;;   (when (eql (slot-value ,object (slot-definition-name slot)) nil)
                  ;;     (error "Missing theme-flat-all setting: ~a" (slot-definition-name slot))))
                  ,object))
              ,frame ,interior ,fore ,back)))

(defun theme-flat-aqua ()
  (deftheme-flat-all-obj (al:map-rgb 0 191 191) (al:map-rgb 0 255 255)))

(defun theme-flat-blue ()
  (deftheme-flat-all-obj (al:map-rgb 0 0 191) (al:map-rgb 0 0 255)))

(defun theme-flat-gray ()
  (deftheme-flat-all-obj (al:map-rgb-f 0.5 0.5 0.5) (al:map-rgb 212 208 200) (al:map-rgb-f 0.0 0.0 0.0) (al:map-rgb-f 1.0 1.0 1.0)
    ;; Override the up color
    (setf (up-color object) (fore-color object))
    ))

(defun theme-flat-green ()
  (deftheme-flat-all-obj (al:map-rgb 0 191 0) (al:map-rgb 0 255 0)))

(defun theme-flat-purple ()
  (deftheme-flat-all-obj (al:map-rgb 191 0 191) (al:map-rgb 255 0 255)))

(defun theme-flat-red ()
  (deftheme-flat-all-obj (al:map-rgb 191 0 0) (al:map-rgb 255 0 0)))

(defun theme-flat-yellow ()
  (deftheme-flat-all-obj (al:map-rgb 191 191 0) (al:map-rgb 255 255 0)))

;;;------------------------------------------------------------------

(defparameter *theme-default* (theme-flat-gray))

