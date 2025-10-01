(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; THEME COMPILE/LOAD ORDER FORCES AFTER THEME ==============================

(defmethod paint-box ((object box))
  (when (filled object)
    (paint-box-interior object))
  (paint-box-frame object)
  (unless (= (length (title object)) 0)
    (paint-box-title object)))

(defmethod paint-box ((object box))
  (when (filled object)
    (paint-box-interior object))
  (paint-box-frame object)
  (unless (= (length (title object)) 0)
    (paint-box-title object)))

(defmethod paint-box-frame ((object box))
  (with-theme-let (font) object
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

          (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors (type-of object) (style object) object)
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

(defmethod paint-box-frame ((object box))
  (with-theme-let (font frame-color) object
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

(defmethod paint-box-interior ((object box))
  (with-local-accessors ((ol left) (ot top) (ow width) (oh height) (tn thickness)) object
    (let ((or (+ ol ow))
          (ob (+ ot oh)))
      (with-theme-let ((ic interior-color)) object
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
          (al:draw-filled-rectangle (2- (+ ol tn)) (2- (+ ot tn)) (1+ (- or tn)) (1+ (- ob tn)) ic))))))

(defmethod paint-box-interior ((object box))
  (with-local-accessors ((ol left) (ot top) (ow width) (oh height) (tn thickness)) object
    (let ((or (+ ol ow))
          (ob (+ ot oh)))
      (with-theme-let ((ic interior-color)) object
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
          (al:draw-filled-rectangle (2- (+ ol tn)) (2- (+ ot tn)) (1+ (- or tn)) (1+ (- ob tn)) ic))))))

(defmethod paint-box-title ((object box))
  (with-theme-let (fore-color font) object
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

(defmethod paint-box-title ((object box))
  (with-theme-let (fore-color font) object
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

