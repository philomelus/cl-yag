(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; macros ==================================================================

(defmacro with-theme-3d-colors ((n d l vd vl) theme &body body)
  (a:with-gensyms (instance)
    `(let ((,instance ,theme))
       (let ((,n (normal-color ,instance))
             (,d (dark-color ,instance))
             (,l (light-color ,instance))
             (,vd (very-dark-color ,instance))
             (,vl (very-light-color ,instance)))
         (declare (ignorable ,n ,d ,l ,vd ,vl))
         (assert (not (eql ,n nil)))
         (assert (not (eql ,d nil)))
         (assert (not (eql ,l nil)))
         (assert (not (eql ,vd nil)))
         (assert (not (eql ,vl nil)))
         ,@body))))

;;;; forward declaration =====================================================

(defvar *theme-default*)

;;;; theme-base ===============================================================

(defclass theme-base (back-fore-color-mixin)
  ())

(defmethod paint-border ((object area-mixin) (theme theme-base))
  (with-borders (bl br bt bb) object
    (let ((blp (not (eql bl nil)))
          (brp (not (eql br nil)))
          (btp (not (eql bt nil)))
          (bbp (not (eql bb nil))))
      (when blp
        (paint-border-left bl object theme :blend-top btp :blend-bottom bbp))
      (when btp
        (paint-border-top bt object theme :blend-left blp :blend-right brp))
      (when brp
        (paint-border-right br object theme :blend-top btp :blend-bottom bbp))
      (when bbp
        (paint-border-bottom bb object theme :blend-left blp :blend-right brp)))))

;;;; theme-flat ===============================================================

(defclass theme-flat (theme-base)
  ((frame-color :initarg :frame-color :initform nil :accessor frame-color)
   (interior-color :initarg :interior-color :initform nil :accessor interior-color)))

(defmacro deftheme-flat (&rest rest &key &allow-other-keys)
  `(make-instance 'theme-flat ,@rest))

;;; methods ---------------------------------------------------------

;;;; theme-3d =================================================================

(defclass theme-3d (theme-base
                    color-3d-mixin)
  ())

(defmacro deftheme-3d (&rest rest &key &allow-other-keys)
  `(make-instance 'theme-3d ,@rest))

;;;; methods ==================================================================

(macrolet ((theme-read-field (name field)
             `(progn
                (defmethod ,name (object)
                  (let ((th (find-theme object)))
                    (assert (not (eql th nil)))
                    (slot-value th ,field)))
                (defmethod ,name ((object theme-mixin))
                  (with-slots (theme) object
                    (assert (typep theme 'theme-3d))
                    (slot-value theme ,field))))))
  (theme-read-field theme-3d-d 'dark)
  (theme-read-field theme-3d-l 'light)
  (theme-read-field theme-3d-n 'normal)
  (theme-read-field theme-3d-vd 'very-dark)
  (theme-read-field theme-3d-vl 'very-light))

;;;; common methods ===========================================================

(defmethod paint-border-bottom ((border border) (object area-mixin) (theme theme-3d) &key blend-left blend-right)
  (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors theme)
    (declare (ignorable lto lti rbo rbi))
    (with-area-and-spacing (object-left object-top object-right object-bottom) object
      (assert (< object-left object-right))
      (assert (< object-top object-bottom))
      (let ((thickness (thickness border)))
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

(defmethod paint-border-bottom ((border border) object (theme theme-flat) &key blend-left blend-right)
  (declare (ignorable blend-left blend-right))
  (let ((color (color theme)))
    (with-accessors ((tn thickness)) border
      (assert (> tn 0))
      (with-area-and-spacing (asl ast asr asb) object
        (let ((c color)
              (w2 (/ tn 2)))
          (if (eql c nil)
              (setq c (frame-color theme)))
          (let ((yy (+ asb (- tn) w2)))
            (al:draw-line asl yy asr yy c tn)))))))

(defmethod paint-border-left ((border border) object (theme theme-3d) &key blend-top blend-bottom)
  (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors theme)
    (declare (ignorable lto lti rbo rbi))
    (with-area-and-spacing (object-left object-top object-right object-bottom) object
      (let ((thickness (thickness border)))
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
                       1/2-thick lti :others others :inside t)
            ))))))

(defmethod paint-border-left ((border border) object (theme theme-flat) &key blend-top blend-bottom)
  (declare (ignorable blend-top blend-bottom))
  (let ((color (color theme)))
    (with-accessors ((tn thickness)) border
      (assert (> tn 0))
      (with-area-and-spacing (asl ast asr asb) object
        (let ((c color)
              (w tn)
              (w2 (/ tn 2)))
          (if (eql c nil)
              (setq c (frame-color theme)))
          (let ((xx (+ asl w2)))
            (assert (not (eql c nil)))
            (al:draw-line xx ast xx asb c w)))))))

(defmethod paint-border-right ((border border) object (theme theme-3d) &key blend-top blend-bottom)
  (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors theme)
    (declare (ignorable lto lti rbo rbi))
    (with-area-and-spacing (object-left object-top object-right object-bottom) object
      (let ((thickness (thickness border)))
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
                       1/2-thick rbi :others others :inside t)))))))

(defmethod paint-border-right ((border border) object (theme theme-flat) &key blend-top blend-bottom)
  (declare (ignorable blend-top blend-bottom))
  (let ((color (color theme)))
    (with-accessors ((tn thickness)) border
      (assert (> tn 0))
      (with-area-and-spacing (asl ast asr asb) object
        (let ((c color)
              (w2 (/ tn 2)))
          (if (eql c nil)
              (setq c (frame-color theme)))
          (let ((xx (+ asr (- tn) w2)))
            (al:draw-line xx ast xx asb c tn)))))))

(defmethod paint-border-top ((border border) object (theme theme-3d) &key blend-left blend-right)
  (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors theme)
    (declare (ignorable lto lti rbo rbi))
    (with-area-and-spacing (object-left object-top object-right object-bottom) object
      (let ((thickness (thickness border)))
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
                       (+ object-right 0) (+ object-bottom 3/4-thick)
                       1/2-thick lti :others others :inside t)))))))

(defmethod paint-border-top ((border border) object (theme theme-flat) &key blend-left blend-right)
  (declare (ignorable blend-left blend-right))
  (let ((color (color theme)))
    (with-accessors ((tn thickness)) border
      (assert (> tn 0))
      (with-area-and-spacing (asl ast asr asb) object
        (let ((c color)
              (w2 (/ tn 2)))
          (if (eql c nil)
              (setq c (frame-color theme)))
          (let ((yy (+ ast w2)))
            (al:draw-line asl yy asr yy c tn)))))))

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
  ;; (unless (equal object (cl-yag-tests::box-tests-b7 cl-yag-tests::*box-data*))
  ;;   (return-from paint-box-frame))
  (with-theme-3d-colors (normal dark light very-dark very-light) theme
    (with-theme (font style) theme
      (with-local-accessors ((object-left left) (object-top top) thickness)
                            object

        ;; (let ((frame-color (al:map-rgb-f 0 1 0))
        ;;       (object-right (right object))
        ;;       (object-bottom (bottom object))
        ;;       (tn2 (/ thickness 2))
        ;;       v)
        ;;   (let ((x1 (+ object-left tn2))
        ;;         (y1 (+ object-top tn2))
        ;;         (x2 (- object-right tn2))
        ;;         (y2 (- object-bottom tn2)))
        ;;     (let ((x1x1 (- x1 tn2))
        ;;           (x1x2 (+ x1 tn2))
        ;;           (y1y1 (- y1 tn2))
        ;;           (y1y2 (+ y1 tn2))
        ;;           (x2x1 (- x2 tn2))
        ;;           (x2x2 (+ x2 tn2))
        ;;           (y2y1 (- y2 tn2))
        ;;           (y2y2 (+ y2 tn2)))

        ;;       ;; Upper left corner
        ;;       (push (list x1x1 y1y1 frame-color) v)
        ;;       (push (list x1x2 y1y2 frame-color) v)

        ;;       ;; To the right upper corner
        ;;       (push (list x2x2 y1y1 frame-color) v)
        ;;       (push (list x2x1 y1y2 frame-color) v)
                         
        ;;       ;; To right lower corner
        ;;       (push (list x2x2 y2y2 frame-color) v)
        ;;       (push (list x2x1 y2y1 frame-color) v)

        ;;       ;; To left lower corner
        ;;       (push (list x1x1 y2y2 frame-color) v)
        ;;       (push (list x1x2 y2y1 frame-color) v)

        ;;       ;; To left upper corner
        ;;       (push (list x1x1 y1y1 frame-color) v)
        ;;       (push (list x1x2 y1y2 frame-color) v)))
        ;;   (draw-prim v :triangle-strip))
        (let ((quarter-thick (/ thickness 4))
              (object-right (right object))
              (object-bottom (bottom object)))
          (multiple-value-bind (lto lti rbo rbi) (theme-3d-style-colors theme)
            ;; (v:info :box "[paint-box-frame] {theme-3d} (1) lto:~a" (print-color lto))
            ;; (v:info :box "[paint-box-frame] {theme-3d} (2) lti:~a" (print-color lti))
            ;; (v:info :box "[paint-box-frame] {theme-3d} (3) rbo:~a" (print-color rbo))
            ;; (v:info :box "[paint-box-frame] {theme-3d} (4) rbi:~a" (print-color rbi))
            (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
              (draw-side :left (- object-left quarter-thick) (- object-top quarter-thick) (- object-right quarter-thick) (+ object-bottom quarter-thick) (/ thickness 2) lto :others '(:top :right :bottom))
              (draw-side :left (+ object-left quarter-thick) (- object-top quarter-thick) (+ object-right quarter-thick) (+ object-bottom quarter-thick) (/ thickness 2) lti :others '(:top :right :bottom) :inside t)
              (draw-side :top (- object-left quarter-thick) (- object-top quarter-thick) (+ object-right quarter-thick) (- object-bottom quarter-thick) (/ thickness 2) lto :others '(:left :right :bottom))
              (draw-side :top (- object-left quarter-thick) (+ object-top quarter-thick) (+ object-right quarter-thick) (+ object-bottom quarter-thick) (/ thickness 2) lti :others '(:left :right :bottom) :inside t)
              (draw-side :right (+ object-left quarter-thick) (- object-top quarter-thick) (+ object-right quarter-thick) (+ object-bottom quarter-thick) (/ thickness 2) rbo :others '(:left :top :bottom))
              (draw-side :right (- object-left quarter-thick) (- object-top quarter-thick) (- object-right quarter-thick) (+ object-bottom quarter-thick) (/ thickness 2) rbi :others '(:left :top :bottom) :inside t)
              (draw-side :bottom (- object-left quarter-thick) (+ object-top quarter-thick) (+ object-right quarter-thick) (+ object-bottom quarter-thick) (/ thickness 2) rbo :others '(:left :top :right))
              (draw-side :bottom (- object-left quarter-thick) (- object-top quarter-thick) (+ object-right quarter-thick) (- object-bottom quarter-thick) (/ thickness 2) rbi :others '(:left :top :right) :inside t)
              )))
        ))))

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
                              (setq x1x4 (- x2x1 amt)))) ;!!!validated!!!
                           (:left-top
                            (setq x1x3 (+ x1x2 thickness ))
                            (setq x1x4 (+ x1x2 thickness title-width thickness))) ;!!!validated!!!
                           (:right-top
                            (setq x1x3 (- x2x1 thickness title-width thickness))
                            (setq x1x4 (- x2x1 thickness)))) ;!!!validated!!!

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
  )

(defmethod paint-box-interior ((object box) (theme theme-flat))
  (with-local-accessors ((ol left) (ot top) (ow width) (oh height) (tn thickness)) object
    (let ((or (+ ol ow))
          (ob (+ ot oh)))
      (with-theme ((ic interior-color)) theme
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-INVERSE-ALPHA+)
          (al:draw-filled-rectangle (2- (+ ol tn)) (2- (+ ot tn)) (1+ (- or tn)) (1+ (- ob tn)) ic))))))

(defmethod paint-box-title ((object box) (theme theme-3d))
  )

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

;;;; functions ================================================================

(defun theme-3d-style-colors (theme)
  "Return 3d drawing colors from 3d drawing style (left-top-outside
left-top-inside right-bottom-outside right-bottom-inside)."
  
  (with-theme-3d-colors (normal dark light very-dark very-light) theme
    (case (style theme)
      (:inset
       (values very-dark dark light very-light))
      ((:outset :default)
       (values light very-light very-dark dark))
      (:flat
       (values very-dark dark very-dark dark)))))

(defun find-theme (o)
  ;; Does object have theme?
  (if (typep o 'theme-mixin)
      ;; Yes, is it valid?
      (if (not (eq nil (theme o)))
          ;; Yes, so use it
          (progn
            (v:debug :theme "find-theme: using object: ~a" (print-raw-object o))
            (return-from find-theme (theme o)))))

  ;; No theme, does it have a parent?
  (unless (typep o 'parent-mixin)
    ;; No parent and no theme so use default
    (v:debug :theme "find-theme: no theme, no parent, using default.")
    (return-from find-theme *theme-default*))

  ;; Object has parent, so a parent with a theme
  (let ((count 0)
        (p (parent o)))
    (loop
      ;; Valid parent?
      (if (not (eq nil p))
          ;; yes, does parent have theme?
          (if (typep p 'theme-mixin)
              ;; Yes, is it valid?
              (progn
                (unless (eq nil (theme p))
                  ;; Yes, so use it
                  (v:debug :theme "find-theme: using parent ~d: ~a" count (print-raw-object p))
                  (return-from find-theme (theme p)))

                ;; Parent has invalid theme, does it have a parent?
                (if (typep p 'parent-mixin)
                    ;; Yes, so loop
                    (progn
                      (setf p (parent p))
                      (incf count 1))
                    ;; No valid theme and no parent, use default
                    (progn
                      (v:debug :theme "find-theme: invalid parent theme, no parent, use default.")
                      (return-from find-theme *theme-default*))))
              ;; No, so does it have a parent?
              (if (typep p 'parent-mixin)
                  ;; Doesn't have a theme, but has a parent
                  (progn
                    (setf p (parent p))
                    (incf count 1))
                  ;; Doesn't have a theme and has no parent, use default
                  (progn
                    (v:debug :theme "find-theme: no contained theme, no parent, use default.")
                    (return-from find-theme *theme-default*))))

          ;; Parent not valid, so use default
          (progn
            (v:debug :theme "find-theme: no theme, no parent, use default.")
            (return-from find-theme *theme-default*))))))

