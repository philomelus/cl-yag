(in-package #:cl-yag)

;;;; box ======================================================================

;;; theme-mixin -----------------------------------------------------

(defclass box-theme-mixin (font-mixin
                           fore-color-mixin
                           interior-color-mixin)
  ())

(defclass box-3d-theme-mixin (color-3d-mixin
                              style-3d-mixin)
  ())

(defclass box-flat-theme-mixin (frame-color-mixin)
  ())

;;; box ---- --------------------------------------------------------

(defparameter +BOX-TITLE-POSITION-OPTIONS+ '(:left-top :left-middle :left-bottom
                                             :center-top :center-middle :center-bottom
                                             :right-top :right-middle :right-bottom))
(defclass box (box-theme-mixin
               area-mixin
               parent-mixin
               theme-mixin
               title-mixin
               v-align-mixin)
  ((thickness :initarg :thickness :initform 1 :accessor thickness :documentation "Number of pixels wide for the box frame.")
   (filled :initarg :filled :initform nil :accessor filled :documentation "When T, fill interior of box.")
   (title-position :initarg :title-position :initform nil :accessor title-position)))

(defmacro defbox (&rest rest &key &allow-other-keys)
  `(make-instance 'box ,@rest))

;;; methods ---------------------------------------------------------

(defmethod initialize-instance :after ((object box) &key)
  (validate-box-options object))

(defmethod (setf title-position) :after (value (object box))
  (validate-box-options object))

;; #+safety
(defmethod (setf theme) (value (object box))
  (assert (member (type-of value) '(box-3d-theme-mixin 'box-flat-theme-mixin))))

(defmethod on-paint ((object box) &key)
  
  (let ((theme (find-theme object)))
   (typecase theme
     (box-3d-theme-mixin (paint-box-3d object theme))
     (box-flat-theme-mixin (paint-box-flat object theme))
     (t (error "don't know how to paint box from ~a" (type-of theme))))))

;;; functions -------------------------------------------------------

(defun paint-box-3d (Object theme)
  (declare (ignore object theme)))

(defun paint-box-flat (object theme)
  (when (filled object)
    (paint-box-flat-interior object theme))
  (paint-box-flat-frame object theme)
  (unless (= (length (title object)) 0)
    (paint-box-flat-title object theme)))

(defun paint-box-flat-interior (object theme)
  (with-area-rb (ol ot or ob) object
    (with-local-accessors ((tn thickness)) object
      (with-theme ((ic interior-color)) theme
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-INVERSE-ALPHA+)
          (al:draw-filled-rectangle (2- (+ ol tn)) (2- (+ ot tn)) (1+ (- or tn)) (1+ (- ob tn)) ic))))))

(defun paint-box-flat-frame (object theme)
  (with-object-and-theme (font) object theme
    (with-theme (frame-color) theme
      (with-local-accessors (title thickness title-position v-align) object
        (with-area-rb (ol ot or ob) object
          (let ((title-height (al:get-font-line-height font)) ; title height
                (title-width (al:get-text-width font title)) ; title width
                (tn2 (/ thickness 2))   ; half thickness
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
                  (al:draw-rectangle (+ ol tn2) (+ ct tn2) (- or tn2) (- cb tn2) frame-color thickness)))))))))

(defun paint-box-flat-title (object theme)
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

(defun validate-box-options (object)
  (with-local-slots ((tp title-position) (ti title)) object
    ;; Make sure they gave title-position if title is not empty
    (if (= (length ti) 0)
        (unless (eq tp nil)
          (warn "title-position not needed when title empty"))
        (when (eql tp nil)
          (error "when using title, title-position is required")))))

