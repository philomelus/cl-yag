(in-package #:cl-yag)

;;;; box ======================================================================

;;; theme-mixin -----------------------------------------------------

(defclass box-theme-mixin (fore-color-mixin ; for title
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

;;; method ----------------------------------------------------------

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
  (flet ((get-params (object theme)
           (with-theme ((fnt font)) theme
             (with-local-slots ((tp title-position)) object
               (let ((th (al:get-font-line-height fnt)))
                 (with-area (ol ot ow oh) object
                   (let (flags left top)
                     (case tp
                       (:left-top
                        (setq flags +ALIGN-LEFT+ left (+ ol (/ th 3)) top ot))
                       (:left-middle
                        (setq flags +ALIGN-LEFT+ left (+ ol (/ th 3)) top (+ ot (/ (- oh th) 2))))
                       (:left-bottom
                        (setq flags +ALIGN-LEFT+ left (+ ol (/ th 3)) top (- (+ ot oh) th)))
                       (:center-top
                        (setq flags +ALIGN-CENTER+ left (+ ol (/ ow 2)) top ot))
                       (:center-middle
                        (setq flags +ALIGN-CENTER+ left (+ ol (/ ow 2)) top (+ ot (/ (- oh th) 2))))
                       (:center-bottom
                        (setq flags +ALIGN-CENTER+ left (+ ol (/ ow 2)) top (- (+ ot oh) th)))
                       (:right-top
                        (setq flags +ALIGN-RIGHT+ left (+ ol ow) top ot))
                       (:right-middle
                        (setq flags +ALIGN-RIGHT+ left (+ ol ow) top (+ ot (/ (- oh th) 2))))
                       (:right-bottom
                        (setq flags +ALIGN-RIGHT+ left (+ ol ow) top (- (+ ot oh) th)))
                       (otherwise
                        (error "unknown title-position option: ~a" tp)))
                     (values flags left top))))))))
    
    (with-theme ((fc frame-color) (frc fore-color) (fnt font)) theme
      (with-area-rb (ol ot or ob) object
        (with-local-accessors ((ti title) (tn thickness) (tp title-position) (va v-align)) object
          (let ((tn2 (/ tn 2))
                (th (al:get-font-line-height fnt))
                (ct ot)
                (cb ob))

            ;; Adjust for top placement
            (when (member tp '(:left-top :center-top :right-top))
              (incf ct th))

            ;; Adjust for bottom placement
            (when (member tp '(:left-bottom :center-bottom :right-bottom))
              (decf cb th))

            ;; Adjust for vertical alignment
            (case va
              ((:top :none))            ; Nothing to do
              (:middle
               (decf ct (/ th 2)))
              (:bottom
               (decf ct th))
              (otherwise
               (error "unkonw v-align option: ~a" va)))
            
            ;; Fill
            (when (filled object)
              (with-theme ((ic interior-color)) theme
                (al:draw-filled-rectangle (2- (+ ol tn)) (2- (+ ct tn)) (1+ (- or tn)) (1+ (- cb tn)) ic)))

            ;; Draw frame
            (when (/= tn 0)
              (if (eql va ':middle)
                  (let ((tw (al:get-text-width fnt ti))
                        ;; (th2 (/ th 2))
                        ;; (th4 (/ th 4))
                        ;; (nt (+ ct tn2 ))
                        v)
                    (let ((x1 (+ ol tn2))
                          (y1 (+ ct tn2))
                          (x2 (- or tn2))
                          (y2 (- cb tn2)))
                      (let ((x1x1 (- x1 tn2))
                            (x1x4 (+ x1 tn2))
                            (y1y1 (- y1 tn2))
                            (y1y2 (+ y1 tn2))
                            (x2x1 (- x2 tn2))
                            (x2x2 (+ x2 tn2))
                            (y2y1 (- y2 tn2))
                            (y2y2 (+ y2 tn2)))
                        (let ((x1x2 (+ x1x1 (/ th 3)))
                              (x1x3 (+ x1x1 tw (* (/ th 3) 2))))

                          ;; After text
                          (push (list x1x3 y1y1 fc) v)
                          (push (list x1x3 y1y2 fc) v)

                          ;; To the right upper corner
                          (push (list x2x2 y1y1 fc) v)
                          (push (list x2x1 y1y2 fc) v)
                      
                          ;; To right lower corner
                          (push (list x2x2 y2y2 fc) v)
                          (push (list x2x1 y2y1 fc) v)

                          ;; To left lower corner
                          (push (list x1x1 y2y2 fc) v)
                          (push (list x1x4 y2y1 fc) v)

                          ;; To left upper corner
                          (push (list x1x1 y1y1 fc) v)
                          (push (list x1x4 y1y2 fc) v)

                          ;; Left upper corner to before text
                          (push (list x1x2 y1y1 fc) v)
                          (push (list x1x2 y1y2 fc) v))))
                  
                    (draw-prim v :triangle-strip))
                  (al:draw-rectangle (+ ol tn2) (+ ct tn2) (- or tn2) (- cb tn2) fc tn)))
          
            ;; Display title
            (unless (= (length (title object)) 0)
              (multiple-value-bind (f cl ct) (get-params object theme)
                
                ;; Draw the text
                (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-INVERSE-ALPHA+)
                  (al:draw-text fnt frc cl ct f ti))))))))))

