(in-package :cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; main =====================================================================

;; (defmethod on-paint :after (object &key)
;;   (v:info :pain "on-paint: ~a" (print-raw-object object))
;;   (my-next-method))

(defparameter *boss* nil)
(defparameter *w* nil)
(defparameter *cl1* nil)
(defparameter *cl2* nil)
(defparameter *t1* nil)
(defparameter *a1* nil)
(defparameter *a2* nil)
(defparameter *a3* nil)
(defparameter *rl* nil)
(defparameter *rt* nil)
(defparameter *rr* nil)
(defparameter *rb* nil)
(defparameter *g* nil)

(defparameter *outline* t)

(defun main-init ()
  (must-init (al:init) "allegro")
  (must-init (al:install-keyboard) "keyboard")
  (must-init (al:install-mouse) "mouse")
  (must-init (al:init-font-addon) "font addon")
  (must-init (al:init-ttf-addon) "ttf addon")
  (must-init (al:init-image-addon) "image addon")
  (must-init (al:init-primitives-addon) "primitives addon"))

(defun main-setup-display ()
  (al:set-new-display-option :sample-buffers 1 :suggest)
  (al:set-new-display-option :samples 0 :require)
  (al:set-new-display-flags '(:resizable))
  (al:set-blender +OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
  (al:create-display 960 720))

(defun main ()
  (main-init)
  (let ((screen (main-setup-display))
        (timer (al:create-timer (/ 1 60.0)))
        (queue (al:create-event-queue))
        (event (cffi:foreign-alloc '(:union al:event))))
    
    (unwind-protect
         (let ((c1 (al:map-rgb-f 0.75 0 0))
               (c2 (al:map-rgb-f 0.50 0 0)))

           ;; Left ruler
           (setq *rl* (ruler-10-2 :left 190 :top 200 :width 10 :height 400 :align :end
                                  :line-color c1 :shortcuts `((:1)) :vertical t
                                  :div-10-color c1 :div-2-color c2
                                  :div-10-extent 1 :div-2-extent 0.5))

           ;; Top ruler
           (setq *rt* (ruler-10-2 :left 200 :top 190 :width 300 :height 10 :align :end
                                  :line-color c1 :shortcuts `((:2))
                                  :div-10-color c1 :div-2-color c2
                                  :div-10-extent 1 :div-2-extent 0.5))

           ;; Right ruler
           (setq *rr* (ruler-10-2 :left 500 :top 200 :width 10 :height 400 :align :begin
                                  :line-color c1 :shortcuts `((:3)) :vertical t
                                  :div-10-color c1 :div-2-color c2
                                  :div-10-extent 1 :div-2-extent 0.5))
           
           ;; Bottom ruler
           (setq *rb* (ruler-10-2 :left 200 :top 600 :width 300 :height 10 :align :begin
                                  :line-color c1 :shortcuts `((:4))
                                  :div-10-color c1 :div-2-color c2
                                  :div-10-extent 1 :div-2-extent 0.5))
           
           (setq *g* (defgrid :major 50 :minor 10 :left 50 :top 50 :width 860 :height 620
                              :shortcuts (list '(:5))))

           (setq *t1* (deftext :title "Title" :h-align :center :v-align :middle
                               :height :auto-min))
           
           (setq *a1* (defactive-text :title "Asteroids" :h-align :center :v-align :middle
                                      :shortcuts (list '(:a :shift) '(:a :none))
                                      :left :center :top :middle :width :auto :height :auto-min
                                      :padding-left 0 :padding-right 0 :padding-top 10 :padding-bottom 10))
           (setq *a2* (defactive-text :title "Blastem" :h-align :center :v-align :middle
                                      :shortcuts (list '(:b :shift) '(:b :none))
                                      :left :center :top :middle :width :auto :height :auto-min
                                      :padding-left 10 :padding-right 10 :padding-top 10 :padding-bottom 10))
           (setq *a3* (defactive-text :title "Quit" :h-align :center :v-align :middle
                                      :shortcuts (list '(:q :shift) '(:q :none))
                                      :left :center :top :middle :width :auto :height :auto-min
                                      :padding-left 0 :padding-right 0 :padding-top 10 :padding-bottom 10))

           (setf (font *theme-default*) (default-font -24))
           
           (setq *cl2* (defcolumn-layout :content (list *a1* *a2* *a3*)))
           (setq *cl1* (defcolumn-layout :content (list '(*t1* :min-height) *cl2*)))
           (setq *w* (defwindow 200 200 300 400 :content (list *cl1*)))
           (setf *boss* (defmanager :content (list *w* *rl* *rt* *rr* *rb* *g*)))
           
           ;; Set grid color
           (setf (major-color *g*) (al:map-rgb-f 0 0.35 0))
           (setf (minor-color *g*) (al:map-rgb-f 0 0.25 0))

           ;; (setf (interior-color *w*) (al:map-rgb 191 63 63))
           
           (setf (padding *w*) 10)

           (defmethod on-command ((obj (eql *a1*)) &key)
             (v:info :app "Item 1 clicked"))

           (defmethod on-command ((obj (eql *a2*)) &key)
             (v:info :app "Item 2 clicked"))

           (defmethod on-command ((obj (eql *a3*)) &key)
             (v:info :app "Item 3 clicked")
             (setf (process *boss*) nil))

           (defmethod on-char ((key (eql :0)) mods (object (eql *boss*)) &key)
             (if *outline*
                 (setf *outline* nil)
                 (setf *outline* t)))
           
           (defmethod on-char (key mods (object (eql *boss*)) &key)
             (if (equal key :escape)
                 ;; Go away now!
                 (setf (process *boss*) nil)
                 ;; Pass on or else non-specific object's won't get told
                 (my-next-method)))

           (defmethod on-timer (source count (object (eql *boss*)) &key)
             (al:clear-to-color (al:map-rgb-f 0 0 0))
             (when *outline*
               (al:draw-line 200.0 0 200.0 719 (al:map-rgb-f 0 1 0) 1)
               (al:draw-line 500.0 0 500.0 719 (al:map-rgb-f 0 1 0) 1)
               (al:draw-line 0 200.0 959 200.0 (al:map-rgb-f 0 1 0) 1)
               (al:draw-line 0 600.0 959 600.0 (al:map-rgb-f 0 1 0) 1))
             (paint *boss*)
             (al:flip-display))

           (al:start-timer timer)
           (al:clear-keyboard-state screen)

           (al:register-event-source queue (al:get-keyboard-event-source))
           (al:register-event-source queue (al:get-display-event-source screen))
           (al:register-event-source queue (al:get-timer-event-source timer))
           (al:register-event-source queue (al:get-mouse-event-source))

           ;; (print *theme-3d-gray*)
           
           (process-events queue *boss*)

           ;; Remove methods
           (cleanup-method on-command (list `(eql ,*a1*)))
           (cleanup-method on-command (list `(eql ,*a2*)))
           (cleanup-method on-command (list `(eql ,*a3*)))
           (cleanup-method on-char (list `(eql :0) t `(eql ,*boss*)))
           (cleanup-method on-char (list t t `(eql ,*boss*)))
           (cleanup-method on-timer (list t t `(eql ,*boss*))))
      
      (progn
        (cffi:foreign-free event)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue))))
  nil)

