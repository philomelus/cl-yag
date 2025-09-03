(in-package :cl-yag)

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
         (progn                   ; Not needed, because of the let ...
           (setq *rl* (defruler :vertical t :major 10 :minor 2 :left 190 :top 200 :width 10 :height 400
                                :shortcuts (list '(:1))))
           (setq *rt* (defruler :vertical nil :major 10 :minor 2 :left 200 :top 190 :width 400 :height 10
                                :shortcuts (list '(:2))))
           (setq *rr* (defruler :vertical t :major 10 :minor 2 :left 510 :top 200 :width 10 :height 400
                                :shortcuts (list '(:3)) :invert t))
           (setq *rb* (defruler :vertical nil :major 10 :minor 2 :left 200 :top 510 :width 400 :height 10
                                :shortcuts (list '(:4)) :invert t))
           
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
           
           ;; Adjust rulers
           (setf (height *rl*) (1+ (height *w*)))
           (setf (width *rt*) (width *w*))
           (setf (left *rr*) (+ (left *w*) (width *w*) 11))
           (setf (height *rr*) (1+ (height *w*)))
           (setf (top *rb*) (+ (top *w*) (height *w*) 11))
           (setf (width *rb*) (width *w*))
           
           (setf (color *rl*) (al:map-rgb-f 1 0 0))
           (setf (color *rt*) (al:map-rgb-f 1 0 0))
           (setf (color *rr*) (al:map-rgb-f 1 0 0))
           (setf (color *rb*) (al:map-rgb-f 1 0 0))
           
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

           (defmethod on-char (key mods (object (eql *boss*)) &key)
             (if (equal key :escape)
                 ;; Go away now!
                 (setf (process *boss*) nil)
                 ;; Pass on or else non-specific object's won't get told
                 (my-next-method)))

           (defmethod on-timer (source count (object (eql *boss*)) &key)
             (al:clear-to-color (al:map-rgb-f 0 0 0))
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
           (cleanup-method on-command (list (list 'eql *a1*)))
           (cleanup-method on-command (list (list 'eql *a2*)))
           (cleanup-method on-command (list (list 'eql *a3*)))
           (cleanup-method on-char (list t t (list 'eql *boss*)))
           (cleanup-method on-timer (list t t (list 'eql *boss*))))
      
      (progn
        (cffi:foreign-free event)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue))))
  nil)

