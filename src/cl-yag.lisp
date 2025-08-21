(in-package :cl-yag)

;;;; main =====================================================================

;; (defmethod on-paint :after (object &key)
;;   (v:info :pain "on-paint: ~a" (print-raw-object object))
;;   (my-next-method))

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
        (font (default-font -24))
        (event (cffi:foreign-alloc '(:union al:event))))
    
    (unwind-protect
         (progn                   ; Not needed, because of the let ...
           (let* ((rl (defruler :vertical t :major 10 :minor 2 :left 190 :top 200 :width 10 :height 400
                                :shortcuts (list '(:1))))
                  (rt (defruler :vertical nil :major 10 :minor 2 :left 200 :top 190 :width 400 :height 10
                                :shortcuts (list '(:2))))
                  (rr (defruler :vertical t :major 10 :minor 2 :left 510 :top 200 :width 10 :height 400
                                :shortcuts (list '(:3)) :invert t))
                  (rb (defruler :vertical nil :major 10 :minor 2 :left 200 :top 510 :width 400 :height 10
                                :shortcuts (list '(:4)) :invert t))
                  
                  (g (defgrid :major 50 :minor 10 :left 50 :top 50 :width 860 :height 620
                              :shortcuts (list '(:5))))

                  (a1 (deftext :title "Title" :font font :h-align :center :v-align :middle))
                  
                  (a2 (defactive-text :title "Asteroids" :font font :h-align :center :v-align :middle
                                      :shortcuts (list '(:a :shift) '(:a :none))
                                      :left :center :top :middle :width :auto :height :auto-min
                                      :padding-top 10 :padding-bottom 10))
                  (a3 (defactive-text :title "Blastem" :font font :h-align :center :v-align :middle
                                      :shortcuts (list '(:b :shift) '(:b :none))
                                      :left :center :top :middle :width :auto-min :height :auto
                                      :padding-left 0 :padding-right 0))
                  (a4 (defactive-text :title "Quit" :font font :h-align :center :v-align :middle
                                      :shortcuts (list '(:q :shift) '(:q :none))
                                      :left :center :top :middle :width :auto :height :auto))
                  ;; (cl (defcolumn-layout :content (list a2 a3 a4)))
                  (cl (defcolumn-layout :content (list a1 a2 a3 a4)))
                  (w (defwindow 200 200 300 400 :content (list cl) ;;:interior-color (al:map-rgb-f 1 1 1)
                       ))
                  (boss (defmanager :content (list w rt rl rr rb g))))

             ;; Adjust rulers
             (setf (height rl) (1+ (height w)))
             (setf (width rt) (width w))
             (setf (left rr) (+ (left w) (width w) 11))
             (setf (height rr) (1+ (height w)))
             (setf (top rb) (+ (top w) (height w) 11))
             (setf (width rb) (width w))
             ;; (setf (color rl) (al:map-rgb-f 1 0 0))
             ;; (setf (color rt) (al:map-rgb-f 1 0 0))
             ;; (setf (color rr) (al:map-rgb-f 1 0 0))
             ;; (setf (color rb) (al:map-rgb-f 1 0 0))
             
             ;; Set grid color
             ;; (setf (major-color g) (al:map-rgb-f 0 0.35 0))
             ;; (setf (minor-color g) (al:map-rgb-f 0 0.25 0))

             
             ;; (setf (theme boss) *theme-3d-gray*)
             ;; (setf (theme a2) *theme-3d-red*)
             ;; (setf (theme a3) *theme-3d-green*)

             ;; (setf (border w) (defborder-3d :width 10))
             ;; (setf (border w) (defborder-3d :width 10))
             ;; (setf (border w) (defborder-flat :width 10 :color (al:map-rgb-f 0 1 0)))
             ;; (setf (border a3) (defborder-flat :color (al:map-rgb-f 1 1 0) :width 10))
             ;; (setf (border a3) (defborder-3d :width 6 :theme *theme-3d-yellow*))

             ;; (setf *theme-default* (theme-3d-gray))
             ;; (setf (interior-color w) (al:map-rgb 127 0 0))
             ;; (setf (hover-color a2) (al:map-rgb 0 255 0))
             ;; (setf (hover-color a3) (al:map-rgb 0 255 0))
             ;; (setf (hover-color a4) (al:map-rgb 0 255 0))
             
             (setf (padding w) 10)
             
             (defmethod on-command ((obj (eql a2)) &key)
               (v:info :app "Item 1 clicked"))

             (defmethod on-command ((obj (eql a3)) &key)
               (v:info :app "Item 2 clicked"))

             (defmethod on-command ((obj (eql a4)) &key)
               (v:info :app "Item 3 clicked")
               (setf (process boss) nil))

             (defmethod on-char (key mods (object (eql boss)) &key)
               (if (equal key :escape)
                   ;; Go away now!
                   (setf (process boss) nil)
                   ;; Pass on or else non-specific object's won't get told
                   (my-next-method)))
             
             (defmethod unhandled-event (event (object (eql boss)))
               (declare (ignore object))
               (case (event-type event)
                 (:timer
                  (al:clear-to-color (al:map-rgb-f 0 0 0))
                  (paint boss)
                  (al:flip-display))
                 
                 (:display-close (setf (process boss) nil))
                 
                 (:display-resize
                  (on-resize w (display-event-x event) (display-event-y event)
                             (display-event-width event) (display-event-height event)))

                 (otherwise
                  (v:debug :event "event: ~a" (event-type event)))))

             (al:start-timer timer)
             (al:clear-keyboard-state screen)

             (al:register-event-source queue (al:get-keyboard-event-source))
             (al:register-event-source queue (al:get-display-event-source screen))
             (al:register-event-source queue (al:get-timer-event-source timer))
             (al:register-event-source queue (al:get-mouse-event-source))

             ;; (print *theme-3d-gray*)
             
             (process-events queue boss)

             ;; (print w)
             ))
      
      (progn
        (cffi:foreign-free event)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue)
        (al:destroy-font font)))))

