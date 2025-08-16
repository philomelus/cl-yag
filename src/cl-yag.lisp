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
  (must-init (al:init-image-addon) "image addon")
  (must-init (al:init-primitives-addon) "primitives addon"))

(defun main-setup-display ()
  (al:set-new-display-option :sample-buffers 1 :suggest)
  (al:set-new-display-option :samples 8 :suggest)
  (al:set-new-display-flags '(:resizable))
  (al:create-display 960 720))

(defun main ()
  (main-init)
  ;; (setf (v:repl-categories) (list :app :theme))
  (let ((screen (main-setup-display))
        (timer (al:create-timer (/ 1 60.0)))
        (queue (al:create-event-queue))
        (font (al:create-builtin-font))
        (buffer (al:create-bitmap 320 240))
        (event (cffi:foreign-alloc '(:union al:event))))
    
    (al:set-blender +OP-ADD+ +BLEND-ONE+ +BLEND-INVERSE-ALPHA+)
    
    (unwind-protect
         (progn                   ; Not needed, because of the let ...
           (let* ((rh (defruler :vertical nil :major 25 :minor 5 :left 200 :top 179 :width 400 :height 10 :color (al:map-rgb-f 1 1 1)))
                  (rv (defruler :vertical t :major 25 :minor 5 :left 179 :top 200 :width 10 :height 400 :color (al:map-rgb-f 1 1 1)))
                  (a2 (defactive-text :title "Asteroids" :font font
                                      :h-align :center :v-align :middle
                                      :shortcuts (list '(:a :shift) '(:a :none))
                                      :left +LAYOUT-LEFT-CALC+
                                      :top +LAYOUT-TOP-CALC+
                                      :width +LAYOUT-WIDTH-CALC+
                                      :height +LAYOUT-HEIGHT-CALC+))
                  (a3 (defactive-text :title "Blastem" :font font
                                      :h-align :center :v-align :middle
                                      :shortcuts (list '(:b :shift) '(:b :none))
                                      :left +LAYOUT-LEFT-CALC+
                                      :top +LAYOUT-TOP-CALC+
                                      :width +LAYOUT-WIDTH-CALC+
                                      :height +LAYOUT-HEIGHT-CALC+))
                  (a4 (defactive-text :title "Quit" :font font
                                      :h-align :center :v-align :middle
                                      :shortcuts (list '(:q :shift) '(:q :none))
                                      :left +LAYOUT-LEFT-CALC+
                                      :top +LAYOUT-TOP-CALC+
                                      :width +LAYOUT-WIDTH-CALC+
                                      :height +LAYOUT-HEIGHT-CALC+))
                  (cl (defcolumn-layout :content (list a2 a3 a4)))
                  (w (defwindow 200 200 400 400 :content (list cl)))
                  (boss (defmanager :content (list w rh rv))))
             
             (defmethod on-command ((obj (eql (first (content (first (content w)))))) &key)
               (v:info :app "Item 1 clicked"))

             (defmethod on-command ((obj (eql (second (content (first (content w)))))) &key)
               (v:info :app "Item 2 clicked"))

             (defmethod on-command ((obj (eql (third (content (first (content w)))))) &key)
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
                  (al:clear-to-color (al:map-rgb-f 0.25 0.25 0.25))
                  (paint boss)
                  (al:flip-display))
                 
                 (:display-close (setf (process boss) nil))
                 
                 (:display-resize
                  (on-resize w (display-event-x event) (display-event-y event)
                             (display-event-width event) (display-event-height event)))

                 (otherwise
                  (v:debug :event "event: ~a" (event-type event)))))

             (setf (theme boss) *theme-flat-blue*)
             (setf (theme a2) *theme-flat-red*)
             (setf (theme a3) *theme-flat-green*)

             (setf (border w) (defborder :color (al:map-rgb-f 0.75 0.75 0.75) :width 10))
             (setf (border a3) (defborder :color (theme-vl *theme-flat-yellow*) :width 10))
             
             (let* ((white (al:map-rgb-f 1 1 1)))
               (setf (fore-color a2) white)
               (setf (fore-color a3) white)
               (setf (fore-color a4) white))

             (al:start-timer timer)
             (al:clear-keyboard-state screen)

             (al:register-event-source queue (al:get-keyboard-event-source))
             (al:register-event-source queue (al:get-display-event-source screen))
             (al:register-event-source queue (al:get-timer-event-source timer))
             (al:register-event-source queue (al:get-mouse-event-source))

             (process-events queue boss)))
      
      (progn
        (cffi:foreign-free event)
        (al:destroy-bitmap buffer)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue)
        (al:destroy-font font)))))

