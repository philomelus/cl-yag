(in-package :cl-yag)

;;;; main =====================================================================

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
    
    (unwind-protect
         (progn                   ; Not needed, because of the let ...
           (let* ((a2 (defactive-text :title "Asteroids" :font font
                                      :h-align :center :v-align :middle
                                      :left +LAYOUT-LEFT-CALC+ :top +LAYOUT-TOP-CALC+
                        ))
                  (a3 (defactive-text :title "Blastem" :font font
                                      :h-align :center :v-align :middle
                                      :left +LAYOUT-LEFT-CALC+ :top +LAYOUT-TOP-CALC+ 
                        ))
                  (a4 (defactive-text :title "Quit" :font font
                                      :h-align :center :v-align :middle
                                      :left +LAYOUT-LEFT-CALC+ :top +LAYOUT-TOP-CALC+ 
                        ))
                  (w (defwindow 200 200 400 400 ((defcolumn-layout (a2 a3 a4)))))
                  (boss (make-instance 'manager :content (list w)))
                  (selected-object nil))
             
             (defmethod on-mouse-click (x y b (obj (eql (first (content (first (content w)))))) &key)
               (v:info :app "Item 1 clicked")
               (setf selected-object obj))

             (defmethod on-mouse-click (x y b (obj (eql (second (content (first (content w)))))) &key)
               (v:info :app "Item 2 clicked"))

             (defmethod on-mouse-click (x y b (obj (eql (third (content (first (content w)))))) &key)
               (v:info :app "Item 3 clicked")
               (setf (process boss) nil))

             (defmethod on-char (key mods (object (eql boss)) &key)
               (if (equal key :escape)
                   (setf (process boss) nil)))
             
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


             (setf (border w) (defborder :color (al:map-rgb-f 1 1 0) :width 10))
             (setf (border a3) (defborder :color (theme-vl *theme-flat-yellow*)))
             
             (setf (theme boss) *theme-flat-blue*)
             (setf (theme a2) *theme-flat-red*)
             (setf (theme a3) *theme-flat-green*)

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

             ;; (print w *standard-output*)
             (process-events queue boss)))
      
      (progn
        (cffi:foreign-free event)
        (al:destroy-bitmap buffer)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue)
        (al:destroy-font font)))))

