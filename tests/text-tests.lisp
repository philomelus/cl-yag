(in-package :cl-yag-tests)

;;;; main =====================================================================

(defun init ()
  (cl-yag::must-init (al:init) "allegro")
  (cl-yag::must-init (al:install-keyboard) "keyboard")
  (cl-yag::must-init (al:install-mouse) "mouse")
  (cl-yag::must-init (al:init-font-addon) "font addon")
  (cl-yag::must-init (al:init-image-addon) "image addon")
  (cl-yag::must-init (al:init-primitives-addon) "primitives addon"))

(defun setup-display ()
  (al:set-new-display-option :sample-buffers 1 :suggest)
  (al:set-new-display-option :samples 8 :suggest)
  (al:set-new-display-flags '(:resizable))
  (al:create-display 1024 768))


(defun text-tests-main ()
  (init)
  (setf (v:repl-categories) (list :app :theme))
  (let ((screen (setup-display))
        (timer (al:create-timer (/ 1 60.0)))
        (queue (al:create-event-queue))
        (font (al:create-builtin-font))
        (event (cffi:foreign-alloc '(:union al:event))))
    
    (unwind-protect
         (progn                         ; Not needed, because of the let ...
           (let* ((w1 (defwindow 200 200 400 400
                        ((defcolumn-layout ((defactive-text :title "Asteroids" :font font
                                                            :h-align :center :v-align :middle
                                                            :left +LAYOUT-LEFT-CALC+ :top +LAYOUT-TOP-CALC+
                                              )
                                            (defactive-text :title "Blastem" :font font
                                                            :h-align :center :v-align :middle
                                                            :left +LAYOUT-LEFT-CALC+ :top +LAYOUT-TOP-CALC+ 
                                              )
                                            (defactive-text :title "Quit" :font font
                                                            :h-align :center :v-align :middle
                                                            :left +LAYOUT-LEFT-CALC+ :top +LAYOUT-TOP-CALC+ 
                                              ))))))
                  (boss (make-instance 'manager :content (list w1)))
                  (selected-object nil))
             
             (defmethod on-mouse-click (x y b (obj (eql (first (content (first (content w1)))))) &key)
               (v:info :app "Item 1 clicked")
               (setf selected-object obj))

             (defmethod on-mouse-click (x y b (obj (eql (second (content (first (content w1)))))) &key)
               (v:info :app "Item 2 clicked"))

             (defmethod on-mouse-click (x y b (obj (eql (third (content (first (content w1)))))) &key)
               (v:info :app "Item 3 clicked")
               (setf (process boss) nil))

             (defmethod on-char (key mods (object (eql boss)) &key)
               (if (equal key :escape)
                   (setf (process boss) nil)))
             
             (defmethod unhandled-event (event (object (eql boss)))
               (declare (ignore object))
               (case (cffi:foreign-slot-value event '(:union al:event) 'al::type)
                 (:timer
                  (al:clear-to-color (al:map-rgb-f 0.25 0.25 0.25))
                  (paint boss)
                  (al:flip-display))
                 
                 (:display-close (setf (process boss) nil))
                 
                 (:display-resize
                  (on-resize w1 (cffi:foreign-slot-value event '(:struct al:display-event) 'al::x)
                       (cffi:foreign-slot-value event '(:struct al:display-event) 'al::y)
                             (cffi:foreign-slot-value event '(:struct al:display-event) 'al::width)
                             (cffi:foreign-slot-value event '(:struct al:display-event) 'al::height)))

                 (otherwise
                  (v:debug :event "event: ~a"
                          (cffi:foreign-slot-value event '(:union al:event) 'al::type)))
                 ))

             (let* ((wc (content w1))
                    (cl (first wc))
                    (cc (content cl))
                    (a1 (first cc))
                    (a2 (second cc))
                    (a3 (third cc))
                    (white (al:map-rgb-f 1 1 1)))

               (setf (border w1) (defborder :color (al:map-rgb-f 1 1 0) :width 10))
               (setf (border a2) (defborder :color (theme-vl *theme-flat-yellow*)))
               
               (setf (theme boss) *theme-flat-blue*)
               (setf (theme a1) *theme-flat-red*)
               (setf (theme a2) *theme-flat-green*)

               (setf (fore-color a1) white)
               (setf (fore-color a2) white)
               (setf (fore-color a3) white)
               )

             (al:start-timer timer)
             (al:clear-keyboard-state screen)

             (al:register-event-source queue (al:get-keyboard-event-source))
             (al:register-event-source queue (al:get-display-event-source screen))
             (al:register-event-source queue (al:get-timer-event-source timer))
             (al:register-event-source queue (al:get-mouse-event-source))

             ;; (print w1 *standard-output*)
             (process-events queue boss)))
      
      (progn
        (cffi:foreign-free event)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue)
        (al:destroy-font font)))))

