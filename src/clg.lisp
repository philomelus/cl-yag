(in-package :clg)

;;;; macros ===================================================================

(defmacro next-method ()
  `(if (next-method-p) (call-next-method)))

;;;; main =====================================================================

(defun main-init ()
  (clg-utils:must-init (al:init) "allegro")
  (clg-utils:must-init (al:install-keyboard) "keyboard")
  (clg-utils:must-init (al:install-mouse) "mouse")
  (clg-utils:must-init (al:init-font-addon) "font addon")
  (clg-utils:must-init (al:init-image-addon) "image addon")
  (clg-utils:must-init (al:init-primitives-addon) "primitives addon"))

(defun main-setup-display ()
  (al:set-new-display-option :sample-buffers 1 :suggest)
  (al:set-new-display-option :samples 8 :suggest)
  (al:set-new-display-flags '(:resizable))
  (al:create-display 960 720))

(defun main ()
  (main-init)
  (let ((screen (main-setup-display))
        (timer (al:create-timer (/ 1 60.0)))
        (queue (al:create-event-queue))
        (font (al:create-builtin-font))
        (buffer (al:create-bitmap 320 240))
        (event (cffi:foreign-alloc '(:union al:event))))
    
    (unwind-protect
         (progn
           
           (let* ((t1 (make-instance 'active-text :title "Asteroids" :font font
                                                  :h-align :center :x 0 :y 0))
                  (t2 (make-instance 'active-text :title "Blastem" :font font
                                                  :h-align :center :x 0 :y 50))
                  (t3 (make-instance 'active-text :title "Quit" :font font
                                                  :h-align :center :x 0 :y 100))
                  (vg (make-instance 'vertical-grid :content (list t1 t2 t3) :h-align :center :v-align :middle))
                  (w (make-instance 'window :x 200 :y 200 :w 400 :h 400 :content (list vg)))
                  (boss (make-instance 'manager :contents (list w))))

             (ready boss)

             (show w)
             (al:start-timer timer)
             (al:clear-keyboard-state screen)

             (al:register-event-source queue (al:get-keyboard-event-source))
             (al:register-event-source queue (al:get-display-event-source screen))
             (al:register-event-source queue (al:get-timer-event-source timer))
             (al:register-event-source queue (al:get-mouse-event-source))

             (flet ((unhandled-events (object event)
                      (declare (ignore object))
                      ;; (format *standard-output* "~&unhandled-events: ~a ~a" object event)
                      (case (cffi:foreign-slot-value event '(:union al:event) 'al::type)
                        (:timer
                         (al:clear-to-color (al:map-rgb-f 0.5 0.5 0.5))
                         (paint boss)
                         (al:flip-display))
                        
                        (:display-close (setf (manager-process boss) nil))
                        
                        (:display-resize
                         (on-resize w (cffi:foreign-slot-value event '(:struct al:display-event) 'al::x)
                                    (cffi:foreign-slot-value event '(:struct al:display-event) 'al::y)
                                    (cffi:foreign-slot-value event '(:struct al:display-event) 'al::width)
                                    (cffi:foreign-slot-value event '(:struct al:display-event) 'al::height)))

                        (otherwise
                         (format *standard-output* "~&event: ~a"
                                 (cffi:foreign-slot-value event '(:union al:event) 'al::type)))
                        ))
                    (on-char (event)
                      (if (equal (first (data event)) :escape)
                          (setf (manager-process boss) nil)))
                    )
               (connect "char" w boss #'on-char)
               (process-events queue boss :unhandled-event-proc #'unhandled-events))
             )
           )
      
      (progn
        (cffi:foreign-free event)
        (al:destroy-bitmap buffer)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue)
        (al:destroy-font font)))))

