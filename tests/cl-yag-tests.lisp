(in-package :cl-yag-tests)

(defstruct tests-data manager event queue timer)

(defgeneric tests-create (data) (:documentation "Create windows/object/data for tests."))

(defgeneric tests-destroy (data) (:documentation "Clean up data created for tests."))

(defgeneric tests-render (data)
  (:method (data)
    nil)
  (:documentation "Called to render display.  If it returns nil, will clear screen and  call
paint for manager object.  Display is flipped automatically after call
regardless of result."))

(defun tests-main (data)
  "Main entry point for tests.
Call this passing in a structure that includes the tests-data object. This will
setup a display, screen, and allocate an event object and queue then enter a
processing loop until manager object's process slot is set to nil.

Default event processing watches for ESCAPE and sets process to nil.
Default event processing watches for :display-close event as well."
  
  (cl-yag::must-init (al:init) "allegro")
  (cl-yag::must-init (al:install-keyboard) "keyboard")
  (cl-yag::must-init (al:install-mouse) "mouse")
  (cl-yag::must-init (al:init-font-addon) "font addon")
  (cl-yag::must-init (al:init-ttf-addon) "ttf addon")
  (cl-yag::must-init (al:init-image-addon) "image addon")
  (cl-yag::must-init (al:init-primitives-addon) "primitives addon")
  (al:set-new-display-option :sample-buffers 1 :suggest)
  (al:set-new-display-option :samples 0 :require)

  (let ((screen (al:create-display 960 720))
        (timer (al:create-timer (/ 1 60.0)))
        (queue (al:create-event-queue))
        (event (cffi:foreign-alloc '(:union al:event))))
    
    (unwind-protect                     ; event
         (progn
           (setf (tests-data-event data) event)
           (setf (tests-data-queue data) queue)
           (setf (tests-data-timer data) timer)
           
           (al:register-event-source queue (al:get-keyboard-event-source))
           (al:register-event-source queue (al:get-display-event-source screen))
           (al:register-event-source queue (al:get-timer-event-source timer))
           (al:register-event-source queue (al:get-mouse-event-source))

           ;; Set up window/objects
           (tests-create data)

           (al:start-timer timer)
           (al:clear-keyboard-state screen)

           (defmethod on-char (key mods (object (eql (tests-data-manager data))) &key)
             (if (equal key :escape)
                 (setf (process (tests-data-manager data)) nil)
                 (if (next-method-p)
                     (call-next-method))))
      
           (defmethod unhandled-event (event (object (eql (tests-data-manager data))))
             (declare (ignore object))
             (case (event-type event)
               (:timer
                ;; Let test render.  If returns nil, perform default rendering
                (unless (tests-render data)
                  (al:clear-to-color (al:map-rgb-f 0.25 0.25 0.25))  
                  (paint (tests-data-manager data)))
                (al:flip-display))
          
               (:display-close (setf (process (tests-data-manager data)) nil))
          
               (otherwise
                (v:debug :event "event: ~a" (event-type event))))
             (if (next-method-p)
                 (call-next-method)))

           ;; Handle events until process is nil
           (process-events queue (tests-data-manager data))
           
           ;; Clean up windows/objects
           (tests-destroy data))

      (progn
        (cffi:foreign-free event)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue)))))

