(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; stubs ====================================================================

(defmethod tests-command-0 (data)
  )

(defmethod tests-command-1 (data)
  )

(defmethod tests-command-2 (data)
  )

(defmethod tests-command-3 (data)
  )

(defmethod tests-command-4 (data)
  )

(defmethod tests-command-5 (data)
  )

(defmethod tests-command-6 (data)
  )

(defmethod tests-command-7 (data)
  )

(defmethod tests-command-8 (data)
  )

(defmethod tests-command-9 (data)
  )

(defmethod tests-command-update (data)
  )

(defmethod tests-destroy (data)
  )

(defmethod tests-ready (data)
  )

(defmethod tests-render (data)
  )

;; structs ====================================================================

(defstruct (tests-data (:include tests-theme-data)
                       (:conc-name tests-))
  
  manager event queue timer)

;;;; functions ================================================================

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
  (al:set-blender +OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)

  (let ((screen (al:create-display (+ +WHS+ +WW+ +WHS+ +WW+ +WHS+ +WW+ +WHS+ +WW+ +WHS+)
                                   (+ +WWS+ +WH+ +WWS+ +WH+ +WWS+ +IH+ +WWS+)))
        (timer (al:create-timer (/ 1 60.0)))
        (queue (al:create-event-queue))
        (event (cffi:foreign-alloc '(:union al:event))))
    
    (unwind-protect                     ; event
         (progn
           (setf (tests-event data) event)
           (setf (tests-queue data) queue)
           (setf (tests-timer data) timer)
           
           (al:register-event-source queue (al:get-keyboard-event-source))
           (al:register-event-source queue (al:get-display-event-source screen))
           (al:register-event-source queue (al:get-timer-event-source timer))
           (al:register-event-source queue (al:get-mouse-event-source))

           ;; Set up themes
           (let ((fnt (default-font -24)))
             (setf (font *theme-default*) fnt)
             (setf (tests-theme1 data) (theme-flat-gray))
             (setf (font (tests-theme1 data)) fnt)
             (setf (tests-theme2 data) (theme-3d-gray))
             (setf (font (tests-theme2 data)) fnt))
           
           ;; Let test set up window/objects
           (tests-create data)

           ;; Set initial theme
           (setf (theme (tests-manager data)) (tests-theme1 data))

           ;; Final preparations
           (al:start-timer timer)
           (al:clear-keyboard-state screen)
           
           (defmethod cl-yag::on-char ((key (eql :escape)) mods (object (eql (tests-manager data))) &key)
             (setf (process object) nil)
             t)

           (defmethod cl-yag::on-char ((key (eql :0)) mods (object (eql (tests-manager data))) &key)
             (if (tests-command-0 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :1)) mods (object (eql (tests-manager data))) &key)
             (if (tests-command-1 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :2)) mods (object (eql (tests-manager data))) &key)
             (if (tests-command-2 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :3)) mods (object (eql (tests-manager data))) &key)
             (if (tests-command-3 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :4)) mods (object (eql (tests-manager data))) &key)
             (if (tests-command-4 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :5)) mods (object (eql (tests-manager data))) &key)
             (if (tests-command-5 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :6)) mods (object (eql (tests-manager data))) &key)
             (if (tests-command-6 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :7)) mods (object (eql (tests-manager data))) &key)
             (if (tests-command-7 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :8)) mods (object (eql (tests-manager data))) &key)
             (if (tests-command-8 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :9)) mods (object (eql (tests-manager data))) &key)
             (if (tests-command-9 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-timer (timer count (object (eql (tests-manager data))) &key)
             ;; Let test render.  If returns nil, perform default rendering
             (unless (tests-render data)
               (al:clear-to-color (al:map-rgb-f 0.25 0.25 0.25))  
               (paint object))
             (al:flip-display)
             (if (next-method-p)
                 (call-next-method)))

           ;; Let test hook up method and such
           (tests-ready data)
           
           ;; Handle events until process is nil
           (process-events queue (tests-manager data))

           ;; Remove methods
           (with-local-slots ((m manager)) data
             (mapcar #'(lambda (char)
                         (cleanup-method cl-yag::on-char (list (list 'eql char) t (list 'eql m))))
                     (list :0 :1 :2 :3 :4 :5 :6 :7 :8 :9))
             (cleanup-method cl-yag::on-timer (list t t (list 'eql m))))
           
           ;; Clean up test
           (tests-destroy data))

      (progn
        (cffi:foreign-free event)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue)))))

