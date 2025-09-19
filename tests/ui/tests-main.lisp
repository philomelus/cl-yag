(in-package :cl-yag-tests)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; stubs ====================================================================

(defmethod tests-command-0 (data)
  nil)

(defmethod tests-command-1 (data)
  nil)

(defmethod tests-command-2 (data)
  nil)

(defmethod tests-command-3 (data)
  nil)

(defmethod tests-command-4 (data)
  nil)

(defmethod tests-command-5 (data)
  nil)

(defmethod tests-command-6 (data)
  nil)

(defmethod tests-command-7 (data)
  nil)

(defmethod tests-command-8 (data)
  nil)

(defmethod tests-command-9 (data)
  nil)

(defmethod tests-command (data key mods)
  nil)

(defmethod tests-command-update (data)
  )

(defmethod tests-destroy (data)
  )

(defmethod tests-get-h-align (data)
  (values nil nil))

(defmethod tests-get-interior-color (data)
  (values nil nil))

(defmethod tests-get-padding (data)
  (values nil nil))

(defmethod tests-get-spacing (data)
  (values nil nil))

(defmethod tests-get-theme (data)
  (values nil nil))

(defmethod tests-get-thickness (data)
  (values nil nil))

(defmethod tests-get-v-align (data)
  (values nil nil))

(defmethod tests-ready (data)
  )

(defmethod tests-render (data)
  nil)

;; structs ====================================================================

(defstruct (tests-data (:include tests-theme-data)
                       (:conc-name tests-))
  
  manager event queue timer
  mono-font)

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
           (setf (tests-mono-font data) (default-mono-font -24))
           
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

           (defmethod cl-yag::on-char ((key (eql :c)) mods (object (eql (tests-manager data))) &key)
             (multiple-value-bind (widgets update) (tests-get-interior-color data)
               (unless (eql widgets nil)
                 (with-slots (manager theme1) data
                   (let ((ic1 (interior-color theme1))
                         (ic2 (al:map-rgb-f 1 0 0)))
                     (mapc #'(lambda (w)
                               (unless (eql w nil)
                                 (with-local-slots ((ic interior-color)) w
                                   (if (or (equal ic ic1)
                                           (equal ic nil))
                                       (setf (interior-color w) ic2)
                                       (setf (interior-color w) ic1)))))
                           widgets)))
                 (when update
                   (tests-command-update data)))))
           
           (defmethod cl-yag::on-char ((key (eql :h)) mods (object (eql (tests-manager data))) &key)
             (multiple-value-bind (widgets update) (tests-get-h-align data)
               (unless (eql widgets nil)
                 (mapc #'(lambda (w)
                           (unless (eql w nil)
                             (with-slots (h-align) w
                               (ecase h-align
                                 (:left (setf h-align :center))
                                 (:center (setf h-align :right))
                                 (:right (setf h-align :left))))))
                       widgets)
                 (when update
                   (tests-command-update data)))))
           
           (defmethod cl-yag::on-char ((key (eql :k)) mods (object (eql (tests-manager data))) &key)
             (multiple-value-bind (widgets update) (tests-get-thickness data)
               (unless (eql widgets nil)
                 (if (member :shift mods)
                     (progn
                       (mapc #'(lambda (w)
                                 (unless (eql w nil)
                                   (when (> (thickness w) 0)
                                     (decf (thickness w)))))
                             widgets))
                     (progn
                       (mapc #'(lambda (w)
                                 (unless (eql w nil)
                                   (when (< (thickness w) 50)
                                     (incf (thickness w)))))
                             widgets)))
                 (when update
                   (tests-command-update data)))))

           (defmethod cl-yag::on-char ((key (eql :p)) mods (object (eql (tests-manager data))) &key)
             (multiple-value-bind (widgets update) (tests-get-padding data)
               (unless (eql widgets nil)
                 (if (member :shift mods)
                     (progn
                       (mapc #'(lambda (w)
                                 (unless (eql w nil)
                                   (when (> (padding-left w) 0)
                                     (decf (padding-left w))
                                     (decf (padding-right w))
                                     (decf (padding-top w))
                                     (decf (padding-bottom w)))))
                             widgets))
                     (progn
                       (mapc #'(lambda (w)
                                 (unless (eql w nil)
                                   (when (< (padding-left w) 50)
                                     (incf (padding-left w))
                                     (incf (padding-right w))
                                     (incf (padding-top w))
                                     (incf (padding-bottom w)))))
                             widgets)))
                 (when update
                   (tests-command-update data)))))
           
           (defmethod cl-yag::on-char ((key (eql :s)) mods (object (eql (tests-manager data))) &key)
             (multiple-value-bind (widgets update) (tests-get-spacing data)
               (unless (eql widgets nil)
                 (if (member :shift mods)
                     (progn
                       (mapc #'(lambda (w)
                                 (when (> (spacing-left w) 0)
                                   (decf (spacing-left w))
                                   (decf (spacing-right w))
                                   (decf (spacing-top w))
                                   (decf (spacing-bottom w))))
                             widgets))
                     (progn
                       (mapc #'(lambda (w)
                                 (when (< (spacing-left w) 50)
                                   (incf (spacing-left w))
                                   (incf (spacing-right w))
                                   (incf (spacing-top w))
                                   (incf (spacing-bottom w))))
                             widgets)))
                 (when update
                   (tests-command-update data)))))
           
           (defmethod cl-yag::on-char ((key (eql :t)) mods (object (eql (tests-manager data))) &key)
             (tests-toggle-theme data))
           
           (defmethod cl-yag::on-char ((key (eql :v)) mods (object (eql (tests-manager data))) &key)
             (multiple-value-bind (widgets update) (tests-get-h-align data)
               (unless (eql widgets nil)
                 (mapc #'(lambda (w)
                           (unless (eql w nil)
                             (with-slots (v-align) w
                               (ecase v-align
                                 (:top (setf v-align :middle))
                                 (:middle (setf v-align :bottom))
                                 (:bottom (setf v-align :top))))))
                       widgets)
                 (when update
                   (tests-command-update data)))))
           
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
                     (list :0 :1 :2 :3 :4 :5 :6 :7 :8 :9 :c :h :p :s :t :v))
             (cleanup-method cl-yag::on-timer (list t t (list 'eql m))))
           
           ;; Clean up test
           (tests-destroy data))

      (progn
        (cffi:foreign-free event)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue)))))

