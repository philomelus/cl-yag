(in-package :cl-yag-tests)

(defstruct text-tests-data
  m w cl a1 a2 a3)

(defun text-tests-main ()
  (cl-yag::must-init (al:init) "allegro")
  (cl-yag::must-init (al:install-keyboard) "keyboard")
  (cl-yag::must-init (al:install-mouse) "mouse")
  (cl-yag::must-init (al:init-font-addon) "font addon")
  (cl-yag::must-init (al:init-image-addon) "image addon")
  (cl-yag::must-init (al:init-primitives-addon) "primitives addon")
  (al:set-new-display-option :sample-buffers 1 :suggest)
  (al:set-new-display-option :samples 8 :suggest)
  (al:set-new-display-flags '(:resizable))

  (let ((screen (al:create-display 960 720))
        (timer (al:create-timer (/ 1 60.0)))
        (queue (al:create-event-queue))
        (font (al:create-builtin-font))
        (buffer (al:create-bitmap 320 240))
        (event (cffi:foreign-alloc '(:union al:event))))
    
    (unwind-protect
         (let* ((data (make-text-tests-data)))

           (setf (text-tests-data-a1 data)
                 (defactive-text :title "First Active Text" :font font
                                 :h-align :center :v-align :middle
                                 :left +LAYOUT-LEFT-CALC+ :top +LAYOUT-TOP-CALC+
                                 :width +LAYOUT-WIDTH-CALC+ :height +LAYOUT-HEIGHT-CALC+))
           (setf (text-tests-data-a2 data)
                 (defactive-text :title "Another Active Text" :font font
                                 :h-align :center :v-align :middle
                                 :left +LAYOUT-LEFT-CALC+ :top +LAYOUT-TOP-CALC+
                                 :width +LAYOUT-WIDTH-CALC+ :height +LAYOUT-HEIGHT-CALC+))
           (setf (text-tests-data-a3 data)
                 (defactive-text :title "Quit" :font font
                                 :h-align :center :v-align :middle
                                 :left +LAYOUT-LEFT-CALC+ :top +LAYOUT-TOP-CALC+
                                 ;;:width +LAYOUT-WIDTH-CALC+ :height +LAYOUT-HEIGHT-CALC+
                   ))
           (setf (text-tests-data-cl data)
                 (defcolumn-layout :content (list (text-tests-data-a1 data)
                                                  (text-tests-data-a2 data)
                                                  (text-tests-data-a3 data))))
           (setf (text-tests-data-w data)
                 (defwindow 200 200 400 400 :content (list (text-tests-data-cl data))))
           (setf (text-tests-data-m data)
                 (make-instance 'manager :content (list (text-tests-data-w data))))

           (al:start-timer timer)
           (al:clear-keyboard-state screen)

           (al:register-event-source queue (al:get-keyboard-event-source))
           (al:register-event-source queue (al:get-display-event-source screen))
           (al:register-event-source queue (al:get-timer-event-source timer))
           (al:register-event-source queue (al:get-mouse-event-source))

           (defmethod on-command ((obj (eql (text-tests-data-a1 data))) &key)
             (v:info :tests "Item 1 clicked"))

           (defmethod on-command ((obj (eql (text-tests-data-a2 data))) &key)
             (v:info :tests "Item 2 clicked"))

           (defmethod on-command ((obj (eql (text-tests-data-a3 data))) &key)
             (v:info :tests "Item 3 clicked")
             (setf (process (text-tests-data-m data)) nil))

           (defmethod on-char (key mods (object (eql (text-tests-data-m data))) &key)
             (if (equal key :escape)
                 (setf (process (text-tests-data-m data)) nil)))
        
           (defmethod unhandled-event (event (object (eql (text-tests-data-m data))))
           ;; (defmethod unhandled-event (event (object manager))
             (declare (ignore object))
             (case (event-type event)
               (:timer
                (al:clear-to-color (al:map-rgb-f 0.25 0.25 0.25))
                (paint (text-tests-data-m data))
                (al:flip-display))
            
               (:display-close (setf (process (text-tests-data-m data)) nil))
            
               ;; (:display-resize
               ;;  (on-resize (text-tests-data-w data) (display-event-x event) (display-event-y event)
               ;;             (display-event-width event) (display-event-height event)))

               (otherwise
                (v:debug :event "event: ~a"
                         (event-type event)))))
           
           ;;(princ (text-tests-data-m data) *standard-output*)
           (process-events queue (text-tests-data-m data))
           )
      
      (progn
        (cffi:foreign-free event)
        (al:destroy-bitmap buffer)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue)
        (al:destroy-font font)))))

