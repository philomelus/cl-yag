(in-package :cl-yag)

;;;; functions ================================================================

(defun dump-cl-yag (object offset)
  (flet ((off (width)
           (let ((c ""))
             (when (> width 0)
               (loop for i from 0 to (1- width) do
                 (setf c (concatenate 'string c "    "))))
             c)))
    (if (typep object 'area-mixin)
        (format *standard-output* "~&~a~a: (~d [~d], ~d [~d]) (~d [~d], ~d [~d])" (off offset) (type-of object)
                (left object) (slot-value object 'left) (top object) (slot-value object 'top)
                (width object) (slot-value object 'width) (height object) (slot-value object 'height))
        (format *standard-output* "~&~a~a:" (off offset) (type-of object)))
    (if (typep object 'content-mixin)
        (dolist (child (content object))
          (dump-cl-yag child (1+ offset))))))

;;;; main =====================================================================

;; (defstruct main-data
;;   (item1)
;;   (item2)
;;   (item3))
;; (defvar *globals* (make-main-data))

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
  (let ((screen (main-setup-display))
        (timer (al:create-timer (/ 1 60.0)))
        (queue (al:create-event-queue))
        (font (al:create-builtin-font))
        (buffer (al:create-bitmap 320 240))
        (event (cffi:foreign-alloc '(:union al:event))))
    
    (unwind-protect
         (progn                         ; Not needed, because of the let ...
           (let* ((w1 (defwindow 200 200 400 400
                        ((defcolumn-layout ((defactive-text :title "Asteroids" :font font :h-align :center
                                                            :left -1 :top -1 :v-align :middle
                                                            :color-down (al:map-rgb-f 0 1 0)
                                                            :color-up (al:map-rgb-f 1 0 1))
                                            (defactive-text :title "Blastem" :font font :h-align :center
                                                            :left -1 :top -1 :v-align :middle
                                                            :color-down (al:map-rgb-f 1 0 1)
                                                            :color-up (al:map-rgb-f 0 1 0))
                                            (defactive-text :title "Quit" :font font :h-align :center
                                                            :left -1 :top -1 :v-align :middle
                                                            :color-down (al:map-rgb-f 1 1 0)
                                                            :color-up (al:map-rgb-f 0 0 1)))))))
                  (boss (make-instance 'manager :content (list w1)))
                  (selected-object nil))

             (defmethod on-mouse-click (x y b (obj (eql (first (content (first (content w1)))))) &key)
               (format *standard-output* "~&Item 1 clicked")
               (setf selected-object obj))

             (defmethod on-mouse-click (x y b (obj (eql (second (content (first (content w1)))))) &key)
               (format *standard-output* "~&Item 2 clicked"))

             (defmethod on-mouse-click (x y b (obj (eql (third (content (first (content w1)))))) &key)
               (format *standard-output* "~&Item 3 clicked")
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
                  ;; (format *standard-output* "~&event: ~a"
                  ;;         (cffi:foreign-slot-value event '(:union al:event) 'al::type))
                  )))

             (setf (border w1) (defborder :color (al:map-rgb-f 1 1 0)))
             (setf (border (second (content (first (content w1))))) (defborder :color (al:map-rgb-f 0 1 1)))
             
             (al:start-timer timer)
             (al:clear-keyboard-state screen)

             (al:register-event-source queue (al:get-keyboard-event-source))
             (al:register-event-source queue (al:get-display-event-source screen))
             (al:register-event-source queue (al:get-timer-event-source timer))
             (al:register-event-source queue (al:get-mouse-event-source))

             (pprint boss)
             (process-events queue boss)))
      
      (progn
        (cffi:foreign-free event)
        (al:destroy-bitmap buffer)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue)
        (al:destroy-font font)))))

