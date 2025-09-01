(in-package :cl-yag-tests)

(defconstant +W1X+ 25)
(defconstant +W1Y+ 25)
(defconstant +W1W+ 200)
(defconstant +W1H+ 300)

(defconstant +W2X+ 250)
(defconstant +W2Y+ 25)
(defconstant +W2W+ 200)
(defconstant +W2H+ 300)

(defconstant +W3X+ 475)
(defconstant +W3Y+ 25)
(defconstant +W3W+ 200)
(defconstant +W3H+ 300)

(defconstant +W4X+ 700)
(defconstant +W4Y+ 25)
(defconstant +W4W+ 200)
(defconstant +W4H+ 300)

(defconstant +W5X+ 25)
(defconstant +W5Y+ 350)
(defconstant +W5W+ 200)
(defconstant +W5H+ 300)

(defconstant +W6X+ 250)
(defconstant +W6Y+ 350)
(defconstant +W6W+ 200)
(defconstant +W6H+ 300)

(defconstant +W7X+ 475)
(defconstant +W7Y+ 350)
(defconstant +W7W+ 200)
(defconstant +W7H+ 300)

(defconstant +W8X+ 700)
(defconstant +W8Y+ 350)
(defconstant +W8W+ 200)
(defconstant +W8H+ 300)

(defconstant +I1X+ 25)
(defconstant +I1Y+ 675)
(defconstant +I1W+ 400)
(defconstant +I1H+ 110)

(defconstant +I2X+ 500)
(defconstant +I2Y+ 675)
(defconstant +I2W+ 400)
(defconstant +I2H+ 110)

(defstruct tests-data
  manager event queue timer
  iw1 iw2
  icl1 icl2
  it1 it2 it3 it4 it5 it6 it7 it8)

(defgeneric tests-create (data) (:documentation "Create windows/object/data for tests."))

(defgeneric tests-destroy (data) (:documentation "Clean up data created for tests."))

(defgeneric tests-ready (data) (:documentation "Called just before entering event loop."))

(defgeneric tests-render (data)
  (:method (data)
    nil)
  (:documentation "Called to render display.  If it returns nil, will clear screen and  call
paint for manager object.  Display is flipped automatically after call
regardless of result."))

(defun tests-add-instruction (data left &optional right)
  (declare (ignore right))
  (macrolet ((make-inst (used field title)
               (let ((u (gensym)))
                 `(let ((,u ,used))
                    (when ,u
                      (setf ,field (deftext :title ,title :height :auto-min :padding-left 10)))
                    ,u))))
    
    (with-slots (iw1 iw2 icl1 icl2 it1 it2 it3 it4 it5 it6 it7 it8) data
      (let ((num-left (length left))
            (num-right 0)
            ;; widgets-l widgets-r
            )
        (declare (ignore num-right))
        
        (assert (<= num-left 4))
        ;; (unless (eql right nil)
        ;;   (setq num-right (length right))
        ;;   (assert (<= num-right 4)))

        ;; Create left instructions
        ;; (if (make-inst (>= num-left 1) it1 (first left)) (push (list it1 :min-height) widgets-l))
        ;; (if (make-inst (>= num-left 2) it2 (second left)) (push (list it2 :min-height) widgets-l))
        ;; (if (make-inst (>= num-left 3) it3 (third left)) (push (list it3 :min-height) widgets-l))
        ;; (if (make-inst (>= num-left 4) it4 (fourth left)) (push (list it4 :min-height) widgets-l))

        ;; Create right instructions
        ;; (if (make-inst (>= num-right 1) it5 (first right)) (push (list it5 :min-height) widgets-r))
        ;; (if (make-inst (>= num-right 2) it6 (second right)) (push (list it6 :min-height) widgets-r))
        ;; (if (make-inst (>= num-right 3) it7 (third right)) (push (list it7 :min-height) widgets-r))
        ;; (if (make-inst (>= num-right 4) it8 (fourth right)) (push (list it8 :min-height) widgets-r))

        ;; Final left preparations
        (setf icl1 (defcolumn-layout))
        ;; (when (> (length widgets-l) 0)
        ;;   (setf (content icl1) widgets-l))
        (setf iw1 (defwindow +I1X+ +I1Y+ +I1W+ +I1H+ :content (list icl1)))
        ;; (setf iw1 (defwindow +I1X+ +I1Y+ +I1W+ +I1H+))
        (assert (not (eql iw1 nil)))

        ;; Final right preparations
        (setf icl2 (defcolumn-layout))
        ;; (when (> (length widgets-r) 0)
        ;;   (setf (content icl2) widgets-r))
        (setf iw2 (defwindow +I2X+ +I2Y+ +I2W+ +I2H+ :content (list icl2)))
        (assert (not (eql iw2 nil)))

        (v:info :fuckyou "iw1:~a iw2:~a" (print-raw-object iw1) (print-raw-object iw2))
        (values iw1 iw2)))))

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

  ;;                                     25     250    475    700   [925]   25     350    675   [810]
  (let ((screen (al:create-display (+ 25 200 25 200 25 200 25 200 25) (+ 25 300 25 300 25 110 25) ))
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

           ;; Use larger font
           (setf (font *theme-default*) (default-font -24))

           ;; Set up window/objects
           (tests-create data)

           (al:start-timer timer)
           (al:clear-keyboard-state screen)

           (defmethod cl-yag::on-char ((key (eql :escape)) mods (object (eql (tests-data-manager data))) &key)
             (setf (process object) nil)
             t)

           ;; (defmethod cl-yag::on-char ((key (eql :1)) mods (object (eql (tests-data-manager data))) &key)
           ;;   (v:info :tests "[on-char] {:1} caught!"))
           
           (defmethod cl-yag::on-timer (timer count (object (eql (tests-data-manager data))) &key)
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
           (process-events queue (tests-data-manager data))
           
           ;; Clean up windows/objects
           (tests-destroy data))

      (progn
        (cffi:foreign-free event)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue)))))

