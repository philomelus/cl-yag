(in-package :cl-yag-tests)

;; TODO: Provide way to create sub-windows easier, like tests-create-window-1, etc.
;;       where the window gets created and its just up to the called method to fill
;;       it in or the like.  It has to be flexible.  Right now there is hard-coding
;;       the below constants all over the place, and that seems unnecessary.
;;
;;       Another option is to provide have defun's that create the windows and adds
;;       the passed in args as the content.  Or the like.

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
  it1 it2 it3 it4 it5 it6 it7 it8

  rv1 rv2
  rh1 rh2 rh3 rh4 rh5 rh6 rh7 rh8

  theme1
  theme2)

(defgeneric tests-command-0 (data) (:documentation "Called when 0 is pressed.") (:method (data)))
(defgeneric tests-command-1 (data) (:documentation "Called when 1 is pressed.") (:method (data)))
(defgeneric tests-command-2 (data) (:documentation "Called when 2 is pressed.") (:method (data)))
(defgeneric tests-command-3 (data) (:documentation "Called when 3 is pressed.") (:method (data)))
(defgeneric tests-command-4 (data) (:documentation "Called when 4 is pressed.") (:method (data)))
(defgeneric tests-command-5 (data) (:documentation "Called when 5 is pressed.") (:method (data)))
(defgeneric tests-command-6 (data) (:documentation "Called when 6 is pressed.") (:method (data)))
(defgeneric tests-command-7 (data) (:documentation "Called when 7 is pressed.") (:method (data)))
(defgeneric tests-command-8 (data) (:documentation "Called when 8 is pressed.") (:method (data)))
(defgeneric tests-command-9 (data) (:documentation "Called when 9 is pressed.") (:method (data)))

(defgeneric tests-command-update (data) (:documentation "Called after calling a tests-command-* and it returns t."))

(defgeneric tests-create (data) (:documentation "Create windows/object/data for tests."))

(defgeneric tests-destroy (data) (:documentation "Clean up data created for tests.") (:method (data)))

(defgeneric tests-ready (data) (:documentation "Called just before entering event loop."))

(defgeneric tests-render (data)
  (:method (data)
    nil)
  (:documentation "Called to render display.  If it returns nil, will clear screen and  call
paint for manager object.  Display is flipped automatically after call
regardless of result."))

(defun tests-instructions-create (data left &optional right)
  (macrolet ((make-inst (used field title is-left)
               (let ((u (gensym)))
                 `(let ((,u ,used))
                    (when ,u
                      ,(if is-left
                          `(setf ,field (deftext :title ,title :height :auto-min :width :auto :padding-left 10))
                          `(setf ,field (deftext :title ,title :height :auto-min :width :auto :padding-right 10 :h-align :right))))
                    ,u))))
    
    (with-slots (iw1 iw2 icl1 icl2 it1 it2 it3 it4 it5 it6 it7 it8) data
      (let ((num-left (length left))
            (num-right 0)
            widgets-l widgets-r)
        
        (assert (<= num-left 4))
        (unless (eql right nil)
          (setq num-right (length right))
          (assert (<= num-right 4)))

        ;; Create left instructions
        (if (make-inst (>= num-left 4) it4 (fourth left) t) (push (list it4 :min-height) widgets-l))
        (if (make-inst (>= num-left 3) it3 (third left) t) (push (list it3 :min-height) widgets-l))
        (if (make-inst (>= num-left 2) it2 (second left) t) (push (list it2 :min-height) widgets-l))
        (if (make-inst (>= num-left 1) it1 (first left) t) (push (list it1 :min-height) widgets-l))

        ;; Create right instructions
        (if (make-inst (>= num-right 4) it8 (fourth right) nil) (push (list it8 :min-height) widgets-r))
        (if (make-inst (>= num-right 3) it7 (third right) nil) (push (list it7 :min-height) widgets-r))
        (if (make-inst (>= num-right 2) it6 (second right) nil) (push (list it6 :min-height) widgets-r))
        (if (make-inst (>= num-right 1) it5 (first right) nil) (push (list it5 :min-height) widgets-r))

        ;; Final left preparations
        (setf icl1 (defcolumn-layout))
        (when (> (length widgets-l) 0)
          (setf (content icl1) widgets-l))
        (setf iw1 (defwindow +I1X+ +I1Y+ +I1W+ +I1H+ :content (list icl1)))
        ;; (setf iw1 (defwindow +I1X+ +I1Y+ +I1W+ +I1H+))
        (assert (not (eql iw1 nil)))

        ;; Final right preparations
        (setf icl2 (defcolumn-layout))
        (when (> (length widgets-r) 0)
          (setf (content icl2) widgets-r))
        (setf iw2 (defwindow +I2X+ +I2Y+ +I2W+ +I2H+ :content (list icl2)))
        (assert (not (eql iw2 nil)))

        (values iw1 iw2)))))

(defun tests-rulers-create (data top-row bottom-row)
  (with-slots (rv1 rv2 rh1 rh2 rh3 rh4 rh5 rh6 rh7 rh8) data
    (let ((majc (al:map-rgb-f 1 0 0))
          (minc (al:map-rgb-f 0.8 0 0)))
      (when top-row
        (setf rv1 (ruler-25-5 :visible t :line-color majc :vertical t
                              :div-25-color majc :div-25-extent 1
                              :div-5-color minc :div-5-extent 0.5
                              :left (- +W1X+ 10) :top +W1Y+ :width 10 :height +W1H+ ))
        (setf rh1 (ruler-25-5 :visible t :line-color majc
                              :div-25-color majc :div-25-extent 1
                              :div-5-color minc :div-5-extent 0.5
                              :left +W1X+ :top (- +W1Y+ 10) :width +W1W+ :height 10))
        (setf rh2 (ruler-25-5 :visible t :line-color majc
                              :div-25-color majc :div-25-extent 1
                              :div-5-color minc :div-5-extent 0.5
                              :left +W2X+ :top (- +W2Y+ 10) :width +W2W+ :height 10))
        (setf rh3 (ruler-25-5 :visible t :line-color majc
                              :div-25-color majc :div-25-extent 1
                              :div-5-color minc :div-5-extent 0.5
                              :left +W3X+ :top (- +W3Y+ 10) :width +W3W+ :height 10))
        (setf rh4 (ruler-25-5 :visible t :line-color majc
                              :div-25-color majc :div-25-extent 1
                              :div-5-color minc :div-5-extent 0.5
                              :left +W4X+ :top (- +W4Y+ 10) :width +W4W+ :height 10)))
      (when bottom-row
        (setf rv2 (ruler-25-5 :visible t :line-color majc :vertical t
                              :div-25-color majc :div-25-extent 1
                              :div-5-color minc :div-5-extent 0.5
                              :left (- +W5X+ 10) :top +W5Y+ :width 10 :height +W5H+))
        (setf rh5 (ruler-25-5 :visible t :line-color majc
                              :div-25-color majc :div-25-extent 1
                              :div-5-color minc :div-5-extent 0.5
                              :left +W5X+ :top (- +W5Y+ 10) :width +W5W+ :height 10))
        (setf rh6 (ruler-25-5 :visible t :line-color majc
                              :div-25-color majc :div-25-extent 1
                              :div-5-color minc :div-5-extent 0.5
                              :left +W6X+ :top (- +W6Y+ 10) :width +W6W+ :height 10))
        (setf rh7 (ruler-25-5 :visible t :line-color majc
                              :div-25-color majc :div-25-extent 1
                              :div-5-color minc :div-5-extent 0.5
                              :left +W7X+ :top (- +W7Y+ 10) :width +W7W+ :height 10))
        (setf rh8 (ruler-25-5 :visible t :line-color majc
                              :div-25-color majc :div-25-extent 1
                              :div-5-color minc :div-5-extent 0.5
                              :left +W8X+ :top (- +W8Y+ 10) :width +W8W+ :height 10))))
    
    (if (and top-row bottom-row)
        (values rv1 rv2 rh1 rh2 rh3 rh4 rh5 rh6 rh7 rh8)
        (if top-row
            (values rv1 rh1 rh2 rh3 rh4)
            (values rv2 rh5 rh6 rh7 rh8)))))

(defun tests-toggle-interior-color (data widgets)
  (with-slots (manager theme1) data
    (let ((ic1 (interior-color theme1))
          (ic2 (al:map-rgb-f 1 0 0)))
      (dolist (w widgets)
        (with-local-slots ((ic interior-color)) w
          (if (or (equal ic ic1)
                  (eql ic nil))
              (setf (interior-color w) ic2)
              (setf (interior-color w) ic1)))))))

(defun tests-toggle-theme (data)
  (with-slots (manager theme1 theme2) data
    (if (eql (theme manager) theme1)
        (setf (theme manager) theme2)
        (setf (theme manager) theme1))))

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

           ;; Set up themes
           (let ((fnt (default-font -24)))
             (setf (font *theme-default*) fnt)
             (setf (tests-data-theme1 data) (theme-flat-gray))
             (setf (font (tests-data-theme1 data)) fnt)
             (setf (tests-data-theme2 data) (theme-3d-gray))
             (setf (font (tests-data-theme2 data)) fnt))
           
           ;; Let test set up window/objects
           (tests-create data)

           ;; Set initial theme
           (setf (theme (tests-data-manager data)) (tests-data-theme1 data))

           ;; Final preparations
           (al:start-timer timer)
           (al:clear-keyboard-state screen)

           (defmethod cl-yag::on-char ((key (eql :escape)) mods (object (eql (tests-data-manager data))) &key)
             (setf (process object) nil)
             t)

           (defmethod cl-yag::on-char ((key (eql :0)) mods (object (eql (tests-data-manager data))) &key)
             (if (tests-command-0 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :1)) mods (object (eql (tests-data-manager data))) &key)
             (if (tests-command-1 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :2)) mods (object (eql (tests-data-manager data))) &key)
             (if (tests-command-2 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :3)) mods (object (eql (tests-data-manager data))) &key)
             (if (tests-command-3 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :4)) mods (object (eql (tests-data-manager data))) &key)
             (if (tests-command-4 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :5)) mods (object (eql (tests-data-manager data))) &key)
             (if (tests-command-5 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :6)) mods (object (eql (tests-data-manager data))) &key)
             (if (tests-command-6 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :7)) mods (object (eql (tests-data-manager data))) &key)
             (if (tests-command-7 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :8)) mods (object (eql (tests-data-manager data))) &key)
             (if (tests-command-8 data)
                 (tests-command-update data))
             t)
           
           (defmethod cl-yag::on-char ((key (eql :9)) mods (object (eql (tests-data-manager data))) &key)
             (if (tests-command-9 data)
                 (tests-command-update data))
             t)
           
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

