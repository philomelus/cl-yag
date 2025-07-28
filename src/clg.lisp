(in-package :clg)

(declaim (optimize (debug 3) (safety 3)))

;; I simply want to create a UI where I have 3 items, Asteroids, Blastem, and Exit/Quit.
;; Something like below (or whatever is better):
;;
;; (window :title "CLG Test"
;;         :content (vertical-grid :items ((active-text "Asteroids" ...)
;;                                         (active-text "Blastem" ...)
;;                                         (active-text "Quit" ...))
;;                                 ...)
;;         ...)
;;

;;;; cl-liballegro objects ====================================================

;; (define-cobject-class (any-event (:struct al::any-event)))

;; (define-cobject-class (keyboard-event (:struct al::keyboard-event)))

;; (define-cobject-class (mouse-event (:struct al::mouse-event)))

;; (define-cobject-class (display-event (:struct al::display-event)))

;;;; generics =================================================================

;;;; macros ===================================================================

(defmacro next-method ()
  `(if (next-method-p) (call-next-method)))

;;;; manager ==================================================================

(defclass manager (dispatch)
  ((contents :initarg :contents :initform () :accessor manager-contents)
   (ready :initform nil :type boolean :accessor manager-ready)
   (process :initform nil :type boolean :accessor manager-process)
   ))

(defmethod connect (name obj (mgr manager) func &key)
  (bind name func :on mgr)
  (next-method))

(defmethod disconnect (name obj (mgr manager) func &key)
  (unbind name func :on mgr)
  (next-method))

(defmethod disconnect-all (mgr &key)
  (unbind-all :* :on mgr)
  (next-method))

(defmethod manage (obj mgr &key &allow-other-keys)
  ;; Maintain order of addition so events are passed in same order
  (setf (manager-contents mgr) (list (manager-contents mgr) obj))
  (next-method))

(defmethod process-events (queue (object manager) &key unhandled-event-proc &allow-other-keys)
  (if (not (manager-ready object))
      (error "You must call ready on manager before using it."))
  (let ((event (cffi:foreign-alloc '(:union al:event))))
    (unwind-protect
         (setf (manager-process object) t)
      (loop while (manager-process object) do
        (al:wait-for-event queue event)
        ;; (format *standard-output* "~&process-events: manager: ~a"
        ;;         (cffi:foreign-slot-value event '(:union al:event) 'al::type))
        (case (cffi:foreign-slot-value event '(:union al:event) 'al::type)
          (:key-char
           (let ((key (cffi:foreign-slot-value event '(:struct al:keyboard-event) 'al::keycode)))
             ;; (format *standard-output* "~&process-events: :key-char = ~a" key)
             (trigger (event "char" :data (list key object)) :on object)))
          
          (:mouse-axis
           ;; (format *standard-output* "~&process-events: :mouse-axis")
           (let ((x (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::x))
                 (y (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::y))
                 (dx (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::dx))
                 (dy (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::dy)))
             (trigger (event "mouse-move" :data (list x y dx dy object)) :on object)))
          
          (:mouse-button-down
           ;; (format *standard-output* "~&process-events: :mouse-button-down")
           (let ((x (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::x))
                 (y (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::y))
                 (b (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::button)))
             (trigger (event "mouse-down" :data (list x y b object)) :on object)))
          
          (:mouse-button-up
           ;; (format *standard-output* "~&process-events: :mouse-button-up")
           (let ((x (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::x))
                 (y (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::y))
                 (b (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::button)))
             (trigger (event "mouse-up" :data (list x y b object)) :on object)
             (trigger (event "mouse-click" :data (list x y b object)) :on object)))

          (otherwise
           ;; (format *standard-output* "~&process-events: otherwise:")
           (if (not (eq nil unhandled-event-proc))
               (apply unhandled-event-proc (list object event))))))
      (cffi:foreign-free event)))
  (next-method))

(defmethod paint ((obj manager) &key)
  (dolist (child (manager-contents obj))
    (on-paint child))
  (next-method))

(defmethod ready ((obj manager) &key)
  ;; If ready allready reset
  (if (manager-ready obj)
      (progn
        (disconnect-all obj)
        (setf (manager-ready obj) nil)))

  (let ((children (manager-contents obj)))
    ;; Allow all managed object to perform layout
    (dolist (child children)
      (layout child obj))
  
   ;; Allow all managed objects to get ready
   (dolist (child children)
     (ready child :manager obj)))

  ;; connect manager needed events
  ;; currently none

  ;; Everyone is ready to start pumping events
  (setf (manager-ready obj) t)
  (next-method))

(defmethod unmanage (obj mgr &key &allow-other-keys)
  (setf (manager-contents mgr) (remove obj (manager-contents mgr)))
  (next-method))

;;;; window ===================================================================

(defclass window (active-mixin
                  area-mixin
                  enable-mixin
                  visible-mixin)
  ((content :initarg :content :initform () :type list :accessor window-content)
   (options :initarg :options :initform () :type list :accessor window-options)
   ))

(defun print-window (object stream)
  (format stream "~&window:~&  options: ~a~&  active: ~a~&  enabled: ~a~&  left/top: ~d ~d~&  width/height: ~d ~d~&  visible: ~a~&  content: ~a"
          (window-options object) (active object) (enabled object) (area-left object)
          (area-top object) (area-width object) (area-height object) (visible object) (window-content object)))

;; If window is visible, hide it
(defmethod hide ((obj window) &key)
  (when (visible obj)
    (setf (visible obj) nil))
  (next-method))

(defmethod layout ((obj window) mgr &key)
  ;; Let contained objects layout
  (dolist (child (window-content obj))
    (layout child mgr :parent obj))
  (next-method))

(defmethod on-mouse-down ((obj window) x y b &key)
  (format *standard-output* "~&on-mouse-down (window): ~d ~d ~8b" x y b)
  (next-method))

(defmethod on-mouse-moved ((obj window) x y dx dy &key)
  (format *standard-output* "~&on-mouse-moved (window): ~d ~d ~d ~d" x y dx dy)
  (next-method))

(defmethod on-mouse-up ((obj window) x y b &key)
  (format *standard-output* "~&on-mouse-up (window): ~d ~d ~8b" x y b)
  (next-method))

;; Paint all contained objects and set clean
(defmethod on-paint ((obj window) &key)
  (let ((children (window-content obj)))
    (dolist (c children)
      (progn
        (on-paint c))))
  (next-method))

(defmethod ready ((obj window) &key manager)
  ;; Let contained object prepare for events
  (dolist (child (window-content obj))
    (ready child :manager manager :parent obj))
  (next-method))

;; If window isn't visible, make it so
(defmethod show ((obj window) &key)
  (unless (visible obj)
    (setf (visible obj) t))
  (next-method))

;; Make sure options is a list
;; TODO:  Make sure its a list of keywords ...
(defmethod (setf window-options) :after (newval (obj window))
  (with-slots (options) obj
    (when (not (typep options 'list))
      (setq newval (list newval))
      (cerror "Retry, using (list ~a)" "options must be a list, receivd ~a" options)
      (setf (window-options obj) newval)))
  (next-method))

;;;; vertical-grid ============================================================

(defclass vertical-grid (h-align-mixin
                         v-align-mixin)
  ((content :initarg :content :initform () :type list :accessor vertical-grid-content)))

(defun print-vertical-grid (obj stream)
  (format stream "vertical-grid:~&  h-align: ~a~&  v-align: ~a~&  content: ~a"
          (h-align obj) (v-align obj)
          (vertical-grid-content obj)))

(defmethod layout ((obj vertical-grid) (mgr manager) &key parent)
  (declare (ignore parent))
  (dolist (child (vertical-grid-content obj))
    (layout child mgr :parent obj))
  (next-method))

(defmethod on-mouse-clicked ((obj vertical-grid) x y b &key)
  ;; If click is within boundary of contained items,
  ;; somehow return that object as the result of the processing...
  (next-method))

(defmethod on-mouse-enter ((obj vertical-grid) x y &key)
  ;; Pass on to contained object if its within their controlled area
  (next-method))

(defmethod on-mouse-exit ((obj vertical-grid) x y &key)
  ;; Pass on to contained object if we were in their area and are not now
  (next-method))

;; Paint all contained objects and set clean
(defmethod on-paint ((obj vertical-grid) &key)
  (dolist (c (vertical-grid-content obj))
    (on-paint c))
  (next-method))

(defmethod ready ((obj vertical-grid) &key manager parent)
  (declare (ignore parent))
  (dolist (child (vertical-grid-content obj))
    (ready child :manager manager :parent obj))
  (next-method))

;;;; text =====================================================================

(defclass text (area-mixin
                color-mixin
                font-mixin
                h-align-mixin
                v-align-mixin)
  ((title :initarg :title :initform "" :type string :accessor text-title)))

(defun print-color (color stream)
  (format stream "R: ~d, G: ~d, B: ~d, A: ~d"
          (clg-utils:color-r color)
          (clg-utils:color-g color)
          (clg-utils:color-b color)
          (clg-utils:color-a color)))

(defun print-text (obj stream &optional (title "text"))
  (format stream "~&~a: ~a~&  title: ~s~&  color: ~a~&  area: ~a~&  font: ~a~&  h-align: ~a~&  v-align: ~a"
          title obj (text-title obj) (print-color (color obj) nil)
          (list (area-left obj) (area-top obj) (area-height obj) (area-width obj))
          (font obj) (h-align obj) (v-align obj)))

(defmethod layout ((obj text) mgr &key parent)
  (declare (ignore mgr parent))
  (next-method))

(defmethod on-paint ((obj text) &key)
  (al:draw-text (font obj) (color obj) (area-left obj) (area-top obj) 0 (text-title obj))
  (next-method))

(defmethod ready ((obj text) &key manager parent)
  (declare (ignore manager parent))
  (next-method))

;;;; active-text ==============================================================

(defclass active-text (text)
  ((inside :initform nil :type boolean :accessor active-text-inside)))

(defun print-active-text (obj stream)
  (print-text obj stream "active-text"))

(defmethod (setf text-title) :after (newval (obj active-text))
  (if (and (not (cffi:null-pointer-p (font obj)))
           (not (eq nil newval))
           (> (length newval) 0)
           (member (h-align obj) (list :left :right :center)))
      (progn
        (case (h-align obj)
          (:left
           (setf (area-width obj) (al:get-text-width (font obj) (text-title obj))))
          (:right)
          (:center)))
      )
  )

(defmethod layout ((obj active-text) mgr &key parent)
  (declare (ignore mgr parent))
  (next-method))

(defmethod on-mouse-enter ((obj active-text) x y &key)
  ;; Repaint with hilite of some sort
  (next-method))

(defmethod on-mouse-exit ((obj active-text) x y &key)
  ;; Repaint normally
  (next-method))

(defmethod on-paint ((obj active-text) &key)
  (al:draw-text (font obj) (if (active-text-inside obj)
                               (color-inverse (color obj))
                               (color obj))
                (area-left obj) (area-top obj) 0 (text-title obj))
  (next-method))

(defmethod ready ((obj active-text) &key manager parent)
  (declare (ignore manager parent))
  (next-method))

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

