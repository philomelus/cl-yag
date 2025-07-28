(in-package #:clg)

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

