(in-package #:clg)

;;;; manager ==================================================================

(defclass manager (dispatch
                   container-mixin)
  ((ready :initform nil :type boolean :accessor manager-ready)
   (process :initform nil :type boolean :accessor manager-process)))

(defmethod initialize-instance :after ((obj manager) &key)
  (when (slot-boundp obj 'content)
    (dolist (child (content obj))
      (when (typep child 'parent-mixin)
        (setf (parent child) nil)))))

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
  (setf (content mgr) (list (content mgr) obj))
  (next-method))

(defmethod process-events (queue (object manager) &key unhandled-event-proc &allow-other-keys)
  (if (not (manager-ready object))
      (error "You must call ready on manager before using it."))
  (let ((event (cffi:foreign-alloc '(:union al:event))))
    (unwind-protect
         (setf (manager-process object) t)
      (loop while (manager-process object) do
        (al:wait-for-event queue event)
        (case (event-type event)
          (:key-char
           (let ((key (keyboard-event event 'al::keycode)))
             (trigger (event "char" :data (list key object)) :on object)))
          
          (:mouse-axis
           (let ((x (mouse-event-x event))
                 (y (mouse-event-y event))
                 (dx (mouse-event event 'al::dx))
                 (dy (mouse-event event 'al::dy)))
             (trigger (event "mouse-move" :data (list x y dx dy object)) :on object)))
          
          (:mouse-button-down
           (let ((x (mouse-event-x event))
                 (y (mouse-event-y event))
                 (b (mouse-event-button event)))
             (trigger (event "mouse-down" :data (list x y b object)) :on object)))
          
          (:mouse-button-up
           (let ((x (mouse-event-x event))
                 (y (mouse-event-y event))
                 (b (mouse-event-button event)))
             (trigger (event "mouse-up" :data (list x y b object)) :on object)
             ;; TODO: This should only be sent if starting and ending positions of click are within
             ;;       some reasonable distance, and not within different controlled areas and there
             ;;       has been some amount of delay since the next mouse down (for multiple click
             ;;       differentation)
             (trigger (event "mouse-click" :data (list x y b object)) :on object)))

          (otherwise
           (if (not (eq nil unhandled-event-proc))
               (apply unhandled-event-proc (list object event))))))
      (cffi:foreign-free event)))
  (next-method))

(defmethod paint ((obj manager) &key)
  (dolist (child (content obj))
    (on-paint child))
  (next-method))

(defmethod ready ((obj manager) &key)
  ;; If ready already reset
  (if (manager-ready obj)
      (progn
        (disconnect-all obj)
        (setf (manager-ready obj) nil)))

  ;; Allow all managed objects to get ready
  (let ((children (content obj)))
   (dolist (child children)
     (ready child :manager obj)))

  ;; Everyone is ready to start pumping events
  (setf (manager-ready obj) t)
  (next-method))

(defmethod unmanage (obj mgr &key &allow-other-keys)
  (setf (content mgr) (remove obj (content mgr)))
  (next-method))

;;;------------------------------------------------------------------

(defun root (obj)
  )
