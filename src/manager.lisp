(in-package #:cl-yag)

;;;; manager ==================================================================

(defclass manager (content-mixin)
  ((process :initform nil :type boolean :accessor process)))

(defmethod process-events (queue (object manager) &key &allow-other-keys)
  (let ((event (cffi:foreign-alloc '(:union al:event))))
    (unwind-protect
         (setf (process object) t)
      (loop while (process object) do
        (al:wait-for-event queue event)
        (case (event-type event)
          (:key-char
           (let ((key (keyboard-event event 'al::keycode))
                 (mods (keyboard-event event 'al::modifiers)))
             (on-char key mods object)))
          
          (:mouse-axis
           (let ((x (mouse-event-x event))
                 (y (mouse-event-y event))
                 (dx (mouse-event event 'al::dx))
                 (dy (mouse-event event 'al::dy)))
             (dolist (child (content object))
               (on-mouse-move x y dx dy child))))
          
          (:mouse-button-down
           (let ((x (mouse-event-x event))
                 (y (mouse-event-y event))
                 (b (mouse-event-button event)))
             (dolist (child (content object))
               (on-mouse-down x y b child))))
          
          (:mouse-button-up
           (let ((x (mouse-event-x event))
                 (y (mouse-event-y event))
                 (b (mouse-event-button event)))
             (dolist (child (content object))
               (on-mouse-up x y b child))))

          (otherwise
           (unhandled-event event object))))
      
      (cffi:foreign-free event)))
  (next-method))

(defmethod paint ((obj manager) &key)
  (dolist (child (content obj))
    (on-paint child))
  (next-method))



