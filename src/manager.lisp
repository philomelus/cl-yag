(in-package #:cl-yag)

;;;; manager ==================================================================

(defclass manager (content-mixin)
  ((process :initform nil :type boolean :accessor process)
   (last-mouse-down :initform nil)))

(defmethod print-object ((o manager) s)
  (pprint-indent :current 0 s)
  (pprint-logical-block (s nil)
    (format s "(defmanager ")
    (print-mixin o s)
    (format s ")")))

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
               (on-mouse-up x y b child)))
           (setf (slot-value object 'last-mouse-down) nil))

          (otherwise
           (unhandled-event event object))))
      
      (cffi:foreign-free event)))
  (my-next-method))

(defmethod paint ((obj manager) &key)
  (dolist (child (content obj))
    (on-paint child))
  (my-next-method))



