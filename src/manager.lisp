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
  (defmethod on-mouse-down-accept (o (m (eql object)))
    (v:info :event "on-mouse-down-accept: ~a" (print-raw-object o))
    (setf (slot-value object 'last-mouse-down) o))

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
           (block mouse-axis
             (let ((x (mouse-event-x event))
                   (y (mouse-event-y event))
                   (dx (mouse-event event 'al::dx))
                   (dy (mouse-event event 'al::dy)))
               (dolist (child (content object))
                 (if (on-mouse-move x y dx dy child)
                     (return-from mouse-axis))))))
          
          (:mouse-button-down
           (block mouse-button-down
             (let ((x (mouse-event-x event))
                   (y (mouse-event-y event))
                   (b (mouse-event-button event)))
               (dolist (child (content object))
                 (if (on-mouse-down x y b child)
                     (return-from mouse-button-down))))))
          
          (:mouse-button-up
           (block mouse-button-up
             (let ((x (mouse-event-x event))
                   (y (mouse-event-y event))
                   (b (mouse-event-button event))
                   (last (slot-value object 'last-mouse-down)))
               (if (not (eq nil last))
                   (progn
                     (on-mouse-up x y b last)
                     (setf (slot-value object 'last-mouse-down) nil)
                     (return-from mouse-button-up)))
               (dolist (child (content object))
                 (on-mouse-up x y b child)))))

          (otherwise
           (v:debug :event "unhandled event: ~a" event)
           (unhandled-event event object))))
      
      (cffi:foreign-free event)))
  (my-next-method))

(defmethod paint ((obj manager) &key)
  (dolist (child (content obj))
    (on-paint child))
  (my-next-method))



