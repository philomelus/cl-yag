(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; manager ==================================================================

(defclass manager (content-mixin
                   theme-mixin)
  ((process :initform nil :type boolean :accessor process)
   (last-mouse-down :initform nil)))

(defmacro defmanager (&rest rest &key &allow-other-keys)
  `(make-instance 'manager ,@rest))

;;; methods ---------------------------------------------------------

(defmethod on-char (key mods (obj manager) &key)
  (v:debug :event "[on-char] {manager} got key:~a mods:~a" key mods)
  (dolist (child (content obj))
    (v:debug :event "[on-char] {manager} passing to ~a" (print-raw-object child))
    (when (on-char key mods child)
      (v:debug :event "[on-char] {manager} claimed by ~a" (print-raw-object child))
      (return-from on-char t)))
  (v:debug :event "[on-char] {manager} unclaimed key:~a mods:~a" key mods)
  (my-next-method))

(defmethod paint ((obj manager) &key)
  (dolist (child (content obj))
    (assert (not (eql child nil)))
    (on-paint child)))

(defmethod process-events (queue (object manager) &key &allow-other-keys)
  ;; TODO: BUGBUG:  This needs to be unbound when this method returns
  ;;                I don't currently know exactly how to do that
  (defmethod on-mouse-down-accept (o (m (eql object)))
    (v:debug :event "on-mouse-down-accept: ~a" (print-raw-object o))
    (setf (slot-value object 'last-mouse-down) o))

  (let ((event (cffi:foreign-alloc '(:union al:event))))
    (unwind-protect
         (setf (process object) t)
      (loop while (process object) do
        (al:wait-for-event queue event)
        (case (event-type event)
          (:display-close
           (setf (process object) nil))
          
          (:display-resize
           (al:with-display-event-slots (x y width height) event
             (on-resize object x y event height)))
          
          (:key-char
           (al:with-keyboard-event-slots (keycode modifiers) event
             (v:debug :event ":key-char ~a ~a" keycode modifiers)
             (on-char keycode modifiers object)))
          
          (:key-down
           (al:with-keyboard-event-slots (keycode modifiers) event
             (v:debug :event ":key-down ~a ~a" keycode modifiers)
             (on-key-down keycode modifiers object)))
          
          (:key-up
           (al:with-keyboard-event-slots (keycode modifiers) event
             (v:debug :event ":key-up ~a ~a" keycode modifiers)
             (on-key-up keycode modifiers object)))

          (:mouse-axis
           (block mouse-axis
             (al:with-mouse-event-slots (x y dx dy) event
               (dolist (child (content object))
                 (if (on-mouse-move x y dx dy child)
                     (return-from mouse-axis))))))
          
          (:mouse-button-down
           (block mouse-button-down
             (al:with-mouse-event-slots (x y button) event
               (dolist (child (content object))
                 (if (on-mouse-down x y button child)
                     (return-from mouse-button-down))))))
          
          (:mouse-button-up
           (block mouse-button-up
             (al:with-mouse-event-slots (x y button) event
               (let ((last (slot-value object 'last-mouse-down)))
                 (if (not (eq nil last))
                     (progn
                       (on-mouse-up x y button last)
                       (setf (slot-value object 'last-mouse-down) nil)
                       (return-from mouse-button-up)))
                 (dolist (child (content object))
                   (on-mouse-up x y button child))))))
          
          (:timer
           (al:with-timer-event-slots (source count) event
             (on-timer source count object)))
          
          (otherwise
           (v:debug :event "unhandled event: ~a" (event-type event))
           (unhandled-event event object))))
      
      (cffi:foreign-free event)))
  
  (my-next-method))

