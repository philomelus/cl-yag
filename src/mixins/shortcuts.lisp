(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; SHORTCUTS-MIXIN-BASE =====================================================

(defclass shortcuts-mixin-base ()
  ())

;;;; SHORTCUTS-MIXIN ==========================================================

(defclass shortcuts-mixin (shortcuts-mixin-base)
  ((shortcuts :type list :initarg :shortcuts :initform nil :accessor shortcuts)
   ;; Internal
   (accels :initform nil)))

(defmethod initialize-instance :after ((object shortcuts-mixin) &key)
  ;; #+safety
  (validate-shortcuts object)
  (convert-shortcuts object))

(defmethod (setf shortcuts) :after (value (object shortcuts-mixin))
  ;; #+safety
  (validate-shortcuts object)
  (convert-shortcuts object))

(defmethod on-char (key mod (object shortcuts-mixin) &key)
  (let ((accels (slot-value object 'accels)))
    (when accels
      ;; Convert mod to value
      (let ((mods (mod2val mod)))
        ;; See if we have been activated
        (dolist (accel accels)
          (if (and (eql key (first accel))
                   (= (second accel) mods))
              (progn
                (v:debug :shortcuts "[on-char] accepted accel:~a key:~a mod:~a ~a ~a" accel key mod
                         (eql key (first accel)) (= (second accel) mods))
                (on-command object)
                (return-from on-char t))
              (v:debug :shortcuts "[on-char] not accepted accel:~a key:~a mod:~a ~a ~a" accel key mod
                       (eql key (first accel)) (= (second accel) mods))
              ))))
    nil)
  (my-next-method))

;;;; functions ================================================================

(defun convert-shortcuts (object)
  "Convert shortcuts to accelerators.

Accelerators are cons of single key and modifiers as bitfield, so there will
most likely be more accellerators than there are shortcuts since shortcuts
allow multiple keys even though they are only allowed once each."

  (let ((accels (list))
        (scs (shortcuts object)))
    (dolist (sc scs)
      ;; Seperate keys and modifiers
      (let ((keys (first sc))
            (mods (second sc)))

        (v:debug :shortcuts "[convert-shortcuts] keys: ~a :mods ~a" keys mods)
        
        ;; Make sure they are in known format
        (unless (eql keys nil)
          (unless (consp keys)
            (setf keys (list keys))))
        (unless (eql mods nil)
          (unless (consp mods)
            (setf mods (list mods))))

        ;; Create bitfield for modifiers
        (let ((mods-val (mod2val mods)))

          ;; Add valid accelerators to list
          (if (eql keys nil)
              ;; No key, are there mods?
              (when (/= mods-val 0)
                  ;; Yes, so its valid
                  (push (list nil mods-val) accels)
                  (v:debug :shortcuts "[convert-shortcuts] added ~a" (list nil mods-val)))
              ;; Add all keys and mods as accels
              (dolist (k keys)
                (push (list k mods-val) accels)
                (v:debug :shortcuts "[convert-shortcuts] added ~a" (list k mods-val)))))))

    ;; Save in object
    (setf (slot-value object 'accels) accels)))

(defun mod2val (mods)
  "Given list of key modifiers, return value to compare with."
  (let ((mods-val 0))
    (if (member :shift mods)
        (incf mods-val 1))
    (if (or (member :control mods)
            (member :ctrl mods))
        (incf mods-val 2))
    (if (member :alt mods)
        (incf mods-val 4))
    (v:debug :shortcuts "[mod2val] mods: ~a = ~d" mods mods-val)
    mods-val))

(defun val2mod (val)
  "Given value for mod comparison, return list of mods."
  (let ((mods ())
        (v val))
    (when (= v 0)
      (push :none mods))
    (when (logbitp 2 val)
      (push :alt mods))
    (when (logbitp 1 val)
      (push :control mods))
    (when (logbitp 0 val)
      (push :shift mods))
    (v:debug :shortcuts "[val2mod] val: ~d = ~a" val mods)
    mods))

(defun validate-shortcuts (object)
  "Validate shortcuts appear to be valid.
Keys aren't checked because of international support.
Modifiers are checked."
  (dolist (sc (shortcuts object))
    (let ((mods (second sc)))
      (if (typep mods 'cons)
          (progn
            (dolist (m mods)
              (unless (member m '(:shift :control :ctrl :none :alt))
                (error "expected :shift, :control, :alt, :none, or :ctrl, but got: ~a" m)))
            (if (and (member :none mods)
                     (> (length mods) 0))
                (error "cannot use :none with any other modifier, got: ~a" mods)))))))


