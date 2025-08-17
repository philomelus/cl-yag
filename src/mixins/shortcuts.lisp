(in-package #:cl-yag)

;;;; functions ================================================================

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
    mods-val))

(defun val2mod (val)
  "Given value for mod comparison, return list of mods."
  (let ((mods ())
        (v val))
    (when (= v 0)
      (push :none mods)
      (return-from val2mod mods))
    (when (logbitp 2 val)
      (push :alt mods))
    (when (logbitp 1 val)
      (push :control mods))
    (when (logbitp 0 val)
      (push :shift mods))
    mods))

(defun validate-shortcuts (object)
  "Validate shortcuts appear to be valid.
Keys aren't checked because if international support.
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

        ;; Make sure they are in known format
        (unless (eql nil keys)
          (unless (typep keys 'cons)
            (setf keys (list keys))))
        (unless (eql nil mods)
          (unless (typep mods 'cons)
            (setf mods (list mods))))

        ;; Create bitfield for modifiers
        (let ((mods-val (mod2val mods)))

          ;; Add valid accelerators to list
          (if (eql nil keys)
              ;; No key, are there mods?
              (if (> mods-val 0)
                  ;; Yes, so its valid
                  (push (list nil mods-val) accels))
              ;; Add all keys and mods as accels
              (dolist (k keys)
                (push (list k mods-val) accels))))))

    ;; Save in object
    (setf (slot-value object 'accels) accels)))

;;;; shortcuts-mixin ==========================================================

(defclass shortcuts-mixin ()
  ((shortcuts :initarg :shortcuts :initform nil :type list :accessor shortcuts)
   ;; Internal
   (accels :initform nil)))

(defmethod print-mixin ((o shortcuts-mixin) s)
  (pprint-indent :current 0 s)
  (format s ":shortcuts (")
  (if (= (length (shortcuts o)) 0)
      (format s " ")
      (let ((accels (slot-value o 'accels)))
        (dolist (accel accels)
          (format s "(:~a " (first accel))
          (let ((mods (val2mod (second accel))))
            (if (= (length mods) 1)
                (format s ":~a) " (first mods))
                (progn
                  (format s "(")
                  (dolist (kw mods)
                    (format s ":~a " kw))
                  (format s ")) ")))))))
  (format s ")")
  (pprint-newline :linear s)
  (my-next-method))

(defmethod initialize-instance :after ((object shortcuts-mixin) &key)
  #+safety
  (validate-shortcuts object)
  (convert-shortcuts object))

(defmethod (setf shortcuts) :after (value (object shortcuts-mixin))
  #+safety
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
                (v:debug :events "on-char: shortcuts-mixin: accepted ~a" accel)
                (on-command object)
                (return-from on-char)))))))
  (my-next-method))
