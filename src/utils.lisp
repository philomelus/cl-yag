(in-package :cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

(declaim (ftype (function (number) number) 2+))
(defun 2+ (value)
  (+ value 2))

(declaim (ftype (function (number) number) 2-))
(defun 2- (value)
  (- value 2))

(defun asset (path)
  (let* ((real-path (concatenate 'string "assets/" path))
         (yag-path (asdf:system-relative-pathname "cl-yag" real-path)))
    (namestring yag-path)))

(defun print-object-as-string (object)
  (with-output-to-string (s)
    (princ object)
    s))

(defun print-raw-object (o &key (name nil) (bare nil))
  (with-output-to-string (s)
    (if bare
        (print-unreadable-object (o s :type nil :identity t)
          (format nil ""))
        (if (eq nil name)
            (print-unreadable-object (o s :type t :identity t)
              (format nil ""))
            (print-unreadable-object (o s :identity t)
              (format s "~a" name))))))

(defun print-thread-name ()
  (format nil "~a" (bt:thread-name (bt:current-thread))))

(defun remove-keyword-params (args declared-keys)
  "Removes keyword parameters and their values from a list of arguments.
   ARGS is the list of arguments (e.g., from &rest).
   DECLARED-KEYS is a list of keywords that are expected as parameters."
  (loop for (key val) on args by #'cddr
        unless (member key declared-keys)
          collect key
          and collect val))

(defun shallow-copy-object (original)
  "Return duplicate of object with identical slot values, including unbound. No
link to original object in new object."
  
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'slot-definition-name
                          (class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))
