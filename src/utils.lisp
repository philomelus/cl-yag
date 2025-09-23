(in-package :cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;; (defmacro must-init (test desc)
;;   (typecase test
;;     (foreign-pointer
;;      `(when (null-pointer-p ,test)
;;         (if (> (al:get-errno) 0)
;;             (error "Failed to initialize ~s (~d)" ,desc (al:get-errno))
;;             (error "Failed to initialize ~s" ,desc))))
;;     (boolean
;;      `(unless ,test
;;         (if (> (al:get-errno) 0)
;;             (error "Failed to initialize ~s (~d)" ,desc (al:get-errno))
;;             (error "Failed to initialize ~s" ,desc))))
;;     (cons) <<<--- Don't know how to deal with this ... (turns out ... its a list ...)
;;     (otherwise
;;      (error "MUST-INIT unknown type: ~a (~a)" (type-of test) test))))

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

(defmethod must-init ((test t) desc)
  (unless test
    (if (>  (al:get-errno) 0)
        (error "Couldn't initialize ~s (~d)." desc (al:get-errno))
        (error "Couldn't initialize ~s." desc))))

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
