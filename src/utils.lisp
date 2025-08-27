(in-package :cl-yag)

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

(defmethod between ((lo integer) (hi integer))
  (let* ((r (+ lo (* (random 1.0) (- hi lo))))
		 (result (truncate r)))
	result))

(defmethod between ((lo float) (hi float))
  (+ lo (* (random 1.0) (- hi lo))))

(defmethod must-init ((test sb-sys::system-area-pointer) desc)
  (when (null-pointer-p test)
    (if (>  (al:get-errno) 0)
        (error "Couldn't initialize ~s (~d)." desc (al:get-errno))
        (error "Couldn't initialize ~s." desc))))

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

(declaim (ftype (function (integer integer integer integer integer integer integer integer) boolean) rect-collide))
(defun rect-collide (left1 top1 right1 bottom1 left2 top2 right2 bottom2)
  (cond ((> left1 right2) (return-from rect-collide nil))
		((< right1 left2) (return-from rect-collide nil))
		((> top1 bottom2) (return-from rect-collide nil))
		((< bottom1 top2) (return-from rect-collide nil)))
  t)

