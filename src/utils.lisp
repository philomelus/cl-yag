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

(defmethod between ((lo integer) (hi integer))
  (let* ((r (+ lo (* (random 1.0) (- hi lo))))
		 (result (truncate r)))
	result))

(defmethod between ((lo float) (hi float))
  (+ lo (* (random 1.0) (- hi lo))))

(defun color2assoc (color)
  (let ((rgba (color2list color)))
    `((:r ,(first rgba)) (:g ,(second rgba)) (:b ,(third rgba)) (:a ,(fourth rgba)))))

(defun color2list (color)
  (list
   (nth (1+ (position 'al::r color)) color)
   (nth (1+ (position 'al::g color)) color)
   (nth (1+ (position 'al::b color)) color)
   (nth (1+ (position 'al::a color)) color)))

(defun color-a (color)
  (fourth (color2list color)))

(defun color-b (color)
  (third (color2list color)))

(defun color-g (color)
  (second (color2list color)))

(defun color-inverse (color)
  (al:map-rgb-f (- 1 (color-r color))
                (- 1 (color-g color))
                (- 1 (color-b color))))

(defun color-r (color)
  (first (color2list color)))

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

(defun print-color (color &optional (stream nil))
  (format stream "al:map-rgba-f ~d ~d ~d ~d" (color-r color) (color-g color) (color-b color) (color-a color)))

(defun print-raw-object (o)
  (with-output-to-string (s)
    (print-unreadable-object (o s :type t :identity t)
      (format nil "")
      (write-char #\Space))))

(declaim (ftype (function (integer integer integer integer integer integer integer integer) boolean) rect-collide))
(defun rect-collide (left1 top1 right1 bottom1 left2 top2 right2 bottom2)
  (cond ((> left1 right2) (return-from rect-collide nil))
		((< right1 left2) (return-from rect-collide nil))
		((> top1 bottom2) (return-from rect-collide nil))
		((< bottom1 top2) (return-from rect-collide nil)))
  t)

(defun show-package-functions (package-name)
  (let ((package (find-package package-name)))
    (when package
      (format t "Functions in package ~a:~%" (package-name package))
      (do-external-symbols (s package)
        (when (fboundp s)
          (format t "~s~%" s))))
    (unless package
      (format t "Package ~a not found.~%" package-name))))

