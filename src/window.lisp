(in-package #:clg)

;;;; window ===================================================================

(defclass window (active-mixin
                  area-mixin
                  enable-mixin
                  visible-mixin)
  ((content :initarg :content :initform () :type list :accessor window-content)
   (options :initarg :options :initform () :type list :accessor window-options)
   ))

;;-------------------------------------------------------------------
;; Make sure our children know who their parent is

(defmethod initialize-instance :after ((obj window) &key)
  (if (slot-boundp obj 'content)
      (dolist (child (window-content obj))
        (if (typep child 'parent-mixin)
            (setf (parent child) obj)))))

(defmethod (setf window-content) :after (val (obj window))
  (dolist (child (window-content obj))
    (setf (parent child) obj)))

;;-------------------------------------------------------------------

;; If window is visible, hide it
(defmethod hide ((obj window) &key)
  (when (visible obj)
    (setf (visible obj) nil))
  (next-method))

(defmethod layout ((obj window) mgr &key)
  ;; Let contained objects layout
  (dolist (child (window-content obj))
    (layout child mgr :parent obj))
  (next-method))

(defmethod on-mouse-down ((obj window) x y b &key)
  (format *standard-output* "~&on-mouse-down (window): ~d ~d ~8b" x y b)
  (next-method))

(defmethod on-mouse-moved ((obj window) x y dx dy &key)
  (format *standard-output* "~&on-mouse-moved (window): ~d ~d ~d ~d" x y dx dy)
  (next-method))

(defmethod on-mouse-up ((obj window) x y b &key)
  (format *standard-output* "~&on-mouse-up (window): ~d ~d ~8b" x y b)
  (next-method))

;; Paint all contained objects and set clean
(defmethod on-paint ((obj window) &key)
  (let ((children (window-content obj)))
    (dolist (c children)
      (progn
        (on-paint c))))
  (next-method))

(defmethod ready ((obj window) &key manager)
  ;; Let contained object prepare for events
  (dolist (child (window-content obj))
    (ready child :manager manager :parent obj))
  (next-method))

;; If window isn't visible, make it so
(defmethod show ((obj window) &key)
  (unless (visible obj)
    (setf (visible obj) t))
  (next-method))

;; Make sure options is a list
;; TODO:  Make sure its a list of keywords ...
(defmethod (setf window-options) :after (newval (obj window))
  (with-slots (options) obj
    (when (not (typep options 'list))
      (setq newval (list newval))
      (cerror "Retry, using (list ~a)" "options must be a list, receivd ~a" options)
      (setf (window-options obj) newval)))
  (next-method))
